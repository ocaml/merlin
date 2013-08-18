;; merlin.el --- Mode for Merlin, an OCaml scriptable toplevel.   -*- coding: utf-8 -*-

;; Licensed under the MIT license.

;; Author: Simon Castellan <simon.castellan(_)iuwt.fr>
;;         Frédéric Bour  <frederic.bour(_)lakaban.net>
;;         Thomas Refis <thomas.refis(_)gmail.com>
;; Created: 18 April 2013
;; Version: 1.0
;; Package-Requires: ((json))
;; Keywords: ocaml languages
;; URL: http://github.com/def-lkb/merlin

;;; Commentary:
;; Description:
;; merlin-mode is an Emacs interface to merlin, the OCaml scriptable toplevel.  It allows you
;; to perform queries such as getting the type of an expression, completion, and so on.

;; Installation:
;; You need merlin installed on your system (ocamlmerlin binary) for merlin-mode to work.

;;; Usage
;; merlin-mode allows you to send merlin commands from within an Emacs
;; buffer to use merlin capabilities.  A bit like proofgeneral, you
;; need to tell merlin about your code (with merlin-to-point), and
;; then you can perform query to know the type of an expression:
;; merlin will fetch it in the typed tree generated on the fly.

;;;; Code:

;; json and cl are mandatory
(require 'tq)
(require 'cl)
(require 'json)
;; auto-complete is not
(require 'auto-complete nil 'noerror)

(defgroup merlin nil
  "merlin binding mode allowing completion and typing in OCaml files."
  :group 'languages)

;;
;; Faces
;;

(defface merlin-locked-face
  '(
    (((background dark)) :background "#222278")
    (t :background "#eaf8ff")
    )
  "Face for a region that merlin knows of."
  :group 'merlin)
(defface merlin-type-face
  '(
    (t :inherit 'caml-types-expr-face)
    )
  "Face for highlighting a typed expr."
  :group 'merlin)

;;
;; Customizable vars
;;


(defcustom merlin-command "ocamlmerlin"
  "The path to merlin in your installation."
  :group 'merlin :type '(file))
(defcustom merlin-completion-types t
  "If non-nil, print the types of the variables during completion with `auto-complete'."
  :group 'merlin :type 'boolean)

(defcustom merlin-debug nil
  "If non-nil, log the data sent and received from merlin."
  :group 'merlin :type 'boolean)
  
(defcustom merlin-report-warnings t
  "If non-nil, report warnings, otherwise ignore them."
  :group 'merlin :type 'boolean)

(defcustom merlin-type-buffer-name "* merlin types *"
  "The name of the buffer storing module signatures."
  :group 'merlin :type 'string)

(defcustom merlin-favourite-caml-mode 'tuareg-mode
  "The OCaml mode to use for the *merlin types* buffer."
  :group 'merlin :type 'symbol)


(defcustom merlin-margin-lock-string "-"
  "String put in the margin to signal the end of the locked zone."
  :group 'merlin :type 'string)

(defcustom merlin-margin-error-string "!"
  "String to put in the margin of a line containing an error."
  :group 'merlin :type 'string
)

(defcustom merlin-margin-warning-string "?"
  "String to put in the margin of a line containing a warning."
  :group 'merlin :type 'string)

(defcustom merlin-display-lock-zone nil
  "How to display the locked zone.
It is a list of methods among:
   - `highlight': highlight the current locked zone (like proofgeneral)
   - `margin': put a symbol (given by `merlin-margin-lock-string') in the margin
     of the line where the zone ends.

In particular you can specify nil, meaning that the locked zone is not represented on the screen."
  :group 'merlin :type '(repeat
                         (choice (const :tag "Highlight the locked zone" highlight)
                                 (const :tag "Display a marker in the left margin at the end of the locked zone" margin))))

(defcustom merlin-default-flags nil
  "The flags to give to ocamlmerlin."
  :group 'merlin :type '(repeat string))
(defcustom merlin-automatically-garbage-processes t
  "If non-nil, deletes a process when it has no more users.  If nil, keep it."
  :group 'merlin :type 'boolean)
(defcustom merlin-rewind-after-refresh t
  "If non-nil, rewind the buffer after refresh."
  :group 'merlin :type 'boolean)
(defcustom merlin-use-auto-complete-mode nil
  "If non nil, use `auto-complete-mode' in any buffer."
  :group 'merlin :type 'boolean)

(defcustom merlin-ac-prefix-size nil
  "If non-nil, specify the minimum number of characters to wait before allowing auto-complete"
  :group 'merlin :type 'boolean)
;;;;;;;;;;;;;;;;;;;;;
;; Internal variables
;;;;;;;;;;;;;;;;;;;;;


;; merlin flags
(defvar merlin-flags-list '("-rectypes" "-nostdlib" "-absname" "-w" )
  "List of flags that can be passed to ocamlmerlin.")
(defvar merlin-current-flags merlin-default-flags
  "The current list of flags to pass to ocamlmerlin.")

;; Process / Reception related variables
(defvar merlin-processes nil
  "The global merlin process table.  It lists the active instances of merlin.")
(defvar merlin-local-process nil
  "The local merlin process.")
(defvar merlin-queue nil
  "The transaction queue for current process.  This variable lives only in process buffers.")
(defvar merlin-process-users nil
  "Buffer that uses the process (local to a process buffer).")
(defvar merlin-process-last-user nil
  "Last buffer that used the process.")
(defvar merlin-result nil
  "Temporary variables to store command results.")
(defvar merlin-buffer nil "Buffer for merlin input.")
(defvar merlin-ready nil "If non-nil, the reception is done.")

;; Overlays
(defvar merlin-lock-zone-highlight-overlay nil
  "Overlay used for the lock zone highlighting.")
(defvar merlin-lock-zone-margin-overlay nil
  "Overlay used for the margin indicator of the lock zone.")


;; Errors related variables
(defvar merlin-pending-errors nil
  "Pending errors.")
(defvar merlin-lock-point 0
  "Position up to which merlin knows about.")
(defvar merlin-pending-errors-overlays nil
  "Overlays for the pending errors.")
(defvar merlin-highlight-overlay nil "Merlin overlay used for highlights.")

;; Completion related variables
(defvar merlin-completion-point nil
  "Stores the point of last completion (beginning of the prefix).")
(defvar merlin-cache nil "Merlin cache for completion.")
; Vars from auto-complete
(defvar ac-point)
(defvar ac-prefix)
(defvar ac-sources)
(defvar merlin-completion-cache-state nil)
(defvar merlin-completion-annotation-table nil
  "Hold a table mapping completion candidates to their types.")
(make-variable-buffer-local 'merlin-completion-cache-state)
(make-variable-buffer-local 'merlin-completion-annotation-table)

;; Type related variables
(defvar merlin-enclosing-types nil
  "List containing the enclosing type.")
(defvar merlin-enclosing-offset nil
  "Current offset in `merlin-enclosing-types'.")
(defvar merlin-last-point-type nil
  "Last position where the user ran `merlin-magic-show-type'.")

;; Locate
(defvar merlin-position-stack nil)

;; Misc
(defvar merlin-project-file nil "The .merlin file for current buffer.")




;; UTILS

(defun merlin-debug (s)
  "Output S if the variable `merlin-debug' is non-nil on the process buffer associated to the current buffer."
  (with-current-buffer (merlin-get-process-buffer-name)
    (goto-char (point-max))
    (insert s)))

(defun merlin-compute-prefix (ident)
  "Compute the prefix of IDENT.  The prefix of `Foo.bar' is `Foo.' and the prefix of bar is \"\"."
  (let* ((l (butlast (split-string ident "\\.")))
         (s (mapconcat 'identity l ".")))
    (if (string-equal s "") s (concat s "."))))

(defun merlin-goto-point (data)
  "Go to the point indicated by DATA which must be an assoc list with fields line and col."
  "Go to the point indicated by `DATA' which must be an assoc list with fields line and col"
  (goto-char (point-min))
  (forward-line (1- (cdr (assoc 'line data))))
  (forward-char (cdr (assoc 'col data)))
)
(defun merlin-goto-file-and-point (data &optional same-buffer-force)
  "Go to the file and position indicated by DATA which is an assoc list containing fields file, line and col.
If SAME-BUFFER-FORCE is non-nil, create a new window even if it is the same buffer."
  (if (> (length (cdr (assoc 'file data))) 0)
      (find-file-other-window (cdr (assoc 'file data)))
    (when same-buffer-force
        (find-file-other-window buffer-file-name)))
  (merlin-goto-point (cdr (assoc 'pos data))))


(defun merlin-make-point (data)
  "Transform DATA (a couple line / col) into a point."
  (save-excursion
    (beginning-of-line)
    ;; goto-line
    (merlin-goto-point data)
    (point)))
(defun merlin-make-bounds (data)
  "From a json object DATA {\"start\": LOC1; \"end\": LOC2}, return (LOC1 . LOC2)."
  (cons
   (merlin-make-point (cdr (assoc 'start data)))
   (merlin-make-point (cdr (assoc 'end data)))))

(defun merlin-unmake-point (point)
  "Destruct POINT to line / col."
  (save-excursion
    (goto-char point)
    (list
      (cons 'line (line-number-at-pos nil))
      (cons 'col (current-column)))))

(defun bounds-of-ocaml-atom-at-point ()
  "Return the start and end points of an ocaml atom near point.
An ocaml atom is any string containing [a-z_0-9A-Z`.]."
  (save-excursion
    (skip-chars-backward "[a-z_0-9A-Z`.]")
    (if (looking-at "[a-z_0-9A-Z`.]+")
        (cons (point) (match-end 0)) ; returns the bounds
      nil))) ; no atom at point

(put 'ocamlatom 'bounds-of-thing-at-point
     'bounds-of-ocaml-atom-at-point)

; overlay management
(defun merlin-put-margin-overlay (overlay string &optional face)
  "Put a margin overlay inside OVERLAY, with face FACE and string STRING."
  (set-window-margins nil 1)
  (if face
      (overlay-put overlay 'face face))
  (overlay-put overlay
               'before-string 
               (propertize " " 'display 
                           `((margin left-margin) ,string)
                           )))

(defun merlin-highlight (bounds face)
  "Create an overlay on BOUNDS (of the form (START . END)) and give it FACE."
  (if merlin-highlight-overlay
      (delete-overlay merlin-highlight-overlay))
  (setq merlin-highlight-overlay (make-overlay (car bounds) (cdr bounds)))
  (overlay-put merlin-highlight-overlay 'face face)
  (unwind-protect
      (sit-for 60)
    (delete-overlay merlin-highlight-overlay)
    (setq merlin-highlight-overlay nil)))
      
;; PROCESS MANAGEMENT

(defun merlin-get-buffer-instance-name ()
  "Return the instance name of the current-projet.
For now it is a constant function (every buffer shares the same instance)."
  "")
(defun merlin-get-process ()
  "Return the process of the current buffer."
  merlin-local-process)

(defun merlin-get-process-buffer-name ()
  "Return the buffer name of the merlin process associated to the current buffer."
  (format "* merlin %s *" (merlin-get-buffer-instance-name)))

(defun merlin-get-process-variable (var)
  "Return the value of VAR (symbol) inside the process buffer."
  (when (get-buffer (merlin-get-process-buffer-name))
    (buffer-local-value var (get-buffer (merlin-get-process-buffer-name)))))


(defun merlin-get-process-name ()
  "Return the process name for the current buffer."
  (concat "merlin-" (merlin-get-buffer-instance-name))
  )

(defun merlin-start-process (flags &optional users)
  "Start the merlin process for the current buffer.
FLAGS are a list of strings denoting the parameters to be passed
to merlin. USERS can be used to set the users of this
buffer. Return the process created"
  (get-buffer-create (merlin-get-process-buffer-name))
  (let ((p (apply #'start-file-process (merlin-get-process-name)
                         (merlin-get-process-buffer-name)
                         merlin-command flags) )
        (name (buffer-name)))
    (set (make-local-variable 'merlin-local-process) p)
    (dolist (buffer users)
      (message "Setting process for buffer %s" buffer)
      (with-current-buffer buffer
        (set (make-local-variable 'merlin-local-process) p)))
    (merlin-debug (format "Running %s with flags %s\n" merlin-command flags))
    (set-process-query-on-exit-flag p nil)
    (push p merlin-processes)
; don't forget to initialize temporary variable
    (with-current-buffer (merlin-get-process-buffer-name)
      (set (make-local-variable 'merlin-queue) (tq-create p))
      (set (make-local-variable 'merlin-process-users) (cons name users))
      (set (make-local-variable 'merlin-local-process) p)
      (set (make-local-variable 'merlin-process-last-user) name)
      )
  p
  ))

(defun merlin-get-current-buffer-users ()
  "Return the list of users of the merlin instance for this buffer."
  (when (get-buffer (merlin-get-process-buffer-name))
    (with-current-buffer (merlin-get-process-buffer-name)
      merlin-process-users)))

(defun merlin-restart-process ()
  "Restart the merlin toplevel for this buffer, taking into account new flags."
  (interactive)
  (let ((users (merlin-get-current-buffer-users)))
    (if (merlin-process-started-p)
        (ignore-errors (merlin-kill-process)))
    (setq merlin-local-process (merlin-start-process merlin-current-flags users))
    (setq merlin-pending-errors nil)
    (merlin-load-project-file)
    (merlin-to-point)))
      
(defun merlin-process-clear-flags ()
  "Clear all flags set up to be passed to merlin.
This sets `merlin-current-flags' to nil."
  (interactive)
  (setq merlin-current-flags merlin-default-flags))

(defun merlin-process-add-flag (flag)
  "Add FLAG to `merlin-current-flags' to be used when starting ocamlmerlin."
  (interactive
   (list (completing-read "Flag to add: " merlin-flags-list nil nil)))
  (add-to-list 'merlin-current-flags flag)
  (message "Flag %s added. Restart ocamlmerlin by `merlin-restart-process' to take it into account." flag)
)
    
(defun merlin-process-add-user ()
  "Add the current buffer as an user for the merlin process."
  (let ((name (buffer-name)))
    (merlin-debug (format "Adding user: %s\n" name))
    (with-current-buffer (merlin-get-process-buffer-name)
      (push name merlin-process-users))
    )
)

(defun merlin-is-last-user-p ()
  "Return whether the current buffer was the current user of its merlin process."
  (equal (merlin-get-process-variable 'merlin-process-last-user)
         (buffer-name))
)

(defun merlin-process-remove-user ()
  "Remove the current buffer as an user for the merlin process.
Kill the process if required."
  (let ((name (buffer-name)))
    (with-current-buffer (merlin-get-process-buffer-name)
      (setq merlin-process-users (delete name merlin-process-users))
      (if (and (not merlin-process-users)
               merlin-automatically-garbage-processes)
          (message "Killed merlin process.")
          (merlin-kill-process))
      )
    )
)
(defun merlin-process-started-p ()
  "Return non-nil if the merlin process for the current buffer is already started."
   (get-buffer (merlin-get-process-buffer-name)))
(defun merlin-kill-process ()
  "Kill the merlin process inside the buffer."
  (setq merlin-processes (delete merlin-local-process merlin-processes))
  (process-send-eof (merlin-get-process))
  (ignore-errors (delete-process (merlin-get-process)))
  (kill-buffer (merlin-get-process-buffer-name))
  (with-current-buffer (merlin-get-process-buffer-name)
    (tq-close merlin-queue))
)
(defun merlin-wait-for-answer ()
  "Waits for merlin to answer."
  (while (not merlin-ready)
    (accept-process-output (merlin-get-process) 0.1 nil nil))
  merlin-result)
(defun merlin-send-command-async (command args callback-if-success &optional callback-if-exn)
  "Send COMMAND (with arguments ARGS) to merlin asynchronously.
Give the result to callback-if-success.  If merlin reported an
error and if CALLBACK-IF-EXN is non-nil, call the function with
the error message otherwise print a generic error message."
  (lexical-let* ((string
	 (concat 
	  (json-encode (if args (append (list command) args) (list command)))
	  "\n"))
        (buffer (current-buffer))
        (name (buffer-name)))
    (if merlin-debug (merlin-debug (format ">%s" string)))
    (with-current-buffer (merlin-get-process-buffer-name)
      (setq merlin-process-last-user name)
      (tq-enqueue merlin-queue string "\n"
                  (cons callback-if-success (cons callback-if-exn command))
                  #'(lambda (closure answer)
                      (with-current-buffer buffer
                        (setq merlin-ready t)
                        (if (>= (length answer) 10000)
                            (message "merlin: Parsing long answer (%dk)" (/ (length answer) 1000)))
                        (let ((a (ignore-errors (json-read-from-string answer))))
                          (if a
                              (progn
                                (if merlin-debug
                                    (merlin-debug (format "<%s" answer)))
                                (if (string-equal (elt a 0) "return")
                                    (funcall (car closure) (elt a 1))
                                  (progn
                                    (if (functionp (cadr closure))
                                        (funcall (cadr closure) (elt a 1))
                                      (message "Command %s failed with error %s" (cddr closure) (elt a 1))))))
                            (message "Invalid answer received from merlin.")))))
      nil))))

(defun merlin-send-command (command args &optional callback-if-exn)
  "Send COMMAND (with arguments ARGS) to merlin and returns the result."
  (setq merlin-result nil)
  (setq merlin-ready nil)
  (merlin-send-command-async command args #'(lambda (data)
                                              (setq merlin-ready t)
                                              (setq merlin-result data)) callback-if-exn)
  (merlin-wait-for-answer))

(defun merlin-is-long-p (type)
  "Return non-nil if TYPE is the signature of a module."
  (string-match "sig" type))
;; SPECIAL CASE OF COMMANDS
(defun merlin-rewind ()
  "Rewind the knowledge of merlin of the current buffer to zero."
  (interactive)
  (merlin-send-command "reset" (list "name" buffer-file-name))
  (setq merlin-lock-point (point-min))
  (merlin-delete-error-overlays)
  (setq merlin-pending-errors nil)
  (merlin-update-lock-zone-display)
)

(defun merlin-refresh ()
  "Refreshe merlin cmis."
  (interactive)
  (merlin-send-command "refresh" nil)
  (when merlin-rewind-after-refresh
    (merlin-rewind)))
  


(defun merlin-get-completion (ident)
  "Return the completion for ident IDENT."
  (merlin-send-command "complete" (list "prefix" ident "at" (merlin-unmake-point (point)))))

(defun merlin-tell-string (mode string)
  "Tell using MODE the string STRING to merlin."
  (merlin-send-command "tell" (list mode string)))

(defun merlin-flush-tell ()
  "Flush merlin teller."
  (merlin-send-command "tell" '("struct" nil)))

(defun merlin-get-position ()
  "Get the current position of merlin."
  (merlin-make-point
   (merlin-send-command "seek" '("position"))))

(defun merlin-seek (point &optional mode)
  "Seek merlin's point to POINT.
MODE is a string that can be \"exact\", \"before\" or \"end\" (default is before)."
  (let ((data 
         (if (string-equal mode "end")
             (merlin-send-command "seek" (list "end"))
           (merlin-send-command "seek"
                                (list (if mode mode "before")
                                      (merlin-unmake-point point))))))
    (merlin-make-point data)))
    
(defun merlin-tell-piece (mode start end)
  "Tell the region using mode MODE between START and END in one chunk using."
  (merlin-tell-string mode (buffer-substring start end)))

(defun merlin-tell-piece-split (mode start end)
   "Tell the region using mode MODE between START and END in several chunks (a chunk is at most 10 lines)."
   (save-excursion
     ;; tell lines 10 by 10
     (goto-char start)
     (forward-line 10)
     (let ((temp start))
       (while (< (point) end)
	 (merlin-tell-piece mode temp (point))
	 (setq temp (point))
	 (forward-line 10))
       (merlin-tell-piece mode temp end))))

(defun merlin-tell-till-end-of-phrase (orig-point view-errors-p)
  "Tell merlin the buffer until the end of the current phrase is met.
It proceeds by telling (with the end mode) each line until it
returns true or until we are at the end of the buffer.  It then
parses the error returned by merlin. If VIEW-ERRORS-P is non-nil, it reports the errors to the user.
ORIG-POINT is where the user was before starting telling merlin."
  (let ((temp-point (point))
        (end-p nil))
    (forward-line 10)
    (while (and (not end-p) (< (point) (point-max)))
      (if (equal ;; this is not tautological since value is never nil
           (merlin-tell-piece "end" temp-point (point))
           t)
          (progn
            (setq end-p t))
        (progn
            (setq temp-point (point))
            (forward-line 10))))
    ;; End of buffer
    (if (not end-p)
        (merlin-send-command "tell" '("end" nil)))
    (if view-errors-p
        (merlin-check-for-errors view-errors-p)
      (progn
        (merlin-seek orig-point))
        (merlin-check-for-errors view-errors-p)
        (merlin-seek (point) "end"))
    (setq merlin-lock-point (merlin-get-position))
    (merlin-update-lock-zone-display)))

      
      
  


;; ERRORS
(defun merlin-next-error ()
  "Jump to the next error."
  (interactive)
  (if merlin-pending-errors
      (let ((err (pop merlin-pending-errors)))
        (merlin-goto-point (cdr (assoc 'start err)))
        (if merlin-pending-errors-overlays
            (delete-overlay (pop merlin-pending-errors-overlays)))
        (if merlin-pending-errors
            (message "%s (%d more errors, use %s to go to the next)" 
                     (cdr (assoc 'message err))
                     (length merlin-pending-errors)
                     (substitute-command-keys "\\[merlin-next-error]")
                     )
          (message "%s" (cdr (assoc 'message err))))
        (merlin-highlight (merlin-make-bounds err) 'next-error))
    (next-error)))

(defun merlin-delete-error-overlays ()
  "Remove margin error overlays."
  (mapc #'delete-overlay merlin-pending-errors-overlays)
  (setq merlin-pending-errors-overlays nil))

(defun merlin-warning-p (msg)
  "Tell if the message MSG is a warning."
  (string-match "^Warning" msg))

(defun merlin-display-errors-in-margin (errors)
  "Given a list of ERRORS, put annotations in the margin corresponding to them."
  (merlin-delete-error-overlays)
  (setq merlin-pending-errors (append errors nil))
  (setq merlin-pending-errors-overlays 
        (mapcar (lambda (err)
                  (let ((overlay (make-overlay
                                  (merlin-make-point (cdr (assoc 'start err)))
                                  (merlin-make-point (cdr (assoc 'end err))))))
                    (if (merlin-warning-p (cdr (assoc 'message err)))
                        (merlin-put-margin-overlay overlay 
                                                   merlin-margin-warning-string 
                                                   compilation-warning-face)
                      (merlin-put-margin-overlay overlay 
                                                 merlin-margin-error-string
                                                 compilation-error-face))
                      overlay))
                  errors)))

(defun merlin-check-for-errors (view-errors-p)
  "Check for errors.
Return t if there were not any or nil if there were.  Moreover, it displays
the errors in the margin. If VIEW-ERRORS-P is non-nil, display a count of them."
  (let ((raw-errors (merlin-send-command "errors" nil)))
    (if (> (length raw-errors) 0)
	(progn
          (let ((errors (delete-if (lambda (e) (not (assoc 'start e)))
                                   (append raw-errors nil))))
            (if (not merlin-report-warnings)
                (delete-if (lambda (e) (merlin-warning-p (cdr (assoc 'message e)))) errors))
            (merlin-display-errors-in-margin errors)
            (when view-errors-p
              (message "(%d pending errors, use %s to jump)"
                       (length errors)
                       (substitute-command-keys "\\[merlin-next-error]")))
            nil))
      (progn
	(if view-errors-p (message "ok"))
	t))))
 

(defun merlin-retract-to (point)
  "Retract merlin's view to POINT."
  (merlin-seek point))

(defun merlin-update-lock-zone-display ()
  "Update the locked zone display, according to `merlin-display-lock-zone', ie.
iterates through it and call each method."
  (dolist (x merlin-display-lock-zone)
    (case x
      (margin (merlin-update-margin-lock-zone))
      (highlight (merlin-update-highlight-lock-zone)))))


(defun merlin-update-margin-lock-zone ()
  "Mark the position of the lock zone by a marker in the margin."
  (if merlin-lock-zone-margin-overlay
      (delete-overlay merlin-lock-zone-margin-overlay))
  (save-excursion
    (goto-char merlin-lock-point)
    (setq merlin-lock-zone-margin-overlay (make-overlay (point) (point)))
    (set-window-margins nil 1)
    (merlin-put-margin-overlay merlin-lock-zone-margin-overlay
                               merlin-margin-lock-string)))

(defun merlin-update-highlight-lock-zone ()
  "Mark the position of the lock zone by highlighting the zone."
    (if merlin-lock-zone-highlight-overlay
      (delete-overlay merlin-lock-zone-highlight-overlay))
  (setq merlin-lock-zone-highlight-overlay (make-overlay (point-min) merlin-lock-point))
  (overlay-put merlin-lock-zone-highlight-overlay 'face 'merlin-locked-face))

(defun merlin-update-point (view-errors-p)
  "Move the merlin point to around the given the current point.
It proceeds as follows:

- It retracts merlin from the point given in argument to get it
to the last phrase ending.

- It tells merlin the contents between the last phrase known to
merlin and the argument

- It continues until it finds the end of a phrase.

The parameter VIEW-ERRORS-P controls whether we should care for errors"
  (let ((orig-point (point)))
    (merlin-delete-error-overlays)
    (setq merlin-pending-errors nil)
    (if (not (merlin-is-last-user-p))
        (merlin-rewind))
    (save-excursion
      (merlin-tell-piece-split "struct" merlin-lock-point (point))
      (merlin-tell-till-end-of-phrase orig-point view-errors-p))))
  
(defun merlin-check-synchronize ()
  "If merlin point is before the end of line send everything up to the end of line."
  (interactive)
  (when (> (point-at-eol) merlin-lock-point)
    (merlin-update-point nil)))

(defun merlin-edit (start end length)
  "Retract the locked zone after an edit.
Called when an edit is made by the user."
  (if (and merlin-mode (< start merlin-lock-point))
      (progn
        (setq merlin-lock-point (merlin-retract-to (1- start)))
        (merlin-update-lock-zone-display))))

;; COMPLETION
(defun merlin-format-completion-entry (entry)
  "Format the completion entry ENTRY."
  (let ((type
         (cond
          ((member (cdr (assoc 'kind entry)) '("Module" "module"))
           ": <module>")
          ((string-equal (cdr (assoc 'kind entry)) "Type")
           (format " [%s]" (cdr (assoc 'desc entry))))
          (t
           (replace-regexp-in-string "^[^:]+:[ \n]+" "" (cdr (assoc 'desc entry)))))))
    (replace-regexp-in-string "\n" "" type)))

(defun merlin-completion-data (ident)
  "Return the data for completion of IDENT, ie. a list of pairs (NAME . TYPE)."
  (let ((prefix (merlin-compute-prefix ident))
        (data (append (merlin-get-completion ident) nil)))
    (mapcar #'(lambda (entry)
               (list (concat prefix (cdr (assoc 'name entry)))
                     (merlin-format-completion-entry entry)
                     (elt (cdr (assoc 'kind entry)) 0)))
            data)))

(defvar merlin-ac-use-summary t
  "Use :summary for the types in AC")
(defvar merlin-ac-use-document nil
  "Use :document for the types in AC")

(defun merlin-make-popup-item (data)
  "Create a popup item from data DATA."
  (popup-make-item
   (car data)
   :summary (if (and merlin-completion-types
                     merlin-ac-use-summary) (cadr data))
   :symbol (cddr data)
   :document (if (and merlin-completion-types
                      merlin-ac-use-document) (cadr data))))

(defun merlin-source-init ()
  "Initialize the cache for `auto-complete' completion.
Called at the beginning of a completion to fill the cache (the
variable `merlin-cache')."
  (merlin-check-synchronize)
  (setq merlin-completion-point ac-point)
  (setq merlin-cache
        (mapcar #'merlin-make-popup-item (merlin-completion-data ac-prefix))))

(defun merlin-try-completion ()
  "Try completing after having synchronized the point."
  (interactive)
  (merlin-check-synchronize)
  (auto-complete '(merlin-ac-source)))

(defun merlin-prefix ()
  "Retrieve the prefix for completion with merlin."
  (skip-syntax-backward "w_.")
  (point))
(defun merlin-fetch-type ()
  "Prints the type of the selected candidate"
  (let ((candidate (buffer-substring-no-properties merlin-completion-point  (point))))
    (if merlin-completion-types
        (mapc
         (lambda (item)
           (if (string-equal candidate item)
               (message "%s: %s" candidate (popup-item-summary item))))
         merlin-cache))))

(defvar merlin-ac-source
  (if merlin-ac-prefix-size
  `((init . merlin-source-init)
    (candidates . (lambda () merlin-cache))
    (action . merlin-fetch-type)
    (prefix . ,merlin-ac-prefix-size))
  '((init . merlin-source-init)
    (candidates . (lambda () merlin-cache))
    (action . merlin-fetch-type))))



(when (featurep 'auto-complete)
  (ac-define-source "merlin" merlin-ac-source))

;; Usual completion
(defun merlin-completion-lookup (string state)
  "Lookup the entry STRING inside the completion table."
  (let ((ret (assoc string merlin-completion-annotation-table)))
    (if ret
        (message "%s%s" (car ret) (cdr ret)))))
(defun merlin-completion-at-point ()
  "Perform completion at point with merlin."
  (save-excursion
    (merlin-check-synchronize))
  (save-excursion
    (skip-syntax-backward "w_.")
    (let ((start (point)))
      (skip-syntax-forward "w_.")
      (list start (point)
            (apply-partially #'merlin-completion-table start)
            . (:exit-function #'merlin-completion-lookup
              :annotation-function '(lambda (s) (cdr (assoc s merlin-completion-annotation-table))))))))
            



(defun merlin-completion-annotate (candidate)
  "Retrieve the annotation for candidate CANDIDATE in `merlin-completion-annotate-table'."
  (cdr (assoc candidate merlin-completion-annotation-table)))
(defun merlin-completion-table (start string pred action)
  "Implement completion for merlin using `completion-at-point' API."
  (if (eq 'metadata action)
      (when merlin-completion-types
        '(metadata (annotation-function . merlin-completion-annotate)))
    (unless (and merlin-completion-annotation-table
                 (eq (car merlin-completion-cache-state) start)
                 (string-prefix-p (cdr merlin-completion-cache-state)
                                  string completion-ignore-case)
                 (string-equal (merlin-compute-prefix string)
                               (merlin-compute-prefix (cdr merlin-completion-cache-state))))
      (save-excursion
        (goto-char start)
        (setq merlin--completion-annotation-table 
              (mapcar #'(lambda (a) (cons (car a) (concat ": " (cadr a)))) (merlin-completion-data string))))
      (setq merlin--completion-cache-state (cons start string)))
    (complete-with-action action merlin--completion-annotation-table string pred)))


;; Switch to ML file
(defun merlin-list-by-ext (ext)
  "List filenames ending by EXT in the path."
  (append (merlin-send-command "which" (list "with_ext" ext))
          nil))

(defun merlin-switch-to (name ext)
  "Switch to NAME.EXT."
  (let ((file
         (merlin-send-command "which" (list "path" (concat (downcase name) "." ext))
                              #'(lambda (err) (message "No such file (message: %s)" err)))))
    (when file (find-file-other-window file))))
(defun merlin-switch-to-ml (name)
  "Switch to the ML file corresponding to the module NAME."
  (interactive (list (completing-read "Module:" (merlin-list-by-ext "ml"))))
  (merlin-switch-to name "ml"))

(defun merlin-switch-to-mli (name)
  "Switch to the MLI file corresponding to the module NAME."
  (interactive (list (completing-read "Module:" (merlin-list-by-ext "mli"))))
  (merlin-switch-to name "mli"))
               
;; Get the type of an element
(defun merlin-type-of-expression-local (exp callback-if-success &optional callback-if-exn)
  "Get the type of EXP inside the local context."
  (if exp
      (merlin-send-command-async "type"
                                 (list "expression" exp "at"
                                       (merlin-unmake-point (point)))
                                 callback-if-success callback-if-exn)))

(defun merlin-type-of-expression-global (exp callback-if-success &optional callback-if-exn)
  "Get the type of EXP globally."
  (if exp
      (merlin-send-command-async "type" (list "expression" exp)
                                 callback-if-success callback-if-exn)))


(defun merlin-type-of-expression (exp callback-if-success &optional callback-if-exn)
  "Get the type of EXP.
It uses three techniques to do so:
- type-expression at (local),
- type expression (global)."
  (merlin-type-of-expression-local exp
                                   callback-if-success
                                   #'(lambda (exn)
                                       (merlin-type-of-expression-global exp
                                                                         callback-if-success
                                                                         callback-if-exn))))


(defun merlin-display-type (bounds type &optional quiet)
  "Display the type TYPE of the the expression occuring at BOUNDS.
If QUIET is non nil, then an overlay and the merlin types can be used."
  (if (not type)
      (if (not quiet)
          (message "<no information>"))
    (progn
      (if (not (merlin-is-long-p type))
          (progn 
            (message "%s" type)
            (if (and (not quiet) bounds)
                (merlin-highlight bounds 'merlin-type-face)))
        (when (not quiet)
          (display-buffer (get-buffer-create merlin-type-buffer-name))
          (with-current-buffer (get-buffer-create merlin-type-buffer-name)
            (erase-buffer)
            (insert type)))))))

(defun merlin-string-at-bounds (bounds)
  "Return the string inbetween BOUNDS."
  (if bounds
      (buffer-substring-no-properties (car bounds)
                                      (cdr bounds))))

(defun merlin-show-type (bounds &optional quiet)
  "Show the type of the expression inside BOUNDS in the current buffer.
If QUIET is non nil then an overlay is displayed and module
types are displayed in another buffer.  Otherwise only value type
are displayed, and without overlay."
  (lexical-let* ((substring (merlin-string-at-bounds bounds))
                 (bounds bounds)
                 (quiet quiet))
    (merlin-type-of-expression substring
                               #'(lambda (type)
                                   (merlin-display-type bounds type quiet)))))

(defun merlin-get-type-codomain (type)
  "Return the codomain of TYPE if it exists, or nil."
  (car (last (split-string type " " nil))))

(defun merlin-show-type-def ()
  "Print the definition of the type of the term under point."
  (interactive)
  (let* ((bounds (bounds-of-thing-at-point 'ocamlatom)))
    (merlin-type-of-expression (merlin-string-at-bounds bounds)
                               #'(lambda (type)
                                   (merlin-type-of-expression (merlin-get-type-codomain type)
                                                              #'(lambda (typedef)
                                                                  (merlin-display-type bounds
                                                                                       (if typedef typedef
                                                                                         (concat type ": <not an atomic type>")))))))))

(defun merlin-show-type-of-region ()
  "Show the type of the region."
  (interactive)
  (merlin-check-synchronize)
  (merlin-show-type (cons (region-beginning) (region-end))))
(defun merlin-show-type-of-point-quiet ()
  "Show the type of the identifier under the point if it is short (a value)."
  (merlin-check-synchronize)
  (merlin-show-type (bounds-of-thing-at-point 'ocamlatom) t))

(defun merlin-show-type-of-point (arg) 
  "Show the type of the identifier under the point. 
If it is called with a prefix argument, then show the type of the region."
  (interactive "p")
  (ignore-errors (merlin-check-synchronize))
  (if (> arg 1)
      (merlin-show-type-of-region)
    (merlin-show-type (bounds-of-thing-at-point 'ocamlatom))))

;; ENCLOSING TYPES
(defun merlin-type-enclosing ()
  "If there is a selected type enclosing, kill it.
Otherwise start a new session at point."
  (interactive)
  (let ((list
         (mapcar
          (lambda (obj)
            (cons
             (cdr (assoc 'type obj))
             (merlin-make-bounds obj)))
          (elt
           (merlin-send-command "type" (list "enclosing" (merlin-unmake-point (point))))
           1))))
    (setq merlin-enclosing-types list)
    (setq merlin-enclosing-offset -1)))

(defun merlin-magic-show-type (arg)
  "Print the type of the expression under point.
If called several times at the same position, it will print types
of bigger expressions around point (it will go up the
ast).  Called with a prefix argument, it will go down the AST.  If
there is no enclosing, falls back to
`merlin-show-type-of-point'."
  (interactive "p")
  (save-excursion
    (forward-line)
    (merlin-check-synchronize))
  (if (and merlin-enclosing-types (equal merlin-last-point-type (point)))
      (if (> arg 1)
          (merlin-type-enclosing-go-down)
        (merlin-type-enclosing-go-up))
    (if (> arg 1)
        (merlin-show-type-of-region)
      (progn
        (setq merlin-last-point-type (point))
        (merlin-type-enclosing)
        (if (not merlin-enclosing-types)
            (merlin-show-type-of-point arg)
          (merlin-type-enclosing-go-up))))))

(defun merlin-show-type-of-user-supplied-expression (s)
  "Show the type of the expression S."
  (interactive "sExpression:")
  (merlin-type-of-expression s #'(lambda (type)
                                   (merlin-display-type nil s nil))))

(defun merlin-type-enclosing-go ()
  "Highlight the given corresponding enclosing data (of the form (TYPE . BOUNDS)."
  (let ((data (elt merlin-enclosing-types merlin-enclosing-offset)))
    (if (cddr data)
        (merlin-display-type (cdr data) (car data)))))

(defun merlin-type-enclosing-go-up ()
  "Go up in the enclosing type zipper."
  (interactive)
  (if merlin-enclosing-types
      (if (> merlin-enclosing-offset (length merlin-enclosing-types))
          (message "cannot go up")
        (progn
          (setq merlin-enclosing-offset (+ 1 merlin-enclosing-offset))
          (merlin-type-enclosing-go)
          ))))

(defun merlin-type-enclosing-go-down ()
  "Go down in the enclosing type zipper."
  (interactive)
  (if merlin-enclosing-types
      (if (<= merlin-enclosing-offset 0)
          (message "cannot go down")
        (progn
          (setq merlin-enclosing-offset (- merlin-enclosing-offset 1))
          (merlin-type-enclosing-go)
          ))))

;; .merlin parsing
(defun merlin-add-path (kind dir dirname)
  "Add an item to the path given by KIND in merlin."
  (merlin-send-command "path" (list "add" kind (concat dirname dir))))

(defun merlin-get-packages ()
  "Get the list of available findlib package."
  (append (merlin-send-command "find" '("list")) nil))
(defun merlin-use (pkg)
  "Use PKG in the current session of merlin."
  (interactive
   (list (completing-read "Package to use:" (merlin-get-packages))))
  (merlin-send-command "find" (list "use" (list pkg)))
  (merlin-rewind))

(defun merlin-load-project-file ()
  "Load the .merlin file corresponding to the current file."
  (interactive)
  (merlin-rewind)
  (let ((r (merlin-send-command "project" (list "find" (buffer-file-name)))))
    (if (and r (listp r))
        (setq merlin-project-file (car r)))))

(defun merlin-goto-project-file ()
  "Goto the merlin file corresponding to the current file."
  (interactive)
  (let ((file merlin-project-file))
    (if file
        (find-file-other-window file)
      (message "No project file for the current buffer."))))
;; Locate
(defun merlin-locate-pure (ident &optional point just-open)
  "Locate the identifier IDENT at POINT.
If POINT is nil, the current point is used.  If JUST-OPEN is
non-nil, don't move to the opened buffer."
  (merlin-check-synchronize)
  (let* ((point (if point point (point)))
         (r (merlin-send-command "locate" (list ident "at" (merlin-unmake-point (point))))))
    (if (and r (listp r))
        (if just-open
            (save-excursion
              (save-selected-window
                (merlin-goto-file-and-point r just-open)
                (merlin-highlight (bounds-of-thing-at-point 'ocamlatom) 'merlin-type-face)))
          (progn
            (merlin-goto-file-and-point r)
            (push (cons (buffer-name) (point)) merlin-position-stack)
            (message "Use %s to go back."
                     (substitute-command-keys "\\[merlin-pop-stack]"))))
          (message "%s not found." ident))))

(defun merlin-locate ()
  "Locate the identifier under point"
  (interactive)
  (merlin-locate-pure (thing-at-point 'ocamlatom)))

;; I don't like it beginning by "ac" but
;; it is the only way I found to get it working (otherwise the completion
;; menu just closes itself)
(defun ac-merlin-locate ()
  "Locate the identifier currently selected in the ac-completion."
  (interactive)
  (when (ac-menu-live-p)
    (when (popup-hidden-p ac-menu)
      (ac-show-menu))
    (merlin-locate-pure (popup-selected-item ac-menu) (point) t)
    (ac-show-menu)))

(defun merlin-pop-stack ()
  "Go back to the last position where the user did a locate."
  (interactive)
  (let ((r (pop merlin-position-stack)))
    (if r
        (progn
          (select-window (display-buffer (car r)))
          (goto-char (cdr r)))

      (message "empty stack"))))
    

;; Semantic movement
(defun merlin-goto-phrase (command indice)
  "Go to the phrase indicated by COMMAND to the end INDICE.
Returns the position."
  (let ((r (merlin-send-command "boundary" (list command "at" (merlin-unmake-point (point))))))
    (when r
      (goto-char (merlin-make-point (elt r indice)))
      (merlin-make-point (elt r indice)))))

(defun merlin-next-phrase ()
  "Go to the beginning of the next phrase."
  (interactive)
  (save-excursion
    (merlin-check-synchronize)
    (merlin-goto-phrase "current" 1)
    (forward-line 1)
    (merlin-check-synchronize))
  (if (not (merlin-goto-phrase "next" 0)) ;; no next phrase => end-of-buffer
      (goto-char (point-max))))

(defun merlin-prev-phrase ()
  "Go to the beginning of the previous phrase."
  (interactive)
  (let ((point (point)))
    (merlin-check-synchronize)
    (if (equal point (merlin-goto-phrase "current" 0))
        (merlin-goto-phrase "prev" 0))))

(defun merlin-to-point ()
  "Update the merlin to the current point, reporting error."
  (interactive)
  (merlin-update-point t))

(defun merlin-customize ()
  "Open the customize buffer for the group merlin."
  (interactive)
  (customize-group 'merlin))
;; Mode definition
(defvar merlin-mode-map
  (let ((merlin-map (make-sparse-keymap))
        (merlin-menu-map (make-sparse-keymap))
        (merlin-show-type-map (make-sparse-keymap)))
    (define-key merlin-map (kbd "C-c <C-return>") 'merlin-to-point)
    (define-key merlin-map (kbd "C-c C-t") 'merlin-magic-show-type)
    (define-key merlin-map (kbd "C-c d") 'merlin-show-type-def)
    (define-key merlin-map (kbd "C-c l") 'merlin-use)
    (define-key merlin-map (kbd "C-c r") 'merlin-restart-process)
    (define-key merlin-map (kbd "C-c C-x") 'merlin-next-error)
    (define-key merlin-map (kbd "C-c C-l") 'merlin-locate)
    (when (featurep 'auto-complete)
      (define-key ac-complete-mode-map (kbd "C-c C-l") 'ac-merlin-locate))
    (define-key merlin-map (kbd "C-c &") 'merlin-pop-stack)
    (define-key merlin-map (kbd "C-c C-r") 'merlin-rewind)
    (define-key merlin-map (kbd "C-c C-u") 'merlin-refresh)
    (define-key merlin-map (kbd "C-c TAB") 'merlin-try-completion)
    (define-key merlin-map (kbd "C-c C-u") 'merlin-refresh)
    (define-key merlin-map (kbd "C-c C-f <C-return>") 'merlin-type-enclosing)
    (define-key merlin-map (kbd "C-c C-f C-<up>") 'merlin-type-enclosing-go-up)
    (define-key merlin-map (kbd "C-c C-f C-<down>") 'merlin-type-enclosing-go-down)
    (define-key merlin-map (kbd "C-c C-n") 'merlin-next-phrase)
    (define-key merlin-map (kbd "C-c C-p") 'merlin-prev-phrase)
    (define-key merlin-menu-map [customize]
      '("Customize merlin-mode" . merlin-customize))
    (define-key merlin-menu-map [separator]
      '("-"))
    (define-key merlin-show-type-map [local]
      '(menu-item "around the cursor" merlin-magic-show-type
                  :help "Show the type of the smallest subexpression near cursor"))
    (define-key merlin-show-type-map [region]
      '(menu-item "of the region" merlin-show-type-of-region
                  :help "Show the type of the region"))
    (define-key merlin-show-type-map [exp]
      '(menu-item "of an expression" merlin-show-type-of-user-supplied-expression
                  :help "Input an expression and show its type"))
    (define-key merlin-show-type-map [def]
      '(menu-item "definition" merlin-show-type-def
                  :help "Show the definition of the type of the expression near point"))
    (define-key merlin-menu-map [showtype]
      (cons "Show type..." merlin-show-type-map))
    (define-key merlin-menu-map [point]
      '(menu-item "Update point" merlin-to-point
                  :help "Updates the part of the buffer merlin knows about"))
    (define-key merlin-menu-map [rewind]
      '(menu-item "Rewind" merlin-rewind
                  :help "Rewind merlin to the beginning of the buffer"))
    (define-key merlin-menu-map [use]
      '(menu-item "Use a package" merlin-use
                  :help "Use a findlib package."))
    (define-key merlin-menu-map [refresh]
      '(menu-item "Refresh" merlin-refresh
                  :help "Refresh the cache of merlin (cmis in particular). Useful after a recompilation."))
    (define-key merlin-menu-map [addflag]
      '(menu-item "Add a flag" merlin-process-add-flag
                  :help "Add a flag to be passed to ocamlmerlin after restarting it."))
    (define-key merlin-menu-map [clearflag]
      '(menu-item "Clear flags" merlin-process-clear-flags
                  :help "Clear all flags set up to be passed to ocamlmerlin."))
    (define-key merlin-menu-map [restartmerlin]
      '(menu-item "Restart merlin" merlin-restart-process
                  :help "Restart merlin for the current buffer."))
    (define-key merlin-map [menu-bar merlin] (cons "merlin" merlin-menu-map))
    merlin-map
    ))

(defun merlin-setup ()
  "Set up a buffer for use with merlin."
  (interactive)
  (set (make-local-variable 'merlin-lock-point) (point-min))
  (set (make-local-variable 'merlin-buffer) nil)
  (set (make-local-variable 'merlin-result) nil)
  (set (make-local-variable 'merlin-completion-point) nil)
  (set (make-local-variable 'merlin-ready) nil)
  (set (make-local-variable 'merlin-pending-errors) nil)
  (set (make-local-variable 'merlin-pending-errors-overlays) nil)
  (set (make-local-variable 'merlin-lock-zone-highlight-overlay) nil)
  (set (make-local-variable 'merlin-lock-zone-margin-overlay) nil)
  (set (make-local-variable 'merlin-project-file) nil)
  (set (make-local-variable 'merlin-enclosing-types) nil)
  (set (make-local-variable 'merlin-enclosing-offset) nil)
  (set (make-local-variable 'merlin-last-point-type) nil)
  ; if there is not yet a buffer for the current buffer, create one
  (when (not (merlin-process-started-p))
      (merlin-start-process nil))
  (set (make-local-variable 'merlin-local-process)
       (merlin-get-process-variable 'merlin-local-process))
  (merlin-process-add-user)

  (when (and (fboundp 'auto-complete-mode)
             merlin-use-auto-complete-mode)
    (auto-complete-mode 1)
    (add-to-list 'ac-sources 'merlin-ac-source))
  (add-hook 'completion-at-point-functions
            #'merlin-completion-at-point nil 'local)
  (add-to-list 'after-change-functions 'merlin-edit)
  (merlin-load-project-file)
  (with-current-buffer (get-buffer-create merlin-type-buffer-name)
    (funcall merlin-favourite-caml-mode)))

(defun merlin-is-ml-buffer ()
  "Return true if current buffer corresponds to a ML file."
  (and
   (buffer-file-name)
   (equal (file-name-extension (buffer-file-name))
          "ml")))
  
;;;###autoload
(define-minor-mode merlin-mode
  "Minor mode for interacting with a merlin process.
Runs a merlin process in the background (one for all merlin
buffers) and perform queries on it.

Short cuts:
\\{merlin-mode-map}"
  nil " merlin"
  :keymap merlin-mode-map
  (if merlin-mode 
      (if (merlin-is-ml-buffer) 
          (merlin-setup)
        (progn
          (if (buffer-file-name)
              (message "merlin can only operate on ml files")
              nil)
          (merlin-mode -1)))
    (when (merlin-is-ml-buffer)
      (if merlin-lock-zone-highlight-overlay
          (delete-overlay merlin-lock-zone-highlight-overlay))
      (if merlin-lock-zone-margin-overlay
          (delete-overlay merlin-lock-zone-margin-overlay))
      (if merlin-highlight-overlay
          (delete-overlay merlin-highlight-overlay))
      (merlin-delete-error-overlays)
      (merlin-process-remove-user)
)))

(defun merlin-kill-buffer-hook ()
  "Cleans the buffer being killed."
  (if merlin-mode
      (merlin-mode -1)))
  
(defun merlin-insinuate ()
  "Initialize merlin."
  (add-hook 'kill-buffer-hook 'merlin-kill-buffer-hook))
                                      
(defun merlin-kill-all-processes ()
  "Kill all the remaining buffers containing merlin processes."
  (interactive)
  (mapc (lambda (p)
          (with-current-buffer (process-buffer p)
            (merlin-kill-process)))
        merlin-processes))

(merlin-insinuate)

(provide 'merlin)
;;; merlin.el ends here

(setq tab-always-indent 'complete)
