;;; merlin.el --- Mode for Merlin, an assistant for OCaml.   -*- coding: utf-8 -*-
;; Licensed under the MIT license.

;; Author: Simon Castellan <simon.castellan(_)iuwt.fr>
;;         Frédéric Bour <frederic.bour(_)lakaban.net>
;;         Thomas Refis <thomas.refis(_)gmail.com>
;; Created: 18 April 2013
;; Version: 1.4
;; Keywords: ocaml languages
;; URL: http://github.com/the-lambda-church/merlin

;;; Commentary:
;; Description:
;; merlin-mode is an Emacs interface to merlin.  It allows you to perform
;; queries such as getting the type of an expression, completion, and so on.

;; Installation:
;; You need merlin installed on your system (ocamlmerlin binary) for merlin-mode
;; to work.

;;; Usage:
;; merlin-mode allows you to send merlin commands from within an Emacs buffer to
;; use merlin capabilities.  A bit like proofgeneral, you need to tell merlin
;; about your code (with merlin-to-point), and then you can perform query to
;; know the type of an expression: merlin will fetch it in the typed tree
;; generated on the fly.

;;;; Code:

;; tq and cl are mandatory
(require 'tq)
(require 'cl)
;; auto-complete is not
(require 'auto-complete nil 'noerror)

;; silence free variable warning
(defvar merlin-mode)

(defgroup merlin nil
  "merlin binding mode allowing completion and typing in OCaml files."
  :group 'languages)

;;
;; Faces
;;

(defface merlin-locked-face
  '((((background dark)) :background "#222278")
    (t :background "#eaf8ff"))
  "Face for a region that merlin knows of."
  :group 'merlin)

(defface merlin-type-face
  '((t :inherit caml-types-expr-face))
  "Face for highlighting a typed expr."
  :group 'merlin)

(defface merlin-compilation-warning-face
  '((t :inherit compilation-error))
  "If non-nil, face to use to highlight merlin warnings."
  :group 'merlin)

(defface merlin-compilation-error-face
  '((t :inherit compilation-warning))
  "If non-nil, face to use to highlight merlin errors."
  :group 'merlin)

;;
;; Customizable vars
;;

(defcustom merlin-show-instance-in-lighter t
  "Show the current instance of the buffer in the lighter."
  :group 'merlin :type 'boolean)

(defcustom merlin-grouping-function 'merlin-one-group
  "The function to know how to group buffers. This function takes
no argument and should return the configuration (see
`merlin-start-process') for the current buffer."
  :group 'merlin :type 'symbol)
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

(defcustom merlin-buffer-name "*merlin*"
  "The name of the buffer storing module signatures."
  :group 'merlin :type 'string)

(defcustom merlin-occurences-buffer-name "*merlin-occurences*"
  "The name of the buffer listing occurences of an identifier after a call to `merlin-occurences'."
  :group 'merlin :type 'string)

(defcustom merlin-type-buffer-name "*merlin-types*"
  "The name of the buffer storing module signatures."
  :group 'merlin :type 'string)

(defcustom merlin-favourite-caml-mode 'tuareg-mode
  "The OCaml mode to use for the *merlin-types* buffer."
  :group 'merlin :type 'symbol)

(defcustom merlin-margin-lock-string "-"
  "String put in the margin to signal the end of the locked zone."
  :group 'merlin :type 'string)

(defcustom merlin-margin-error-string "!"
  "String to put in the margin of a line containing an error."
  :group 'merlin :type 'string)

(defcustom merlin-margin-warning-string "?"
  "String to put in the margin of a line containing a warning."
  :group 'merlin :type 'string)

(defcustom merlin-error-after-save t
  "If non-nil, check for errors after saving"
  :group 'merlin :type 'boolean)

(defcustom merlin-error-in-margin t
  "If non-nil, display errors in margin"
  :group 'merlin :type 'boolean)

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

(defcustom merlin-use-auto-complete-mode nil
  "If non nil, use `auto-complete-mode' in any buffer."
  :group 'merlin :type 'boolean)

(defcustom merlin-ac-prefix-size nil
  "If non-nil, specify the minimum number of characters to wait before allowing auto-complete"
  :group 'merlin :type 'boolean)

(defcustom merlin-occurences-show-buffer 'other
  "Determine how to display the occurences list after a call to `merlin-occurences'."
  :group 'merlin :type '(choice (const :tag "Don't show list" never)
                                (const :tag "Show in the current window" same)
                                (const :tag "Show in another window" other)))

(defcustom merlin-locate-in-new-window 'diff
  "Determine whether to display results of `merlin-locate' in a new window or not."
  :group 'merlin :type '(choice (const :tag "Always open a new window" always)
                                (const :tag "Never open a new window" never)
                                (const :tag "Open a new window only if the target file is different from current buffer." diff)))

(defcustom merlin-locate-focus-new-window t
  "If non-nil, when locate opens a new window it will give it the focus."
  :group 'merlin :type 'boolean)

;;;;;;;;;;;;;;;;;;;;;;;;
;; Internal variables ;;
;;;;;;;;;;;;;;;;;;;;;;;;


;; merlin flags
(defvar merlin-flags-list '("-rectypes" "-nostdlib" "-absname" "-w" )
  "List of flags that can be passed to ocamlmerlin.")

(defvar merlin-current-flags merlin-default-flags
  "The current list of flags to pass to ocamlmerlin.")

;; Process / Reception related variables
(defvar merlin-process nil
  "The merlin process for this buffer (only valid in a process buffer).")
(make-variable-buffer-local 'merlin-process)

(defvar merlin-instance nil
  "The name of the merlin instance for this buffer.")
(make-variable-buffer-local 'merlin-instance)

(defvar merlin-process-queue nil
  "The transaction queue for the local process (only valid in a process buffer).")
(make-variable-buffer-local 'merlin-process-queue)

(defvar merlin-process-last-user nil
  "Last buffer that used the local process (only valid in a process buffer).")
(make-variable-buffer-local 'merlin-process-last-user)

(defvar merlin-result nil
  "Temporary variables to store command results.")
(make-variable-buffer-local 'merlin-result)

(defvar merlin-buffer nil
  "Buffer for merlin input.")
(make-variable-buffer-local 'merlin-buffer)

(defvar merlin-ready nil
  "If non-nil, the reception is done.")
(make-variable-buffer-local 'merlin-ready)

(defvar merlin-dirty-point 0
  "Position after which buffer content may differ.")
(make-variable-buffer-local 'merlin-dirty-point)

;; Overlays
(defvar merlin-lock-zone-highlight-overlay nil
  "Overlay used for the lock zone highlighting.")
(make-variable-buffer-local 'merlin-lock-zone-highlight-overlay)

(defvar merlin-lock-zone-margin-overlay nil
  "Overlay used for the margin indicator of the lock zone.")
(make-variable-buffer-local 'merlin-lock-zone-margin-overlay)

;; Errors related variables
(defvar merlin-pending-errors nil
  "Pending errors.")
(make-variable-buffer-local 'merlin-pending-errors)

(defvar merlin-pending-errors-overlays nil
  "Overlays for the pending errors.")
(make-variable-buffer-local 'merlin-pending-errors-overlays)

(defvar merlin-highlight-overlay nil
  "Merlin overlay used for highlights.")

;; Completion related variables
(defvar merlin-completion-point nil
  "Stores the point of last completion (beginning of the prefix).")
(make-variable-buffer-local 'merlin-completion-point)

; Vars from auto-complete
(defvar ac-point)
(defvar ac-prefix)
(defvar ac-sources)
(defvar merlin-completion-annotation-table nil
  "Hold a table mapping completion candidates to their types.")
(make-variable-buffer-local 'merlin-completion-annotation-table)
(defvar merlin-ac-cache nil
  "Hold a table mapping completion cache for auto-complete.")
(make-variable-buffer-local 'merlin-ac-cache)

(defvar merlin-completion-at-point-cache-query (cons "" 0)
  "The cache for calls to completion-at-point so that it does not
trigger useless merlin calls.")
(make-variable-buffer-local 'merlin-completion-at-point-cache-query)


;; Type related variables
(defvar merlin-enclosing-types nil
  "List containing the enclosing type.")
(make-variable-buffer-local 'merlin-enclosing-types)
(defvar merlin-enclosing-offset nil
  "Current offset in `merlin-enclosing-types'.")
(make-variable-buffer-local 'merlin-enclosing-offset)

;; Locate
(defvar merlin-position-stack nil)

;; Misc
(defvar merlin-project-file nil
  "The .merlin file for current buffer.")
(make-variable-buffer-local 'merlin-project-file)


;;;;;;;;;;;
;; UTILS ;;
;;;;;;;;;;;

(defun merlin-debug (s)
  "Output S if the variable `merlin-debug' is non-nil on the process buffer
associated to the current buffer."
  (with-current-buffer (merlin-process-buffer)
    (goto-char (point-max))
    (insert s)))

(defun merlin-get-occ-buff ()
  (get-buffer-create merlin-occurences-buffer-name))

(defun merlin-goto-point (data)
  "Go to the point indicated by `DATA' which must be an assoc list with fields
line and col"
  (goto-char (point-min))
  (forward-line (1- (lookup-default 'line data 0)))
  (move-to-column (max 0 (lookup-default 'col data 0))))

(defun merlin-goto-file-and-point (data)
  "Go to the file and position indicated by DATA which is an assoc list
containing fields file, line and col."
  (let* ((file (assoc 'file data))
         (open-window (cond ((equal merlin-locate-in-new-window 'never) nil)
                            ((equal merlin-locate-in-new-window 'always))
                            (file)))
         (filename (if file (cdr file) (buffer-file-name)))
         (focus-window (or (not open-window) merlin-locate-focus-new-window))
         (do-open (lambda ()
                    (if open-window
                      (find-file-other-window filename)
                      (find-file filename))
                    (merlin-goto-point (cdr (assoc 'pos data))))))
    (if focus-window
        (progn
          (push (cons (buffer-name) (point)) merlin-position-stack)
          (funcall do-open)
          (message "Use %s to go back."
                   (substitute-command-keys "\\[merlin-pop-stack]")))
      (save-excursion (save-selected-window (funcall do-open))))))

(defun merlin-make-point (data)
  "Transform DATA (a remote merlin position) into a point."
  (save-excursion
    (merlin-goto-point data)
    (point)))

(defun merlin-make-bounds (data)
  "From a remote merlin object DATA {\"start\": LOC1; \"end\": LOC2},
return (LOC1 . LOC2)."
  (cons
   (merlin-make-point (cdr (assoc 'start data)))
   (merlin-make-point (cdr (assoc 'end data)))))

(defun merlin-unmake-point (point)
  "Destruct POINT to line / col."
  (save-excursion (goto-char point)
                  (list (cons 'assoc nil)
                        (cons 'line (line-number-at-pos nil))
                        (cons 'col (current-column)))))

(defun bounds-of-ocaml-atom-at-point ()
  "Return the start and end points of an ocaml atom near point.
An ocaml atom is any string containing [a-z_0-9A-Z`.]."
  (save-excursion
    (skip-chars-backward "[a-z_0-9A-Z'`.]")
    (if (looking-at "['a-z_0-9A-Z`.]*['a-z._A-Z0-9]")
        (cons (point) (match-end 0)) ; returns the bounds
      nil))) ; no atom at point

(put 'ocaml-atom 'bounds-of-thing-at-point
     'bounds-of-ocaml-atom-at-point)

; overlay management
(defun merlin-put-margin-overlay (overlay string &optional face)
  "Put a margin overlay inside OVERLAY, with face FACE and string STRING."
  (set-window-margins nil 1)
  (when face (overlay-put overlay 'face face))
  (overlay-put overlay 'before-string
               (propertize " " 'display `((margin left-margin) ,string))))

(defun merlin-highlight (bounds face)
  "Create an overlay on BOUNDS (of the form (START . END)) and give it FACE."
  (when merlin-highlight-overlay
    (delete-overlay merlin-highlight-overlay))
  (setq merlin-highlight-overlay (make-overlay (car bounds) (cdr bounds)))
  (overlay-put merlin-highlight-overlay 'face face)
  (unwind-protect (sit-for 60)
    (delete-overlay merlin-highlight-overlay)
    (setq merlin-highlight-overlay nil)))

;;;;;;;;;;;;;;;;;;;;;;;;
;; PROCESS MANAGEMENT ;;
;;;;;;;;;;;;;;;;;;;;;;;;

(defun lookup-default (key list &optional default)
  "Lookup KEY in LIST which a list of pairs. If it not found,
return DEFAULT or the value associated to KEY otherwise."
  (let ((v (assoc key list)))
    (if v (cdr v)
      default)))


(defun merlin-instance-buffer-name (name)
  "Return the buffer name corresponding to the merlin instance NAME."
  (format "*merlin (%s)*" name))

(defun merlin-process-buffer ()
  "Return the process buffer of the current buffer."
  (get-buffer (merlin-instance-buffer-name merlin-instance)))

(defun merlin-process ()
  "Return the process of the current buffer."
  (buffer-local-value 'merlin-process (merlin-process-buffer)))

(defun merlin-last-user ()
  "Return the last user of the process of the current buffer."
  (buffer-local-value 'merlin-process-last-user (merlin-process-buffer)))

(defun merlin-start-process (flags &optional configuration)
  "Start the merlin process by fetching the information inside CONFIGURATION. FLAGS contains the list of flags to give merlin.
   CONFIGURATION is an association list with the following keys:
- `extra-flags': extra flags to give merlin

- `command': command to run

- `env': list of strings (of the shape VARIABLE=FOO) (see
`process-environment') that will be prepended to the environment of merlin

- `name': the name of the instance."
  (let* ((command (lookup-default 'command configuration merlin-command))
        (extra-flags (lookup-default 'flags configuration nil))
        (name (lookup-default 'name configuration "default"))
        (environment (lookup-default 'env configuration nil))
        (buffer-name (merlin-instance-buffer-name name)))
    (message "Starting merlin instance: %s (binary=%s)." name command)
    (setq merlin-instance name)
    (when (not (merlin-process-started-p name))
      (let* ((buffer (get-buffer-create buffer-name))
             (process-environment (append environment process-environment))
             (p (apply #'start-file-process "merlin" buffer-name
                       command `("-protocol" "sexp" . ,(append extra-flags flags)))))
        (with-current-buffer buffer
          (set-process-query-on-exit-flag p nil)
          (setq merlin-process p)
          (setq merlin-instance name)
          (setq merlin-process-queue (tq-create p)))
        p))))

(defun merlin-toggle-view-errors ()
  "Toggle the viewing of errors in the buffer."
  (interactive)
  (setq merlin-error-after-save (not merlin-error-after-save))
  (if merlin-error-after-save
      (progn
        (merlin-rewind)
        (merlin-after-save)
        (message "Errors are now reported. Use %s to stop reporting them."
                 (substitute-command-keys "\\[merlin-toggle-view-errors]")))
    (progn
      (merlin-error-reset)
      (message "Errors are not reported anymore. Use %s to start again reporting them."
               (substitute-command-keys "\\[merlin-toggle-view-errors]")))))

(defun merlin-restart-process ()
  "Restart the merlin toplevel for this buffer, taking into account new flags."
  (interactive)
  (when (get-buffer (merlin-process-buffer))
    (ignore-errors (merlin-kill-process)))
  (merlin-start-process merlin-current-flags (funcall merlin-grouping-function))
  (setq merlin-pending-errors nil)
  (merlin-load-project-file)
  (merlin-to-point))

(defun merlin-process-clear-flags ()
  "Clear all flags set up to be passed to merlin.
This sets `merlin-current-flags' to nil."
  (interactive)
  (setq merlin-current-flags merlin-default-flags))

(defun merlin-process-add-flag (flag-string)
  "Add FLAG to `merlin-current-flags' to be used when starting ocamlmerlin."
  (interactive "sFlag to add: ")
  (let* ((flags     (split-string flag-string))
         (flag-list (append flags merlin-current-flags)))
    (setq merlin-current-flags flag-list))
  (message "Flag %s added.  Restart ocamlmerlin by `merlin-restart-process' to take it into account." flag-string))

(defun merlin-list-instances ()
  "Return the list of instances currently started."
  (let ((result nil))
    (dolist (buffer (buffer-list))
      (with-current-buffer buffer
        (if merlin-process
            (push merlin-instance result))))
    result))

(defun merlin-kill-instance (instance)
  (interactive (list (ido-completing-read "Instance:" (merlin-list-instances))))
  (message "Instance: %s" instance)
  (merlin-kill-process (get-buffer (merlin-instance-buffer-name instance))))

(defun merlin-is-last-user-p ()
  "Return whether the current buffer was the current user of the merlin process."
  (equal (merlin-last-user) (buffer-name)))

(defun merlin-process-started-p (name)
  "Return non-nil if the merlin process for the instance NAME is already started."
  (get-buffer (merlin-instance-buffer-name name)))

(defun merlin-kill-process (&optional buffer)
  "Kill the merlin process inside BUFFER. If BUFFER is nil, use
the merlin buffer of the current buffer."
  (let ((buffer (if buffer buffer (merlin-process-buffer))))
    (with-current-buffer buffer
      (tq-close merlin-process-queue)
      (ignore-errors (kill-process merlin-process)))
    (kill-buffer buffer)))

(defun merlin-wait-for-answer ()
  "Waits for merlin to answer."
  (while (not merlin-ready)
    (accept-process-output (merlin-process) 0.1 nil t))
  merlin-result)

(defun merlin-send-command-async (command callback-if-success &optional callback-if-exn)
  "Send COMMAND (with arguments ARGS) to merlin asynchronously.
Give the result to callback-if-success.  If merlin reported an
error and if CALLBACK-IF-EXN is non-nil, call the function with
the error message otherwise print a generic error message."
  (lexical-let*
      ((string (concat (prin1-to-string (if (listp command) command (list command)))
                        "\n"))
       (buffer (current-buffer))
       (name (buffer-name)))
    (if (not (equal (process-status (merlin-process)) 'run))
        (progn
          (error "Merlin process not running (try restarting with %s)"
                 (substitute-command-keys "\\[merlin-restart-process]"))
          nil)
      (with-current-buffer (merlin-process-buffer)
        (if merlin-debug (merlin-debug (format ">%s" string)))
        (setq merlin-process-last-user name)
        (tq-enqueue merlin-process-queue string "\n"
                    (cons callback-if-success (cons callback-if-exn command))
                    #'(lambda (closure answer)
                        (with-current-buffer buffer
                          (setq merlin-ready t)
                          (if merlin-debug (merlin-debug (format "<%s" answer)))
                          (let ((a (car (read-from-string answer))))
                            (if a
                                (cond ((string-equal (elt a 0) "return")
                                       (funcall (car closure) (elt a 1)))
                                      ((string-equal (elt a 0) "exception")
                                       (message "Merlin failed with exception: %s" (elt a 1)))
                                      ((progn
                                         (if (functionp (cadr closure))
                                             (funcall (cadr closure) (elt a 1))
                                           (message "Command %s failed with error %s" (cddr closure) (elt a 1))))))
                              (message "Invalid answer received from merlin.")))))
                    nil)
        nil)
      t)))

(defun merlin-send-command (command &optional callback-if-exn)
  "Send COMMAND (with arguments ARGS) to merlin and returns the result."
  (setq merlin-result nil)
  (setq merlin-ready nil)
  (when (merlin-send-command-async command
                                   (lambda (data)
                                     (setq merlin-ready t)
                                     (setq merlin-result data))
                                   callback-if-exn)
    (merlin-wait-for-answer)))

;; SPECIAL CASE OF COMMANDS

(defun merlin-rewind ()
  "Rewind the knowledge of merlin of the current buffer to zero."
  (interactive)
  (let ((ext (if (buffer-file-name)
                 (file-name-extension (buffer-file-name))
               "ml")))
    (merlin-send-command
     `(reset ,(if (string-equal ext "mli") 'mli 'ml) ,(buffer-file-name)))
    (merlin-error-reset)
    (setq merlin-dirty-point (point-min))))

(defun merlin-refresh ()
  "Refresh changed merlin cmis."
  (interactive)
  (merlin-send-command 'refresh)
  (merlin-after-save))

(defun merlin-get-completion (ident)
  "Return the completion for ident IDENT."
  (merlin-send-command
    `(complete prefix ,ident at ,(merlin-unmake-point (- (point) (length ident))))))

(defun merlin-parse-position (result)
  "Returns a pair whose first member is a point set at merlin cursor position
  and second member is the state of the marker"
  (cons (merlin-make-point (lookup-default 'cursor result nil))
        (when (equal (lookup-default 'marker result nil) 'true) t)))

(defun merlin-send-cursor-command (command &optional callback-if-exn)
  "Send COMMAND (with arguments ARGS) to merlin and returns the result parsed
  as a position."
  (merlin-parse-position (merlin-send-command command callback-if-exn)))


(defun merlin-get-position ()
  "Get the current position of merlin."
  (car (merlin-send-cursor-command '(seek position))))

(defun merlin-seek-before (point)
  "Move merlin's point to the valid definition before POINT."
  (merlin-send-cursor-command
    `(seek before ,(merlin-unmake-point point))))

(defun merlin-seek-exact (point)
  "Move merlin's point to the definition containing POINT."
  (merlin-send-cursor-command
    `(seek exact ,(merlin-unmake-point point))))

(defun merlin-seek-end ()
  "Move merlin's point to the end of its own view of the buffer."
  (merlin-send-cursor-command '(seek end)))

;;;;;;;;;;;;;;;;;;;;
;; FILE SWITCHING ;;
;;;;;;;;;;;;;;;;;;;;

(defun merlin-switch-list-by-ext (ext)
  "List filenames ending by EXT in the path."
  (append (merlin-send-command `(which with_ext ,ext)) nil))

(defun merlin-switch-to (name ext)
  "Switch to NAME.EXT."
  (let ((file (merlin-send-command
               `(which path ,(concat name ext))
               #'(lambda (err) (message "No such file (message: %s)" err)))))
    (when file (find-file-other-window file))))

(defun merlin-switch-to-ml (name)
  "Switch to the ML file corresponding to the module NAME."
  (interactive (list (completing-read "Module: " (merlin-switch-list-by-ext ".ml"))))
  (merlin-switch-to name ".ml"))

(defun merlin-switch-to-mli (name)
  "Switch to the MLI file corresponding to the module NAME."
  (interactive (list (completing-read "Module: " (merlin-switch-list-by-ext ".mli"))))
  (merlin-switch-to name ".mli"))

;;;;;;;;;;;;;;;;;;;;;;;;;
;; EXTENSION SELECTION ;;
;;;;;;;;;;;;;;;;;;;;;;;;;

(defun merlin-extension-enable (name)
  "Enable merlin support for language extension NAME."
  (interactive (list (completing-read "Extension: " (merlin-send-command '(extension list disabled)))))
  (merlin-send-command `(extension enable (,name)))
  (merlin-error-reset))

(defun merlin-extension-disable (name)
  "Disable merlin support for language extension NAME."
  (interactive (list (completing-read "Extension: " (merlin-send-command '(extension list enabled)))))
  (merlin-send-command `(extension disable (,name)))
  (merlin-error-reset))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; BUFFER SYNCHRONIZATION ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun merlin-buffer-substring (start end)
   "Return content of buffer between two points or empty string if points are not valid"
   (if (< start end) (buffer-substring-no-properties start end) ""))

(defun merlin-tell-source (code)
  "Tell CODE to merlin and returns whether the position if marker is still active."
  (let ((result (merlin-send-cursor-command `(tell source ,code))))
     (when (cdr result) (car result))))

(defun merlin-tell-rest ()
  "Put a marker and tell merlin until marker is satisfied."
  (let* ((marker (cdr (merlin-send-cursor-command '(tell marker))))
         (point (when marker (point))))
    (while (and point (not (= point (point-max))))
      (forward-line 10)
      (setq point (merlin-tell-source (merlin-buffer-substring point (point))))))
    (when point (merlin-send-cursor-command '(tell eof))))

(defun merlin-tell-to-point (&optional point)
  "Tell to merlin part of the buffer between START and END. START
may be nil, in that case the current cursor of merlin is used."
  (let* ((point (if point point (point)))
         (start (min point merlin-dirty-point))
         (start (car (merlin-send-cursor-command
                       `(tell start at ,(merlin-unmake-point start))))))
    (setq merlin-dirty-point point)
    (save-excursion
      (merlin-tell-source (merlin-buffer-substring start point))
      (goto-char point)
      (merlin-tell-rest))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; POINT SYNCHRONIZATION ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun merlin-sync-margin-lock-zone ()
  "Mark the position of the lock zone by a marker in the margin."
  (if merlin-lock-zone-margin-overlay
      (delete-overlay merlin-lock-zone-margin-overlay))
  (save-excursion
    (goto-char merlin-dirty-point)
    (setq merlin-lock-zone-margin-overlay (make-overlay (point) (point)))
    (set-window-margins nil 1)
    (merlin-put-margin-overlay merlin-lock-zone-margin-overlay
                               merlin-margin-lock-string)))

(defun merlin-sync-highlight-lock-zone ()
  "Mark the position of the lock zone by highlighting the zone."
    (if merlin-lock-zone-highlight-overlay
      (delete-overlay merlin-lock-zone-highlight-overlay))
  (setq merlin-lock-zone-highlight-overlay (make-overlay (point-min) merlin-dirty-point))
  (overlay-put merlin-lock-zone-highlight-overlay 'face 'merlin-locked-face))

(defun merlin-sync-lock-zone-display ()
  "Update the locked zone display, according to `merlin-display-lock-zone', ie.
iterates through it and call each method."
  (dolist (x merlin-display-lock-zone)
    (case x
      (margin (merlin-sync-margin-lock-zone))
      (highlight (merlin-sync-highlight-lock-zone)))))

(defun merlin-sync-edit (start end length)
  "Retract the locked zone after an edit.
Called when an edit is made by the user."
  (if (and merlin-mode (< start merlin-dirty-point))
      (progn (setq merlin-dirty-point (1- start))
             (merlin-sync-lock-zone-display))))

(defun merlin-sync-to-point (&optional point skip-marker)
  "Makes sure the buffer is synchronized on merlin-side and centered around (point)."
  (unless point (setq point (point)))
  (merlin-tell-to-point point)
  (unless skip-marker
    (merlin-send-cursor-command '(seek marker)))
  (merlin-sync-lock-zone-display))

;;;;;;;;;;;;;;;;;;
;; ERROR REPORT ;;
;;;;;;;;;;;;;;;;;;

(defun merlin-find-error-for-line (line errors)
  "Return the first error mentioning line number LINE among ERRORS.
Return nil if there is no error on this line."
  (let* ((found nil))
    (while (and (not found) errors)
      (let* ((err (car errors))
             (start-line (cdr (assoc 'line (cdr (assoc 'start err)))))
             (end-line (cdr (assoc 'line (cdr (assoc 'end err))))))
        (if (and (>= line start-line) (<= line end-line))
            (setq found err)
          (setq errors (cdr errors)))))
    found))

(defvar merlin-error-timer nil
  "Timer to show the error at point in the echo area.")

(defun merlin-error-start-timer ()
  "Start the error timer as an idle timer.
When it expires, the current Merlin error is shown in the echo
area."
  (merlin-error-cancel-timer)
  (setq merlin-error-timer
        (run-with-idle-timer 0.1 'repeat 'merlin-show-error-on-current-line))
  (merlin-error-start-gc-timer))

(defun merlin-error-cancel-timer ()
  "Cancel the error display timer."
  (when merlin-error-timer
    (cancel-timer merlin-error-timer)
    (setq merlin-error-timer nil)))

(defvar merlin-error-gc-timer nil
  "Timer to collect unused Merlin timers.
This triggers `merlin-error-gc' to check whether there are any
buffers left using Merlin.  If not, we can cancel this timer and
`merlin-error-timer'.")

(defun merlin-error-gc ()
  "Check whether there are still buffers using Merlin.
Clean up Merlin timers if there are none."
  (when (not (member t
                     (mapcar
                      (lambda (buf) (buffer-local-value 'merlin-mode buf))
                      (buffer-list))))
    ;; No buffer uses Merlin anymore. Kill all hu^H^Htimers.
    (merlin-error-cancel-timer)
    (merlin-error-cancel-gc-timer)))

(defun merlin-error-start-gc-timer ()
  "Start the Merlin GC timer (see `merlin-error-gc-timer').
The timer fires every 10 seconds of idle time."
  (merlin-error-cancel-gc-timer)
  (setq merlin-error-gc-timer (run-at-time 10 10 'merlin-error-gc)))

(defun merlin-error-cancel-gc-timer ()
  "Cancel the Merlin GC timer (see `merlin-error-gc-timer')."
  (when merlin-error-gc-timer
    (cancel-timer merlin-error-gc-timer)
    (setq merlin-error-gc-timer nil)))

(defun merlin-chomp (str)
  "Remove whitespace at the beginning and end of STR."
  (replace-regexp-in-string "^[[:space:]\n]\+\\|[[:space:]\n]\+$" "" str))

(defun merlin-show-error-on-current-line ()
  "Show the error of the current line in the echo area.
If there is no error, do nothing."
  (when (and merlin-mode (not (current-message)))
    (lexical-let* ((current-line (line-number-at-pos))
                   (err (merlin-find-error-for-line
                         (line-number-at-pos) merlin-pending-errors)))
      (if err (message (merlin-chomp (cdr (assoc 'message err))))))))

(defun merlin-error-next ()
  "Jump to the next error."
  (interactive)
  (if merlin-pending-errors
      (let ((err (pop merlin-pending-errors)))
        (goto-char (cadr (assoc 'bounds err)))
        (if merlin-pending-errors-overlays
            (delete-overlay (pop merlin-pending-errors-overlays)))
        (if merlin-pending-errors
            (message "%s (%d more errors, use %s to go to the next)"
                     (cdr (assoc 'message err))
                     (length merlin-pending-errors)
                     (substitute-command-keys "\\[merlin-error-next]"))
          (message "%s" (cdr (assoc 'message err))))
        (merlin-highlight (cdr (assoc 'bounds err)) 'next-error))
    (next-error)))

(defun merlin-error-delete-overlays ()
  "Remove margin error overlays."
  (mapc #'delete-overlay merlin-pending-errors-overlays)
  (setq merlin-pending-errors-overlays nil))

(defun merlin-error-warning-p (msg)
  "Tell if the message MSG is a warning."
  (string-match "^Warning" msg))

(defun merlin-error-reset ()
  "Clear error list."
  (interactive)
  (setq merlin-pending-errors nil)
  (merlin-error-delete-overlays))

(defun merlin--kill-error-if-edited (overlay
				     is-after
				     beg
				     end
				     &optional length)
  "Remove an error from the pending error lists if it is edited by the user."
  (when is-after
    (let ((err (nth (position overlay merlin-pending-errors-overlays)
		    merlin-pending-errors)))
      (setq merlin-pending-errors
	    (delete err merlin-pending-errors))
      (setq merlin-pending-errors-overlays
	    (delete overlay merlin-pending-errors-overlays))
      (delete-overlay overlay))))

(defun merlin-error-display-in-margin (errors)
  "Given a list of ERRORS, put annotations in the margin corresponding to them."
  (let* ((err-point
          (lambda (err)
            (let* ((bounds (merlin-make-bounds err))
                   (bounds (cons (copy-marker (car bounds))
                                 (copy-marker (cdr bounds)))))
              (acons 'bounds bounds err))))
         (err-overlay
          (lambda (err)
            (let* ((bounds (cdr (assoc 'bounds err)))
                   (overlay (make-overlay (car bounds) (cdr bounds))))
	      (push #'merlin--kill-error-if-edited
		    (overlay-get overlay 'modification-hooks))
              (if (merlin-error-warning-p (cdr (assoc 'message err)))
                  (merlin-put-margin-overlay overlay
                                             merlin-margin-warning-string
                                             'merlin-compilation-warning-face)
                (merlin-put-margin-overlay overlay
                                           merlin-margin-error-string
                                           'merlin-compilation-error-face))
              overlay)))
         (errors   (mapcar err-point errors)))
    (setq merlin-pending-errors errors)
    (when merlin-error-in-margin (setq merlin-pending-errors-overlays
                                       (mapcar err-overlay errors)))))

(defun merlin-error-check (view-errors-p)
  "Check for errors.
Return t if there were not any or nil if there were.  Moreover, it displays the
errors in the margin.  If VIEW-ERRORS-P is non-nil, display a count of them."
  (merlin-error-reset)
  (merlin-sync-to-point (point-max) t)
  (let* ((errors (merlin-send-command 'errors))
         (errors (delete-if (lambda (e) (not (assoc 'start e))) errors))
         (errors (if merlin-report-warnings errors
                   (delete-if (lambda (e) (merlin-error-warning-p (cdr (assoc 'message e))))
                              errors))))
    (if (not errors) (when view-errors-p (message "ok"))
      (progn
        (merlin-error-display-in-margin errors)
        (when view-errors-p
          (message "(%d pending errors, use %s to jump)"
                   (length errors)
                   (substitute-command-keys "\\[merlin-error-next]")))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; COMPLETION-AT-POINT SUPPORT ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun merlin-completion-format-entry (entry)
  "Format the completion entry ENTRY."
  (let ((type
         (cond
          ((member (cdr (assoc 'kind entry)) '("Module" "module"))
           " <module>")
          ((string-equal (cdr (assoc 'kind entry)) "Type")
           (format " [%s]" (cdr (assoc 'desc entry))))
          (t
           (replace-regexp-in-string "^[^:]+:[ \n]+" "" (cdr (assoc 'desc entry)))))))
    (replace-regexp-in-string "[\n ]+" " " type)))

(defun merlin-completion-prefix (ident)
  "Compute the prefix of IDENT.  The prefix of `Foo.bar' is `Foo.' and the prefix of `bar' is `'."
  (let* ((l (butlast (split-string ident "\\.")))
         (s (mapconcat 'identity l ".")))
    (if (string-equal s "") s (concat s "."))))

(defun merlin-completion-data (ident)
  "Return the data for completion of IDENT, i.e. a list of pairs (NAME . TYPE)."
  (let ((prefix (merlin-completion-prefix ident))
        (data (append (merlin-get-completion ident) nil)))
    (mapcar (lambda (entry)
                (list (concat prefix (cdr (assoc 'name entry)))
                      (merlin-completion-format-entry entry)
                      (elt (cdr (assoc 'kind entry)) 0)))
            data)))

(defun merlin-completion-lookup (string state)
  "Lookup the entry STRING inside the completion table."
  (let ((ret (assoc string merlin-completion-annotation-table)))
    (if ret (message "%s%s" (car ret) (cdr ret)))))

(defun merlin-completion-at-point ()
  "Perform completion at point with merlin."
  (lexical-let*
      ((bounds (bounds-of-thing-at-point 'ocaml-atom))
       (start  (if bounds (car bounds) (point)))
       (end    (if bounds (cdr bounds) (point)))
       (string (if bounds (merlin-buffer-substring start end) ""))
       (request (if string (replace-regexp-in-string "[^\\.]+$" "" string))))
    (when (or (not merlin-completion-at-point-cache-query)
              (not (equal (cons request start)  merlin-completion-at-point-cache-query)))
      (setq merlin-completion-at-point-cache-query (cons request start))
      (merlin-sync-to-point)
      (setq merlin-completion-annotation-table
            (mapcar (lambda (a) (cons (car a) (concat ": " (cadr a))))
                    (merlin-completion-data request))))
    (list start end #'merlin-completion-table
          . (:exit-function #'merlin-completion-lookup
             :annotation-function #'merlin-completion-annotate))))

(defun merlin-completion-annotate (candidate)
  "Retrieve the annotation for candidate CANDIDATE in `merlin-completion-annotate-table'."
  (cdr (assoc candidate merlin-completion-annotation-table)))

(defun merlin-completion-table (string pred action)
  "Implement completion for merlin using `completion-at-point' API."
  (if (eq 'metadata action)
      (when merlin-completion-types
        '(metadata ((annotation-function . merlin-completion-annotate)
                    (exit-function . merlin-completion-lookup))))
    (complete-with-action action merlin-completion-annotation-table string pred)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; AUTO-COMPLETE SUPPORT ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defvar merlin-ac-prefix ""
  "The cache of the prefix for completion")
(make-variable-buffer-local 'merlin-ac-prefix)

(defvar merlin-ac-use-summary t
  "Use :summary for the types in AC")

(defvar merlin-ac-use-document nil
  "Use :document for the types in AC")

(defun merlin-ac-make-popup-item (data)
  "Create a popup item from data DATA."
  (popup-make-item
   (car data)
   :summary (if (and merlin-completion-types
                     merlin-ac-use-summary) (cadr data))
   :symbol (cddr data)
   :document (if (and merlin-completion-types
                      merlin-ac-use-document) (cadr data))))

(defun merlin-ac-source-refresh-cache()
  "Refresh the cache of completion."
  (setq merlin-ac-prefix ac-prefix)
  (setq merlin-ac-cache
        (mapcar #'merlin-ac-make-popup-item (merlin-completion-data merlin-ac-prefix))))


(defun merlin-ac-source-init ()
  "Initialize the cache for `auto-complete' completion.
Called at the beginning of a completion to fill the cache (the
variable `merlin-ac-cache')."
  (merlin-sync-to-point ac-point)
  (setq merlin-completion-point ac-point)
  (merlin-ac-source-refresh-cache))

(defun merlin-try-completion ()
  "Try completing after having synchronized the point."
  (interactive)
  (auto-complete '(merlin-ac-source)))

(defun merlin-ac-prefix ()
  "Retrieve the prefix for completion with merlin."
  (car (bounds-of-thing-at-point 'ocaml-atom)))

(defun merlin-ac-fetch-type ()
  "Prints the type of the selected candidate"
  (let ((candidate (merlin-buffer-substring merlin-completion-point  (point))))
    (when merlin-completion-types
      (mapc (lambda (item)
              (when (string-equal candidate item)
                (message "%s: %s" candidate (popup-item-summary item))))
            merlin-ac-cache))))

(defun merlin-auto-complete-candidates ()
  "Return the candidates for auto-completion with
  auto-complete. If the cache is wrong then recompute it."
  (if (not (equal ac-prefix
                  merlin-ac-prefix))
      (merlin-ac-source-refresh-cache))
  merlin-ac-cache)

(defvar merlin-ac-source
  (if merlin-ac-prefix-size
  `((init . merlin-ac-source-init)
    (candidates . merlin-auto-complete-candidates)
    (action . merlin-ac-fetch-type)
    (prefix . ,merlin-ac-prefix-size))
  '((init . merlin-ac-source-init)
    (candidates . merlin-auto-complete-candidates)
    (action . merlin-ac-fetch-type)
    (prefix . merlin-ac-prefix))))

(when (featurep 'auto-complete)
  (eval '(ac-define-source "merlin" merlin-ac-source)))

;;;;;;;;;;;;;;;;;;;;;;;
;; EXPRESSION TYPING ;;
;;;;;;;;;;;;;;;;;;;;;;;

(defun merlin-type-expression (exp callback-if-success &optional callback-if-exn)
  "Get the type of EXP inside the local context."
  (if exp (merlin-send-command-async
           (list 'type 'expression (substring-no-properties exp)
                 'at (merlin-unmake-point (point)))
           callback-if-success callback-if-exn)))

(defun merlin-type-display-in-buffer (text)
  "Change content of type-buffer."
  (let ((initialized (get-buffer merlin-type-buffer-name)))
    (with-current-buffer
      (cond (initialized)
            ((get-buffer-create merlin-type-buffer-name)))
    (unless initialized (funcall merlin-favourite-caml-mode))
    (erase-buffer)
    (insert text)
    (goto-char (point-min)))))

(defun merlin-type-display (bounds type &optional quiet)
  "Display the type TYPE of the expression occuring at BOUNDS.
If QUIET is non nil, then an overlay and the merlin types can be used."
  (if (not type)
      (if (not quiet)
          (message "<no information>"))
    (let ((count 0)
          (pos   0))
      (merlin-type-display-in-buffer type)
      (while (and (<= count 8)
                  (string-match "\n" type pos))
        (setq pos (match-end 0))
        (setq count (1+ count)))
      (if (> count 8)
          (display-buffer merlin-type-buffer-name)
        (message "%s"
          (with-current-buffer merlin-type-buffer-name
            (font-lock-fontify-region (point-min) (point-max))
            (buffer-string))))
      (if (and (not quiet) bounds)
          (merlin-highlight bounds 'merlin-type-face)))))

(defun merlin-type-region ()
  "Show the type of the region."
  (lexical-let*
      ((substring (merlin-buffer-substring (region-beginning) (region-end)))
       (on-success (lambda (type) (merlin-type-display nil type nil)))
       (on-error   (lambda (err)
                     (let ((msg (assoc 'message err))
                           (typ (assoc 'type err)))
                       (cond ((and typ (equal (cdr typ) "parser"))
                              (message "Error: the content of the region failed to parse."))
                             (msg (message "Error: %s" (cdr msg)))
			     (t
			      (message "Unexpected error")))))))
    (merlin-type-expression substring on-success on-error)))

(defun merlin-type-expr (exp)
  "Prompt the user for expression EXP, then show its type."
  (interactive "s# ")
  (merlin-sync-to-point)
  (let ((on-success (lambda (type) (merlin-type-display nil type nil)))
        (on-error   (lambda (err)
                      (let ((msg (assoc 'message err)))
                        (if msg (message "Error: %s" (cdr msg))
                          (message "unknown error"))))))
    (merlin-type-expression exp on-success on-error)))

;; TYPE ENCLOSING
(defun merlin-type-enclosing-query ()
  "Get the enclosings around point from merlin and sets MERLIN-ENCLOSING-TYPES."
  (let* ((bounds (bounds-of-thing-at-point 'ocaml-atom))
         (start  (if bounds (car bounds) (point)))
         (end    (if bounds (cdr bounds) (point)))
         (string (if bounds (merlin-buffer-substring start end) ""))
         (fallback (list (cons 'assoc nil)
                         (cons 'expr string)
                         (cons 'offset (- (point) start))))
         (types (merlin-send-command (list 'type 'enclosing fallback (merlin-unmake-point (point)))
                                     (lambda (exn) '(nil))))
         (list (mapcar (lambda (obj) (cons (cdr (assoc 'type obj))
                                           (merlin-make-bounds obj)))
                       types)))
    (setq merlin-enclosing-types list)
    (setq merlin-enclosing-offset -1)
    merlin-enclosing-types))

(defun merlin-type-enclosing-go ()
  "Highlight the given corresponding enclosing data (of the form (TYPE . BOUNDS)."
  (let ((data (elt merlin-enclosing-types merlin-enclosing-offset)))
    (if (cddr data)
        (merlin-type-display (cdr data) (car data)))))

(defun merlin-type-enclosing-go-up ()
  "Go up in the enclosing type list."
  (interactive)
  (when merlin-enclosing-types
    (if (>= merlin-enclosing-offset (1- (length merlin-enclosing-types)))
        (setq merlin-enclosing-offset -1))
    (setq merlin-enclosing-offset (1+ merlin-enclosing-offset))
    (merlin-type-enclosing-go)))

(defun merlin-type-enclosing-go-down ()
  "Go down in the enclosing type list."
  (interactive)
  (when merlin-enclosing-types
    (if (<= merlin-enclosing-offset 0)
        (setq merlin-enclosing-offset (length merlin-enclosing-types)))
    (setq merlin-enclosing-offset (1- merlin-enclosing-offset))
    (merlin-type-enclosing-go)))

(defun merlin-type-enclosing ()
  "Print the type of the expression under point (or of the region, if it exists)."
  (interactive)
  (save-excursion
    (merlin-sync-to-point)
    (if (region-active-p)
        (merlin-type-region)
      (if (merlin-type-enclosing-query)
          (merlin-type-enclosing-go-up)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; PACKAGE, PROJECT AND FLAGS MANAGEMENT ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun merlin-get-packages ()
  "Get the list of available findlib package."
  (append (merlin-send-command '(find list)) nil))

(defun merlin-use (pkg)
  "Use PKG in the current session of merlin."
  (interactive
   (list (completing-read "Package to use: " (merlin-get-packages))))
  (let* ((r (merlin-send-command (list 'find 'use (list pkg))))
         (failed (assoc 'failures r)))
    (when failed (message (cdr failed))))
  (merlin-error-reset))

(defun merlin-load-project-file ()
  "Load the .merlin file corresponding to the current file."
  (interactive)
  (merlin-rewind)
  (let* ((r (merlin-send-command (list 'project 'find (buffer-file-name))))
         (failed (assoc 'failures r))
         (result (assoc 'result r)))
    (when failed (message (cdr failed)))
    (when (and result (listp (cdr result)))
      (setq merlin-project-file (cadr result)))))

(defun merlin-goto-project-file ()
  "Goto the merlin file corresponding to the current file."
  (interactive)
  (let* ((file merlin-project-file)
         (file (if (listp file) (car file) file)))
    (if file
      (find-file-other-window file)
      (message "No project file for the current buffer."))))

;;;;;;;;;;;;
;; LOCATE ;;
;;;;;;;;;;;;

(defun merlin-locate-pure (ident)
  "Locate the identifier IDENT at point."
  (merlin-sync-to-point)
  (let* ((r (merlin-send-command
             (list 'locate (substring-no-properties ident)
                   'at (merlin-unmake-point (point))))))
    (if r
        (if (listp r)
            (merlin-goto-file-and-point r)
          (message r))
      (error "%s not found. (No answer from merlin)" ident))))

(defun merlin-locate ()
  "Locate the identifier under point"
  (interactive)
  (let ((ident (thing-at-point 'ocaml-atom)))
    (when ident (merlin-locate-pure ident))))

;; I don't like it beginning by "ac" but it is the only way I found to get it
;; working (otherwise the completion menu just closes itself)
(defun ac-merlin-locate ()
  "Locate the identifier currently selected in the ac-completion."
  (interactive)
  (when (ac-menu-live-p)
    (when (popup-hidden-p ac-menu)
      (ac-show-menu))
    (let ((merlin-locate-in-new-window 'always))
      (merlin-locate-pure (ac-selected-candidate)))
    (ac-show-menu)))

(defun merlin-pop-stack ()
  "Go back to the last position where the user did a locate."
  (interactive)
  (let ((r (pop merlin-position-stack)))
    (cond ((not r) (message "empty stack"))
          ((equal merlin-locate-in-new-window 'never)
           (switch-to-buffer (car r)))
          ((or (equal merlin-locate-in-new-window 'always)
               (not (equal (buffer-name) (car r))))
           (select-window (display-buffer (car r)))))
    (when r (goto-char (cdr r)))))

;;;;;;;;;;;;;;;;
;; OCCURENCES ;;
;;;;;;;;;;;;;;;;

(defun merlin-occurences-populate-buffer (lst)
  (lexical-let ((src-buff (buffer-name))
                (occ-buff (merlin-get-occ-buff)))
    (with-current-buffer occ-buff
      (let ((inhibit-read-only t)
            (buffer-undo-list t))
        (erase-buffer)
        (mapcar
         (lambda (pos)
           (lexical-let*
               ((start (assoc 'start pos))
                (line  (cdr (assoc 'line start)))
                (col   (cdr (assoc 'col  start)))
                (action (lambda (ev)
                          (let ((buff (get-buffer src-buff)))
                            (if buff
                                (progn
                                  (pop-to-buffer buff)
                                  (merlin-goto-point start))
                              (message "Closed buffer : %s" src-buff))))))
             (insert "  + ")
             (insert-button
              (format "occurence at line %d column %d" line col)
              'action action)
             (insert "\n")))
             lst)))))

(defun merlin-occurences-list (lst)
  (merlin-occurences-populate-buffer lst)
  (cond ((equal merlin-occurences-show-buffer 'same)
         (switch-to-buffer (merlin-get-occ-buff)))
        ((equal merlin-occurences-show-buffer 'other)
         (switch-to-buffer-other-window (merlin-get-occ-buff)))
	(t nil)))

(defun merlin-occurences ()
  (interactive)
  (merlin-sync-to-point)
  (let* ((r (merlin-send-command
             (list 'occurences 'ident 'at
                   (merlin-unmake-point (point))))))
    (when r
      (if (listp r)
          (merlin-occurences-list r)
        (message r)))))

;;;;;;;;;;;;;;;;;;;;;;;
;; SEMANTIC MOVEMENT ;;
;;;;;;;;;;;;;;;;;;;;;;;

(defun merlin-phrase-goto (command indice)
  "Go to the phrase indicated by COMMAND to the end INDICE.
Returns the position."
  (let ((r (merlin-send-command (list 'boundary command))))
    (unless (equal r 'null)
      (merlin-goto-point (elt r indice))
      (point))))

(defun merlin-phrase-next (&optional point)
  "Go to the beginning of the next phrase. FIXME"
  (interactive)
  nil)
;;  (unless point (setq point (point)))
;;  (merlin-sync-to-point point)
;;  (cond
;;    ((merlin-phrase-goto 'next 0))
;;    ((progn (merlin-tell-definitions 2)
;;            (goto-char (merlin-seek-exact point))
;;            (merlin-phrase-goto 'next 0)))
;;    (t (goto-char (point-max)))))

(defun merlin-phrase-prev ()
  "Go to the beginning of the previous phrase."
  (interactive)
  (let ((point (point)))
    (merlin-sync-to-point)
    (if (equal point (merlin-phrase-goto 'current 0))
        (merlin-phrase-goto 'prev 0))))

(defun merlin-to-point ()
  "Update merlin to the current point, reporting errors."
  (interactive)
  (merlin-sync-to-point)
  (merlin-error-check t))

(defun merlin-to-end ()
  "Update merlin to the end-of-file, reporting errors."
  (interactive)
  (when merlin-mode
    (merlin-sync-to-point (point-max))
    (merlin-error-check t)))

(defun merlin-customize ()
  "Open the customize buffer for the group merlin."
  (interactive)
  (customize-group 'merlin))

(defun merlin-version ()
  "Print the version of the ocamlmerlin binary."
  (interactive)
  (message "%s" (replace-regexp-in-string
                 "\n$" ""
                 (shell-command-to-string (concat merlin-command " -version")))))

;;;;;;;;;;;;;;;;
;; MODE SETUP ;;
;;;;;;;;;;;;;;;;

(defvar merlin-mode-map
  (let ((merlin-map (make-sparse-keymap))
        (merlin-menu-map (make-sparse-keymap))
        (merlin-show-type-map (make-sparse-keymap)))
    (define-key merlin-map (kbd "C-c <C-return>") 'merlin-to-point)
    (define-key merlin-map (kbd "C-c C-x") 'merlin-error-next)
    (define-key merlin-map (kbd "C-c C-l") 'merlin-locate)
    (when (featurep 'auto-complete)
      (define-key ac-complete-mode-map (kbd "C-c C-l") 'ac-merlin-locate))
    (define-key merlin-map (kbd "C-c &") 'merlin-pop-stack)
    (define-key merlin-map (kbd "C-c C-r") 'merlin-rewind)
    (define-key merlin-map (kbd "C-c C-u") 'merlin-refresh)
    (define-key merlin-map (kbd "C-c TAB") 'merlin-try-completion)
    (define-key merlin-map (kbd "C-c C-t") 'merlin-type-enclosing)
;; See the discussion on #129 for the future of these bindings
;   (define-key merlin-map (kbd "C-<up>") 'merlin-type-enclosing-go-up)
;   (define-key merlin-map (kbd "C-<down>") 'merlin-type-enclosing-go-down)
    (define-key merlin-map (kbd "C-c C-n") 'merlin-phrase-next)
    (define-key merlin-map (kbd "C-c C-p") 'merlin-phrase-prev)
    (define-key merlin-menu-map [customize]
      '("Customize merlin-mode" . merlin-customize))
    (define-key merlin-menu-map [separator]
      '("-"))
    (define-key merlin-show-type-map [point]
      '(menu-item "around the cursor" merlin-type-enclosing
                  :help "Show the type of the smallest subexpression near cursor"))
    (define-key merlin-show-type-map [exp]
      '(menu-item "of an expression" merlin-type-expr
                  :help "Input an expression and show its type"))
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
                  :help "Refresh the cache of merlin (cmis in particular).  Useful after a recompilation."))
    (define-key merlin-menu-map [addflag]
      '(menu-item "Add a flag" merlin-process-add-flag
                  :help "Add a flag to be passed to ocamlmerlin after restarting it."))
    (define-key merlin-menu-map [clearflag]
      '(menu-item "Clear flags" merlin-process-clear-flags
                  :help "Clear all flags set up to be passed to ocamlmerlin."))
    (define-key merlin-menu-map [restartmerlin]
      '(menu-item "Restart merlin" merlin-restart-process
                  :help "Restart merlin for the current buffer."))
    (define-key merlin-menu-map [versionmerlin]
      '(menu-item "Version" merlin-version
                  :help "Print the version of the merlin binary."))
    (define-key merlin-map [menu-bar merlin] (cons "merlin" merlin-menu-map))
    merlin-map
    ))

(defun merlin-one-group ()
  "Groups every buffer in one emacs instance."
  '(
    (name . "default")))

(defun merlin-dir-group ()
  "Group buffers by directory" ()
  (list
    (cons 'name (file-name-directory (expand-file-name (buffer-file-name))))))

(defun merlin-setup ()
  "Set up a buffer for use with merlin."
  (interactive)
  (let* ((conf (funcall merlin-grouping-function))
         (instance (lookup-default 'name conf "default")))
    (setq merlin-instance instance)
    ; if there is not yet a merlin process
    (unless (merlin-process-started-p instance)
      (merlin-start-process merlin-current-flags conf))
    (when (and (fboundp 'auto-complete-mode)
               merlin-use-auto-complete-mode)
      (auto-complete-mode 1)
      (add-to-list 'ac-sources 'merlin-ac-source))
    (add-hook 'completion-at-point-functions
              #'merlin-completion-at-point nil 'local)
    (add-to-list 'after-change-functions 'merlin-sync-edit)
    (merlin-load-project-file)))

(defun merlin-can-handle-buffer ()
  "Simple sanity check (used to avoid running merlin on, e.g., completion buffer)."
  (buffer-file-name))

(defun merlin-process-dead-p ()
  "Return non-nil if merlin process is dead."
  (and (merlin-process)
       (not (equal (process-status (merlin-process)) 'run))))

(defun merlin-lighter ()
  "Return the lighter for merlin which indicates the status of merlin process."
  (if (merlin-process-dead-p)
      " merlin(??)"
    (if merlin-show-instance-in-lighter
        (format " merlin (%s)" merlin-instance)
      " merlin")))

;;;###autoload

(define-minor-mode merlin-mode
  "Minor mode for interacting with a merlin process.
Runs a merlin process in the background and perform queries on it.

Short cuts:
\\{merlin-mode-map}"
  nil :lighter (:eval (merlin-lighter))
  :keymap merlin-mode-map
  (if merlin-mode
      (merlin-setup)
    (when (merlin-can-handle-buffer)
      (merlin-error-gc)
      (when merlin-lock-zone-highlight-overlay
        (delete-overlay merlin-lock-zone-highlight-overlay))
      (when merlin-lock-zone-margin-overlay
        (delete-overlay merlin-lock-zone-margin-overlay))
      (when merlin-highlight-overlay
        (delete-overlay merlin-highlight-overlay))
      ;;(merlin-error-delete-overlays)
     )))

(defun merlin-after-save ()
  (when merlin-error-after-save (merlin-to-end)))

(add-hook 'merlin-mode-hook
          (lambda ()
            (add-hook 'after-save-hook 'merlin-after-save
                      nil 'make-it-local)
            (merlin-error-start-timer)))

(provide 'merlin)
;;; merlin.el ends here
