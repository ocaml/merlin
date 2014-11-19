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
  '((t :inherit compilation-warning))
  "Face to use to highlight merlin warnings."
  :group 'merlin)

(defface merlin-compilation-error-face
  '((t :inherit compilation-error))
  "Face to use to highlight merlin errors."
  :group 'merlin)

;;
;; Customizable vars
;;

(defcustom merlin-show-instance-in-lighter t
  "Show the current instance of the buffer in the lighter."
  :group 'merlin :type 'boolean)

(defcustom merlin-report-errors-in-lighter nil
  "Report absence of .merlin or errors in .merlin in the lighter."
  :group 'merlin :type 'boolean)

(defcustom merlin-grouping-function 'merlin-one-group
  "The function to know how to group buffers. This function takes
no argument and should return the configuration (see
`merlin-start-process') for the current buffer."
  :group 'merlin :type 'symbol)

(defcustom merlin-quiet-startup nil
  "If non-nil, suppress process startup message."
  :group 'merlin :type 'boolean)

(defcustom merlin-command "ocamlmerlin"
  "The path to merlin in your installation."
  :group 'merlin :type '(choice (file :tag "Filename")
                                (const :tag "Use current opam switch" opam)))


(defcustom merlin-completion-types t
  "If non-nil, print the types of the variables during completion with `auto-complete'."
  :group 'merlin :type 'boolean)

(defcustom merlin-debug nil
  "If non-nil, log the data sent and received from merlin."
  :group 'merlin :type 'boolean)

(defcustom merlin-report-warnings t
  "If non-nil, report warnings, otherwise ignore them."
  :group 'merlin :type 'boolean)

(defcustom merlin-occurrences-buffer-name "*merlin-occurrences*"
  "The name of the buffer listing occurrences of an identifier after a call to `merlin-occurrences'."
  :group 'merlin :type 'string)

(defcustom merlin-type-buffer-name "*merlin-types*"
  "The name of the buffer storing module signatures."
  :group 'merlin :type 'string)

(defcustom merlin-favourite-caml-mode nil
  "The OCaml mode to use for the *merlin-types* buffer."
  :group 'merlin :type 'symbol)

; If user did not specify its merlin-favourite-caml-mode, try to guess it from
; the buffer being edited
(defvar merlin-guessed-favorite-caml-mode nil)

(defcustom merlin-error-after-save '("ml" "mli")
  "Determines whether merlin should check for errors after saving.
If t, always check for errors after saving.
If nil, never check.
If a string list, check only if the extension of the buffer-file-name is in the list."
  :group 'merlin :type '(choice (repeat string) boolean))

(defcustom merlin-error-in-fringe (>= emacs-major-version 24)
  "If non-nil, display errors in fringe"
  :group 'merlin :type 'boolean)

(defcustom merlin-error-on-single-line nil
  "Only highlight first line of multi-line error messages"
  :group 'merlin :type 'boolean)

(defcustom merlin-display-lock-zone nil
  "How to display the locked zone.
It is a list of methods among:
   - `highlight': highlight the current locked zone (like proofgeneral)
   - `fringe': put a symbol in the fringe of the line where the
     zone ends.

In particular you can specify nil, meaning that the locked zone is not represented on the screen."
  :group 'merlin :type '(repeat
                         (choice (const :tag "Highlight the locked zone" highlight)
                                 (const :tag "Display a marker in the left fringe at the end of the locked zone" fringe))))

(defcustom merlin-default-flags nil
  "The flags to give to ocamlmerlin."
  :group 'merlin :type '(repeat string))

(defcustom merlin-use-auto-complete-mode nil
  "If non nil, use `auto-complete-mode' in any buffer."
  :group 'merlin :type '(choice (const :tag "Integrate with auto-complete" t)
                                (const :tag "Don't integrate with auto-complete" nil)
                                (const :tag "Integrate with auto-complet, use sane default options" easy)))

(defcustom merlin-ac-prefix-size nil
  "If non-nil, specify the minimum number of characters to wait before allowing auto-complete"
  :group 'merlin :type 'boolean)

(defcustom merlin-occurrences-show-buffer 'other
  "Determine how to display the occurrences list after a call to `merlin-occurrences'."
  :group 'merlin :type '(choice (const :tag "Don't show list" never)
                                (const :tag "Show in the current window" same)
                                (const :tag "Show in another window" other)))

(defcustom merlin-locate-in-new-window 'diff
  "Determine whether to display results of `merlin-locate' in a new window or not."
  :group 'merlin :type '(choice (const :tag "Always open a new window" always)
                                (const :tag "Never open a new window" never)
                                (const :tag "Open a new window only if the target file is different from current buffer." diff)))

(defcustom merlin-locate-preference 'ml
  "Determine whether locate should in priority look in ml or mli files."
  :group 'merlin :type '(choice (const :tag "Look at implementation" ml)
                                (const :tag "Look at interfaces" mli)))

(defcustom merlin-locate-focus-new-window t
  "If non-nil, when locate opens a new window it will give it the focus."
  :group 'merlin :type 'boolean)

(defcustom merlin-logfile nil
  "If non-nil, use this file for the log file (should be an absolute path).

Note that works only if you use the default grouping function. If
you are using your own grouping function, you should include a
field logfile (see `merlin-start-process')"
  :group 'merlin :type 'filename)

(defcustom merlin-arrow-keys-type-enclosing t
  "If non-nil, after a type enclosing, up and down arrow are used to go up and down the AST."
  :group 'merlin :type 'boolean)

(defcustom merlin-type-after-locate nil
  "If non-nil, use type-enclosing after locate."
  :group 'merlin :type 'boolean)

;;;;;;;;;;;;;;;;;;;;;;;;
;; Internal variables ;;
;;;;;;;;;;;;;;;;;;;;;;;;


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

(defvar merlin-process-data nil
  "The process data (as returned by the grouping function) (only valid in a process buffer).")
(make-variable-buffer-local 'merlin-process-data)

(defvar merlin-process-owner nil
  "Name of the buffer owning the local process (only valid in a process buffer).")
(make-variable-buffer-local 'merlin-process-owner)

(defvar merlin-buffer nil
  "Buffer for merlin input.")
(make-variable-buffer-local 'merlin-buffer)

;; Those variables will be bound dynamically in the scope of asynchronous
;; closures (merlin-send-command-async & merlin-wait-for-answer)
; (defvar merlin-result nil
;   "Temporary variables to store command results.")
; (make-variable-buffer-local 'merlin-result)

; (defvar merlin-ready nil
;   "If non-nil, the reception is done.")
; (make-variable-buffer-local 'merlin-ready)

(defvar merlin-dirty-point 0
  "Position after which buffer content may differ.")
(make-variable-buffer-local 'merlin-dirty-point)

;; Overlays
(defvar merlin-lock-zone-highlight-overlay nil
  "Overlay used for the lock zone highlighting.")
(make-variable-buffer-local 'merlin-lock-zone-highlight-overlay)

(defvar merlin-lock-zone-fringe-overlay nil
  "Overlay used for the fringe indicator of the lock zone.")
(make-variable-buffer-local 'merlin-lock-zone-fringe-overlay)

;; Errors related variables
(defvar merlin-erroneous-buffer nil
  "Whether the buffer is erroneous or not")
(make-variable-buffer-local 'merlin-erroneous-buffer)

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
(defvar merlin--project-cache nil "Cache for merlin--project-get")
(make-variable-buffer-local 'merlin--project-cache)
(defvar merlin--project-failures nil
  "When loading .merlin, list of errors reported. Only update error messages if
  error list changes")
(make-variable-buffer-local 'merlin--project-failures)

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
  (get-buffer-create merlin-occurrences-buffer-name))

(defun merlin-goto-point (data)
  "Go to the point indicated by `DATA' which must be an assoc list with fields
line and col"
  (goto-char (point-min))
  (forward-line (1- (lookup-default 'line data 0)))
  (forward-char (max 0 (lookup-default 'col data 0))))

(defun merlin-point-of-pos (pos)
  "Return the buffer position corresponding to the merlin
position POS."
  (save-excursion
    (merlin-goto-point pos)
    (point)))

(defun merlin-goto-file-and-point (data)
  "Go to the file and position indicated by DATA which is an assoc list
containing fields file, line and col."
  (let* ((file (assoc 'file data))
         (open-window (cond ((equal merlin-locate-in-new-window 'never) nil)
                            ((equal merlin-locate-in-new-window 'always))
                            (file)))
         (filename (if file (cdr file) buffer-file-name))
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
    (if (looking-at "['a-z_0-9A-Z`.]*['a-z_A-Z0-9]")
        (cons (point) (match-end 0)) ; returns the bounds
      nil))) ; no atom at point

(put 'ocaml-atom 'bounds-of-thing-at-point
     'bounds-of-ocaml-atom-at-point)

(defun merlin-add-display-properties (overlay bitmap string &optional face)
  "Add the necessary properties to OVERLAY to display it nicely."
  (let ((prop (if window-system
		  `(left-fringe ,bitmap . ,(if face (list face) nil))
		`((margin left-margin) ,string))))
    (when face (overlay-put overlay 'face face))
    (overlay-put overlay 'before-string
		 (propertize " " 'display prop))))

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
  "Lookup KEY in LIST which is a list of pairs. If not found,
return DEFAULT or the value associated to KEY."
  (let ((v (assoc key list)))
    (if v (cdr v)
      default)))


(defun merlin-instance-buffer-name (name)
  "Return the buffer name corresponding to the merlin instance NAME."
  (format " *merlin (%s)*" name))

(defun merlin-process-buffer ()
  "Return the process buffer of the current buffer."
  (get-buffer (merlin-instance-buffer-name merlin-instance)))

(defun merlin-process ()
  "Return the process of the current buffer."
  (and (merlin-process-buffer)
       (buffer-local-value 'merlin-process (merlin-process-buffer))))

(defun merlin-process-owner ()
  "Return the last user of the process of the current buffer."
  (buffer-local-value 'merlin-process-owner (merlin-process-buffer)))

(defun merlin-start-process (flags &optional configuration)
  "Start the merlin process by fetching the information inside CONFIGURATION. FLAGS contains the list of flags to give merlin.
   CONFIGURATION is an association list with the following keys:
- `extra-flags': extra flags to give merlin

- `command': command to run

- `env': list of strings (of the shape VARIABLE=FOO) (see
`process-environment') that will be prepended to the environment of merlin

- `logfile': path to the logfile

- `name': the name of the instance."
  ; Assuming process is started from the main caml buffer,
  ; try to guess the favourite ocaml mode
  (unless (or merlin-favourite-caml-mode merlin-guessed-favorite-caml-mode)
    (let ((main-caml-mode (member major-mode '(tuareg-mode caml-mode))))
      (when main-caml-mode
        (setq merlin-guessed-favorite-caml-mode (car main-caml-mode)))))

  ; Really start process
  (let* ((command (lookup-default 'command configuration (merlin-command)))
        (extra-flags (lookup-default 'flags configuration nil))
        (name (lookup-default 'name configuration "default"))
        (environment (lookup-default 'env configuration nil))
        (logfile (lookup-default 'logfile configuration nil))
        (buffer-name (merlin-instance-buffer-name name)))
    (when (not merlin-quiet-startup)
      (message "Starting merlin instance: %s (binary=%s)."
	       name command))
    (setq merlin-instance name)
    (when (not (merlin-process-started-p name))
      (let* ((buffer (get-buffer-create buffer-name))
             (process-environment (append
                                    (if logfile
                                      (list (format "MERLIN_LOG=%s" (expand-file-name logfile))))
                                    environment
                                    process-environment))
             (p (apply #'start-file-process "merlin" buffer-name
                       command `("-protocol" "sexp" . ,(append extra-flags flags)))))
        (with-current-buffer buffer
          (set-process-query-on-exit-flag p nil)
          (setq merlin-process p)
          (setq merlin-process-data configuration)
          (setq merlin-instance name)
          (setq merlin-process-queue (tq-create p)))
        p))))

(defun merlin-process-data ()
  "Return the process configuration of the current buffer."
  (buffer-local-value 'merlin-process-data (merlin-process-buffer)))

(defun merlin-error-after-save ()
  "Determine whether the buffer should be checked for errors depending on the value of merlin-error-after-save setting."
  (cond
    ((equal merlin-error-after-save t) t)
    ((equal merlin-error-after-save nil) nil)
    ((and (listp merlin-error-after-save)
          buffer-file-name)
     (member (file-name-extension buffer-file-name)
             merlin-error-after-save))))

(defun merlin-toggle-view-errors ()
  "Toggle the viewing of errors in the buffer."
  (interactive)
  (setq merlin-error-after-save (not (merlin-error-after-save)))
  (if (merlin-error-after-save)
      (progn
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
  (merlin-start-process merlin-default-flags (funcall merlin-grouping-function))
  (setq merlin-erroneous-buffer nil))

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

(defun merlin--acquired-buffer ()
  "Return whether the current buffer was the current user of the merlin process."
  (equal (merlin-process-owner) (buffer-name)))

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
  (let ((w32-pipe-read-delay 0)) ;; fix 50ms latency of emacs on win32
    (while (not merlin-ready)
      (accept-process-output (merlin-process) 0.1 nil t)))
  merlin-result)

(defun merlin--reset ()
  "Rewind the knowledge of merlin of the current buffer to zero."
  (let* ((ext (if buffer-file-name
                  (file-name-extension buffer-file-name)
                "ml"))
         (ext (if (string-equal ext "mli") 'mli 'ml))
         (name (or buffer-file-name "toplevel")))
    (merlin-send-command (list 'reset ext name))
    (merlin-error-reset)
    (setq merlin-dirty-point (point-min))))

(defun merlin--check-project-file ()
  "Check if .merlin file loaded successfully."
  (let* ((project (merlin--project-get))
         (failures (cdr project)))
    (unless (equal failures merlin--project-failures)
      (message (mapconcat 'identity failures "\n"))
      (setq merlin--project-failures failures))))

(defun merlin--acquire-buffer (&optional force)
  "Prepare merlin to receive data from current buffer."
  (unless (and (merlin--acquired-buffer) (not force))
    (let ((name (buffer-name)))
      (with-current-buffer (merlin-process-buffer)
        (setq merlin-process-owner name)))
    (merlin--reset)
    (merlin--check-project-file)))

(defun merlin-send-command-async (command callback-if-success &optional callback-if-exn)
  "Send COMMAND (with arguments ARGS) to merlin asynchronously.
Give the result to callback-if-success.  If merlin reported an
error and if CALLBACK-IF-EXN is non-nil, call the function with
the error message otherwise print a generic error message."
  (assert (merlin--acquired-buffer))
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
  (let ((merlin-result nil)
        (merlin-ready nil))
    (when (merlin-send-command-async command
                                     (lambda (data) (setq merlin-result data))
                                     callback-if-exn)
      (merlin-wait-for-answer))))

;; SPECIAL CASE OF COMMANDS

(defun merlin-refresh ()
  "Refresh changed merlin cmis."
  (interactive)
  (merlin--acquire-buffer)
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
  (merlin--acquire-buffer)
  (append (merlin-send-command `(which with_ext ,ext)) nil))

(defun merlin-switch-to (name ext)
  "Switch to NAME.EXT."
  (merlin--acquire-buffer)
  (let* ((exts (if (listp ext) ext (list ext)))
         (names (mapcar (lambda (ext) (concat name ext)) exts))
         (file (merlin-send-command `(which path ,names)
                 #'(lambda (err) (message "No such file (message: %s)" err)))))
    (when file (find-file-other-window file))))

(defun merlin-switch-to-ml (name)
  "Switch to the ML file corresponding to the module NAME (fallback to MLI if no ML is provided)."
  (interactive (list (completing-read "Module: " (merlin-switch-list-by-ext '(".ml" ".mli")))))
  (merlin-switch-to name '(".ml" ".mli")))

(defun merlin-switch-to-mli (name)
  "Switch to the MLI file corresponding to the module NAME (fallback to ML if no MLI is provided)."
  (interactive (list (completing-read "Module: " (merlin-switch-list-by-ext '(".mli" ".ml")))))
  (merlin-switch-to name '(".mli" ".ml")))

;;;;;;;;;;;;;;;;;;;;;;;;;
;; EXTENSION SELECTION ;;
;;;;;;;;;;;;;;;;;;;;;;;;;

(defun merlin-extension-enable (name)
  "Enable merlin support for language extension NAME."
  (interactive (list (completing-read "Extension: "
                                      (progn
                                        (merlin--acquire-buffer)
                                        (merlin-send-command '(extension list disabled))))))
  (merlin--acquire-buffer)
  (let* ((r (merlin-send-command `(extension enable (,name))))
         (failed (assoc 'failures r)))
    (when failed (message (cdr failed))))
  (merlin-error-reset))

(defun merlin-extension-disable (name)
  "Disable merlin support for language extension NAME."
  (interactive (list (completing-read "Extension: "
                                      (progn
                                        (merlin--acquire-buffer)
                                        (merlin-send-command '(extension list enabled))))))
  (merlin--acquire-buffer)
  (let* ((r (merlin-send-command `(extension disable (,name))))
         (failed (assoc 'failures r)))
    (when failed (message (cdr failed))))
  (merlin-error-reset))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; BUFFER SYNCHRONIZATION ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun merlin--buffer-substring (start end)
   "Return content of buffer between two points or empty string if points are not valid"
   (if (< start end) (buffer-substring-no-properties start end) ""))

(defun merlin--tell-source (code)
  "Tell CODE to merlin and returns whether the position if marker is still active."
  (let ((result (merlin-send-cursor-command `(tell source ,code))))
     (when (cdr result) (car result))))

(defun merlin--tell-rest ()
  "Put a marker and tell merlin until marker is satisfied."
  (let* ((marker (cdr (merlin-send-cursor-command '(tell marker))))
         (point (when marker (point)))
         (lines 20))
    (while (and point (not (= point (point-max))))
      (forward-line lines)
      (unless (> lines 1000) (setq lines (* lines 2)))
      (setq point (merlin--tell-source
		   (merlin--buffer-substring point (point)))))
    (when point
      (merlin-send-cursor-command '(tell eof)))))

(defun merlin--tell-to-point (&optional point)
  "Tell to merlin part of the buffer between START and END. START
may be nil, in that case the current cursor of merlin is used."
  (let* ((point (if point point (point)))
         (start (min point merlin-dirty-point))
         (start (car (merlin-send-cursor-command
                       `(tell start at ,(merlin-unmake-point start))))))
    (setq merlin-dirty-point point)
    (save-excursion
      (merlin--tell-source (merlin--buffer-substring start point))
      (goto-char point)
      (merlin--tell-rest))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; POINT SYNCHRONIZATION ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun merlin-sync-fringe-lock-zone ()
  "Display the position of the lock zone by a marker in the fringe."
  (if merlin-lock-zone-fringe-overlay
      (delete-overlay merlin-lock-zone-fringe-overlay))
  (save-excursion
    (goto-char merlin-dirty-point)
    (setq merlin-lock-zone-fringe-overlay (make-overlay (point) (point)))
    (merlin-add-display-properties
     merlin-lock-zone-fringe-overlay
     'horizontal-bar
     "-")))

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
      (fringe (merlin-sync-fringe-lock-zone))
      (highlight (merlin-sync-highlight-lock-zone)))))

(defun merlin-sync-edit (start end length)
  "Retract the locked zone after an edit.
Called when an edit is made by the user."
  (if (and merlin-mode (< start merlin-dirty-point))
      (progn (setq merlin-dirty-point (1- start))
             (merlin-sync-lock-zone-display))))

(defun merlin-sync-to-point (&optional point skip-marker)
  "Makes sure the buffer is synchronized on merlin-side and centered around (point)."
  (merlin--acquire-buffer)
  (unless point (setq point (point)))
  (merlin--tell-to-point point)
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

(defun merlin--error-position-delta (point err)
  "Distance between point and error."
  (setq err (cdr (assoc 'bounds err)))
  (cond ((< point (car err)) (cons (- (car err) point) 0))
        ((> point (cdr err)) (cons (- point (cdr err)) 0))
        (t (cons 0 (min (- (cdr err) point) (- point (car err)))))))

(defun merlin--error-at-position (point errors)
  "Returns error from ERRORS list most relevant at POINT"
  (let ((err nil) (d nil))
    (dolist (err- errors err)
      (let ((d- (merlin--error-position-delta (point) err-)))
        (when (or (not err) (< (car d-) (car d))
                  (and (= (car d-) (car d)) (< (cdr d-) (cdr d))))
          (setq d d-) (setq err err-))))))

(defun merlin-show-error-on-current-line ()
  "Show the error of the current line in the echo area.
  If there is no error, do nothing."
  (when (and merlin-mode (not (current-message)))
    (let* ((errors (overlays-in (line-beginning-position) (line-end-position)))
           (err nil))
      (setq errors (remove nil (mapcar 'merlin--overlay-pending-error errors)))
      (setq err (merlin--error-at-position (point) errors))
      (when err (message (merlin-chomp (cdr (assoc 'message err))))))))

(defun merlin--overlay-next-property-set (point prop &optional limit)
  "Find next point where PROP is set (like next-single-char-property-change but ensure that prop is not-nil)."
  (setq point (next-single-char-property-change point prop nil limit))
  (unless (find-if (lambda (a) (overlay-get a prop)) (overlays-at point))
    (setq point (next-single-char-property-change point prop nil limit)))
  point)

(defun merlin--overlay-previous-property-set (point prop &optional limit)
  "Find previous point where PROP is set (like previous-single-char-property-change but ensure that prop is not-nil)."
  (setq point (previous-single-char-property-change point prop nil limit))
  (unless (find-if (lambda (a) (overlay-get a prop)) (overlays-at point))
    (setq point (previous-single-char-property-change point prop nil limit)))
  point)

(defun merlin--error-prev-cycle ()
  "Returns previous error, cycling when reaching beginning of buffer"
  (let ((point (point)) (errors nil) (err nil))
    (setq point (merlin--overlay-previous-property-set point 'merlin-pending-error))
    (when (eq point (point-min))
      (setq point (merlin--overlay-previous-property-set (point-max) 'merlin-pending-error (point))))
    (setq errors (overlays-at point))
    (setq errors (remove nil (mapcar 'merlin--overlay-pending-error errors)))
    (setq err (merlin--error-at-position point errors))
    (if err (cons point err) nil)))

(defun merlin--error-next-cycle ()
  "Returns next error, cycling when reaching end of buffer"
  (let ((point (point)) (errors nil) (err nil))
    (setq point (merlin--overlay-next-property-set point 'merlin-pending-error))
    (when (eq point (point-max))
      (setq point (merlin--overlay-next-property-set (point-min) 'merlin-pending-error (point))))
    (setq errors (overlays-at point))
    (setq errors (remove nil (mapcar 'merlin--overlay-pending-error errors)))
    (setq err (merlin--error-at-position point errors))
    (if err (cons point err) nil)))

(defun merlin-error-prev ()
  "Jump back to previous error."
  (interactive)
  (let ((err (merlin--error-prev-cycle)))
    (prin1 err)
    (unless err
      (merlin--acquire-buffer)
      (prin1 "ERROR CHECK")
      (merlin--error-check nil)
      (setq err (merlin--error-prev-cycle)))
    (unless (or err merlin-erroneous-buffer) (message "No errors"))
    (when err
      (goto-char (car err))
      (message (merlin-chomp (cdr (assoc 'message (cdr err)))))
      (merlin-highlight (cdr (assoc 'bounds (cdr err))) 'next-error))))

(defun merlin-error-next ()
  "Jump to next error."
  (interactive)
  (let ((err (merlin--error-next-cycle)))
    (unless err
      (merlin--acquire-buffer)
      (prin1 "ERROR CHECK")
      (merlin--error-check nil)
      (setq err (merlin--error-next-cycle)))
    (unless (or err merlin-erroneous-buffer) (message "No errors"))
    (when err
      (goto-char (car err))
      (message (merlin-chomp (cdr (assoc 'message (cdr err)))))
      (merlin-highlight (cdr (assoc 'bounds (cdr err))) 'next-error))))

(defun merlin-error-delete-overlays ()
  "Remove error overlays."
  (remove-overlays nil nil 'merlin-kind 'error))

(defun merlin-error-warning-p (msg)
  "Tell if the message MSG is a warning."
  (string-match "^Warning" msg))

(defun merlin-error-reset ()
  "Clear error list."
  (interactive)
  (setq merlin-erroneous-buffer nil)
  (merlin-error-delete-overlays))

(defun merlin--overlay (overlay)
  "Returns non-nil if OVERLAY is managed by merlin."
  (if overlay (overlay-get overlay 'merlin-kind) nil))

(defun merlin--overlay-pending-error (overlay)
  "Returns non-nil if OVERLAY is about a pending error."
  (if overlay (overlay-get overlay 'merlin-pending-error) nil))

(defun merlin--kill-error-if-edited (overlay
				     is-after
				     beg
				     end
				     &optional length)
  "Remove an error from the pending error lists if it is edited by the user."
  (when is-after (delete-overlay overlay)))

(defun merlin-transform-display-errors (errors)
  "Populate the error list with ERRORS, transformed into an
  emacs-friendly form. Do display of error list."
  (let* ((err-point
          (lambda (err)
            (let ((bounds (merlin-make-bounds err)))
              (when merlin-error-on-single-line
                (setq bounds (cons (car bounds)
                                   (min (cdr bounds)
                                        (save-excursion
                                          (goto-char (car bounds))
                                          (line-end-position))))))
              (when (= (car bounds) (cdr bounds))
                (setq bounds (if (> (car bounds) (point-min))
                               (cons (1- (car bounds)) (cdr bounds))
                               (cons (car bounds) (1+ (cdr bounds))))))
              (setq bounds (cons (copy-marker (car bounds))
                                 (copy-marker (cdr bounds))))
              (acons 'bounds bounds err))))
         (errors (mapcar err-point errors)))
    (dolist (err errors)
      (let* ((bounds (cdr (assoc 'bounds err)))
             (overlay (make-overlay (car bounds) (cdr bounds))))
        (overlay-put overlay 'merlin-kind 'error)
        (overlay-put overlay 'merlin-pending-error err)
        (push #'merlin--kill-error-if-edited
              (overlay-get overlay 'modification-hooks))
        (when merlin-error-in-fringe
          (if (merlin-error-warning-p (cdr (assoc 'message err)))
              (merlin-add-display-properties overlay
                                             'question-mark
                                             "?"
                                             'merlin-compilation-warning-face)
            (merlin-add-display-properties overlay
                                           'exclamation-mark
                                           "!"
                                           'merlin-compilation-error-face)))
        overlay))))

(defun merlin--error-check (view-errors-p)
  "Check for errors.
Return t if there were not any or nil if there were.  Moreover, it displays the
errors in the fringe.  If VIEW-ERRORS-P is non-nil, display a count of them."
  (merlin-error-reset)
  (merlin-sync-to-point (point-max) t)
  (let* ((errors (merlin-send-command 'errors))
         (no-loc (remove-if (lambda (e) (assoc 'start e)) errors))
         (errors (remove-if (lambda (e) (not (assoc 'start e))) errors))
         (errors (if merlin-report-warnings errors
                   (remove-if (lambda (e) (merlin-error-warning-p (cdr (assoc 'message e))))
                              errors))))
    (if (not (or errors no-loc))
        (when view-errors-p (message "No errors"))
      (progn
        (setq merlin-erroneous-buffer t)
        (when no-loc
          (mapcar (lambda (e) (message (cdr (assoc 'message e)))) no-loc))
        (when errors
          (merlin-transform-display-errors errors)
          (when view-errors-p
            (message "(%d pending errors, use %s to jump)"
                     (length errors)
                     (substitute-command-keys "\\[merlin-error-next]"))))))))

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
                      (cdr (assoc 'kind entry))
                      (cdr (assoc 'info entry))))
            data)))
(defun merlin-completion-info (ident)
  "Return the info of IDENT."
  (cadddr (assoc ident (merlin-completion-data ident))))

(defun merlin-completion-lookup (string state)
  "Lookup the entry STRING inside the completion table."
  (let ((ret (assoc string merlin-completion-annotation-table)))
    (if ret (message "%s%s" (car ret) (cdr ret)))))

(defun merlin--kill-overlapping-errors (start end)
  "Kill any pending errors that overlap the region START..END."
  ;; fixme: factor out common bits with kill-if-edited
  (dolist (overlay (overlays-in start end))
    (when (merlin--overlay overlay) (delete-overlay overlay))))

(defun merlin--completion-bounds ()
  "Returns a pair (start . end) of the content to complete"
  (let ((bounds (bounds-of-thing-at-point 'ocaml-atom)))
    (cons (if bounds (car bounds) (point))
          (point))))

(defun merlin-completion-at-point ()
  "Perform completion at point with merlin."
  (lexical-let*
      ((bounds (merlin--completion-bounds))
       (start  (car bounds))
       (end    (cdr bounds))
       (prefix (merlin--buffer-substring start end)))
    (merlin--kill-overlapping-errors start end)
    (when (or (not merlin-completion-at-point-cache-query)
              (not (equal (cons prefix start)  merlin-completion-at-point-cache-query)))
      (setq merlin-completion-at-point-cache-query (cons prefix start))
      (merlin-sync-to-point)
      (setq merlin-completion-annotation-table
            (mapcar (lambda (a) (cons (car a) (concat ": " (cadr a))))
                    (merlin-completion-data prefix))))
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


;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; COMPANY MODE SUPPORT ;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun merlin--company-doc-buffer (candidate)
  "Computes the /doc/ of CANDIDATE and returns the buffer where it printed it"
  ; TODO: at the moment "doc" is understood as "type", but hopefully someday we
  ; will access ocamldoc comments and display that.
  (let ((typ (get-text-property 0 'merlin-meta candidate)))
    (if (not (equal typ " <module>"))
      (merlin--type-display-in-buffer typ)
      (let* ((expr (substring-no-properties candidate))
             (loc  (merlin-unmake-point (point)))
             (cmd  (list 'type 'expression expr 'at loc))
             (res  (merlin-send-command cmd)))
        (merlin--type-display-in-buffer res)))
    (get-buffer merlin-type-buffer-name)))

(defun merlin-company-backend (command &optional arg &rest ignored)
    (interactive (list 'interactive))
    (if merlin-mode
        (case command
          (interactive (company-begin-backend 'company-my-backend))
          (prefix
           (let ((bounds (merlin--completion-bounds)))
             (merlin--buffer-substring (car bounds) (cdr bounds))))
          (no-cache t)
          (sorted t)
          (init (merlin-sync-to-point))
          (doc-buffer (merlin--company-doc-buffer arg))
          (location
           (let ((data (merlin--locate-pos arg)))
             (when (listp data)
               (let ((filename (lookup-default 'file data buffer-file-name))
                     (linum (cdr (assoc 'line (assoc 'pos data)))))
                 (cons filename linum)))))
          (candidates
           (merlin-sync-to-point)
           (mapcar #'(lambda (x) (propertize (car x) 'merlin-meta (cadr x)))
                   (merlin-completion-data arg)))
          (post-completion
            (let ((minibuffer-message-timeout nil))
              (minibuffer-message "%s : %s" arg (get-text-property 0 'merlin-meta arg))))
          (meta
           (get-text-property 0 'merlin-meta arg))
          (annotation
           (concat " : " (get-text-property 0 'merlin-meta arg)))
          )))

;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; AUTO-COMPLETE SUPPORT ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defvar merlin-ac-prefix ""
  "The cache of the prefix for completion")
(make-variable-buffer-local 'merlin-ac-prefix)

(defvar merlin-ac-ac-prefix ""
  "The original value of ac-prefix used when computing merlin-ac-prefix")
(make-variable-buffer-local 'merlin-ac-ac-prefix)

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
   :symbol (format "%c" (elt (caddr data) 0))
   :document (if (and merlin-completion-types
                      merlin-ac-use-document) (cadr data))))

(defun merlin-ac-source-refresh-cache()
  "Refresh the cache of completion."
  (setq merlin-ac-prefix (merlin-completion-prefix ac-prefix))
  (setq merlin-ac-ac-prefix ac-prefix)
  (setq merlin-ac-cache (mapcar #'merlin-ac-make-popup-item
                                (merlin-completion-data ac-prefix))))


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
  (car (merlin--completion-bounds)))

(defun merlin-ac-fetch-type ()
  "Prints the type of the selected candidate"
  (let ((candidate (merlin--buffer-substring merlin-completion-point  (point))))
    (when merlin-completion-types
      (mapc (lambda (item)
              (when (string-equal candidate item)
                (message "%s: %s" candidate (popup-item-summary item))))
            merlin-ac-cache))))

(defun merlin-auto-complete-candidates ()
  "Return the candidates for auto-completion with
  auto-complete. If the cache is wrong then recompute it."
  (if (not (and (equal (merlin-completion-prefix ac-prefix) merlin-ac-prefix)
                (string-prefix-p merlin-ac-ac-prefix ac-prefix)))
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

(defun merlin--type-expression (exp callback-if-success &optional callback-if-exn)
  "Get the type of EXP inside the local context."
  (when exp (merlin-send-command-async
             (list 'type 'expression (substring-no-properties exp)
                   'at (merlin-unmake-point (point)))
             callback-if-success callback-if-exn)))

(defun merlin--type-display-in-buffer (text)
  "Change content of type-buffer."
  (with-current-buffer (get-buffer-create merlin-type-buffer-name)
     (when (member major-mode '(nil fundamental-mode))
       ; Guess value for merlin-favourite-caml-mode
       (let ((caml-mode (or merlin-favourite-caml-mode
                            merlin-guessed-favorite-caml-mode)))
         (when caml-mode (funcall caml-mode))))
     (erase-buffer)
     (insert text)
     (goto-char (point-min))))

(defun merlin--type-display (bounds type &optional quiet)
  "Display the type TYPE of the expression occuring at BOUNDS.
If QUIET is non nil, then an overlay and the merlin types can be used."
  (if (not type)
      (unless quiet (message "<no information>"))
    (let ((count 0)
          (pos   0))
      (merlin--type-display-in-buffer type)
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

(defun merlin--type-region ()
  "Show the type of the region."
  (lexical-let*
    (substring (merlin--buffer-substring (region-beginning) (region-end)))
    (on-success (lambda (type) (merlin--type-display nil type nil)))
    (on-error   (lambda (err)
                  (let ((msg (assoc 'message err))
                        (typ (assoc 'type err)))
                    (cond ((and typ (equal (cdr typ) "parser"))
                           (message "Error: the content of the region failed to parse."))
                          (msg (message "Error: %s" (cdr msg)))
                          (t
                            (message "Unexpected error"))))))
    (merlin--type-expression substring on-success on-error)))

(defun merlin-type-expr (exp)
  "Prompt the user for expression EXP, then show its type."
  (interactive "s# ")
  (merlin-sync-to-point)
  (let ((on-success (lambda (type) (merlin--type-display nil type nil)))
        (on-error   (lambda (err)
                      (let ((msg (assoc 'message err)))
                        (if msg (message "Error: %s" (cdr msg))
                          (message "unknown error"))))))
    (merlin--type-expression exp on-success on-error)))

;; TYPE ENCLOSING
(defun merlin--type-enclosing-query ()
  "Get the enclosings around point from merlin and sets MERLIN-ENCLOSING-TYPES."
  (let ((types (merlin-send-command (list 'type 'enclosing 'at (merlin-unmake-point (point)))
                                    (lambda (exn) nil))))
    (when types
      (setq merlin-enclosing-types
            (mapcar (lambda (obj)
                      (let* ((tail (cdr (assoc 'tail obj)))
                             (tail (cond ((equal tail "position")
                                          " (* tail position *)")
                                         ((equal tail "call")
                                          " (* tail call *)")
                                         (t "")))
                             (type (cdr (assoc 'type obj))))
                        (cons (concat type tail)
                              (merlin-make-bounds obj))))
                    types))
      (setq merlin-enclosing-offset -1)
      merlin-enclosing-types)))

(defun merlin--type-enclosing-go ()
  "Highlight the given corresponding enclosing data (of the form (TYPE . BOUNDS)."
  (let ((data (elt merlin-enclosing-types merlin-enclosing-offset)))
    (if (cddr data)
        (merlin--type-display (cdr data) (car data)))))

(defun merlin-type-enclosing-go-up ()
  "Go up in the enclosing type list."
  (interactive)
  (when merlin-enclosing-types
    (if (>= merlin-enclosing-offset (1- (length merlin-enclosing-types)))
        (setq merlin-enclosing-offset -1))
    (setq merlin-enclosing-offset (1+ merlin-enclosing-offset))
    (merlin--type-enclosing-go)))

(defun merlin-type-enclosing-go-down ()
  "Go down in the enclosing type list."
  (interactive)
  (when merlin-enclosing-types
    (if (<= merlin-enclosing-offset 0)
        (setq merlin-enclosing-offset (length merlin-enclosing-types)))
    (setq merlin-enclosing-offset (1- merlin-enclosing-offset))
    (merlin--type-enclosing-go)))

(defvar merlin-type-enclosing-map
  (let ((keymap (make-sparse-keymap)))
    (define-key keymap (kbd "C-<up>") 'merlin-type-enclosing-go-up)
    (define-key keymap (kbd "C-<down>") 'merlin-type-enclosing-go-down)
    (define-key keymap (kbd "C-w") #'(lambda ()
                                     (interactive)
                                     (let ((data (elt merlin-enclosing-types merlin-enclosing-offset)))
                                       (when (cddr data)
                                           (message "Copied %s to kill-ring" (car data))
                                           (kill-new (car data))))))

    keymap)
  "The local map to navigate type enclosing.")

(defun merlin--type-enclosing-after ()
  (when (and (fboundp 'set-temporary-overlay-map)
             merlin-arrow-keys-type-enclosing)
    (set-temporary-overlay-map merlin-type-enclosing-map t)))

(defun merlin-type-enclosing ()
  "Print the type of the expression under point (or of the region, if it exists)."
  (interactive)
  (merlin-sync-to-point)
  (if (region-active-p)
      (merlin--type-region)
    (if (merlin--type-enclosing-query)
      (progn
        (merlin-type-enclosing-go-up)
        (merlin--type-enclosing-after))
      (message "merlin: no result"))))

(defun merlin--find-extents (list low high)
  "Return the smallest extent in LIST that LOW and HIGH fit
strictly within, or nil if there is no such element."
  (find-if (lambda (extent)
	     (let ((start (merlin-point-of-pos (assoc 'start extent)))
		   (end (merlin-point-of-pos (assoc 'end extent))))
	       (or (and (> low start)
			(<= high end))
		   (and (< high end)
			(>= low start)))))
	   list))

(defun merlin-enclosing-expand ()
  "Select the construct enclosing point (or the region, if it
is active)."
  (interactive)
  (merlin-sync-to-point)
  (let* ((enclosing-extents
	  (merlin-send-command
	   `(enclosing ,(merlin-unmake-point (point)))))
	 (extents (if (use-region-p)
		      (merlin--find-extents enclosing-extents
					    (region-beginning)
					    (region-end))
		    (first enclosing-extents))))
    (if (not extents)
	(error "No enclosing construct")
      (merlin-goto-point (cdr (assoc 'start extents)))
      (push-mark (merlin-point-of-pos (cdr (assoc 'end extents)))
		 t t))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; PACKAGE, PROJECT AND FLAGS MANAGEMENT ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun merlin-get-packages ()
  "Get the list of available findlib package."
  (merlin--acquire-buffer)
  (append (merlin-send-command '(find list)) nil))

(defun merlin-use (pkg)
  "Use PKG in the current session of merlin."
  (interactive
   (list (completing-read-multiple "Package to use: " (merlin-get-packages))))
  (merlin--acquire-buffer)
  (unless (listp pkg) (setq pkg (list pkg)))
  (let* ((r (merlin-send-command (list 'find 'use pkg)))
         (failed (assoc 'failures r)))
    (when failed (message (cdr failed))))
  (merlin-error-reset))

(defun merlin--project-get ()
  "Returns a pair of two string lists (dot_merlins . failures) with a list of
.merlins file loaded and a list of error messages, if any error occured during
loading"
  (let* ((r (merlin-send-command '(project get)))
         (failed (cdr (assoc 'failures r)))
         (result (cdr (assoc 'result r)))
         (ret (cons result failed)))
    (setq merlin--project-cache (cons (float-time) ret))
    ret))

(defun merlin--project-get-cached ()
  "Like `merlin--project-get' but use a cache to prevent to limit number of
calls (lighter can be updated at a high frequency)"
  (if (and merlin--project-cache
           (< (abs (- (float-time) (car merlin--project-cache))) 4.))
      (cdr merlin--project-cache)
    (merlin--project-get)))

(defun merlin-goto-project-file ()
  "Goto the merlin file corresponding to the current file."
  (interactive)
  (merlin--acquire-buffer)
  (let* ((dot_merlins (car (merlin--project-get)))
         (file (if (listp dot_merlins) (car dot_merlins) nil)))
    (if file
        (find-file-other-window file)
      (message "No project file for the current buffer."))))

(defun merlin-flags-clear ()
  "Clear flags for the current project"
  (interactive)
  (let* ((r (merlin-send-command '(flags clear)))
         (failed (assoc 'failures r)))
    (when failed (message (cdr failed))))
  (merlin-error-reset))

(defun merlin-flags-add (flag-string)
  "Set FLAG for the current project"
  (interactive "sFlag to add: ")
  (let* ((flag-list (split-string flag-string))
         (r (merlin-send-command (list 'flags 'add flag-list)))
         (failed (assoc 'failures r)))
    (when failed (message (cdr failed))))
  (merlin-error-reset))

;;;;;;;;;;;;
;; LOCATE ;;
;;;;;;;;;;;;

(defun merlin--locate-pos (ident)
  "Locate the identifier IDENT at point and return the result."
  (merlin-send-command
   (list 'locate (if ident (substring-no-properties ident) 'null)
         merlin-locate-preference 'at (merlin-unmake-point (point)))))

(defun merlin--locate-pure (&optional ident)
  "Locate the identifier IDENT at point."
  (let* ((r (merlin--locate-pos ident)))
    (if r
        (if (listp r)
            (merlin-goto-file-and-point r)
          (error "%s" r))
      (error "%s not found. (No answer from merlin)" ident))))

(defun merlin-locate ()
  "Locate the identifier under point"
  (interactive)
  (merlin-sync-to-point)
  (merlin--locate-pure)
  (if merlin-type-after-locate
      (merlin-type-enclosing)))

;; I don't like it beginning by "ac" but it is the only way I found to get it
;; working (otherwise the completion menu just closes itself)
(defun ac-merlin-locate ()
  "Locate the identifier currently selected in the ac-completion."
  (interactive)
  (when (ac-menu-live-p)
    (merlin-sync-to-point)
    (when (popup-hidden-p ac-menu)
      (ac-show-menu))
    (let ((merlin-locate-in-new-window 'always))
      (merlin--locate-pure (ac-selected-candidate)))
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

;;;;;;;;;;;;;;;;;
;; OCCURRENCES ;;
;;;;;;;;;;;;;;;;;

(defun merlin--occurrence-text (line-num marker start end source-buf)
  (concat (propertize (format "%7d:" line-num)
		      'occur-prefix t
		      'occur-target marker
		      'follow-link t
		      'front-sticky t
		      'rear-nonsticky t
		      'mouse-face '(highlight))
	  (propertize (replace-regexp-in-string
		       "\n"
		       "\n       :"
		       (with-current-buffer source-buf
			 (buffer-substring
			  (progn
			    (goto-char start)
			    (line-beginning-position))
			  (progn
			    (goto-char end)
			    (line-end-position)))))
		      'follow-link t
		      'mouse-face '(highlight)
		      'occur-target marker)
	  (propertize "\n" 'occur-target marker)))

(defun merlin-occurrences-populate-buffer (lst)
  (let ((src-buff (buffer-name))
	(occ-buff (merlin-get-occ-buff))
	(positions
	 (mapcar (lambda (pos)
		   (merlin-goto-point (assoc 'start pos))
		   (cons (cons 'marker (point-marker)) pos))
		 lst)))
    (with-current-buffer occ-buff
      (let ((inhibit-read-only t)
	    (buffer-undo-list t)
	    (pending-line)
	    (pending-lines-text))
	(erase-buffer)
	(occur-mode)
	(insert (propertize (format "%d occurrences in buffer: %s"
				    (length lst)
				    src-buff)
			    'font-lock-face list-matching-lines-buffer-name-face
			    'read-only t
			    'occur-title (get-buffer src-buff)))
	(insert "\n")
	(dolist (pos positions)
	  (let* ((marker (cdr (assoc 'marker pos)))
		 (start (assoc 'start pos))
		 (end (assoc 'end pos))
		 (line (cdr (assoc 'line start)))
		 (start-buf-pos (with-current-buffer src-buff
				  (merlin-goto-point start)
				  (point)))
		 (end-buf-pos (with-current-buffer src-buff
				(merlin-goto-point end)
				(point)))
		 (prefix-length 8)
		 (start-offset (+ prefix-length
				  (cdr (assoc 'col start))))
		 (lines-text
		  (if (equal line pending-line)
		      pending-lines-text
		    (merlin--occurrence-text line
					    marker
					    start-buf-pos
					    end-buf-pos
					    src-buff))))

	    ;; Insert the critical text properties that occur-mode
	    ;; makes use of
	    (add-text-properties start-offset
				 (+ start-offset
				    (- end-buf-pos start-buf-pos))
				 (list 'occur-match t
				       'face list-matching-lines-face)
				 lines-text)

	    ;; Inserting text is delayed until non-equal lines are
	    ;; found in order to accumulate multiple matches within
	    ;; one line.
	    (when (and pending-lines-text
		       (not (equal line pending-line)))
	      (insert pending-lines-text))
	    (setq pending-line line)
	    (setq pending-lines-text lines-text)))

	;; Catch final pending text
	(when pending-lines-text
	  (insert pending-lines-text))
	(goto-char (point-min))))))

(defun merlin-occurrences-list (lst)
  (save-excursion
    (merlin-occurrences-populate-buffer lst)
    (cond ((equal merlin-occurrences-show-buffer 'same)
           (switch-to-buffer (merlin-get-occ-buff)))
          ((equal merlin-occurrences-show-buffer 'other)
           (switch-to-buffer-other-window (merlin-get-occ-buff)))
          (t nil))))

(defun merlin-occurrences ()
  "List all occurrences of identifier under cursor in buffer."
  (interactive)
  (merlin-sync-to-point (point-max) t)
  (let* ((r (merlin-send-command
             (list 'occurrences 'ident 'at
                   (merlin-unmake-point (point))))))
    (when r
      (if (listp r)
          (merlin-occurrences-list r)
        (error "%s" r)))))

;;;;;;;;;;;;;;;;;;;;;;;
;; SEMANTIC MOVEMENT ;;
;;;;;;;;;;;;;;;;;;;;;;;

(defun merlin--phrase-goto (command)
  "Go to the phrase indicated by COMMAND.
Returns the position."
  (let ((r (merlin-send-command
             (list 'boundary command 'at (merlin-unmake-point (point))))))
    (unless (equal r 'null)
      (merlin-goto-point (car r))
      (point))))

(defun merlin-phrase-next ()
  "Go to the beginning of the next phrase."
  (interactive)
  (merlin-sync-to-point (point-max) t)
  (merlin--phrase-goto 'next))

(defun merlin-phrase-prev ()
  "Go to the beginning of the previous phrase."
  (interactive)
  (merlin-sync-to-point (point-max) t)
  (merlin--phrase-goto 'prev))

(defun merlin-error-check ()
  "Update merlin to the end-of-file, reporting errors."
  (interactive)
  (merlin--acquire-buffer)
  (when merlin-mode (merlin--error-check t)))

(defun merlin-project-check ()
  "Display loaded .merlin files and eventual errors."
  (interactive)
  (merlin--acquire-buffer)
  (let* ((project (merlin--project-get))
         (dots (car project))
         (messages (cdr project))) ; failures list
    (add-to-list 'messages
                 (if dots (concat "Loaded .merlin files: "
                                  (mapconcat 'identity dots ", "))
                   "No .merlin loaded"))
    (message (mapconcat 'identity messages "\n"))))

(defun merlin-customize ()
  "Open the customize buffer for the group merlin."
  (interactive)
  (customize-group 'merlin))

(defun merlin-version ()
  "Print the version of the ocamlmerlin binary."
  (interactive)
  (if (merlin-process-dead-p)
      (let* ((command (concat (merlin-command) " -version"))
             (version (shell-command-to-string command))
             (version (replace-regexp-in-string "\n$" "" version)))
        (message "%s (from shell)" version))
    (message "%s" (merlin-send-command '(version)))))

(defun merlin-command ()
  "Return path of ocamlmerlin binary selected by configuration"
  (if (equal merlin-command 'opam)
    (concat (replace-regexp-in-string "\n$" ""
              (shell-command-to-string "opam config var bin"))
       "/ocamlmerlin")
    merlin-command))

;;;;;;;;;;;;;;;;
;; MODE SETUP ;;
;;;;;;;;;;;;;;;;

(defvar merlin-mode-map
  (let ((merlin-map (make-sparse-keymap))
        (merlin-menu-map (make-sparse-keymap))
        (merlin-show-type-map (make-sparse-keymap)))
    (define-key merlin-map (kbd "C-c C-x") 'merlin-error-next)
    (define-key merlin-map (kbd "C-c C-l") 'merlin-locate)
    (when (featurep 'auto-complete)
      (define-key ac-complete-mode-map (kbd "C-c C-l") 'ac-merlin-locate))
    (define-key merlin-map (kbd "C-c &") 'merlin-pop-stack)
    (define-key merlin-map (kbd "C-c C-u") 'merlin-refresh)
    (define-key merlin-map (kbd "C-c C-r") 'merlin-error-check)
    (define-key merlin-map (kbd "C-c TAB") 'merlin-try-completion)
    (define-key merlin-map (kbd "C-c C-t") 'merlin-type-enclosing)
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
    (define-key merlin-menu-map [use]
      '(menu-item "Use a package" merlin-use
                  :help "Use a findlib package."))
    (define-key merlin-menu-map [refresh]
      '(menu-item "Refresh" merlin-refresh
                  :help "Refresh the cache of merlin (cmis in particular).  Useful after a recompilation."))
    (define-key merlin-menu-map [error]
      '(menu-item "Error check" merlin-error-check
                  :help "Check current buffer for any error."))
    (define-key merlin-menu-map [dot-merlin]
      '(menu-item "dot-merlin check" merlin-project-check
                  :help "Display status of '.merlin'."))
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
  (append
   '((name . "default"))
   (if merlin-logfile (list (cons 'logfile merlin-logfile)))))

(defun merlin-dir-group ()
  "Group buffers by directory" ()
  (list
    (cons 'name (file-name-directory (expand-file-name buffer-file-name)))))

(defun merlin-setup ()
  "Set up a buffer for use with merlin."
  (interactive)
  (let* ((conf (funcall merlin-grouping-function))
         (instance (lookup-default 'name conf "default")))
    (setq merlin-instance instance)
    ; if there is not yet a merlin process
    (unless (merlin-process-started-p instance)
      (merlin-start-process merlin-default-flags conf))
    (when (and (fboundp 'auto-complete-mode)
               merlin-use-auto-complete-mode)
      (if (equal merlin-use-auto-complete-mode 'easy)
          (merlin-setup-auto-complete)
        (auto-complete-mode t))
      (add-to-list 'ac-sources 'merlin-ac-source))
    (add-hook 'completion-at-point-functions
              #'merlin-completion-at-point nil 'local)
    (add-to-list 'after-change-functions 'merlin-sync-edit)))

(defun merlin-can-handle-buffer ()
  "Simple sanity check (used to avoid running merlin on, e.g., completion buffer)."
  buffer-file-name)

(defun merlin-view-log ()
  "Jump to the log file of merlin."
  (interactive)
  (let ((file (lookup-default 'logfile (merlin-process-data) nil)))
    (if file (find-file-other-window file)
      (message "No log file for this instance."))))

(defun merlin-process-dead-p ()
  "Return non-nil if merlin process is dead."
  (not (and (merlin-process)
            (equal (process-status (merlin-process)) 'run))))

(defun merlin-lighter ()
  "Return the lighter for merlin which indicates the status of merlin process."
  (if (merlin-process-dead-p) " merlin (DEAD)"
    (progn
      (merlin--acquire-buffer)
      (let* ((messages nil)
             (project (merlin--project-get-cached)))
        (when merlin-report-errors-in-lighter
          (cond ((cdr project) (add-to-list 'messages "errors in .merlin"))
                ((not (car project)) (add-to-list 'messages "no .merlin"))))
        (when merlin-erroneous-buffer
          (add-to-list 'messages "errors in buffer"))
        (when merlin-show-instance-in-lighter
          (add-to-list 'messages merlin-instance))
        (if messages (concat " merlin (" (mapconcat 'identity messages ",") ")")
          " merlin")))))

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
      (when merlin-lock-zone-fringe-overlay
        (delete-overlay merlin-lock-zone-fringe-overlay))
      (when merlin-highlight-overlay
        (delete-overlay merlin-highlight-overlay))
      (merlin-error-delete-overlays)
      )))

(defun merlin-after-save ()
  (when (merlin-error-after-save) (merlin-error-check)))

(add-hook 'merlin-mode-hook
          (lambda ()
            (add-hook 'after-save-hook 'merlin-after-save
                      nil 'make-it-local)
            (merlin-error-start-timer)))

(defun merlin-setup-auto-complete ()
  "Integrate merlin to auto-complete with sane defaults"
  (require 'auto-complete)
  (auto-complete-mode t)
  (set (make-local-variable 'ac-auto-show-menu) t)
  (set (make-local-variable 'ac-auto-start) nil)
  (set (make-local-variable 'ac-delay) 0.0)
  (set (make-local-variable 'ac-expand-on-auto-complete) nil)
  (set (make-local-variable 'ac-ignore-case) nil)
  (set (make-local-variable 'ac-trigger-commands) nil))

(provide 'merlin)
;;; merlin.el ends here
