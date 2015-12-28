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
;; caml-types for highlighting (https://github.com/the-lambda-church/merlin/issues/331)
(require 'caml-types nil 'noerror)

;; silence free variable warning
(defvar merlin-mode)

(defgroup merlin nil
  "merlin binding mode allowing completion and typing in OCaml files."
  :group 'languages :prefix "merlin-")

;;
;; Faces
;;

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
  "The function to know how to group buffers. This function takes no argument and should return the configuration (see `merlin-start-process') for the current buffer."
  :group 'merlin :type 'symbol)

(defcustom merlin-quiet-startup nil
  "If non-nil, suppress process startup message."
  :group 'merlin :type 'boolean)

(defcustom merlin-command "ocamlmerlin"
  "The path to merlin in your installation."
  :group 'merlin :type '(choice (file :tag "Filename")
                                (const :tag "Use current opam switch" opam)))

(defcustom merlin-completion-with-doc nil
  "If non-nil, tries to retrieve ocamldoc comments associated with each completion candidate"
  :group 'merlin :type 'boolean)

(defcustom merlin-completion-dwim t
  "If non-nil, fallback to fuzzier completion when normal completion gives no result."
  :group 'merlin :type 'boolean)

(defcustom merlin-completion-types t
  "If non-nil, print the types of the variables during completion."
  :group 'merlin :type 'boolean)

(defcustom merlin-completion-arg-type t
  "If non-nil, print the type of the expected argument during completion on an application."
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

;; If user did not specify its merlin-favourite-caml-mode, try to guess it from
;; the buffer being edited
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

(defcustom merlin-error-check-then-move t
  "If t, merlin-error-next and merlin-error-prev first update the errors then move the cursor.
If nil, they both update and move at the same time."
  :group 'merlin :type 'boolean)

(defcustom merlin-default-flags nil
  "The flags to give to ocamlmerlin."
  :group 'merlin :type '(repeat string))

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
  "If non-nil, after a type enclosing, C+up and C+down arrow are used to go up and down the AST. As well, C+w copy the type to the kill ring and C+d destructure the expression."
  :group 'merlin :type 'boolean)

(defcustom merlin-type-after-locate nil
  "If non-nil, use type-enclosing after locate."
  :group 'merlin :type 'boolean)

(defcustom merlin-allow-sit-for t
  "When user attention is required, merlin will use `sit-for' only if `merlin-allow-sit-for' is `t'."
  :group 'merlin :type 'boolean)



;;;;;;;;;;;;;;;;;;;;;;;;
;; Internal variables ;;
;;;;;;;;;;;;;;;;;;;;;;;;

;; Process / Reception related variables
(defvar-local merlin-process nil
  "The merlin process for this buffer (only valid in a process buffer).")

(defvar-local merlin-instance nil
  "The name of the merlin instance for this buffer.")

(defvar-local merlin-process-queue nil
  "The transaction queue for the local process (only valid in a process buffer).")

(defvar-local merlin--process-busy nil
  "Non-nil if process is busy treating a synchronous operation (only valid in a process buffer).")

(defvar-local merlin-process-data nil
  "The process data (as returned by the grouping function) (only valid in a process buffer).")

(defvar-local merlin--grouping nil
  "Configuration returned by merlin-grouping-function.")

(defvar-local merlin--dirty-point 0
  "Position after which buffer content may differ.")

;; Errors related variables
(defvar-local merlin-erroneous-buffer nil
  "Whether the buffer is erroneous or not")

(defvar merlin-highlight-overlay nil
  "Merlin overlay used for highlights.")

;; Type related variables
(defvar-local merlin-enclosing-types nil
  "List containing the enclosing type.")
(defvar-local merlin-enclosing-offset nil
  "Current offset in `merlin-enclosing-types'.")

;; Locate
(defvar merlin-position-stack nil)

;; Misc
(defvar-local merlin--project-cache nil "Cache for merlin--project-get")
(defvar-local merlin--project-failures nil
  "When loading .merlin, list of errors reported. Only update error messages if
error list changes")
(defvar-local merlin--dwimed nil
  "Remember if we used dwim for the current completion or not")


;;;;;;;;;;;
;; UTILS ;;
;;;;;;;;;;;

(defun merlin-debug (s)
  "Output S if the variable `merlin-debug' is non-nil on the process buffer
associated to the current buffer."
  (with-current-buffer (merlin-process-buffer)
    (goto-char (point-max))
    (insert s)))

(defun merlin--get-occ-buff ()
  (get-buffer-create merlin-occurrences-buffer-name))

(defun merlin--goto-point (data)
  "Go to the point indicated by `DATA' which must be an assoc list with fields
line and col"
  (goto-char (point-min))
  (forward-line (1- (lookup-default 'line data 0)))
  (forward-char (max 0 (lookup-default 'col data 0))))

(defun merlin--point-of-pos (pos)
  "Return the buffer position corresponding to the merlin
position POS."
  (save-excursion
    (merlin--goto-point pos)
    (point)))

(defun merlin--goto-file-and-point (data)
  "Go to the file and position indicated by DATA which is an assoc list
containing fields file, line and col."
  (let* ((file (assoc 'file data))
         (open-window (cond ((equal merlin-locate-in-new-window 'never) nil)
                            ((equal merlin-locate-in-new-window 'always))
                            (file)))
         (filename (if file (cdr file) (buffer-file-name (buffer-base-buffer))))
         (focus-window (or (not open-window) merlin-locate-focus-new-window))
         (do-open (lambda ()
                    (if open-window
                      (find-file-other-window filename)
                      (find-file filename))
                    (merlin--goto-point (cdr (assoc 'pos data))))))
    (if focus-window
        (progn
          (push (cons (buffer-name) (point)) merlin-position-stack)
          (funcall do-open)
          (message "Use %s to go back."
                   (substitute-command-keys "\\[merlin-pop-stack]")))
      (save-excursion (save-selected-window (funcall do-open))))))

(defun merlin/make-point (data)
  "Transform DATA (a remote merlin position) into a point."
  (save-excursion
    (merlin--goto-point data)
    (point)))

(defun merlin--make-bounds (data)
  "From a remote merlin object DATA {\"start\": LOC1; \"end\": LOC2},
return (LOC1 . LOC2)."
  (cons
   (merlin/make-point (cdr (assoc 'start data)))
   (merlin/make-point (cdr (assoc 'end data)))))

(defun merlin/unmake-point (point)
  "Destruct POINT to line / col."
  (save-excursion (goto-char point)
                  (list (cons 'assoc nil)
                        (cons 'line (line-number-at-pos nil))
                        (cons 'col (current-column)))))

(defun bounds-of-ocaml-atom-at-point ()
  "Return the start and end points of an ocaml atom near point.
An ocaml atom is any string containing [a-z_0-9A-Z`.]."
  (save-excursion
    (skip-chars-backward "a-z0-9A-Z_'.")
    (skip-chars-backward "~?`" (1- (point)))
    (if (or (looking-at "[~?`]?['a-z_0-9A-Z.]*['a-z_A-Z0-9]")
            (looking-at "[~?`]"))
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

(defun merlin--highlight (bounds face)
  "Create an overlay on BOUNDS (of the form (START . END)) and give it FACE."
  (remove-overlays nil nil 'merlin-kind 'highlight)
  (lexical-let ((overlay (make-overlay (car bounds) (cdr bounds))))
    (overlay-put overlay 'face face)
    (overlay-put overlay 'merlin-kind 'highlight)
    (if merlin-allow-sit-for
        (unwind-protect (sit-for 60) (delete-overlay overlay)))
      (run-with-idle-timer 0.5 nil
        (lambda () (delete-overlay overlay)))))

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

(defun merlin-process-buffer (&optional instance-name)
  "Return the process buffer of the current buffer."
  (get-buffer (merlin-instance-buffer-name
                (if instance-name instance-name merlin-instance))))

(defun merlin-process (&optional instance-name)
  "Return the process of the current buffer."
  (and (merlin-process-buffer instance-name)
       (buffer-local-value 'merlin-process (merlin-process-buffer instance-name))))

(defun merlin--process-busy (&optional instance-name)
  "Nil if no synchronous work is being done, sexp representation of
synchronous command being processed by merlin otherwise."
  (buffer-local-value 'merlin--process-busy (merlin-process-buffer instance-name)))

(put 'merlin-cancelled
     'error-conditions
     '(merlin-cancelled))

(defun merlin--process-idle ()
  "Non-nil iff nothing (synchronous nor asynchronous) is being done by merlin"
  (tq-queue-empty
    (buffer-local-value 'merlin-process-queue (merlin-process-buffer))))

(defun merlin--process-busy-set (value &optional instance-name)
  (with-current-buffer (merlin-process-buffer instance-name)
    (let ((command-priority (and (boundp 'merlin-command-priority)
                                 merlin-command-priority)))
      (setq merlin--process-busy
            ;; entering new command
            (cond ((and value (not merlin--process-busy))
                   (cons command-priority value))
                  ;; exiting old command
                  ((and (not value) merlin--process-busy) nil)
                  ;; our command was removed :(
                  ((and (not value) (not merlin--process-busy))
                   (signal 'merlin-cancelled
                           "This message should not appear, please report the error"))
                  ;; two commands are competing:
                  ;; - none can handle cancellation, just fail
                  ((and (not command-priority) (not (car merlin--process-busy)))
                   (error "Merlin was already processing %S while %S was attempted"
                          merlin--process-busy value))
                  ;; - previous one has lower priority, accept request
                  ((or (not command-priority)
                       (<= (car merlin--process-busy) command-priority))
                   (cons command-priority value))
                  ;; - previous one has higher priority, cancel request
                  ((or (not (car merlin--process-busy))
                       (<= command-priority (car merlin--process-busy)))
                   ;; if we get there, caller knows how to handle the error
                   (assert command-priority)
                   (signal 'merlin-cancelled
                           "This message should not appear, please report the error"))
                  (t (error "Unhandled case in merlin--process-busy-set %S => %S"
                            merlin--process-busy value)))))))

(defun merlin--find-default-directory (directory)
  "`start-(file-)process' behaves bad when default-directory doesn't exist.
Try to find a satisfying default directory."
  (if (file-exists-p directory)
      (file-name-as-directory directory)
    (let* ((parent (file-name-directory (directory-file-name directory))))
      (if (equal parent directory)
          default-directory
        (merlin--find-default-directory parent)))))

(defun merlin--start-process (args)
  "Collection of workarounds for starting processes"
  (let* (;; issue #321, start-file-process not always defined
         (start-file-process
          (if (boundp 'start-file-process)
              #'start-file-process
            #'start-process))
         ;; issue #341, emacs-pty broken with lines >=1024 chars on OSX
         (process-connection-type nil)
         ;; if default-directory doesn't exist, start-file-process will fail
         (default-directory (or (ignore-errors (merlin--find-default-directory default-directory))
                                default-directory)))
    (message "selected directory: %S" default-directory)
    (apply start-file-process args)))

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
    (when (get-buffer buffer-name)
      (kill-buffer buffer-name))
    (when (merlin-process-dead-p name)
      (let* ((buffer (get-buffer-create buffer-name))
             (process-environment (append
                                   (if logfile
                                       (list (format "MERLIN_LOG=%s" (expand-file-name logfile))))
                                   environment
                                   process-environment))
             (p (merlin--start-process
                 `("merlin" ,buffer-name ,command
                   "-protocol" "sexp" . ,(append extra-flags flags)))))
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
          (buffer-file-name (buffer-base-buffer)))
     (member (file-name-extension (buffer-file-name (buffer-base-buffer)))
             merlin-error-after-save))))

(defun merlin-toggle-view-errors ()
  "Toggle the viewing of errors in the buffer."
  (interactive)
  (setq merlin-error-after-save (not (merlin-error-after-save)))
  (if (merlin-error-after-save)
      (progn
        (merlin--after-save)
        (message "Errors are now reported. Use %s to stop reporting them."
                 (substitute-command-keys "\\[merlin-toggle-view-errors]")))
    (progn
      (merlin-error-reset)
      (message "Errors are not reported anymore. Use %s to start again reporting them."
               (substitute-command-keys "\\[merlin-toggle-view-errors]")))))

(defun merlin--grouping-function ()
  "Wrapper to call merlin-grouping-function and update internal variable."
  (setq merlin--grouping (merlin--sexp-remove-string-properties
                          (condition-case-unless-debug err
                              (funcall merlin-grouping-function)
                            (error (message "Error in merlin-grouping-function: %S" err) nil))))
  merlin--grouping)

(defun merlin-restart-process ()
  "Restart the merlin toplevel for this buffer, taking into account new flags."
  (interactive)
  (unless merlin-mode (message "Buffer is not managed by merlin."))
  (when merlin-mode
    (when (merlin-process-buffer) (ignore-errors (merlin-kill-process)))
    (setq merlin-erroneous-buffer nil)
    (setq merlin--dirty-point 0)
    (merlin-setup)
    (message "Restarted merlin %S" merlin-instance)))

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

(defun merlin-process-started-p (&optional name)
  "Return non-nil if the merlin process for the instance NAME is already started."
  (and (merlin-process name)
       (equal (process-status (merlin-process name)) 'run)))

(defun merlin-process-dead-p (&optional name)
  "Return non-nil if merlin process is dead."
  (not (merlin-process-started-p name)))

(defun merlin-kill-process (&optional buffer)
  "Kill the merlin process inside BUFFER. If BUFFER is nil, use
the merlin buffer of the current buffer."
  (let ((buffer (if buffer buffer (merlin-process-buffer))))
    (with-current-buffer buffer
      (tq-close merlin-process-queue)
      (ignore-errors (kill-process merlin-process)))
    (unless merlin-debug (kill-buffer buffer))))

(defun merlin--context ()
  "Rewind the knowledge of merlin of the current buffer to zero."
  (let* ((name (or (buffer-file-name (buffer-base-buffer)) "toplevel"))
         (dot-merlin (lookup-default 'dot-merlin merlin--grouping nil))
         (dot-merlins (lookup-default 'dot-merlins merlin--grouping nil)))
    (setq dot-merlins
          (append (if (stringp dot-merlin)  (list dot-merlin)  dot-merlin)
                  (if (stringp dot-merlins) (list dot-merlins) dot-merlins)))
    (if dot-merlins
      (list 'dot_merlin dot-merlins 'auto name)
      (list 'auto name))))

(defun merlin--check-project-file ()
  "Check if .merlin file loaded successfully."
  (let* ((project (merlin--project-get))
         (failures (cdr project)))
    (unless (equal failures merlin--project-failures)
      (message "%s" (mapconcat 'identity failures "\n"))
      (setq merlin--project-failures failures))))

(defun merlin--send-command-async-handler (closure answer)
  "Callback sent by merlin/send-command-async to tq-enqueue."
  (let ((promise       (elt closure 0))
        (cb-if-success (elt closure 1))
        (cb-if-exn     (elt closure 2))
        (command       (elt closure 3))
        (buffer        (elt closure 4)))
    (setcar promise t)
    (with-current-buffer buffer
      (with-demoted-errors "Error in merlin/send-command-async callback: %S"
        (when merlin-debug (merlin-debug (format "<%s" answer)))
        (setq answer (car (read-from-string answer)))
        (cond ((not answer)
               (message "Invalid answer received from merlin."))
              ((string-equal (elt answer 0) "return")
               (setcdr promise (funcall cb-if-success (elt answer 1))))
              ((string-equal (elt answer 0) "exception")
               (message "Merlin failed with exception: %s" (elt answer 1))
               (when (functionp cb-if-exn)
                 (setcdr promise (funcall cb-if-exn (elt answer 1)))))
              ((and (string-equal (elt answer 0) "error")
                    (assoc 'message (elt answer 1)))
               (message "Merlin failed with error: \"%s\""
                        (cdr (assoc 'message (elt answer 1)))))
              (t (error "Command %s failed with error %s" command (elt answer 1))))))))

(defun merlin--sexp-remove-string-properties (sexp)
  "Workaround retarded emacs objects printing API.
See http://lists.gnu.org/archive/html/help-gnu-emacs/2013-09/msg00376.html"
  (cond
    ((stringp sexp) (substring-no-properties sexp))
    ((atom sexp) sexp)
    ((consp sexp)
     (cons (merlin--sexp-remove-string-properties (car sexp))
           (merlin--sexp-remove-string-properties (cdr sexp))))
    (t sexp)))

(defun merlin/send-command-async (command callback-if-success &optional callback-if-exn)
  "Send COMMAND (with arguments ARGS) to merlin asynchronously.
Give the result to callback-if-success.  If merlin reported an
error and if CALLBACK-IF-EXN is non-nil, call the function with
the error message otherwise print a generic error message."
  (unless (listp command) (setq command (list command)))
  (setq command (merlin--sexp-remove-string-properties command))
  (setq command (list (cons 'assoc nil)
                      (cons 'context (merlin--context))
                      (cons 'query command)))
  (let* ((string (concat (prin1-to-string command) "\n"))
         (promise (cons nil nil))
         (closure (list promise
                        callback-if-success
                        callback-if-exn
                        command
                        (current-buffer))))
    (if (not (equal (process-status (merlin-process)) 'run))
        (progn (error "Merlin process not running (try restarting with %s)"
                      (substitute-command-keys "\\[merlin-restart-process]"))
               nil)
      (with-current-buffer (merlin-process-buffer)
        (when merlin-debug
          (merlin-debug (format ">%s" string)))
        (tq-enqueue merlin-process-queue string "\n"
                    closure #'merlin--send-command-async-handler)
        promise))))

(defvar merlin-command-priority nil
 "Keep track of command-priority. If non-nil, competing command with lower
  priority might be cancelled.  This is signaled with 'merlin-cancelled
  symbol.
  This is used to handle the case where two synchronous commands happen to be
  nested, for instance when flycheck or lighter are updated while the user
  starts a completion.
  The solution is to make completion have 'nil' priority, meaning it can't be
  cancelled, and the background process have some integer priority.
  If two syncronous commands with nil priority are executed, an error is
  reported. If they have the same integer priority, one is executed.
  Otherwise the one with the lowest integer is cancelled.")

(defun merlin/send-command (command &optional callback-if-exn)
  "Send COMMAND (with arguments ARGS) to merlin and returns the result."
  (let ((promise (merlin/send-command-async
                            command (lambda (data) data) callback-if-exn)))
    (when promise
      (merlin--process-busy-set (list command))
      (unwind-protect
          (let ((w32-pipe-read-delay 0)        ; fix 50ms latency of emacs on win32
                (merlin-command-priority nil)) ; reset priority to default
            (while (not (car promise))
              (accept-process-output (merlin-process) 1.0)))
        (merlin--process-busy-set nil))
      (cdr promise))))

;; SPECIAL CASE OF COMMANDS

(defun merlin--parse-position (result)
  "Returns a pair whose first member is a point set at merlin cursor position
  and second member is the state of the marker"
  (cons (merlin/make-point (lookup-default 'cursor result nil))
        (when (equal (lookup-default 'marker result nil) 'true) t)))

(defun merlin--send-cursor-command (command &optional callback-if-exn)
  "Send COMMAND (with arguments ARGS) to merlin and returns the result parsed
  as a position."
  (merlin--parse-position (merlin/send-command command callback-if-exn)))

;;;;;;;;;;;;;;;;;;;;
;; FILE SWITCHING ;;
;;;;;;;;;;;;;;;;;;;;

(defun merlin-switch-list-by-ext (ext)
  "List filenames ending by EXT in the path."
  (append (merlin/send-command `(which with_ext ,ext)) nil))

(defun merlin-switch-to (name ext)
  "Switch to NAME.EXT."
  (let* ((exts (if (listp ext) ext (list ext)))
         (names (mapcar (lambda (ext) (concat name ext)) exts))
         (file (merlin/send-command `(which path ,names)
                 #'(lambda (err) (message "No such file (message: %s)" err)))))
    (when file (find-file-other-window file))))

(defun merlin-switch-to-ml (name)
  "Switch to the ML file corresponding to the module NAME (fallback to MLI if no ML is provided)."
  (interactive (list (ido-completing-read "Module: " (merlin-switch-list-by-ext '(".ml" ".mli")))))
  (merlin-switch-to name '(".ml" ".mli")))

(defun merlin-switch-to-mli (name)
  "Switch to the MLI file corresponding to the module NAME (fallback to ML if no MLI is provided)."
  (interactive (list (ido-completing-read "Module: " (merlin-switch-list-by-ext '(".mli" ".ml")))))
  (merlin-switch-to name '(".mli" ".ml")))

;;;;;;;;;;;;;;;;;;;;;;;;;
;; EXTENSION SELECTION ;;
;;;;;;;;;;;;;;;;;;;;;;;;;

(defun merlin-extension-enable (name)
  "Enable merlin support for language extension NAME."
  (interactive (list (completing-read "Extension: "
                                        (merlin/send-command '(extension list disabled)))))
  (let* ((r (merlin/send-command `(extension enable (,name))))
         (failed (assoc 'failures r)))
    (when failed (message "%s" (cdr failed))))
  (merlin-error-reset))

(defun merlin-extension-disable (name)
  "Disable merlin support for language extension NAME."
  (interactive (list (completing-read "Extension: "
                                        (merlin/send-command '(extension list enabled)))))
  (let* ((r (merlin/send-command `(extension disable (,name))))
         (failed (assoc 'failures r)))
    (when failed (message "%s" (cdr failed))))
  (merlin-error-reset))


;;;;;;;;;;;;;;;;;;;;;
;; SYNCHRONIZATION ;;
;;;;;;;;;;;;;;;;;;;;;

(defun merlin/buffer-substring (start end)
   "Return content of buffer between two points or empty string if points are not valid"
   (if (< start end) (buffer-substring-no-properties start end) ""))

(defvar-local merlin--last-edit nil
   "Coordinates (start . end) of last edition or nil, to prevent error messages from flickering when cursor is around edition.")

(defun merlin--sync-edit (start end length)
  "Retract merlin--dirty-point, used when the buffer is edited."
  (when merlin-mode
    (setq merlin--last-edit (cons start end))
    (when (< (1- start) merlin--dirty-point)
      (setq merlin--dirty-point (1- start)))))

(defun merlin/sync ()
  "Synchronize buffer with merlin"
  (unless (eq merlin--dirty-point (point-max))
    (let ((point (merlin/unmake-point merlin--dirty-point)))
      (setq point (car (merlin--send-cursor-command
                         (list 'tell 'start 'at point))))
      (setq merlin--dirty-point (point-max))
      (merlin--send-cursor-command
        (list 'tell 'source-eof (merlin/buffer-substring point (point-max)))))))

(defun merlin/sync-async (k &optional kerr)
  "Asynchronous synchronization of buffer with merlin"
  (if (eq merlin--dirty-point (point-max)) (funcall k)
    (lexical-let ((k k) (kerr kerr))
      (merlin/send-command-async
        (list 'tell 'start 'at (merlin/unmake-point merlin--dirty-point))
        (lambda (answer)
          (let* ((point (car (merlin--parse-position answer)))
                 (source (merlin/buffer-substring point (point-max))))
            (merlin/send-command-async
              (list 'tell 'source-eof source)
              (lambda (answer)
                (setq merlin--dirty-point (point-max))
                (funcall k))
              kerr)))
        kerr))))

;;;;;;;;;;;;;;;;;;
;; ERROR REPORT ;;
;;;;;;;;;;;;;;;;;;

(defun merlin--chomp (str)
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
  "Show the error of the current line in the echo area.  If there is no error, do nothing."
  (when (and merlin-mode (not (current-message)))
    (let* ((errors (overlays-in (line-beginning-position) (line-end-position)))
           (err nil))
      (when (or (not merlin--last-edit)
                (not (or (= (point) (car merlin--last-edit))
                         (= (point) (cdr merlin--last-edit)))))
        (setq errors (remove nil (mapcar 'merlin--overlay-pending-error errors)))
        (setq err (merlin--error-at-position (point) errors))
        (when err (message "%s" (cdr (assoc 'message err))))))))

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
    (unless (eq point (point))
      (setq errors (overlays-at point))
      (setq errors (remove nil (mapcar 'merlin--overlay-pending-error errors))))
    (unless errors
      (setq point (merlin--overlay-previous-property-set (point-max) 'merlin-pending-error (point)))
      (setq errors (overlays-at point))
      (setq errors (remove nil (mapcar 'merlin--overlay-pending-error errors))))
    (setq err (merlin--error-at-position point errors))
    (if err (cons point err) nil)))

(defun merlin--error-next-cycle ()
  "Returns next error, cycling when reaching end of buffer"
  (let ((point (point)) (errors nil) (err nil))
    (setq point (merlin--overlay-next-property-set point 'merlin-pending-error))
    (when (eq point (point-max))
      (setq point (point-min))
      (setq errors (overlays-at point))
      (setq errors (remove nil (mapcar 'merlin--overlay-pending-error errors)))
      (unless errors
        (setq point (merlin--overlay-next-property-set (point-min) 'merlin-pending-error (point)))))
    (unless errors
      (setq errors (overlays-at point))
      (setq errors (remove nil (mapcar 'merlin--overlay-pending-error errors))))
    (setq err (merlin--error-at-position point errors))
    (if err (cons point err) nil)))

(defun merlin--after-save ()
  (when (and merlin-mode merlin-error-after-save) (merlin-error-check)))

(defadvice basic-save-buffer (after merlin--after-save activate)
  "The save hook is called only if buffer was modified, but user might want fresh errors anyway"
  (merlin--after-save))

(defun merlin-error-prev ()
  "Jump back to previous error."
  (interactive)
  (let ((old-errors merlin-erroneous-buffer))
    (merlin--error-check nil)
    (let ((err (merlin--error-prev-cycle)))
      (unless (or err merlin-erroneous-buffer) (message "No errors"))
      (when err
        (if (and merlin-error-check-then-move
                 (not (equal old-errors merlin-erroneous-buffer)))
            (message "(%d pending errors, use %s to jump)"
                     (length merlin-erroneous-buffer)
                     (substitute-command-keys "\\[merlin-error-prev]"))
          (goto-char (car err))
          (message "%s" (cdr (assoc 'message (cdr err))))
          (merlin--highlight (cdr (assoc 'bounds (cdr err))) 'next-error))))))

(defun merlin-error-next ()
  "Jump to next error."
  (interactive)
  (let ((old-errors merlin-erroneous-buffer))
    (merlin--error-check nil)
    (let ((err (merlin--error-next-cycle)))
      (unless (or err merlin-erroneous-buffer) (message "No errors"))
      (when err
        (if (and merlin-error-check-then-move
                 (not (equal old-errors merlin-erroneous-buffer)))
            (message "(%d pending errors, use %s to jump)"
                     (length merlin-erroneous-buffer)
                     (substitute-command-keys "\\[merlin-error-next]"))
          (goto-char (car err))
          (message "%s" (cdr (assoc 'message (cdr err))))
          (merlin--highlight (cdr (assoc 'bounds (cdr err))) 'next-error))))))

(defun merlin--error-warning-p (msg)
  "Tell if the message MSG is a warning."
  (string-match "^Warning" msg))

(defun merlin-error-reset ()
  "Clear error list."
  (interactive)
  (setq merlin-erroneous-buffer nil)
  (remove-overlays nil nil 'merlin-kind 'error))

(defun merlin--overlay (overlay)
  "Returns non-nil if OVERLAY is managed by merlin."
  (if overlay (overlay-get overlay 'merlin-kind) nil))

(defun merlin--overlay-pending-error (overlay)
  "Returns non-nil if OVERLAY is about a pending error."
  (if overlay (overlay-get overlay 'merlin-pending-error) nil))

(defun merlin--kill-error-if-edited (overlay is-after beg end &optional length)
  "Remove an error from the pending error lists if it is edited by the user."
  (when is-after (delete-overlay overlay)))

(defun merlin-transform-display-errors (errors)
  "Populate the error list with ERRORS, transformed into an emacs-friendly
form. Do display of error list."
  (setq errors (mapcar (lambda (err)
                         (let ((bounds (merlin--make-bounds err)))
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
                           (acons 'bounds bounds err)))
                       errors))
  (dolist (err errors)
    (let* ((bounds (cdr (assoc 'bounds err)))
           (overlay (make-overlay (car bounds) (cdr bounds))))
      (overlay-put overlay 'merlin-kind 'error)
      (overlay-put overlay 'merlin-pending-error err)
      (push #'merlin--kill-error-if-edited
            (overlay-get overlay 'modification-hooks))
      (when merlin-error-in-fringe
        (if (merlin--error-warning-p (cdr (assoc 'message err)))
            (merlin-add-display-properties overlay
                                           'question-mark
                                           "?"
                                           'merlin-compilation-warning-face)
          (merlin-add-display-properties overlay
                                         'exclamation-mark
                                         "!"
                                         'merlin-compilation-error-face)))
      overlay))
  errors)

(defun merlin--error-check (view-errors-p)
  "Check for errors.
Return t if there were not any or nil if there were.  Moreover, it displays the
errors in the fringe.  If VIEW-ERRORS-P is non-nil, display a count of them."
  (merlin-error-reset)
  (merlin/sync)
  (let* ((errors (merlin/send-command 'errors))
         (no-loc (remove-if (lambda (e) (assoc 'start e)) errors)))
    (setq errors (remove-if-not (lambda (e) (assoc 'start e)) errors))
    (unless merlin-report-warnings
      (setq errors (remove-if (lambda (e)
                                (merlin--error-warning-p (cdr (assoc 'message e))))
                              errors)))
    (setq merlin-erroneous-buffer (or errors no-loc))
    (dolist (e no-loc)
      (message "%s" (cdr (assoc 'message e))))
    (merlin-transform-display-errors errors)
    (when view-errors-p
      (if merlin-erroneous-buffer
          (message "(%d pending errors, use %s to jump)"
                   (length errors)
                   (substitute-command-keys "\\[merlin-error-next]"))
        (message "No errors")))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; ACTION WHEN IDLING ACTION ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun merlin--idle-action ()
  (merlin-show-error-on-current-line))

(defvar merlin--idle-timer nil)

;; Timer is started when merlin-mode is enabled for the first time.
(defun merlin--idle-timer ()
  (unless merlin--idle-timer
    (setq merlin--idle-timer
          (run-with-idle-timer 0.5 t 'merlin--idle-action))))

;;;;;;;;;;;;;;;;;;;;;;;;
;; COMPLETION HELPERS ;;
;;;;;;;;;;;;;;;;;;;;;;;;

(defun merlin/completion-entry-short-description (entry)
  "Return a short string describing the content a completion entry (e.g kind of
identifier, type of a value, etc)."
  (let* ((kind (cdr (assoc 'kind entry)))
         (desc (or  (cdr (assoc 'desc entry)) (cdr (assoc 'type entry))))
         (type (cond ((member kind '("Module" "module")) " <module>")
                     ((string-equal kind "Type") (format " [%s]" desc))
                     (t desc))))
    (replace-regexp-in-string "[\n ]+" " " type)))

(defun merlin/completion-entry-text (compl-prefix entry)
  "Return the text that should replace COMPL-PREFIX in the buffer if the user
chooses this completion entry.
COMPL-PREFIX is the prefix that was used to start completion."
  (let ((entry-name (cdr (assoc 'name entry))))
    (if merlin--dwimed entry-name (concat compl-prefix entry-name))))

(defun merlin/completion-prefix (ident)
  "Compute the prefix of IDENT.  The prefix of `Foo.bar' is `Foo.' and the
prefix of `bar' is `'."
  (car (merlin/completion-split-ident ident)))

(defun merlin/completion-split-ident (ident)
  "Split IDENT into a (cons prefix suffix). See merlin/completion-prefix."
  (let* ((l (split-string ident "\\."))
         (s (mapconcat 'identity (butlast l) "."))
         (suffix (if l (car (last l)) ident))
         (prefix (if (string-equal s "") s (concat s "."))))
    (cons prefix suffix)))

(defun merlin--completion-prepare-labels (labels suffix)
  ; Remove non-matching entry, adjusting optional labels if needed
  (setq labels (delete-if-not (lambda (x)
                                (let ((name (cdr (assoc 'name x))))
                                  (or (string-prefix-p suffix name)
                                      (when (equal (aref name 0) ??)
                                        (aset name 0 ?~)
                                        (string-prefix-p suffix name)))))
                              labels))
  (mapcar (lambda (x) (append x '((kind . "Label") (info . nil)))) labels))

(defun merlin/complete (ident)
  "Return the data for completion of IDENT, i.e. a list of lists of the form
  '(NAME TYPE KIND INFO)."
  (setq-local merlin--dwimed nil)
  (let* ((ident- (merlin/completion-split-ident ident))
         (suffix (cdr ident-))
         (prefix (car ident-))
         (pos    (merlin/unmake-point (point)))
         (data   (merlin/send-command
                   (if merlin-completion-with-doc
                     `(complete prefix ,ident at ,pos with doc)
                     `(complete prefix ,ident at ,pos))))
         ;; all classic entries
         (entries (cdr (assoc 'entries data)))
         ;; context is 'null or ('application ...)
         (context (cdr (assoc 'context data)))
         (application (and (listp context)
                           (equal (car context) "application")
                           (cadr context)))
         ;; Argument-type
         (expected-ty (and application
                           (not (string-equal "'_a"
                                  (cdr (assoc 'argument_type application))))
                           (cdr (assoc 'argument_type application))))
         ;; labels
         (labels (and application (cdr (assoc 'labels application)))))
    (setq labels (merlin--completion-prepare-labels labels suffix))
    ;; DWIM completion
    (when (and merlin-completion-dwim (not labels) (not entries))
      (setq data (merlin/send-command `(expand prefix ,ident at ,pos)))
      (setq entries (cdr (assoc 'entries data)))
      (setq-local merlin--dwimed t)
      (setq prefix ""))
    ;; Concat results
    (let ((result (append labels entries)))
      (if expected-ty
        (mapcar (lambda (x) (append x `((argument_type . ,expected-ty))))
                result)
        result))))

(defun merlin/completion-bounds ()
  "Returns a pair (start . end) of the content to complete"
  (let ((bounds (bounds-of-thing-at-point 'ocaml-atom)))
    (cons (if bounds (car bounds) (point))
          (point))))

;;;;;;;;;;;;;;;;;
;; TYPE BUFFER ;;
;;;;;;;;;;;;;;;;;

(defun merlin--count-lines (text)
  (let ((count 0)
        (pos   0))
    (while (and (<= count 8)
                (string-match "\n" text pos))
           (setq pos (match-end 0))
           (setq count (1+ count)))
    count))

(defun merlin/display-in-type-buffer (text)
  "Change content of type-buffer."
  (with-current-buffer (get-buffer-create merlin-type-buffer-name)
    (when (member major-mode '(nil fundamental-mode))
                                        ; Guess value for merlin-favourite-caml-mode
      (let ((caml-mode (or merlin-favourite-caml-mode
                           merlin-guessed-favorite-caml-mode)))
        (when caml-mode
          (with-demoted-errors "Error when setting up merlin type-buffer: %S"
            (funcall caml-mode)))))
    (erase-buffer)
    (insert text)
    (goto-char (point-min))))


;;;;;;;;;;;;;;;;;;;;;;;
;; EXPRESSION TYPING ;;
;;;;;;;;;;;;;;;;;;;;;;;

(defun merlin--type-expression (exp callback-if-success &optional callback-if-exn)
  "Get the type of EXP inside the local context."
  (when exp (merlin/send-command-async
             (list 'type 'expression (substring-no-properties exp)
                   'at (merlin/unmake-point (point)))
             callback-if-success callback-if-exn)))

(defun merlin--type-display (bounds type &optional quiet)
  "Display the type TYPE of the expression occuring at BOUNDS.
If QUIET is non nil, then an overlay and the merlin types can be used."
  (if (not type)
      (unless quiet (message "<no information>"))
    (let ((count (merlin--count-lines type)))
      (merlin/display-in-type-buffer type)
      (if (> count 8)
          (display-buffer merlin-type-buffer-name)
        (message "%s"
          (with-current-buffer merlin-type-buffer-name
            (font-lock-fontify-region (point-min) (point-max))
            (buffer-string))))
      (if (and (not quiet) bounds)
          (merlin--highlight bounds 'merlin-type-face)))))

(defun merlin--type-region ()
  "Show the type of the region."
  (lexical-let*
    ((substring  (merlin/buffer-substring (region-beginning) (region-end)))
     (on-success (lambda (type) (merlin--type-display nil type nil)))
     (on-error   (lambda (err)
                   (let ((msg (assoc 'message err))
                         (typ (assoc 'type err)))
                     (cond ((and typ (equal (cdr typ) "parser"))
                            (message "Error: the content of the region failed to parse."))
                           (msg (message "Error: %s" (cdr msg)))
                           (t
                            (message "Unexpected error")))))))
    (merlin--type-expression substring on-success on-error)))

(defun merlin-type-expr (exp)
  "Prompt the user for expression EXP, then show its type."
  (interactive "s# ")
  (merlin/sync)
  (let ((on-success (lambda (type) (merlin--type-display nil type nil)))
        (on-error   (lambda (err)
                      (let ((msg (assoc 'message err)))
                        (if msg (message "Error: %s" (cdr msg))
                          (message "unknown error"))))))
    (merlin--type-expression exp on-success on-error)))

;; TYPE ENCLOSING
(defvar merlin-type-enclosing-map
  (let ((keymap (make-sparse-keymap)))
    (define-key keymap (kbd "C-<up>") 'merlin-type-enclosing-go-up)
    (define-key keymap (kbd "C-<down>") 'merlin-type-enclosing-go-down)
    (define-key keymap (kbd "C-d") 'merlin--destruct-enclosing)
    (define-key keymap (kbd "C-w") 'merlin--copy-enclosing)
    keymap)
  "The local map to navigate type enclosing.")

(defun merlin--type-enclosing-reset ()
  "Clear enclosing information, necessary for destruct"
  (setq merlin-enclosing-types nil)
  (setq merlin-enclosing-offset -1))

(defun merlin--type-enclosing-reset-hooked ()
  "Reimplement on-exit logic from set-temporary-overlay-map for emacs pre 24.4"
  (let ((map merlin-type-enclosing-map))
    (unless (or (not (eq map (cadr overriding-terminal-local-map)))
                (eq this-command (lookup-key map (this-command-keys-vector))))
      (merlin--type-enclosing-reset)
      (remove-hook 'pre-command-hook 'merlin--type-enclosing-reset-hooked))))

(defun merlin--type-enclosing-query ()
  "Get the enclosings around point from merlin and sets MERLIN-ENCLOSING-TYPES."
  (merlin--type-enclosing-reset)
  (let ((types (merlin/send-command (list 'type 'enclosing 'at (merlin/unmake-point (point)))
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
                              (merlin--make-bounds obj))))
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

(defun merlin--copy-enclosing ()
  (interactive)
  (let ((data (elt merlin-enclosing-types merlin-enclosing-offset)))
    (when (cddr data)
      (message "Copied %s to kill-ring" (car data))
      (kill-new (car data)))))

(defun merlin--type-enclosing-after ()
  (when (and (fboundp 'set-temporary-overlay-map)
             merlin-arrow-keys-type-enclosing)
    (if (version< emacs-version "24.4")
        (progn
          (set-temporary-overlay-map merlin-type-enclosing-map t)
          (add-hook 'pre-command-hook 'merlin--type-enclosing-reset-hooked))
      (set-temporary-overlay-map merlin-type-enclosing-map t
                                 'merlin--type-enclosing-reset))))

(defun merlin-type-enclosing ()
  "Print the type of the expression under point (or of the region, if it exists)."
  (interactive)
  (merlin/sync)
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
	     (let ((start (merlin--point-of-pos (assoc 'start extent)))
		   (end (merlin--point-of-pos (assoc 'end extent))))
	       (or (and (> low start)
			(<= high end))
		   (and (< high end)
			(>= low start)))))
	   list))

(defun merlin-enclosing-expand ()
  "Select the construct enclosing point (or the region, if it
is active)."
  (interactive)
  (merlin/sync)
  (let* ((enclosing-extents
	  (merlin/send-command
	   `(enclosing ,(merlin/unmake-point (point)))))
	 (extents (if (use-region-p)
		      (merlin--find-extents enclosing-extents
					    (region-beginning)
					    (region-end))
		    (first enclosing-extents))))
    (if (not extents)
	(error "No enclosing construct")
      (merlin--goto-point (cdr (assoc 'start extents)))
      (push-mark (merlin--point-of-pos (cdr (assoc 'end extents)))
		 t t))))

;; Destruct
(defun merlin--replace-buff-portion (start stop txt)
  (let ((start (merlin--point-of-pos start))
        (stop  (merlin--point-of-pos stop)))
    (save-excursion
      (delete-region start stop)
      (goto-char start)
      (insert txt)
      (indent-region start (point)))))

(defun merlin--destruct-enclosing ()
  (interactive)
  (let* ((bounds (cdr (elt merlin-enclosing-types merlin-enclosing-offset)))
	 (start  (merlin/unmake-point (car bounds)))
	 (stop   (merlin/unmake-point (cdr bounds)))
	 (result
	  (merlin/send-command
	   (list 'case 'analysis 'from start 'to stop)
	   (lambda (errinfo)
	     (let ((msg (cdr (assoc 'message errinfo))))
	       (if msg
		   (message "%s" msg)
		 (message "bug in merlin: failed to destructure error")))))))
    (when result
      (let* ((loc (car result))
	     (start (cdr (assoc 'start loc)))
	     (stop (cdr (assoc 'end loc))))
	(merlin--replace-buff-portion start stop (cadr result))))
    (merlin--type-enclosing-reset)))

(defun merlin-destruct ()
  "Case analyse the current enclosing"
  (interactive)
  (merlin/sync)
  (if (not merlin-enclosing-types)
    (if (merlin--type-enclosing-query)
      (merlin--destruct-enclosing)
      (error "merlin: no result"))
    (merlin--destruct-enclosing)))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; PACKAGE, PROJECT AND FLAGS MANAGEMENT ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun merlin-get-packages ()
  "Get the list of available findlib package."
  (append (merlin/send-command '(find list)) nil))

(defun merlin-use (pkg)
  "Use PKG in the current session of merlin."
  (interactive
   (list (completing-read-multiple "Package to use: " (merlin-get-packages))))
  (unless (listp pkg) (setq pkg (list pkg)))
  (let* ((r (merlin/send-command (list 'find 'use pkg)))
         (failed (assoc 'failures r)))
    (when failed (message "%s" (cdr failed))))
  (merlin-error-reset))

(defun merlin--project-get ()
  "Returns a pair of two string lists (dot_merlins . failures) with a list of
.merlins file loaded and a list of error messages, if any error occured during
loading"
  (let* ((r (merlin/send-command '(project get)))
         (failed (cdr (assoc 'failures r)))
         (result (cdr (assoc 'result r)))
         (ret (cons result failed)))
    (setq merlin--project-cache ret)
    ret))

(defun merlin-goto-project-file ()
  "Goto the merlin file corresponding to the current file."
  (interactive)
  (let* ((dot_merlins (car (merlin--project-get)))
         (file (if (listp dot_merlins) (car dot_merlins) nil)))
    (if file
        (find-file-other-window file)
      (message "No project file for the current buffer."))))

(defun merlin-flags-set (flag-string)
  "Set user flags for current project."
  (interactive (let ((flags (merlin/send-command '(flags get))))
                 (list (read-string "Flags: " (mapconcat 'identity flags " ")))))
  (let* ((flag-list (split-string flag-string))
         (r (merlin/send-command (list 'flags 'set flag-list)))
         (failed (assoc 'failures r)))
    (when failed (message "%s" (cdr failed))))
  (merlin-error-reset))

;;;;;;;;;;;;
;; LOCATE ;;
;;;;;;;;;;;;

(defun merlin/locate (&optional ident)
  "Locate the identifier IDENT at point."
  (let ((result (merlin/send-command
                 (list 'locate (if ident (substring-no-properties ident) 'null)
                       merlin-locate-preference 'at (merlin/unmake-point (point))))))
    (unless result
      (error "Not found. (Check *Messages* for potential errors)"))
    (unless (listp result)
      (error result))
    result))

(defun merlin--locate-result (result)
  "Default actions after getting results from locate"
  (merlin--goto-file-and-point result)
  (when merlin-type-after-locate (merlin-type-enclosing)))

(defun merlin-locate-ident (ident)
  "Locate the inputed identifier"
  (interactive "s> ")
  (merlin/sync)
  (merlin--locate-result (merlin/locate ident)))

(defun merlin-locate ()
  "Locate the identifier under point"
  (interactive)
  (merlin/sync)
  (merlin--locate-result (merlin/locate)))

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

;;;;;;;;;;
;; JUMP ;;
;;;;;;;;;;

(defun merlin/jump (&optional target)
  "Jump to the TARGET"
  (let ((result (merlin/send-command
                  (list 'jump (if (equal target "") "fun let module match" target)
                        'at (merlin/unmake-point (point))))))
    (unless result
      (error "Not found. (Check *Messages* for potential errors)"))
    (unless (listp result)
      (error result))
    result))

(defun merlin-jump (&optional target)
  "Jump to enclosing fun, let, module or match.

Any combination of the above may be entered, separated by spaces, ex.:

fun let or module or module fun match

Empty string defaults to jumping to all these."
  (interactive "sfun, let, module or match > ")
  (merlin/sync-to-end)
  (merlin--goto-file-and-point (merlin/jump target)))

;;;;;;;;;;;;;;
;; DOCUMENT ;;
;;;;;;;;;;;;;;

(defun merlin--document-pos (ident)
  "Document the identifier IDENT at point and return the result."
  (merlin/send-command
   (list 'document (if ident (substring-no-properties ident) 'null)
         'at (merlin/unmake-point (point)))))

(defun merlin--document-pure (&optional ident)
  "Document the identifier IDENT at point."
  (let* ((raw-doc  (merlin--document-pos ident))
         (doc      (concat "(*" raw-doc "*)"))
         (nb-lines (merlin--count-lines doc)))
    (merlin/display-in-type-buffer doc)
    (if (> nb-lines 8)
        (display-buffer merlin-type-buffer-name)
      (message "%s"
        (with-current-buffer merlin-type-buffer-name
          (font-lock-fontify-region (point-min) (point-max))
          (buffer-string))))))

(defun merlin-document ()
  "Document the identifier under point"
  (interactive)
  (merlin/sync)
  (merlin--document-pure))

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
	(occ-buff (merlin--get-occ-buff))
	(positions
	 (mapcar (lambda (pos)
		   (merlin--goto-point (assoc 'start pos))
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
				  (merlin--goto-point start)
				  (point)))
		 (end-buf-pos (with-current-buffer src-buff
				(merlin--goto-point end)
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
           (switch-to-buffer (merlin--get-occ-buff)))
          ((equal merlin-occurrences-show-buffer 'other)
           (switch-to-buffer-other-window (merlin--get-occ-buff)))
          (t nil))))

(defun merlin-occurrences ()
  "List all occurrences of identifier under cursor in buffer."
  (interactive)
  (merlin/sync)
  (let* ((r (merlin/send-command
             (list 'occurrences 'ident 'at
                   (merlin/unmake-point (point))))))
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
  (let ((r (merlin/send-command
             (list 'boundary command 'at (merlin/unmake-point (point))))))
    (unless (equal r 'null)
      (merlin--goto-point (car r))
      (point))))

(defun merlin-phrase-next ()
  "Go to the beginning of the next phrase."
  (interactive)
  (merlin/sync)
  (merlin--phrase-goto 'next))

(defun merlin-phrase-prev ()
  "Go to the beginning of the previous phrase."
  (interactive)
  (merlin/sync)
  (merlin--phrase-goto 'prev))

(defun merlin-error-check ()
  "Update merlin to the end-of-file, reporting errors."
  (interactive)
  (when merlin-mode (merlin--error-check t)))

(defun merlin-project-check ()
  "Display loaded .merlin files and eventual errors."
  (interactive)
  (let* ((project (merlin--project-get))
         (dots (car project))
         (messages (cdr project))) ; failures list
    (add-to-list 'messages
                 (if dots (concat "Loaded .merlin files: "
                                  (mapconcat 'identity dots ", "))
                   "No .merlin loaded"))
    (message "%s" (mapconcat 'identity messages "\n"))))

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
      (message "%s" (merlin/send-command '(version)))))

(defun merlin-command ()
  "Return path of ocamlmerlin binary selected by configuration"
  (if (equal merlin-command 'opam)
    (concat (replace-regexp-in-string "\n$" ""
              (shell-command-to-string "opam config var bin"))
       "/ocamlmerlin")
    merlin-command))

;;;;;;;;;;;;;;;
;; DEBUGGING ;;
;;;;;;;;;;;;;;;

(defun merlin-dump (arg)
  (interactive "sWhat to dump: ")
  (let ((res (merlin/send-command (list 'dump arg))))
    (print res)))

;;;;;;;;;;;;;;;;
;; MODE SETUP ;;
;;;;;;;;;;;;;;;;

(defvar merlin-mode-map
  (let ((merlin-map (make-sparse-keymap))
        (merlin-menu-map (make-sparse-keymap))
        (merlin-show-type-map (make-sparse-keymap)))
    (define-key merlin-map (kbd "C-c C-x") 'merlin-error-next)
    (define-key merlin-map (kbd "C-c C-l") 'merlin-locate)
    (define-key merlin-map (kbd "C-c &"  ) 'merlin-pop-stack)
    (define-key merlin-map (kbd "C-c C-r") 'merlin-error-check)
    (define-key merlin-map (kbd "C-c C-t") 'merlin-type-enclosing)
    (define-key merlin-map (kbd "C-c C-d") 'merlin-destruct)
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
    (define-key merlin-menu-map [error]
      '(menu-item "Error check" merlin-error-check
                  :help "Check current buffer for any error."))
    (define-key merlin-menu-map [dot-merlin]
      '(menu-item "dot-merlin check" merlin-project-check
                  :help "Display status of '.merlin'."))
    (define-key merlin-menu-map [setflags]
      '(menu-item "Set flags" merlin-flags-set
                  :help "Set compiler flags for current project."))
    (define-key merlin-menu-map [restartmerlin]
      '(menu-item "Restart merlin" merlin-restart-process
                  :help "Restart merlin for the current buffer."))
    (define-key merlin-menu-map [versionmerlin]
      '(menu-item "Version" merlin-version
                  :help "Print the version of the merlin binary."))
    (define-key merlin-map [menu-bar merlin] (cons "Merlin" merlin-menu-map))
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
    (cons 'name (file-name-directory (expand-file-name (buffer-file-name (buffer-base-buffer)))))))

(defun merlin-setup ()
  "Set up a buffer for use with merlin."
  (interactive)
  (let* ((conf (merlin--grouping-function))
         (instance (lookup-default 'name conf "default")))
    (setq merlin-instance instance)
    ;; if there is not yet a merlin process
    (when (merlin-process-dead-p instance)
      (merlin-start-process merlin-default-flags conf))
    (add-to-list 'after-change-functions 'merlin--sync-edit)
    (merlin--idle-timer)
    ;; Synchronizing will only do parsing and no typing.
    ;; That should be fast enough that the user don't realize.
    ;; Having knowledge of the buffer content, merlin idle jobs will be able to preload
    ;; type information to make upcoming requests much faster.
    (merlin/sync)))

(defun merlin-can-handle-buffer ()
  "Simple sanity check (used to avoid running merlin on, e.g., completion buffer)."
  (cond ((equal (buffer-name) merlin-type-buffer-name) nil)
        ((buffer-file-name (buffer-base-buffer)) t)))

(defun merlin-view-log ()
  "Jump to the log file of merlin."
  (interactive)
  (let ((file (lookup-default 'logfile (merlin-process-data) nil)))
    (if file (find-file-other-window file)
      (message "No log file for this instance."))))

(defun merlin-lighter ()
  "Return the lighter for merlin which indicates the status of merlin process."
  (if (merlin-process-dead-p) " Merlin (DEAD)"
    (progn
      (let ((messages nil))
        (when merlin-report-errors-in-lighter
          (cond ((not merlin--project-cache) nil)
                ((cdr merlin--project-cache) (add-to-list 'messages "errors in .merlin"))
                ((not (car merlin--project-cache)) (add-to-list 'messages "no .merlin"))))
        (when merlin-erroneous-buffer
          (add-to-list 'messages "errors in buffer"))
        (when merlin-show-instance-in-lighter
          (add-to-list 'messages merlin-instance))
        (if messages (concat " Merlin (" (mapconcat 'identity messages ",") ")")
          " Merlin")))))

;;;###autoload
(define-minor-mode merlin-mode
  "Minor mode for interacting with a merlin process.
Runs a merlin process in the background and perform queries on it.

Short cuts:
\\{merlin-mode-map}"
  nil
  :lighter (:eval (merlin-lighter))
  :keymap merlin-mode-map
  (if merlin-mode
    ;; When enabling merlin
    (progn
      (if (not (merlin-can-handle-buffer))
        (merlin-mode -1)
        (merlin-setup)))
    ;; When disabling merlin
    (progn
      (when merlin-highlight-overlay
        (delete-overlay merlin-highlight-overlay))
      (remove-overlays nil nil 'merlin-kind 'highlight)
      (remove-overlays nil nil 'merlin-kind 'error))))

(provide 'merlin)

;; Load these after (provide 'merlin) because they (require 'merlin)
;;;###autoload
(eval-after-load 'company '(require 'merlin-company))
;;;###autoload
(eval-after-load 'auto-complete '(require 'merlin-ac))
;;;###autoload
(eval-after-load 'iedit '(require 'merlin-iedit))
(require 'merlin-cap)

;; Deprecated, remove at some point
(require 'merlin-compat)

;;; merlin.el ends here
