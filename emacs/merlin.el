;; Mode for Merlin


;; json is mandatory
(require 'json)
;; auto-complete is not
(require 'auto-complete nil 'noerror)

;;
;; Variables
;;
(defvar merlin-command "ocamlmerlin"
  "The command for ocamlmerlin in your installation")

(defvar merlin-process nil
  "The merlin process associated to the current buffer")
(defvar merlin-continuation nil
  "The callback to be called with the result of the next command")
(defvar merlin-point nil
  "Stores the point of last completion (beginning of the prefix)")
(defvar merlin-result nil
  "Temporary variables to store command results")
(defvar merlin-debug t
  "If true, logs the data sent and received from merlin")
(defvar merlin-type-buffer
  (get-buffer-create "*merlin types*")
  "The buffer to use to display types"
  )

(defvar merlin-cache nil "Merlin cache for completion")
(defvar merlin-prefix nil "Merlin prefix")
(defvar merlin-name nil "Merlin name")
(defvar merlin-error-overlay nil "Merlin overlay used for errors")

;; UTILS
(defun merlin-compute-prefix (ident)
  "Computes the prefix of an identifier. The prefix of Foo.bar is Foo. and the prefix of bar is \"\""
  (let ((l (butlast (split-string ident "\\."))))
    (let ((s (mapconcat 'identity l ".")))
      (if (string-equal s "") s (concat s ".")))))
(defun merlin-make-point (line col)
  "Creates a point from a couple line / col"
  (save-excursion
    (beginning-of-line)
    (goto-line line)
    (forward-char col)
    (point)))

(defun merlin-ident-under-point ()
  "Returns the ident under point in the current buffer"
  (let ((x (bounds-of-thing-at-point 'symbol)))
    (buffer-substring-no-properties (car x) (cdr x))))

;; PROCESS MANAGEMENT
(defun merlin-make-buffer-name ()
  "Returns the buffer name associated to the current buffer, for the merlin process"
  (concat "*"
	  (buffer-file-name nil)
	  " merlin *"))

(defun merlin-make-process-name ()
  "Returns the process name for the current buffer"
  (concat "merlin-" (buffer-file-name nil)))

(defun merlin-get-process ()
  "Returns the process of the current buffer"
  merlin-process)

(defun merlin-filter (process output)
  "The filter on merlin's output"
  (if merlin-debug (message output))
  (if merlin-continuation
      (let ((a (ignore-errors (json-read-from-string output))))
	(if a (funcall merlin-continuation a)))))

(defun merlin-wait-for-answer ()
  "Waits for merlin to answer"
  (if (not (accept-process-output (merlin-get-process) 0.1 nil nil))
      (merlin-wait-for-answer)))

(defun merlin-start-process ()
  "Start the merlin process"
  (let ((p (start-process (merlin-make-process-name)
			 (merlin-make-buffer-name)
			 merlin-command)))
    (set (make-local-variable 'merlin-process) p)
    (set-process-filter p 'merlin-filter)))

(defun merlin-send-command (name args callback)
  "Send a command to merlin. Callback is called with the sexp result of the command"
  (let ((string
	 (concat 
	  (json-encode 
	   (if args (append (list name) args) (list name)))
	  "\n")))
    (if merlin-debug (message string))
    (set (make-local-variable 'merlin-continuation) callback)
    (process-send-string (merlin-get-process) string)
    (merlin-wait-for-answer)
))

(defun merlin-send-command-sync (name args)
  "Same as `merlin-send-command' but is synchronous: it returns
the result of the command"
  (setq merlin-result nil)
  (merlin-send-command name args '(lambda (output) (setq merlin-result output)))
  (sleep-for 0.05)
  merlin-result)


;; SPECIAL CASE OF COMMANDS
(defun merlin-rewind ()
  "Rewind the knowledge of merlin of the current buffer to zero"
  (merlin-send-command "reset" nil nil))

(defun merlin-dump-env ()
  "Dump the environment"
  (merlin-send-command "dump" '("env")
		       '(lambda (sexp) 
			  (message "Env: %s" (append (elt sexp 1) nil)))))

(defun merlin-get-completion (ident)
  "Returns the completion for ident `ident'"
  (merlin-send-command-sync "complete" (list "prefix" ident)))

(defun merlin-tell-string (mode string)
  "Tell a string to merlin using `mode'"
  (merlin-send-command "tell" (list "struct" string) nil))

(defun merlin-flush-tell ()
  "Flush merlin teller"
  (merlin-send-command "tell" '("struct" nil) nil))

(defun merlin-tell-piece (mode start end)
  "Tell part of the current buffer to merlin using `mode'"
  (merlin-tell-string mode (buffer-substring start end)))


;; ERRORS
(defun merlin-remove-error-overlay ()
  "Remove the error overlay"
  (delete-overlay merlin-error-overlay))

(defun merlin-error-highlight (start end)
  "Add an overlay between `start' and `end' for errors"
  (setq merlin-overlay
	(make-overlay start end))
  (overlay-put merlin-overlay 'face 'next-error)
  (run-at-time "5 sec" nil 'merlin-remove-overlay)
  )
(defun merlin-handle-errors (errors)
  "Goes to the location of the first error and adds an overlay"
  (let ((message (cdr (assoc 'message errors)))
	(lbeginning (cdr (assoc 'line (cdr (assoc 'start errors)))))
	(cbeginning (cdr (assoc 'col (cdr (assoc 'start errors)))))
	(lend (cdr (assoc 'line (cdr (assoc 'end errors)))))
	(cend (cdr (assoc 'col (cdr (assoc 'end errors)))))
	(type (cdr (assoc 'type errors))))
    (goto-char (merlin-make-point lbeginning cbeginning))
    (merlin-highlight message (merlin-make-point lbeginning cbeginning)
		      (merlin-make-point lend cend))
    (message "%s: %s" type message)
))

(defun merlin-view-errors ()
  "View the errors of the data that have been fed to merlin"
  (merlin-send-command "errors" nil
		       '(lambda (output)
			  (if (> (length (elt output 1)) 0)
			      (merlin-handle-errors (elt (elt output 1) 0))
			    (message "ok")))))

(defun merlin-tell-previous-lines ()
  "Tell merlin the lines up to the point"
  (interactive)
  (merlin-rewind)
  (save-excursion
    (move-beginning-of-line nil)
    (merlin-tell-piece "struct" (point-min) (point)))
  (merlin-flush-tell)
  (merlin-view-errors)
)

;; COMPLETION
(defun merlin-extract-complete (prefix l)
  "Parses and format completion results"
  (mapcar '(lambda (c) 
	     (concat prefix
		     (cdr (assoc 'name c)) 
		     ": " 
		     (cdr (assoc 'desc c))
		     ))
	  (append l nil)))

(defun merlin-source-action ()
  "Called when the user has pressed RET on a completion candidate. Remove the garbage"
  (save-excursion
    (let ((endpoint (point)))
      (goto-char merlin-point)
      (search-forward ":")
      (backward-char 1)
      (delete-region (point) endpoint)))
)


    
(defun merlin-complete-identifier (ident)
  "Returns the formatted result of the completion of `ident'"
  (setq merlin-cache
	(merlin-extract-complete (merlin-compute-prefix ident) 
			   (elt (merlin-get-completion ident) 1))))

(defun merlin-source-init ()
  (merlin-complete-identifier ac-prefix))
  

(defvar merlin-ac-source
  '((init . merlin-source-init)
    (candidates . (lambda () merlin-cache))
    (action . merlin-source-action)
    (requires . 3)
    ))

(ac-define-source "merlin" merlin-ac-source)

;; Get the type of an element"
(defun merlin-get-type () 
  (interactive)
  (let ((sexp (merlin-get-completion (merlin-ident-under-point))))
    (if (= (length (elt sexp 1)) 0) 
	(message "nothing found for %s" (merlin-ident-under-point))
      (let ((ans (elt (elt sexp 1) 0)))
	(if (string-equal (cdr (assoc 'kind ans)) "Value")
	    (message "%s" (cdr (assoc 'desc ans)))
	  (progn
	    (display-buffer merlin-type-buffer)
	    (with-current-buffer merlin-type-buffer
	      (erase-buffer)
	      (insert (cdr (assoc 'info ans))))))))))

;; .merlin parsing
(defun merlin-add-path (kind dir)
  "Adds an item to a path in merlin"
  (merlin-send-command "path" (list "add" kind dir) nil))

(defun merlin-get-packages ()
  "Get the list of available findlib package"
  (setq merlin-packages nil)
  (append (elt (merlin-send-command-sync "find" '("list")) 1) nil))
(defun merlin-use (pkg)
  "Use a package in the current session of merlin"
  (interactive
   (list (completing-read "Package to use:" (merlin-get-packages))))
  (merlin-send-command "find" (list "use" pkg) nil))

(defun merlin-handle-line (buffer words)
  "Handles a line in a .merlin file"
  (with-current-buffer buffer
    (cond
     ((string-equal (elt words 0) "S") (merlin-add-path "source" (elt words 1)))
     ((string-equal (elt words 0) "B") (merlin-add-path "build" (elt words 1)))
     ((string-equal (elt words 0) "PKG") (merlin-use (elt words 1))))))
(defun merlin-parse ()
  "Parses a .merlin"
  (if (file-exists-p ".merlin")
      (progn
	(setq lines nil)
	(setq buf (current-buffer))
	(with-current-buffer (find-file ".merlin")
	  (goto-char (point-min))
	  (while (not (eq (point) (point-max)))
	    (let ((words (split-string (buffer-substring-no-properties (point-at-bol) (point-at-eol)))))
	      (merlin-handle-line buf words)
	      (forward-line))))
	(switch-to-buffer buf))))
    
;; Mode definition
(defun merlin-setup ()
  "Sets up a buffer for use with merlin"
  (interactive)
  (merlin-start-process)
  (auto-complete-mode)
  (setq ac-sources '(merlin-ac-source))
  (merlin-parse)
  (with-current-buffer merlin-type-buffer
    (tuareg-mode))
)
(define-minor-mode merlin-mode
  "Mode to use merlin tool inside OCaml tools."
  nil
  " merlin"
  '(("\C-i" . merlin-tell-previous-lines)
    ("\C-o" . merlin-get-type))
  :after-hook 'merlin-setup)
