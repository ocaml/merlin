;; Mode for Merlin


;; json is mandatory
(require 'json)
;; auto-complete is not
(require 'auto-complete nil 'noerror)

;;
;; Faces
;;

(defface merlin-locked-face
  '((t :background "#eaf8ff"))
  "Face for a region that merlin knows of."
  :group 'merlin-faces)


;;
;; Variables
;;
(defvar merlin-command "ocamlmerlin"
  "The command for ocamlmerlin in your installation")

(defvar merlin-process nil
  "The merlin process associated to the current buffer")
(defvar merlin-continuation nil
  "The callback to be called with the result of the next command")
(defvar merlin-completion-point nil
  "Stores the point of last completion (beginning of the prefix)")
(defvar merlin-idle-point nil
  "Position of the last time we printed the type of point")
(defvar merlin-lock-point nil
  "Position up to which merlin knows about")
(defvar merlin-result nil
  "Temporary variables to store command results")
(defvar merlin-completion-types t
  "Prints the types of the variables during completion")
(defvar merlin-debug nil
  "If true, logs the data sent and received from merlin")
(defvar merlin-type-buffer
  (get-buffer-create "*merlin types*")
  "The buffer to use to display types"
  )

(defvar merlin-cache nil "Merlin cache for completion")
(defvar merlin-prefix nil "Merlin prefix")
(defvar merlin-name nil "Merlin name")
(defvar merlin-error-overlay nil "Merlin overlay used for errors")
(defvar merlin-overlay nil "Merlin overlay used for the lock zone")
(defvar merlin-buffer nil "Buffer for merlin input")
(defvar merlin-ready nil "Is reception done?")
(defvar merlin-idle-delay 0.50
  "Number of seconds of idle time to wait before printing the type of the ident under the point.
If user input arrives before this interval of time has elapsed
after the last input, no documentation will be printed.

If the timer is zero or negative, nothing is done."
)

(defvar merlin-idle-timer nil
  "The timer used to print the type of the expression under point")

;; UTILS

(defun merlin-debug (s)
  (with-current-buffer (merlin-make-buffer-name)
    (insert s)))

(defun merlin-compute-prefix (ident)
  "Computes the prefix of an identifier. The prefix of Foo.bar is Foo. and the prefix of bar is \"\""
  (let ((l (butlast (split-string ident "\\."))))
    (let ((s (mapconcat 'identity l ".")))
      (if (string-equal s "") s (concat s ".")))))
(defun merlin-make-point (data)
  "Creates a point from a couple line / col"
  (save-excursion
    (beginning-of-line)
    (goto-line (cdr (assoc 'line data)))
    (forward-char (cdr (assoc 'col data)))
    (point)))

(defun merlin-unmake-point (point)
  "Destructs the given point to line / col"
  (save-excursion
    (goto-char point)
    (list
      (cons 'line (line-number-at-pos nil))
      (cons 'col (current-column)))))

(defun merlin-ident-under-point ()
  "Returns the ident under point in the current buffer"
  (let ((x (bounds-of-thing-at-point 'symbol)))
    (if x
	(buffer-substring-no-properties (car x) (cdr x))
      nil)))

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
  (setq merlin-buffer (concat merlin-buffer output))
  (let ((a (ignore-errors (json-read-from-string merlin-buffer))))
  (if a
      (progn
	(if merlin-debug (merlin-debug (format "< %s" merlin-buffer)))
	(setq merlin-result a)
	(setq merlin-ready t)))))

(defun merlin-wait-for-answer ()
  "Waits for merlin to answer"
  (if (or
       (not (accept-process-output (merlin-get-process) 0.1 nil nil))
       (not merlin-ready))
      (merlin-wait-for-answer)
    (progn
      (setq merlin-buffer nil)
      (setq merlin-ready nil))))

(defun merlin-start-process ()
  "Start the merlin process"
  (let ((p (start-process (merlin-make-process-name)
			 (merlin-make-buffer-name)
			 merlin-command)))
    (set (make-local-variable 'merlin-process) p)
    (set-process-filter p 'merlin-filter)))

(defun merlin-send-command (name args)
  "Send a command to merlin. Returns the result"
  (let ((string
	 (concat 
	  (json-encode 
	   (if args (append (list name) args) (list name)))
	  "\n")))
    (if merlin-debug (merlin-debug (format "> %s" string)))
    (process-send-string (merlin-get-process) string)
    (merlin-wait-for-answer)
    merlin-result
))


(defun merlin-is-long (s)
  "Returns true if its parameter is long, ie. contains a new line"
  (string-match "\n" s))
;; SPECIAL CASE OF COMMANDS
(defun merlin-is-return (data)
  "Returns the actual data of a response or nil if there was an error"
  (if (string-equal (elt data 0) "return")
      (elt data 1)
    nil))
		    
(defun merlin-rewind ()
  (interactive)
  "Rewind the knowledge of merlin of the current buffer to zero"
  (merlin-send-command "reset" nil)
  (setq merlin-lock-point (point-min))
  (merlin-update-overlay)
)

(defun merlin-dump-env ()
  "Dump the environment"
  (interactive)
  (merlin-seek (point))
  (message "Env: %s"
	   (append (elt (merlin-send-command "dump" '("env")) 1) nil))
  (merlin-seek merlin-lock-point))

(defun merlin-get-completion (ident)
  "Returns the completion for ident `ident'"
  (merlin-send-command "complete" (list "prefix" ident)))

(defun merlin-tell-string (mode string)
  "Tell a string to merlin using `mode'"
  (merlin-send-command "tell" (list "struct" string)))

(defun merlin-flush-tell ()
  "Flush merlin teller"
  (merlin-send-command "tell" '("struct")))

(defun merlin-tell-piece (mode start end)
  "Tell part of the current buffer to merlin using `mode'"
  (merlin-tell-string mode (buffer-substring start end)))

(defun merlin-seek (point)
  "Seeks merlin's point to `point'"
  (let ((data 
	 (merlin-is-return (merlin-send-command "seek" (list "position" (merlin-unmake-point point))))))
    (merlin-make-point data)))
    
  


;; ERRORS
(defun merlin-remove-error-overlay ()
  "Remove the error overlay"
  (delete-overlay merlin-error-overlay))

(defun merlin-error-highlight (start end)
  "Add an overlay between `start' and `end' for errors"
  (setq merlin-error-overlay (make-overlay start end))
  (overlay-put merlin-error-overlay 'face 'next-error)
  (run-at-time "1 sec" nil 'merlin-remove-error-overlay)
  )
(defun merlin-handle-errors (errors)
  "Goes to the location of the first error and adds an overlay"
  (let ((message (cdr (assoc 'message errors)))
	(beg (cdr (assoc 'start errors)))
	(end (cdr (assoc 'end errors)))
	(type (cdr (assoc 'type errors))))
    (goto-char (merlin-make-point beg))
    ;; make the idle thread shut up
    (setq merlin-idle-point (point))
    (merlin-error-highlight (merlin-make-point beg)
		      (merlin-make-point end))
    (message "%s: %s" type message)
))

(defun merlin-view-errors ()
  "View the errors of the data that have been fed to merlin"
  (let ((output (merlin-send-command "errors" nil)))
    (if (> (length (elt output 1)) 0)
	(progn
	  (merlin-handle-errors (elt (elt output 1) 0))
	  nil)
      (progn
	(message "ok")
	t))))
 

(defun merlin-retract-to (point)
  "Retract merlin's view to `point'"
  (merlin-seek point))

(defun merlin-update-overlay ()
  (if merlin-overlay
      (delete-overlay merlin-overlay))
  (setq merlin-overlay (make-overlay (point-min) merlin-lock-point))
  (overlay-put merlin-overlay 'face 'merlin-locked-face))

(defun merlin-update-point (point)
  "Moves the merlin point to the given point. This functions compares its arguments
with the current position where merlin stops. It updates the merlin state by doing two things:
- either retract merlin's knowledge if `point' < `merlin-lock-point'
- or send the region between `merlin-lock-point' and `point'"
  (if (< point merlin-lock-point)
      (progn 
	(message "Going to retract your mother")
	(setq merlin-lock-point (merlin-retract-to point)))
    (progn
      (merlin-tell-piece "struct" merlin-lock-point point)
      (merlin-flush-tell)
      (if (merlin-view-errors)
	  (setq merlin-lock-point (point))
	(let ((msg (current-message)))
	  (merlin-seek merlin-lock-point)
	  (message msg)))))
  (merlin-update-overlay)
)    
  
(defun merlin-point ()
  "Tell merlin the lines up to the current point"
  (interactive)
  (merlin-update-point (point))
)
(defun merlin-synchronize-point ()
  "Synchronize emacs and merlin wrt point"
  (interactive)
  (setq merlin-point (merlin-make-point 
		      (merlin-is-return (merlin-send-command "seek" '("position")))))
  (merlin-update-overlay))
(defun merlin-edit (start end length)
  (if (< start merlin-lock-point)
      (merlin-update-point start)))
;; COMPLETION
(defun merlin-extract-complete (prefix l)
  "Parses and format completion results"
  (mapcar `(lambda (c) 
	    (concat ,prefix
		    (cdr (assoc 'name c)) 
		    ": " 
		    (cdr (assoc 'desc c))
		    ))
	  (append l nil)))

(defun merlin-source-action ()
  "Called when the user has pressed RET on a completion candidate. Remove the garbage"
  (save-excursion
    (let ((endpoint (point)))
      (goto-char merlin-completion-point)
      (search-forward ":")
      (backward-char 1)
      (delete-region (point) endpoint)))
)


    
(defun merlin-complete-identifier (ident)
  "Returns the formatted result of the completion of `ident'"
  (setq merlin-cache nil)
  (merlin-extract-complete (merlin-compute-prefix ident) (elt (merlin-get-completion ident) 1))
  (setq merlin-cache
	(merlin-extract-complete (merlin-compute-prefix ident) 
			   (elt (merlin-get-completion ident) 1)))
  )

(defun merlin-source-init ()
  (setq merlin-completion-point ac-point)
  (merlin-complete-identifier ac-prefix))
  

(defvar merlin-ac-source
  '((init . merlin-source-init)
    (candidates . (lambda () merlin-cache))
    (action . merlin-source-action)
    (requires . 3)
    ))

(ac-define-source "merlin" merlin-ac-source)

;; Get the type of an element"
(defun merlin-trim (s)
  (replace-regexp-in-string "\n\\'" "" s))
(defun merlin-type-of-expression (exp)
  "Get the type of an expression"
  (let ((ans (merlin-send-command "type" (list "expression" exp))))
    (if (string-equal (elt ans 0) "return")
	(elt ans 1)
      nil)))
  
(defun merlin-get-type ()
  (let ((sexp (merlin-get-completion (merlin-ident-under-point))))
    (if (= (length (elt sexp 1)) 0)
	nil
      (merlin-trim (elt (elt sexp 1) 0)))))

(defun merlin-show-type-minibuffer ()
  (let ((ident (merlin-ident-under-point)))
    (let ((typ (merlin-type-of-expression ident)))
      (if (and typ (not (merlin-is-long typ)))
	  (message "%s : %s" ident typ)))))
(defun merlin-show-type () 
  (interactive)
  (let ((ans (merlin-type-of-expression (merlin-ident-under-point))))
    (if (not ans)
	(message "nothing found for %s" (merlin-ident-under-point))
      (if (not (merlin-is-long ans))
	  (message "%s : %s" (merlin-ident-under-point) ans)
	(progn
	  (display-buffer merlin-type-buffer)
	  (with-current-buffer merlin-type-buffer
	    (erase-buffer)
	    (insert ans)))))))

;; .merlin parsing
(defun merlin-add-path (kind dir)
  "Adds an item to a path in merlin"
  (merlin-send-command "path" (list "add" kind dir)))

(defun merlin-get-packages ()
  "Get the list of available findlib package"
  (setq merlin-packages nil)
  (append (elt (merlin-send-command "find" '("list")) 1) nil))
(defun merlin-use (pkg)
  "Use a package in the current session of merlin"
  (interactive
   (list (completing-read "Package to use:" (merlin-get-packages))))
  (merlin-send-command "find" (list "use" pkg)))

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

;; Idle 
(defun merlin-idle-hook ()
  (if (<= merlin-idle-delay 0.)
      (cancel-timer merlin-idle-timer)
    (if (and merlin-mode
	     (not (eq (point) merlin-idle-point)))
	(progn
	  (merlin-show-type-of-point-quiet)
	  (setq merlin-idle-point (point))))))

;; Mode definition
(defvar merlin-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd "C-c <C-return>") 'merlin-point)
    (define-key map (kbd "C-c C-t") 'merlin-show-type)
    (define-key map (kbd "C-c e") 'merlin-dump-env)
    (define-key map (kbd "C-c C-r") 'merlin-rewind)
    map
    ))

(defun merlin-setup ()
  "Sets up a buffer for use with merlin"
  (interactive)
  (progn
    (merlin-start-process)
    (auto-complete-mode)
    (set (make-local-variable 'merlin-lock-point) (point-min))
    (setq ac-sources '(merlin-ac-source))
    (add-to-list 'after-change-functions 'merlin-edit)
    (set-process-query-on-exit-flag (merlin-get-process) nil)
    (merlin-parse)
    (if (> merlin-idle-delay 0.)
	(setq merlin-idle-timer
	      (run-with-idle-timer merlin-idle-delay t 'merlin-idle-hook)))
    (with-current-buffer merlin-type-buffer
      (tuareg-mode))))
(define-minor-mode merlin-mode
  "Mode to use merlin tool inside OCaml tools."
  nil
  " merlin"
  :keymap merlin-mode-map
  (if merlin-mode 
      (merlin-setup)
    (progn
      (process-send-eof (merlin-get-process))
      (delete-process (merlin-get-process))
      (cancel-timer merlin-idle-timer)
      (delete-overlay merlin-overlay)
)))
