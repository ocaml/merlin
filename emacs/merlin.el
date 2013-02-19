;; Mode for Merlin


;; json is mandatory
(require 'json)
;; auto-complete is not
(require 'auto-complete nil 'noerror)

;;
;; Faces
;;

(defface merlin-locked-face
  '(
    (((background dark)) :background "#222278")
    (t :background "#eaf8ff")
    )
  "Face for a region that merlin knows of."
  :group 'merlin-faces)


;;
;; Variables
;;
(defvar merlin-command "ocamlmerlin"
  "The command for ocamlmerlin in your installation")

(defvar merlin-process nil
  "The merlin process associated to the current buffer")
(defvar merlin-continuous-feed nil
  "If set to t, each time you hit RET, the line is fed to merlin")

(defvar merlin-pending-errors nil
  "Pending errors")
(defvar merlin-pending-errors-overlay nil
  "Overlays for the pending errors")
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

(defvar merlin-favourite-caml-mode
  'tuareg-mode
  "The OCaml mode to use for the *merlin types* buffer")

(defvar merlin-cache nil "Merlin cache for completion")
(defvar merlin-prefix nil "Merlin prefix")
(defvar merlin-name nil "Merlin name")
(defvar merlin-error-overlay nil "Merlin overlay used for errors")
(defvar merlin-overlay nil "Merlin overlay used for the lock zone")
(defvar merlin-buffer nil "Buffer for merlin input")
(defvar merlin-ready nil "Is reception done?")
(defvar merlin-idle-delay 1.0
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

(defun bounds-of-ocaml-atom-at-point ()
  "Return the start and end points of an ocaml atom near point. An ocaml atom
   is any string containing [a-zA-Z`.]"
  (save-excursion
    (skip-chars-backward "[a-zA-Z`.]")
    (if (looking-at "[a-zA-Z`.]+")
        (cons (point) (match-end 0)) ; returns the bounds
      nil))) ; no atom at point

(put 'ocamlatom 'bounds-of-thing-at-point
     'bounds-of-ocaml-atom-at-point)

(defun merlin-ident-under-point ()
  "Returns the ident under point in the current buffer"
  (let ((x (bounds-of-thing-at-point 'ocamlatom)))
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
	(if merlin-debug (merlin-debug (format "Received:\n%s\n----\n" merlin-buffer)))
	(setq merlin-result a)
	(setq merlin-ready t)))))

(defun merlin-wait-for-answer ()
  "Waits for merlin to answer"
  (while (or
	  (not (accept-process-output (merlin-get-process) 0.1 nil nil))
	  (not merlin-ready))
    t)
  (setq merlin-buffer nil)
  (setq merlin-ready nil))

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
    (process-send-string (merlin-get-process) string)
    (if merlin-debug (merlin-debug (format "Sending:\n%s\n---\n" string)))
    (merlin-wait-for-answer)
    merlin-result
))


(defun merlin-is-long (s)
  "Returns true if its parameter is long, ie. contains a new line"
  (string-match "sig" s))
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

(defun merlin-refresh ()
  "Refreshes merlin cmis"
  (interactive)
  (merlin-send-command "refresh" nil))

(defun merlin-get-completion (ident)
  "Returns the completion for ident `ident'"
  (merlin-send-command "complete" (list "prefix" ident "at" (merlin-unmake-point (point)))))

(defun merlin-tell-string (mode string)
  "Tell a string to merlin using `mode'"
  (merlin-send-command "tell" (list "struct" string)))

(defun merlin-flush-tell ()
  "Flush merlin teller"
  (merlin-send-command "tell" '("struct" nil)))

(defun merlin-tell-piece (mode start end)
  (merlin-tell-string mode (buffer-substring start end)))

(defun merlin-tell-piece-split (mode start end)
   "Tell part of the current buffer to merlin using `mode'"
   (save-excursion
     ;; tell lines 10 by 10
     (goto-char start)
     (forward-line 10)
     (let ((temp start))
       (while (< (point) end)
	 (merlin-tell-piece mode temp (point))
	 ;; at this point
	 ;; you might be wondering
	 ;; "yeah he got to send lines 10 by 10 because otherwise emacs (or json.el ?) can't handle that much data
	 ;;  how comes this sordid computation can be right ?"
	 (setq temp (point))
	 (forward-line 10))
       (merlin-tell-piece mode temp end))))
(defun merlin-seek (point)
  "Seeks merlin's point to `point'"
  (let ((data 
	 (merlin-is-return (merlin-send-command "seek" (list "position" (merlin-unmake-point point))))))
    (merlin-make-point data)))
    
  


;; ERRORS
(defun merlin-next-error ()
  "Jump to the next error"
  (interactive)
  (if merlin-pending-errors
      (let ((err (pop merlin-pending-errors)))
        (goto-char (merlin-make-point (cdr (assoc 'start err))))
        (merlin-error-highlight (merlin-make-point (cdr (assoc 'start err)))
                                (merlin-make-point (cdr (assoc 'end err))))
        (setq merlin-idle-point (point))
        (message (cdr (assoc 'message err))))
    (message "no more errors")))

(defun merlin-remove-error-overlay ()
  "Remove the error overlay"
  (delete-overlay merlin-error-overlay))

(defun merlin-error-highlight (start end)
  "Add an overlay between `start' and `end' for errors"
  (setq merlin-error-overlay (make-overlay start end))
  (overlay-put merlin-error-overlay 'face 'next-error)
  (run-at-time "1 sec" nil 'merlin-remove-error-overlay)
  )
;; (defun merlin-handle-errors (errors)
;;   "Goes to the location of the first error and adds an overlay"
;;   (let ((message (cdr (assoc 'message errors)))
;; 	(beg (cdr (assoc 'start errors)))
;; 	(end (cdr (assoc 'end errors)))
;; 	(type (cdr (assoc 'type errors))))
;;     (goto-char (merlin-make-point beg))
;;     ;; make the idle thread shut up
;;     (setq merlin-idle-point (point))
;;     (merlin-error-highlight (merlin-make-point beg)
;; 		      (merlin-make-point end))
;;     (message "%s: %s" type message)
;; ))

(defun merlin-delete-error-overlays ()
  "Removes error overlays"
  (mapc '(lambda (over) (delete-overlay over)) merlin-pending-errors-overlay)
  (setq merlin-pending-errors-overlay nil))

(defun merlin-handle-errors (errors)
;  (setq merlin-error-overlay (make-overlay start end))
;  (overlay-put merlin-error-overlay 'face 'next-error)
  (merlin-delete-error-overlays)
  (setq merlin-pending-errors (append errors nil))
  (setq merlin-pending-errors-overlay 
        (mapcar '(lambda (err)
                   (let ((overlay (make-overlay
                                   (merlin-make-point (cdr (assoc 'start err)))
                                   (merlin-make-point (cdr (assoc 'end err))))))
                     (overlay-put overlay 'face 'next-error)
                     overlay)) errors))
  (message "(pending error, use C-c C-x to jump)"))

(defun merlin-view-errors (view-errors-p)
  "View the errors of the data that have been fed to merlin"
  (let ((output (merlin-send-command "errors" nil)))
    (if (> (length (elt output 1)) 0)
	(progn
          (if view-errors-p
              (merlin-handle-errors (elt output 1)))
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

(defun merlin-update-point (point view-errors-p)
  "Moves the merlin point to the given point. This functions compares its arguments
with the current position where merlin stops. It updates the merlin state by doing two things:
- either retract merlin's knowledge if `point' < `merlin-lock-point'
- or send the region between `merlin-lock-point' and `point'"
  (merlin-delete-error-overlays)
  (setq merlin-lock-point (merlin-retract-to point))
  (merlin-tell-piece-split "struct" merlin-lock-point point)
  (merlin-flush-tell)
  (if (merlin-view-errors view-errors-p)
      (setq merlin-lock-point point)
    (let ((msg (current-message)))
      (setq merlin-lock-point (merlin-seek merlin-lock-point))
      (message msg)))
  (merlin-update-overlay)
)    
  
(defun merlin-point (view-errors-p)
  "Tell merlin the lines up to the current point"
  (interactive)
  (merlin-update-point (point) view-errors-p)
)
(defun merlin-check-synchronize (&optional clean)
  "If merlin point is before the end of line send everything up to the end of line"
  (interactive)
  (let ((p merlin-lock-point))
    (if (> (point-at-eol) merlin-lock-point)
        (merlin-update-point (point-at-eol) nil))
    (if (and t (< merlin-lock-point p))
        (merlin-update-point p nil))))

(defun merlin-edit (start end length)
  (if (< start merlin-lock-point)
      (progn
        (message "muhaha")
        (merlin-update-point start nil))))
;; COMPLETION
(defun merlin-extract-complete (prefix l)
  "Parses and format completion results"
  (mapcar `(lambda (c) 
	     (if merlin-completion-types
		 (format "%s%s: %s" ,prefix
			 (cdr (assoc 'name c))
			 (cdr (assoc 'desc c)))
	       (concat ,prefix (cdr (assoc 'name c)))))
	  (append l nil)))

(defun merlin-source-action ()
  "Called when the user has pressed RET on a completion candidate. Remove the garbage"
  (if merlin-completion-types
      (save-excursion
	(let ((endpoint (point)))
	  (goto-char merlin-completion-point)
	  (search-forward ":")
	  (backward-char 1)
	  (delete-region (point) endpoint))
        ;; workaround for merlin-enter
        (merlin-check-synchronize))))


    
(defun merlin-complete-identifier (ident)
  "Returns the formatted result of the completion of `ident'"
  (setq merlin-cache nil)
  (merlin-extract-complete (merlin-compute-prefix ident) (elt (merlin-get-completion ident) 1))
  (setq merlin-cache
	(merlin-extract-complete (merlin-compute-prefix ident) 
			   (elt (merlin-get-completion ident) 1)))
  )

(defun merlin-source-init ()
  (merlin-check-synchronize)
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

(defun merlin-type-of-expression-local (exp)
  "Get the type of an expression inside the local context"
  (merlin-is-return (merlin-send-command "type" 
					 (list "expression" exp "at" (merlin-unmake-point (point))))))

(defun merlin-type-of-expression-global (exp)
  "Get the type of an expression globally"
  (merlin-is-return (merlin-send-command "type" (list "expression" exp))))


(defun merlin-type-of-expression (exp)
  "Get the type of an expression by using `merlin-type-of-expression-local',
and if it fails, it uses `merlin-type-of-expression-global'"
  (or
   (merlin-type-of-expression-local exp)
   (merlin-type-of-expression-global exp)))
(defun merlin-show-type (name typ)
  "Show the given type. If typ is nil, nothing is done"
  (if typ
      (if (not (merlin-is-long typ))
	  (message "%s : %s" name typ)
	(progn
	  (display-buffer merlin-type-buffer)
	  (with-current-buffer merlin-type-buffer
	    (erase-buffer)
	    (insert typ))))))

(defun merlin-show-type-of-region ()
  "Show the type of the region"
  (interactive)
  (let ((exp (buffer-substring-no-properties 
	       (region-beginning) 
	       (region-end))))
    (if exp
	(merlin-show-type exp (merlin-type-of-expression exp)))))

(defun merlin-show-type-of-point-quiet ()
  "Show the type of the identifier under the point if it is short (a value)"
  (merlin-check-synchronize t)
  (let ((ident (merlin-ident-under-point)))
    (if ident
	(let ((typ (merlin-type-of-expression ident)))
	  (if (and typ (not (merlin-is-long typ)))
	      (message "%s : %s" ident typ))))))

(defun merlin-show-type-of-point (arg) 
  "Show the type of the identifier under the point. If it is called with a prefix argument, then show the type of the region."
  (merlin-check-synchronize)
  (interactive "p")
  (if (> arg 1)
      (merlin-show-type-of-region)
    (let ((ident (merlin-ident-under-point)))
      (if ident
	  (merlin-show-type ident (merlin-type-of-expression ident))))))

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

(defun merlin-enter ()
  "Tries to update the merlin point and fail silently"
  (interactive)
  (newline)
  (if merlin-continuous-feed
      (merlin-point nil))) 
(defun merlin-to-point ()
  "Updates the merlin to the current point, reporting error"
  (interactive)
  (merlin-point t))

;; Mode definition
(defvar merlin-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd "C-c <C-return>") 'merlin-to-point)
    (define-key map (kbd "C-c C-t") 'merlin-show-type-of-point)
    (define-key map (kbd "C-c l") 'merlin-use)
    (define-key map (kbd "C-c C-x") 'merlin-next-error)
    (define-key map (kbd "C-c C-r") 'merlin-rewind)
    (define-key map (kbd "C-c C-u") 'merlin-refresh)
    (define-key map (kbd "RET") 'merlin-enter)
    map
    ))

(defun merlin-setup ()
  "Sets up a buffer for use with merlin"
  (interactive)
  (progn
    (merlin-start-process)
    (auto-complete-mode)
    (set (make-local-variable 'merlin-lock-point) (point-min))
    (set (make-local-variable 'merlin-buffer) nil)
    (set (make-local-variable 'merlin-result) nil)
    (set (make-local-variable 'merlin-idle-point) nil)
    (set (make-local-variable 'merlin-completion-point) nil)
    (set (make-local-variable 'merlin-ready) nil)
    (set (make-local-variable 'merlin-pending-errors) nil)
    (set (make-local-variable 'merlin-pending-errors-overlay) nil)
    (set (make-local-variable 'merlin-overlay) nil)
    (set (make-local-variable 'merlin-prefix) nil)
    (set (make-local-variable 'merlin-error-prefix) nil)
    (setq ac-sources '(merlin-ac-source))
    (add-to-list 'after-change-functions 'merlin-edit)
    (set-process-query-on-exit-flag (merlin-get-process) nil)
    (merlin-parse)
    (if (and (> merlin-idle-delay 0.) (not merlin-idle-timer))
	(setq merlin-idle-timer
              (run-with-idle-timer merlin-idle-delay t 'merlin-idle-hook)))
    (with-current-buffer merlin-type-buffer
      (funcall merlin-favourite-caml-mode))))
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
      (merlin-delete-error-overlays)
      (kill-buffer (merlin-make-buffer-name))
)))


(provide 'merlin)
