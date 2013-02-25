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
(defvar merlin-enclosing-overlay nil
  "Stores the overlay during showing enclosing types")
(defvar merlin-enclosing-types nil
  "List containing the enclosing type")
(defvar merlin-enclosing-offset nil
  "Current offset in `merlin-enclosing-types'")
(defvar merlin-last-point-type nil
  "Last position where the user hit C-c C-t")

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
(defvar merlin-type-overlay nil "Merlin overlay used for type-checking")
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
  (let* ((l (butlast (split-string ident "\\.")))
         (s (mapconcat 'identity l ".")))
    (if (string-equal s "") s (concat s "."))))

(defun merlin-make-point (data)
  "Creates a point from a couple line / col"
  (save-excursion
    (beginning-of-line)
    (goto-line (cdr (assoc 'line data)))
    (forward-char (cdr (assoc 'col data)))
    (point)))
(defun merlin-make-bounds (data)
  "From a json object {\"start\": loc; \"end\": loc'}
returns (loc . loc')"
  (cons
   (merlin-make-point (cdr (assoc 'start data)))
   (merlin-make-point (cdr (assoc 'end data)))))

(defun merlin-unmake-point (point)
  "Destructs the given point to line / col"
  (save-excursion
    (goto-char point)
    (list
      (cons 'line (line-number-at-pos nil))
      (cons 'col (current-column)))))

(defun bounds-of-ocaml-atom-at-point ()
  "Return the start and end points of an ocaml atom near point. An ocaml atom
   is any string containing [a-z_0-9A-Z`.]"
  (save-excursion
    (skip-chars-backward "[a-z_0-9A-Z`.]")
    (if (looking-at "[a-z_0-9A-Z`.]+")
        (cons (point) (match-end 0)) ; returns the bounds
      nil))) ; no atom at point

(put 'ocamlatom 'bounds-of-thing-at-point
     'bounds-of-ocaml-atom-at-point)

; overlay management
(defun merlin-create-overlay (var bounds face timer)
  "Creates an overlay in the current buffer starting at `start', ending at `end',
using `face' and storing it in `var'. If `timer' is non-nil, the overlay is to disappear after `timer' seconds
`timer' is a string that can be understood by `run-at-time' (eg. \"1 sec\")"
  (if (symbol-value var) (delete-overlay (symbol-value var)))
  (set var (make-overlay (car bounds) (cdr bounds)))
  (overlay-put (symbol-value var) 'face face)
  (if timer
      (run-at-time timer nil `(lambda ()
                                (delete-overlay ,var)
                                (set ,var nil)))))
      
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
  (merlin-send-command "tell" (list mode string)))

(defun merlin-flush-tell ()
  "Flush merlin teller"
  (merlin-send-command "tell" '("struct" nil)))

(defun merlin-get-position ()
  "Get the current position of merlin"
  (merlin-make-point
   (merlin-is-return
    (merlin-send-command "seek" '("position")))))

(defun merlin-seek (point)
  "Seeks merlin's point to `point'"
  (let ((data 
	 (merlin-is-return (merlin-send-command "seek" (list "position" (merlin-unmake-point point))))))
    (merlin-make-point data)))
    
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

(defun merlin-tell-till-end-of-phrase ()
  "Tell merlin the buffer until the end of phrase.
It proceeds by telling (with the end mode) each line until it returns true or until we are at the end of the buffer"
  (let ((temp-point (point))
        (end-p nil))
    (forward-line 1)
    (while (and (not end-p) (< (point) (point-max)))
      (if (equal ;; this is not tautological since value is never nil
           (merlin-is-return ;; tell end returned true => we are done
            (merlin-tell-piece "end" temp-point (point)))
           t)
          (progn
            (setq end-p t))
        (progn
            (setq temp-point (point))
            (forward-line 1)
            (end-of-line))))
    ;; End of buffer
    (if (not end-p)
        (progn
          (merlin-send-command "tell" '("end" nil))
          (merlin-get-position))
      (merlin-retract-to (merlin-get-position)))))

      
      
  


;; ERRORS
(defun merlin-next-error ()
  "Jump to the next error"
  (interactive)
  (if merlin-pending-errors
      (let ((err (pop merlin-pending-errors)))
        (goto-char (merlin-make-point (cdr (assoc 'start err))))
        (if merlin-pending-errors-overlay
            (delete-overlay (pop merlin-pending-errors-overlay)))
        (merlin-create-overlay 'merlin-error-overlay 
                               (merlin-make-bounds err)
                               'next-error
                               "2 sec")
        (setq merlin-idle-point (point))
        (if merlin-pending-errors
            (message "%s (%d more errors)" 
                     (cdr (assoc 'message err))
                     (length merlin-pending-errors)
                     )
          (message "%s" (cdr (assoc 'message err)))))
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

(defun merlin-delete-error-overlays ()
  "Removes error overlays"
  (mapc '(lambda (over) (delete-overlay over)) merlin-pending-errors-overlay)
  (setq merlin-pending-errors-overlay nil))

(defun merlin-handle-errors (errors)
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
              (merlin-handle-errors (nreverse (append (elt output 1) nil))))
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

(defun merlin-update-point (view-errors-p)
  "Moves the merlin point to around the given the current
point. It proceeds as follows: 

- It retracts merlin from the point given in argument to get it
to the last phrase ending.

- It tells merlin the contents between the last phrase known to
merlin and the argument

- It continues until it finds the end of a phrase.

The parameter `view-errors-p' controls whether we should care for errors"
  (merlin-delete-error-overlays)
  (save-excursion
    (end-of-line)
    (setq merlin-lock-point (merlin-retract-to (point)))
    (merlin-tell-piece-split "struct" merlin-lock-point (point))
    (setq merlin-lock-point (merlin-tell-till-end-of-phrase))
    (merlin-view-errors view-errors-p)
    (merlin-update-overlay))
)    
  
(defun merlin-check-synchronize (&optional clean)
  "If merlin point is before the end of line send everything up to the end of line"
  (interactive)
  (save-excursion
    (previous-line 1)
    (let ((p merlin-lock-point))
      (if (> (point-at-eol) merlin-lock-point)
          (merlin-update-point nil)))))

(defun merlin-edit (start end length)
  (if (< start merlin-lock-point)
      (progn
        (setq merlin-lock-point (merlin-retract-to start))
        (merlin-update-overlay))))
;; COMPLETION
(defun merlin-extract-complete (prefix l)
  "Parses and format completion results"
  (mapcar `(lambda (c) 
             (if merlin-completion-types
                 (popup-make-item (concat prefix (cdr (assoc 'name c)))
                                  :symbol (format "%c" (car (string-to-list (cdr (assoc 'kind c)))))
                                  :summary (cdr (assoc 'desc c)))
               (cdr (assoc 'name c))))
	  (append l nil)))

(defun merlin-complete-identifier (ident)
  "Returns the formatted result of the completion of `ident'"
  (setq merlin-cache nil)
  (setq merlin-cache
	(merlin-extract-complete (merlin-compute-prefix ident) 
			   (elt (merlin-get-completion ident) 1)))
  )

(defun merlin-source-init ()
  (setq merlin-idle-point (point))
  (setq merlin-completion-point ac-point)
  (merlin-complete-identifier ac-prefix))

(defun merlin-try-completion ()
  "Try the merlin completion after having synchronized the point"
  (interactive)
  (merlin-check-synchronize)
  (ac-complete-merlin))

(defvar merlin-ac-source
  '((init . merlin-source-init)
    (candidates . (lambda () merlin-cache))
    (requires . 3)
    ))

(when (featurep 'auto-complete)
  (ac-define-source "merlin" merlin-ac-source))

;; Get the type of an element"
(defun merlin-trim (s)
  (replace-regexp-in-string "\n\\'" "" s))

(defun merlin-type-of-expression-node ()
  "Get the type of the node under point."
  (let ((ret 
         (merlin-is-return 
          (merlin-send-command "type" (list "at" (merlin-unmake-point (point)))))))
    (if ret
        (let ((bounds (merlin-make-bounds ret))
              (type (cdr (assoc 'type ret))))
          (if (and
               (>= (car bounds) (point))
               (<= (point) (cdr bounds)))
              (cons bounds type)
            nil)))))
          

        
    

(defun merlin-type-of-expression-local (bounds exp)
  "Get the type of an expression inside the local context"
  (if (and bounds exp)
      (cons 
       bounds
       (merlin-is-return 
        (merlin-send-command "type"
                             (list "expression" exp "at" 
                                   (merlin-unmake-point (point))))))))

(defun merlin-type-of-expression-global (bounds exp)
  "Get the type of an expression globally"
  (if exp
      (cons
       bounds
       (merlin-is-return (merlin-send-command "type" (list "expression" exp))))))


(defun merlin-type-of-expression (bounds exp)
  "Get the type of `exp' whose bounds in the buffer is `bounds'. It uses three techniques to do so:
- type at (node)
- type-expression at (local)
- type expression (global)"
  (or
   (merlin-type-of-expression-node)
   (merlin-type-of-expression-local bounds exp)
   (merlin-type-of-expression-global bounds exp)))

(defun merlin-show-type (bounds &optional quiet)
  "This functions shows the type of the expression inside
`bounds' in the current buffer. If `quiet' is non nil then an
overlay is displayed and module types are displayed in another
buffer. Otherwise only value type are displayed, and without
overlay"
  (let* ((substring (if bounds
                       (buffer-substring-no-properties (car bounds)
                                                       (cdr bounds))))
         (result (merlin-type-of-expression bounds substring)))
    (cond
     ((not (cdr result))
      (if (not quiet)
          (message "<no information>"))) ;; no types
     ((and (not (merlin-is-long (cdr result)))
           (not quiet))
      (merlin-create-overlay 'merlin-type-overlay
                             (car result)
                             'next-error "1 sec")
      (message "%s" (cdr result)))
     ((not (merlin-is-long (cdr result)))
      (message "%s"
               (cdr result)))
     ((not quiet)
      (display-buffer merlin-type-buffer)
      (with-current-buffer merlin-type-buffer
        (erase-buffer)
        (insert (cdr result)))))))

(defun merlin-show-type-def ()
  "Prints the definition of the type of the term under point"
  (interactive)
  (setq merlin-idle-point (point))
  (let* ((bounds (bounds-of-thing-at-point 'ocamlatom))
         (result (merlin-type-of-expression bounds (buffer-substring-no-properties (car bounds) (cdr bounds))))
         (typedef (cdr (merlin-type-of-expression-global nil (cdr result)))))
    (if typedef
        (message "%s" typedef)
      (message "%s: <no information>" (cdr result)))))


(defun merlin-show-type-of-region ()
  "Show the type of the region"
  (interactive)
  (merlin-show-type (cons (region-beginning) (region-end))))
(defun merlin-show-type-of-point-quiet ()
  "Show the type of the identifier under the point if it is short (a value)"
  (merlin-check-synchronize t)
  (merlin-show-type (bounds-of-thing-at-point 'ocamlatom) t))

(defun merlin-show-type-of-point (arg) 
  "Show the type of the identifier under the point. If it is called with a prefix argument, then show the type of the region."
  (merlin-check-synchronize)
  (interactive "p")
  (if (> arg 1)
      (merlin-show-type-of-region)
    (merlin-show-type (bounds-of-thing-at-point 'ocamlatom))))

;; ENCLOSING TYPES
(defun merlin-type-enclosing ()
  "If there is a selected type enclosing, kill it. Otherwise start a new session at point"
  (interactive)
  (let ((list
         (mapcar
          '(lambda (obj)
             (cons
              (cdr (assoc 'type obj))
              (merlin-make-bounds obj)))
          (elt
               (merlin-is-return
                (merlin-send-command "type" (list "enclosing" (merlin-unmake-point (point)))))
               1))))
    (setq merlin-enclosing-types list)
    (setq merlin-enclosing-offset -1)
    (if merlin-enclosing-overlay (delete-overlay merlin-enclosing-overlay))
    (setq merlin-enclosing-overlay nil)))

(defun merlin-magic-show-type (arg)
  "Prints the type of the expression under point. If called several times at the same position,
it will print types of bigger expressions around point (it will go up the ast). Called with a prefix argument, it will go down the AST"
  (interactive "p")
  (save-excursion
    (forward-line)
    (merlin-check-synchronize))
  (setq merlin-idle-point (point))
  (if (equal merlin-last-point-type (point))
      (if (> arg 1)
          (merlin-type-enclosing-go-down)
        (merlin-type-enclosing-go-up))
    (progn
      (setq merlin-last-point-type (point))
      (merlin-type-enclosing)
      (merlin-type-enclosing-go-up))))

(defun merlin-type-enclosing-go ()
  "Highlight the given corresponding enclosing data (of the form (type . bounds)"
  (let ((data (elt merlin-enclosing-types merlin-enclosing-offset)))
    (if (cddr data)
        (merlin-create-overlay 'merlin-enclosing-overlay (cdr data) 'next-error "5 sec"))
    (message "%s" (car data))))

(defun merlin-type-enclosing-go-up ()
  "Goes up in the enclosing type zipper."
  (interactive)
  (if merlin-enclosing-types
      (if (> merlin-enclosing-offset (length merlin-enclosing-types))
          (message "cannot go up")
        (progn
          (setq merlin-enclosing-offset (+ 1 merlin-enclosing-offset))
          (merlin-type-enclosing-go)
          ))))

(defun merlin-type-enclosing-go-down ()
  "Goes down in the enclosing type zipper."
  (interactive)
  (if merlin-enclosing-types
      (if (<= merlin-enclosing-offset 0)
          (message "cannot go down")
        (progn
          (setq merlin-enclosing-offset (- merlin-enclosing-offset 1))
          (merlin-type-enclosing-go)
          ))))

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
      (merlin-update-ppoint nil)))
(defun merlin-to-point ()
  "Updates the merlin to the current point, reporting error"
  (interactive)
  (merlin-update-point t))

;; Mode definition
(defvar merlin-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd "C-c <C-return>") 'merlin-to-point)
    (define-key map (kbd "C-c C-t") 'merlin-magic-show-type)
    (define-key map (kbd "C-c d") 'merlin-show-type-def)
    (define-key map (kbd "C-c l") 'merlin-use)
    (define-key map (kbd "C-c C-x") 'merlin-next-error)
    (define-key map (kbd "C-c C-r") 'merlin-rewind)
    (define-key map (kbd "C-c C-u") 'merlin-refresh)
    (define-key map (kbd "C-c TAB") 'merlin-try-completion)
    (define-key map (kbd "C-c C-u") 'merlin-refresh)
    (define-key map (kbd "C-c C-f <C-return>") 'merlin-type-enclosing)
    (define-key map (kbd "C-c C-f C-<up>") 'merlin-type-enclosing-go-up)
    (define-key map (kbd "C-c C-f C-<down>") 'merlin-type-enclosing-go-down)
    (define-key map (kbd "RET") 'merlin-enter)
    map
    ))

(defun merlin-setup ()
  "Sets up a buffer for use with merlin"
  (interactive)
  (progn
    (merlin-start-process)
    (when (featurep 'auto-complete)
      (auto-complete-mode))
    (set (make-local-variable 'merlin-lock-point) (point-min))
    (set (make-local-variable 'merlin-buffer) nil)
    (set (make-local-variable 'merlin-result) nil)
    (set (make-local-variable 'merlin-idle-point) nil)
    (set (make-local-variable 'merlin-completion-point) nil)
    (set (make-local-variable 'merlin-ready) nil)
    (set (make-local-variable 'merlin-pending-errors) nil)
    (set (make-local-variable 'merlin-pending-errors-overlay) nil)
    (set (make-local-variable 'merlin-type-overlay) nil)
    (set (make-local-variable 'merlin-overlay) nil)
    (set (make-local-variable 'merlin-prefix) nil)
    (set (make-local-variable 'merlin-error-prefix) nil)
    (set (make-local-variable 'merlin-enclosing-overlay) nil)
    (set (make-local-variable 'merlin-enclosing-types) nil)
    (set (make-local-variable 'merlin-enclosing-offset) nil)
    (set (make-local-variable 'merlin-last-point-type) nil)
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
      (if (equal (file-name-extension (buffer-file-name))
                 "ml")
          (merlin-setup)
        (progn
          (message "merlin can only operate on ml files")
          (merlin-mode -1)))
    (progn
      (if merlin-process
          (progn
            (process-send-eof (merlin-get-process))
            (delete-process (merlin-get-process))))
      (cancel-timer merlin-idle-timer)
      (if merlin-overlay (delete-overlay merlin-overlay))
      (if merlin-enclosing-overlay (delete-overlay merlin-enclosing-overlay))
      (merlin-delete-error-overlays)
      (if (get-buffer (merlin-make-buffer-name))
          (kill-buffer (merlin-make-buffer-name)))
)))


(provide 'merlin)
