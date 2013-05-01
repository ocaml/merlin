;; Mode for Merlin

;; json and cl are mandatory
(require 'cl)
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

;; Variable the user can change
(defvar merlin-command "ocamlmerlin"
  "The command for ocamlmerlin in your installation")
(defvar merlin-continuous-feed nil
  "If set to t, each time you hit RET, the line is fed to merlin")
(defvar merlin-completion-types t
  "Prints the types of the variables during completion")
(defvar merlin-debug nil
  "If true, logs the data sent and received from merlin")
(defvar merlin-report-warnings t
  "Should merlin care about warnings ?")
(defvar merlin-favourite-caml-mode
  'tuareg-mode
  "The OCaml mode to use for the *merlin types* buffer")
(defvar merlin-idle-delay 3.0
  "Number of seconds of idle time to wait before printing the type of the ident under the point.
If user input arrives before this interval of time has elapsed
after the last input, no documentation will be printed.

If the timer is zero or negative, nothing is done."
)

(defvar merlin-margin-lock-string "-"
  "String put in the margin to signal the end of the locked zone")

(defvar merlin-margin-error-string "!"
  "String to put in the margin of a line containing an error"
)

(defvar merlin-margin-warning-string "?"
  "String to put in the margin of a line containing an error")
(defvar merlin-display-lock-zone '(margin)
  "How to display the locked zone. It is a list of methods among:
   - 'highlight: highlight the current locked zone (like proofgeneral)
   - 'margin: put a symbol (given by `merlin-margin-lock-string') in the margin
     of the line where the zone ends.

In particular you can specify nil, meaning that the locked zone is not represented at the screen"
  )

(defvar merlin-automatically-garbage-processes t
  "If set to t, deletes a process when it has no more users. If set to nil, keep it")

;; Internal variables


; Process / Reception related variables
(defvar merlin-processes nil
  "The global merlin process table. It lists the active instances of merlin")
(defvar merlin-local-process nil
  "The local merlin process (buffer local in every process buffer)"
)
(defvar merlin-process-users nil
  "Buffer that uses the process (local to a process buffer)")
(defvar merlin-process-last-user nil
  "Last buffer that used the process")
(defvar merlin-result nil
  "Temporary variables to store command results")
(defvar merlin-lock-zone-highlight-overlay nil 
  "Overlay used for the lock zone highlighting")
(defvar merlin-lock-zone-margin-overlay nil 
  "Overlay used for the margin indicator of the lock zone")
(defvar merlin-buffer nil "Buffer for merlin input")
(defvar merlin-ready nil "Is reception done?")

; Errors related variables
(defvar merlin-pending-errors nil
  "Pending errors")
(defvar merlin-lock-point nil
  "Position up to which merlin knows about")
(defvar merlin-pending-errors-overlay nil
  "Overlays for the pending errors")
(defvar merlin-error-overlay nil "Merlin overlay used for errors")

; Completion related variables
(defvar merlin-completion-point nil
  "Stores the point of last completion (beginning of the prefix)")
(defvar merlin-cache nil "Merlin cache for completion")

; Type related variables
(defvar merlin-enclosing-types nil
  "List containing the enclosing type")
(defvar merlin-enclosing-offset nil
  "Current offset in `merlin-enclosing-types'")
(defvar merlin-last-point-type nil
  "Last position where the user hit C-c C-t")
(defvar merlin-idle-point nil
  "Position of the last time we printed the type of point")
(defvar merlin-type-buffer
  (get-buffer-create "*merlin types*")
  "The buffer to use to display types"
  )
(defvar merlin-type-overlay nil "Merlin overlay used for type-checking")
(defvar merlin-idle-timer nil
  "The timer used to print the type of the expression under point")




;; UTILS

(defun merlin-debug (s)
  "If in debug-mode (controlled by `merlin-debug', outputs the given string
   on the buffer associated to the current merlin process"
  (with-current-buffer (merlin-get-process-buffer-name)
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
(defun merlin-put-margin-overlay (overlay s &optional face)
  "Put a margin overlay inside `overlay', with face `face' and string `string'"
  (set-window-margins nil 1)
  (if face
      (overlay-put overlay 'face face))
  (overlay-put overlay
               'before-string 
               (propertize " " 'display 
                           `((margin left-margin) ,s)
                           )))

(defun merlin-create-overlay (var bounds face timer)
  "Creates an overlay in the current buffer starting at `start', ending at `end',
using `face' and storing it in `var'. If `timer' is non-nil, the overlay is to disappear after `timer' seconds
`timer' is a string that can be understood by `run-at-time' (eg. \"1 sec\")"
  (if (symbol-value var) (delete-overlay (symbol-value var)))
  (set var (make-overlay (car bounds) (cdr bounds)))
  (overlay-put (symbol-value var) 'face face)
  (if timer
      (run-at-time timer nil `(lambda ()
                                (when ,var (delete-overlay ,var)
                                      (setq ,var nil))))))
      
;; PROCESS MANAGEMENT

(defun merlin-get-buffer-instance-name ()
  "Returns the instance name of the current-projet.
For now it is a constant function (every buffer shares the same instance)"
  "")
(defun merlin-get-process ()
  "Returns the process of the current buffer"
  merlin-local-process)

(defun merlin-get-process-buffer-name ()
  "Returns the buffer name of the merlin process associated to the current buffer"
  (format "* merlin %s *" (merlin-get-buffer-instance-name)))

(defun merlin-get-process-variable (var)
  "Returns the value of a variable (symbol) inside the process buffer"
  (buffer-local-value var (get-buffer (merlin-get-process-buffer-name))))


(defun merlin-get-process-name ()
  "Returns the process name for the current buffer"
  (concat "merlin-" (merlin-get-buffer-instance-name))
  )


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
  (with-current-buffer (merlin-get-process-buffer-name)
    (let ((times 0))
      (while
          (and (< times 20)
               (or
                (not (accept-process-output (merlin-get-process) 0.1 nil nil))
                (not merlin-ready)))
        (setq times (+ times 1)))
      (setq merlin-buffer nil)
      (setq merlin-ready nil))))

(defun merlin-start-process ()
  "Start the merlin process for the current buffer"
  (let ((p (start-process (merlin-get-process-name)
			 (merlin-get-process-buffer-name)
			 merlin-command))
        (name (buffer-name)))
;; set the global process (for now)
    (set (make-local-variable 'merlin-local-process) p)
    (set-process-query-on-exit-flag p nil)
    (set-process-filter p 'merlin-filter)
    (push p merlin-processes)
  ; don't forget to initialize temporary variable
  (with-current-buffer (merlin-get-process-buffer-name)
    (set (make-local-variable 'merlin-process-users) (list name))
    (set (make-local-variable 'merlin-local-process) p)
    (set (make-local-variable 'merlin-process-last-user) name)
    )
  ))

(defun merlin-process-add-user ()
  "Add the current buffer as an user for the merlin process"
  (let ((name (buffer-name)))
    (merlin-debug (format "Adding user: %s\n" name))
    (with-current-buffer (merlin-get-process-buffer-name)
      (push name merlin-process-users))
    )
)

(defun merlin-is-last-user-p ()
  "Returns whether the current buffer was the current user of its merlin process"
  (equal (merlin-get-process-variable 'merlin-process-last-user)
         (buffer-name))
)

(defun merlin-process-remove-user ()
  "Remove the current buffer as an user for the merlin process, and
kill the process if required"
  (let ((name (buffer-name)))
    (with-current-buffer (merlin-get-process-buffer-name)
      (setq merlin-process-users (delete name merlin-process-users))
      (if (and (not merlin-process-users)
               merlin-automatically-garbage-processes)
          (merlin-kill-process))
      )
    )
)
(defun merlin-kill-process ()
  "Kills the merlin process inside the buffer"
  (setq merlin-processes (delete merlin-local-process merlin-processes))
  (process-send-eof (merlin-get-process))
  (delete-process (merlin-get-process))
  (kill-buffer (merlin-get-process-buffer-name))
)
(defun merlin-send-command (name args)
  "Send a command to merlin. Returns the result"
  (let ((string
	 (concat 
	  (json-encode (if args (append (list name) args) (list name)))
	  "\n"))
        (name (buffer-name)))
    (process-send-string (merlin-get-process) string)
    (setq merlin-result nil)
    (if merlin-debug (merlin-debug (format "Sending:\n%s\n---\n" string)))
    (with-current-buffer (merlin-get-process-buffer-name)
      (setq merlin-process-last-user name)
      (merlin-wait-for-answer)
      merlin-result
      )
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
  (merlin-update-lock-zone-display)
)

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
  "Tell the region between `start' and `end' in one chunk using mode `mode'"
  (merlin-tell-string mode (buffer-substring start end)))

(defun merlin-tell-piece-split (mode start end)
   "Tell the region between `start' and `end' in several chunks (a chunk is at most 10 lines) using mode `mode'"
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

(defun merlin-tell-till-end-of-phrase ()
  "Tell merlin the buffer until the end of the current phrase is met.
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
          (merlin-seek (merlin-get-position)))
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
            (message "%s (%d more errors, use %s to go to the next)" 
                     (cdr (assoc 'message err))
                     (length merlin-pending-errors)
                     (substitute-command-keys "\\[merlin-next-error]")
                     )
          (message "%s" (cdr (assoc 'message err)))))
    (message "no more errors")))

(defun merlin-remove-error-overlay ()
  "Remove the error overlay"
  (delete-overlay merlin-error-overlay))

(defun merlin-delete-error-overlays ()
  "Removes margin error overlays"
  (mapc '(lambda (over) (delete-overlay over)) merlin-pending-errors-overlay)
  (setq merlin-pending-errors-overlay nil))

(defun merlin-is-warning (msg)
  "Tell if the given message is a warning"
  (string-match "^Warning" msg))

(defun merlin-display-errors-in-margin (errors)
  "Given a list of errors, puts annotations in the margin corresponding to them"
  (merlin-delete-error-overlays)
  (setq merlin-pending-errors (append errors nil))
  (setq merlin-pending-errors-overlay 
        (mapcar '(lambda (err)
                   (let ((overlay (make-overlay
                                   (merlin-make-point (cdr (assoc 'start err)))
                                   (merlin-make-point (cdr (assoc 'end err))))))
                     (if (merlin-is-warning (cdr (assoc 'message err)))
                         (merlin-put-margin-overlay overlay "?" compilation-warning-face)
                       (merlin-put-margin-overlay overlay "!" compilation-error-face))
                     overlay)) errors))
  (message "(pending errors, use %s to jump)"
           (substitute-command-keys "\\[merlin-next-error]")))

(defun merlin-check-for-errors (view-errors-p)
  "Check for errors. It returns `t' if there were not any or nil if there were.




Moreover if `view-errors-p' is not nil, it will display them in the margin."
  (let ((output (merlin-send-command "errors" nil)))
    (if (> (length (elt output 1)) 0)
	(progn
          (when view-errors-p
            (let ((errors (nreverse (append (elt output 1) nil))))
              (if (not merlin-report-warnings)
                  (delete-if (lambda (e) (merlin-is-warning (cdr (assoc 'message e)))) errors))
              (merlin-display-errors-in-margin errors)))
	  nil)
      (progn
	(message "ok")
	t))))
 

(defun merlin-retract-to (point)
  "Retract merlin's view to `point'"
  (merlin-seek point))

(defun merlin-update-lock-zone-display ()
  "Updates the locked zone display, according to `merlin-display-lock-zone', ie.
 iterates through it and call each method"
  (mapc
   '(lambda (x)
      (case x
        (margin (merlin-update-margin-lock-zone))
        (highlight (merlin-update-highlight-lock-zone))))
   merlin-display-lock-zone))


(defun merlin-update-margin-lock-zone ()
    "Marks the position of the lock zone by a marker in the margin"
  (if merlin-lock-zone-margin-overlay
      (delete-overlay merlin-lock-zone-margin-overlay))
  (save-excursion
    (goto-char merlin-lock-point)
    (setq merlin-lock-zone-margin-overlay (make-overlay (point) (point)))
    (set-window-margins nil 1)
    (merlin-put-margin-overlay merlin-lock-zone-margin-overlay
                               merlin-margin-lock-string)))

(defun merlin-update-highlight-lock-zone ()
  "Marks the position of the lock zone by highlighting the zone"
    (if merlin-lock-zone-highlight-overlay
      (delete-overlay merlin-lock-zone-highlight-overlay))
  (setq merlin-lock-zone-highlight-overlay (make-overlay (point-min) merlin-lock-point))
  (overlay-put merlin-lock-zone-highlight-overlay 'face 'merlin-locked-face))

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
  (if (not (merlin-is-last-user-p))
      (merlin-rewind))
  (save-excursion
    (setq merlin-lock-point (merlin-retract-to (point)))
    (end-of-line)
    (merlin-tell-piece-split "struct" merlin-lock-point (point))
    (setq merlin-lock-point (merlin-tell-till-end-of-phrase))
    (merlin-check-for-errors view-errors-p)
    (merlin-update-lock-zone-display)))
  
(defun merlin-check-synchronize ()
  "If merlin point is before the end of line send everything up to the end of line"
  (interactive)
  (save-excursion
    (previous-line 1)
    (let ((p merlin-lock-point))
      (if (> (point-at-eol) merlin-lock-point)
          (merlin-update-point nil)))))

(defun merlin-edit (start end length)
  "Called when an edit is make to retract the locked zone if it is needed"
  (if (< start merlin-lock-point)
      (progn
        (setq merlin-lock-point (merlin-retract-to start))
        (merlin-update-lock-zone-display))))

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
  "Called at the beginning of a completion to fill the cache (the
variable `merlin-cache')"
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

(defun merlin-type-of-expression-local (exp)
  "Get the type of an expression inside the local context"
  (if (and bounds exp)
      (merlin-is-return 
       (merlin-send-command "type"
                            (list "expression" exp "at" 
                                  (merlin-unmake-point (point)))))))

(defun merlin-type-of-expression-global (exp)
  "Get the type of an expression globally"
  (if exp
      (merlin-is-return (merlin-send-command "type" (list "expression" exp)))))


(defun merlin-type-of-expression (exp)
  "Get the type of `exp'. It uses three techniques to do so:
- type-expression at (local)
- type expression (global)"
  (or
   (merlin-type-of-expression-local exp)
   (merlin-type-of-expression-global exp)))


(defun merlin-display-type (bounds type &optional quiet)
  "Displays the type `type' of the the expression occuring at `bounds' in the current buffer.
   If `quiet' is non nil, then an overlay and the merlin types can be used"
  (if (not type)
      (if (not quiet)
          (message "<no information>"))
    (progn
      (if (not (merlin-is-long type))
          (progn 
            (message "%s" type)
            (if (not quiet) 
                (merlin-create-overlay 'merlin-type-overlay bounds 'highlight "1 sec")))
        (when (not quiet)
          (display-buffer merlin-type-buffer)
          (with-current-buffer merlin-type-buffer
            (erase-buffer)
            (insert type)))))))
          
(defun merlin-show-type (bounds &optional quiet)
  "This functions shows the type of the expression inside
`bounds' in the current buffer. If `quiet' is non nil then an
overlay is displayed and module types are displayed in another
buffer. Otherwise only value type are displayed, and without
overlay"
  (let* ((substring (if bounds
                       (buffer-substring-no-properties (car bounds)
                                                       (cdr bounds))))
         (type (merlin-type-of-expression substring)))
    (merlin-display-type bounds type quiet)))

(defun merlin-get-type-codomain (type)
  "Given a functional type, tries to return its codomain"
  (car (last (split-string type " " nil))))

(defun merlin-show-type-def ()
  "Prints the definition of the type of the term under point"
  (interactive)
  (setq merlin-idle-point (point))
  (let* ((bounds (bounds-of-thing-at-point 'ocamlatom))
         (type (merlin-type-of-expression (buffer-substring-no-properties (car bounds) (cdr bounds))))
         (typedef (merlin-type-of-expression (merlin-get-type-codomain type))))
    (merlin-display-type bounds
                         (if typedef typedef 
                           (concat type ": <not an atomic type>")))))


(defun merlin-show-type-of-region ()
  "Show the type of the region"
  (interactive)
  (merlin-check-synchronize)
  (merlin-show-type (cons (region-beginning) (region-end))))
(defun merlin-show-type-of-point-quiet ()
  "Show the type of the identifier under the point if it is short (a value)"
  (merlin-check-synchronize)
  (merlin-show-type (bounds-of-thing-at-point 'ocamlatom) t))

(defun merlin-show-type-of-point (arg) 
  "Show the type of the identifier under the point. If it is called with a prefix argument, then show the type of the region."
  (interactive "p")
  (ignore-errors (merlin-check-synchronize))
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
    (setq merlin-enclosing-offset -1)))

(defun merlin-magic-show-type (arg)
  "Prints the type of the expression under point. If called several times at the same position,
it will print types of bigger expressions around point (it will go up the ast). Called with a prefix argument, it will go down the AST. If there is no enclosing, falls back to `merlin-show-type-of-point'"
  (interactive "p")
  (save-excursion
    (forward-line)
    (merlin-check-synchronize))
  (setq merlin-idle-point (point))
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

(defun merlin-type-enclosing-go ()
  "Highlight the given corresponding enclosing data (of the form (type . bounds)"
  (let ((data (elt merlin-enclosing-types merlin-enclosing-offset)))
    (if (cddr data)
        (merlin-display-type (cdr data) (car data)))))

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
(defun merlin-add-path (kind dir dirname)
  "Adds an item to a path in merlin"
  (merlin-send-command "path" (list "add" kind (concat dirname dir))))

(defun merlin-get-packages ()
  "Get the list of available findlib package"
  (setq merlin-packages nil)
  (append (elt (merlin-send-command "find" '("list")) 1) nil))
(defun merlin-use (pkg)
  "Use a package in the current session of merlin"
  (interactive
   (list (completing-read "Package to use:" (merlin-get-packages))))
  (merlin-send-command "find" (list "use" pkg)))

(defun merlin-handle-line (buffer words dirname)
  "Handles a line in a .merlin file"
  (with-current-buffer buffer
    (cond
     ((string-equal (elt words 0) "S") (merlin-add-path "source" (elt words 1) dirname))
     ((string-equal (elt words 0) "B") (merlin-add-path "build" (elt words 1) dirname))
     ((string-equal (elt words 0) "PKG") (merlin-use (elt words 1))))))
(defun merlin-file-parse (filename)
  "Parses a .merlin. It should exist"
  (message "Parsing .merlin file %s" filename)
  (setq lines nil)
  (setq buf (current-buffer))
  (with-current-buffer (find-file-noselect filename)
    (goto-char (point-min))
    (while (not (eq (point) (point-max)))
      (let ((words (split-string (buffer-substring-no-properties (point-at-bol) (point-at-eol)))))
        (merlin-handle-line buf words (file-name-directory filename))
        (forward-line)))))

(defun merlin-parse ()
  "Parses all .merlin file lying beneath the current directory in the file system."
  (setq dir (expand-file-name default-directory))
  (let ((buf (current-buffer)))
    (while (and (stringp dir) (not (string-equal dir "/")))
      (when (file-exists-p (concat dir ".merlin") )
        (merlin-file-parse (concat dir ".merlin")))
      (setq dir (file-name-directory (directory-file-name dir))))
    (switch-to-buffer buf)))
    

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
      (merlin-update-point nil)))
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
  ; if there is not yet a buffer for the current buffer, create one
  (if (not (get-buffer (merlin-get-process-buffer-name)))
      (merlin-start-process)
    (set (make-local-variable 'merlin-local-process)
         (merlin-get-process-variable 'merlin-local-process)))
  (merlin-process-add-user)

  (when (featurep 'auto-complete)
    (auto-complete-mode)
    (add-to-list 'ac-sources 'merlin-ac-source))
  (set (make-local-variable 'merlin-lock-point) (point-min))
  (set (make-local-variable 'merlin-buffer) nil)
  (set (make-local-variable 'merlin-result) nil)
  (set (make-local-variable 'merlin-idle-point) nil)
  (set (make-local-variable 'merlin-completion-point) nil)
  (set (make-local-variable 'merlin-ready) nil)
  (set (make-local-variable 'merlin-pending-errors) nil)
  (set (make-local-variable 'merlin-pending-errors-overlay) nil)
  (set (make-local-variable 'merlin-type-overlay) nil)
  (set (make-local-variable 'merlin-lock-zone-highlight-overlay) nil)
  (set (make-local-variable 'merlin-lock-zone-margin-overlay) nil)
  (set (make-local-variable 'merlin-error-prefix) nil)
  (set (make-local-variable 'merlin-enclosing-types) nil)
  (set (make-local-variable 'merlin-enclosing-offset) nil)
  (set (make-local-variable 'merlin-last-point-type) nil)
  (add-to-list 'after-change-functions 'merlin-edit)
  (merlin-parse)
  (if (and (> merlin-idle-delay 0.) (not merlin-idle-timer))
      (setq merlin-idle-timer
            (run-with-idle-timer merlin-idle-delay t 'merlin-idle-hook)))
  (with-current-buffer merlin-type-buffer
    (funcall merlin-favourite-caml-mode)))
(define-minor-mode merlin-mode
  "Mode to use merlin tool inside OCaml tools."
  nil
  " merlin"
  :keymap merlin-mode-map
  (if merlin-mode 
      (if (and
           (buffer-file-name)
           (equal (file-name-extension (buffer-file-name))
                 "ml"))
          (merlin-setup)
        (progn
          (if (buffer-file-name)
              (message "merlin can only operate on ml files")
              nil)
          (merlin-mode -1)))
    (progn
      (cancel-timer merlin-idle-timer)
      (if merlin-lock-zone-highlight-overlay
          (delete-overlay merlin-lock-zone-highlight-overlay))
      (if merlin-lock-zone-margin-overlay
          (delete-overlay merlin-lock-zone-margin-overlay))
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
  "Kill all the remaining buffers containing merlin processes"
  (interactive)
  (mapc '(lambda (p)
           (with-current-buffer (process-buffer p)
             (merlin-kill-process)))))

(provide 'merlin)
