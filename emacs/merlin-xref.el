;; -*- lexical-binding: t -*-
(require 'cl-lib)
(require 'xref)
(require 'merlin)

;;;###autoload
(defun merlin-xref-backend ()
  "Merlin backend for Xref."
  'merlin-xref)

(defun merlin-xref--line (loc)
  (save-excursion
    (goto-char loc)
    (buffer-substring (line-beginning-position) (line-end-position))))

(defun merlin-xref--line-col-to-pos (line-col)
  (let ((line (cdr (assq 'line line-col)))
        (col (cdr (assq 'col line-col))))
    (goto-char (point-min))
    (forward-line (1- line))
    (byte-to-position (+ (position-bytes (point)) col))))

(cl-defmethod xref-backend-references ((_backend (eql merlin-xref)) symbol)
  (save-excursion
    (save-restriction
      (widen)
      (let ((locations
             ;; Transform the result into a list of (START END START-LINE).
             (mapcan
              (lambda (loc)
                (let* ((start-line-col (cdr (assq 'start loc)))
                       (start-line (cdr (assq 'line start-line-col))))
                  ;; We sometimes get bogus locations (line 0, col -1)
                  ;; in the reply. That should be fixed, but meanwhile
                  ;; filter them out (issue #1410).
                  (and (> start-line 0)
                       (let* ((end-line-col (cdr (assq 'end loc)))
                              (start
                               (merlin-xref--line-col-to-pos start-line-col))
                              (end (merlin-xref--line-col-to-pos end-line-col)))
                         (list (list start end start-line))))))
              (let ((pt (get-text-property 0 'merlin-xref-point symbol)))
                (when pt
                  (goto-char pt))
                (merlin--occurrences)))))
        ;; To provide correct context for Xref, lines with multiple matches
        ;; need to be partitioned into substrings, each containing a single
        ;; match. (Prior to Emacs 28 we use the entire line because the Xref
        ;; machinery did not put multiple matches on a single line.)
        (let ((whole-line (< emacs-major-version 28))
              (match-face (if (facep 'xref-match) 'xref-match 'match))
              (file-name buffer-file-name)
              (matches nil)
              (prev nil)
              (tail locations))
          (while tail
            (let* ((match (car tail))
                   (next (cadr tail))
                   (start (nth 0 match))
                   (end (nth 1 match))
                   (line (nth 2 match))
                   (prev-end (nth 1 prev))
                   (next-start (nth 0 next))
                   (bol (progn (goto-char start)
                               (line-beginning-position)))
                   (eol (line-end-position))
                   (str-start (if (and prev-end (>= prev-end bol)
                                       (not whole-line))
                                  start
                                bol))
                   (str-end (if (and next-start (< next-start eol)
                                     (not whole-line))
                                next-start
                              eol))
                   (str (buffer-substring str-start str-end))
                   (location (xref-make-file-location
                              file-name line (- start bol))))
              (add-face-text-property (- start str-start) (- end str-start)
                                      match-face nil str)
              (push (xref-make-match str location (- end start))
                    matches)
              (setq prev match)
              (setq tail (cdr tail))))
          (nreverse matches))))))

(cl-defmethod xref-backend-definitions ((_backend (eql merlin-xref)) symbol)
  (let* ((loc (save-excursion
                (let ((pt (get-text-property 0 'merlin-xref-point symbol)))
                  (when pt
                    (goto-char pt)))
               (merlin-call-locate)))
         (file (alist-get 'file loc))
         (pos (alist-get 'pos loc))
         (line (alist-get 'line pos))
         (col (alist-get 'col pos)))
    (save-excursion
      (find-file file)
      (let ((desc (merlin-xref--line (merlin-make-point pos))))
        (list (xref-make desc (xref-make-file-location file line col)))))))

(cl-defmethod xref-backend-identifier-completion-table ((_backend (eql merlin-xref)))
  nil)

(defconst merlin-xref--operator-regexp
  (eval-when-compile
    (let* ((core-operator-char
            `(or "$" "&" "*" "+" "-" "/" "=" ">" "@" "^" "|"))
           (operator-char `(or "~" "!" "?" ,core-operator-char "%" "<" ":" "."))
           (prefix-symbol `(or (seq "!" (* ,operator-char))
                               (seq (or "?" "~") (+ ,operator-char))))
           (infix-symbol `(or (seq (or ,core-operator-char "%" "<")
                                   (* ,operator-char))
                              (seq "#" (+ ,operator-char))))
           (infix-op `(or ,infix-symbol
                          ":="
                          ;; Already handled as part of `infix-symbol':
                          ;; "*" "+" "-" "-." "=" "!=" "<" ">" "||" "&" "&&"
                          ;; Treated as normal symbols:
                          ;; "or" "mod" "land" "lor" "lxor" "lsl" "lsr" "asr"
                          ))
           (operator-name `(or ,prefix-symbol ,infix-op)))
      (rx-to-string operator-name t))))

(defconst merlin-xref--binding-operator-regexp
  (eval-when-compile
    (let* ((core-operator-char
            `(or "$" "&" "*" "+" "-" "/" "=" ">" "@" "^" "|"))
           (dot-operator-char
            `(or "!" "?" ,core-operator-char "%" ":"))
           (binding-suffix
            `(seq (or ,core-operator-char "<") (* ,dot-operator-char)))
           (binding-operator
            `(seq symbol-start (or "let" "and") ,binding-suffix)))
      (rx-to-string binding-operator t))))

(defconst merlin-xref--identifier-regexp
  (rx symbol-start (in "A-Za-z_") (* (in "A-Za-z0-9_'"))))

(cl-defmethod xref-backend-identifier-at-point ((_backend (eql merlin-xref)))
  (let ((symbol
         (and
          (or
           ;; binding operator starting at point
           (looking-at merlin-xref--binding-operator-regexp)
           ;; ... before point
           (and (save-excursion
                  (skip-chars-backward "letand$&*+/=<>@^|!?%:-")
                  (looking-at merlin-xref--binding-operator-regexp))
                (<= (point) (match-end 0)))
           ;; ordinary name starting at point
           (looking-at merlin-xref--identifier-regexp)
           ;; operator starting at or before point
           (and (save-excursion
                  (skip-chars-backward "$&*+/=<>@^|!?%:.~#-")
                  (looking-at merlin-xref--operator-regexp))
                (<= (point) (match-end 0)))
           ;; ordinary name starting before point
           (and (save-excursion
                  (skip-chars-backward "A-Za-z0-9_'")
                  (looking-at merlin-xref--identifier-regexp))
                (<= (point) (match-end 0))))
          (match-string 0))))
    ;; Return a string with the buffer position in a property, in case
    ;; point changes before the string is used by one of the methods above.
    (and symbol (propertize symbol 'merlin-xref-point (point)))))

(provide 'merlin-xref)
