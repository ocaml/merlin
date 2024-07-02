;;; merlin-cap.el --- Merlin and completion-at-point integration   -*- coding: utf-8; lexical-binding: t -*-
;; Licensed under the MIT license.

;; Author: Simon Castellan <simon.castellan(_)iuwt.fr>
;;         Frédéric Bour <frederic.bour(_)lakaban.net>
;;         Thomas Refis <thomas.refis(_)gmail.com>
;; Created: 15 May 2015
;; Version: 0.1
;; Keywords: ocaml languages
;; URL: http://github.com/ocaml/merlin

;;; Commentary:

;; Call merlin-completion-at-point when you want merlin guided completion-at-point.

;;; Code:

(require 'merlin)
(require 'subr-x)

(defvar-local merlin-cap--cache nil
  "An alist mapping contexts to lists of completions.

This is used to cache completions; it is indexed by the return
value of `merlin-completion-prefix'.

The keys are strings like \"List.\" or the empty string.
The values are lists of completion strings.")

(defvar-local merlin-cap--cache-position nil
  "The position for which `merlin-cap--cache' is valid.")

;; Internal functions

(defun merlin-cap--exit-function (string _state)
  "Print a message for completion STRING."
  (let ((ret (merlin-cap--annotate string)))
    (if ret (message "%s%s" string ret))))

(defun merlin-cap--annotate (candidate)
  "Retrieve the annotation for candidate CANDIDATE."
  ;; Fancy completion styles (like partial-completion) can concatenate multiple candidates
  ;; together into a single candidate (e.g. "List." and "map" concatenated into "List.map"),
  ;; and each component of that will have its own annotation text property.
  ;; We want to print the type of the overall expression, so grab the annotation from the end.
  (get-text-property (1- (length candidate)) 'merlin-cap--annotation candidate))

(defvar merlin-cap--interrupt-symbol nil)

(define-error 'merlin-cap--test-interrupt "Test-only interrupt")
(defun merlin-cap--interrupt-in-test (position-symbol)
  "Error if POSITION-SYMBOL is equal to `merlin-cap--interrupt-symbol'."
  (when (eq position-symbol merlin-cap--interrupt-symbol)
    (signal 'merlin-cap--test-interrupt position-symbol)))

(defvar-local merlin-cap--process-last-event nil
  "The most recent process event for a Merlin process in this buffer.")

(defun merlin-cap--sentinel (process event)
  "Store EVENT in `merlin-cap--process-last-event' in PROCESS's buffer."
  (let ((buf (process-buffer process)))
    (when (buffer-live-p buf)
      (with-current-buffer buf
        (setq merlin-cap--process-last-event event)))))

;; Buffer-local variables for request buffers
(defvar-local merlin-cap--request-debug-cons nil
  "This request's entry in the variable `merlin-debug-last-commands'.

This is a cons whose cdr starts out nil, is set to the symbol
`pending' when the request is fully created, and is set to the
raw output string from the request when it's completed.")
(defvar-local merlin-cap--request-stderr-proc nil
  "The stderr pipe process for this request.")
(defvar-local merlin-cap--request-originating-buffer nil
  "The buffer that this completion request is for.")
(defvar-local merlin-cap--request-position nil
  "The POSITION argument to `merlin-cap--complete-prefix' for this request.")
(defvar-local merlin-cap--request-prefix nil
  "The PREFIX argument to `merlin-cap--complete-prefix' for this request.")
(defvar-local merlin-cap--request-ignore-region nil
  "The IGNORE-REGION argument to `merlin-cap--complete-prefix' for this request.")

(defun merlin-cap--pending-request (position ignore-region)
  "Return a pre-existing request for POSITION and IGNORE-REGION.

Specifically, return a cons whose car is the prefix that the
request was for and whose cdr is the request buffer.  Return nil
if no pending request exists, or its inputs do not match."
  (let ((buf (current-buffer)))
    (with-current-buffer (get-buffer-create " *merlin-async-proc-stdout*" t)
      (when (and (eq (cdr merlin-cap--request-debug-cons) 'pending)
                 (eq buf merlin-cap--request-originating-buffer)
                 (eql position merlin-cap--request-position)
                 (equal ignore-region merlin-cap--request-ignore-region))
        (cons merlin-cap--request-prefix
              (current-buffer))))))

(defun merlin-cap--complete-prefix (position prefix omit-region)
  "Request completions that could come after PREFIX at POSITION.

POSITION is some position in the current buffer.  It doesn't need
to actually have PREFIX before it.

Returns the buffer that the data from Merlin will be inserted
into; this should be waited for with
`merlin-cap--wait-for-request'.

When sending the current buffer to Merlin, we ignore the region
at OMIT-REGION, which is a cons of the start and end."
  (let* ((buffer (with-current-buffer (get-buffer-create " *merlin-async-proc-stdout*" t)
                   (when-let ((proc (get-buffer-process (current-buffer))))
                     (delete-process proc))
                   (when merlin-cap--request-stderr-proc
                     (delete-process merlin-cap--request-stderr-proc))
                   (fundamental-mode)
                   (erase-buffer)
                   (merlin-cap--interrupt-in-test 'prepare-buffer)
                   (current-buffer)))
         (process-environment (merlin--process-environment))
         (command (cons (merlin-command)
                        (merlin--command-args "complete-prefix"
                                              "-position" (merlin-unmake-point position)
                                              "-prefix" prefix)))
         (debug-cons (cons command nil))
         (stderr-proc (make-pipe-process
                       :name "merlin complete-prefix stderr"
                       :buffer (get-buffer-create " *merlin-async-proc-stderr*" t)
                       :noquery t)))
    (unwind-protect
        (progn
          (push debug-cons merlin-debug-last-commands)
          (merlin-cap--interrupt-in-test 'make-stderr-proc)
          (let ((proc (make-process
                       :name "merlin complete-prefix"
                       :buffer buffer
                       :stderr stderr-proc
                       :command command
                       :connection-type 'pipe
                       :noquery t
                       :sentinel #'merlin-cap--sentinel)))
            (unwind-protect
                (progn
                  (merlin-cap--interrupt-in-test 'make-main-proc)
                  (save-restriction
                    ;; The current narrowing might be syntactically invalid, so widen first
                    (widen)
                    (process-send-region proc (point-min) (car omit-region))
                    (merlin-cap--interrupt-in-test 'sent-half-input)
                    (process-send-region proc (cdr omit-region) (point-max))
                    (process-send-eof proc))
                  (merlin-cap--interrupt-in-test 'sent-eof)
                  (let ((originating-buffer (current-buffer)))
                    (with-current-buffer buffer
                      (setq-local merlin-cap--request-debug-cons debug-cons
                                  merlin-cap--request-stderr-proc stderr-proc
                                  merlin-cap--request-originating-buffer originating-buffer
                                  merlin-cap--request-position position
                                  merlin-cap--request-prefix prefix)))
                  (setcdr debug-cons 'pending)
                  buffer)
              (unless (eq (cdr debug-cons) 'pending)
                (delete-process proc)))))
      (unless (eq (cdr debug-cons) 'pending)
        (delete-process stderr-proc)))))

(defun merlin-cap--wait-for-request (request-buffer)
  "Wait for the process in REQUEST-BUFFER, then parse and return its output."
  (with-current-buffer request-buffer
    (cl-assert (eq (cdr merlin-cap--request-debug-cons) 'pending))
    (when-let ((proc (get-buffer-process (current-buffer))))
      ;; The process may have already finished and been deleted; but if it's still around,
      ;; wait for it.
      (while (accept-process-output proc))
      ;; Ensure that the sentinel runs.
      (delete-process proc))
    (delete-process merlin-cap--request-stderr-proc)
    (merlin-cap--interrupt-in-test 'process-finished)
    (setcdr merlin-cap--request-debug-cons (buffer-string))
    (unless (equal merlin-cap--process-last-event "finished\n")
      (error "merlin-cap--wait-for-request: %s %s" merlin-cap--process-last-event (cdr merlin-cap--request-debug-cons)))
    (goto-char (point-min))
    (let ((result (read (current-buffer))))
      (cl-assert (= (1+ (point)) (point-max))
                 "This request buffer contains more than one response from Merlin?")
      (merlin-cap--interrupt-in-test 'read-buffer)
      (merlin--handle-result (nth 0 (car merlin-cap--request-debug-cons))
                             (nth 1 (car merlin-cap--request-debug-cons))
                             result))))

(defcustom merlin-cap-dot-after-module t
  "Automatically add a dot after completing a module name.

Specifically, the completion strings returned for modules have a
\".\" appended to them which is included when accepting a module
as a completion.  For example, when this is non-nil, the List
module is completed as \"List.\" rather than \"List\".

This is convenient when completing a value name like
\"Foo.Bar.baz\", since you don't have to type \".\" after
completing \"Foo\" and \"Bar\".  When completing a module name
like \"(module Foo.Bar)\", this is less convenient, since you
need to remove the trailing dot after completing \"Bar\".  Since
the former is much more common than the latter, this is on by
default.

This also has a non-obvious effect: when module names have a dot
after them, partial completion works substantially better.  For
example, if this is on, `completion-at-point' will complete
\"Li.map\" to \"List.map\".

The reason for this is somewhat hard to explain, but essentially
the partial completion style tries to expand the module path by
expanding the glob \"Li*.\".  That expands to \"List.\" but not
to \"List\" because the latter doesn't include the trailing dot."
  :type 'boolean
  :group 'merlin)

(defun merlin-cap--process-completion-data (completion-data prefix)
  "Process COMPLETION-DATA from Merlin into an alist of completion to description.
PREFIX should be the prefix used to request this data."
  (let (ret)
    ;; Collect all the normal completion entries
    (dolist (entry (alist-get 'entries completion-data))
      (let ((name (alist-get 'name entry)))
        ;; We could delay some of this work to merlin-cap--annotate,
        ;; but it doesn't help much in benchmarks.
        (put-text-property
         0 (length name) 'merlin-cap--annotation
         (let ((kind (alist-get 'kind entry))
               (desc (replace-regexp-in-string "[\n ]+" " " (alist-get 'desc entry))))
           (cond
            ((equal kind "Module")
             (if merlin-cap-dot-after-module
                 (progn
                   (setq name (concat name "."))
                   nil)
               ":  <module>"))
            ((equal kind "Type")
             (format ": [%s]" desc))
            ((equal kind "Label")
             (concat ": " desc))
            ((member kind '("Constructor" "Variant"))
             (put-text-property 0 (length name) 'merlin-cap--sort-early t name)
             (concat ": " desc))
            (t (concat ": " desc))))
         name)
        (push name ret)))
    ;; Only include labels if there's no Dotted.Context. before the completion region
    (when (string-empty-p prefix)
      (dolist (label (alist-get 'labels (car (cdr-safe (alist-get 'context completion-data)))))
        (let ((name (alist-get 'name label))
              (type (alist-get 'type label)))
          (put-text-property 0 (length name) 'merlin-cap--sort-early t name)
          (put-text-property 0 (length name) 'merlin-cap--annotation (concat ": " type) name)
          (push name ret)
          (when (= (aref name 0) ??)
            (let ((tilde-name (substring name))
                  (tilde-type (string-remove-suffix " option" type)))
              (aset tilde-name 0 ?~)
              (put-text-property 0 (length tilde-name) 'merlin-cap--annotation (concat ": " tilde-type) tilde-name)
              (push tilde-name ret))))))
    (nreverse ret)))

(defun merlin-cap--omit-bounds ()
  "Return a region around point to omit when requesting Merlin completions."
  (let* ((atom (merlin-cap--atom-bounds))
         ;; By default, we omit the entire atom...
         (omit-start (car atom))
         (omit-end (cdr atom))
         (is-label (memq (char-after omit-start) '(?~ ?? ?`))))
    (unless is-label
      ;; ...except, to work around the lack of https://github.com/ocaml/merlin/issues/1751
      ;; we can't omit the entire atom if we're in a record projection.  Merlin ignores
      ;; PREFIX when determining what record we're projecting from, and only looks at the
      ;; buffer contents.  To support that, lower-case components before point in the
      ;; buffer must be included.
      (save-excursion
        ;; Go to the start of the current component...
        (skip-chars-backward "a-z0-9A-Z_'")
        (setq omit-start (point))
        ;; ...and skip backwards over uppercase components only, or components starting
        ;; with a star (to allow for partial-completion globbing)
        (while (and (not (zerop (skip-chars-backward ".")))
                    (not (zerop (skip-chars-backward "a-z0-9A-Z_*'")))
                    (let ((first-char (char-after (point))))
                      (or (eq 'Lu (get-char-code-property first-char 'general-category))
                          (eq first-char ?*))))
          (setq omit-start (point)))))
    (cons omit-start omit-end)))

(defun merlin-cap--display-sort-predicate (a b)
  "Sort completions A and B alphabetically, with a text property overriding."
  (let ((a-early (get-text-property 0 'merlin-cap--sort-early a))
        (b-early (get-text-property 0 'merlin-cap--sort-early b)))
    (cond
     ((and a-early (not b-early)) t)
     ((and (not a-early) b-early) nil)
     ;; Don't sort early completions alphabetically; merlin returns them
     ;; grouped them together by kind.
     ((and a-early b-early) nil)
     (t (string-lessp a b)))))

(defun merlin-cap--display-sort (completions)
  "Sort COMPLETIONS with `merlin-cap--display-sort-predicate'."
  (sort completions #'merlin-cap--display-sort-predicate))

(defun merlin-cap--get-completions (prefix)
  "Get (and cache) completions for PREFIX in the current buffer.

Specifically this returns an alist mapping completion candidates,
which don't include PREFIX, to a string description of the
completion.

Completions are cached in `merlin-cap--cache'."
  (let* ((omit-bounds (merlin-cap--omit-bounds))
         ;; point is inside omit-bounds, so just return the start of
         ;; the omitted region as the position.
         (position (car omit-bounds)))
    ;; Invalidate the cache whenever the position we're completing at has changed, or
    ;; the buffer contents (outside omit-bounds) has changed.  The latter is expensive
    ;; to check, though, so we only check if the position has changed.  That's close
    ;; enough, since a change before the position will probably change the position.
    (when (or (null merlin-cap--cache-position) (/= merlin-cap--cache-position position))
      (setq-local merlin-cap--cache-position position)
      (setq-local merlin-cap--cache nil))
    (let ((entry (assoc prefix merlin-cap--cache)))
      ;; If there is a pending request running whose prefix/position/buffer
      ;; matches this one, just wait for it to return a result.
      (unless entry
        (when-let ((request (merlin-cap--pending-request position omit-bounds)))
          (let ((data (merlin-cap--wait-for-request (cdr request))))
            (let ((request-entry (cons (car request) (merlin-cap--process-completion-data data prefix))))
              (push request-entry merlin-cap--cache)
              (when (equal (car request-entry) prefix)
                (setq entry request-entry))))))
      ;; Otherwise, send a new request to merlin
      (unless entry
        (if (string-search "*" prefix)
            ;; Merlin will incorrectly return completions when we have
            ;; a star in the prefix, even though that's not a valid
            ;; module name, so just forcibly return nothing.
            (progn
              (setq entry (cons prefix nil))
              (push entry merlin-cap--cache))
          (let* ((request-buffer (merlin-cap--complete-prefix position prefix omit-bounds))
                 (data (merlin-cap--wait-for-request request-buffer)))
            (setq entry (cons prefix (merlin-cap--process-completion-data data prefix)))
            (push entry merlin-cap--cache))))
      (cdr entry))))

(defun merlin-cap--last-position (needle haystack)
  "Get the index of the start of the last occurence of NEEDLE in HAYSTACK."
  (when-let ((pos (string-search needle (reverse haystack))))
    (- (length haystack) pos)))

(defun merlin-cap--table (string pred action)
  "Implement completion for merlin using `completion-at-point' API.

Does programmed completion using STRING, PRED, and ACTION; see
Info node `(elisp)Programmed Completion'.

This caches completions in `merlin-cap--cache'."
  (let* ((last-dot-position (merlin-cap--last-position "." string))
         (context (if last-dot-position (substring string 0 last-dot-position) ""))
         (component (if last-dot-position (substring string last-dot-position) string)))
    (cond
     ((eq action 'metadata)
      (list 'metadata
            (cons 'display-sort-function #'merlin-cap--display-sort)
            (cons 'annotation-function (when merlin-completion-types #'merlin-cap--annotate))))
     ((and (consp action) (eq (car action) 'boundaries))
      (let ((start (length context))
            (end (string-search "." (cdr action))))
        ;; include the . in the boundaries
        (when end (setq end (1+ end)))
        (cons 'boundaries (cons start end))))
     (t
      (let ((completions
             ;; Wrap in while-no-input if we're in what seems to be an idle completion, so that we
             ;; don't block the user.
             (if non-essential
                 (let ((ret (while-no-input (merlin-cap--get-completions context))))
                   (if (eq ret t)
                       ;; Interrupted by while-no-input, just complete on an empty list.
                       '()
                     ret))
               ;; Use with-local-quit in case something in our call stack binds
               ;; inhibit-quit; without this, our blocking calls to accept-process-output
               ;; will result in warnings being messaged.
               (with-local-quit (merlin-cap--get-completions context)))))
        (cond
         ((null action)
          (let ((ret (try-completion component completions pred)))
            (cond
             ((eq ret t) t)
             ((null ret) nil)
             ;; When try-completion returns a string, that string should cover the entire
             ;; completion region, not just the last component.
             (t (concat context ret)))))
         ((eq action t) (all-completions component completions pred))
         ((eq action 'lambda) (test-completion component completions pred))))))))

(defun merlin-cap--atom-bounds ()
  "This is like `merlin-bounds-of-ocaml-atom-at-point', but correct.

Also, this function treats \"*\" as a normal alphanumeric
character.  When partial-completion is in `completion-styles' (as
it is by default), this allows including arbitrary globs in the
middle of an OCaml atom and completing over them.

The bounds returned by that function incorrectly omit a \".\"
found at the end of the atom.  Also, it treats \"~foo.bar\" as an
atom."
  (let (atom-start atom-end)
    (save-excursion
      (skip-chars-backward "a-z0-9A-Z*_'")
      ;; Either skip over a single label-starting character, or a Dotted.Ocaml.atom
      (when (zerop (skip-chars-backward "?`~" (1- (point))))
        (skip-chars-backward "a-z0-9A-Z*_'."))
      (setq atom-start (point))
      ;; We may not have actually moved backwards at all, in which case we'll be
      ;; returning an atom found after point, possibly a label.
      (if (zerop (skip-chars-forward "?`~" (1+ (point))))
          (skip-chars-forward "a-z0-9A-Z*_'.")
        (skip-chars-forward "a-z0-9A-Z*_'"))
      (setq atom-end (point)))
    (cons atom-start atom-end)))

;; Public functions

(defun merlin-cap ()
  "Perform completion at point with merlin.

This completes only on the current fragment; e.g., with point at
the end of \"Aaa.bbb.ccc\", the current fragment is \"ccc\", and
we'll request all completions for \"Aaa.bbb.\" and filter them
down in Emacs.  This means if we backspace and type
\"Aaa.bbb.ddd\" instead, we won't need to re-request completions.

This caches completions between calls as long as we're completing
on the same ocaml atom at the same position, as determined by
`merlin-cap--atom-bounds'."
  (let ((atom (merlin-cap--atom-bounds)))
    (list (car atom) (cdr atom)
          #'merlin-cap--table
          :exit-function (when merlin-completion-types #'merlin-cap--exit-function))))

(defun merlin-cap--split-on-boundaries (prefix suffix table)
  "Split PREFIX and SUFFIX using `completion-boundaries' from TABLE.

Returns a list of the string before the boundaries, within the
boundaries, and after the boundaries."
  (let ((boundaries (completion-boundaries prefix table nil suffix)))
    (list
     (substring-no-properties prefix 0 (car boundaries))
     (concat (substring-no-properties prefix (car boundaries))
             (substring-no-properties suffix 0 (cdr boundaries)))
     (substring-no-properties suffix (cdr boundaries)))))

(defun merlin-cap--regions (prefix suffix)
  "Split PREFIX and SUFFIX with `merlin-cap' as if point was between them.

Returns:
- the part of the completion region before point that will be
  included when we send the buffer contents to Merlin
- the part of the completion region before the current completion
  boundaries, which is what will be sent to Merlin as \"-prefix\"
- the contents of the current completion boundaries
- the part of the OCaml atom after the current completion boundaries

This function is only used for testing."
  (with-temp-buffer
    (insert "?before-ignored  ")
    (insert prefix)
    (save-excursion (insert suffix) (insert "?after-ignored  "))
    (let ((cap (let ((merlin-cap-dot-after-module t)) (merlin-cap))))
      (cons
       (buffer-substring-no-properties (nth 0 cap) (car (merlin-cap--omit-bounds)))
       (merlin-cap--split-on-boundaries
        (buffer-substring-no-properties (nth 0 cap) (point))
        (buffer-substring-no-properties (point) (nth 1 cap))
        (nth 2 cap))))))

(ert-deftest test-merlin-cap--bounds ()
  (should (equal (merlin-cap--regions "Aaa.bbb.c" "cc.ddd")
                 '("Aaa.bbb." "Aaa.bbb." "ccc." "ddd")))
  (should (equal (merlin-cap--regions "~fo" "o.bar")
                 '("" "" "~foo" "")))
  (should (equal (merlin-cap--regions "" "~foo.bar")
                 '("" "" "~foo" "")))
  (should (equal (merlin-cap--regions "~fo" "o~bar")
                 '("" "" "~foo" "")))
  (should (equal (merlin-cap--regions "~foo" "~bar")
                 '("" "" "~foo" "")))
  (should (equal (merlin-cap--regions "~fo" "o.b~ar")
                 '("" "" "~foo" "")))
  ;; There's no obvious correct thing to return in this case, so this is fine.
  (should (equal (merlin-cap--regions "~foo.bar" "")
                 '("foo." "foo." "bar" "")))
  (should (equal (merlin-cap--regions "" "~")
                 '("" "" "~" "")))
  (should (equal (merlin-cap--regions "" "Aaa.bbb.ccc.ddd")
                 '("" "" "Aaa." "bbb.ccc.ddd")))
  (should (equal (merlin-cap--regions "A" "aa.bbb.ccc.ddd")
                 '("" "" "Aaa." "bbb.ccc.ddd")))
  ;; An "atom" can also just be a dotted path projecting from an expression
  (should (equal (merlin-cap--regions "(foo bar)." "")
                 '("." "." "" "")))
  (should (equal (merlin-cap--regions "(foo bar).Aa" "a")
                 '("." "." "Aaa" "")))
  (should (equal (merlin-cap--regions "(foo bar).Aaa.Bb" "b.ccc")
                 '("." ".Aaa." "Bbb." "ccc")))
  (should (equal (merlin-cap--regions "(foo bar).Aaa.bb" "b.ccc")
                 '("." ".Aaa." "bbb." "ccc")))
  (should (equal (merlin-cap--regions "(foo bar).aaa.bb" "b.ccc")
                 '(".aaa." ".aaa." "bbb." "ccc")))
  ;; We should omit only uppercase components before point, not lowercase ones
  (should (equal (merlin-cap--regions "M." "x")
                 '("" "M." "x" "")))
  (should (equal (merlin-cap--regions "M.t." "x")
                 '("M.t." "M.t." "x" "")))
  (should (equal (merlin-cap--regions "M.N." "x")
                 '("" "M.N." "x" "")))
  (should (equal (merlin-cap--regions "M.t.N." "x")
                 '("M.t." "M.t.N." "x" "")))
  (should (equal (merlin-cap--regions "aa.bB.CC.x" "")
                 '("aa.bB." "aa.bB.CC." "x" "")))
  (should (equal (merlin-cap--regions "Aa.bB.CC.x" "")
                 '("Aa.bB." "Aa.bB.CC." "x" "")))
  (should (equal (merlin-cap--regions "aa.Bb.cc.x" "")
                 '("aa.Bb.cc." "aa.Bb.cc." "x" "")))
  (should (equal (merlin-cap--regions "aa.Bb.Cc.x" "")
                 '("aa." "aa.Bb.Cc." "x" ""))))

(defun merlin-cap--current-message ()
  "Like `current-message' but work in batch mode and use `messages-buffer-name'."
  (with-current-buffer messages-buffer-name
    (save-excursion
      (forward-line -1)
      (buffer-substring (point) (pos-eol)))))

(defmacro merlin-cap--with-test-buffer (&rest body)
  "Run BODY with a temp buffer set up for Merlin completion."
  `(with-temp-buffer
     (merlin-mode)
     (setq-local completion-at-point-functions '(merlin-cap))
     (insert "
module Mmaa = struct
  module Mmbb = struct
    type ttaa = { ffaa : int }
    type ttbb = { ffbb : ttaa }
    let (vvaa : ttbb) = { ffbb = { ffaa = 0 } }
    ;;
  end
end

let () = ")
     ;; Don't log during the tests
     (let ((merlin-client-log-function nil))
       ,@body)))

(defun merlin-cap--test-complete (prefix suffix new-prefix new-suffix message)
  "Trigger completion with point between PREFIX and SUFFIX and compare results.

NEW-PREFIX and NEW-SUFFIX are what's before and after point after
completion, and MESSAGE is the message printed."
  (let ((start (point)))
    (insert prefix)
    (save-excursion (insert suffix))
    ;; clear any previous message, to avoid coalescing [no message]
    (message "\n")
    (message "[no message]")
    (completion-at-point)
    (let ((end (pos-eol))
          ;; Just so the ERT error renders more nicely
          (point (point)))
      (should (equal (list (buffer-substring start point)
                           (buffer-substring point end)
                           (merlin-cap--current-message))
                     (list new-prefix new-suffix message))))
    (delete-region start (pos-eol))))

(ert-deftest test-merlin-cap-completion ()
  (with-temp-buffer
    (let ((messages-buffer-name (buffer-name (current-buffer))))
      (merlin-cap--with-test-buffer
       (let ((merlin-cap-dot-after-module nil))
         (merlin-cap--test-complete "Mma" ""
                                    "Mmaa" ""
                                    "Mmaa:  <module>")
         (merlin-cap--test-complete "Mmaa.Mmb" ""
                                    "Mmaa.Mmbb" ""
                                    "Mmaa.Mmbb:  <module>")
         (merlin-cap--test-complete "Mmaa.Mmbb.vva" ""
                                    "Mmaa.Mmbb.vvaa" ""
                                    "Mmaa.Mmbb.vvaa: Mmaa.Mmbb.ttbb"))
       ;; Manually clear the cache, since the differences produced by
       ;; `merlin-cap-dot-after-module' are persisted in the cache.
       (setq-local merlin-cap--cache nil)
       (let ((merlin-cap-dot-after-module t))
         (merlin-cap--test-complete "Mma" ""
                                    "Mmaa." ""
                                    "[no message]")
         (merlin-cap--test-complete "Mmaa.Mmb" ""
                                    "Mmaa.Mmbb." ""
                                    "[no message]")
         (merlin-cap--test-complete "Mmaa.Mmbb.vva" ""
                                    "Mmaa.Mmbb.vvaa" ""
                                    "Mmaa.Mmbb.vvaa: Mmaa.Mmbb.ttbb")
         (should (equal (length merlin-cap--cache) 3))
         (merlin-cap--test-complete "Mmaa.Mmbb.vvaa.ff" ""
                                    "Mmaa.Mmbb.vvaa.ffbb" ""
                                    "Mmaa.Mmbb.vvaa.ffbb: Mmaa.Mmbb.ttbb -> Mmaa.Mmbb.ttaa")
         ;; When completing inside a record we have to include the record name in the
         ;; buffer contents sent to Merlin; that invalidates the cache
         (should (equal (length merlin-cap--cache) 1))
         (merlin-cap--test-complete "Mmaa.Mmbb.vvaa.ffbb.ff" ""
                                    "Mmaa.Mmbb.vvaa.ffbb.ffaa" ""
                                    "Mmaa.Mmbb.vvaa.ffbb.ffaa: Mmaa.Mmbb.ttaa -> int")
         ;; We're completing in a new part of the record, so again the cache is invalidated
         (should (equal (length merlin-cap--cache) 1))
         ;; completion in the middle of the atom
         (merlin-cap--test-complete "Mmaa.Mmb" ".vva"
                                    "Mmaa.Mmbb." "vva"
                                    "[no message]")
         ;; partial completion (PCM)
         (setq-local merlin-cap--cache nil)
         (merlin-cap--test-complete "Mma.Mmb.vva" ""
                                    "Mmaa.Mmbb.vvaa" ""
                                    "Mmaa.Mmbb.vvaa: Mmaa.Mmbb.ttbb")
         ;; The cache entries appear in reverse order of PCM's lookups;
         ;; first it looks up the existing string, removing a component from the end each time it finds no results;
         ;; eventually PCM just has "Mma." and it queries for "" to find completions, and it finds "Mmaa.";
         ;; from there it can query for "Mmaa." and "Mmaa.Mmbb." to find completions and expand each component.
         (should (equal (reverse (mapcar #'car merlin-cap--cache))
                        '("Mma.Mmb." "Mma." "" "Mmaa." "Mmaa.Mmbb.")))
         ;; partial completion with a glob
         (merlin-cap--test-complete "Mma.*.vva" ""
                                    "Mmaa.Mmbb.vvaa" ""
                                    "Mmaa.Mmbb.vvaa: Mmaa.Mmbb.ttbb")
         ;; When PCM looks up "Mma.*." and gets no results, that's how it knows it is safe to glob instead.
         (should (member "Mma.*." (mapcar #'car merlin-cap--cache)))
         ;; completion with no results
         (merlin-cap--test-complete "Mmaa.Mmbbxxx." ""
                                    "Mmaa.Mmbbxxx." ""
                                    "No match")
         ;; The lack of results is cached.
         (should (equal (length merlin-cap--cache) 7))
         ;; completion in and after a parenthesized expression
         (merlin-cap--test-complete "(Mmaa.Mmbb.vv" ""
                                    "(Mmaa.Mmbb.vvaa" ""
                                    "Mmaa.Mmbb.vvaa: Mmaa.Mmbb.ttbb")
         (merlin-cap--test-complete "(Mmaa.Mmbb.vvaa).ffb" ""
                                    "(Mmaa.Mmbb.vvaa).ffbb" ""
                                    ".ffbb: Mmaa.Mmbb.ttbb -> Mmaa.Mmbb.ttaa")
         ;; We're completing after a different expression, so no caching.
         (should (equal (length merlin-cap--cache) 1))
         (merlin-cap--test-complete "((fun x -> x) Mmaa.Mmbb.vvaa).ffbb.ffa" ""
                                    "((fun x -> x) Mmaa.Mmbb.vvaa).ffbb.ffaa" ""
                                    ".ffbb.ffaa: Mmaa.Mmbb.ttaa -> int"))))))

(ert-deftest test-merlin-cap-interrupts ()
  "Test that `merlin-cap' is robust to being interrupted.

At least at some hardcoded interruption points."
  (merlin-cap--with-test-buffer
   (let (syms)
     ;; Collect the interruption position symbols
     (cl-letf (((symbol-function 'merlin-cap--interrupt-in-test)
                (lambda (sym) (push sym syms))))
       (merlin-cap--get-completions ""))
     ;; Make sure we're actually doing something
     (should (> (length syms) 3))
     ;; For each position, interrupt at that position.
     (dolist (sym-to-interrupt syms)
       (let ((procs (process-list)))
         (let ((merlin-cap--interrupt-symbol sym-to-interrupt))
           ;; Interrupt it a few times, in case there's only an error the
           ;; second or third time.
           (should-error (merlin-cap--get-completions "Mmaa.")
                         :type 'merlin-cap--test-interrupt)
           ;; Also with a different prefix.
           (should-error (merlin-cap--get-completions "Non.existent.Thing.")
                         :type 'merlin-cap--test-interrupt)
           (should-error (merlin-cap--get-completions "Mmaa.")
                         :type 'merlin-cap--test-interrupt))
         (should (equal (merlin-cap--get-completions "Mmaa.") '("Mmbb")))
         ;; Remove the cache entry added by that presumably-successful completion.
         (setq merlin-cap--cache nil)
         ;; All the created processes have been deleted
         (should (equal (cl-set-difference (process-list) procs) '())))))))

(ert-deftest test-merlin-cap-closed-pipe ()
  "Test the Merlin server is robust to an EPIPE caused by Emacs.

We delete the Merlin client process without sending all input,
which causes the Merlin server to get EPIPE from all IO, which
it's had bugs with before.

Reliably reproducing these errors may require increasing the
count in `dotimes'."
  (merlin-cap--with-test-buffer
   (dotimes (_ 10)
     (dotimes (_ 3)
       (let ((merlin-cap--interrupt-symbol 'sent-half-input))
         (should-error (merlin-cap--get-completions "Mmaa.Mmbb.")
                       :type 'merlin-cap--test-interrupt)))
     (should (equal (merlin-cap--get-completions "Mmaa.") '("Mmbb"))))))

(defalias 'merlin-completion-at-point 'merlin-cap)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Register into completion-at-point ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun merlin-cap--setup ()
  (add-hook 'completion-at-point-functions #'merlin-completion-at-point nil 'local))

(add-hook 'merlin-mode-hook #'merlin-cap--setup)
(when merlin-mode (merlin-cap--setup))

(provide 'merlin-cap)
;;; merlin-cap.el ends here
