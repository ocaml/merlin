;;; merlin-cap-test.el --- Merlin and completion-at-point integration   -*- coding: utf-8; lexical-binding: t -*-
;; Licensed under the MIT license.

;; Author: Simon Castellan <simon.castellan(_)iuwt.fr>
;;         Frédéric Bour <frederic.bour(_)lakaban.net>
;;         Thomas Refis <thomas.refis(_)gmail.com>
;;         Tim McGilchrist <timmcgil@gmail.com>
;; Created: 13 Sep 2024
;; Version: 0.1
;; Keywords: ocaml languages
;; Package-Requires: ((emacs "25.1"))
;; URL: http://github.com/ocaml/merlin

;;; Commentary:

;; Run tests for merlin-completion-at-point code.

;;; Code:

(require 'merlin)
(require 'ert)

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

(defvar-local messages-buffer-name "*Messages*")

(defun merlin-cap--current-message ()
  "Like `current-message' but work in batch mode and use `messages-buffer-name'."
  (with-current-buffer messages-buffer-name
    (save-excursion
      (forward-line -1)
      (buffer-substring (point) (line-end-position)))))

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
    (let ((end (line-end-position))
          ;; Just so the ERT error renders more nicely
          (point (point)))
      (should (equal (list (buffer-substring start point)
                           (buffer-substring point end)
                           (merlin-cap--current-message))
                     (list new-prefix new-suffix message))))
    (delete-region start (line-end-position))))

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
         (should (equal (merlin-cap--get-completions "Mmaa.") '("Mmbb.")))
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
     (should (equal (merlin-cap--get-completions "Mmaa.") '("Mmbb."))))))

(provide 'merlin-cap-test)
;;; merlin-cap-test.el ends here
