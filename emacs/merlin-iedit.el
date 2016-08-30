;;; merlin-iedit.el --- Merlin and iedit integration.   -*- coding: utf-8 -*-
;; Licensed under the MIT license.

;; Author: Simon Castellan <simon.castellan(_)iuwt.fr>
;;         Frédéric Bour <frederic.bour(_)lakaban.net>
;;         Thomas Refis <thomas.refis(_)gmail.com>
;; Created: 27 June 2014
;; Version: 0.1
;; Keywords: ocaml languages
;; URL: http://github.com/the-lambda-church/merlin

(require 'iedit)

(defun merlin-iedit--printable ()
  "Stub substituting `iedit-printable' during merlin-iedit-occurrences"
  "merlin-iedit-occurrences")

(defun merlin-iedit--make-occurrences-overlays (occurrences)
  "Stub substituting `iedit-make-occurrences-overlays' during merlin-iedit-occurrences"
  (setq iedit-aborting nil)
  (setq iedit-occurrences-overlays nil)
  (setq iedit-read-only-occurrences-overlays nil)
  (save-excursion
    (save-window-excursion
      (dolist (pos occurrences)
        (let* ((start (assoc 'start pos))
               (end   (assoc 'end   pos))
               (beginning (merlin/make-point start))
               (ending    (merlin/make-point end)))
          (if (text-property-not-all beginning ending 'read-only nil)
              (push (iedit-make-read-only-occurrence-overlay beginning ending)
                    iedit-read-only-occurrences-overlays)
            (push (iedit-make-occurrence-overlay beginning ending)
                  iedit-occurrences-overlays))))
      (when (and occurrences iedit-unmatched-lines-invisible)
        (iedit-hide-unmatched-lines iedit-occurrence-context-lines))))
  (length occurrences))

(defun merlin-iedit-occurrences ()
  "Edit occurrences of identifier under cursor using `iedit'"
  (interactive)
  (merlin/sync)
  (let* ((pos (merlin/unmake-point (point)))
         (r (merlin/send-command `(occurrences ident at ,pos))))
    (when r
      (if (listp r)
          (flet ((iedit-printable (a)
                   (merlin-iedit--printable))
                 (iedit-make-occurrences-overlays (a b c)
                   (merlin-iedit--make-occurrences-overlays a)))
            (iedit-start r (point-min) (point-max)))
        (message r)))))

(provide 'merlin-iedit)
;;; merlin.el ends here
