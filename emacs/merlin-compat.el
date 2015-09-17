;;; merlin-compat.el --- Deprecated merlin-mode functions.   -*- coding: utf-8 -*-
;; Licensed under the MIT license.

;; Author: Frédéric Bour <frederic.bour(_)lakaban.net>
;; Created: 22 Jul 2015
;; Version: 0.1
;; Keywords: ocaml languages
;; URL: http://github.com/the-lambda-church/merlin

(require 'merlin)

(defalias 'merlin--completion-bounds      'merlin/completion-bounds)
(defalias 'merlin--buffer-substring       'merlin/buffer-substring)
(defalias 'merlin-send-command-async      'merlin/send-command-async)
(defalias 'merlin--completion-split-ident 'merlin/completion-split-ident)
(defalias 'merlin--completion-data        'merlin/complete)
(defalias 'merlin--completion-prefix      'merlin/completion-prefix)

(defun merlin-sync-to-point (&rest ignored)
  (merlin/sync))
(defun merlin/sync-to-end ()
  (merlin/sync))

(defun merlin-refresh ()
  "Deprecated. Was used to reload cmis, this is handled automatically by merlin now."
  (interactive))
(make-obsolete 'merlin-refresh nil "merlin 2.0")

(provide 'merlin-compat)
;;; merlin-compat.el ends here
