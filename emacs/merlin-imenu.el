;;; merlin-imenu.el --- Merlin and imenu integration.   -*- coding: utf-8 -*-
;; Licensed under the MIT license.

;; Author: Ta Quang Trung
;; Version: 0.3
;; Release log:
;;   - v0.1: July 2016
;;   - v0.2: 27 April 2017
;;   - v0.3: 21 August 2019
;; Keywords: ocaml, imenu, merlin
;; URL:

(require 'imenu)
(require 'tuareg)
(require 'subr-x)
(require 'merlin)

;;; enable depth and size threshold for OCaml modules with big size
(setq max-lisp-eval-depth 10000)
(setq max-specpdl-size 10000)

;; lists of different outline items
(defvar-local value-list nil)
(defvar-local type-list nil)
(defvar-local exception-list nil)

(defun merlin-imenu-compute-position (line col)
  "Get location of the item."
  (save-excursion
    (condition-case nil
        (progn
          (goto-char (point-min))
          (forward-line (- line 1))
          (move-to-column col)
          (point))
      (error -1))))

(defun merlin-imenu-create-entry (prefix name type kind line col)
  (let* ((name (concat prefix name))
         (type (cond ((not (string= kind "Value")) "null")
                     ((not (string= type "null")) type)
                     (t (let* ((types (merlin/call
                                       "type-enclosing"
                                       "-position" (format "%d:%d" line col)
                                       "-expression" name)))
                          (cdr (nth 3 (car types)))))))
         (type (replace-regexp-in-string "\n" " " type))
         (type (propertize type 'face 'font-lock-doc-face)))
    (if (string= type "null") name (concat name " : " type))))

(defun merlin-imenu-parse-outline-item (prefix item)
  "Parse one item of the outline tree."
  ;; (message "Item: %s" item)
  (let* ((line (cdr (assoc 'line (assoc 'start item))))
         (col (cdr (assoc 'col (assoc 'start item))))
         (name (cdr (assoc 'name item)))
         (kind (cdr (assoc 'kind item)))
         (type (cdr (assoc 'type item)))
         (sub-trees (cdr (assoc 'children item)))
         (entry (merlin-imenu-create-entry prefix name type kind line col))
         (position (merlin-imenu-compute-position line col))
         (marker (cons entry (set-marker (make-marker) position))))
    (cond ((string= kind "Value")
           (setq value-list (cons marker value-list)))
          ((string= kind "Type")
           (setq type-list (cons marker type-list)))
          ((string= kind "Exn")
           (setq exception-list (cons marker exception-list))))
    (if (and (listp sub-trees) (not (null sub-trees)))
        (merlin-imenu-parse-outline-tree (concat prefix entry " / ")
                                         sub-trees))))

(defun merlin-imenu-parse-outline-tree (prefix outline)
  "Parse outline tree."
  (when (not (null outline))
    (merlin-imenu-parse-outline-item prefix (car outline))
    (merlin-imenu-parse-outline-tree prefix (cdr outline))))

(defun merlin-imenu-create-index ()
  "Create data for imenu using the merlin outline feature."
  (interactive)
  ;; Reset local vars
  (setq value-list nil
        type-list nil
        exception-list nil)
  ;; Read outline tree
  (merlin-imenu-parse-outline-tree "" (merlin/call "outline"))
  (let ((index ()))
    (when value-list (push (cons "Value" value-list) index))
    (when exception-list (push (cons "Exception" exception-list) index))
    (when type-list (push (cons "Type" type-list) index))
    index))

;; enable Merlin to use the merlin-imenu module
(defun merlin-use-merlin-imenu ()
  "Merlin: use the custom imenu feature from Merlin"
  (interactive)
  ;; change the index function and force a rescan of imenu-index
  (setq imenu-create-index-function 'merlin-imenu-create-index)
  (imenu--cleanup)
  (setq imenu--index-alist nil)
  (message "Merlin: merlin-imenu is selected, rescanning buffer..."))

;; enable Merlin to use the default tuareg-imenu module
(defun merlin-use-tuareg-imenu ()
  "Merlin: use the default imenu feature from Tuareg"
  (interactive)
  ;; change the index function and force a rescan of imenu-index
  (setq imenu-create-index-function 'tuareg-imenu-create-index)
  (imenu--cleanup)
  (setq imenu--index-alist nil)
  (message "Merlin: tuareg-imenu is selected, rescanning buffer..."))

(provide 'merlin-imenu)
;;; merlin-imenu.el ends here
