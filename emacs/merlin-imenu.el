;;; merlin-imenu.el --- Merlin and imenu integration.   -*- coding: utf-8 -*-
;; Licensed under the MIT license.

;; Author: Ta Quang Trung <taquangtrungvn(_)yahoo.com>
;; Created: 10 July 2016
;; Version: 0.1
;; Keywords: ocaml languages
;; URL: 

(require 'imenu)
(require 'tuareg)


(defun merlin-imenu--list-to-string (items)
  (cl-labels ((visit (xs)
                     (cond ((null xs) "")
                           ((null (cdr xs)) (car xs))
                           (t (concat (visit (cdr xs)) " / " (car xs))))))
    (if (null items) "" (visit items))))

(defun merlin-imenu--print-item (item)
  (cl-labels ((visit (xs)
                     (cond ((null xs) "")
                           ((null (cdr xs)) (car (car xs)))
                           (t (concat (visit (cdr xs)) " / " (car (car xs)))))))
    (if (null item) "" (visit item))))

(defun merlin-imenu--print-all-values (items)
  (cl-labels ((visit (xs)
                  (when (not (null xs))
                    (message (concat "Value: " (car (car xs))))
                    (visit (cdr xs))
                    )))
    (visit items)))

(defun merlin-imenu--print-all-modules (items)
  (cl-labels ((visit (xs)
                  (when (not (null xs))
                    (message (concat "Module: " (car (car xs))))
                    (visit (cdr xs))
                    )))
    (visit items)))

(defun merlin-imenu-create-index ()
  "Set imenu function"
  (interactive)
  (merlin/sync)
  (let* ((pos (merlin/unmake-point (point)))
         (outline (merlin/send-command `outline)))
    (when outline
      ;; (message outline)
      (let ((module-list) (value-list) (type-list) (class-list) (misc-list))
        (cl-labels
            ((visit-one (prefix x)
                        (let* ((fstart (nth 1 x))
                               (fend (nth 2 x))
                               (fname (nth 3 x))
                               (fkind (nth 4 x))
                               (start-line (cdr (nth 2 fstart)))
                               (start-col (cdr (nth 3 fstart)))
                               (end-line (cdr (nth 2 fend)))
                               (end-col (cdr (nth 3 fend)))
                               (name (cdr fname))
                               (kind (cdr fkind))
                               (children (nth 5 x)))
                          (message "start line %d" start-line)
                          (message "start col %d" start-col)
                          (message "end line %d" end-line)
                          (message "end col %d" end-col)
                          ;; (message (concat "start line -- " (nth 1 fstart)))
                          (if (null (cdr children))
                              (let* ((item (merlin-imenu--list-to-string
                                            (cons name prefix)))
                                     (fitem (cons item (point-marker))))
                                ;; (message (concat "Item: " item))
                                (cond
                                 ((string= (string-trim kind) "Value")
                                  (setq value-list (cons fitem value-list)))
                                 ((string= (string-trim kind) "Type")
                                  (setq type-list (cons fitem type-list)))
                                 ((string= (string-trim kind) "Class")
                                  (setq class-list (cons fitem class-list)))
                                 ((string= (string-trim kind) "Module")
                                  (setq module-list (cons fitem module-list)))
                                 (t
                                  (setq misc-list (cons fitem misc-list)))))
                            (visit-many (cons name prefix) (cdr children))
                            )))
             (visit-many (prefix xs)
                         (when (not (null xs))
                           (visit-one prefix (car xs))
                           (visit-many prefix (cdr xs)))))
          (visit-many '() outline)
          (merlin-imenu--print-all-values value-list)
          ;; (merlin-imenu--print-all-modules module-list)
          (let ((index ()))
            (when module-list (push (cons "Module" module-list) index))
            (when type-list   (push (cons "Type" type-list) index))
            (when class-list  (push (cons "Class" class-list) index))
            (when value-list  (push (cons "Value" value-list) index))
            (when misc-list   (push (cons "Misc" misc-list) index))
            index)
          )))))

(defun merlin-use-merlin-imenu ()
  "Merlin: use the custom imenu feature from Merlin"
  (interactive)
  ;; change the index function
  (setq imenu-create-index-function 
        'merlin-imenu-create-index)
  ;; clear the index list of imenu to force a rescan
  (imenu--cleanup)
  (setq imenu--index-alist nil)
  ;; (imenu--menubar-select imenu--rescan-item)
  (message "Merlin1: merlin-imenu is selected, rescanning buffer..."))

(defun merlin-use-tuarge-imenu ()
  "Merlin: use the default imenu feature from Tuareg"
  (interactive)
  ;; change the index function
  (setq imenu-create-index-function 
        'tuareg-imenu-create-index)
  ;; clear the index list of imenu to force a rescan
  (imenu--cleanup)
  (setq imenu--index-alist nil)
  ;; (imenu--menubar-select imenu--rescan-item)
  (message "Merlin: tuareg-imenu is selected, rescanning buffer...")
  )

(message "Eval Merlin-IMenu")

(provide 'merlin-imenu)
;;; merlin.el ends here
