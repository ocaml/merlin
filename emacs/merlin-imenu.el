;;; merlin-imenu.el --- Merlin and imenu integration.   -*- coding: utf-8 -*-
;; Licensed under the MIT license.

;; Author: Ta Quang Trung <taquangtrungvn(_)gmail.com>
;; Created: 10 July 2016
;; Version: 0.1
;; Keywords: ocaml, imenu, merlin
;; URL:

(require 'imenu)
(require 'tuareg)
(require 'subr-x)
(require 'merlin)

;;; enable depth and size threshold for OCaml modules with big size
(setq max-lisp-eval-depth 10000)
(setq max-specpdl-size 10000)

;;;
;;; For debugging purpose
;;;
;; (defun merlin-imenu--print-item (item)
;;   (cl-labels ((visit (xs)
;;                      (cond ((null xs) "")
;;                            ((null (cdr xs)) (car (car xs)))
;;                            (t (concat (visit (cdr xs)) " / " (car (car xs)))))))
;;     (if (null item) "" (visit item))))

;;;
;;; For debugging purpose
;;;
;; (defun merlin-imenu--print-all-values (items)
;;   (cl-labels ((visit (xs)
;;                   (when (not (null xs))
;;                     (message (concat "Value: " (car (car xs))))
;;                     (visit (cdr xs))
;;                     )))
;;     (visit items)))

;;;
;;; For debugging purpose
;;;
;; (defun merlin-imenu--print-all-modules (items)
;;   (cl-labels ((visit (xs)
;;                   (when (not (null xs))
;;                     (message (concat "Module: " (car (car xs))))
;;                     (visit (cdr xs))
;;                     )))
;;     (visit items)))

(defun merlin-imenu--list-to-string (items)
  (cl-labels ((visit (xs)
                     (cond ((null xs) "")
                           ((null (cdr xs)) (car xs))
                           (t (concat (visit (cdr xs)) " / " (car xs))))))
    (if (null items) "" (visit items))))

;; go to the location of the item
(defun merlin-imenu--goto-item (line col item)
  (save-excursion
    ;; go to line
    (goto-char (point-min))
    (forward-line (- line 1))
    ;; go to column
    (move-to-column col)
    ;; go to the beginning position of the item
    (search-forward item)
    (search-backward item)
    ;; return the marker
    (point)))

;; the main indexing function
(defun merlin-imenu-create-index ()
  "Set imenu function"
  (merlin/sync)
  (let* ((pos (merlin/unmake-point (point)))
         (outline (merlin/send-command `outline)))
    (when outline
      ;; (message outline)
      (let ((module-list) (value-list) (type-list)
            (class-list) (exception-list) (constructor-list)
            (label-list) (misc-list)
            (marker) (start-pos) (fitem))
        (cl-labels
            ((visit-one (prefix x)
                        (let* ((fstart (nth 1 x))
                               (fname (nth 3 x))
                               (fkind (nth 4 x))
                               (start-line (cdr (nth 2 fstart)))
                               (start-col (cdr (nth 3 fstart)))
                               (name (cdr fname))
                               (kind (cdr fkind))
                               (children (nth 5 x))
                               (item (merlin-imenu--list-to-string
                                      (cons name prefix))))
                          (progn
                            (setq marker (make-marker))
                            (setq start-pos
                                  (merlin-imenu--goto-item start-line
                                                           start-col
                                                           name))
                            (set-marker marker start-pos)
                            (setq fitem (cons item marker))
                            ;; (message "name %s : kind %s" name kind)
                            (cond
                             ((string= (string-trim kind) "Value")
                              (setq value-list
                                    (cons fitem value-list)))
                             ((string= (string-trim kind) "Type")
                              (setq type-list
                                    (cons fitem type-list)))
                             ((string= (string-trim kind) "Class")
                              (setq class-list
                                    (cons fitem class-list)))
                             ((string= (string-trim kind) "Module")
                              (setq module-list
                                    (cons fitem module-list)))
                             ((string= (string-trim kind) "Exn")
                              (setq exception-list
                                    (cons fitem exception-list)))
                             ((string= (string-trim kind) "Constructor")
                              (setq constructor-list
                                    (cons fitem constructor-list)))
                             ((string= (string-trim kind) "Label")
                              (setq label-list
                                    (cons fitem label-list)))
                             (t (setq misc-list (cons fitem misc-list))))
                            (if (not (null (cdr children)))
                                (visit-many (cons name prefix)
                                            (cdr children))))))
             (visit-many (prefix xs)
                         (when (not (null xs))
                           (visit-one prefix (car xs))
                           (visit-many prefix (cdr xs)))))
          (visit-many '() outline)
          ;; (merlin-imenu--print-all-values value-list)
          (let ((index ()))
            (when module-list
              (push (cons "Module" module-list) index))
            (when exception-list
              (push (cons "Exception" exception-list) index))
            (when label-list
              (push (cons "Label" label-list) index))
            (when constructor-list
              (push (cons "Constructor" constructor-list) index))
            (when type-list
              (push (cons "Type" type-list) index))
            (when class-list
              (push (cons "Class" class-list) index))
            (when value-list
              (push (cons "Value" value-list) index))
            (when misc-list
              (push (cons "Misc" misc-list) index))
            index))))))

;; enable Merlin to use the merlin-imenu module
(defun merlin-use-merlin-imenu ()
  "Merlin: use the custom imenu feature from Merlin"
  (interactive)
  ;; change the index function
  (setq imenu-create-index-function 'merlin-imenu-create-index)
  ;;;;; For testing: comment out the above line and
  ;;;;; uncomment the below function to print time spent by merlin-imenu
  ;; (setq imenu-create-index-function
  ;;       '(lambda ()
  ;;          (setq time (current-time))
  ;;          (setq res (merlin-imenu-create-index))
  ;;          (message "** Time spent by Merlin-Imenu: %0.6fs"
  ;;                   (float-time (time-since time)))
  ;;          res))
  ;; clear the index list of imenu to force a rescan
  (imenu--cleanup)
  (setq imenu--index-alist nil)
  ;; (imenu--menubar-select imenu--rescan-item)
  (message "Merlin: merlin-imenu is selected, rescanning buffer..."))

;; enable Merlin to use the default tuareg-imenu module
(defun merlin-use-tuareg-imenu ()
  "Merlin: use the default imenu feature from Tuareg"
  (interactive)
  ;; change the index function
  (setq imenu-create-index-function 'tuareg-imenu-create-index)
  ;;;;; For testing: comment out the above line and
  ;;;;; uncomment the below function to print time spent by tuareg-imenu
  ;; (setq imenu-create-index-function
  ;;       '(lambda ()
  ;;          (setq time (current-time))
  ;;          (setq res (tuareg-imenu-create-index))
  ;;          (message "** Time spent by Tuareg-Imenu: %0.6fs"
  ;;                   (float-time (time-since time)))
  ;;          res))
  ;; clear the index list of imenu to force a rescan
  (imenu--cleanup)
  (setq imenu--index-alist nil)
  ;; (imenu--menubar-select imenu--rescan-item)
  (message "Merlin: tuareg-imenu is selected, rescanning buffer..."))

;; (message "Eval Merlin-IMenu")  ;; for debugging

(provide 'merlin-imenu)
;;; merlin-imenu.el ends here
