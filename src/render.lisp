;;;; render.lisp
;;;;
;;;; This file is part of the rigidus.ru web site, released under GNU Affero General Public License, Version 3.0
;;;; See file COPYING for details.
;;;;
;;;; Authors:
;;;;   Moskvitin Andrey <archimag@gmail.com>
;;;;   Glukhov Michail <i.am.rigidus@gmail.com>


(in-package #:rigidus)

(defclass orgdata ()
  ((content :accessor orgdata-content)
   (sections :accessor orgdata-sections)
   (directives :accessor orgdata-directives)))

(defun menu ()
  (list (list :link "/" :title "Главная")
        (list :link "/about" :title "About")
        (list :link "/articles/" :title "Статьи")
        (list :link "/resources/" :title "Ресурсы")
        (list :link "/contacts" :title "Контакты")))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; default-render
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defclass rigidus-render () ())

(setf *default-render-method* (make-instance 'rigidus-render))

(defmethod restas:render-object ((designer rigidus-render) (data list))
  (destructuring-bind (headtitle navpoints content) data
    (tpl:root (list :headtitle headtitle
                    :content (tpl:base (list :navpoints navpoints
                                             :content content
                                             :stat (tpl:stat)))))))

(defmethod restas:render-object ((designer rigidus-render) (file pathname))
  (if (string= (pathname-type file) "org")
      (restas:render-object designer (parse-org file))
      (call-next-method)))

(defmethod restas:render-object ((designer rigidus-render) (data orgdata))
  (let* ((content (orgdata-content data))
         (sections (orgdata-sections data))
         (directives (orgdata-directives data))
         (title (getf directives :title))
         (menu-memo (menu)))
    (restas:render-object
     designer
     (list title
           menu-memo
           (tpl:default
               (list :title title
                     :navpoints menu-memo
                     :sections (iter (for i from 1)
                                     (for section in sections)
                                     (collect (list :anchor (format nil "anchor-~a" i)
                                                    :level (format nil "level-~a" (car section))
                                                    :title (cadr section))))
                     :links (get-sape-links (hunchentoot:REQUEST-URI*))
                     :content (get-sape-context (hunchentoot:REQUEST-URI*) content)))))))

;; (defclass rigidus-wiki-render (restas.wiki:drawer) ())

;; (defmethod restas.wiki:finalize-page ((drawer rigidus-wiki-render) content)
;;   (tpl:root (list :headtitle (getf content :title)
;;                   :content (tpl:base (list :navpoints (menu)
;;                                            :content (concatenate 'string
;;                                                                  (restas.wiki.view:show-page-menu (getf content :menu-links))
;;                                                                  (getf content :content))
;;                                            :stat (tpl:stat))))))

;; (defmethod restas.wiki:generate-content-from-markup ((drawer rigidus-wiki-render) data)
;;   (orgdata-content (parse-org data)))

