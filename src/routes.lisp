;;;; routes.lisp
;;;;
;;;; This file is part of the rigidus.ru web site, released under GNU Affero General Public License, Version 3.0
;;;; See file COPYING for details.
;;;;
;;;; Author: Moskvitin Andrey <archimag@gmail.com>

(in-package #:rigidus)

(restas:define-route main ("/")
  (let* ((lines (iter (for line in-file (path "afor.txt") using #'read-line)
                     (collect line)))
         (line (nth (random (length lines))
                    lines)))
    (list "Программирование - как искусство"
          (menu)
          (tpl:main (list :title line)))))

(restas:define-route about ("about")
  (path "content/about.org"))

(restas:define-route contacts ("contacts")
  (path "content/contacts.org"))

(restas:define-route articles ("articles/")
  (let ((data (parse-org (path "content/articles/articles.org"))))
    (setf (orgdata-content data)
          (ppcre:regex-replace-all
                     "@make-list-by-category(.*)@"
                     (orgdata-content data)
                     (list #'(lambda (match reg)
                               (declare (ignore match))
                               (let* ((instr (string-trim '(#\Space #\Tab #\Newline) reg)))
                                 (multiple-value-bind (star color category)
                                     (values-list (split-sequence:split-sequence #\Space instr))
                                   (format nil
                                           "<ul>~{~a~}</ul>"
                                           (iter (for x in (find-articles-by-category category))
                                                 (collect (tpl:li (append x (list :star star :color color))))))))))
                     :simple-calls t))
    data))

(restas:define-route article ("articles/:article")
  (path (format nil "content/articles/~A.org" article)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; submodules
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; (restas:mount-submodule -static- (#:restas.directory-publisher)
;;   (restas.directory-publisher:*directory* (path "static/")))

;; (restas:mount-submodule -resources- (#:restas.directory-publisher)
;;   (restas.directory-publisher:*baseurl* '("resources"))
;;   (restas.directory-publisher:*directory* (path "content/resources/"))
;;   (restas.directory-publisher:*default-render-method* *default-render-method*)
;;   (restas.directory-publisher:*directory-index-files* '("resources.org")))

;; (restas:mount-submodule -wiki- (#:restas.wiki)
;;   (restas.wiki:*baseurl* '("wiki"))
;;   (restas.wiki:*storage* (make-instance 'restas.wiki:file-storage
;;                                         :dir #P"/var/rigidus.ru/wiki/"))
;;   (restas.wiki:*default-render-method* (make-instance 'rigidus-wiki-render))
;;   (restas.wiki:*wiki-user-function* #'(lambda () "anonymous")))


(defparameter *host* "localhost")
(defparameter *port* 8092)

(restas:start '#:wiki :hostname (format nil "wiki.~a" *host*) :port *port*)
(restas:start '#:rigidus :hostname *host* :port *port*)
(setf restas:*default-host-redirect* *host*)
