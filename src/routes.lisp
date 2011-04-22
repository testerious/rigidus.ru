;;;; routes.lisp
;;;;
;;;; This file is part of the rigidus.ru web site, released under GNU Affero General Public License, Version 3.0
;;;; See file COPYING for details.
;;;;
;;;; Authors:
;;;;   Moskvitin Andrey <archimag@gmail.com>
;;;;   Glukhov Michail <i.am.rigidus@gmail.com>

(in-package #:rigidus)


(restas:define-route main ("/")
  ;; (hunchentoot:set-cookie "test-cookie" :value "test-value")
  ;; (hunchentoot:set-cookie "test-cookie2" :value "test-value2")
  (let* ((lines (iter (for line in-file (path "afor.txt") using #'read-line)
                     (collect line)))
         (line (nth (random (length lines))
                    lines)))
    (list "Программирование - как искусство"
          (menu)
          (tpl:main (list :title line
                          :links (get-sape-links "/"))))))

(restas:define-route about ("about")
  (path "content/about.org"))

(restas:define-route contacts ("contacts")
  (path "content/contacts.org"))

(defparameter *cached-articles* nil)

(restas:define-route articles ("articles/")
  (if (null *cached-articles*)
      (progn
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
          (setf *cached-articles* data)))
      ;; else
      *cached-articles*))


(restas:define-route article ("articles/:article")
  (gethash article *articles*))
  ;; (path (format nil "content/articles/~A.org" article)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; submodules
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(restas:mount-submodule -static- (#:restas.directory-publisher)
  (restas.directory-publisher:*directory* (path "static/")))

;; ;; новый вариант для http://wiki.nginx.org/XSendfile
;; (restas:mount-submodule -static- (#:restas.directory-publisher restas:@nginx-accel-redirect)
;;   (restas.directory-publisher:*directory* (path "static/")))

(restas:mount-submodule -resources- (#:restas.directory-publisher)
  (restas.directory-publisher:*baseurl* '("resources"))
  (restas.directory-publisher:*directory* (path "content/resources/"))
  (restas.directory-publisher:*default-render-method* *default-render-method*)
  (restas.directory-publisher:*directory-index-files* '("resources.org")))

(restas:mount-submodule -wiki- (#:restas.wiki)
  (restas.wiki:*baseurl* '("wiki"))
  (restas.wiki:*storage* (make-instance 'restas.wiki:file-storage
                                        :dir (path "wiki/")))
  (restas.wiki:*default-render-method* (make-instance 'rigidus-wiki-render))
  (restas.wiki:*wiki-user-function* #'(lambda () "anonymous")))



;; (setf restas:*default-host-redirect* *host*)

;; (restas:make-context (restas.directory-publisher:*baseurl* '("tmp"))
;;                      (restas.directory-publisher:*directory* #P"/tmp/")
;;                      (restas.directory-publisher:*autoindex* t)))
;; http://restas.lisper.ru/ru/manual/modules.html#%D0%B4%D1%83%D0%B0%D0%BB%D0%B8%D0%B7%D0%BC
