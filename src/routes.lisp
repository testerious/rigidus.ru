;;;; routes.lisp
;;;;
;;;; This file is part of the rigidus.ru web site, released under GNU Affero General Public License, Version 3.0
;;;; See file COPYING for details.
;;;;
;;;; Author: Moskvitin Andrey <archimag@gmail.com>

(in-package #:rigidus)

(defun get-sape-links (uri)
  (let ((rs "")
        (extproc (sb-ext:run-program "/usr/bin/php" '("-q" "links.php")
                                     :environment (append (sb-ext:posix-environ)
                                                          (list (format nil "REQUEST_URI=~a" uri)))
                                     :wait nil
                                     :input nil
                                     :output :stream)))
    (unwind-protect
         (with-open-stream (out (sb-ext:process-output extproc))
           (setf rs (loop
                       :for line := (read-line out nil nil)
                       :while line
                       :collect line))))
    (when extproc (sb-ext:process-close extproc))
    (format nil "~{~a~}" rs)))

(defun get-sape-context (uri content)
  (let* ((rs "")
         (input-stream (make-string-input-stream content))
         (extproc (sb-ext:run-program "/usr/bin/php" '("-q" "context.php")
                               :environment (append (sb-ext:posix-environ)
                                                    (list (format nil "REQUEST_URI=~a" uri)))
                               :wait nil
                               :input input-stream
                               :output :stream)))
      (unwind-protect
           (with-open-stream (out (sb-ext:process-output extproc))
             (setf rs (loop
                         :for line := (read-line out nil nil)
                         :while line
                         :collect line))))
      (when extproc (sb-ext:process-close extproc))
      (format nil "~{~a~}" rs)))


(restas:define-route main ("/")
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

;; (restas:define-route accel ("/static")
;;   ;; (setf (hunchentoot:header-out :x-accel-redirect)
;;   ;;       "/home/rigidus/rigidus.ru/static/style.css")
;;   "---")


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



;; (restas:mount-submodule -wiki- (#:restas.wiki)
;;   (restas.wiki:*baseurl* '("wiki"))
;;   (restas.wiki:*storage* (make-instance 'restas.wiki:file-storage
;;                                         :dir #P"/var/rigidus.ru/wiki/"))
;;   (restas.wiki:*default-render-method* (make-instance 'rigidus-wiki-render))
;;   (restas.wiki:*wiki-user-function* #'(lambda () "anonymous")))


(defparameter *host* "localhost")
(defparameter *port* 8092)

;; (restas:start '#:restas.wiki :hostname (format nil "wiki.~a" *host*) :port *port*)
(restas:start '#:rigidus :hostname *host* :port *port*)
(setf restas:*default-host-redirect* *host*)

;; (restas:make-context (restas.directory-publisher:*baseurl* '("tmp"))
;;                      (restas.directory-publisher:*directory* #P"/tmp/")
;;                      (restas.directory-publisher:*autoindex* t)))
;; http://restas.lisper.ru/ru/manual/modules.html#%D0%B4%D1%83%D0%B0%D0%BB%D0%B8%D0%B7%D0%BC
