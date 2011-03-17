;;;; routes.lisp
;;;;
;;;; This file is part of the rigidus.ru web site, released under GNU Affero General Public License, Version 3.0
;;;; See file COPYING for details.
;;;;
;;;; Author: Moskvitin Andrey <archimag@gmail.com>

(in-package #:rigidus)

(defun base64-cookies ()
  (let* ((cookies   (hunchentoot:cookies-out*))
         (serialize (mapcar #'(lambda (x)
                                (let ((name (car x))
                                      (value (hunchentoot:cookie-value (cdr x))))
                                  (format nil "s:~d:\"~a\";s:~d:\"~a\";"
                                          (length name)
                                          name
                                          (length value)
                                          value)))
                            cookies))
         (seri-str  (format nil "a:~d:{~a}"
                            (length cookies)
                            (if (null cookies)
                                ""
                                (format nil "~{~a~}" serialize)))))
    (base64:string-to-base64-string seri-str)))

(defun recode (content from to)
  (sb-ext:octets-to-string (sb-ext:string-to-octets content :external-format from) :external-format to))

(defun get-sape-links (uri)
  (let ((rs "")
        (extproc (sb-ext:run-program "/usr/bin/php" '("-q" "links.php")
                                     :environment (append (sb-ext:posix-environ)
                                                          (list (format nil "REQUEST_URI=~a" uri))
                                                          (list (format nil "COOKIE=~a" (base64-cookies))))
                                     :wait nil
                                     :input nil
                                     :output :stream)))
    (unwind-protect
         (with-open-stream (out (sb-ext:process-output extproc))
           (do ((c (read-char out) (read-char out nil 'the-end)))
               ((not (characterp c)))
             (setf rs (concatenate 'string rs (string c))))))
    (when extproc (sb-ext:process-close extproc))
    ;; latin-1 = :ISO8859-1 = :cp1252 (http://ru.wikipedia.org/wiki/ISO_8859-1)
    (format nil "~a" (recode (base64:base64-string-to-string rs) :ISO8859-1 :cp1251))))

(defun get-sape-context (uri content)
  (let* ((rs "")
         (input-stream (make-string-input-stream content)) ;; no recode - utf-8
         (extproc (sb-ext:run-program "/usr/bin/php" '("-q" "context.php")
                                      :environment (append (sb-ext:posix-environ)
                                                           (list (format nil "REQUEST_URI=~a" uri))
                                                           (list (format nil "COOKIE=~a" (base64-cookies))))
                                      :wait nil
                                      :input input-stream
                                      :output :stream)))
      (unwind-protect
           (with-open-stream (out (sb-ext:process-output extproc))
             (do ((c (read-char out) (read-char out nil 'the-end)))
                 ((not (characterp c)))
               (setf rs (concatenate 'string rs (string c))))))
      (when extproc (sb-ext:process-close extproc))
      ;; recode from latin-1 to utf
      (format nil "~a" (recode (base64:base64-string-to-string rs) :ISO8859-1 :utf-8))))

(restas:define-route main ("/")
  (hunchentoot:set-cookie "test-cookie" :value "test-value")
  (hunchentoot:set-cookie "test-cookie2" :value "test-value2")
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
  (path (format nil "content/articles/~A.org" article)))

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

;; (setf restas:*default-host-redirect* *host*)

;; (restas:make-context (restas.directory-publisher:*baseurl* '("tmp"))
;;                      (restas.directory-publisher:*directory* #P"/tmp/")
;;                      (restas.directory-publisher:*autoindex* t)))
;; http://restas.lisper.ru/ru/manual/modules.html#%D0%B4%D1%83%D0%B0%D0%BB%D0%B8%D0%B7%D0%BC
