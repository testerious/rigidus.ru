;;;; render.lisp
;;;;
;;;; This file is part of the rigidus.ru web site, released under GNU Affero General Public License, Version 3.0
;;;; See file COPYING for details.
;;;;
;;;; Author: Moskvitin Andrey <archimag@gmail.com>

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
                     :content content))))))

(defclass rigidus-wiki-render (restas.wiki:drawer) ())
  
(defmethod restas.wiki:finalize-page ((drawer rigidus-wiki-render) content)
  (tpl:root (list :headtitle (getf content :title)
                  :content (tpl:base (list :navpoints (menu)
                                           :content (concatenate 'string
                                                                 (restas.wiki.view:show-page-menu (getf content :menu-links))
                                                                 (getf content :content))
                                           :stat (tpl:stat))))))

(defmethod restas.wiki:generate-content-from-markup ((drawer rigidus-wiki-render) data)
  (orgdata-content (parse-org data)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; org support
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defgeneric parse-org (src)
  (:documentation "Transform org markup into HTML"))

(defmethod parse-org ((file pathname))
  (parse-org (alexandria:read-file-into-string file)))

(defmethod parse-org ((org-content string))
  ;; Разбиваем входный текст по строкам
  (let ((strings (split-sequence:split-sequence #\NewLine org-content))
        (sections)    ;; Информация о заголовках секций
        (mode nil)    ;; Режимы в котором мы находимся
        (directives)  ;; Директивы, например @category
        (br 0)        ;; Счетчик переводов строки для вывода обычного текста
        (save)        ;; Внутренняя переменная для сохранения и последующего вывода в файл
        (result (make-instance 'orgdata)))
    ;; Возвратим html
    (setf (orgdata-content result)
          (format nil "~{~A~%~}"
                  (remove-if #'(lambda (line)
                                 (search "ℕ" line))
                             (loop :for line :in strings
                                :collect
                                (progn
                                  ;; Удаляем директиву -*-
                                  (when (search "-*-" line)
                                    (setf line "ℕ"))
                                  ;; @title
                                  (when (equal 0 (search "@title" line))
                                    (setf (getf directives :title)
                                          (string-trim '(#\  #\tab #\Newline) (subseq line 7)))
                                    (setf line "ℕ"))
                                  ;; @category
                                  (when (equal 0 (search "@category" line))
                                    (setf (getf directives :category)
                                          (string-trim '(#\  #\tab #\Newline) (subseq line 10)))
                                    (setf line "ℕ"))
                                  ;; @sort
                                  (when (equal 0 (search "@sort" line))
                                    (setf (getf directives :sort)
                                          (string-trim '(#\  #\tab #\Newline) (subseq line 6)))
                                    (setf line "ℕ"))
                                  ;; *
                                  (when (ppcre:scan "\\A\\*+\\s+" line)
                                    (setf line
                                          (let ((cnt 1)) ;; Подcчитаем количество звездочек
                                            (loop :for item :across line :do
                                               (if (char= #\* item)
                                                   (incf cnt)
                                                   (return)))
                                            (let ((headline (subseq line cnt)))
                                              (push (list cnt headline) sections)
                                              (format nil "<h~a><a name=\"anchor-~a\">~a</a></h~a>"
                                                      cnt (length sections) headline cnt)))))
                                  ;; @/code
                                  (when (or (equal 0 (search "@/code" line))
                                            (equal 0 (search "</source" line)))
                                    (setf (getf mode :code) nil)
                                    (setf line "</pre>"))
                                  ;; @store
                                  (when (equal 0 (search "@store" line))
                                    (with-open-file (fstream (path (subseq line 7)) :direction :output :if-exists :supersede)
                                      (format fstream "~{~a~%~}"
                                              (loop :for i :in (reverse save)
                                                 :unless (search "ℕ" i)
                                                 :collect i)))
                                    ;; (setf line "ℕ")
                                    )
                                  ;; @append
                                  (when (equal 0 (search "@append" line))
                                    (with-open-file (fstream (path (subseq line 8)) :direction :output :if-exists :append)
                                      (format fstream "~{~a~%~}"
                                              (loop :for i :in (reverse save)
                                                 :unless (search "ℕ" i)
                                                 :collect i)))
                                    ;; (setf line "ℕ")
                                    )
                                  ;; mode:code
                                  (when (getf mode :code)
                                    (push line save)
                                    (setf line (format nil "~a" (ppcre:regex-replace-all "<" line "&lt;"))))
                                  ;; Проверка на малый разделитель
                                  (when (ppcre:scan "\\A\\-{3,}" line)
                                    (setf line "<div class=\"divider\">.</div>"))
                                  ;; Проверка на большой разделитель
                                  (when (ppcre:scan "\\A\\={3,}(.*)" line)
                                    (setf line
                                          (cl-ppcre:regex-replace "\\A\\={3,}(.*)" line
                                                                  (list #'(lambda (match reg)
                                                                            (declare (ignore match))
                                                                            (format nil "~a<div ~a ~a>~a~a"
                                                                                    "<div class=\"divider\">.</div>"
                                                                                    "class=\"guideNum\""
                                                                                    "id=\"config-options\""
                                                                                    reg
                                                                                    "<a href=\"#top\">top</a></div>")))
                                                                  :simple-calls t)))
                                  ;; default
                                  (setf line
                                        (if (not (string= "" line))
                                            line
                                            (progn
                                              (incf br)
                                              (if (> br 1)
                                                  (progn
                                                    (setf br 0)
                                                    "<p>")
                                                  "</p>"))))
                                  ;; @code
                                  (if (or (equal 0 (search "@code" line))
                                          (equal 0 (search "<source" line)))
                                      (prog1
                                          "<pre>"
                                        (setf save nil)
                                        (setf (getf mode :code) t))
                                      ;; else
                                      ;; (format nil "~a~a" (length save) line)
                                      line
                                      ))))))
     ;; заголовки секций (в обратном порядке)
    (setf (orgdata-sections result)
          (reverse sections))
     ;; директивы
    (setf (orgdata-directives result)
          directives)
    result))

(defun find-articles-by-category (param)
  "Возвращает все статьи, у которых @category соотвествует параметру"
  (sort (iter (for filepath in (directory (path "content/articles/*.org")))
              (let ((directives (orgdata-directives (parse-org filepath))))
                (when (string= param (getf directives :category))
                  (collect (list :title (getf directives :title)
                                 :link  (concatenate 'string "/articles/" (pathname-name filepath))
                                 :sort  (getf directives :sort))))))
        #'string<
        :key #'(lambda (x) (getf x :title))))

