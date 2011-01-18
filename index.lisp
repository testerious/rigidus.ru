#|
@description: http://rigidus.ru source code
@autor: Глухов Михаил /Glikhov Michail/, aka Rigidus ⓒ 2011
@contact: i.am.rigidus@gmail.com
@license: GPLv3

Внимание! Этот код использует расширенный синтаксис определенный
с помощью set-macro-character в .sbclrc. Более подробную информацию
можно запросить по email. Она будет опубликована в ближайшее время.
|#

►
* Введение

 Это экспериментальный проект, сделанный как расширяемый и понятный
 прототип сайта и предназначенный для обучения программистов, желающих
 освоить веб-программирование на Common Lisp.

* Подключаемые библиотеки

 Я использую ряд библиотек, о которых хочу вкратце рассказать.

 • RESTAS - библиотека для диспетчеризации (роутинга), запросов в
 стиле REST. Подробная документация на русском и английском языке
 доступна на http://restas.lisper.ru/

 • CLOSURE-TEMPLATE - движок шаблонов с компиляцией в машинный
 код. Техническая документация (пригодится при написании шаблонов) на
 английском тут:
 http://code.google.com/closure/templates/docs/overview.html
 Документация на русском с подробными примерами и обяснениями — в
 блоге автора по соответствующему тегу:
 http://archimag-dev.blogspot.com/search/label/closuretemplates

 • SPLIT-SEQUENCE и CL-PPCRE — библиотеки для работы со строками

 • ANAPHORA — библиотека макросов, расширающая стандартные управляющие
 конструкции лиспа, имеет смысл гуглить более подробно — она широко
 используется.

 • HUNCHENTOOT — веб-сервер. Подробная документация доступна на сайте
 разработчика: http://www.weitz.de/hunchentoot/

* Инициализация

 В первую очередь, стоит подключить необходимые библиотеки и
 определить пакет, в котором мы в дальнейшем будем работать —
 RIGIDUS. Также для вычисления правильных путей наш сайт будет
 смотреть в каталог из которого запущен — для этого я объявляю макрос
 BASE-PATH, который раскрывается в код, возвращающий
 PATHNAME. Вероятно, здесь можно было бы обойтись и функцией, но я
 рассчитываю на возможные неожиданные изменения. В этом макросе на
 всякий случай фильтруются двоеточия, чтобы злонамеренные пользователи
 не имели возможности выхода за пределы каталога сайта.

◄
(asdf:operate 'asdf:load-op '#:restas)
(asdf:operate 'asdf:load-op '#:closure-template)
(asdf:operate 'asdf:load-op '#:split-sequence)
(asdf:operate 'asdf:load-op '#:cl-ppcre)
(asdf:operate 'asdf:load-op '#:anaphora)
(asdf:operate 'asdf:load-op '#:hunchentoot)

(restas:debug-mode-on)

(restas:define-module #:rigidus
    (:use :cl
          :closure-template
          :anaphora
          :split-sequence
          :cl-ppcre
          :alexandria
          ))

(in-package #:rigidus)

(defparameter *base-dir* (format nil "~arigidus.ru" (user-homedir-pathname)))
(defparameter *main-title* "Программирование - как искусство")

(defmacro base-path (fname)
  `(pathname
    (regex-replace-all "\\.\\."
                       (format nil "~a/~a" *base-dir* ,fname)
                       ".")))

►

* Основные шаблоны
** Корневой шаблон ROOT

 Я использую корневой шаблон, который назвал ROOT, чтобы включать в
 него все остальные шаблоны. Вся остальная разметка передается этому
 шаблону через переменную $CONTENT, а заголовок страницы — через
 переменную $HEADTITLE.

▼
{namespace tpl}
{template root}
<!DOCTYPE html PUBLIC "-//W3C//DTD XHTML 1.0 Strict//EN" "http://www.w3.org/TR/xhtml1/DTD/xhtml1-strict.dtd">{\n}
<html xmlns="http://www.w3.org/1999/xhtml" xml:lang="en" lang="en">{\n}
  <head>{\n}
	<title>{$headtitle}</title>{\n}
	<meta http-equiv="Content-Type" content="text/html; charset=utf-8" />{\n}
	<link rel="stylesheet" type="text/css" media="screen" href="/style.css" />{\n}
	<link rel="Shortcut Icon" type="image/x-icon" href="/favicon.ico" />{\n}
  </head>{\n}
  <body id="top">{\n}
    {$content | noAutoescape}{\n}
  </body>{\n}
</html>{\n}
{/template}
▲
Գ

** Меню — NAVELT, SECTELT и функция (MENU)

 В этом сайте на текущий момент два вида меню — основное меню сайта,
 отбражаемое на каждой странице и меню секций статьи, которое отражает
 структуру статьи сайта. Для отображения элемента первого меню
 используется шаблон NAVELT:

▼
{namespace tpl}
{template navelt}
<li><a href="{$link}">{$title}</a></li>
{/template}
▲
Գ

 А для отображения меню секций — шаблон SECTELT:

▼
{namespace tpl}
{template sectelt}
<li class="{$level}">★<a href="#{$anchor}">{$title}</a>
{/template}
▲
Գ

 Простая функция заглушка пока не занимается каким-либо построением
 меню по данным, а просто отдает необходимый список пунктов — в
 дальнейшем ее можно расширять:

◄
(defun menu ()
  (list (list :link "/" :title "Главная")
        (list :link "/about" :title "About")
        (list :link "/articles" :title "Статьи")
        (list :link "/resourses" :title "Ресурсы")
        (list :link "/contacts" :title "Контакты")))
►

** Счетчики статистики

 Здесь мы определим шаблон со счетчиками статистики сайта

▼
{namespace tpl}
{template stat}
<div style="margin-top: -29px; margin-left: 150px;">{\n}
  <!--Google Analitics -->{\n}
  <script type="text/javascript">{\n}
    var _gaq = _gaq || [];{\n}
    _gaq.push(['_setAccount', 'UA-20801780-1']);{\n}
    _gaq.push(['_trackPageview']);{\n}
    (function() {{\n}
    var ga = document.createElement('script'); ga.type = 'text/javascript'; ga.async = true;{\n}
    ga.src = ('https:' == document.location.protocol ? 'https://ssl' : 'http://www') + '.google-analytics.com/ga.js';{\n}
    var s = document.getElementsByTagName('script')[0]; s.parentNode.insertBefore(ga, s);{\n}
    })();{\n}
  </script>{\n}
  <!--Google Analitics -->

  <!--LiveInternet counter-->{\n}
  <script type="text/javascript">{\n}
    <!--{\n}
       document.write("<a href='http://www.liveinternet.ru/click' "+{\n}
       "target=_blank><img src='//counter.yadro.ru/hit?t24.5;r"+{\n}
       escape(document.referrer)+((typeof(screen)=="undefined")?"":{\n}
       ";s"+screen.width+"*"+screen.height+"*"+(screen.colorDepth?{\n}
       screen.colorDepth:screen.pixelDepth))+";u"+escape(document.URL)+{\n}
       ";h"+escape(document.title.substring(0,80))+";"+Math.random()+{\n}
       "' alt='' title='LiveInternet: показано число посетителей за"+{\n}
       " сегодня' "+{\n}
       "border='0' width='88' height='15'><\/a>"){\n}
       //-->{\n}
  </script>{\n}
  <!--/LiveInternet-->{\n}
  {\n}&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;
  <!-- Yandex.Metrika informer -->{\n}
  <a href="http://metrika.yandex.ru/stat/?id=3701317&amp;from=informer"{\n}
     target="_blank"><img src="//bs.yandex.ru/informer/3701317/1_0_9F9F9FFF_7F7F7FFF_0_pageviews"{\n}
                          width="80" height="15" alt="Яндекс.Метрика" border="0" /></a>{\n}
  <!-- /Yandex.Metrika informer -->{\n}
  {\n}
  <!-- Yandex.Metrika counter -->{\n}
  <div style="display:none;"><script type="text/javascript">{\n}
      (function(w, c) {{\n}
      (w[c] = w[c] || []).push(function() {{\n}
      try {{\n}
      w.yaCounter3701317 = new Ya.Metrika(3701317);{\n}
      yaCounter3701317.clickmap(true);{\n}
      yaCounter3701317.trackLinks(true);{\n}
      {\n}
      } catch(e) {}{\n}
      });{\n}
      })(window, 'yandex_metrika_callbacks');{\n}
  </script></div>{\n}
  <script src="//mc.yandex.ru/metrika/watch.js" type="text/javascript" defer="defer"></script>{\n}
  <noscript><div style="position:absolute"><img src="//mc.yandex.ru/watch/3701317" alt="" /></div></noscript>{\n}
  <!-- /Yandex.Metrika counter -->{\n}
  {\n}
</div>{\n}
{/template}
▲
Գ

** Базовая сетка сайте — BASE

 Для этого сайта я выбрал вот такую простую разметку, которую назвал
 BASE. Эта разметка вставляется в переменную $CONTENT шаблона ROOT, а
 сама в свою очередь принимает в качестве параметров CONTENT — для
 вставки собственно содержимого страницы, NAVPOINTS — для вставки
 элементов меню и STAT - для вставки всякого рода счетчиков.

▼
{namespace tpl}
{template base}
<div id="center">
  <div class="col1 left">
    <a id="logo" href="/">
      <img src="http://www.gravatar.com/avatar/d8a986606b9d5e4769ba062779e95d9f?s=45"
           style="border: 1px solid #7F7F7F"/>
    </a>
    <ul id="nav">
      {foreach $elt in $navpoints}
      {call navelt data="$elt" /}
      {/foreach}
    </ul>
  </div>
  {$content |noAutoescape}
  <div class="clear">.</div>
</div>
<div id="footer">
  <p>
    <a href="/about">About</a> |
    <a href="/contacts">Contacts</a>
    {$stat |noAutoescape}
  </p>
</div>
{/template}
▲
Գ

* Отображение страниц

 Для отображения страниц на нижнем уровне используется функция PAGE,
 которая определяется так:

◄
(defun page (headtitle navpoints content)
  (tpl:root (list :headtitle headtitle
                  :content (tpl:base (list :navpoints navpoints
                                           :content content
                                           :stat (tpl:stat))))))
►

 В дальнейшем мы определим более высокоуровневые функции для разных
 типов страниц.

* Маршрутизация

** Статика

 Для того, чтобы отдавать статические файлы я сделал простой маршрут
 STATIC, который ищет файлы в текущем каталоге:

◄
(restas:define-route static
    ("/:staticfile"
     :requirement (lambda ()
                    (let ((request-file
                           (base-path (hunchentoot:request-uri hunchentoot:*request*)))
                          (files (directory (format nil "~a/*.*" *base-dir*))))
                      (not (null (find request-file files :test #'equal))))))
  (base-path staticfile))
►

** Главная страница

 Так как пока мне проще отредактировать html главной страницы, чем
 придумывать что-то особенное, я просто сделал шаблон MAIN, который
 выводится на главную страницу:

▼
{namespace tpl}
{template main}
<div class="col3 right">
  <h1 style="margin: 0">{$title}</h1>
  <div class="col4 left">
    <p class="alert" style="width: 250px; color: red; font-size: 18px;"><strong>Что здесь происходит?</strong> </p>
    <ul style="color: #feffb8">
      <li><span style="color: red">★</span> Хомяк фаломорфировал,
      почернел и был переписан на Common Lisp. В процессе был освоен
      RESTAS. URL-ы вроде-бы остались прежними</li>
      <li><span style="color: red">★</span> <a href="http://habrahabr.ru/blogs/webdev/111365/">Первый пост</a> на хабре</li>
      <li><span style="color: red">★</span> Освоил Org-mode. Головокружение от успехов...</li>
      <li><span style="color: red">★</span> Новый год случился...</li>
      <li><span style="color: green">★</span> Выложил <a href="https://github.com/rigidus/cl-eshop">исходный код
      рабочего проекта</a> на github. OpenSource грядет!</li>
      <li><span style="color: green">★</span> Ура, зарплата!</li>
      <li><span style="color: green">★</span> Вторая версия интернет-магазина окончательно готова!</li>
      <li><span style="color: green">★</span> Вторая версия интернет-магазина полностью готова!</li>
      <li><span style="color: green">★</span> Вторая версия интернет-магазина готова!</li>
      <li><span style="color: green">★</span> Практически доделал
        новую версию старой опердени...  Осталось капельку, да почти
        что ничего...</li>
      <li><span style="color: green">★</span> Внедрил
        <a href="http://dropbox.com">Dropbox</a> в продакшн. Теперь
        писать веселее!</li>
    </ul>
  </div>
  <div class="col4 left">
    <p class="alert" style="width: 250px; color: red; font-size: 18px;">
      <strong>Кто все эти люди?</strong>
    </p>
    Пока это всего лишь я, Rigidus, собираю здесь все свои посты,
    сделанный в  livejournal, так как им стало невозможно пользоваться
    из-за ошеломляющего количества спама.

    <p class="alert" style="width: 250px; color: red; font-size: 18px;">
      <strong>Что дальше будет?</strong>
    </p>
    Будут дописываться всяческие модули, сервисы и прочий стафф. Если
    вам чего-то прямо сейчас хочется - напишите прямо вот сюда ⬇
    <form method="post">
      <textarea name="featurerequest" class="input"
                style="width: 360px;
                       height: 70px;
                       margin-top: 5px;
                       margin-bottom: 5px;
                       "></textarea>
      <br />
      <input type="submit"
             style="font: normal 11px 'lucida sans unicode', 'lucida grande', tahoma, arial, trebuchet
	                background: #303030;
	                border: 0;
	                padding: 3px;
	                color: #333333;"
             value="Send!" />
    </form>
  </div>
  <div class="divider">.</div>
</div>
{/template}
▲
Գ

 Чтобы сделать изменяемый h1-заголовок я использую случайную выборку
 цитаты из файла afor.txt в котором цитаты идут друг за другом, каждая
 на новой строке. Разбитый по строкам файл мы кешируем в переменной
 замыкания LINES, чтобы при каждой загрузке страницы не обращаться к
 файловой системе. Вот код маршрута, который этим занимается:

◄
(let ((lines
       (split-sequence:split-sequence
        #\Newline
        (alexandria:read-file-into-string (base-path "afor.txt")))))
  (restas:define-route main ("/")
    (page *main-title*
          (menu)
          (tpl:main (list :title (nth (random (length lines)) lines))))))
►

** Страница по умолчанию

 Это шаблон обычной страницы, встраеваемый в переменную CONTENT
 шаблона BASE. Он предназначен главным образом для вывода статей,
 поэтому содержит в себе меню, передаваемое в параметре $SECTIONS. В
 конце страницы для удобства пользователя повторяется главное меню
 сайта.

▼
{namespace tpl}
{template default}
<div class="col3 right">
  <h1>{$title}</h1>
  <div class="col2 right" id="guideNav">
    <ul>
      {foreach $elt in $sections}
      {call sectelt data="$elt" /}
      {/foreach}
    </ul>
  </div>
  {$content |noAutoescape}

  <div class="divider">.</div>

  <ul id="share">
    {foreach $elt in $navpoints}
    {call navelt data="$elt" /}
    {/foreach}
  </ul>
</div>
{/template}
▲
Գ

 Для того чтобы отображать обычные страницы я написал макрос,
 использующий функцию ORG-TO-HTML, которая будет рассмотрена несколько
 позже. Этот макрос позволяет встраивать дополнительный опциональный
 код между обработкой данных страницы и собственно отображением. Это
 очень удобно, если необходимо вмешаться в процесс отображения.

 Для того чтобы меню дважды не вычислялось, оно запоминается в
 переменной функции MENU-MEMO внутри функции

◄
(defmacro default-page (menu file-path &optional (body nil))
  `(let ((menu-memo ,menu))
     (multiple-value-bind (content sections directives)
         (org-to-html (alexandria:read-file-into-string ,file-path))
       (let ((title (getf directives :title)))
         ,body
         (page title menu-memo
               (tpl:default
                   (list :title title :navpoints menu-memo
                         :sections
                         (loop
                            :for i :from 1
                            :for section :in sections :collect
                            (list :anchor (format nil "anchor-~a" i)
                                  :level (format nil "level-~a" (car section))
                                  :title (cadr section)))
                         :content content)))))))
►

** Обычные страницы первого уровня

 Теперь мы можем определить маршруты обычных страниц, вроде ABOUT. Я
 определяю дупликаты страниц с адресами, заканчивающимися на слеш,
 чтобы облегчить жизнь тем пользователям, которые предпочитают
 перемещаться по сайту с помощью ручного редактирования адресной
 строки броузера.

◄
(restas:define-route about ("/about")
  (default-page (menu) (base-path "about.org")))
(restas:define-route about/ ("/about/")
  (default-page (menu) (base-path "about.org")))

(restas:define-route resourses ("/resourses")
  (default-page (menu)(base-path "resourses.org")))
(restas:define-route resourses/ ("/resourses/")
  (default-page (menu)(base-path "resourses.org")))

(restas:define-route contacts ("/contacts")
  (default-page (menu)(base-path "contacts.org")))
(restas:define-route contacts/ ("/contacts/")
  (default-page (menu)(base-path "contacts.org")))
►

** Отправка писем с главной страницы
 Для обратной связи с пользователями я реализовал отправку писем с
 формы, расположенной на главной странице, что можно увидеть в
 соответствующем шаблоне (TPL:MAIN).

 Для отображения двух вариантов страниц отдаваемых пользователю
 после отправки письма используются шаблоны SEND-OK и SEND-ERR

▼
{namespace tpl}
{template send-ok}
Письмо ушло, я отвечу вам, если вы не забыли оставить свой e-mail
<br/><br/>
<a href="/">Обратно</a>
{/template}

{template send-err}
Какие-то проблемы с почтой, не могу отправить e-mail.
Буду благодарен, если вы продублируете на
<span style="color: red;"> <strong>avenger-f</strong></span>
<span style="color: white;"> <b>טּ</b> </span>
<span style="color: yellow;">yandex.ru</span>.<br /></br />
Или на
<span style="color: red;"> <strong>i.am.rigidus</strong></span>
<span style="color: white;"> <b>טּ</b> </span>
<span style="color: yellow;">gmail.com</span>.<br /><br />
Вот текст сообщения:
<div style="margin: 15px 0 30px 0; padding: 5px; background-color: #303030">
  <pre>
    {$message}
  </pre>
</div>
<a href="/">Вернуться обратно</a>
<br /><br />
{/template}
▲
Գ

 Контроллер, который обрабатывает POST-запрос вот такой:

◄
(restas:define-route main/post ("/" :method :post)
  (let ((menu-memo (menu))
        (message (hunchentoot:post-parameter "featurerequest"))
        (ret-code (send-mail (list "avenger-f@yandex.ru")
                             (hunchentoot:post-parameter "featurerequest"))))
    (page *main-title* menu-memo
          (tpl:default (list :title "Спасибо!"
                             :navpoints menu-memo
                             :content (if ret-code
                                          (tpl:send-ok)
                                          (tpl:send-err (list :message message))))))))
►

 Он использует специальную процедуру SEND-EMAIL чтобы отправить письмо:

◄
(defvar *sendmail*
  (find-if #'fad:file-exists-p
           (list "/usr/bin/sendmail"
                 "/usr/sbin/sendmail")))

(progn
  (defparameter *mailbox* nil)
  (defun send-mail (to clientmail)
    (push clientmail *mailbox*)
    (if (null *sendmail*)
        nil
        (let* ((sendmail-process (sb-ext:run-program *sendmail*
                                                     to
                                                     :input :stream
                                                     :output nil
                                                     :error nil
                                                     :wait nil))
               (sendmail (sb-ext:process-input sendmail-process)))
          (unwind-protect
               (progn
                 (format sendmail "From: no-reply@rigidus.ru~%")
                 (format sendmail "To: ~a~%" (car to))
                 (format sendmail "Subject: ~a~%" "mail-from-site")
                 (format sendmail "MIME-Version: ~a~%" "1.0")
                 (format sendmail "Content-Type: ~a~%" "text/plain")
                 (format sendmail "%")
                 (format sendmail "~a" clientmail))
            (close sendmail)
            (sb-ext:process-wait sendmail-process)
            (sb-ext:process-close sendmail-process))
          t))))
►

* Обработка статей
 Каждая статья представляет собой файл в формате org-mode, содержащий
 метаинформацию и разметку. Этот файл преобразует в html-код функция
 ORG-TO-HTML. Она же извлекает из разметки информацию о разделах
 статьи, по которым потом можно построить меню статьи.

◄
(defun org-to-html (org-content)
  ;; Разбиваем входный текст по строкам
  (let ((strings (split-sequence:split-sequence #\NewLine org-content))
        (sections)    ;; Информация о заголовках секций
        (mode nil)    ;; Режимы в котором мы находимся
        (directives)  ;; Директивы, например @category
        (br 0)        ;; Счетчик переводов строки для вывода обычного текста
        (save)        ;; Внутренняя переменная для сохранения и последующего
        )             ;; вывода в файл
    (values
     ;; Возвратим html
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
                  (when (scan "\\A\\*+\\s+" line)
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
                    (with-open-file (fstream (base-path (subseq line 7)) :direction :output :if-exists :supersede)
                      (format fstream "~{~a~%~}"
                              (loop :for i :in (reverse save)
                                 :unless (search "ℕ" i)
                                 :collect i)))
                    ;; (setf line "ℕ")
                    )
                  ;; @append
                  (when (equal 0 (search "@append" line))
                    (with-open-file (fstream (base-path (subseq line 8)) :direction :output :if-exists :append)
                      (format fstream "~{~a~%~}"
                              (loop :for i :in (reverse save)
                                 :unless (search "ℕ" i)
                                 :collect i)))
                    ;; (setf line "ℕ")
                    )
                  ;; mode:code
                  (when (getf mode :code)
                    (push line save)
                    (setf line (format nil "~a" (regex-replace-all "<" line "&lt;"))))
                  ;; Проверка на малый разделитель
                  (when (scan "\\A\\-{3,}" line)
                    (setf line "<div class=\"divider\">.</div>"))
                  ;; Проверка на большой разделитель
                  (when (scan "\\A\\={3,}(.*)" line)
                    (setf line
                          (cl-ppcre:regex-replace "\\A\\={3,}(.*)" line
                                                  (list #'(lambda (match reg)
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
                      )))))
     ;; Возвратим заголовки секций (в обратном порядке)
     (reverse sections)
     ;; Возвратим директивы
     directives)))
►

 А этот код занимается собственно отображением статьи:

◄
(restas:define-route article ("/articles/:article")
  (let ((files (directory (format nil "~a/*.org" *base-dir*)))
        (article-filepath (base-path (concatenate 'string article ".org"))))
    (if (null (find article-filepath files :test #'equal))
        hunchentoot:+HTTP-NOT-FOUND+
        (default-page (menu) article-filepath))))
►

* Оглавление статей
 На этом сайте статьи размещены в файлах в текущем каталоге
 пользователя. Каждый файл может содержать в себе метаинформацию о
 статье, например тег @category. Функция GET-ORG-BY-CATEGORY
 возвращает список, содержащий заголовок, адрес и признак сортировки
 всех статей, у которых тэг @category сооотвествует строке, переданной
 в параметре. Эта функция используется при построении меню статей,
 разбитого на категории на странице "/articles".

◄
(defun get-org-by-category (param)
  "Возвращает все статьи, у которых @category соотвествует параметру"
  (let ((orgs))
    (mapcar #'(lambda (filepath)
                (multiple-value-bind (content sections directives)
                    (org-to-html (alexandria:read-file-into-string filepath))
                  (if (string= param (getf directives :category))
                      (push (list :title (getf directives :title)
                                  :link  (concatenate 'string "/articles/" (pathname-name filepath))
                                  :sort  (getf directives :sort))
                            orgs))))
            (directory (base-path "*.org")))
    orgs))
►

 Так как при большом количестве статей построение меню может занимать
 значительное время — я кеширую результат в переменной замыкания MEMO
 при построении страницы "/articles".

 Эта функция использует шаблон элемента TPL:LI, для отображения
 элементов подуровней разделов статьи:

▼
{namespace tpl}
{template li}
<li>
  <span style="color: {$color}">{$star}</span>
  <a href="{$link}">{$title}</a>
</li>
{/template}
▲
Գ

◄
(let ((memo))
  (restas:define-route articles ("/articles")
    (when (null memo)
      (setf memo
            (default-page (menu) (base-path "articles.org")
              (setf content
                    (regex-replace-all
                     "@make-list-by-category(.*)@" content
                     (list #'(lambda (match reg)
                               (let* ((instr (string-trim '(#\Space #\Tab #\Newline) reg)))
                                 (multiple-value-bind (star color category)
                                     (values-list (split-sequence #\Space instr))
                                   (format nil "<ul>~{~a~}</ul>"
                                           (mapcar #'(lambda (x)
                                                       (tpl:li (append x (list :star star :color color))))
                                                   (sort (get-org-by-category category)
                                                         #'(lambda (a b)
                                                             (string< (getf a :sort) (getf b :sort))))))))))
                     :simple-calls t)))))
    memo))
►


* Запуск сервера

 Теперь, когда у нас есть вся необходимая инфраструктура мы можем
 запусить наш сервер:

◄
(+ 1 2)


(restas:start '#:rigidus :port 8080)
