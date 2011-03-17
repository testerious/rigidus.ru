;;;; rigidus.ru.asd
;;;;
;;;; This file is part of the rigidus.ru web site, released under GNU Affero General Public License, Version 3.0
;;;; See file COPYING for details.
;;;;
;;;; Author: Moskvitin Andrey <archimag@gmail.com>

(defsystem rigidus.ru
  :depends-on (#:restas-directory-publisher #:restas-wiki #:closure-template)
  :components ((:module "src"
                        :components ((:file "defmodule")
                                     (:file "render" :depends-on ("defmodule"))
                                     (:file "routes" :depends-on ("render"))))))
