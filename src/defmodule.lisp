;;;; defmodule.ru
;;;;
;;;; This file is part of the rigidus.ru web site, released under GNU Affero General Public License, Version 3.0
;;;; See file COPYING for details.
;;;;
;;;; Author: Moskvitin Andrey <archimag@gmail.com>

(require 'restas)
(require 'closure-template)
(require 'restas-directory-publisher)
(require 'restas-wiki)
(require 'cl-base64)

(restas:define-module #:rigidus
  (:use #:cl #:iter))

(in-package #:rigidus)

;; (push #p"/root/rigidus.ru/" asdf:*central-registry*)


(defparameter *basedir*
  (make-pathname :directory "media/CB64-E73A/rigidus.ru"))
  ;;                (pathname-directory (asdf:component-pathname (asdf:find-system '#:rigidus.ru)))))

(defun path (relative)
  (merge-pathnames relative *basedir*))

(closure-template:compile-template :common-lisp-backend (path "src/templates.soy"))

(restas:start '#:rigidus :port 8080)
;; (restas:start '#:rigidus :port 4243)
(restas:debug-mode-on)
(restas:debug-mode-off)
;; (setf hunchentoot:*catch-errors-p* t)
