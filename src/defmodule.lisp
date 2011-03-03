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

(restas:define-module #:rigidus
  (:use #:cl #:iter))

(in-package #:rigidus)

(defparameter *basedir*
  (make-pathname :directory
                 (pathname-directory (asdf:component-pathname (asdf:find-system '#:rigidus.ru)))))

(defun path (relative)
  (merge-pathnames relative *basedir*))

(closure-template:compile-template :common-lisp-backend (path "src/templates.soy"))
