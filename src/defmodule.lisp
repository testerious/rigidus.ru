;;;; defmodule.ru
;;;;
;;;; This file is part of the rigidus.ru web site, released under GNU Affero General Public License, Version 3.0
;;;; See file COPYING for details.
;;;;
;;;; Authors:
;;;;   Moskvitin Andrey <archimag@gmail.com>
;;;;   Glukhov Michail <i.am.rigidus@gmail.com>

(require 'restas)
(require 'closure-template)
(require 'restas-directory-publisher)
(require 'restas-wiki)
(require 'cl-base64)

(restas:define-module #:rigidus
  (:use #:cl #:iter))

(in-package #:rigidus)

(let ((path '(:RELATIVE "rigidus.ru")))
  (setf asdf:*central-registry*
        (remove-duplicates (append asdf:*central-registry*
                                   (list (merge-pathnames
                                          (make-pathname :directory path)
                                          (user-homedir-pathname))))
                           :test #'equal)))

(defparameter *basedir*
  (asdf:component-pathname (asdf:find-system '#:rigidus.ru)))

(defun path (relative)
  (merge-pathnames relative *basedir*))

(closure-template:compile-template :common-lisp-backend (path "src/templates.soy"))

(restas:start '#:rigidus :port 8000)
(restas:debug-mode-on)
(restas:debug-mode-off)
;; (setf hunchentoot:*catch-errors-p* t)

;; (restas:start '#:rigidus :hostname "rigidus.ru" :port 8081)
;; (restas:start '#:rigidus :hostname "www.rigidus.ru" :port 8081)
;; (restas:start '#:eshop :hostname "bzzr.ru" :port 8081)
;; (restas:start '#:eshop :hostname "www.bzzr.ru" :port 8081)

;; (restas:start '#:svdba :hostname "svdba.ru" :port 8081)
;; (restas:start '#:svdba :hostname "www.svdba.ru" :port 8081)
