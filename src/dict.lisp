;;;; -*- Mode: lisp; indent-tabs-mode: nil -*-

;;; Copyright (C) 2017, Dmitry Ignatiev <lovesan.ru at gmail.com>

;;; Permission is hereby granted, free of charge, to any person
;;; obtaining a copy of this software and associated documentation
;;; files (the "Software"), to deal in the Software without
;;; restriction, including without limitation the rights to use, copy,
;;; modify, merge, publish, distribute, sublicense, and/or sell copies
;;; of the Software, and to permit persons to whom the Software is
;;; furnished to do so, subject to the following conditions:

;;; The above copyright notice and this permission notice shall be
;;; included in all copies or substantial portions of the Software.

;;; THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND,
;;; EXPRESS OR IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF
;;; MERCHANTABILITY, FITNESS FOR A PARTICULAR PURPOSE AND
;;; NONINFRINGEMENT.  IN NO EVENT SHALL THE AUTHORS OR COPYRIGHT
;;; HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER LIABILITY,
;;; WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM,
;;; OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER
;;; DEALINGS IN THE SOFTWARE.

(in-package #:clave)

(defcstruct av-dict-entry
  (key (:pointer (:string :encoding :utf-8)))
  (value (:pointer (:string :encoding :utf-8))))

(defcfun (av-dict-set "av_dict_set" :library libavformat)
    :int
  (dict :pointer)
  (key :pointer)
  (val :pointer)
  (flags :int))

(defconstant +av-dict-ignore-suffix+ 2)

(defcfun (av-dict-get "av_dict_get" :library libavformat)
    :pointer
  (dict :pointer)
  (key :pointer)
  (prev :pointer)
  (flags :int))

(defcfun (av-dict-free "av_dict_free" :library libavformat)
    :void
  (pp :pointer))

(declaim (inline to-dict))
(defun to-dict (pp alist)
  (declare (type foreign-pointer pp)
           (type list alist))
  (setf (mem-ref pp :pointer) (null-pointer))
  (loop :for (k . v) :in alist
        :do (with-foreign-pointer (pk 512)
              (with-foreign-pointer (pv 512)
                (lisp-string-to-foreign (to-string k) pk 512 :encoding :utf-8)
                (lisp-string-to-foreign (to-string v) pv 512 :encoding :utf-8)
                (av-dict-set pp pk pv 0))))
  (values))

(declaim (inline from-dict))
(defun from-dict (pp &optional free)
  (declare (type foreign-pointer pp))
  (with-foreign-pointer (pkey 1)
    (setf (mem-ref pkey :char) 0)
    (loop :with p :of-type foreign-pointer = (mem-ref pp :pointer)
          :for e :of-type foreign-pointer
            = (av-dict-get p pkey (null-pointer) +av-dict-ignore-suffix+)
              :then (av-dict-get p pkey e +av-dict-ignore-suffix+)
          :until (null-pointer-p e)
          :collect (cons (foreign-slot-value e '(:struct av-dict-entry) 'key)
                         (foreign-slot-value e '(:struct av-dict-entry) 'value))
          :finally (when free (av-dict-free pp)))))

(defmacro with-dict ((var alist) &body body)
  `(with-foreign-object (,var :pointer)
     (to-dict ,var ,alist)
     (unwind-protect (locally ,@body)
       (av-dict-free ,var))))

;; vim: ft=lisp et
