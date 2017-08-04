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

(defcenum log-level
  (:quiet -8)
  (:panic 0)
  (:fatal 8)
  (:error 16)
  (:warning 24)
  (:info 32)
  (:verbose 40)
  (:debug 48)
  (:trace 56))

(defcfun (av-log "av_log" :library libavutil)
    :void
  (ctx :pointer)
  (level log-level)
  (fmt :pointer)
  (arg1 :pointer))

(defcfun (av-log-set-level "av_log_set_level" :library libavutil)
    :void
  (level log-level))

(defcfun (av-log-set-callback "av_log_set_callback" :library libavutil)
    :void
  (callback :pointer))

(defcfun (vsnprintf "vsnprintf")
    :int
  (buf :pointer)
  (size size-t)
  (format :pointer)
  (args :pointer))

(defvar *log-callback* nil)

(defcallback log-callback :void ((ctx :pointer)
                                 (level log-level)
                                 (fmt :pointer)
                                 (arg1 :pointer))
  (declare (ignore ctx))
  (let ((callback *log-callback*))
    (with-foreign-pointer (buf 1024)
      (vsnprintf buf 1024 fmt arg1)
      (when callback (funcall callback level
                              (foreign-string-to-lisp buf :encoding :iso-8859-1))))))

(defun set-log-callback (callback)
  (setf *log-callback* callback)
  (av-log-set-callback (callback log-callback))
  t)

(defun set-log-level (level)
  (declare (type keyword level))
  (av-log-set-level level))

(defun restore-log-callback ()
  (setf *log-callback* nil)
  (av-log-set-callback (foreign-symbol-pointer "av_log_default_callback" :library 'libavutil)))

;; vim: ft=lisp et
