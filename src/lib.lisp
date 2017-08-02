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

(define-foreign-library libavcodec
  (:win32 "avcodec-57.dll")
  (t "libavcodec.so.57"))

(use-foreign-library libavcodec)

(define-foreign-library libavformat
  (:win32 "avformat-57.dll")
  (t "libavformat.so.57"))

(use-foreign-library libavformat)

(define-foreign-library libavfilter
  (:win32 "avfilter-6.dll")
  (t "libavfilter.so.6"))

(use-foreign-library libavfilter)

(define-foreign-library libavdevice
  (:win32 "avdevice-57.dll")
  (t "libavdevice.so.57"))

(use-foreign-library libavdevice)

(define-foreign-library libavutil
  (:win32 "avutil-55.dll")
  (t "libavutil.so.55"))

(use-foreign-library libavutil)

(define-foreign-library libswresample
  (:win32 "swresample-2.dll")
  (t "libswresample.so.2"))

(use-foreign-library libswresample)

(define-foreign-library libswscale
  (:win32 "swscale-4.dll")
  (t "libswscale.so.4"))

(use-foreign-library libswscale)

;; vim: ft=lisp et
