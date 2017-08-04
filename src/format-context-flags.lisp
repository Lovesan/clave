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

(defbitfield format-context-flags
  (:generate-pts #x0001)
  (:ignore-index #x0002)
  (:non-block #x0004)
  (:ignore-dts #x0008)
  (:no-fillin #x0010)
  (:no-parse #x0020)
  (:no-buffer #x0040)
  (:custom-io #x0080)
  (:discard-corrupt #x0100)
  (:flush-packets #x0200)
  (:bit-exact #x0400)
  (:mp4a-latm #x8000)
  (:sort-dts #x10000)
  (:priv-opt #x20000)
  (:keep-side-data #x40000)
  (:fast-seek #x80000)
  (:shortest #x100000)
  (:auto-bsf #x200000))

;; vim: ft=lisp et
