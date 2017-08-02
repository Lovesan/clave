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

(defbitfield format-flags
  (:no-file #x0001)
  (:need-number #x0002)
  (:show-ids #x0008)
  (:raw-picture #x0020)
  (:global-header #x0040)
  (:no-timestamps #x0080)
  (:generic-index #x0100)
  (:ts-discount #x0200)
  (:variable-fps #x0400)
  (:no-dimensions #x0800)
  (:no-streams #x1000)
  (:no-bin-search #x2000)
  (:no-gen-search #x4000)
  (:no-byte-seek #x8000)
  (:allow-flush #x10000)
  (:ts-nostrict #x20000)
  (:ts-negative #x40000)
  (:seek-to-pts #x4000000))

;; vim: ft=lisp et
