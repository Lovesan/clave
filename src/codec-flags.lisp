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

(defbitfield (codec-flags :uint)
  (:unaligned #x00000001)
  (:qscale #x00000002)
  (:four-mv #x00000004)
  (:output-corrupt #x00000008)
  (:qpel #x00000010)
  (:pass1 #x00000200)
  (:pass2 #x00000400)
  (:loop-filter #x00000800)
  (:gray #x00002000)
  (:psnr #x00008000)
  (:truncated #x00010000)
  (:interlaced-dct #x00040000)
  (:low-delay #x00080000)
  (:global-header #x00400000)
  (:bit-exact #x00800000)
  (:ac-prec #x01000000)
  (:interlaced-me #x20000000)
  (:closed-gop #x80000000))

;; vim: ft=lisp et
