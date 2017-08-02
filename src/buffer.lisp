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

(defcstruct av-buffer-ref
  (buffer :pointer)
  (data :pointer)
  (size :int))

(defcfun (av-buffer-alloc "av_buffer_alloc" :library libavcodec)
    :pointer
  (size :int))

(defcfun (av-buffer-create "av_buffer_create" :library libavcodec)
    :pointer
  (data :pointer)
  (size :int)
  (free-callback :pointer)
  (opaque :pointer)
  (flags :int))

(defcfun (av-buffer-ref "av_buffer_ref" :library libavcodec)
    :pointer
  (buf :pointer))

(defcfun (av-buffer-unref "av_buffer_unref" :library libavcodec)
    :void
  (pp :pointer))

;; vim: ft=lisp et
