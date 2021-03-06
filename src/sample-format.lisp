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

(defcenum sample-format
  :unsigned8
  :signed16
  :signed32
  :float
  :double
  :unsigned8-planar
  :signed16-planar
  :signed32-planar
  :float-planar
  :double-planar
  :signed64
  :signed64-planar)

(define-foreign-type sample-format-list ()
  ()
  (:actual-type :pointer)
  (:simple-parser sample-format-list))

(defmethod translate-from-foreign (ptr (type sample-format-list))
  (if (null-pointer-p ptr)
    '()
    (loop :with p :of-type foreign-pointer = ptr
          :for x = (mem-ref p :int)
          :until (= -1 x)
          :collect (foreign-enum-keyword 'sample-format x)
          :do (incf-pointer p (foreign-type-size 'sample-format)))))

(defmethod expand-from-foreign (ptr (type sample-format-list))
  `(if (null-pointer-p ,ptr)
     '()
     (loop :with p :of-type foreign-pointer = ,ptr
           :for x = (mem-ref p :int)
           :until (= -1 x)
           :collect (foreign-enum-keyword 'sample-format x)
           :do (incf-pointer p (foreign-type-size 'sample-format)))))

;; vim: ft=lisp et
