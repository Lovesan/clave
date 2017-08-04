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

(eval-when (:compile-toplevel :load-toplevel :execute)
  (if (= 8 (foreign-type-size :pointer))
    (pushnew '64-bit *features*)
    (pushnew '32-bit *features*)))

#+clave::64-bit
(defctype size-t :uint64)
#+clave::64-bit
(defctype size-t :uint32)

(declaim (inline mklist))
(defun mklist (x)
  (if (listp x) x (list x)))

(define-modify-macro mklistf () mklist)

(defmacro with-gensyms ((&rest vars) &body body)
  (let ((bindings (mapcar (lambda (x)
                            (if (listp x)
                              `(,(first x) ',(gensym (string (second x))))
                              `(,x ',(gensym (string x)))))
                          vars)))
    `(let ,bindings ,@body)))

(defun to-string (x)
  (typecase x
    (base-string (coerce x '(vector character)))
    ((or (vector character)) x)
    ((or symbol character base-char) (coerce (string x) '(vector character)))
    (T (princ-to-string x))))

(declaim (inline ws-char-p))
(defun ws-char-p (x)
  (case x ((#\space #\tab #\return #\newline) t)))

(defun split-string (str char &optional (skip-empty t))
  (declare (type (vector character) str)
           (type character char))
  (prog* ((str (to-string str))
          (i 0)
          (start 0)
          (has-chars nil)
          (len (length str))
          (list '())
          (c #\space))
   :start
     (unless (> len i)
       (when (or has-chars (not skip-empty))
         (push (subseq str start i) list))
       (return (nreverse list)))
     (setf c (elt str i))
     (cond
       ((char= char c)
        (when (or has-chars (not skip-empty))
          (push (subseq str start i) list))
        (setf start (1+ i)
              has-chars nil))
       ((not (ws-char-p c)) (setf has-chars t)))
     (incf i)
     (go :start)))

(declaim (inline mkrational))
(defun mkrational (rational)
  (declare (type rational rational))
  (logior (logand #xFFFFFFFF (numerator rational))
          (ash (logand #xFFFFFFFF (denominator rational)) 32)))

(defconstant +no-pts+ (- #x8000000000000000))

(deftype uint8 () '(unsigned-byte 8))

(deftype int32 () '(signed-byte 32))

(deftype int () '(signed-byte 32))

(deftype int64 () '(signed-byte 64))

(deftype index () '(integer 0 #.most-positive-fixnum))

(defmacro let-when ((var value &optional (condition var)) &body body)
  `(let ((,var ,value))
     (when ,condition (locally ,@body))))

(defcfun (av-mallocz "av_mallocz" :library libavutil)
    :pointer
  (size size-t))

(defcfun (av-free "av_free" :library libavutil)
    :void
  (p :pointer))

(defcfun (memcpy "memcpy")
    :void
  (dest :pointer)
  (src :pointer)
  (size size-t))

;; vim: ft=lisp et
