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

(defcstruct avio-context
  (av-class :pointer)
  ;; other slots are considered private
  )

(defcfun (avio-open2 "avio_open2" :library libavformat)
    :int
  (pp :pointer)
  (url :pointer)
  (flags io-flags)
  (icb :pointer)
  (pdict :pointer))

(defcfun (avio-alloc-context "avio_alloc_context" :library libavformat)
    :pointer
  (buf :pointer)
  (size :int)
  (write :boolean)
  (opaque :pointer)
  (rcb :pointer)
  (wcb :pointer)
  (scb :pointer))

(defcfun (avio-close "avio_close" :library libavformat)
    :void
  (p :pointer))

(defcfun (avio-closep "avio_closep" :library libavformat)
    :void
  (pp :pointer))

(defcstruct avio-interrupt-callback
  (callback :pointer)
  (opaque :pointer))

(defcallback io-interrupt-callback :boolean ((opaque :pointer))
  (let ((ctx (get-io-ctx (pointer-address opaque))))
    (when ctx
      (let ((icb (%io-context-icb ctx)))
        (and icb (funcall icb))))))

(defcallback io-read-callback :int ((opaque :pointer)
                                    (buffer :pointer)
                                    (buffer-size :int))
  (let ((ctx (get-io-ctx (pointer-address opaque))))
    (if ctx
      (let ((rcb (%io-context-rcb ctx)))
        (if rcb
          (let* ((buf (%io-context-buf ctx))
                 (rv (funcall rcb buf buffer-size)))
            (when (> rv 0)
              (with-pointer-to-vector-data (pdata buf)
                (memcpy buffer pdata rv)))
            rv)
          -1))
      -1)))

(defcallback io-write-callback :int ((opaque :pointer)
                                     (buffer :pointer)
                                     (buffer-size :pointer))
  (let ((ctx (get-io-ctx (pointer-address opaque))))
    (if ctx
      (let ((wcb (%io-context-wcb ctx)))
        (if wcb
          (let* ((buf (%io-context-buf ctx))
                 (rv (funcall wcb buf buffer-size)))
            (when (> rv 0)
              (with-pointer-to-vector-data (pdata buf)
                (memcpy pdata buffer rv)))
            rv)
          -1))
      -1)))

(defcallback io-seek-callback :int64 ((opaque :pointer)
                                      (offset :int64)
                                      (whence :int))
  (let ((ctx (get-io-ctx (pointer-address opaque))))
    (if ctx
      (let ((scb (%io-context-scb ctx)))
        (if scb
          (let ((rv (funcall scb offset whence)))
            rv)
          -1))
      -1)))

(defun io-context-alive-p (ctx)
  (declare (type io-context ctx))
  (not (null-pointer-p (%io-context-ptr ctx))))

(defun free-io-context (ctx)
  (declare (type io-context ctx))
  (let ((ptr (%io-context-ptr ctx))
        (token (%io-context-token-ptr ctx))
        (opened (%io-context-opened ctx)))
    (unless (null-pointer-p ptr)
      (cancel-finalization ctx)
      (setf (%io-context-token-ptr ctx) (null-pointer)
            (%io-context-ptr ctx) (null-pointer))
      (unless (null-pointer-p token)
        (av-free token))
      (if opened
        (avio-close ptr)
        (av-free ptr))))
  (values))

(defun open-io-context (url &key (flags :read)
                                 interrupt-callback
                                 options)
  (declare (type string url)
           (type (or list keyword) flags)
           (type list options)
           (type (or null function symbol) interrupt-callback))
  (mklistf flags)
  (with-foreign-objects ((icb '(:struct avio-interrupt-callback))
                         (pp :pointer))
    (setf (mem-ref pp :pointer) (null-pointer))    
    (with-dict (dict options) 
      (with-foreign-pointer (purl 1024)
        (lisp-string-to-foreign url purl 1024 :encoding :utf-8)
        (let ((token (av-mallocz (foreign-type-size :pointer))))
          (when (null-pointer-p token) (error 'out-of-memory))
          (setf (mem-aref icb :pointer 0) (callback io-interrupt-callback)
                (mem-aref icb :pointer 1) token)
          (let ((rv (avio-open2 pp purl flags (if interrupt-callback icb (null-pointer)) dict)))
            (when (< rv 0)
              (foreign-free token)
              (check-rv rv))
            (let* ((p (mem-ref pp :pointer))
                   (ctx (%io-context p token t interrupt-callback (make-array 1 :element-type 'uint8))))
              (finalize ctx (lambda ()
                              (av-free token)
                              (avio-close p)))
              (set-io-ctx (pointer-address token) ctx)
              (values ctx (from-dict dict)))))))))

(defun make-io-context (buffer-size &key writep
                                         read-callback
                                         write-callback
                                         seek-callback)
  (declare (type int32 buffer-size)
           (type (or null function symbol)
                 read-callback write-callback seek-callback))
  (let ((token (av-mallocz buffer-size)))
    (when (null-pointer-p token) (error 'out-of-memory))
    (let ((ptr (avio-alloc-context token
                                   buffer-size
                                   writep
                                   token
                                   (if read-callback
                                     (callback io-read-callback)
                                     (null-pointer))
                                   (if write-callback
                                     (callback io-write-callback)
                                     (null-pointer))
                                   (if seek-callback
                                     (callback io-seek-callback)
                                     (null-pointer)))))
      (when (null-pointer-p ptr)
        (av-free token)
        (error 'out-of-memory))
      (let ((ctx (%io-context ptr token nil nil
                              (make-array buffer-size :element-type 'uint8)
                              read-callback
                              write-callback
                              seek-callback)))
        (finalize ctx (lambda ()
                        (av-free token)
                        (av-free ptr)))
        ctx))))

(defmacro with-io-context ((var context) &body body)
  `(let ((,var ,context))
     (unwind-protect (locally ,@body)
       (free-io-context ,var))))

(defmethod print-object ((ctx io-context) stream)
  (print-unreadable-object (ctx stream :type t :identity t)
    (when (null-pointer-p (%io-context-ptr ctx))
      (format stream "Closed ")))
  ctx)

;; vim: ft=lisp et
