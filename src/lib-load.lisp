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

(defcfun (av-register-all "av_register_all" :library libavformat)
    :void)

(defcfun (avformat-network-init "avformat_network_init" :library libavformat)
    :void)

(defcfun (avcodec-register-all "avcodec_register_all" :library libavcodec)
    :void)

(defcfun (avfilter-register-all "avfilter_register_all" :library libavfilter)
    :void)

(defcfun (avdevice-register-all "avdevice_register_all" :library libavdevice)
    :void)

(defvar *ffmpeg-initialized* nil)

#+sbcl
(progn
  (defvar *io-contexts* (make-hash-table :test #'eql :weakness :value
                                         :synchronized t))
  (defvar *fmt-contexts* (make-hash-table :test #'eql :weakness :value
                                          :synchronized t))
  (defun get-io-ctx (addr)
    (gethash addr *io-contexts*))
  (defun set-io-ctx (addr ctx)
    (setf (gethash addr *io-contexts*) ctx))
  (defun get-fmt-ctx (addr)
    (gethash addr *fmt-contexts*))
  (defun set-fmt-ctx (addr ctx)
    (setf (gethash addr *fmt-contexts*) ctx)))
#-sbcl
(progn
  (defvar *io-contexts-mutex* (bt:make-lock))
  (defvar *io-contexts* (tg:make-weak-hash-table :test #'eql :weakness :value))
  (defvar *fmt-contexts-mutex* (bt:make-lock))
  (defvar *fmt-contexts* (tg:make-weak-pointer :test #'eql :weakness :value))
  (defun get-io-ctx (addr)
    (bt:with-lock-held (*io-contexts-mutex*)
      (gethash addr *io-contexts*)))
  (defun set-io-ctx (addr ctx)
    (bt:with-lock-held (*io-contexts-mutex*)
      (setf (gethash addr *io-contexts*) ctx)))
  (defun get-fmt-ctx (addr)
    (bt:with-lock-held (*fmt-contexts-mutex*)
      (gethash addr *fmt-contexts*)))
  (defun set-fmt-ctx (addr ctx)
    (bt:with-lock-held (*fmt-contexts-mutex*)
      (setf (gethash addr *fmt-contexts*) ctx))))

(defun mark-ffmpeg-not-initialized ()
  (setf *ffmpeg-initialized* nil))

(defun initialize-ffmpeg ()
  (unless *ffmpeg-initialized*
    #-sbcl
    (setf *io-contexts-mutex* (bt:make-lock))
    #-sbcl
    (setf *fmt-contexts-mutex* (bt:make-lock))
    (av-register-all)
    (avformat-network-init)
    (avcodec-register-all)
    (avfilter-register-all)
    (avdevice-register-all)
    (setf *ffmpeg-initialized* t)))

(defun restore-ffmpeg ()
  (mark-ffmpeg-not-initialized)
  (initialize-ffmpeg))

(initialize-ffmpeg)

(pushnew 'restore-ffmpeg uiop:*image-restore-hook*)

;; vim: ft=lisp et
