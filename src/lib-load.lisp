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

(defun mark-ffmpeg-not-initialized ()
  (setf *ffmpeg-initialized* nil))

(defun initialize-ffmpeg ()
  (unless *ffmpeg-initialized*
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
