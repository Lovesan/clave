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

(defun io-format-input-p (io-format)
  (declare (type io-format io-format))
  (%io-format-input io-format))

(defcstruct av-input-format
  (name (:string :encoding :utf-8))
  (long-name (:string :encoding :utf-8))
  (flags format-flags)
  (extensions :pointer)
  (codec-tag :pointer)
  (priv-class :pointer)
  (mime-type :pointer))

(defcstruct av-output-format
  (name (:string :encoding :utf-8))
  (long-name (:string :encoding :utf-8))
  (mime-type :pointer)
  (extensions :pointer)
  (audio-codec codec-id)
  (video-codec codec-id)
  (subtitle-codec codec-id)
  (flags format-flags)
  (codec-tag :pointer))

(defmacro with-io-format-slots
    ((format-value accessor-name &optional is-input-var) &body body)
  (with-gensyms (fmt ptr)
    (unless is-input-var (setf is-input-var (gensym)))
    `(let* ((,fmt ,format-value)
            (,ptr (%io-format-ptr ,fmt))
            (,is-input-var (%io-format-input ,fmt)))
       (declare (ignorable ,is-input-var))
       (if ,is-input-var
         (macrolet ((,accessor-name (slot)
                      `(foreign-slot-value ,',ptr '(:struct av-input-format) ',slot)))
           ,@body)
         (macrolet ((,accessor-name (slot)
                      `(foreign-slot-value ,',ptr '(:struct av-output-format) ',slot)))
           ,@body)))))

(defcfun (av-find-input-format "av_find_input_format" :library libavformat)
    :pointer
  (name :pointer))

(defun find-input-format (name)
  (declare (type string name))
  (with-foreign-pointer (buf 128)
    (lisp-string-to-foreign name buf 128 :encoding :utf-8)
    (let ((p (av-find-input-format buf)))
      (when (null-pointer-p p)
        (error 'demuxer-not-found))
      (%io-format p t))))

(defcfun (av-guess-format "av_guess_format" :library libavformat)
    :pointer
  (name :pointer)
  (filename :pointer)
  (mime-type :pointer))

(defun guess-output-format (name &optional filename mime-type)
  (declare (type (or null string) name filename mime-type))
  (with-foreign-pointer (pname 128)
    (with-foreign-pointer (pfilename 128)
      (with-foreign-pointer (pmime 128)
        (when name (lisp-string-to-foreign name pname 128 :encoding :utf-8))
        (when filename (lisp-string-to-foreign filename pfilename 128 :encoding :utf-8))
        (when mime-type (lisp-string-to-foreign mime-type pmime 128 :encoding :utf-8))
        (let ((p (av-guess-format (if name pname (null-pointer))
                                  (if filename pfilename (null-pointer))
                                  (if mime-type pmime (null-pointer)))))
          (when (null-pointer-p p)
            (error 'muxer-not-found))
          (%io-format p nil))))))

(defun io-format-name (io-format)
  (declare (type io-format io-format))
  (with-io-format-slots (io-format acc)
    (acc name)))

(defun io-format-long-name (io-format)
  (declare (type io-format io-format))
  (with-io-format-slots (io-format acc)
    (acc long-name)))

(defun io-format-mime-type (io-format)
  (declare (type io-format io-format))
  (let ((ptr (with-io-format-slots (io-format acc)
               (acc mime-type))))
    (if (null-pointer-p ptr)
      nil
      (foreign-string-to-lisp ptr :encoding :utf-8))))

(defun io-format-flags (io-format)
  (declare (type io-format io-format))
  (with-io-format-slots (io-format acc)
    (acc flags)))

(defun io-format-extensions (io-format)
  (declare (type io-format io-format))
  (let ((ptr (with-io-format-slots (io-format acc)
               (acc extensions))))
    (if (null-pointer-p ptr)
      '()
      (split-string (foreign-string-to-lisp ptr :encoding :utf-8) #\,))))

(defun io-format-audio-codec (io-format)
  (declare (type io-format io-format))
  (if (%io-format-input io-format)
    :none
    (foreign-slot-value (%io-format-ptr io-format)
                        '(:struct av-output-format)
                        'audio-codec)))

(defun io-format-video-codec (io-format)
  (declare (type io-format io-format))
  (if (%io-format-input io-format)
    :none
    (foreign-slot-value (%io-format-ptr io-format)
                        '(:struct av-output-format)
                        'video-codec)))

(defun io-format-subtitle-codec (io-format)
  (declare (type io-format io-format))
  (if (%io-format-input io-format)
    :none
    (foreign-slot-value (%io-format-ptr io-format)
                        '(:struct av-output-format)
                        'subtitle-codec)))

(defmethod print-object ((fmt io-format) stream)
  (if *print-pretty*
    (print-unreadable-object (fmt stream :type t)
      (pprint-logical-block (stream nil)
        (format stream "~:[Output~;Input~]" (io-format-input-p fmt))
        (pprint-newline :mandatory stream)
        (format stream "Name: ~a" (io-format-name fmt))
        (pprint-newline :mandatory stream)
        (format stream "Long name: ~a" (io-format-long-name fmt))
        (pprint-newline :mandatory stream)
        (let ((mime (io-format-mime-type fmt)))
          (when mime
            (format stream "MIME type: ~a" mime)
            (pprint-newline :mandatory stream)))
        (let ((flags (io-format-flags fmt)))
          (when flags
            (format stream "Flags: ~{~a~^, ~}" flags)
            (pprint-newline :mandatory stream)))
        (let ((exts (io-format-extensions fmt)))
          (when exts
            (format stream "Extensions: ~{~a~^, ~}" exts)
            (pprint-newline :mandatory stream)))
        (let ((codec-id (io-format-audio-codec fmt)))
          (unless (eq codec-id :none)
            (format stream "Default audio codec: ~a" codec-id)
            (pprint-newline :mandatory stream)))
        (let ((codec-id (io-format-video-codec fmt)))
          (unless (eq codec-id :none)
            (format stream "Default video codec: ~a" codec-id)
            (pprint-newline :mandatory stream)))
        (let ((codec-id (io-format-subtitle-codec fmt)))
          (unless (eq codec-id :none)
            (format stream "Default subtitle codec: ~a" codec-id)
            (pprint-newline :mandatory stream)))))
    (call-next-method)))

;; vim: ft=lisp et
