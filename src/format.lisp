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

(defcstruct (av-input-format :class av-output-format)
  (name (:string :encoding :utf-8))
  (long-name (:string :encoding :utf-8))
  (flags format-flags)
  (extensions :pointer)
  (codec-tag :pointer)
  (priv-class :pointer)
  (mime-type :pointer))

(defcstruct (av-output-format :class av-output-format)
  (name (:string :encoding :utf-8))
  (long-name (:string :encoding :utf-8))
  (mime-type :pointer)
  (extensions :pointer)
  (audio-codec codec-id)
  (video-codec codec-id)
  (subtitle-codec codec-id)
  (flags format-flags)
  (codec-tag :pointer))

(defcfun (av-find-input-format "av_find_input_format" :library libavformat)
    :pointer
  (name :pointer))

(defcfun (av-guess-format "av_guess_format" :library libavformat)
    :pointer
  (name :pointer)
  (filename :pointer)
  (mime-type :pointer))

(defmacro %with-io-format-slots
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

(defun io-format-input-p (io-format)
  "Returns non-nil value when IO-FORMAT is input format"
  (declare (type io-format io-format))
  (%io-format-input io-format))

(defun find-input-format (name)
  "Finds input format named NAME"
  (declare (type string name))
  (with-stack-string (buf name 128)
    (let ((p (av-find-input-format buf)))
      (when (null-pointer-p p)
        (error 'demuxer-not-found))
      (%io-format p t))))

(defun guess-output-format (name &optional filename mime-type)
  "Guess output format using name, filename or mime-type"
  (declare (type (or null string) name filename mime-type))
  (with-stack-string (pname name 128 name)
    (with-stack-string (pfilename name 128 filename)
      (with-stack-string (pmime name 128 mime-type)
        (let ((p (av-guess-format (if name pname (null-pointer))
                                  (if filename pfilename (null-pointer))
                                  (if mime-type pmime (null-pointer)))))
          (when (null-pointer-p p)
            (error 'muxer-not-found))
          (%io-format p nil))))))

(defun io-format-name (io-format)
  "Format name"
  (declare (type io-format io-format))  
  (%with-io-format-slots (io-format acc)
    (acc name)))

(defun io-format-long-name (io-format)
  "Format long name"
  (declare (type io-format io-format))
  (%with-io-format-slots (io-format acc)
    (acc long-name)))

(defun io-format-mime-types (io-format)
  "Format mime type list"
  (declare (type io-format io-format))
  (let ((ptr (%with-io-format-slots (io-format acc)
               (acc mime-type))))
    (if (null-pointer-p ptr)
      nil
      (split-string (foreign-string-to-lisp ptr :encoding :utf-8) #\,))))

(defun io-format-flags (io-format)
  "Format flags"
  (declare (type io-format io-format))
  (%with-io-format-slots (io-format acc)
    (acc flags)))

(defun io-format-extensions (io-format)
  "Format file extension list"
  (declare (type io-format io-format))
  (let ((ptr (%with-io-format-slots (io-format acc)
               (acc extensions))))
    (if (null-pointer-p ptr)
      '()
      (split-string (foreign-string-to-lisp ptr :encoding :utf-8) #\,))))

(defun io-format-audio-codec (io-format)
  "Format default audio codec"
  (declare (type io-format io-format))
  (if (%io-format-input io-format)
    :none
    (foreign-slot-value (%io-format-ptr io-format)
                        '(:struct av-output-format)
                        'audio-codec)))

(defun io-format-video-codec (io-format)
  "Format default video codec"
  (declare (type io-format io-format))
  (if (%io-format-input io-format)
    :none
    (foreign-slot-value (%io-format-ptr io-format)
                        '(:struct av-output-format)
                        'video-codec)))

(defun io-format-subtitle-codec (io-format)
  "Format default subtitle codec"
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
        (let ((mime (io-format-mime-types fmt)))
          (when mime
            (format stream "MIME types: ~{~a~^, ~}" mime)
            (pprint-newline :mandatory stream)))
        (let ((flags (io-format-flags fmt)))
          (when flags
            (format stream "Flags: ~{~a~^, ~}" flags)
            (pprint-newline :mandatory stream)))
        (let ((exts (io-format-extensions fmt)))
          (when exts
            (write-string "Extensions: " stream)
            (pprint-logical-block (stream nil)
              (loop :for (ext . next) :on exts
                    :do (format stream "~a" ext)
                        (when next
                          (write-string ", " stream)
                          (pprint-newline :fill stream))))
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
            (pprint-newline :mandatory stream))))
      (format stream "{#x~8,'0X}" (pointer-address (%io-format-ptr fmt))))
    (call-next-method)))

;; vim: ft=lisp et
