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

(defconstant +ff-profile-unknown+ -99)

(defcstruct av-profile
  (profile :int)
  (name :pointer))

(defcstruct av-codec
  (name (:string :encoding :utf-8))
  (long-name (:string :encoding :utf-8))
  (media-type media-type)
  (id codec-id)
  (caps codec-capabilities)
  (framerates :pointer)
  (pix-fmts :pointer)
  (samplerates :pointer)
  (sample-fmts :pointer)
  (layouts :pointer)
  (max-lowres :uint8)
  (priv-class :pointer)
  (profiles :pointer)
  )

(defmacro with-codec-slots ((codec-value accessor-name) &body body)
  (with-gensyms (ptr)
    `(let ((,ptr (%codec-ptr ,codec-value)))
       (macrolet ((,accessor-name (slot)
                    `(foreign-slot-value ,',ptr '(:struct av-codec) ',slot)))
         ,@body))))

(defcfun (avcodec-find-decoder "avcodec_find_decoder" :library libavcodec)
    :pointer
  (id codec-id))

(defcfun (avcodec-find-encoder "avcodec_find_encoder" :library libavcodec)
    :pointer
  (id codec-id))

(defcfun (avcodec-find-decoder-by-name "avcodec_find_decoder_by_name" :library libavcodec)
    :pointer
  (name :pointer))

(defcfun (avcodec-find-encoder-by-name "avcodec_find_encoder_by_name" :library libavcodec)
    :pointer
  (name :pointer))

(defun find-decoder (id)
  (declare (type keyword id))
  (let ((ptr (avcodec-find-decoder id)))
    (when (null-pointer-p ptr)
      (error 'decoder-not-found))
    (%codec ptr)))

(defun find-encoder (id)
  (declare (type keyword :id))
  (let ((ptr (avcodec-find-encoder id)))
    (when (null-pointer-p ptr)
      (error 'encoder-not-found))
    (%codec ptr)))

(defun find-decoder-by-name (name)
  (declare (type string name))
  (with-foreign-pointer (p 128)
    (lisp-string-to-foreign name p 128 :encoding :utf-8)
    (let ((ptr (avcodec-find-decoder-by-name p)))
      (when (null-pointer-p ptr)
        (error 'decoder-not-found))
      (%codec ptr))))

(defun find-encoder-by-name (name)
  (declare (type string name))
  (with-foreign-pointer (p 128)
    (lisp-string-to-foreign name p 128 :encoding :utf-8)
    (let ((ptr (avcodec-find-encoder-by-name p)))
      (when (null-pointer-p ptr)
        (error 'encoder-not-found))
      (%codec ptr))))

(defun codec-name (codec)
  (declare (type codec codec))
  (with-codec-slots (codec acc)
    (acc name)))

(defun codec-long-name (codec)
  (declare (type codec codec))
  (with-codec-slots (codec acc)
    (acc name)))

(defun codec-type (codec)
  (declare (type codec codec))
  (with-codec-slots (codec acc)
    (acc media-type)))

(defun codec-id (codec)
  (declare (type codec codec))
  (with-codec-slots (codec acc)
    (acc id)))

(defun codec-capabilities (codec)
  (declare (type codec codec))
  (with-codec-slots (codec acc)
    (acc caps)))

(defun codec-framerates (codec)
  (declare (type codec codec))
  (with-codec-slots (codec acc)
    (let ((p (acc framerates)))
      (if (null-pointer-p p)
        '()
        (loop :with list = '()
              :for n = (mem-aref p :int 0)
              :for d = (mem-aref p :int 1)
              :until (zerop d)
              :do (push (/ n d) list)
                  (incf-pointer p 8)
              :finally (return (nreverse list)))))))

(defun codec-pixel-formats (codec)
  (declare (type codec codec))
  (with-codec-slots (codec acc)
    (let ((p (acc pix-fmts)))
      (if (null-pointer-p p)
        '()
        (loop :with list = '()
              :until (= -1 (mem-ref p :int))
              :do (push (mem-ref p 'pixel-format) list)
                  (incf-pointer p 4)
              :finally (return (nreverse list)))))))

(defun codec-sample-formats (codec)
  (declare (type codec codec))
  (with-codec-slots (codec acc)
    (let ((p (acc sample-fmts)))
      (if (null-pointer-p p)
        '()
        (loop :with list = '()
              :until (= -1 (mem-ref p :int))
              :do (push (mem-ref p 'sample-format) list)
                  (incf-pointer p 4)
              :finally (return (nreverse list)))))))

(defun codec-samplerates (codec)
  (declare (type codec codec))
  (with-codec-slots (codec acc)
    (let ((p (acc samplerates)))
      (if (null-pointer-p p)
        '()
        (loop :with list = '()
              :for x = (mem-ref p :int)
              :until (zerop x)
              :do (push x list)
                  (incf-pointer p 4)
              :finally (return (nreverse list)))))))

(defun codec-channel-layouts (codec)
  (declare (type codec codec))
  (with-codec-slots (codec acc)
    (let ((p (acc layouts)))
      (if (null-pointer-p p)
        '()
        (loop :with list = '()
              :for x = (mem-ref p :uint64)
              :until (zerop x)
              :do (push (to-channel-layout x) list)
                  (incf-pointer p 8)
              :finally (return (nreverse list)))))))

(defcfun (av-codec-get-max-lowres "av_codec_get_max_lowres" :library libavcodec)
    :int
  (codec :pointer))

(defun codec-max-lowres (codec)
  (declare (type codec codec))
  (check-rv (av-codec-get-max-lowres (%codec-ptr codec))))

(defun codec-profiles (codec)
  (declare (type codec codec))
  (with-codec-slots (codec acc)
    (let ((p (acc profiles)))
      (if (null-pointer-p p)
        '()
        (loop :with list = '()
              :for profile = (foreign-slot-value p '(:struct av-profile) 'profile)
              :for name = (foreign-slot-value p '(:struct av-profile) 'name)
              :until (= profile +ff-profile-unknown+)
              :do (push (%codec-profile profile
                                        (or (and (not (null-pointer-p name))
                                                 (foreign-string-to-lisp name :encoding :utf-8))
                                            (to-string "<No name>")))
                        list)
                  (incf-pointer p (foreign-type-size '(:struct av-profile)))
              :finally (return (nreverse list)))))))

(defun codec-profile-id (profile)
  (declare (type codec-profile profile))
  (%codec-profile-id profile))

(defun codec-profile-name (profile)
  (declare (type codec-profile profile))
  (%codec-profile-name profile))

(defmethod print-object ((profile codec-profile) stream)
  (if *print-pretty*
    (print-unreadable-object (profile stream :type t)
      (format stream "Id: ~a; Name: ~a"
              (codec-profile-id profile)
              (codec-profile-name profile)))
    (call-next-method)))

(defmethod print-object ((codec codec) stream)
  (if *print-pretty*
    (print-unreadable-object (codec stream :type t)
      (pprint-logical-block (stream nil)
        (format stream "Name: ~a" (codec-name codec))
        (pprint-newline :mandatory stream)
        (format stream "Long name: ~a" (codec-long-name codec))
        (pprint-newline :mandatory stream)
        (format stream "Type: ~a" (codec-type codec))
        (pprint-newline :mandatory stream)
        (format stream "Id: ~a" (codec-id codec))
        (pprint-newline :mandatory stream)
        (let ((caps (codec-capabilities codec)))
          (when caps
            (format stream "Caps: ~{~a~^, ~}" caps)
            (pprint-newline :mandatory stream)))
        (let ((fmts (codec-pixel-formats codec)))
          (when fmts
            (write-string "Pixel formats: " stream)
            (pprint-logical-block (stream nil)
              (loop :for (fmt . next) :on fmts
                    :do (format stream "~a" fmt)
                        (when next
                          (write-string ", " stream)
                          (pprint-newline :fill stream))))
            (pprint-newline :linear stream)))
        (let ((fmts (codec-sample-formats codec)))
          (when fmts
            (format stream "Sample formats: ~{~a~^, ~}" fmts)
            (pprint-newline :mandatory stream)))
        (let ((rates (codec-framerates codec)))          
          (when rates
            (write-string "Framerates: " stream)
            (pprint-logical-block (stream nil)
              (loop :for (rate . next) :on rates
                    :do (format stream "~a" rate)
                        (when next
                          (write-string ", " stream)
                          (pprint-newline :fill stream))))
            (pprint-newline :linear stream)))
        (let ((rates (codec-samplerates codec)))
          (when rates
            (write-string "Samplerates: " stream)
            (pprint-logical-block (stream nil)
              (loop :for (rate . next) :on rates
                    :do (format stream "~a" rate)
                        (when next
                          (write-string ", " stream)
                          (pprint-newline :fill stream))))
            (pprint-newline :linear stream)))
        (let ((layouts (codec-channel-layouts codec)))          
          (when layouts
            (write-string "Channel layouts: " stream)
            (pprint-logical-block (stream nil)
              (loop :for (layout . next) :on layouts
                    :do (format stream "~a" layout)
                        (when next
                          (write-string ", " stream)
                          (pprint-newline :fill stream))))
            (pprint-newline :linear stream)))
        (let ((profiles (codec-profiles codec)))
          (when profiles
            (write-string "Profiles: " stream)
            (pprint-logical-block (stream nil)
              (loop :for (p . next) :on profiles
                    :do (format stream "Id: ~a; ~a"
                                (codec-profile-id p)
                                (codec-profile-name p))
                        (when next
                          (pprint-newline :linear stream))))))))
    (call-next-method)))

;; vim: ft=lisp et
