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

(defcstruct (av-profile :class av-profile)
  (profile :int)
  (name :pointer))

(defcstruct (av-codec :class av-codec)
  (name (:string :encoding :utf-8))
  (long-name (:string :encoding :utf-8))
  (media-type media-type)
  (id codec-id)
  (caps codec-capabilities)
  (framerates :pointer)
  (pix-fmts pixel-format-list)
  (samplerates :pointer)
  (sample-fmts sample-format-list)
  (layouts channel-layout-list)
  (max-lowres :uint8)
  (priv-class :pointer)
  (profiles :pointer)
  )

(defmacro %with-codec-slots (((&rest vars) codec) &body body)
  `(with-foreign-slots ((,@vars) (%codec-ptr ,codec) (:struct av-codec))
     ,@body))

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

(defcfun (av-codec-get-max-lowres "av_codec_get_max_lowres" :library libavcodec)
    :int
  (codec :pointer))

(defaccessors codec codec- (:struct av-codec) %codec-ptr
  (name string "Codec name")
  (long-name string "Codec long name")
  ((type media-type) keyword "Codec type")
  (id keyword "Codec id")
  ((capabilities caps) list "Codec capabilities" nil nil)
  ((pixel-formats pix-fmts) list "Pixel formats supported by codec" nil nil)
  ((sample-formats sample-fmts) list "Sample formats supported by codec" nil nil)
  ((channel-layouts layouts) list "Channel layouts supported by codec" nil nil))

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
  (with-stack-string (p name 128)
    (let ((ptr (avcodec-find-decoder-by-name p)))
      (when (null-pointer-p ptr)
        (error 'decoder-not-found))
      (%codec ptr))))

(defun find-encoder-by-name (name)
  (declare (type string name))
  (with-stack-string (p name 128)
    (let ((ptr (avcodec-find-encoder-by-name p)))
      (when (null-pointer-p ptr)
        (error 'encoder-not-found))
      (%codec ptr))))

(defun codec-framerates (codec)
  (declare (type codec codec))
  (%with-codec-slots ((framerates) codec)
    (if (null-pointer-p framerates)
      '()
      (loop :with list = '()
            :with p = framerates
            :for rate = (mem-ref p '(:struct av-rational))
            :until (zerop rate)
            :do (push rate list)
                (incf-pointer p (foreign-type-size '(:struct av-rational)))
            :finally (return (nreverse list))))))

(defun codec-samplerates (codec)
  "Sample rates supported by codec"
  (declare (type codec codec))
  (%with-codec-slots ((samplerates) codec)
    (let ((p samplerates))
      (if (null-pointer-p p)
        '()
        (loop :with list = '()
              :for x = (mem-ref p :int)
              :until (zerop x)
              :do (push x list)
                  (incf-pointer p (foreign-type-size :int))
              :finally (return (nreverse list)))))))

(defun codec-max-lowres (codec)
  "Codec max lowres"
  (declare (type codec codec))
  (check-rv (av-codec-get-max-lowres (%codec-ptr codec))))

(defun codec-profiles (codec)
  "Codec profile list"
  (declare (type codec codec))
  (%with-codec-slots ((profiles) codec)
    (let ((p profiles))
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
  "Codec profile identifier"
  (declare (type codec-profile profile))
  (%codec-profile-id profile))

(defun codec-profile-name (profile)
  "Codec profile name"
  (declare (type codec-profile profile))
  (%codec-profile-name profile))

(define-foreign-type codec-ptr ()
  ()
  (:actual-type :pointer)
  (:simple-parser codec-ptr))

(defmethod translate-to-foreign (codec (type codec-ptr))
  (if codec (%codec-ptr codec) (null-pointer)))

(defmethod translate-from-foreign (ptr (type codec-ptr))
  (if (null-pointer-p ptr) nil (%codec ptr)))

(defmethod expand-to-foreign (codec (type codec-ptr))
  `(if ,codec (%codec-ptr ,codec) (null-pointer)))

(defmethod expand-from-foreign (ptr (type codec-ptr))
  `(if (null-pointer-p ,ptr) nil (%codec ,ptr)))

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
                          (pprint-newline :linear stream)))))))
      (format stream " {#x~8,'0X}" (pointer-address (%codec-ptr codec))))
    (call-next-method)))

;; vim: ft=lisp et
