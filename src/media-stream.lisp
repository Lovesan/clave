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

(defconstant +max-reorder-delay+ 16)

(defcstruct av-probe-data
  (filename (:pointer (:string :encoding :utf-8)))
  (buf :pointer)
  (buf-size :int)
  (mime-type (:pointer :string)))

(defcstruct (av-stream :class av-stream)
  (index :int)
  (id :int)
  #-FF-API-LAVF-AVCTX (codec :pointer) ;; deprecated
  (priv-data :pointer)
  (time-base (:struct av-rational))
  (start-time :int64)
  (duration :int64)
  (nb-frames :int64)
  (disposition disposition)
  (discard discard)
  (sample-aspect-ratio (:struct av-rational))
  (metadata :pointer) ;; av-dictionary
  (avg-frame-rate (:struct av-rational))
  (attached-pic (:struct av-packet))
  (side-data :pointer) ;; av-packet-side-data
  (nb-side-data :int)
  (event-flags :int)
  (r-frame-rate (:struct av-rational))
  #-FF-API-LAVF-SERVER (recommended-encoder-configuration :pointer) ;; deprecated
  (codecpar :pointer)

  ;; --- rest is not part of the public API ---
  (info :pointer)
  (pts-wrap-bits :int)

  (first-dts :int64)
  (cur-dts :int64)
  (last-ip-pts :int64)
  (last-ip-duration :int)

  (probe-packets :int)
  (codec-info-nb-frames :int)
  (need-parsing stream-parse-type)
  (parser :pointer)

  (last-in-packet-buffer :pointer)
  (probe-data (:struct av-probe-data))
  (pts-buffer :int64 :count #.(1+ +max-reorder-delay+))
  (index-entries :pointer)
  (nb-index-entries :int)
  (index-entries-allocated-size :uint)
  (stream-identifier :int)

  (program-num :int)
  (pmt-version :int)
  (pmt-stream-idx :int)

  (interleaver-chunk-size :int64)
  (interleaver-chunk-duration :int64)

  (request-probe :int)
  (skip-to-keyframe :int)
  (skip-samples :int)
  (start-skip-samples :int64)
  (first-discard-sample :int64)
  (last-discard-sample :int64)
  (nb-decoded-frames :int)
  (mux-ts-offset :int64)
  (pts-wrap-reference :int64)
  (pts-wrap-behavior :int)
  (update-initial-duration-done :int)
  (pts-reorder-error :int64 :count #.(1+ +max-reorder-delay+))
  (pts-reorder-error-count :uint8 :count #.(1+ +max-reorder-delay+))
  (last-dts-for-order-check :int64)
  (dts-ordered :uint8)
  (dts-misordered :uint8)
  (inject-global-side-data :int)
  (display-aspect-ratio (:struct av-rational))
  (internal :pointer))

(defmacro with-stream-slots ((var stream accessor-name) &body body)
  `(let ((,var (%media-stream-ptr ,stream)))
     (declare (ignorable ,var))
     (macrolet ((,accessor-name (slot)
                  `(foreign-slot-value ,',var '(:struct av-stream) ',slot)))
       ,@body)))

(defaccessors media-stream media-stream- (:struct av-stream) %media-stream-ptr
  (id int "Stream ID" t)
  (index int "Stream index" t)
  (sample-aspect-ratio rational "Sample aspect ratio" t)
  (disposition keyword "Stream disposition" t)
  (time-base rational "Stream time base" t)
  (avg-frame-rate rational "Stream average frame rate" t))

(defun media-stream-codecpar (media-stream)
  "Stream codec parameters"
  (declare (type media-stream media-stream))
  (%codec-parameters
   (mem-ref (%media-stream-ptr media-stream)
            :pointer
            (foreign-slot-offset '(:struct av-stream) 'codecpar))
   media-stream))

(defmethod print-object ((media-stream media-stream) stream)
  (if *print-pretty*
    (print-unreadable-object (media-stream stream :type t)
      (pprint-logical-block (stream nil)
        (format stream "Id: ~a~:@_" (media-stream-id media-stream))
        (format stream "Index: ~a~:@_" (media-stream-index media-stream))
        (let-when (sar (media-stream-sample-aspect-ratio media-stream) (/= sar 0))
          (format stream "Sample aspect ratio: ~a~:@_" sar))
        (let-when (tb (media-stream-time-base media-stream) (/= tb 0))
          (format stream "Time base: ~a~:@_" tb))
        (let-when (fr (media-stream-avg-frame-rate media-stream) (/= fr 0))
          (format stream "Average frame rate: ~a~:@_" (float fr)))
        (format stream "Disposition: ~a~:@_" (media-stream-disposition media-stream))
        (write-string "Codec params: " stream)
        (write (media-stream-codecpar media-stream) :stream stream)
        (pprint-newline :mandatory stream)
        (format stream "{#x~8,'0X}" (pointer-address (%media-stream-ptr media-stream)))))
    (call-next-method)))

;; vim: ft=lisp et
