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
  (filename :pointer)
  (buf :pointer)
  (buf-size :int)
  (mime-type :pointer))

(defcstruct av-stream
  (index :int)
  (id :int)
  (codec :pointer)
  (priv-data :pointer)
  (pts-val :int64)
  (pts-num :int64)
  (pts-denom :int64)
  (tb-num :int)
  (tb-denom :int)
  (start-time :int64)
  (duration :int64)
  (nb-frames :int64)
  (disposition disposition)
  (discard discard)
  (sar-num :int)
  (sar-denom :int)
  (metadata :pointer)
  (avg-frame-rate-num :int)
  (avg-frame-rate-denom :int)
  (attached-pic (:struct av-packet))
  (side-data :pointer)
  (nb-side-data :int)
  (metadata-updated :boolean)
  (info :pointer)
  (pts-wrap-bits :pointer)
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
  (pts-buffer :int64 :count 17)
  (index-entries :pointer)
  (nb-index-entries :int)
  (index-entries-allocated-size :uint)
  (r-frame-rate-num :int)
  (r-frame-rate-denom :int)
  (stream-identifier :int)
  (interleaver-chunk-size :int64)
  (interleaver-chunk-duration :int64)
  (request-probe :int)
  (skip-to-keyframe :boolean)
  (skip-samples :int)
  (start-skip-samples :int64)
  (first-discard-sample :int64)
  (last-discard-sample :int64)
  (nb-decoded-frames :int)
  (mux-ts-offset :int64)
  (pts-wrap-reference :int64)
  (pts-wrap-behavior :int)
  (update-initial-durations-done :boolean)
  (pts-reorder-error :int64 :count 17)
  (pts-reorder-error-count :uint8 :count 17)
  (last-dts-for-order-check :int64)
  (dts-ordered :uint8)
  (dts-misordered :uint8)
  (inject-global-side-data :int)
  (recommended-encoder-configuration :pointer)
  (dar-num :int)
  (dar-denom :int)
  (prv-pts :pointer)
  (internal :pointer)
  (codecpar :pointer))

(defmacro with-stream-slots ((var stream accessor-name) &body body)
  `(let ((,var (%media-stream-ptr ,stream)))
     (declare (ignorable ,var))
     (macrolet ((,accessor-name (slot)
                  `(foreign-slot-value ,',var '(:struct av-stream) ',slot)))
       ,@body)))

(macrolet ((defaccessor (name accessor-name value-type value-var read-form write-form)
             `(progn
                (declaim (inline ,name))
                (defun ,name (media-stream)
                  (declare (type media-stream media-stream))
                  (with-stream-slots (ptr media-stream ,accessor-name)
                    ,read-form))
                (declaim (inline (setf ,name)))
                (defun (setf ,name) (,value-var media-stream)
                  (declare (type media-stream media-stream)
                           (type ,value-type ,value-var))
                  (with-stream-slots (ptr media-stream ,accessor-name)
                    ,write-form)
                  ,value-var))))
  (defaccessor media-stream-id acc int32 value
    (acc id)
    (setf (acc id) value))
  (defaccessor media-stream-index acc int32 value
    (acc index)
    (setf (acc index) value))
  (defaccessor media-stream-sample-aspect-ratio acc rational value
    (let ((n (acc sar-num))
          (d (acc sar-denom)))
      (if (zerop d) 0 (/ n d)))
    (setf (acc sar-num) (numerator value)
          (acc sar-denom) (denominator value)))
  (defaccessor media-stream-disposition acc keyword value
    (acc disposition)
    (setf (acc disposition) value))
  (defaccessor media-stream-codecpar acc codec-parameters value
    (%codec-parameters (acc codecpar) media-stream)
    (check-rv (avcodec-parameters-copy
               (acc codecpar)
               (%codec-parameters-ptr value))))
  (defaccessor media-stream-time-base acc rational value
    (let ((n (acc tb-num))
          (d (acc tb-denom)))
      (if (zerop d) 0 (/ n d)))
    (setf (acc tb-num) (numerator value)
          (acc tb-denom) (denominator value)))
  (defaccessor media-stream-avg-frame-rate acc rational value
    (let ((n (acc avg-frame-rate-num))
          (d (acc avg-frame-rate-denom)))
      (if (zerop d) 0 (/ n d)))
    (setf (acc avg-frame-rate-num) (numerator value)
          (acc avg-frame-rate-denom) (denominator value)))
  )

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
          (format stream "Average frame rate: ~a~:@_" fr))      
        (format stream "Disposition: ~a~:@_" (media-stream-disposition media-stream))
        (write-string "Codec params: " stream)
        (write (media-stream-codecpar media-stream) :stream stream)
        (pprint-newline :mandatory stream)
        (format stream "{#x~8,'0X}" (pointer-address (%media-stream-ptr media-stream)))))
    (call-next-method)))

;; vim: ft=lisp et
