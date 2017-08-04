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

(defcenum ff-cmp
  :sad
  :sse
  :satd
  :dct
  :psnr
  :bit
  :rd
  :zero
  :vsad
  :vsse
  :nsse
  :w53
  :w97
  :dctmax
  :dct264
  :median-sad
  (:chroma 256))

(defbitfield slice-flags
  (:coded-order 1)
  (:allow-field 2)
  (:allow-plane 4))

(defcenum mb-decision
  :simple
  :bits
  :rd)

(defcenum error-concealment
  (:guess-mvs 1)
  (:deblock 2)
  (:favor-inter 256))

(defcenum dct-algo
  :auto
  :fastint
  :int
  :mmx
  :altivec
  :faan)

(defcenum idct-algo
  :auto
  :int
  :simple
  :simplemmx
  (:arm 7)
  :altivec
  :sh4
  :simplearm
  (:ipp 13)
  (:xvid 14)
  (:xvidmmx 14)
  (:simplearmv5te 16)
  :simplearmv6
  :simplevis
  (:faan 20)
  (:simpleneon 22)
  (:simplealpha 23)
  (:simpleauto 128))

(defcenum thread-type
  (:frame 1)
  (:slice 2))

(defcstruct av-codec-context
  (av-class :pointer)
  (log-level-offset :int)
  (codec-type media-type)
  (codec :pointer)
  (codec-name :char :count 32) ;; deprecated
  (codec-id codec-id)
  (codec-tag :uint)
  (stream-codec-tag :uint)
  (priv-data :pointer)
  (internal :pointer)
  (opaque :pointer)
  (bit-rate :int64)
  (bit-rate-tolerance :int)
  (global-quality :int)
  (compression-level :int)
  (flags codec-flags)
  (flags2 :int)
  (extradata :pointer)
  (extradata-size :int)
  (tb-num :int)
  (tb-denom :int)
  (ticks-per-frame :int)
  (delay :int)
  (width :int)
  (height :int)
  (coded-width :int)
  (coded-height :int)
  (gop-size :int)
  (pixel-format pixel-format)
  (me-method :int) ;; deprecated
  (draw-horiz-band :pointer)
  (get-format :pointer)
  (max-b-frames :int)
  (b-quant-factor :float)
  (rc-strategy :int) ;; deprecated
  (b-frame-strategy :int) ;; deprecated
  (b-quant-offset :float)
  (has-b-frames :boolean)
  (mpeg-quant :int) ;; deprecated
  (i-quant-factor :float)
  (i-quant-offset :float)
  (lumi-masking :float)
  (temporal-cplx-masking :float)
  (spatial-cplx-masking :float)
  (p-masking :float)
  (dark-masking :float)
  (slice-count :int)
  (prediction-method :int) ;; deprecated
  (slice-offset :pointer)
  (sar-num :int)
  (sar-denom :int)
  (me-cmp ff-cmp)
  (me-sub-cmp ff-cmp)
  (mb-cmp ff-cmp)
  (ildct-cmp ff-cmp)
  (dia-size :int)
  (last-predictor-count :int)
  (pre-me :int) ;; deprecated
  (me-pre-cmp ff-cmp)
  (pre-dia-size :int)
  (me-subpel-quality :int)
  (dtg-active-format :int) ;; deprecated
  (me-range :int)
  (intra-quant-bias :int) ;; deprecated
  (inter-quant-bias :int) ;; deprecated
  (slice-flags slice-flags)
  (xvmc-acceleration :int) ;; deprecated
  (mb-decision mb-decision)
  (intra-matrix :pointer)
  (inter-matrix :pointer)
  (scenechange-threshold :int) ;; deprecated
  (noise-reduction :int) ;; deprecated
  (me-threshold :int) ;; deprecated
  (mb-threshold :int) ;; deprecated
  (intra-dc-precision :int)
  (skip-top :int)
  (skip-bottom :int)
  (border-masking :float) ;; deprecated
  (mb-lmin :int)
  (mb-lmax :int)
  (mb-penalty-compensation :int) ;; deprecated
  (bidir-refine :int)
  (brd-scale :int) ;; deprecated
  (keyint-min :int)
  (refs :int)
  (chromaoffset :int) ;; deprecated
  (scenechange-factor :int) ;; deprecated
  (mv0-threshold :int)
  (b-sensitivity :int) ;; deprecated
  (color-primaries color-primaries)
  (color-trc color-transfer-characteristic)
  (color-space color-space)
  (color-range color-range)
  (chroma-location chroma-location)
  (slices :int)
  (field-order field-order)
  (sample-rate :int)
  (channels :int)
  (sample-fmt sample-format)
  (frame-size :int)
  (frame-number :int)
  (block-align :int)
  (cutoff :int)
  (channel-layout :uint64)
  (request-channel-layout :uint64)
  (audio-service-type :int)
  (request-sample-format sample-format)
  (get-buffer2 :pointer)
  (refcounted-frames :boolean)
  (qcompress :float)
  (qblur :float)
  (qmin :int)
  (qmax :int)
  (max-qdiff :int)
  (rc-qsquish :float) ;; deprecated
  (rc-qmod-amp :float) ;; deprecated
  (rc-qmod-freq :float) ;; deprecated
  (rc-buffer-size :int)
  (rc-override-count :int)
  (rc-override :pointer)
  (rc-eq :pointer) ;; deprecated
  (rc-max-rate :int64)
  (rc-min-rate :int64)
  (rc-buffer-aggressivity :float) ;; deprecated
  (rc-initial-cplx :float) ;; deprecated
  (rc-max-available-vbv-use :float)
  (rc-min-vbv-overflow-use :float)
  (rc-initial-buffer-occupancy :int)
  (coder-type :int) ;; deprecated
  (context-model :int) ;; deprecated
  (lmin :int) ;; deprecated
  (lmax :int) ;; deprecated
  (frame-skip-threshold :int) ;; deprecated
  (frame-skip-factor :int) ;; deprecated
  (frame-skip-exp :int) ;; deprecated
  (frame-skip-cmp :int) ;; deprecated
  (trellis :int)
  (min-prediction-order :int) ;; deprecated
  (max-prediction-order :int) ;; deprecated
  (timecode-frame-start :int64) ;; deprecated
  (rtp-callback :pointer) ;; deprecated
  (rtp-payload-size :int) ;; deprecated
  (mv-bits :int) ;; deprecated
  (header-bits :int) ;; deprecated
  (i-tex-bits :int) ;; deprecated
  (p-tex-bits :int) ;; deprecated
  (i-count :int) ;; deprecated
  (p-count :int) ;; deprecated
  (skip-count :int) ;; deprecated
  (misc-bits :int) ;; deprecated
  (frame-bits :int) ;; deprecated
  (stats-out :pointer)
  (stats-in :pointer)
  (workaround-bugs :int)
  (struct-std-compliance compliance)
  (error-concealment error-concealment)
  (debug :int) ;; TODO: enum
  (debug-mv :int) ;; TODO: enum
  (err-recognition :int)
  (reordered-opaque :int64)
  (hwaccel :pointer)
  (hwaccel-context :pointer)
  (errors :uint64 :count 8)
  (dct-algo dct-algo)
  (idct-algo idct-algo)
  (bits-per-coded-sample :int)
  (bits-per-raw-sample :int)
  (lowres :int)
  (coded-frame :pointer) ;; deprecated
  (thread-count :int)
  (thread-type thread-type)
  (active-thread-type thread-type)
  (thread-safe-callbacks :boolean)
  (execute :pointer)
  (execute2 :pointer)
  (nsse-weight :int)
  (profile :int)
  (level :int)
  (skip-loop-filter discard)
  (skip-idct discard)
  (skip-frame discard)
  (subtitle-header :pointer)
  (subtitle-header-size :int)
  (error-rate :int) ;; deprecated
  (vbv-delay :uint64) ;; deprecated
  (side-data-only-packets :int) ;; deprecated
  (initial-padding :int)
  (framerate-num :int)
  (framerate-denom :int)
  (sw-pix-fmt pixel-format)
  (pkt-timebase-num :int)
  (pkt-timebase-denom :int)
  (codec-descriptor :pointer)
  (pts-correction-num-faulty-pts :int64)
  (pts-correction-num-faulty-dts :int64)
  (pts-correction-last-pts :int64)
  (pts-correction-last-dts :int64)
  (sub-charenc :pointer)
  (sub-charenc-mode :int)
  (skip-alpha :boolean)
  (seek-preroll :int)
  (chroma-intra-matrix :pointer)
  (dump-separator :pointer)
  (codec-whitelist :pointer)
  (properties :uint)
  (coded-side-data :pointer)
  (nb-coded-side-data :int)
  (hw-frames-ctx :pointer)
  (sub-text-format :int)
  (trailing-padding :int))

(defmacro with-codec-context-slots ((var ctx accessor-name) &body body)
  `(let ((,var (%codec-context-ptr ,ctx)))
     (declare (ignorable ,var))
     (macrolet ((,accessor-name (slot)
                  `(foreign-slot-value ,',var '(:struct av-codec-context) ',slot)))
       ,@body)))

(defcfun (avcodec-alloc-context3 "avcodec_alloc_context3" :library libavcodec)
    :pointer
  (codec :pointer))

(defcfun (avcodec-free-context "avcodec_free_context" :library libavcodec)
    :void
  (pp :pointer))

(defcfun (avcodec-open2 "avcodec_open2" :library libavcodec)
    :int
  (ctx :pointer)
  (codec :pointer)
  (dict :pointer))

(defcfun (avcodec-is-open "avcodec_is_open" :library libavcodec)
    :boolean
  (ctx :pointer))

(defcfun (avcodec-send-packet "avcodec_send_packet" :library libavcodec)
    :int
  (ctx :pointer)
  (packet :pointer))

(defcfun (avcodec-receive-packet "avcodec_receive_packet" :library libavcodec)
    :int
  (ctx :pointer)
  (packet :pointer))

(defcfun (avcodec-send-frame "avcodec_send_frame" :library libavcodec)
    :int
  (ctx :pointer)
  (frame :pointer))

(defcfun (avcodec-receive-frame "avcodec_receive_frame" :library libavcodec)
    :int
  (ctx :pointer)
  (frame :pointer))

(declaim (inline %free-codec-ctx))
(defun %free-codec-ctx (p)
  (declare (type foreign-pointer p))
  (with-foreign-object (pp :pointer)
    (setf (mem-ref pp :pointer) p)
    (avcodec-free-context pp))
  (values))

(defun free-codec-context (ctx)
  (declare (type codec-context ctx))
  (let ((ptr (%codec-context-ptr ctx)))
    (unless (null-pointer-p ptr)
      (%free-codec-ctx ptr)
      (setf (%codec-context-ptr ctx) (null-pointer))
      (cancel-finalization ctx))
    (values)))

(defun codec-context-alive-p (ctx)
  (declare (type codec-context ctx))
  (not (null-pointer-p (%codec-context-ptr ctx))))

(defun codec-context-open-p (ctx)
  (declare (type codec-context ctx))
  (avcodec-is-open (%codec-context-ptr ctx)))

(defun make-codec-context (&optional codec codecpar)
  (declare (type (or null codec) codec)
           (type (or null codec-parameters) codecpar))
  (let ((ptr (avcodec-alloc-context3
              (if codec (%codec-ptr codec) (null-pointer)))))
    (when (null-pointer-p ptr)
      (error 'out-of-memory))
    (let ((ctx (%codec-context ptr)))
      (finalize ctx (lambda () (%free-codec-ctx ptr)))
      (when codecpar
        (parameters-to-context ctx codecpar))
      ctx)))

(defmacro with-codec-context ((var &rest args) &body body)
  `(let ((,var (make-codec-context ,@args)))
     (unwind-protect (locally ,@body)
       (free-codec-context ,var))))

(defun open-codec-context (ctx &optional codec options)
  (declare (type codec-context ctx)
           (type (or null codec) codec)
           (type list options))
  (with-dict (dict options)
    (check-rv (avcodec-open2 (%codec-context-ptr ctx)
                             (if codec (%codec-ptr codec) (null-pointer))
                             dict))
    (from-dict dict)))

(defun send-packet (ctx packet)
  (declare (type codec-context ctx)
           (type packet packet))
  (let ((rv (avcodec-send-packet (%codec-context-ptr ctx)
                                 (%packet-ptr packet))))
    (if (< rv 0)
      (case rv
        ((#.+ffmpeg-eof+ #.+not-available+) nil)
        (t (check-rv rv) nil))
      t)))

(defun send-flush-packet (ctx)
  (declare (type codec-context ctx))
  (let ((rv (avcodec-send-packet (%codec-context-ptr ctx)
                                 (null-pointer))))
    (if (< rv 0)
      (case rv
        ((#.+ffmpeg-eof+ #.+not-available+) nil)
        (t (check-rv rv) nil))
      t)))

(defun receive-packet (ctx packet)
  (declare (type codec-context ctx)
           (type packet packet))
  (let ((rv (avcodec-receive-packet (%codec-context-ptr ctx)
                                    (%packet-ptr packet))))
    (if (< rv 0)
      (case rv
        ((#.+ffmpeg-eof+ #.+not-available+) nil)
        (t (check-rv rv) nil))
      t)))

(defun send-frame (ctx frame)
  (declare (type codec-context ctx)
           (type frame frame))
  (let ((rv (avcodec-send-frame (%codec-context-ptr ctx)
                                (%frame-ptr frame))))
    (if (< rv 0)
      (case rv
        ((#.+ffmpeg-eof+ #.+not-available+) nil)
        (t (check-rv rv) nil))
      t)))

(defun send-flush-frame (ctx)
  (declare (type codec-context ctx))
  (let ((rv (avcodec-send-frame (%codec-context-ptr ctx)
                                (null-pointer))))
    (if (< rv 0)
      (case rv
        ((#.+ffmpeg-eof+ #.+not-available+) nil)
        (t (check-rv rv) nil))
      t)))

(defun receive-frame (ctx frame)
  (declare (type codec-context ctx)
           (type frame frame))
  (let ((rv (avcodec-receive-frame (%codec-context-ptr ctx)
                                   (%frame-ptr frame))))
    (if (< rv 0)
      (case rv
        ((#.+ffmpeg-eof+ #.+not-available+) nil)
        (t (check-rv rv) nil))
      t)))

(macrolet ((defaccessor (name accessor-name value-type value-var read-form &optional write-form)
             `(progn
                (declaim (inline ,name))
                (defun ,name (ctx)
                  (declare (type codec-context ctx))
                  (with-codec-context-slots (ptr ctx ,accessor-name)
                    ,read-form))
                ,@(when write-form
                    `((declaim (inline (setf ,name)))
                      (defun (setf ,name) (,value-var ctx)
                        (declare (type codec-context ctx)
                                 (type ,value-type ,value-var))
                        (with-codec-context-slots (ptr ctx ,accessor-name)
                          ,write-form)
                        ,value-var))))))
  (defaccessor codec-context-codec-id acc keyword value
    (acc codec-id)
    (setf (acc codec-id) value))
  (defaccessor codec-context-codec-type acc keyword value
    (acc codec-type)
    (setf (acc codec-type) value))
  (defaccessor codec-context-codec acc (or codec null) value
    (let ((p (acc codec)))
      (if (null-pointer-p p) nil (%codec p)))
    (let ((p (if value (%codec-ptr value) (null-pointer))))
      (setf (acc codec) p)))
  (defaccessor codec-context-flags acc (or list keyword) value
    (acc flags)
    (setf (acc flags) value))
  (defaccessor codec-context-bit-rate acc int64 value
    (acc bit-rate)
    (setf (acc bit-rate) value))
  (defaccessor codec-context-bit-rate-tolerance acc int32 value
    (acc bit-rate-tolerance)
    (setf (acc bit-rate-tolerance) value))
  (defaccessor codec-context-global-quality acc int32 value
    (acc global-quality)
    (setf (acc global-quality) value))
  (defaccessor codec-context-compression-level acc int32 value
    (acc compression-level)
    (setf (acc compression-level) value))
  (defaccessor codec-context-time-base acc rational value
    (let ((n (acc tb-num))
          (d (acc tb-denom)))
      (if (zerop d) 0 (/ n d)))
    (setf (acc tb-num) (numerator value)
          (acc tb-denom) (denominator value)))
  (defaccessor codec-context-ticks-per-frame acc int value
    (acc ticks-per-frame)
    (setf (acc ticks-per-frame) value))
  (defaccessor codec-context-delay acc int value
    (acc delay))
  (defaccessor codec-context-width acc int value
    (acc width)
    (setf (acc width) value))
  (defaccessor codec-context-height acc int value
    (acc height)
    (setf (acc height) value))
  (defaccessor codec-context-gop-size acc int value
    (acc gop-size)
    (setf (acc gop-size) value))
  (defaccessor codec-context-pixel-format acc keyword value
    (acc pixel-format)
    (setf (acc pixel-format) value))
  (defaccessor codec-context-max-b-frames acc int value
    (acc max-b-frames)
    (setf (acc max-b-frames) value))
  (defaccessor codec-context-has-b-frames-p acc boolean value
    (acc has-b-frames))
  (defaccessor codec-context-slice-count acc int value
    (acc slice-count)
    (setf (acc slice-count) value))
  (defaccessor codec-context-sample-aspect-ratio acc rational value
    (let ((n (acc sar-num))
          (d (acc sar-denom)))
      (if (zerop d) 0 (/ n d)))
    (setf (acc sar-num) (numerator value)
          (acc sar-denom) (denominator value)))
  (defaccessor codec-context-slice-flags acc (or keyword list) value
    (acc slice-flags)
    (setf (acc slice-flags) value))
  (defaccessor codec-context-refs acc int value
    (acc refs)
    (setf (acc refs) value))
  (defaccessor codec-context-color-primaries acc keyword value
    (acc color-primaries)
    (setf (acc color-primaries) value))
  (defaccessor codec-context-color-trc acc keyword value
    (acc color-trc)
    (setf (acc color-trc) value))
  (defaccessor codec-context-color-space acc keyword value
    (acc color-space)
    (setf (acc color-space) value))
  (defaccessor codec-context-color-range acc keyword value
    (acc color-range)
    (setf (acc color-range) value))
  (defaccessor codec-context-chroma-location acc keyword value
    (acc chroma-location)
    (setf (acc chroma-location) value))
  (defaccessor codec-context-slices acc int value
    (acc slices)
    (setf (acc slices) value))
  (defaccessor codec-context-field-order acc keyword value
    (acc field-order)
    (setf (acc field-order) value))
  (defaccessor codec-context-sample-rate acc int value
    (acc sample-rate)
    (setf (acc sample-rate) value))
  (defaccessor codec-context-channels acc int value
    (acc channels)
    (setf (acc channels) value))
  (defaccessor codec-context-sample-format acc keyword value
    (acc sample-fmt)
    (setf (acc sample-fmt) value))
  (defaccessor codec-context-frame-size acc int value
    (acc frame-size))
  (defaccessor codec-context-frame-number acc int value
    (acc frame-number))
  (defaccessor codec-context-channel-layout acc (or list keyword) value
    (to-channel-layout (acc channel-layout))
    (setf (acc channel-layout) (from-channel-layout value)))
  (defaccessor codec-context-req-channel-layout acc (or list keyword) value
    (to-channel-layout (acc request-channel-layout))
    (setf (acc request-channel-layout) (from-channel-layout value)))
  (defaccessor codec-context-qmin acc int value
    (acc qmin)
    (setf (acc qmin) value))
  (defaccessor codec-context-qmax acc int value
    (acc qmax)
    (setf (acc qmax) value))
  (defaccessor codec-context-max-qdiff acc int value
    (acc max-qdiff)
    (setf (acc max-qdiff) value))
  (defaccessor codec-context-min-rate acc int64 value
    (acc rc-min-rate)
    (setf (acc rc-min-rate) value))
  (defaccessor codec-context-max-rate acc int64 value
    (acc rc-max-rate)
    (setf (acc rc-max-rate) value))
  (defaccessor codec-context-bits-per-coded-sample acc int value
    (acc bits-per-coded-sample)
    (setf (acc bits-per-coded-sample) value))
  (defaccessor codec-context-bits-per-raw-sample acc int value
    (acc bits-per-raw-sample)
    (setf (acc bits-per-raw-sample) value))
  (defaccessor codec-context-hwaccel-context acc foreign-pointer value
    (acc hwaccel-context)
    (setf (acc hwaccel-context) value))
  (defaccessor codec-context-thread-count acc int value
    (acc thread-count)
    (setf (acc thread-count) value))
  (defaccessor codec-context-thread-safe-callbacks-p acc boolean value
    (acc thread-safe-callbacks)
    (setf (acc thread-safe-callbacks) value))
  (defaccessor codec-context-profile-id acc int value
    (acc profile)
    (setf (acc profile) value))
  (defaccessor codec-context-level acc int value
    (acc level)
    (setf (acc level) value))
  (defaccessor codec-context-frame-rate acc rational value
    (let ((n (acc framerate-num))
          (d (acc framerate-denom)))
      (if (zerop d) 0 (/ n d)))
    (setf (acc framerate-num) (numerator value)
          (acc framerate-denom) (denominator value)))
  ;; there are many more
  )

(defmethod print-object ((ctx codec-context) stream)
  (print-unreadable-object (ctx stream :type t :identity t)
    (if (codec-context-alive-p ctx)
      (pprint-logical-block (stream nil)
        )
      (format stream "Closed ")))
  ctx)

;; vim: ft=lisp et
