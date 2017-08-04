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

(in-package :cl-user)

(uiop:define-package #:clave
  (:use #:cl #:cffi #:trivial-garbage)
  (:export

   ;; enums
   
   #:codec-id
   #:pixel-format
   #:sample-format
   #:channels
   #:channel-layout
   #:media-type
   #:format-flags
   #:codec-flags
   #:format-context-flags
   #:codec-capabilities
   #:scale-flags
   #:package-flags
   #:frame-flags
   #:color-range
   #:color-space
   #:color-transfer-characteristic
   #:color-primaries
   #:chroma-location
   #:decode-error-flags
   #:field-order
   #:disposition
   #:discard
   #:stream-parse-type
   #:avoid-negative-ts
   #:duration-estimation-method
   #:compliance

   ;; utils
   
   #:get-channel-layout
   #:get-channel-layout-string
   #:get-channel-layout-channels
   #:get-default-channel-layout

   #:+no-pts+

   ;; log
   #:log-level
   #:set-log-callback
   #:set-log-level
   #:restore-log-callback

   ;; format

   #:io-format
   #:io-format-p
   #:io-format-input-p
   #:find-input-format
   #:guess-output-format
   #:io-format-name
   #:io-format-long-name
   #:io-format-mime-types
   #:io-format-flags
   #:io-format-extensions
   #:io-format-audio-codec
   #:io-format-video-codec
   #:io-format-subtitle-codec

   ;; codec

   #:codec
   #:codecp
   #:codec-name
   #:codec-long-name
   #:codec-type
   #:codec-id
   #:codec-capabilities
   #:codec-framerates
   #:codec-pixel-formats
   #:codec-samplerates
   #:codec-sample-formats
   #:codec-channel-layouts
   #:codec-max-lowres
   #:codec-profiles
   #:codec-profile
   #:codec-profile-p
   #:codec-profile-name
   #:codec-profile-id
   #:find-decoder
   #:find-encoder
   #:find-decoder-by-name
   #:find-encoder-by-name

   ;; packet

   #:packet
   #:packetp
   #:make-packet
   #:clone-packet
   #:packet-alive-p
   #:free-packet
   #:with-packet
   #:shrink-packet
   #:grow-packet
   #:packet-rescale-ts
   #:packet-data
   #:packet-pts
   #:packet-dts
   #:packet-size
   #:packet-flags
   #:packet-duration
   #:packet-position
   #:packet-stream-index

   ;; frame

   #:frame
   #:framep
   #:make-frame
   #:free-frame
   #:with-frame
   #:clone-frame
   #:frame-alive-p
   #:frame-writable-p
   #:frame-make-writable
   #:frame-data
   #:frame-data-pointer
   #:frame-linesize
   #:frame-linesize-pointer
   #:frame-extended-data
   #:frame-width
   #:frame-height
   #:frame-samples
   #:frame-format
   #:frame-sample-rate
   #:frame-key-p
   #:frame-picture-type
   #:frame-sample-aspect-ratio
   #:frame-pts
   #:frame-packet-dts
   #:frame-coded-picture-number
   #:frame-display-picture-number
   #:frame-quality
   #:frame-opaque
   #:frame-repeat-picture
   #:frame-interlaced-p
   #:frame-top-field-first-p
   #:frame-palette-changed-p
   #:frame-reordered-opaque
   #:frame-channel-layout
   #:frame-flags
   #:frame-color-range
   #:frame-color-primaries
   #:frame-color-trc
   #:frame-colorspace
   #:frame-chroma-location
   #:frame-best-effort-timestamp
   #:frame-packet-position
   #:frame-packet-duration
   #:frame-decode-error-flags
   #:frame-channels
   #:frame-packet-size
   #:+frame-data-pointers+

   ;; codec parameters
   #:codec-parameters
   #:codec-parameters-p
   #:make-codec-parameters
   #:parameters-to-context
   #:parameters-from-context
   #:codecpar-codec-type
   #:codecpar-codec-id
   #:codecpar-bit-rate
   #:codecpar-bits-per-coded-sample
   #:codecpar-bits-per-raw-sample
   #:codecpar-profile-id
   #:codecpar-level
   #:codecpar-width
   #:codecpar-height
   #:codecpar-field-order
   #:codecpar-color-range
   #:codecpar-color-primaries
   #:codecpar-color-trc
   #:codecpar-color-space
   #:codecpar-chroma-location
   #:codecpar-video-delay
   #:codecpar-sample-rate
   #:codecpar-block-align
   #:codecpar-frame-size
   #:codecpar-initial-padding
   #:codecpar-seek-preroll
   #:codecpar-format
   #:codecpar-sample-aspect-ratio
   #:codecpar-channel-layout
   #:codecpar-channels

   ;; media-stream

   #:media-stream
   #:media-stream-p
   #:media-stream-id
   #:media-stream-index
   #:media-stream-codecpar
   #:media-stream-sample-aspect-ratio
   #:media-stream-disposition
   #:media-stream-time-base
   #:media-stream-avg-frame-rate

   ;; io-context
   #:io-context
   #:io-context-p
   #:io-context-alive-p
   #:make-io-context
   #:open-io-context
   #:free-io-context
   #:with-io-context

   ;; format-context

   #:format-context
   #:format-context-p
   #:format-context-input-p
   #:format-context-alive-p
   #:free-format-context
   #:open-input-context
   #:make-output-context
   #:create-sdp
   #:with-input-context
   #:with-output-context
   #:format-context-streams
   #:format-context-format
   #:format-context-filename
   #:format-context-flags
   #:format-context-format-flags
   #:add-media-stream
   #:find-stream-info
   #:dump-format
   #:init-output
   #:write-header
   #:write-trailer
   #:read-frame
   #:write-frame
   #:interleaved-write-frame

   ;; codec-context

   #:codec-context
   #:codec-context-p
   #:codec-context-alive-p
   #:codec-context-open-p
   #:make-codec-context
   #:free-codec-context
   #:with-codec-context
   #:open-codec-context
   #:send-packet
   #:receive-packet
   #:send-flush-packet
   #:send-frame
   #:receive-frame
   #:send-flush-frame
   #:codec-context-codec-id
   #:codec-context-codec-type
   #:codec-context-codec
   #:codec-context-flags
   #:codec-context-bit-rate
   #:codec-context-bit-rate-tolerance
   #:codec-context-global-quality
   #:codec-context-compression-level
   #:codec-context-time-base
   #:codec-context-ticks-per-frame
   #:codec-context-delay
   #:codec-context-width
   #:codec-context-height
   #:codec-context-gop-size
   #:codec-context-pixel-format
   #:codec-context-max-b-frames
   #:codec-context-has-b-frames-p
   #:codec-context-slice-count
   #:codec-context-sample-aspect-ratio
   #:codec-context-slice-flags
   #:codec-context-color-primaries
   #:codec-context-color-trc
   #:codec-context-color-space
   #:codec-context-range
   #:codec-context-chroma-location
   #:codec-context-slices
   #:codec-context-field-order
   #:codec-context-sample-rate
   #:codec-context-channels
   #:codec-context-channel-layout
   #:codec-context-req-channel-layout
   #:codec-context-sample-format
   #:codec-context-frame-size
   #:codec-context-frame-number
   #:codec-context-qmin
   #:codec-context-qmax
   #:codec-context-max-qdiff
   #:codec-context-min-rate
   #:codec-context-max-rate
   #:codec-context-bits-per-coded-sample
   #:codec-context-bits-per-raw-sample
   #:codec-context-hwaccel-context
   #:codec-context-thread-count
   #:codec-context-thread-safe-callbacks-p
   #:codec-context-profile-id
   #:codec-context-level
   #:codec-context-frame-rate
   ))

;; vim: ft=lisp et
