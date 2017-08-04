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

(defstruct (packet (:constructor %packet (ptr))
                   (:conc-name %packet-)
                   (:copier nil)
                   (:predicate packetp))
  (ptr (null-pointer) :type foreign-pointer))

(defstruct (frame (:constructor %frame (ptr))
                  (:conc-name %frame-)
                  (:copier nil)
                  (:predicate framep))
  (ptr (null-pointer) :type foreign-pointer))

(defstruct (codec (:constructor %codec (ptr))
                  (:conc-name %codec-)
                  (:copier nul)
                  (:predicate codecp))
  (ptr (null-pointer) :type foreign-pointer))

(defstruct (io-format (:constructor %io-format (ptr input))
                      (:conc-name %io-format-)
                      (:copier nil)
                      (:predicate io-format-p))
  (ptr (null-pointer) :type foreign-pointer)
  (input nil :type boolean))

(defstruct (io-context (:constructor %io-context (ptr token-ptr opened &optional icb buf rcb wcb scb))
                       (:conc-name %io-context-)
                       (:copier nil)
                       (:predicate io-context-p))
  (ptr (null-pointer) :type foreign-pointer)
  (token-ptr (null-pointer) :type foreign-pointer)
  (opened nil :type boolean)
  (icb nil :type (or null (function () t) symbol))
  (buf (make-array 1 :element-type 'uint8) :type (simple-array uint8 (*)))
  (rcb nil :type (or null function symbol))
  (wcb nil :type (or null function symbol))
  (scb nil :type (or null function symbol)))

(defstruct (format-context (:constructor %format-context (ptr input &optional io icb))
                           (:conc-name %format-context-)
                           (:copier nil)
                           (:predicate format-context-p))
  (ptr (null-pointer) :type foreign-pointer)
  input
  (io nil :type (or null io-context))
  (icb nil :type (or null symbol function)))

(defstruct (media-stream (:constructor %media-stream (ptr source))
                         (:conc-name %media-stream-)
                         (:copier nil)
                         (:predicate media-stream-p))
  source
  (ptr (null-pointer) :type foreign-pointer))

(defstruct (codec-parameters (:constructor %codec-parameters (ptr &optional source))
                             (:conc-name %codec-parameters-)
                             (:copier nil)
                             (:predicate codec-parameters-p))
  source
  (ptr (null-pointer) :type foreign-pointer))

(defstruct (codec-context (:constructor %codec-context (ptr))
                          (:conc-name %codec-context-)
                          (:copier nil)
                          (:predicate codec-context-p))
  (ptr (null-pointer) :type foreign-pointer))

(defstruct (resample-context (:constructor %resample-context (ptr))
                             (:conc-name %resample-context-)
                             (:copier nil)
                             (:predicate resample-context-p))
  (ptr (null-pointer) :type foreign-pointer))

(defstruct (scale-context (:constructor %scale-context (ptr))
                          (:conc-name %scale-context)
                          (:copier nil)
                          (:predicate scale-context-p))
  (ptr (null-pointer) :type foreign-pointer))

(defstruct (codec-profile (:constructor %codec-profile (id name))
                               (:conc-name %codec-profile-)
                               (:copier nil)
                               (:predicate codec-profile-p))
  (id 0 :type (signed-byte 32))
  (name "" :type (vector character)))

;; vim: ft=lisp et
