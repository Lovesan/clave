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

(defcenum pixel-format
  (:NONE -1)
  :YUV420P   ;;;< planar YUV 4:2:0, 12bpp, (1 Cr & Cb sample per 2x2 Y samples)
  :YUYV422   ;;;< packed YUV 4:2:2, 16bpp, Y0 Cb Y1 Cr
  :RGB24     ;;;< packed RGB 8:8:8, 24bpp, RGBRGB...
  :BGR24     ;;;< packed RGB 8:8:8, 24bpp, BGRBGR...
  :YUV422P   ;;;< planar YUV 4:2:2, 16bpp, (1 Cr & Cb sample per 2x1 Y samples)
  :YUV444P   ;;;< planar YUV 4:4:4, 24bpp, (1 Cr & Cb sample per 1x1 Y samples)
  :YUV410P   ;;;< planar YUV 4:1:0,  9bpp, (1 Cr & Cb sample per 4x4 Y samples)
  :YUV411P   ;;;< planar YUV 4:1:1, 12bpp, (1 Cr & Cb sample per 4x1 Y samples)
  :GRAY8     ;;;<        Y        ,  8bpp
  :MONOWHITE ;;;<        Y        ,  1bpp, 0 is white, 1 is black, in each byte pixels are ordered from the msb to the lsb
  :MONOBLACK ;;;<        Y        ,  1bpp, 0 is black, 1 is white, in each byte pixels are ordered from the msb to the lsb
  :PAL8      ;;;< 8 bits with :RGB32 palette
  :YUVJ420P  ;;;< planar YUV 4:2:0, 12bpp, full scale (JPEG), deprecated in favor of :YUV420P and setting color_range
  :YUVJ422P  ;;;< planar YUV 4:2:2, 16bpp, full scale (JPEG), deprecated in favor of :YUV422P and setting color_range
  :YUVJ444P   ;;;< planar YUV 4:4:4, 24bpp, full scale (JPEG), deprecated in favor of :YUV444P and setting color_range
  :XVMC-MPEG2-MC ;;;< XVideo Motion Acceleration via common packet passing
  :XVMC-MPEG2-IDCT
  (:XVMC 16)
  :UYVY422   ;;;< packed YUV 4:2:2, 16bpp, Cb Y0 Cr Y1
  :UYYVYY411 ;;;< packed YUV 4:1:1, 12bpp, Cb Y0 Y1 Cr Y2 Y3
  :BGR8      ;;;< packed RGB 3:3:2,  8bpp, (msb)2B 3G 3R(lsb)
  :BGR4      ;;;< packed RGB 1:2:1 bitstream,  4bpp, (msb)1B 2G 1R(lsb), a byte contains two pixels, the first pixel in the byte is the one composed by the 4 msb bits
  :BGR4-BYTE ;;;< packed RGB 1:2:1,  8bpp, (msb)1B 2G 1R(lsb)
  :RGB8      ;;;< packed RGB 3:3:2,  8bpp, (msb)2R 3G 3B(lsb)
  :RGB4      ;;;< packed RGB 1:2:1 bitstream,  4bpp, (msb)1R 2G 1B(lsb), a byte contains two pixels, the first pixel in the byte is the one composed by the 4 msb bits
  :RGB4-BYTE ;;;< packed RGB 1:2:1,  8bpp, (msb)1R 2G 1B(lsb)
  :NV12      ;;;< planar YUV 4:2:0, 12bpp, 1 plane for Y and 1 plane for the UV components, which are interleaved (first byte U and the following byte V)
  :NV21      ;;;< as above, but U and V bytes are swapped
  
  :ARGB      ;;;< packed ARGB 8:8:8:8, 32bpp, ARGBARGB...
  :RGBA      ;;;< packed RGBA 8:8:8:8, 32bpp, RGBARGBA...
  :ABGR      ;;;< packed ABGR 8:8:8:8, 32bpp, ABGRABGR...
  :BGRA      ;;;< packed BGRA 8:8:8:8, 32bpp, BGRABGRA...
  
  :GRAY16BE  ;;;<        Y        , 16bpp, big-endian
  :GRAY16LE  ;;;<        Y        , 16bpp, little-endian
  :YUV440P   ;;;< planar YUV 4:4:0 (1 Cr & Cb sample per 1x2 Y samples)
  :YUVJ440P  ;;;< planar YUV 4:4:0 full scale (JPEG), deprecated in favor of :YUV440P and setting color_range
  :YUVA420P  ;;;< planar YUV 4:2:0, 20bpp, (1 Cr & Cb sample per 2x2 Y & A samples)
  :VDPAU-H264 ;;;< H.264 HW decoding with VDPAU, data[0] contains a vdpau_render_state struct which contains the bitstream of the slices as well as various fields extracted from headers
  :VDPAU-MPEG1 ;;;< MPEG-1 HW decoding with VDPAU, data[0] contains a vdpau_render_state struct which contains the bitstream of the slices as well as various fields extracted from headers
  :VDPAU-MPEG2 ;;;< MPEG-2 HW decoding with VDPAU, data[0] contains a vdpau_render_state struct which contains the bitstream of the slices as well as various fields extracted from headers
  :VDPAU-WMV3 ;;;< WMV3 HW decoding with VDPAU, data[0] contains a vdpau_render_state struct which contains the bitstream of the slices as well as various fields extracted from headers
  :VDPAU-VC1 ;;;< VC-1 HW decoding with VDPAU, data[0] contains a vdpau_render_state struct which contains the bitstream of the slices as well as various fields extracted from headers
  :RGB48BE   ;;;< packed RGB 16:16:16, 48bpp, 16R, 16G, 16B, the 2-byte value for each R/G/B component is stored as big-endian
  :RGB48LE   ;;;< packed RGB 16:16:16, 48bpp, 16R, 16G, 16B, the 2-byte value for each R/G/B component is stored as little-endian
  
  :RGB565BE  ;;;< packed RGB 5:6:5, 16bpp, (msb)   5R 6G 5B(lsb), big-endian
  :RGB565LE  ;;;< packed RGB 5:6:5, 16bpp, (msb)   5R 6G 5B(lsb), little-endian
  :RGB555BE  ;;;< packed RGB 5:5:5, 16bpp, (msb)1X 5R 5G 5B(lsb), big-endian   , X=unused/undefined
  :RGB555LE  ;;;< packed RGB 5:5:5, 16bpp, (msb)1X 5R 5G 5B(lsb), little-endian, X=unused/undefined
  
  :BGR565BE  ;;;< packed BGR 5:6:5, 16bpp, (msb)   5B 6G 5R(lsb), big-endian
  :BGR565LE  ;;;< packed BGR 5:6:5, 16bpp, (msb)   5B 6G 5R(lsb), little-endian
  :BGR555BE  ;;;< packed BGR 5:5:5, 16bpp, (msb)1X 5B 5G 5R(lsb), big-endian   , X=unused/undefined
  :BGR555LE  ;;;< packed BGR 5:5:5, 16bpp, (msb)1X 5B 5G 5R(lsb), little-endian, X=unused/undefined
  
  :VAAPI-MOCO ;;;< HW acceleration through VA API at motion compensation entry-point, Picture.data[3] contains a vaapi_render_state struct which contains macroblocks as well as various fields extracted from headers
  :VAAPI-IDCT ;;;< HW acceleration through VA API at IDCT entry-point, Picture.data[3] contains a vaapi_render_state struct which contains fields extracted from headers
  :VAAPI-VLD  ;;;< HW decoding through VA API, Picture.data[3] contains a VASurfaceID
  (:VAAPI 53)
  
  :YUV420P16LE  ;;;< planar YUV 4:2:0, 24bpp, (1 Cr & Cb sample per 2x2 Y samples), little-endian
  :YUV420P16BE  ;;;< planar YUV 4:2:0, 24bpp, (1 Cr & Cb sample per 2x2 Y samples), big-endian
  :YUV422P16LE  ;;;< planar YUV 4:2:2, 32bpp, (1 Cr & Cb sample per 2x1 Y samples), little-endian
  :YUV422P16BE  ;;;< planar YUV 4:2:2, 32bpp, (1 Cr & Cb sample per 2x1 Y samples), big-endian
  :YUV444P16LE  ;;;< planar YUV 4:4:4, 48bpp, (1 Cr & Cb sample per 1x1 Y samples), little-endian
  :YUV444P16BE  ;;;< planar YUV 4:4:4, 48bpp, (1 Cr & Cb sample per 1x1 Y samples), big-endian
  :VDPAU-MPEG4  ;;;< MPEG-4 HW decoding with VDPAU, data[0] contains a vdpau_render_state struct which contains the bitstream of the slices as well as various fields extracted from headers
  :DXVA2-VLD    ;;;< HW decoding through DXVA2, Picture.data[3] contains a LPDIRECT3DSURFACE9 pointer
  
  :RGB444LE  ;;;< packed RGB 4:4:4, 16bpp, (msb)4X 4R 4G 4B(lsb), little-endian, X=unused/undefined
  :RGB444BE  ;;;< packed RGB 4:4:4, 16bpp, (msb)4X 4R 4G 4B(lsb), big-endian,    X=unused/undefined
  :BGR444LE  ;;;< packed BGR 4:4:4, 16bpp, (msb)4X 4B 4G 4R(lsb), little-endian, X=unused/undefined
  :BGR444BE  ;;;< packed BGR 4:4:4, 16bpp, (msb)4X 4B 4G 4R(lsb), big-endian,    X=unused/undefined
  :YA8       ;;;< 8 bits gray, 8 bits alpha
  
  (:Y400A 66) ;;;< alias for :YA8
  (:GRAY8A 66) ;;;< alias for :YA8
  
  :BGR48BE   ;;;< packed RGB 16:16:16, 48bpp, 16B, 16G, 16R, the 2-byte value for each R/G/B component is stored as big-endian
  :BGR48LE   ;;;< packed RGB 16:16:16, 48bpp, 16B, 16G, 16R, the 2-byte value for each R/G/B component is stored as little-endian
  
  ;;
  ;; The following 12 formats have the disadvantage of needing 1 format for each bit depth.
  ;; Notice that each 9/10 bits sample is stored in 16 bits with extra padding.
  ;; If you want to support multiple bit depths, then using :YUV420P16* with the bpp stored separately is better.
  ;;
  :YUV420P9BE ;;;< planar YUV 4:2:0, 13.5bpp, (1 Cr & Cb sample per 2x2 Y samples), big-endian
  :YUV420P9LE  ;;;< planar YUV 4:2:0, 13.5bpp, (1 Cr & Cb sample per 2x2 Y samples), little-endian
  :YUV420P10BE ;;;< planar YUV 4:2:0, 15bpp, (1 Cr & Cb sample per 2x2 Y samples), big-endian
  :YUV420P10LE ;;;< planar YUV 4:2:0, 15bpp, (1 Cr & Cb sample per 2x2 Y samples), little-endian
  :YUV422P10BE ;;;< planar YUV 4:2:2, 20bpp, (1 Cr & Cb sample per 2x1 Y samples), big-endian
  :YUV422P10LE ;;;< planar YUV 4:2:2, 20bpp, (1 Cr & Cb sample per 2x1 Y samples), little-endian
  :YUV444P9BE  ;;;< planar YUV 4:4:4, 27bpp, (1 Cr & Cb sample per 1x1 Y samples), big-endian
  :YUV444P9LE ;;;< planar YUV 4:4:4, 27bpp, (1 Cr & Cb sample per 1x1 Y samples), little-endian
  :YUV444P10BE ;;;< planar YUV 4:4:4, 30bpp, (1 Cr & Cb sample per 1x1 Y samples), big-endian
  :YUV444P10LE ;;;< planar YUV 4:4:4, 30bpp, (1 Cr & Cb sample per 1x1 Y samples), little-endian
  :YUV422P9BE  ;;;< planar YUV 4:2:2, 18bpp, (1 Cr & Cb sample per 2x1 Y samples), big-endian
  :YUV422P9LE  ;;;< planar YUV 4:2:2, 18bpp, (1 Cr & Cb sample per 2x1 Y samples), little-endian
  :VDA-VLD    ;;;< hardware decoding through VDA
  :GBRP      ;;;< planar GBR 4:4:4 24bpp
  (:GBR24P 82) ;;; alias for #:GBRP
  :GBRP9BE   ;;;< planar GBR 4:4:4 27bpp, big-endian
  :GBRP9LE   ;;;< planar GBR 4:4:4 27bpp, little-endian
  :GBRP10BE  ;;;< planar GBR 4:4:4 30bpp, big-endian
  :GBRP10LE  ;;;< planar GBR 4:4:4 30bpp, little-endian
  :GBRP16BE  ;;;< planar GBR 4:4:4 48bpp, big-endian
  :GBRP16LE  ;;;< planar GBR 4:4:4 48bpp, little-endian
  :YUVA422P  ;;;< planar YUV 4:2:2 24bpp, (1 Cr & Cb sample per 2x1 Y & A samples)
  :YUVA444P  ;;;< planar YUV 4:4:4 32bpp, (1 Cr & Cb sample per 1x1 Y & A samples)
  :YUVA420P9BE  ;;;< planar YUV 4:2:0 22.5bpp, (1 Cr & Cb sample per 2x2 Y & A samples), big-endian
  :YUVA420P9LE  ;;;< planar YUV 4:2:0 22.5bpp, (1 Cr & Cb sample per 2x2 Y & A samples), little-endian
  :YUVA422P9BE  ;;;< planar YUV 4:2:2 27bpp, (1 Cr & Cb sample per 2x1 Y & A samples), big-endian
  :YUVA422P9LE  ;;;< planar YUV 4:2:2 27bpp, (1 Cr & Cb sample per 2x1 Y & A samples), little-endian
  :YUVA444P9BE  ;;;< planar YUV 4:4:4 36bpp, (1 Cr & Cb sample per 1x1 Y & A samples), big-endian
  :YUVA444P9LE  ;;;< planar YUV 4:4:4 36bpp, (1 Cr & Cb sample per 1x1 Y & A samples), little-endian
  :YUVA420P10BE ;;;< planar YUV 4:2:0 25bpp, (1 Cr & Cb sample per 2x2 Y & A samples, big-endian)
  :YUVA420P10LE ;;;< planar YUV 4:2:0 25bpp, (1 Cr & Cb sample per 2x2 Y & A samples, little-endian)
  :YUVA422P10BE ;;;< planar YUV 4:2:2 30bpp, (1 Cr & Cb sample per 2x1 Y & A samples, big-endian)
  :YUVA422P10LE ;;;< planar YUV 4:2:2 30bpp, (1 Cr & Cb sample per 2x1 Y & A samples, little-endian)
  :YUVA444P10BE ;;;< planar YUV 4:4:4 40bpp, (1 Cr & Cb sample per 1x1 Y & A samples, big-endian)
  :YUVA444P10LE ;;;< planar YUV 4:4:4 40bpp, (1 Cr & Cb sample per 1x1 Y & A samples, little-endian)
  :YUVA420P16BE ;;;< planar YUV 4:2:0 40bpp, (1 Cr & Cb sample per 2x2 Y & A samples, big-endian)
  :YUVA420P16LE ;;;< planar YUV 4:2:0 40bpp, (1 Cr & Cb sample per 2x2 Y & A samples, little-endian)
  :YUVA422P16BE ;;;< planar YUV 4:2:2 48bpp, (1 Cr & Cb sample per 2x1 Y & A samples, big-endian)
  :YUVA422P16LE ;;;< planar YUV 4:2:2 48bpp, (1 Cr & Cb sample per 2x1 Y & A samples, little-endian)
  :YUVA444P16BE ;;;< planar YUV 4:4:4 64bpp, (1 Cr & Cb sample per 1x1 Y & A samples, big-endian)
  :YUVA444P16LE ;;;< planar YUV 4:4:4 64bpp, (1 Cr & Cb sample per 1x1 Y & A samples, little-endian)
  
  :VDPAU     ;;;< HW acceleration through VDPAU, Picture.data[3] contains a VdpVideoSurface
  
  :XYZ12LE      ;;;< packed XYZ 4:4:4, 36 bpp, (msb) 12X, 12Y, 12Z (lsb), the 2-byte value for each X/Y/Z is stored as little-endian, the 4 lower bits are set to 0
  :XYZ12BE      ;;;< packed XYZ 4:4:4, 36 bpp, (msb) 12X, 12Y, 12Z (lsb), the 2-byte value for each X/Y/Z is stored as big-endian, the 4 lower bits are set to 0
  :NV16         ;;;< interleaved chroma YUV 4:2:2, 16bpp, (1 Cr & Cb sample per 2x1 Y samples)
  :NV20LE       ;;;< interleaved chroma YUV 4:2:2, 20bpp, (1 Cr & Cb sample per 2x1 Y samples), little-endian
  :NV20BE       ;;;< interleaved chroma YUV 4:2:2, 20bpp, (1 Cr & Cb sample per 2x1 Y samples), big-endian
  
  :RGBA64BE     ;;;< packed RGBA 16:16:16:16, 64bpp, 16R, 16G, 16B, 16A, the 2-byte value for each R/G/B/A component is stored as big-endian
  :RGBA64LE     ;;;< packed RGBA 16:16:16:16, 64bpp, 16R, 16G, 16B, 16A, the 2-byte value for each R/G/B/A component is stored as little-endian
  :BGRA64BE     ;;;< packed RGBA 16:16:16:16, 64bpp, 16B, 16G, 16R, 16A, the 2-byte value for each R/G/B/A component is stored as big-endian
  :BGRA64LE     ;;;< packed RGBA 16:16:16:16, 64bpp, 16B, 16G, 16R, 16A, the 2-byte value for each R/G/B/A component is stored as little-endian
  
  :YVYU422   ;;;< packed YUV 4:2:2, 16bpp, Y0 Cr Y1 Cb
  
  :VDA          ;;;< HW acceleration through VDA, data[3] contains a CVPixelBufferRef
  
  :YA16BE       ;;;< 16 bits gray, 16 bits alpha (big-endian)
  :YA16LE       ;;;< 16 bits gray, 16 bits alpha (little-endian)
  
  :GBRAP        ;;;< planar GBRA 4:4:4:4 32bpp
  :GBRAP16BE    ;;;< planar GBRA 4:4:4:4 64bpp, big-endian
  :GBRAP16LE    ;;;< planar GBRA 4:4:4:4 64bpp, little-endian

  ;;
  ;; HW acceleration through QSV, data[3] contains a pointer to the
  ;; mfxFrameSurface1 structure.
  ;;
  :QSV
  ;;
  ;; HW acceleration though MMAL, data[3] contains a pointer to the
  ;; MMAL_BUFFER_HEADER_T structure.
  ;;
  :MMAL
  
  :D3D11VA-VLD  ;;;< HW decoding through Direct3D11, Picture.data[3] contains a ID3D11VideoDecoderOutputView pointer
  
  ;;
  ;; HW acceleration through CUDA. data[i] contain CUdeviceptr pointers
  ;; exactly as for system memory frames.
  ;;
  :CUDA
  
  (:0RGB 295)    ;;;< packed RGB 8:8:8, 32bpp, XRGBXRGB...   X=unused/undefined
  :RGB0        ;;;< packed RGB 8:8:8, 32bpp, RGBXRGBX...   X=unused/undefined
  :0BGR        ;;;< packed BGR 8:8:8, 32bpp, XBGRXBGR...   X=unused/undefined
  :BGR0        ;;;< packed BGR 8:8:8, 32bpp, BGRXBGRX...   X=unused/undefined
  
  :YUV420P12BE ;;;< planar YUV 4:2:0,18bpp, (1 Cr & Cb sample per 2x2 Y samples), big-endian
  :YUV420P12LE ;;;< planar YUV 4:2:0,18bpp, (1 Cr & Cb sample per 2x2 Y samples), little-endian
  :YUV420P14BE ;;;< planar YUV 4:2:0,21bpp, (1 Cr & Cb sample per 2x2 Y samples), big-endian
  :YUV420P14LE ;;;< planar YUV 4:2:0,21bpp, (1 Cr & Cb sample per 2x2 Y samples), little-endian
  :YUV422P12BE ;;;< planar YUV 4:2:2,24bpp, (1 Cr & Cb sample per 2x1 Y samples), big-endian
  :YUV422P12LE ;;;< planar YUV 4:2:2,24bpp, (1 Cr & Cb sample per 2x1 Y samples), little-endian
  :YUV422P14BE ;;;< planar YUV 4:2:2,28bpp, (1 Cr & Cb sample per 2x1 Y samples), big-endian
  :YUV422P14LE ;;;< planar YUV 4:2:2,28bpp, (1 Cr & Cb sample per 2x1 Y samples), little-endian
  :YUV444P12BE ;;;< planar YUV 4:4:4,36bpp, (1 Cr & Cb sample per 1x1 Y samples), big-endian
  :YUV444P12LE ;;;< planar YUV 4:4:4,36bpp, (1 Cr & Cb sample per 1x1 Y samples), little-endian
  :YUV444P14BE ;;;< planar YUV 4:4:4,42bpp, (1 Cr & Cb sample per 1x1 Y samples), big-endian
  :YUV444P14LE ;;;< planar YUV 4:4:4,42bpp, (1 Cr & Cb sample per 1x1 Y samples), little-endian
  :GBRP12BE    ;;;< planar GBR 4:4:4 36bpp, big-endian
  :GBRP12LE    ;;;< planar GBR 4:4:4 36bpp, little-endian
  :GBRP14BE    ;;;< planar GBR 4:4:4 42bpp, big-endian
  :GBRP14LE    ;;;< planar GBR 4:4:4 42bpp, little-endian
  :YUVJ411P    ;;;< planar YUV 4:1:1, 12bpp, (1 Cr & Cb sample per 4x1 Y samples) full scale (JPEG), deprecated in favor of :YUV411P and setting color_range
  
  :BAYER-BGGR8    ;;;< bayer, BGBG..(odd line), GRGR..(even line), 8-bit samples */
  :BAYER-RGGB8    ;;;< bayer, RGRG..(odd line), GBGB..(even line), 8-bit samples */
  :BAYER-GBRG8    ;;;< bayer, GBGB..(odd line), RGRG..(even line), 8-bit samples */
  :BAYER-GRBG8    ;;;< bayer, GRGR..(odd line), BGBG..(even line), 8-bit samples */
  :BAYER-BGGR16LE ;;;< bayer, BGBG..(odd line), GRGR..(even line), 16-bit samples, little-endian */
  :BAYER-BGGR16BE ;;;< bayer, BGBG..(odd line), GRGR..(even line), 16-bit samples, big-endian */
  :BAYER-RGGB16LE ;;;< bayer, RGRG..(odd line), GBGB..(even line), 16-bit samples, little-endian */
  :BAYER-RGGB16BE ;;;< bayer, RGRG..(odd line), GBGB..(even line), 16-bit samples, big-endian */
  :BAYER-GBRG16LE ;;;< bayer, GBGB..(odd line), RGRG..(even line), 16-bit samples, little-endian */
  :BAYER-GBRG16BE ;;;< bayer, GBGB..(odd line), RGRG..(even line), 16-bit samples, big-endian */
  :BAYER-GRBG16LE ;;;< bayer, GRGR..(odd line), BGBG..(even line), 16-bit samples, little-endian */
  :BAYER-GRBG16BE ;;;< bayer, GRGR..(odd line), BGBG..(even line), 16-bit samples, big-endian */
  :YUV440P10LE ;;;< planar YUV 4:4:0,20bpp, (1 Cr & Cb sample per 1x2 Y samples), little-endian
  :YUV440P10BE ;;;< planar YUV 4:4:0,20bpp, (1 Cr & Cb sample per 1x2 Y samples), big-endian
  :YUV440P12LE ;;;< planar YUV 4:4:0,24bpp, (1 Cr & Cb sample per 1x2 Y samples), little-endian
  :YUV440P12BE ;;;< planar YUV 4:4:0,24bpp, (1 Cr & Cb sample per 1x2 Y samples), big-endian
  :AYUV64LE    ;;;< packed AYUV 4:4:4,64bpp (1 Cr & Cb sample per 1x1 Y & A samples), little-endian
  :AYUV64BE    ;;;< packed AYUV 4:4:4,64bpp (1 Cr & Cb sample per 1x1 Y & A samples), big-endian
  
  :VIDEOTOOLBOX ;;;< hardware decoding through Videotoolbox
  
  :P010LE ;;;< like NV12, with 10bpp per component, data in the high bits, zeros in the low bits, little-endian
  :P010BE ;;;< like NV12, with 10bpp per component, data in the high bits, zeros in the low bits, big-endian
  
  :GBRAP12BE  ;;;< planar GBR 4:4:4:4 48bpp, big-endian
  :GBRAP12LE  ;;;< planar GBR 4:4:4:4 48bpp, little-endian
  
  :GBRAP10BE  ;;;< planar GBR 4:4:4:4 40bpp, big-endian
  :GBRAP10LE  ;;;< planar GBR 4:4:4:4 40bpp, little-endian
  
  :MEDIACODEC ;;;< hardware decoding through MediaCodec
  )

;; vim: ft=lisp et
