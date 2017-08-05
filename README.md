# Common Lisp Audio Video Engine - Lisp FFmpeg interface

### Work in progress

### Current FFmpeg version - 3.2.4

## Simple example - convert media file to .mp3

```common-lisp

(defun convert-audio-file (infile outfile)
  (declare (type string infile outfile))
  (clave:with-input-context (input infile)
    (clave:find-stream-info input)
    (clave:dump-format input :url infile)
    (multiple-value-bind
          (stream-index codecpar)
        (loop :for i :from 0
              :for stream :in (clave:format-context-streams input)
              :for codecpar = (clave:media-stream-codecpar stream)
              :when (eq :audio (clave:codecpar-codec-type codecpar))
                :do (return (values i codecpar))
              :finally (error "No audio stream found"))
      (let ((decoder (clave:find-decoder (clave:codecpar-codec-id codecpar)))
            (encoder (clave:find-encoder :mp3)))
        (clave:with-codec-context (inctx decoder codecpar)
          (clave:open-codec-context inctx)
          (clave:with-codec-context (outctx encoder)
            (clave:with-codec-context-slots
                (sample-format sample-rate channels channel-layout global-quality) outctx
              (setf sample-format (clave:codecpar-format codecpar)
                    sample-rate (clave:codecpar-sample-rate codecpar)
                    channels (clave:codecpar-channels codecpar)
                    channel-layout (clave:codecpar-channel-layout codecpar)
                    global-quality 1))
            (clave:open-codec-context outctx encoder)
            (clave:with-output-context (output :filename outfile)
              (let* ((outstream (clave:add-media-stream output encoder))
                     (outpar (clave:media-stream-codecpar outstream)))
                (clave:parameters-from-context outpar outctx)
                (clave:write-header output)
                (clave:dump-format output :url outfile)
                (clave:with-packet (packet)
                  (clave:with-frame (frame)
                    (loop :while (clave:read-frame input packet)
                          :when (= stream-index (clave:packet-stream-index packet)) :do
                            (clave:send-packet inctx packet)
                            (loop :while (clave:receive-frame inctx frame) :do
                              (clave:send-frame outctx frame)
                              (loop :while (clave:receive-packet outctx packet) :do
                                (clave:write-frame output packet))))
                    (clave:send-flush-frame outctx)
                    (loop :while (clave:receive-packet outctx packet) :do
                      (clave:write-frame output packet))
                    (clave:write-trailer output)))))))))))

```

## TODO

* swresample & swscale
* Missing codec-context/format-context/media-stream slots
* Documentation
* Math & other utils
