#lang scheme
(require sgl
         sgl/gl
         sgl/gl-vectors
         (planet jaymccarthy/gl-world)
         (planet jaymccarthy/gl2d)
         scheme/runtime-path
         scheme/package)

;; 

;; Display loop
; Constants
(define frames-per-second 1/60)
(define scale-px 20)
(define width-px (* 9 scale-px))
(define height-px (* 16 scale-px))

; Helpers
#;(define (scale-to img 
                  #:width [w (image-width img)]
                  #:height [h (image-height img)])
  (scale/xy (/ w (image-width img))
            (/ h (image-height img))
            img))

#;(define (vertically-scroll img rtop height)
  (define ih (image-height img))
  (define top (modulo rtop ih))
  (define rh (- height top))
  (above
   (crop 0 (- ih top)
         (image-width img) top
         img)
   (above-duplicate img rh)))

#;(define (above-duplicate img h)
  (define ih (image-height img))
  (cond
    [(h . <= . ih)
     (crop 0 0
           (image-width img) h
           img)]
    [else
     (above img
            (above-duplicate img (- h ih)))]))   

(define-runtime-path bk-path "r/mushihimesama-futari/level1_complete.png")
(define background #f)
(define (compute-frame! f)
  (add1 f))

(define (draw-state f)
  (gl-init width-px height-px)
  (gl-viewport/restrict width-px height-px
                        width-px height-px
                        0 0)
  
  (gl-clear-color 1 1 1 1)
  (gl-clear 'color-buffer-bit)
  
  #;(gl-color 0 0 0 1)
  (gl-bind-texture background)
  (gl-draw-rectangle/texture-part 0 (max (/ (- (gl-texture-height background) 
                                               (+ height-px f))
                                            (gl-texture-height background))
                                         0)
                                  (/ width-px (gl-texture-width background))
                                  (/ height-px (gl-texture-height background))
                                  width-px height-px))

(define (draw-init)
  (set! background (gl-load-texture bk-path)))

(big-bang number? 0
          #:height height-px
          #:width width-px
          #:on-tick compute-frame!
          #:tick-rate frames-per-second
          #:on-key (lambda (w ke) w)
          #:draw-init draw-init
          #:on-draw draw-state
          #:stop-when (lambda (w) #f)
          #:stop-timer (lambda (w) #f))

(gl-free-texture background)