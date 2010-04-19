#lang scheme
(require sgl
         sgl/gl
         sgl/gl-vectors
         (planet jaymccarthy/gl-world)
         (planet jaymccarthy/gl2d)
         (planet jaymccarthy/bulletml/lib)
         (prefix-in col: "collision.ss"))

(define filename "/Users/jay/Dev/svn/byu-plt/trunk/planet/bulletml/examples/[Psyvariar]_X-A_boss_winder.xml")

(define ship-rad 12.0)
(define speed 20.0)
(define bullet-rad (/ ship-rad 4.0))
(define screen-scale 25.0)
(define screen-width (* screen-scale 16))
(define screen-height (* screen-scale 9))

(define bml (compile-bulletml (parse-file filename)))
(define bullet-sim (load-bml screen-width screen-height bml))

(define scale 2)

(define dead? #f)
(define (collide! b1 b2)
  (printf "Collide! ~S ~S~n" b1 b2)
  (set! dead? #t))

(define tick-rate (exact->inexact 1/30))

(define-struct screen (time))
(define initial-screen (make-screen 0))

(define (collision-bullets bullet-sim)
  (cons (col:make-body 'ship (bullets-target-posn bullet-sim) ship-rad)
        (map (lambda (b)
               (col:make-body 'bullet (body-posn b) bullet-rad))
             (bullets-bs bullet-sim))))

(define frames 0)
(define start-time (current-seconds))
(define (current-fps)
  (define time-since (- (current-seconds) start-time))
  (cond
    [(zero? time-since)
     tick-rate]
    [(zero? frames)
     tick-rate]
    [else
     (/ time-since frames)]))

(define step-simulation
  (match-lambda
    [(struct screen (time))
     (define time-step tick-rate)
     (set! frames (add1 frames))
     
     (step! bullet-sim)
     
     (col:find-collisions! collide! (collision-bullets bullet-sim))
     
     (make-screen (+ time time-step))]))

(define (ship-steer s k)
  (match s
    [(struct screen (time))
     (define speed-adj (* speed 2 tick-rate))
     (define adjustment
       (match (send k get-key-code)
         ['up (make-cartesian-posn 0.0 speed-adj)]
         ['down (make-cartesian-posn 0.0 (* -1 speed-adj))]
         ['right (make-cartesian-posn speed-adj 0.0)]
         ['left (make-cartesian-posn (* -1 speed-adj) 0.0)]
         [_ (make-cartesian-posn 0.0 0.0)]))
     
     (set-bullets-target-posn! bullet-sim (posn+2 (bullets-target-posn bullet-sim) adjustment))
     (make-screen time)]))

(define draw-screen
  (match-lambda
    [(struct screen (time))
     
     (gl-init display-width display-height)
     (gl-viewport/restrict screen-width screen-height
                           screen-width screen-height
                           (cartesian-posn-x (bullets-target-posn bullet-sim))
                           (cartesian-posn-y (bullets-target-posn bullet-sim)))
     
     (gl-clear-color 1 1 1 1)
     (gl-clear 'color-buffer-bit)
     
     (gl-color 0 0 0 1)
     (for ([b (in-list (collision-bullets bullet-sim))])
       (match b
         [(struct col:body (layer (struct cartesian-posn [x y]) radius))
          
          (with-translate x y
            (with-scale radius radius
              (gl-draw-circle 
               (case layer
                 [(ship) 'outline]
                 [(bullet) 'solid]))))]))]))

(define stop?
  (match-lambda
    [(struct screen (time))
     dead?]))

(define display-width (inexact->exact (* scale screen-width)))
(define display-height (inexact->exact (* scale screen-height)))

(big-bang 
 screen?
 initial-screen
 #:height display-height
 #:width display-width
 #:on-tick step-simulation
 #:tick-rate (inexact->exact tick-rate)
 #:on-key ship-steer
 #:draw-init void
 #:on-draw draw-screen
 #:stop-when stop?
 #:stop-timer stop?)