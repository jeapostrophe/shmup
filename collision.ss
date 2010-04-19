#lang scheme/base
(require scheme/list
         scheme/local
         scheme/math
         scheme/contract
         (planet jaymccarthy/bulletml/posn))

(define (posn-distance p1 p2)
  (sqrt (+ (sqr (- (cartesian-posn-x p1) (cartesian-posn-x p2)))
           (sqr (- (cartesian-posn-y p1) (cartesian-posn-y p2))))))

(define-struct body (layer [posn #:mutable] radius)
  #:prefab)

(define how-many-collision-tests 0)
(define (bodies-overlap? b1 b2)
  (define p1 (body-posn b1))
  (define p2 (body-posn b2))
  (define d (posn-distance p1 p2))
  (set! how-many-collision-tests (add1 how-many-collision-tests))  
  (< d (+ (body-radius b1) (body-radius b2))))

(define (hash-ref!2 ht x y def)
  (define x-ht (hash-ref! ht x make-hasheq))
  (hash-ref! x-ht y def))
(define (hash-has-key?2 ht x y)
  (define x-ht (hash-ref ht x #f))
  (and x-ht
       (hash-has-key? x-ht y)))
(define (hash-set!2 ht x y v)
  (define x-ht (hash-ref! ht x make-hasheq))
  (hash-set! x-ht y v))

(define-syntax-rule (body-for-each-corner! the-hash b y-ht expr ...)
  (local [(define the-b b)
          (define pos (body-posn the-b))
          (define rad (body-radius the-b))
          ; Compute corners
          (define min-x (- (cartesian-posn-x pos) rad))
          (define max-x (+ (cartesian-posn-x pos) rad))
          (define min-y (- (cartesian-posn-y pos) rad))
          (define max-y (+ (cartesian-posn-y pos) rad))
          ; Find cells
          (define min-x-c (comp->cell min-x))
          (define max-x-c (comp->cell max-x))
          (define min-y-c (comp->cell min-y))
          (define max-y-c (comp->cell max-y))
          ; Find hash components
          (define minx-ht (hash-ref! the-hash min-x-c make-hasheq))
          (define maxx-ht (hash-ref! the-hash max-x-c make-hasheq))
          ; Find second level hashs
          (define minx-miny-ht (hash-ref! minx-ht min-y-c make-hasheq))
          (define maxx-miny-ht (hash-ref! maxx-ht min-y-c make-hasheq))
          (define minx-maxy-ht (hash-ref! minx-ht max-y-c make-hasheq))
          (define maxx-maxy-ht (hash-ref! maxx-ht max-y-c make-hasheq))]
    ; Run expr ... on each corner
    (local [(define y-ht minx-miny-ht)]
      expr ...)
    (local [(define y-ht minx-maxy-ht)]
      expr ...)
    (local [(define y-ht maxx-miny-ht)]
      expr ...)
    (local [(define y-ht maxx-maxy-ht)]
      expr ...)))

(define cell-size 20.0)
(define (comp->cell x)
  (inexact->exact (floor (/ x cell-size))))

(define (hash-symmetric-n2-for-each ht f)
  (let outer-loop ([pos (hash-iterate-first ht)])
    (when pos
      (let ([pos-v (hash-iterate-value ht pos)]
            [next-pos (hash-iterate-next ht pos)])
        (let inner-loop ([after-pos next-pos])
          (when after-pos
            (let ([after-v (hash-iterate-value ht after-pos)])
              (f pos-v after-v))
            (inner-loop (hash-iterate-next ht after-pos))))
        (outer-loop next-pos)))))

(define (spatial-hash-collisions collide! bodies)
  (define the-hash (make-hasheq))
  (define (add-body-to-hash! b)
    (define b1-layer (body-layer b))
    (body-for-each-corner!
     the-hash b y-ht
     (define old (hash-ref! y-ht b1-layer empty))
     (hash-set! y-ht b1-layer (list* b old))))
  (define seen?-ht (make-hasheq))
  (for-each add-body-to-hash! bodies)
  
  ; For each X cell
  (for ([x-hash (in-hash-values the-hash)])
    ; Get all the Y cells
    (for ([y-hash (in-hash-values x-hash)])
      ; Iterate over each layer vs all the later layers in the hash/list
      (hash-symmetric-n2-for-each
       y-hash
       (lambda (b1-bodies b2-bodies)
         ; Compare every body vs every other
         (for* ([b1 (in-list b1-bodies)]
                [b2 (in-list b2-bodies)]
                #:when (not (hash-has-key?2 seen?-ht b1 b2)))
           (hash-set!2 seen?-ht b1 b2 #t)
           (when (bodies-overlap? b1 b2)
             (collide! b1 b2))))))))

(define (find-collisions! collide! bodies)
  (set! how-many-collision-tests 0)
  (spatial-hash-collisions collide! bodies)
  (printf "~a collisons tests for ~a bodies~n" 
          how-many-collision-tests
          (length bodies)))

(provide/contract
 [struct body ([layer symbol?]
               [posn cartesian-posn?]
               [radius inexact-real?])]
 [find-collisions! ((body? body? . -> . void)
                    (listof body?)
                    . -> .
                    void)])