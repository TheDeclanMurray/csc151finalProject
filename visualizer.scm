(import music)
(import canvas)

(define width 1800)
(define height 300)
(define c1 (canvas width height))
(define c2 (canvas width height))
(define reset
  (lambda (canv)
    (rectangle canv 0 0 width height "solid" "white")))


(define getRandom
  (lambda (n)
    (+ 50 (random n))))

(define randomVec
  (lambda (n)
    (let([vec (vector-range n)])
      (begin
        (vector-map
          (lambda (i)
            (vector-set! vec i (getRandom 250)))
          vec)
        vec))))

(define getFrequency
  (lambda (x)
    (* 440 (expt 2 (/ (- x 69) 12)))))

(define make-rectangles
  (lambda (vec canv)
    (let ([count (vector 1)])
      (vector-map
        (lambda (h)
          (begin
            (rectangle canv (* 25 (vector-ref count 0)) (- height h) 24 h "solid" "black")
            (vector-set! count 0 (+ 1 (vector-ref count 0))))) vec))))

(define testVec (make-vector 10 100))

; (define make-rectangles-2
;   (lambda (vec canv)
;     (let ([count (vector 1)])
;       (vector-map
;         (lambda (h)
;           (begin
;             (rectangle canv (* 25 (vector-ref count 0)) (- height h) 24 h "solid" "black")
;             (vector-set! count 0 (+ 1 (vector-ref count 0))))) vec))))

  
  
  (define lastFrame (vector 0))
  (define getLastFrame
    (lambda () (vector-ref lastFrame 0)))
  (define setLastFrame
    (lambda (time) (vector-set! lastFrame 0 time)))
  
  (define animation-testing
    (lambda (t)
      (if (> (getLastFrame) t)
        void
        
        (begin
          (setLastFrame (+ t 500))
          (reset c2)
          (make-rectangles (randomVec 20) c2)))))
  
  (animate-with animation-testing)
  c2


(define snareDrum (getFrequency 38))
snareDrum

(define numSamples (* snareDrum 0.5))
numSamples

(define freqTest
  (vector-map
  (lambda (h)
    (floor (* 50 h )))(vector-map abs (vector-range -1 1 (/ 1 36)))))

freqTest

(define c3 (canvas width height))
(reset c3)

; (make-rectangles freqTest c3)
; c3


; (define test (tri (vector-range 0 30) (/ 30 73.4)))

; (define testMap
;   (vector-map 
;   (lambda (i)
;     (abs (floor (* 150 i)))) test))

; (make-rectangles testMap c3)
; c3
  

  
(define storage (list->vector (reverse (vector->list (vector-range 1 30)))))
storage

(define storageMap
  (vector-map
    (lambda (x)
      (* 10 x)) storage))

; (make-rectangles storageMap c3)
; c3