(import canvas)
(import html)

(define width 1800)
(define height 300)
(define c1 (canvas width height))

;;; (reset canv) -> void?
;;;   canv : canvas?
;;; "Resets" the given canvas by creating a blank rectangle with the same dimensions of the canvas.
(define reset
  (lambda (canv)
    (rectangle canv 0 0 width height "solid" "gray")))

;;; (getRandom n) -> integer?
;;;   n : integer?, n >= 0
;;; Returns the result of adding 50 to a random integer between 0 and n, exclusive.
(define getRandom
  (lambda (n)
    (+ 50 (random n))))

;;; (randomVec n) -> vector?
;;;   n : integer?
;;; Creates a vector of length n and fills it with random integers between 50 and 300, 
;;; exclusive, using the getRandom function.
(define randomVec
  (lambda (n)
    (let([vec (vector-range n)])
      (begin
        (vector-map
          (lambda (i)
            (vector-set! vec i (getRandom 250)))
          vec)
        vec))))

;;; (makeVisualizer vec canv) -> void?
;;;   vec : vector?, a vector of integers
;;;   canv : canvas?
;;; Draws (vector-length vec) solid black rectangles on the given canvas, each separated by
;;; 2 pixels. Each rectangle has width 23 and height of the corresponding element in vec.
(define makeVisualizer
  (lambda (vec canv)
    (let ([count (vector 1)])
      (vector-map
        (lambda (h)
          (begin
            (rectangle canv (* 25 (vector-ref count 0)) (- height h) 23 h "solid" "black")
            (vector-set! count 0 (+ 1 (vector-ref count 0))))) 
        vec))))


  
  
  (define lastFrame (vector 0))

;;; (getLastFrame) -> integer?
;;; Returns the time of the last visual update, in milliseconds.
  (define getLastFrame
    (lambda () 
      (vector-ref lastFrame 0)))

;;; (setLastFrame time) -> void?
;;;   time : integer?, time in milliseconds
;;; Sets the time of the last visual update to the given time.
  (define setLastFrame
    (lambda (time) 
      (vector-set! lastFrame 0 time)))
  
;;; (animateVisualizer canv) -> void?
;;;   canv : canvas?
;;; If getLastFrame is > time, t, returns void. Otherwise, resets the given canvas and
;;; calls makeVisualizer once every 500 milliseconds, or half-second, with 20 rectangles. 
  (define animateVisualizer
    (lambda (canv)
      (lambda (t)
        (if (> (getLastFrame) t)
          void
          (begin
            (setLastFrame (+ t 500))
            (reset canv)
            (makeVisualizer (randomVec 20) canv))))))

(animate-with (animateVisualizer c1))
c1
  
; (define startVisualizerBtn (button "button1" "click to start"))

; (button-onclick startVisualizerBtn 
;   (lambda ()
;       (begin
;       (animate-with (animateVisualizer c1)))))

; startVisualizerBtn
; c1