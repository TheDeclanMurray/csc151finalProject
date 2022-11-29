(import canvas)
; (import image)


(define width 300)
(define height 100)
(define cords (vector (/ width 8) (/ height 2)))
(define canv (canvas width height))
(define backgroundColor "gray")
(rectangle canv 0 0  width height "solid" backgroundColor)


(define getX
    (lambda () (vector-ref cords 0)))
(define getY
    (lambda () (vector-ref cords 1)))
(define setX
    (lambda (x) (vector-set! cords 0 x)))
(define setY
    (lambda (y) (vector-set! cords 1 y)))
(define reset
    (lambda ()
        (rectangle canv 0 0  width height "solid" backgroundColor)))


(define drawFrame 
    (lambda (t)
        (begin
            (reset)
            (setX (+ (getX) .2))
            ; (vector-set! canv 0 (canvas width height))
            (circle canv (getX) (getY) 20 "solid" "red")
        ) 
         
    )
) 

; (circle canv (getX) (getY) 20 "solid" "red")
; canv
(let ([canv (canvas width height)])
    (begin
        (setX (+ (getX) 1))
        (circle canv (getX) (getY) 20 "solid" "red")
        canv
        ))

(animate-with drawFrame)
; (circle canv (getX) (getY) 20 "solid" "green")


canv

"running?"