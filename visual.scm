(import canvas)
(import music)

; Struct for common use
(struct button (x y row col))

; Canvas x and y are off so this is the alignment 
(define calabrateX -20)
(define calabrateY -15)

; Canvas Width and grid properties deturmin the Height
(define canWidth 500)
(define grid-width 10)
(define grid-height 6)
(define buttonSize (/ canWidth grid-width))
(define topHeight 50)
(define spacerWidth 3)
(define canHeight (floor (+ (* buttonSize grid-height) topHeight)))
(define canv (canvas canWidth canHeight))
(define backgroundColor "gray")

; Vector Storage for if button is pressed
(define storage (vector-map (lambda (x) (make-vector grid-width #f)) 
                            (make-vector grid-height 3)))

; Music
(define volume 110)
(define ln (dur 1 8))
(define repetitions 2)
(define highHat (mod (dynamics volume) (mod percussion (note 44 ln))))
(define snareDrum (mod (dynamics volume) (mod percussion (note 37 ln))))
(define bassDrum (mod (dynamics volume) (mod percussion (note 35 ln))))


;;; (getColor row col) -> string?
;;;   row : positive integer?, the row of the button
;;;   col : positive integer?, the column of the button
;;; return the color of the button
(define getColor
    (lambda (row col)
        (if (vector-ref (vector-ref storage row) col)
            "green"
            "red")))

;;; (constructGridRow) -> list? of button?
;;;   col : positive integer?, the column of the button
;;;   row : positive integer?, the row of the button
;;;   num : positive integer?, current col
;;; returns a list? of button?
(define constructGridRow
    (lambda (col row num)
        (match num
            [0 null]
            [_ (cons (button (* col buttonSize) (+ (* row buttonSize) topHeight) row col) 
                    (constructGridRow (+ col 1) row (- num 1)))])))

;;; (constructGrid numRows)-> list? of List? of button?
;;;   numRows : positive integer?, number of rows
;;; returns a grid of buttons in the form of a 2D list
(define constructGrid
    (lambda (numRows)
        (match numRows
            [0 null]
            [_ (cons (constructGridRow 0 (- grid-height numRows) grid-width) 
                    (constructGrid (- numRows 1)))])))

(define grid (constructGrid grid-height))

;;; (activateNote val) -> music?
;;;   val : integer?
;;; return note depending on val
(define activateNote
    (lambda (val)
        (match val
            [0 (rest ln)]
            [1 highHat]
            [2 snareDrum]
            [3 bassDrum]
            [other (rest ln)])))

;;; (activateLine line lineNum) -> music?
;;;   line : vector?
;;;   lineNum : number?, positive 
;;; return a sequence of notes dictated by the line and lineNum
(define activateLine
    (lambda (line lineNum)
        (apply seq 
            (vector->list
                (vector-map activateNote
                    (vector-map (lambda (num) 
                        (if num
                            lineNum
                            0)) line))))))

;;; (forEachLine array lineNum) -> vector? of music?
;;;   array : vector? of vector?
;;;   lineNum : number?, positive
;;; return a list of sequences of notes
(define forEachLine
    (lambda (array lineNum)
        (match lineNum
            [0 null]
            [_ (cons (activateLine (vector-ref array (- grid-height lineNum)) lineNum)
                    (forEachLine array (- lineNum 1)))])))


;;; (beatMachine array reps) -> music?
;;;   array : vector? of vector?
;;;   reps : non zero integer?, number of repetitions
;;; return music represented by the array repeated reps times
(define beatMachine 
    (lambda () 
        (repeat repetitions (apply par 
            (forEachLine storage grid-height)))))

; <<<<<<<<<<Draw Frame>>>>>>>>>>
;   |-\                        |----                 
;   |  \  ___   __             |     ___   __    _   _   ___ 
;   |   |/   \ /  \ \        / |--- /   \ /  \ |/ \ / \ /   \             
;   |  / |     |  |  \  /\  /  |    |     |  | |   |  | |----    
;   |_/  |     \ /|   \/  \/   |    |     \ /| |   |  | \___/     

;;; (reset) -> void
;;; resets the canvas to the background color
(define reset
    (lambda ()
        (rectangle canv 0 0  canWidth canHeight "solid" backgroundColor)))

;;; (drawButton btn cnv) -> void
;;;   btn : button?
;;;   cnv : canvas?
;;; draw a button 
(define drawButton
    (lambda (btn cnv)
        (rectangle cnv 
            (+ spacerWidth (button-x btn)) 
            (+ spacerWidth (button-y btn))
            (- buttonSize (* 2 spacerWidth))
            (- buttonSize (* 2 spacerWidth))
            "solid"
            (getColor (button-row btn) (button-col btn)))))

;;; (drawRow row cnv) - void?
;;;   row : list? of button?
;;;   cnv : canvas?
;;; draw a row of buttons
(define drawRow
    (lambda (row cnv)
        (map (lambda (btn)
                    (drawButton btn cnv)) row)))

;;; (drawgrid grid cnv) -> void?
;;;   grid : list? of list? of button?
;;;   cnv : canvas?
;;; draw a grid of buttons
(define drawGrid
    (lambda (grid cnv)
        (map (lambda (row) 
                    (drawRow row cnv)) grid)))

;;; (drawFrame t) -> void
;;;   t : time
;;; next frame
(define drawFrame 
    (lambda (t)
        (begin
            (reset)
            (drawGrid grid canv)))) 



; <<<<<<<<<<On Click>>>>>>>>>>
;   /    \         /    \  |          | /  
;  /      \   __  /        |  *   __  |/  
;  |      | |/  \ |        |  |  /  \ |\   
;  \      / |   | \      / |  | |     | \ 
;   \____/  |   |  \____/  |_ |  \__/ |  \             

;;; (getColNum x colNum) -> integer?
;;;   x : the x value of the curser
;;;   colNum : current column
;;; return the column number that x resides in
(define getColNum
    (lambda (x colNum)
        (if (<= x (* buttonSize (+ 1 colNum)))
            colNum
            (getColNum x (+ colNum 1)))))

;;; (getColNum y rowNum) -> integer?
;;;   y : the y value of the curser
;;;   rowNum : current row
;;; return the row number that y resides in
(define getRowNum
    (lambda (y rowNum)
        (if (<= (- y topHeight) (* buttonSize (+ 1 rowNum)) )
            rowNum
            (getRowNum y (+ 1 rowNum)))))

;;; (onClick x y) -> void?
;;;   x : x value of the curser
;;;   y : y value of the curser
;;; handle an on click
(define onClick
    (lambda (x y)
        (if (< y topHeight)
            (beatMachine)
            (let* ([rowNum (getRowNum (+ y calabrateY) 0)]
                [colNum (getColNum (+ x calabrateX) 0)]
                [spotVal (vector-ref (vector-ref storage rowNum) colNum)])
            (begin
                ; (circle canv (+ x calabrateX) (+ y calabrateY) 15  "solid" "black")
                (vector-set! (vector-ref storage rowNum) colNum (not spotVal)))))))

; animate canvas and add click listener
(begin
    (reset)
    (animate-with drawFrame)
    (canvas-onclick canv onClick)
    canv)




"Other"
(beatMachine)
(note 60 qn)

"Everything Ran"