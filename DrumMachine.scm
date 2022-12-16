(import canvas)
(import music)

; Struct for common use
(struct button (x y row col))

; Canvas x and y are off so this is the alignment 
(define calabrateX -20)
(define calabrateY -15)

; Canvas Width and grid properties deturmin the Height
(define canWidth 700)
(define grid-width 12)
(define grid-height 8)
(define buttonSize (floor (/ canWidth grid-width)))
(define topHeight 200)
(define playButtonHeight 40)
(define playButtonWidth 50)
(define spacerWidth 3)
(define canHeight (floor (+ (* buttonSize grid-height) topHeight)))
(define canv (canvas canWidth canHeight))
(define backgroundColor "gray")

; Vector Storage for if button is pressed
(define storage (vector-map (lambda (x) (make-vector grid-width #f)) 
                            (make-vector grid-height 3)))

; Visualizer
(define visualizerHeight (- topHeight 10))
(define visualizerWidth (floor (* (/ canWidth 5) 3)))
(define visualizerNumCol 30)
(define visualizerColWidth (floor (/ visualizerWidth visualizerNumCol)))
(define visualizerRefreshRate 500)

; Music
(define volume 110)
(define ln (dur 1 8))
(define repetitions 2)

;;; (makePercNote n) -> music?
;;;     n : integer?, a MIDI note value
;;; Creates a percussion note with volume volume and duration ln, defined as global variables,
;;; and MIDI value n. 
(define makePercNote
    (lambda (n)
        (mod (dynamics volume) (mod percussion (note n ln)))))

(define pedalHighHat (makePercNote 44))
(define snareDrum (makePercNote 38))
(define bassDrum (makePercNote 35))
(define rideCymbal (makePercNote 51))
(define lowFloorTom (makePercNote 41))
(define highFloorTom (makePercNote 43))
(define highBongo (makePercNote 60))
(define lowBongo (makePercNote 61))

; print function used for testing, please ignore
(define printRecete (vector "\nTerminal Print:"))
(define print 
    (lambda (str) 
        (vector-set! printRecete 0 
            (string-append (vector-ref printRecete 0) "\n  " str))))

;;; (getColor row col) -> string?
;;;   row : positive integer?, the row of the button
;;;   col : positive integer?, the column of the button
;;; return the color of the button
(define getColor
    (lambda (row col)
        (if (vector-ref (vector-ref storage row) col)
            "cyan"
            "black")))

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


; <<<<<<<<<<Visualizer>>>>>>>>>>
; \        /                |    
;  \      /   __        __  |   ___  __   ___
;   \    / * /  ' |  | /  \ | *   / |   |/   \
;    \  /  | \--\ |  | |  | | |  /  |-- |   
;     \/   |  __/ \__/ \_/| | | /__ |__ |         

;;; (resetVisualizer) -> void
;;; resets the top part of the canvas to the background color
(define resetVisualizer
    (lambda ()
        (rectangle canv 0 0 canWidth topHeight "solid" backgroundColor)))

;;; (getRandom n) -> integer?
;;;   n : integer?, n >= 0
;;; Returns the result of adding 50 to a random integer between 0 and n, exclusive.
(define getRandom
  (lambda (n)
    (+ 10 (random n))))

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
            (vector-set! vec i (getRandom (- visualizerHeight 10))))
          vec)
        vec))))

;;; (makeVisualizer vec canv) -> void?
;;;   vec : vector?, a vector of integers
;;;   canv : canvas?
;;; Draws (vector-length vec) solid black rectangles on the given canvas, each separated by
;;; 2 pixels. Each rectangle has width 23 and height of the corresponding element in vec.
(define makeVisualizer
  (lambda (vec canv)
    (let ([count (vector 0)])
      (vector-map
        (lambda (h)
          (begin
            (rectangle canv (+ (* visualizerColWidth (vector-ref count 0)) (/ (- canWidth visualizerWidth) 2) )
                (- visualizerHeight h -6) visualizerColWidth h "solid" "black")
            (rectangle canv (+ (* visualizerColWidth (vector-ref count 0)) (/ (- canWidth visualizerWidth) 2) )
                (- visualizerHeight h -6) (- visualizerColWidth 2) h "solid" "cyan")
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

;;; (animateVisualizer t) -> void?
;;;   t : time
;;; If getLastFrame is > time, t, returns void. Otherwise, resets the given canvas and
;;; calls makeVisualizer once every 500 milliseconds, or half-second, with some number of rectangles. 
(define animateVisualizer    
    (lambda (t)
        (if (> (getLastFrame) t)
            void
            (begin
                (resetVisualizer)
                (setLastFrame (+ t visualizerRefreshRate))
                (makeVisualizer (randomVec visualizerNumCol) canv)))))


; <<<<<<<<<<Drum Machine>>>>>>>>>>
;   |-\           
;   |  \  ___         _   _  
;   |   |/   \ |  | |/ \ / \      
;   |  / |     |  | |   |  | 
;   |_/  |     \ /| |   |  |                           

;;; (activateNote val) -> music?
;;;   val : integer?
;;; return note depending on val
(define activateNote
    (lambda (val)
        (begin
            (match val
                [0 (rest ln)]
                [1 pedalHighHat]   
                [2 snareDrum]
                [3 bassDrum]
                [4 rideCymbal]
                [5 lowFloorTom]
                [6 highFloorTom]
                [7 highBongo]
                [8 lowBongo]
                [other (rest ln)]))))

;;; (activateLine line lineNum) -> music?
;;;   line : vector?
;;;   lineNum : number?, positive 
;;; return a sequence of notes dictated by the line and lineNum
(define activateLine
    (lambda (line lineNum)
        (begin
            (print (string-append "Line Number " (number->string lineNum)))
            (apply seq 
                (vector->list
                    (vector-map activateNote
                        (vector-map (lambda (num) 
                            (if num
                                lineNum
                                0)) line)))))))

;;; (forEachLine array lineNum) -> vector? of music?
;;;   array : vector? of vector?
;;;   lineNum : number?, positive
;;; return a list of sequences of notes
(define forEachLine
    (lambda (array lineNum)
        (match lineNum
            [0 null]
            [_ (cons (activateLine (vector-ref array (- lineNum 1)) lineNum)
                    (forEachLine array (- lineNum 1)))])))

;;; (drumMachine array reps) -> music?
;;;   array : vector? of vector?
;;;   reps : non zero integer?, number of repetitions
;;; return music represented by the array repeated reps times
(define drumMachine 
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
        (rectangle canv 0 topHeight canWidth (- canHeight topHeight) "solid" backgroundColor)))

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

(define drawPlayButton
    (lambda (cnv)
        (let* ([triHeight (floor (* playButtonHeight (/ 3 4)))]
                [x1 (- canWidth triHeight spacerWidth)]
                [x2 (+ (- canWidth triHeight spacerWidth) triHeight)]
                [y1 (+ spacerWidth 10)]
                [y2 (+ spacerWidth triHeight)]
                [y3 (floor (/ (+ y1 y2) 2))]
                [coords (list (pair x1 y1) (pair x2 y3) (pair x1 y2) (pair x1 y1))])
            (begin
                (rectangle cnv (- canWidth playButtonWidth spacerWidth)
                    spacerWidth
                    playButtonWidth
                    playButtonHeight
                    "solid"
                    "black" )
                (text cnv "Play" 
                    (floor (- canWidth playButtonWidth (/ spacerWidth 2))) 
                    (floor (+ (/ playButtonHeight 1.4) spacerWidth)) 
                    "solid" "cyan" "24px sans-serif")
                    ))))

;;; (drawFrame t) -> void
;;;   t : time
;;; next frame
(define drawFrame 
    (lambda (t)
        (begin
            (reset)
            (animateVisualizer t)
            (drawPlayButton canv)
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
            (trigger drumMachine)
            (let* ([rowNum (getRowNum (+ y calabrateY) 0)]
                    [colNum (getColNum (+ x calabrateX) 0)]
                    [spotVal (vector-ref (vector-ref storage rowNum) colNum)])
                (begin
                    ; (circle canv (+ x calabrateX) (+ y calabrateY) 15  "solid" "black")
                    (vector-set! (vector-ref storage rowNum) colNum (not spotVal))
                    )))))

; animate canvas and add click listener
(begin
    (reset)
    (animate-with drawFrame)
    (canvas-onclick canv onClick)
    canv)

"Example DrumBeat:"
; Testing to see if it would work 
(begin
    (vector-set! (vector-ref storage 2) 3 #t)
    (vector-set! (vector-ref storage 2) 5 #t)
    (vector-set! (vector-ref storage 0) 4 #t)
    (vector-set! (vector-ref storage 0) 6 #t)
    (vector-set! (vector-ref storage 0) 0 #t)
    (vector-set! (vector-ref storage 1) 0 #t)
    (vector-set! (vector-ref storage 2) 0 #t)
    (vector-set! (vector-ref storage 3) 0 #t)
    (drumMachine))

; (string-append (vector-ref printRecete 0) "\n")