
(import music)

; The Following are the modifiable elements of our program
(define grid-width 10)
(define grid-height 3)
(define documentName "csc151finalProject/noteDocument.txt")
(define repetitions 2)
(define volume 110)
(define ln (dur 1 8))

;;; (constructRow array start length) -> list?
;;;   array : a list? of string?
;;;   start : integer?, positive, the starting spot of this line
;;;   length : integer?, positive, the length of the rest of the line 
;;; returns an list? of strings? that spans the length of the line 
;;;   starting from position start
(define constructRow
    (lambda (array start length)
        (match length
            [0 null]
            [_ (cons (list-ref array (+ start (- grid-width length))) 
                    (constructRow array start (- length 1))) ])))

;;; (constructGrid array row) -> list? of list?
;;;   array : list? of string?
;;;   row : integer? positive, the current row
;;; return a list? of list? of string?, turns the array into a grid with 
;;;   height and width
(define constructGrid
    (lambda (array row)
        (match row
            [0 null]
            [_ (cons (constructRow array (* grid-width (- grid-height row)) grid-width) 
                    (constructGrid array (- row 1)))])))


(define highHat (mod (dynamics volume) (mod percussion (note 44 ln))))
(define snareDrum (mod (dynamics volume) (mod percussion (note 37 ln))))
(define bassDrum (mod (dynamics volume) (mod percussion (note 35 ln))))

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
            [other "Room for Expansion"])))

;;; (activateLine line lineNum) -> music?
;;;   line : list?
;;;   lineNum : number?, positive 
;;; return a sequence of notes dictated by the line and lineNum
(define activateLine
    (lambda (line lineNum)
        (apply seq 
            (map activateNote
                (map (lambda (num) 
                    (if (equal? num "true")
                        lineNum
                        0)) line)))))

;;; (forEachLine array lineNum) -> list? of music?
;;;   array : list? of list?
;;;   lineNum : number?, positive
;;; return a list of sequences of notes
(define forEachLine
    (lambda (array lineNum)
        (match lineNum
            [0 null]
            [_ (cons (activateLine (list-ref array (- grid-height lineNum) )lineNum)
                    (forEachLine array (- lineNum 1)))])))

;;; (beatMachine array reps) -> music?
;;;   array : list? of list?
;;;   reps : non zero integer?, number of repetitions
;;; return music represented by the array repeated reps times
(define beatMachine 
    (lambda (array reps) 
        (repeat reps (apply par (forEachLine array grid-height)))))

(define noteDocument (file->lines documentName))
(define grid (constructGrid noteDocument grid-height))
(define beat (beatMachine grid repetitions))
beat

"Everything Finished Running"