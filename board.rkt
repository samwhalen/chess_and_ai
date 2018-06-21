#lang typed/racket

;; CMSC15100 Winter 2018
;; Project 2 -- board.rkt
;; Sam Whalen
;;
;; Definition of basic types to represent players, boards, and games.

;; include CS151-specific definitions
(require "../include/cs151-core.rkt")

;; include testing framework
(require typed/test-engine/racket-tests)

;; DATA DEFINITIONS ============================================================

;; A player is identified by the color of his/her/its pieces
(define-type Player (U 'black 'white))

;; A (Coord r c) specifies the row (r) and column (c) of a board location
(define-struct Coord
  ([row : Integer]
   [col : Integer]))

;; A cell in the board either has a player's piece or is empty ('_)
(define-type Cell (U Player '_))

;; A (Board w cells) represents a (w x w) Othello board, where cells is a
;; list of the cells in the board in row-major order.  We require that w be
;; even and in the range 6..16.
(define-struct Board
  ([size : Integer]            ;; the dimension of the board;
   [cells : (Listof Cell)]))   ;; a list of length size*size

;; UTILITY FUNCTIONS ===========================================================

(: player=? : Cell Cell -> Boolean)
;; checks to see if two players are the same color
(define (player=? p1 p2)
  (match* (p1 p2)
    [('black 'black) #t]
    [('white 'white) #t]
    [(_ _) #f]))

(check-expect (player=? 'black 'black) #t)
(check-expect (player=? 'white 'white) #t)
(check-expect (player=? 'white 'black) #f)
(check-expect (player=? 'black 'white) #f)

(: other-player : Player -> Player)
;; return the other player
(define (other-player p)
  (if (player=? p 'black)
      'white
      'black))

(check-expect (other-player 'black) 'white)
(check-expect (other-player 'white) 'black)

(: cell-is-player? : Player -> (Cell -> Boolean))
;; curried function for testing if a cell holds a player's piece
(define (cell-is-player? p)
  (lambda ([cell : Cell])
    (player=? p cell)))
           
(check-expect ((cell-is-player? 'black) 'black) #t)
(check-expect ((cell-is-player? 'white) 'white) #t)
(check-expect ((cell-is-player? 'black) 'white) #f)
(check-expect ((cell-is-player? 'white) 'black) #f)

(: player->string : Player -> String)
;; return the name of the player; i.e., "Black" or "White"
(define (player->string p)
  (if (player=? p 'white)
      "White"
      "Black"))

(check-expect (player->string 'white) "White")
(check-expect (player->string 'black) "Black")

(: empty-cell? : Cell -> Boolean)
;; is the cell an empty (i.e., '_) cell?
(define (empty-cell? c)
  (symbol=? c '_))

(check-expect (empty-cell? 'black) #f)
(check-expect (empty-cell? 'white) #f)
(check-expect (empty-cell? '_) #t)

(: coord+ : Coord Coord -> Coord)
;; add two coordinates
(define (coord+ c1 c2)
  (Coord (+ (Coord-row c1)
            (Coord-row c2))
         (+ (Coord-col c1)
            (Coord-col c2))))

(check-expect (coord+ (Coord 1 2) (Coord 2 1)) (Coord 3 3))
(check-expect (coord+ (Coord 0 0) (Coord 12 425)) (Coord 12 425))
(check-expect (coord+ (Coord -2 4) (Coord -2 -3)) (Coord -4 1))

(: make-board : Natural -> Board)
;; make an empty board of the specified width.  Note that this function does
;; not check that the size of the board is valid.
(define (make-board n)
  (local {(: count : Integer (Listof Cell) -> (Listof Cell))
          ;; this will use an accumulator to do the actual work of the
          ;; counting
          (define (count i acc)
            (if (> i 0)
                (count (sub1 i) (cons '_ acc))
                acc))}
    (Board n (count (sqr n) '()))))

(check-expect (make-board 6) (Board 6 (list '_ '_ '_ '_ '_ '_
                                            '_ '_ '_ '_ '_ '_
                                            '_ '_ '_ '_ '_ '_
                                            '_ '_ '_ '_ '_ '_
                                            '_ '_ '_ '_ '_ '_
                                            '_ '_ '_ '_ '_ '_)))
(check-expect (make-board 3) (Board 3 (list '_ '_ '_
                                            '_ '_ '_
                                            '_ '_ '_)))
(check-expect (make-board 5) (Board 5 (list '_ '_ '_ '_ '_
                                             '_ '_ '_ '_ '_
                                             '_ '_ '_ '_ '_
                                             '_ '_ '_ '_ '_
                                             '_ '_ '_ '_ '_)))

(: num-cells : Board -> Integer)
;; return the number of cells in the board
(define (num-cells b)
  (match b
    [(Board w loc)
     (sqr w)]))

(check-expect (num-cells (Board 6 (list '_ '_ '_ '_ '_ '_
                                        '_ '_ '_ '_ '_ '_
                                        '_ '_ '_ '_ '_ '_
                                        '_ '_ '_ '_ '_ '_
                                        '_ '_ '_ '_ '_ '_
                                        '_ '_ '_ '_ '_ '_))) 36)
(check-expect (num-cells (Board 3 (list '_ '_ '_
                                        '_ '_ '_
                                        '_ '_ '_))) 9)
(check-expect (num-cells (Board 5 (list '_ '_ '_ '_ '_
                                        '_ '_ '_ '_ '_
                                        '_ '_ '_ '_ '_
                                        '_ '_ '_ '_ '_
                                        '_ '_ '_ '_ '_))) 25)

(: on-board? : Board Coord -> Boolean)
;; is a position on the board?
(define (on-board? b c)
  (match b
    [(Board w loc)
     (and (<= (Coord-row c) (sub1 w))
          (>= (Coord-row c) 0)
          (<= (Coord-col c) (sub1 w))
          (>= (Coord-col c) 0))]))

(check-expect (on-board? (Board 6 (list '_ '_ '_ '_ '_ '_
                                        '_ '_ '_ '_ '_ '_
                                        '_ '_ '_ '_ '_ '_
                                        '_ '_ '_ '_ '_ '_
                                        '_ '_ '_ '_ '_ '_
                                        '_ '_ '_ '_ '_ '_)) (Coord 0 0)) #t)
(check-expect (on-board? (Board 3 (list '_ '_ '_
                                        '_ '_ '_
                                        '_ '_ '_)) (Coord 1 2)) #t)
(check-expect (on-board? (Board 5 (list '_ '_ '_ '_ '_
                                        '_ '_ '_ '_ '_
                                        '_ '_ '_ '_ '_
                                        '_ '_ '_ '_ '_
                                        '_ '_ '_ '_ '_))
                         (Coord 10 9)) #f)
(check-expect (on-board? (Board 3 (list '_ '_ '_
                                        '_ '_ '_
                                        '_ '_ '_))
                         (Coord -1 -1)) #f)
              
(: coord->index : Board Coord -> Integer)
;; convert a coordinate to an index into the list of cells for the board
(define (coord->index b c)
  (match* (b c)
    [((Board w loc) (Coord r c))
     (+ (* w r) c)]))

(check-expect (coord->index (Board 6 (list '_ '_ '_ '_ '_ '_
                                           '_ '_ '_ '_ '_ '_
                                           '_ '_ '_ '_ '_ '_
                                           '_ '_ '_ '_ '_ '_
                                           '_ '_ '_ '_ '_ '_
                                           '_ '_ '_ '_ '_ '_)) (Coord 3 4)) 22)
(check-expect (coord->index (Board 3 (list '_ '_ '_
                                           '_ '_ '_
                                           '_ '_ '_)) (Coord 1 2)) 5)
(check-expect (coord->index (Board 5 (list '_ '_ '_ '_ '_
                                            '_ '_ '_ '_ '_
                                            '_ '_ '_ '_ '_
                                            '_ '_ '_ '_ '_
                                            '_ '_ '_ '_ '_))
                            (Coord 2 3)) 13)

(: index->coord : Board Integer -> Coord)
;; convert an index into the list of cells to a coordinate
(define (index->coord b i)
  (match b
    [(Board w loc)
     (Coord (exact-floor (/ i w))
            (- i (* (exact-floor (/ i w)) w)))]))

(check-expect (index->coord (Board 6 (list '_ '_ '_ '_ '_ '_
                                           '_ '_ '_ '_ '_ '_
                                           '_ '_ '_ '_ '_ '_
                                           '_ '_ '_ '_ '_ '_
                                           '_ '_ '_ '_ '_ '_
                                           '_ '_ '_ '_ '_ '_)) 22) (Coord 3 4))
(check-expect (index->coord (Board 3 (list '_ '_ '_
                                           '_ '_ '_
                                           '_ '_ '_)) 5) (Coord 1 2)) 
(check-expect (index->coord (Board 5 (list '_ '_ '_ '_ '_
                                           '_ '_ '_ '_ '_
                                           '_ '_ '_ '_ '_
                                           '_ '_ '_ '_ '_
                                           '_ '_ '_ '_ '_)) 22)
              (Coord 4 2))

(: board-ref : Board Coord -> Cell)
;; return the cell value at the given position
(define (board-ref b c)
  (match b
    [(Board w loc)
     (list-ref loc (coord->index b c))]))

(check-expect (board-ref (Board 6 (list '_ '_ '_ '_ '_ '_
                                        '_ '_ '_ '_ '_ '_
                                        '_ '_ '_ '_ '_ '_
                                        '_ '_ '_ '_ 'black '_
                                        '_ '_ '_ '_ '_ '_)) (Coord 3 4))
              'black)
(check-expect (board-ref (Board 6 (list '_ '_ '_ '_ '_ '_
                                        '_ '_ '_ '_ '_ '_
                                        '_ '_ '_ '_ '_ '_
                                        '_ '_ '_ '_ '_ '_
                                        '_ '_ '_ '_ '_ '_)) (Coord 3 4))
              '_)
(check-expect (board-ref (Board 6 (list '_ '_ '_ 'white '_ '_
                                        '_ '_ '_ '_ '_ '_
                                        '_ '_ '_ '_ '_ '_
                                        '_ '_ '_ '_ '_ '_
                                        '_ '_ '_ '_ '_ '_)) (Coord 0 3))
              'white)

(: board-update : Board Coord Cell -> Board)
;; functional update of a board
(define (board-update b coord cell)
  (match b
    [(Board w loc)
     (Board w (list-set loc (coord->index b coord) cell))]))

(check-expect (board-update (Board 3 (list 'black 'white '_
                                           'white 'black '_
                                           'black 'white '_))
                            (Coord 1 1) 'white)
              (Board 3 (list 'black 'white '_
                             'white 'white '_
                             'black 'white '_)))

(check-error (board-update (Board 3 (list 'black 'white '_
                                           'white 'black '_
                                           'black 'white '_))
                            (Coord 5 5) 'white)
              "car: expects a pair, given '()")
(check-expect (board-update (Board 3 (list 'black 'white '_
                                           'white 'black '_
                                           'black 'white '_))
                            (Coord 2 2) 'white)
              (Board 3 (list 'black 'white '_
                             'white 'black '_
                             'black 'white 'white)))

;; Helpers for Neighbors function ==============================================

(: N : Coord -> Coord)
;; gives N neighbor
(define (N c)
  (coord+ c (Coord -1 0)))

(check-expect (N (Coord 1 1)) (Coord 0 1))
(check-expect (N (Coord 2 2)) (Coord 1 2))

(: NE : Coord -> Coord)
;; gives NW neighbor
(define (NE c)
  (coord+ c (Coord -1 1)))

(check-expect (NE (Coord 1 1)) (Coord 0 2))
(check-expect (NE (Coord 2 2)) (Coord 1 3))

(: E : Coord -> Coord)
;; gives E neighbor
(define (E c)
  (coord+ c (Coord 0 1)))

(check-expect (E (Coord 1 1)) (Coord 1 2))
(check-expect (E (Coord 2 2)) (Coord 2 3))

(: SE : Coord -> Coord)
;; gives SE neighbor
(define (SE c)
  (coord+ c (Coord 1 1)))

(check-expect (SE (Coord 1 1)) (Coord 2 2))
(check-expect (SE (Coord 2 2)) (Coord 3 3))

(: S : Coord -> Coord)
;; gives S neighbor
(define (S c)
  (coord+ c (Coord 1 0)))

(check-expect (S (Coord 1 1)) (Coord 2 1))
(check-expect (S (Coord 2 2)) (Coord 3 2))

(: SW : Coord -> Coord)
;; gives SW neighbor
(define (SW c)
  (coord+ c (Coord 1 -1)))

(check-expect (SW (Coord 1 1)) (Coord 2 0))
(check-expect (SW (Coord 2 2)) (Coord 3 1))

(: W : Coord -> Coord)
;; gives W neighbor
(define (W c)
  (coord+ c (Coord 0 -1)))

(check-expect (W (Coord 1 1)) (Coord 1 0))
(check-expect (W (Coord 2 2)) (Coord 2 1))

(: NW : Coord -> Coord)
;; gives NW nieghbor
(define (NW c)
  (coord+ c (Coord -1 -1)))

(check-expect (NW (Coord 1 1)) (Coord 0 0))
(check-expect (NW (Coord 2 2)) (Coord 1 1))

(: top : Coord -> (Listof Coord))
;; gives the nieghbors of a top cell
(define (top c)
  (list (E c)
        (SE c)
        (S c)
        (SW c)
        (W c)))

(check-expect (top (Coord 0 1)) (list (Coord 0 2) (Coord 1 2)
                                      (Coord 1 1) (Coord 1 0) (Coord 0 0)))
(check-expect (top (Coord 0 5)) (list (Coord 0 6) (Coord 1 6)
                                      (Coord 1 5) (Coord 1 4) (Coord 0 4)))

(: left : Coord -> (Listof Coord))
;; gives neighbors of a left cell
(define (left c)
  (list (N c)
        (NE c)
        (E c)
        (SE c)
        (S c)))

(check-expect (left (Coord 5 0)) (list (Coord 4 0) (Coord 4 1) (Coord 5 1)
                                       (Coord 6 1) (Coord 6 0)))
(check-expect (left (Coord 1 0)) (list (Coord 0 0) (Coord 0 1) (Coord 1 1)
                                       (Coord 2 1) (Coord 2 0)))

(: right : Coord -> (Listof Coord))
;; gives neighbors of a right cell
(define (right c)
  (list (S c)
        (SW c)
        (W c)
        (NW c)
        (N c)))

(check-expect (right (Coord 5 0)) (list (Coord 6 0) (Coord 6 -1) (Coord 5 -1)
                                        (Coord 4 -1) (Coord 4 0)))
(check-expect (right (Coord 1 0)) (list (Coord 2 0) (Coord 2 -1) (Coord 1 -1)
                                        (Coord 0 -1) (Coord 0 0)))

(: bottom : Coord -> (Listof Coord))
;; gives neighbors of a down cell
(define (bottom c)
  (list (W c)
        (NW c)
        (N c)
        (NE c)
        (E c)))

(check-expect (bottom (Coord 5 0))
              (list (Coord 5 -1) (Coord 4 -1)
                    (Coord 4 0) (Coord 4 1) (Coord 5 1)))
(check-expect (bottom (Coord 1 0))
              (list (Coord 1 -1) (Coord 0 -1)
                    (Coord 0 0) (Coord 0 1) (Coord 1 1)))

(: topleft : Coord -> (Listof Coord))
;; gives neighbors of a topleft corner cell
(define (topleft c)
  (list (E c)
        (SE c)
        (S c)))

(check-expect (topleft (Coord 0 0)) (list (Coord 0 1) (Coord 1 1) (Coord 1 0)))
(check-expect (topleft (Coord 3 4)) (list (Coord 3 5) (Coord 4 5) (Coord 4 4)))

(: topright : Coord -> (Listof Coord))
;; gives neighbors of a topright corner cell
(define (topright c)
  (list (S c)
        (SW c)
        (W c)))

(check-expect (topright (Coord 0 5)) (list (Coord 1 5) (Coord 1 4) (Coord 0 4)))
(check-expect (topright (Coord 3 4)) (list (Coord 4 4) (Coord 4 3) (Coord 3 3)))

(: bottomleft : Coord -> (Listof Coord))
;; gives neighbors of a bottomleft corner cell
(define (bottomleft c)
  (list (N c)
        (NE c)
        (E c)))

(check-expect (bottomleft (Coord 0 5))
              (list (Coord -1 5) (Coord -1 6) (Coord 0 6)))
(check-expect (bottomleft (Coord 3 4))
              (list (Coord 2 4) (Coord 2 5) (Coord 3 5)))

(: bottomright : Coord -> (Listof Coord))
;; gives neighbors of a bottomright corner cell
(define (bottomright c)
  (list (N c)
        (NW c)
        (W c)))

(check-expect (bottomright (Coord 0 5))
              (list (Coord -1 5) (Coord -1 4) (Coord 0 4)))
(check-expect (bottomright (Coord 3 4))
              (list (Coord 2 4) (Coord 2 3) (Coord 3 3)))

(: all : Coord -> (Listof Coord))
;; gives all neighboring coordinates
(define (all c)
  (list (N c)
        (NE c)
        (E c)
        (SE c)
        (S c)
        (SW c)
        (W c)
        (NW c)))

(check-expect (all (Coord 3 4))
              (list (Coord 2 4) (Coord 2 5) (Coord 3 5)
                    (Coord 4 5) (Coord 4 4) (Coord 4 3)
                    (Coord 3 3) (Coord 2 3)))
(check-expect (all (Coord 5 2))
              (list (Coord 4 2) (Coord 4 3) (Coord 5 3)
                    (Coord 6 3) (Coord 6 2) (Coord 6 1)
                    (Coord 5 1) (Coord 4 1)))

;; Complete Neighbors Function =================================================

(: neighbors : Board Coord -> (Listof Coord))
;; return the list of valid neighbors to a position
;; takes into account the boarders of any size board
;; Note: returns neighbors in a clockwise order
(define (neighbors b c)
  (match* (b c)
    [((Board w loc) (Coord row col))
     (cond
       [(and (<= (- w (sub1 w)) row (- w 2)) 
             (<= (- w (sub1 w)) col (- w 2))) ;; touches no borders
        (all c)]
       [(and (= row (sub1 w)) 
             (= col (sub1 w))) ;; bottomright
        (bottomright c)]
       [(and (= row 0) 
             (= col 0)) ;; topleft
        (topleft c)]
       [(and (= row 0)
             (= col (sub1 w))) ;; topright
        (topright c)]
       [(and (= row (sub1 w))
             (= col 0)) ;; bottomleft
        (bottomleft c)]
       [(= row 0) ;; top
        (top c)]
       [(= col 0) ;; left
        (left c)]
       [(= col (sub1 w)) ;; right
        (right c)]
       [else ;; bottom
        (bottom c)])]))
     
(check-expect (neighbors (Board 3 (list '_ '_ '_
                                        '_ '_ '_
                                        '_ '_ '_)) (Coord 1 1))
              (list (Coord 0 1)
                    (Coord 0 2)
                    (Coord 1 2)
                    (Coord 2 2)
                    (Coord 2 1)
                    (Coord 2 0)
                    (Coord 1 0)
                    (Coord 0 0)))
(check-expect (neighbors (Board 3 (list '_ '_ '_
                                        '_ '_ '_
                                        '_ '_ '_)) (Coord 0 0))
              (list (Coord 0 1)
                    (Coord 1 1)
                    (Coord 1 0)))
(check-expect (neighbors (Board 3 (list '_ '_ '_
                                        '_ '_ '_
                                        '_ '_ '_)) (Coord 1 2))
              (list (Coord 2 2)
                    (Coord 2 1)
                    (Coord 1 1)
                    (Coord 0 1)
                    (Coord 0 2)))

(: count-pieces : Board Player -> Integer)
;; count the pieces on the board belonging to the player
(define (count-pieces b p)
  (match b
    [(Board w loc)
     (match loc
       ['() 0]
       [(cons hd tl) 
        (if (player=? hd p)
            (+ 1 (count-pieces (Board w tl) p))
            (count-pieces (Board w tl) p))])]))
     
(check-expect (count-pieces (Board 3 (list 'white 'black '_
                                           'white 'white '_
                                           '_ '_ '_)) 'black)
              1)
(check-expect (count-pieces (Board 3 (list '_ 'white '_
                                           '_ 'black '_
                                           '_ 'white '_)) 'white)
              2)

;;;;; board.rkt API ============================================================

;; type exports
(provide
  Player
  (struct-out Coord)
  Cell
  (struct-out Board))

;; operations on players and cells
(provide
  other-player
  cell-is-player?
  player->string
  empty-cell?)

;; operations on coordinates
(provide
  coord+)

;; operations on boards
(provide
  make-board
  (rename-out [Board-size board-size])
  num-cells
  on-board?
  index->coord
  coord->index
  board-ref
  board-update
  neighbors
  count-pieces)

;; End of board.rkt ==============================================

(test)