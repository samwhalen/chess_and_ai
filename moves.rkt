#lang typed/racket

;; CMSC15100 Winter 2018
;; Project 2 -- moves.rkt
;; Sam Whalen
;;
;; Game logic

;; include CS151-specific definitions
(require "../include/cs151-core.rkt")

;; include testing framework
(require typed/test-engine/racket-tests)

;; include board.rkt
(require "board.rkt")

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Test Boards

(: tb6-1 : Board)
(define tb6-1
  (Board 6 (list '_ '_ '_ '_ '_ '_
                 'black 'white '_ '_ '_ '_
                 '_ '_ '_ '_ '_ '_
                 '_ '_ '_ '_ '_ '_
                 '_ '_ '_ '_ '_ '_
                 '_ '_ '_ '_ '_ '_)))

(: tb6-2 : Board)
(define tb6-2
  (Board 6 (list '_ '_ '_ '_ '_ '_
                 'black 'white '_ '_ '_ '_
                 'white 'black '_ '_ '_ '_
                 '_ '_ '_ '_ '_ '_
                 '_ '_ '_ '_ '_ '_
                 '_ '_ '_ '_ '_ '_)))

(: tb6-3 : Board)
(define tb6-3
  (Board 6 (list '_ '_ '_ '_ '_ '_
                 'black 'white '_ '_ '_ '_
                 'white 'black '_ '_ '_ 'white
                 'black '_ '_ '_ '_ 'black
                 'black '_ '_ '_ '_ 'black
                 '_ '_ '_ '_ '_ '_)))

(: tb8-1 : Board)
(define tb8-1
  (Board 8 (list '_ '_ 'white 'black '_ '_ '_ '_
                 '_ '_ 'white 'black '_ '_ '_ '_
                 '_ '_ 'white 'black '_ '_ '_ '_
                 '_ '_ 'black 'white '_ '_ '_ '_
                 '_ 'white      '_ '_ '_ '_ '_ '_
                 '_ '_ '_ '_ '_ '_ '_ '_
                 '_ '_ '_ '_ '_ '_ '_ '_
                 '_ '_ '_ '_ '_ '_ '_ '_)))

;; Utility Functions ===========================================================

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

;; Beginning of moves.rkt ======================================================

;; A game state is a Board and a next player
(define-struct Game
 ([board : Board]
  [next : Player]))

;; A move is a player and a position on the board
(define-struct Move
 ([player : Player]
  [coord : Coord]))

(: neighbor-offsets : (Listof Coord))
(define neighbor-offsets
  (list (Coord -1 0)
        (Coord -1 1)
        (Coord 0 1)
        (Coord 1 1)
        (Coord 1 0)
        (Coord 1 -1)
        (Coord 0 -1)
        (Coord -1 -1)))

(: centerwhite : Integer -> Coord)
;; given a board will return the center white coordinate given by dividing
;; w by 2 for both the row and col
(define (centerwhite n)
  (Coord (quotient n 2) (quotient n 2)))

(check-expect (centerwhite 6) (Coord 3 3))
(check-expect (centerwhite 8) (Coord 4 4))

(: make-board-int : Integer -> Board)
;; make an empty board of the specified width. Works with integers not
;; naturals
(define (make-board-int n)
  (local {(: count : Integer (Listof Cell) -> (Listof Cell))
          (define (count i acc)
            (if (> i 0)
                (count (sub1 i) (cons '_ acc))
                acc))}
    (Board n (count (sqr n) '()))))

(check-expect (make-board-int 6) (Board 6 (list '_ '_ '_ '_ '_ '_
                                            '_ '_ '_ '_ '_ '_
                                            '_ '_ '_ '_ '_ '_
                                            '_ '_ '_ '_ '_ '_
                                            '_ '_ '_ '_ '_ '_
                                            '_ '_ '_ '_ '_ '_)))
(check-expect (make-board-int 3) (Board 3 (list '_ '_ '_
                                            '_ '_ '_
                                            '_ '_ '_)))
(check-expect (make-board-int 5) (Board 5 (list '_ '_ '_ '_ '_
                                             '_ '_ '_ '_ '_
                                             '_ '_ '_ '_ '_
                                             '_ '_ '_ '_ '_
                                             '_ '_ '_ '_ '_)))

(: centerfour : Board -> Board)
;; will update an empty board to have the center four starting pieces
(define (centerfour b)
  (match b
    [(Board w loc)
     (board-update
      (board-update
       (board-update
        (board-update
         (make-board-int w)
         (centerwhite w)
         'white)
        (W (centerwhite w))
        'black)
       (NW (centerwhite w))
       'white)
      (N (centerwhite w))
      'black)]))

(check-expect (centerfour (Board 6 (list '_ '_ '_ '_ '_ '_
                                         '_ '_ '_ '_ '_ '_
                                         '_ '_ '_ '_ '_ '_
                                         '_ '_ 'black 'white '_ '_
                                         '_ '_ '_ '_ '_ '_
                                         '_ '_ '_ '_ '_ '_)))
              (Board 6 (list '_ '_ '_ '_ '_ '_
                             '_ '_ '_ '_ '_ '_
                             '_ '_ 'white 'black '_ '_
                             '_ '_ 'black 'white '_ '_
                             '_ '_ '_ '_ '_ '_
                             '_ '_ '_ '_ '_ '_)))
(check-expect (centerfour (Board 8 (list '_ '_ '_ '_ '_ '_ '_ '_
                                         '_ '_ '_ '_ '_ '_ '_ '_
                                         '_ '_ '_ '_ '_ '_ '_ '_
                                         '_ '_ '_ '_ '_ '_ '_ '_ 
                                         '_ '_ '_ '_ '_ '_ '_ '_
                                         '_ '_ '_ '_ '_ '_ '_ '_
                                         '_ '_ '_ '_ '_ '_ '_ '_
                                         '_ '_ '_ '_ '_ '_ '_ '_)))
                          (Board 8 (list '_ '_ '_ '_ '_ '_ '_ '_
                                         '_ '_ '_ '_ '_ '_ '_ '_
                                         '_ '_ '_ '_ '_ '_ '_ '_
                                         '_ '_ '_ 'white 'black '_ '_ '_ 
                                         '_ '_ '_ 'black 'white '_ '_ '_
                                         '_ '_ '_ '_ '_ '_ '_ '_
                                         '_ '_ '_ '_ '_ '_ '_ '_
                                         '_ '_ '_ '_ '_ '_ '_ '_))) 
                   
(: initial-board : Natural -> Board)
;; create an initial board of the given size, which should be even and
;; between 6 and 16, and four pieces (two of each color) in the
;; center.  Signal an error if the width is invalid
(define (initial-board n)
  (if (and (even? n)
           (<= 6 n 16))
      (centerfour (make-board n))
      (error "make-board: invalid width")))

(check-expect (initial-board 6) (Board 6 (list '_ '_ '_ '_ '_ '_
                                               '_ '_ '_ '_ '_ '_
                                               '_ '_ 'white 'black '_ '_
                                               '_ '_ 'black 'white '_ '_
                                               '_ '_ '_ '_ '_ '_
                                               '_ '_ '_ '_ '_ '_)))
(check-expect (initial-board 8) (Board 8 (list '_ '_ '_ '_ '_ '_ '_ '_
                                               '_ '_ '_ '_ '_ '_ '_ '_
                                               '_ '_ '_ '_ '_ '_ '_ '_
                                               '_ '_ '_ 'white 'black '_ '_ '_ 
                                               '_ '_ '_ 'black 'white '_ '_ '_
                                               '_ '_ '_ '_ '_ '_ '_ '_
                                               '_ '_ '_ '_ '_ '_ '_ '_
                                               '_ '_ '_ '_ '_ '_ '_ '_)))                                          


(: make-game : Natural -> Game)
;; make a game in its initial state with the specified board width
(define (make-game n)
  (Game (initial-board n) 'black))

(check-error (make-game 5) "make-board: invalid width")
(check-error (make-game 15) "make-board: invalid width")
(check-expect (make-game 6) (Game (Board 6 (list '_ '_ '_ '_ '_ '_
                                                 '_ '_ '_ '_ '_ '_
                                                 '_ '_ 'white 'black '_ '_
                                                 '_ '_ 'black 'white '_ '_
                                                 '_ '_ '_ '_ '_ '_
                                                 '_ '_ '_ '_ '_ '_)) 'black))
(check-expect (make-game 10)
              (Game (Board 10 (list '_ '_ '_ '_ '_ '_ '_ '_ '_ '_
                                    '_ '_ '_ '_ '_ '_ '_ '_ '_ '_
                                    '_ '_ '_ '_ '_ '_ '_ '_ '_ '_
                                    '_ '_ '_ '_ '_ '_ '_ '_ '_ '_
                                    '_ '_ '_ '_ 'white 'black '_ '_ '_ '_
                                    '_ '_ '_ '_ 'black 'white '_ '_ '_ '_
                                    '_ '_ '_ '_ '_ '_ '_ '_ '_ '_
                                    '_ '_ '_ '_ '_ '_ '_ '_ '_ '_
                                    '_ '_ '_ '_ '_ '_ '_ '_ '_ '_
                                    '_ '_ '_ '_ '_ '_ '_ '_ '_ '_)) 'black))

;; Helper Functions for Outflanks? =============================================

(: outflanksdirection? : Board Move (Coord -> Coord) -> Boolean)
;; checks in any direction of the proposed flanking cell to see
;; if it's a valid move
;; Note: can only be used after checkdirection? has been run on the same
;; board with the same direction input
(define (outflanksdirection? b m dir)
  (match b
    [(Board w loc)
     (match m
       [(Move p c)
        (local
          {(: checkdirection? : Board Move (Coord -> Coord) -> Boolean)
           ;; returns true if there is a piece of the opposite color of the
           ;; moving piece adjacent to the proposed cell in the direction
           ;; specified
           (define (checkdirection? b m dir)
             (and (on-board? b (dir c))
                  (player=? (other-player p) (board-ref b (dir c)))))}
          (and (checkdirection? b m dir)
               (on-board? b (dir (dir c)))
               (not (empty-cell? (board-ref b (dir (dir c)))))
               (match (board-ref b (dir (dir c)))
                 ['black (or (player=? 'black p)
                             (outflanksdirection? b (Move p (dir c)) dir))]
                 ['white (or (player=? 'white p)
                             (outflanksdirection? b (Move p (dir c)) dir))])))])]))

(check-expect (outflanksdirection?
               (Board 6 (list '_ '_ '_ '_ '_ '_
                              '_ '_ '_ '_ '_ '_
                              '_ '_ 'white 'black '_ '_
                              '_ '_ 'black 'white '_ '_
                              '_ '_ '_ '_ '_ '_
                              '_ '_ '_ '_ '_ '_))
               (Move 'black (Coord 3 4))
               W) #t)
(check-expect (outflanksdirection?
               (Board 6 (list '_ '_ '_ '_ '_ '_
                              '_ '_ '_ '_ '_ '_
                              '_ '_ 'white 'black '_ '_
                              '_ 'black 'white 'white '_ '_
                              '_ '_ '_ '_ '_ '_
                              '_ '_ '_ '_ '_ '_))
               (Move 'black (Coord 3 4))
               W) #t)
(check-expect (outflanksdirection?
               (Board 6 (list '_ '_ '_ '_ '_ '_
                              '_ '_ '_ '_ '_ '_
                              '_ '_ '_ '_ '_ 'black
                              'white 'white 'white 'white '_ '_
                              '_ '_ '_ '_ '_ '_
                              '_ '_ '_ '_ '_ '_))
               (Move 'black (Coord 3 4))
               W) #f)
(check-expect (outflanksdirection?
               (Board 6 (list '_ '_ '_ '_ '_ '_
                              '_ '_ '_ '_ '_ '_
                              '_ '_ 'white 'black '_ '_
                              '_ '_ 'black 'white '_ '_
                              '_ '_ '_ '_ '_ '_
                              '_ '_ '_ '_ '_ '_))
               (Move 'black (Coord 1 2))
               S) #t)

(define dirlist
  (list N NE E SE S SW W NW))
         
(: outflanks? : Board Move -> Boolean)
;; checks to see if a proposed move will outflank the opponent and
;; hence be valid
(define (outflanks? b m)
  (match m
    [(Move plr c)
     (local {(: cycler : Board Move (Listof (Coord -> Coord)) -> Boolean)
             (define (cycler b m lof)
               (match lof
                 ['() #f]
                 [(cons hd tl)
                  (or (outflanksdirection? b m hd)
                      (cycler b m tl))]))}
       (and (on-board? b c)
            (empty-cell? (board-ref b c))
            (cycler b m dirlist)))]))
           
           
(check-expect (outflanks?
               (Board 6 (list '_ '_ '_ '_ '_ '_
                              '_ '_ '_ '_ '_ '_
                              '_ '_ '_ '_ '_ 'black
                              'white 'white 'white 'white '_ '_
                              '_ '_ '_ '_ '_ '_
                              '_ '_ '_ '_ '_ '_))
               (Move 'black (Coord 3 4))) #f)
(check-expect (outflanks?
               (Board 6 (list '_ '_ '_ '_ '_ '_
                              '_ '_ '_ '_ '_ '_
                              '_ '_ 'white 'black '_ '_
                              '_ '_ 'black 'white '_ '_
                              '_ '_ '_ '_ '_ '_
                              '_ '_ '_ '_ '_ '_))
               (Move 'black (Coord 3 4))) #t)
(check-expect (outflanks?
               (Board 6 (list '_ '_ '_ '_ '_ '_
                              '_ '_ '_ '_ '_ '_
                              '_ '_ 'white 'black '_ '_
                              '_ 'black 'white 'white '_ '_
                              '_ '_ '_ '_ '_ '_
                              '_ '_ '_ '_ '_ '_))
               (Move 'black (Coord 3 4))) #t)
(check-expect (outflanks?
               (Board 6 (list '_ '_ '_ '_ '_ '_
                              '_ '_ '_ '_ '_ '_
                              '_ '_ 'white 'black '_ '_
                              '_ '_ 'black 'white '_ '_
                              '_ '_ '_ '_ 'black '_
                              '_ '_ '_ '_ '_ '_))
               (Move 'black (Coord 1 1))) #t)
(check-expect (outflanks?
               (Board 6 (list '_ '_ '_ '_ '_ '_
                              '_ '_ '_ '_ '_ '_
                              '_ '_ 'white 'black '_ '_
                              '_ '_ 'black 'white '_ '_
                              '_ '_ '_ '_ 'black '_
                              '_ '_ '_ '_ '_ '_))
               (Move 'white (Coord 5 2))) #f)
(check-expect (outflanks?
               (Board 6 (list '_ '_ '_ '_ '_ '_
                              '_ '_ '_ '_ '_ '_
                              '_ 'white 'white 'black '_ '_
                              '_ 'white 'black 'white '_ '_
                              '_ 'black '_ '_ 'black '_
                              '_ '_ '_ '_ '_ '_))
               (Move 'black (Coord 1 1))) #t)

;; Flips Helpers ===============================================================

(: cells-to-flip :
   Board Coord Player (Coord -> Coord) (Listof Coord) -> (Listof Coord))
;; will take in a board, the cell of the proposed move, the player of
;; the proposed move, and a valid "flanking direction" determined by
;; the outflanks? function, and return a list of the cells to flip on
;; completion of the move using TAIL recursion
(define (cells-to-flip b pc pp dir acc)
  (if (not (player=? pp (board-ref b (dir pc))))
      (cells-to-flip b (dir pc) pp dir (append acc (list (dir pc))))
      acc))

(check-expect (cells-to-flip
               (Board 6 (list '_ '_ '_ '_ '_ '_
                              '_ '_ '_ '_ '_ '_
                              '_ '_ 'white 'black '_ '_
                              '_ '_ 'black 'white '_ '_
                              '_ '_ '_ '_ '_ '_
                              '_ '_ '_ '_ '_ '_))
               (Coord 2 1)
               'black
               E
               '())
              (list (Coord 2 2)))
(check-expect (cells-to-flip
               (Board 6 (list '_ '_ 'white 'black '_ '_
                              '_ '_ 'white 'black '_ '_
                              '_ '_ 'white 'black '_ '_
                              '_ '_ 'black 'white '_ '_
                              '_ 'white '_ '_ '_ '_
                              '_ '_ '_ '_ '_ '_))
               (Coord 1 4)
               'white
               SW
               '())
              (list (Coord 2 3)
                    (Coord 3 2)))

(: flips : Board Move -> (Listof Coord))
;; given a board and a move, returns the (possibly empty) list
;; of coordinates of cells containing pieces to flip if that move is made
(define (flips b m)
  (match b
    [(Board w loc)
     (match m
       [(Move p c)
        (local
          {(: cycler : Board Move (Listof (Coord -> Coord)) -> (Listof Coord))
           (define (cycler b m lof)
             (match lof
               ['() '()]
               [(cons hd tl)
                (if (outflanksdirection? b m hd)
                    (append (cells-to-flip b c p hd '())
                            (cycler b m tl))
                    (cycler b m tl))]))}
          (cycler b m dirlist))])]))
        
(check-expect (flips
               (Board 6 (list '_ '_ 'white 'black '_ '_
                              '_ '_ 'white 'black '_ '_
                              '_ '_ 'white 'black '_ '_
                              '_ '_ 'black 'white '_ '_
                              '_ 'white '_ '_ '_ '_
                              '_ '_ '_ '_ '_ '_))
               (Move 'black (Coord 1 1)))
              (list (Coord 1 2)))
(check-expect (flips
               (Board 6 (list '_ '_ 'white 'black '_ '_
                              '_ '_ 'white 'black '_ '_
                              '_ '_ 'white 'black '_ '_
                              '_ '_ 'black 'white '_ '_
                              '_ 'white '_ '_ '_ '_
                              '_ '_ '_ '_ '_ '_))
               (Move 'white (Coord 1 4)))
              (list (Coord 2 3)
                    (Coord 3 2)
                    (Coord 1 3)))
(check-expect (flips
               (Board 6 (list '_ '_ 'white 'black '_ '_
                              '_ '_ 'white 'black '_ '_
                              '_ '_ 'white 'black '_ '_
                              '_ '_ 'black 'white '_ '_
                              '_ '_ '_ '_ '_ '_
                              '_ '_ '_ '_ '_ '_))
               (Move 'white (Coord 0 5)))
              '())
(check-expect (flips
               (Board 6 (list '_ '_ 'white 'black '_ '_
                              '_ '_ 'white 'black '_ '_
                              '_ '_ 'white 'black '_ '_
                              '_ '_ 'black 'white '_ '_
                              '_ '_ '_ '_ '_ '_
                              '_ '_ '_ '_ '_ '_))
               (Move 'white (Coord 5 5)))
              '())
(check-expect (flips
               (Board 6 (list '_ '_ 'white 'black '_ '_
                              '_ '_ 'white 'black '_ '_
                              '_ '_ 'white 'black '_ '_
                              '_ '_ 'black 'white '_ '_
                              '_ '_ '_ '_ '_ '_
                              '_ '_ '_ '_ '_ '_))
               (Move 'white (Coord 0 4)))
              (list (Coord 1 3)
                    (Coord 0 3)))
(check-expect (flips
               (Board 6 (list '_ '_ 'white 'black '_ '_
                              '_ '_ 'white 'black 'black '_
                              '_ '_ 'white 'black '_ '_
                              '_ '_ 'black 'white '_ '_
                              '_ 'black '_ '_ '_ '_
                              'white '_ '_ '_ '_ '_))
               (Move 'white (Coord 0 5)))
              (list (Coord 1 4)
                    (Coord 2 3)
                    (Coord 3 2)
                    (Coord 4 1)))
(check-expect (flips
               (Board 6 (list '_ 'white 'white 'white '_ '_
                              '_ 'white '_ 'white '_ '_
                              '_ 'white 'white 'white '_ '_
                              '_ '_ '_ '_ '_ '_
                              '_ '_ '_ '_ '_ '_
                              '_ '_ '_ '_ '_ '_))
               (Move 'black (Coord 1 2)))
              '())
(check-expect (flips
               (Board 6 (list   '_   'white 'white 'white   '_   '_
                              'black 'white   '_   'white 'black '_
                                '_   'white 'white 'white   '_   '_
                                'black '_   'white   '_   'black '_
                                '_     '_   'white   '_     '_   '_
                                '_     '_   'black   '_     '_   '_))
               (Move 'black (Coord 1 2)))
              (list (Coord 1 3)
                    (Coord 2 3)
                    (Coord 2 2)
                    (Coord 3 2)
                    (Coord 4 2)
                    (Coord 2 1)
                    (Coord 1 1)))
(check-expect (flips
               (Board 8 (list '_ '_ 'white 'black '_ '_ '_ '_
                              '_ '_ 'white 'black '_ '_ '_ '_
                              '_ '_ 'white 'black '_ '_ '_ '_
                              '_ '_ 'black 'white '_ '_ '_ '_
                              '_ 'white '_ '_ '_ '_ '_ '_
                              '_ '_ '_ '_ '_ '_ '_ '_
                              '_ '_ '_ '_ '_ '_ '_ '_
                              '_ '_ '_ '_ '_ '_ '_ '_))
               (Move 'black (Coord 0 0)))
              '())
(check-expect (flips
               (Board 8 (list '_ '_ 'white 'black '_ '_ '_ '_
                              '_ '_ 'white 'black '_ '_ '_ '_
                              '_ '_ 'white 'black '_ '_ '_ '_
                              '_ '_ 'black 'white '_ '_ '_ '_
                              '_ 'white '_ '_ '_ '_ '_ '_
                              '_ '_ '_ '_ '_ '_ '_ '_
                              '_ '_ '_ '_ '_ '_ '_ '_
                              '_ '_ '_ '_ '_ '_ '_ '_))
               (Move 'white (Coord 1 4)))
              (list (Coord 2 3)
                    (Coord 3 2)
                    (Coord 1 3)))
(check-expect (flips
               (Board 8 (list '_ '_ 'white 'black '_ '_ '_ '_
                              '_ '_ 'white 'black '_ '_ '_ '_
                              '_ '_ 'white 'black '_ '_ '_ '_
                              '_ '_ 'black 'white '_ '_ '_ '_
                              '_ 'white '_ '_ '_ '_ '_ '_
                              '_ '_ '_ '_ '_ '_ '_ '_
                              '_ '_ '_ '_ '_ '_ '_ '_
                              '_ '_ '_ '_ '_ '_ '_ '_))
               (Move 'black (Coord 1 4)))
              '())

;; Beginning of Apply-move =====================================================

;; NOTE TO GRADER: Professor Tim Black explicitly said that this does NOT have
;; to be a local function. Please do not take off points again. Thank you.
(: curryformap : Board -> (Coord -> Integer))
;; takes in a board to prepare the coord->index function for the map function
(define (curryformap b)
  (lambda ([c : Coord])
    (coord->index b c)))

(check-expect ((curryformap (Board 3 (list '_ '_ '_
                                           '_ '_ '_
                                           '_ '_ '_)))
               (Coord 1 3))
              6)
(check-expect ((curryformap (Board 6 (list '_ '_ '_ '_ '_ '_
                                           '_ '_ '_ '_ '_ '_
                                           '_ '_ '_ '_ '_ '_
                                           '_ '_ '_ '_ '_ '_
                                           '_ '_ '_ '_ '_ '_
                                           '_ '_ '_ '_ '_ '_)))
               (Coord 4 5))
              29)
(check-expect ((curryformap (Board 6 (list '_ '_ '_ '_ '_ '_
                                           '_ '_ '_ '_ '_ '_
                                           '_ '_ '_ '_ '_ '_
                                           '_ '_ '_ '_ '_ '_
                                           '_ '_ '_ '_ '_ '_
                                           '_ '_ '_ '_ '_ '_)))
               (Coord 3 1))
              19)
               
(: coord->indmap :
   (Board Coord -> Integer) Board (Listof Coord) -> (Listof Integer))
;; helper function that serves the same purpose as map but that I had to
;; re-write to accomadate functions of two inputs (like Coord->index)
(define (coord->indmap f b loc)
  (match loc
    ['() '()]
    [(cons hd tl)
     (cons (f b hd) (coord->indmap f b tl))]))

;; for the above, instead of not using map, write a new function -- a local in apply move
;; where we can feed the board width into coord->index so that map can take in
;; just a coord -> integer function instead of Board Coord -> Integer

(check-expect (coord->indmap coord->index (Board 3 (list '_ '_ '_
                                                       '_ '_ '_
                                                       '_ '_ '_))
                           (list (Coord 2 2)
                                 (Coord 1 1)
                                 (Coord 0 0)))
              (list 8 4 0))
(check-expect (coord->indmap coord->index (Board 6 (list '_ '_ '_ '_ '_ '_
                                                       '_ '_ '_ '_ '_ '_
                                                       '_ '_ '_ '_ '_ '_
                                                       '_ '_ '_ '_ '_ '_
                                                       '_ '_ '_ '_ '_ '_
                                                       '_ '_ '_ '_ '_ '_))
                           (list (Coord 2 2)
                                 (Coord 1 1)
                                 (Coord 0 0)))
              (list 14 7 0))
(check-expect (coord->indmap coord->index (Board 3 (list '_ '_ '_
                                                       '_ '_ '_
                                                       '_ '_ '_))
                           '())
              '())

(: insert : Integer (Listof Integer) -> (Listof Integer))
;; will insert the given integer into the list
(define (insert n loi)
  (match loi
    ['() (list n)]
    [(cons hd tl)
     (if (< n hd)
         (cons n loi)
         (cons hd (insert n tl)))]))

(check-expect (insert 5 (list 1 2 5 7))
              (list 1 2 5 5 7))
(check-expect (insert 1 (list 1 2 3 4))
              (list 1 1 2 3 4))
(check-expect (insert -1 (list 1 2 3 4))
              (list -1 1 2 3 4))
    
(: insertion-sort : (Listof Integer) -> (Listof Integer))
;; will sort a given list of integers in ascending order
(define (insertion-sort loi)
  (foldl insert '() loi))

(check-expect (insertion-sort (list 1 3 2 76 3 4))
              (list 1 2 3 3 4 76))
(check-expect (insertion-sort (list -5 -3 34 -1 0 1 4 3))
              (list -5 -3 -1 0 1 3 4 34))
(check-expect (insertion-sort '())
              '())

(: other-cell : Cell -> Cell)
;; returns the opposite color of a cell if occupied, if empty
;; then returns empty
(define (other-cell c)
  (match c
    ['_ '_]
    ['black 'white]
    ['white 'black]))

(check-expect (other-cell '_) '_)
(check-expect (other-cell 'black) 'white)
(check-expect (other-cell 'white) 'black)

(: make-color : Player -> Cell)
;; flips cells a certain color
(define (make-color p)
  (match p
    ['black 'black]
    ['white 'white]))

(check-expect (make-color 'black) 'black)
(check-expect (make-color 'white) 'white)

(: efficient-flip : (Listof Integer) (Listof Cell) Integer Player -> (Listof Cell))
;; efficient-flip takes in a list of indices and a partial list of the board
;; and the current position in the
(define (efficient-flip loi loc acc p)
  (match* (loi loc)
    [(_ '()) '()]
    [('() _) loc]
    [((cons hd tl) (cons x xr))
     (if (>= acc hd)
         (cons (make-color p)
               (efficient-flip tl xr (add1 acc) p))
         (cons x (efficient-flip loi xr (add1 acc) p)))]))

(check-expect (efficient-flip (list 1 2 3 4)
                     (list 'white 'black 'black 'white 'white)
                     0
                     'black)
              (list 'white 'black 'black 'black 'black))
(check-expect (efficient-flip '()
                     (list 'black 'white 'black '_)
                     0
                     'white)
              (list 'black 'white 'black '_))
 
(: apply-move : Game Move -> Game)
;; takes a game state and a move, determines which pieces are flipped,
;; and flips them. It should signal the error "apply-move: illegal move"
;; when given an illegal move
(define (apply-move g m)
  (match* (g m)
    [((Game b next) (Move p c))
     (match b
       [(Board w loc)
        (match (flips b m)
          ['() (error "apply-move: illegal move")]
          [(cons hd tl)
           (Game
            (Board w
                   (efficient-flip
                    (insertion-sort
                     (map
                      (curryformap b)
                      (cons c (flips b m))))
                     loc
                     0
                     p))
            (other-player next))])])]))

(check-expect
 (apply-move
  (Game (Board 6 (list '_ '_ '_ '_ '_ '_
                       'black 'white '_ '_ '_ '_
                       '_ '_ '_ '_ '_ '_
                       '_ '_ '_ '_ '_ '_
                       '_ '_ '_ '_ '_ '_
                       '_ '_ '_ '_ '_ '_))
        'black)
  (Move 'black (Coord 1 2)))
 (Game (Board 6 (list '_ '_ '_ '_ '_ '_
                       'black 'black 'black'_ '_ '_
                       '_ '_ '_ '_ '_ '_
                       '_ '_ '_ '_ '_ '_
                       '_ '_ '_ '_ '_ '_
                       '_ '_ '_ '_ '_ '_))
       'white))
(check-expect
 (apply-move
  (Game (Board 6 (list   '_   'white 'white 'white   '_   '_
                         'black 'white   '_   'white 'black '_
                         '_   'white 'white 'white   '_   '_
                         'black '_   'white   '_   'black '_
                         '_     '_   'white   '_     '_   '_
                         '_     '_   'black   '_     '_   '_))
        'black)
  (Move 'black (Coord 1 2)))
 (Game (Board 6 (list   '_   'white 'white 'white   '_   '_
                         'black 'black   'black   'black 'black '_
                         '_   'black 'black 'black   '_   '_
                         'black '_   'black   '_   'black '_
                         '_     '_   'black   '_     '_   '_
                         '_     '_   'black   '_     '_   '_))
       'white))
(check-error
 (apply-move
  (Game (Board 6 (list   '_   'white 'white 'white   '_   '_
                         'black 'white   '_   'white 'black '_
                         '_   'white 'white 'white   '_   '_
                         'black '_   'white   '_   'black '_
                         '_     '_   'white   '_     '_   '_
                         '_     '_   'black   '_     '_   '_))
        'black)
  (Move 'white (Coord 5 5)))
 "apply-move: illegal move")
(check-error
 (apply-move
  (Game (Board 6 (list   '_   'white 'white 'white   '_   '_
                         'black 'white   '_   'white 'black '_
                         '_   'white 'white 'white   '_   '_
                         'black '_   'white   '_   'black '_
                         '_     '_   'white   '_     '_   '_
                         '_     '_   'black   '_     '_   '_))
        'black)
  (Move 'black (Coord 0 0)))
 "apply-move: illegal move")

;; Changes to moves.rkt ========================================================

(: valid-move? : Game -> Coord -> Boolean)
;; curried function for testing if placing a piece at a given position is valid
(define (valid-move? g)
  (match g
    [(Game b next)
     (lambda ([c : Coord])
       (outflanks? b (Move next c)))]))

(check-expect
 ((valid-move?
   (Game
    (Board 6
           (list '_ '_ '_ '_ '_ '_
                 'black 'white 'black '_ '_ '_
                 '_ '_ '_ '_ '_ '_
                 '_ '_ '_ '_ '_ '_
                 '_ '_ '_ '_ '_ '_
                 '_ '_ '_ '_ '_ '_))
    'black))
  (Coord 1 2))
 #f)
(check-expect
 ((valid-move?
   (Game
    (Board 6
           (list '_ '_ '_ '_ '_ '_
                 'black 'white '_ '_ '_ '_
                 '_ '_ '_ '_ '_ '_
                 '_ '_ '_ '_ '_ '_
                 '_ '_ '_ '_ '_ '_
                 '_ '_ '_ '_ '_ '_))
    'black))
  (Coord 1 2))
 #t)
(check-expect
 ((valid-move?
   (Game
    (initial-board 6) 'black))
  (Coord 3 4))
 #t)
(check-expect
 ((valid-move?
   (Game
    (initial-board 6) 'black))
  (Coord 3 3))
 #f)
   
(: skip-move : Game -> Game)
;; The skip-move function just flips the current player in a game.
;; It is used when a player is forced to skip her current move
(define (skip-move g)
  (match g
    [(Game b next)
     (Game b (other-player next))]))

(check-expect (skip-move
               (Game (initial-board 8)
                     'black))
              (Game (initial-board 8)
                    'white))
(check-expect (skip-move
               (Game (initial-board 16)
                     'white))
              (Game (initial-board 16)
                    'black))

(: count-flips : Board Move -> Integer)
;; count the number of flips if the given move is made. Return 0 on
;; invalid moves (e.g., no flips or the square already has a piece in it)
(define (count-flips brd move)
  (match move
    [(Move player start)
     (if (empty-cell? (board-ref brd start))
         (local
           {(: count-flips-in-dir : Coord Integer -> Integer)
            ;; count the number of flips in the specified direction;
            ;; the second argument is an accumulator of the total
            ;; number of flips
            (define (count-flips-in-dir dir nflips)
              (local
                {(: count : Coord Integer -> Integer)
                 ;; helper function for counting flips in the direction dir
                 (define (count coord n)
                   (if (on-board? brd coord)
                       (match (board-ref brd coord)
                         ['_ nflips] ;; not an out-flanking move
                         [p (if (symbol=? p player)
                                (+ n nflips)
                                (count (coord+ coord dir) (+ n 1)))])
                       nflips))} ;; not an out-flanking move
                (count (coord+ start dir) 0)))}
           (foldl count-flips-in-dir 0 neighbor-offsets))
         0)]))

;; no tests needed for provided code

(: possible-moves : Game -> (Listof Move))
;; The possible-moves function takes a game and returns a list of
;; the legal moves for the next player. The list will be empty if
;; there are no legal moves, which means that the player will have
;; to skip her turn
(define (possible-moves g)
  (match g
    [(Game b next)
     (match b
       [(Board sz cls)
        (local
          {(: board-moves : Integer Integer Integer (Listof Move)
              -> (Listof Move))
           ;; will generate a list of valid coordinates
           (define (board-moves sz rowacc colacc moveacc)
             (cond
               [(and (< colacc sz)
                     (< rowacc sz))
                (if ((valid-move? g) (Coord rowacc colacc))
                    (board-moves
                     sz
                     rowacc
                     (add1 colacc)
                     (cons (Move next (Coord rowacc colacc))
                           moveacc))
                    (board-moves
                     sz
                     rowacc
                     (add1 colacc)
                     moveacc))]
               [(< rowacc sz)
                (if ((valid-move? g) (Coord rowacc colacc))
                    (board-moves
                     sz
                     (add1 rowacc)
                     0
                     (cons (Move next (Coord rowacc colacc))
                           moveacc))
                    (board-moves
                     sz
                     (add1 rowacc)
                     0
                     moveacc))]
               [else moveacc]))}

          (reverse (board-moves sz 0 0 '())))])]))

(check-expect
 (possible-moves
  (Game (Board 6 (list '_ '_ '_ '_ '_ '_
                       'black 'white '_ '_ '_ '_
                       '_ '_ '_ '_ '_ '_
                       '_ '_ '_ '_ '_ '_
                       '_ '_ '_ '_ '_ '_
                       '_ '_ '_ '_ '_ '_))
        'black))
 (list (Move 'black (Coord 1 2))))
(check-expect
 (possible-moves
  (Game (initial-board 6)
        'black))
 (list (Move 'black (Coord 1 2))
       (Move 'black (Coord 2 1))
       (Move 'black (Coord 3 4))
       (Move 'black (Coord 4 3))))
(check-expect
 (possible-moves
  (Game (initial-board 8)
        'black))
 (list (Move 'black (Coord 2 3))
       (Move 'black (Coord 3 2))
       (Move 'black (Coord 4 5))
       (Move 'black (Coord 5 4))))

(check-expect
 (possible-moves
  (Game tb6-3 'white))
 (list (Move 'white (Coord 0 0))
       (Move 'white (Coord 2 2))
       (Move 'white (Coord 3 1))
       (Move 'white (Coord 5 0))
       (Move 'white (Coord 5 5))))

(: move-possible? : Game -> Boolean)
;; The move-possible? function determines if the next player in
;; a game state has a legal move.
(define (move-possible? g)
  (match g
    [(Game b next)
     (match b
       [(Board sz cls)
        (local
          {(: first-outflank : Integer -> Boolean)
           ;; will look for the first outflank
           (define (first-outflank index)
             (and (< index (sub1 (sqr sz)))
                  (or (outflanks? b (Move next (index->coord b index)))
                      (first-outflank (add1 index)))))}
          (first-outflank 0))])]))

(check-expect
 (move-possible?
  (Game (initial-board 8)
        'black))
 #t)
(check-expect
 (move-possible?
  (Game (initial-board 16)
        'white))
 #t)
(check-expect
 (move-possible?
  (Game
   (Board 6 (list '_ '_ '_ '_ '_ '_
                  '_ '_ '_ '_ '_ '_
                  '_ '_ '_ '_ '_ '_
                  '_ '_ '_ '_ '_ '_
                  '_ '_ '_ '_ '_ '_
                  '_ '_ '_ '_ '_ '_))
        'black))
 #f)
(check-expect
 (move-possible?
  (Game
   (Board 6 (list '_ '_ '_ '_ '_ '_
                  '_ '_ '_ '_ '_ '_
                  '_ '_ '_ '_ '_ '_
                  '_ '_ '_ '_ '_ '_
                  '_ '_ '_ '_ '_ '_
                  '_ '_ '_ '_ '_ '_))
        'white))
 #f)

(: game-over? : Game -> Boolean)
;; this function checks to see if the game is over
(define (game-over? g)
  (and (not (move-possible? g))
       (not (move-possible? (skip-move g)))))
       
(check-expect (game-over? (Game (initial-board 10) 'black))
              #f)
(check-expect (game-over? (Game
                           (Board 6 (list '_ '_ '_ '_ '_ '_
                                          '_ '_ '_ '_ '_ '_
                                          '_ '_ '_ '_ '_ '_
                                          '_ '_ '_ '_ '_ '_
                                          '_ '_ '_ '_ '_ '_
                                          '_ '_ '_ '_ '_ '_))
                           'black))
              #t)
(check-expect (game-over? (Game (initial-board 12) 'black))
              #f)

;;;;; moves.rkt API ============================================================

;; type exports
;;
(provide
  (struct-out Game)
  (struct-out Move))

;; project-2 exports
(provide
  valid-move?
  skip-move
  count-flips
  possible-moves
  move-possible?
  game-over?)

;; operations
(provide
  initial-board
  make-game
  outflanks?
  flips
  apply-move)

;(test)