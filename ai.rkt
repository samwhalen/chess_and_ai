#lang typed/racket

;; CMSC15100 Winter 2018
;; Project 2 -- ai.rkt
;; Sam

;; include CS151-specific definitions
(require "../include/cs151-core.rkt")

;; include testing framework
(require typed/test-engine/racket-tests)

;; include board.rkt
(require "board.rkt")

;; include moves.rkt
(require "moves.rkt")

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

(: tb6-4 : Board)
(define tb6-4
  (Board 6 (list '_ '_ '_ '_ '_ '_
                 'black 'white '_ '_ '_ 'white
                 'white 'black '_ '_ '_ 'black
                 'black '_ '_ '_ '_ 'black
                 'black '_ '_ '_ '_ '_
                 '_ '_ '_ '_ '_ '_)))

(: tb6-5 : Board)
(define tb6-5
  (Board 6 (list '_ '_ '_ '_ '_ '_
                 'black '_ '_ '_ '_ 'white
                 'white '_ '_ '_ '_ 'black
                 'black '_ '_ '_ '_ 'black
                 'black '_ '_ '_ '_ '_
                 '_ '_ '_ '_ '_ '_)))

(: tb6-6 : Board)
(define tb6-6
  (Board 6 (list '_ '_ '_ '_ '_ '_
                 'black '_ '_ '_ '_ 'white
                 'white '_ '_ '_ '_ 'black
                 'black '_ '_ '_ '_ 'black
                 'black '_ '_ '_ '_ 'black
                 '_ '_ '_ '_ '_ '_)))

(: tb6-7 : Board)
(define tb6-7
  (Board 6 (list '_ '_ '_ '_ '_ '_
                 'black '_ '_ '_ '_ '_
                 '_ '_ '_ '_ '_ '_
                 '_ '_ '_ '_ 'white '_
                 '_ 'white '_ '_ '_ '_
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

(: tb8-2 : Board)
(define tb8-2
  (Board 8 (list '_ '_ 'white 'black '_ '_ '_ '_
                 '_ '_ 'white 'black 'black '_ '_ '_
                 '_ '_ 'white 'black '_ '_ '_ '_
                 '_ '_ 'black 'white '_ '_ '_ '_
                 '_ 'white      '_ '_ '_ '_ '_ '_
                 '_ '_ '_ '_ '_ '_ '_ '_
                 '_ '_ '_ '_ '_ '_ '_ '_
                 '_ '_ '_ '_ '_ '_ '_ '_)))

(: tb8-3 : Board)
(define tb8-3
  (Board 8 (list '_ 'black 'white 'black 'white 'black '_ '_
                 '_ '_ 'white 'black 'white '_ '_ '_
                 '_ '_ 'white 'black 'white '_ '_ '_
                 '_ 'white 'black 'white '_ '_ '_ '_
                 '_ '_ '_ '_ '_ '_ '_ '_
                 '_ '_ '_ '_ '_ '_ '_ '_
                 '_ '_ '_ '_ '_ '_ '_ '_
                 '_ '_ '_ '_ '_ '_ '_ '_)))

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

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; DATA DEFINITIONS

;; A Strategy is a function for determining a move; it returns
;; 'Skip when there is no move available for the current player
;;
(define-type Strategy (Game -> (U Move 'Skip)))

;; A evaluator evaluates the state of a game for the next player
;; and returns a signed integer value, where zero is neutral, positive
;; values are favorable for the next player, and negative values
;; mean a favorable position for the other player
;;
(define-type Evaluator (Game -> Integer))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; STRATEGIES

(: first-move : Strategy)
;; pick the first valid move on the board in order of increasing index
;;
(define (first-move game)
  (local
    {(define brd : Board (Game-board game))
     (define player : Player (Game-next game))
     (: find : Integer (Listof Cell) -> (U Move 'Skip))
     ;; find the first valid move or else return 'Skip
     (define (find ix cells)
       (match cells
        ['() 'Skip]
        [(cons '_ cellr)
         (local {(define mv (Move player (index->coord brd ix)))}
          (if (outflanks? brd mv)
              mv
              (find (+ ix 1) cellr)))]
        [(cons _ cellr) (find (+ ix 1) cellr)]))}
    (find 0 (Board-cells brd))))

(: maximize-flips : Strategy)
;; Pick the valid move that causes the maximum number of flips.  If there is more
;; than one move that maximizes flips, then pick the first in cell-index order.
(define (maximize-flips g)
  (local
    {(define brd : Board (Game-board g))
     (define player : Player (Game-next g))
     (: find : Integer Integer Move (Listof Cell) -> Move)
     ;; find the first move that flips the most cells or else return 'Skip
     (define (find ix acc moveacc loc)
       (match loc
        ['() moveacc]
        [(cons '_ tl)
         (local {(define mv (Move player (index->coord brd ix)))}
           (if (< acc (count-flips brd mv))
               (find (add1 ix) (count-flips brd mv) mv tl)
               (find (add1 ix) acc moveacc tl)))]
         [(cons _ tl) (find (add1 ix) acc moveacc tl)]))}
    (if (empty? (possible-moves g))
        'Skip   
        (find 0 0 (Move player (Coord 0 0)) (Board-cells brd)))))

(check-expect
 (maximize-flips
  (Game tb6-1 'black))
 (Move 'black (Coord 1 2)))
(check-expect
 (maximize-flips
  (Game tb6-1 'white))
 'Skip)
(check-expect
 (maximize-flips
  (Game tb8-1 'black))
 (Move 'black (Coord 0 1)))
(check-expect
 (maximize-flips
  (Game tb8-1 'white))
 (Move 'white (Coord 1 4)))
(check-expect
 (maximize-flips
  (Game (initial-board 6) 'black))
 (Move 'black (Coord 1 2)))

(: max-flips-invariant : Game (Board -> (Move -> Boolean)) -> (U 'Skip Move))
;; this will look at a list of possible moves a find the best possible one
;; according to the max flips invariant (i. e. more flips the better)
;; where the list is filtered according to the preference of corner, edge etc.
(define (max-flips-invariant g f)
  (match g
    [(Game b next)
     (match b
       [(Board sz cells)
        (local
          {(: find : Integer (U 'Skip Move) (Listof Move) -> (U 'Skip Move))
           ;; will find move that flips the most
           (define (find acc moveacc lom)
             (match lom
               ['() moveacc]
               [(cons hd tl)
                (if (< acc (count-flips b hd))
                    (find (count-flips b hd)
                          hd
                          tl)
                    (find acc moveacc tl))]))}
          (find 0
                'Skip
                (filter (f b) (possible-moves g))))])]))

(check-expect
 (max-flips-invariant
  (Game tb6-2 'white)
  corner?)
 (Move 'white (Coord 0 0)))
(check-expect
 (max-flips-invariant
  (Game tb6-3 'white)
  corner?)
 (Move 'white (Coord 5 0)))
(check-expect
 (max-flips-invariant
  (Game tb6-2 'black)
  strict-edge?)
 (Move 'black (Coord 0 1)))
(check-expect
 (max-flips-invariant
  (Game tb6-1 'white)
  corner?)
 'Skip)
(check-expect
 (max-flips-invariant
  (Game tb6-1 'white)
  strict-edge?)
 'Skip)

(: corner-test : Board Coord -> Boolean)
;; will test the coordinate at hand for being a corner
(define (corner-test b c)
  (match* (b c)
    [((Board sz loc) (Coord row col))
     (and (or (= row 0)
              (= row (sub1 sz)))
          (or (= col 0)
              (= col (sub1 sz))))]))

(check-expect (corner-test tb6-3 (Coord 0 0))
              #t)
(check-expect (corner-test tb6-3 (Coord 5 5))
              #t)
(check-expect (corner-test tb6-3 (Coord 0 5))
              #t)
(check-expect (corner-test tb6-3 (Coord 5 0))
              #t)
(check-expect (corner-test tb8-3 (Coord 2 3))
              #f)
(check-expect (corner-test tb6-2 (Coord 0 4))
              #f)

(: corner? : Board -> (Move -> Boolean))
;; curried function to test whether a move is a corner
(define (corner? b)
  (lambda ([m : Move])
    (match m 
      [(Move player c)
       (corner-test b c)])))

(check-expect ((corner? tb6-1) (Move 'black (Coord 0 0)))
              #t)
(check-expect ((corner? tb8-1) (Move 'white (Coord 1 2)))
              #f)
(check-expect ((corner? tb6-1) (Move 'white (Coord 5 5)))
              #t)

(: loose-edge-test : Board Coord -> Boolean)
;; will test a coord for being an edge (loose means it includes corners)
(define (loose-edge-test b c)
  (match* (b c)
    [((Board sz loc) (Coord row col))
     (or (and (< 0 row (sub1 sz))
              (= col 0))
         (and (< 0 row (sub1 sz))
              (= col (sub1 sz)))
         (and (= row 0)
              (< 0 col (sub1 sz)))
         (and (= row (sub1 sz))
              (< 0 col (sub1 sz))))]))

(check-expect (loose-edge-test tb6-6 (Coord 1 3))
              #f)
(check-expect (loose-edge-test tb6-6 (Coord 3 0))
              #t)
(check-expect (loose-edge-test tb6-3 (Coord 4 0))
              #t)
(check-expect (loose-edge-test tb6-6 (Coord 2 5))
              #t)
(check-expect (loose-edge-test tb6-6 (Coord 3 4))
              #f)
(check-expect (loose-edge-test tb6-6 (Coord 4 5))
              #t)

(: strict-edge-test : Board Coord -> Boolean)
;; will test a coord for being strictly an edge (not a corner as well)
(define (strict-edge-test b c)
  (and (not (corner-test b c))
       (loose-edge-test b c)))

(check-expect (strict-edge-test tb6-7 (Coord 0 1))
              #t)
(check-expect (strict-edge-test tb6-7 (Coord 5 0))
              #f)
(check-expect (strict-edge-test tb8-2 (Coord 0 0))
              #f)
(check-expect (strict-edge-test tb6-2 (Coord 5 4))
              #t)
(check-expect (strict-edge-test tb8-1 (Coord 4 4))
              #f)

(: strict-edge? : Board -> (Move -> Boolean))
;; curried function to test whether a move is a non corner edge
(define (strict-edge? b)
  (lambda ([m : Move])
    (match m
      [(Move player c)
       (strict-edge-test b c)])))

(check-expect ((strict-edge? tb6-1) (Move 'black (Coord 0 0)))
              #f)
(check-expect ((strict-edge? tb8-1) (Move 'white (Coord 0 2)))
              #t)
(check-expect ((strict-edge? tb6-1) (Move 'white (Coord 5 4)))
              #t)
(check-expect ((strict-edge? tb6-6) (Move 'white (Coord 5 5)))
              #f)

(: interior? : Board -> (Move -> Boolean))
;; curried function to test whether a move is neither a corner or an edge move
(define (interior? b)
  (lambda ([m : Move])
    (match* (b m)
       [((Board sz cells) (Move player c))
        (and (not ((corner? b) m))
             (not ((strict-edge? b) m)))])))

(check-expect ((interior? tb6-1) (Move 'black (Coord 0 0)))
              #f)
(check-expect ((interior? tb8-1) (Move 'black (Coord 1 1)))
              #t)
(check-expect ((interior? tb6-1) (Move 'white (Coord 3 3)))
              #t)
(check-expect ((interior? tb6-5) (Move 'white (Coord 0 0)))
              #f)
(check-expect ((interior? tb6-3) (Move 'black (Coord 1 5)))
              #f)

(: findcorner : Strategy)
;; will find the ideal corner, if choosing among more than one, will select
;; the move that flips the most, and it will break a tie by index first selection
(define (findcorner g)
  (max-flips-invariant g corner?))  

(check-expect
 (findcorner
  (Game tb6-2 'white))
 (Move 'white (Coord 0 0)))
(check-expect
 (findcorner
  (Game tb6-3 'white))
 (Move 'white (Coord 5 0)))

(: findedge : Strategy)
;; will find the ideal edge, if choosing among more than one, will select
;; the move that flips the most, and it will break a tie by index first selection
(define (findedge g)
  (max-flips-invariant g strict-edge?))

(check-expect
 (findedge
  (Game tb6-2 'black))
 (Move 'black (Coord 0 1)))
(check-expect
 (findedge
  (Game tb8-1 'white))
 (Move 'white (Coord 0 4)))
(check-expect
 (findedge
  (Game tb8-1 'white))
 (Move 'white (Coord 0 4)))
(check-expect
 (findedge
  (Game tb6-4 'white))
 (Move 'white (Coord 4 5)))
(check-expect
 (findedge
  (Game tb6-5 'white))
 (Move 'white (Coord 4 5)))

(: findinterior : Strategy)
;; will find the ideal interior move based on the max-flips invariant
(define (findinterior g)
  (max-flips-invariant g interior?))

(check-expect
 (findinterior
  (Game tb6-2 'black))
 (Move 'black (Coord 1 2)))
(check-expect
 (findinterior
  (Game tb8-1 'white))
 (Move 'white (Coord 1 4)))
(check-expect
 (findinterior
  (Game tb8-2 'white))
 (Move 'white (Coord 1 5)))
(check-expect
 (findinterior
  (Game tb6-5 'white))
 'Skip)

(: immediate-tactics : Strategy)
;; if a corner move is available, then choose it
;; otherwise, if a non-corner edge is available, then choose it
;; otherwise choose an interior move (if one exists)
(define (immediate-tactics g)
  (if (symbol? (findcorner g))
      (if (symbol? (findedge g))
          (findinterior g)
          (findedge g))
      (findcorner g)))
        
(check-expect
 (immediate-tactics
  (Game tb6-4 'black))
 (Move 'black (Coord 0 5)))
(check-expect
 (immediate-tactics
  (Game tb6-5 'white))
 (Move 'white (Coord 5 0)))
(check-expect
 (immediate-tactics
  (Game tb6-1 'white))
 'Skip)
(check-expect
 (immediate-tactics
  (Game tb6-2 'black))
 (Move 'black (Coord 0 1)))
(check-expect
 (immediate-tactics
  (Game tb8-1 'black))
 (Move 'black (Coord 0 1)))
(check-expect
 (immediate-tactics
  (Game tb8-3 'black))
 (Move 'black (Coord 4 0)))

;; Evaluating Boards ===========================================================

(: winner : Evaluator)
;; simple evaluator that returns +1 for next player winning, -1 for
;; the other player winning, and 0 otherwise
(define (winner game)
  (if (game-over? game)
      (match game
        [(Game brd player)
          (max -1 (min 1 (- (count-pieces brd player)
                            (count-pieces brd (other-player player)))))])
      0))

(: piece-counting : Evaluator)
;; will count the piees on the board and subtract the other player's pieces
;; from the next player's pieces. It will return the number 
(define (piece-counting g)
  (match g
    [(Game b next)
     (match b
       [(Board sz cells)
        (local {(: diff : Board Player -> Integer)
                ;; will compute the difference between the pieces on the board
                ;; of different colors
                (define (diff b p)
                  (- (count-pieces b p)
                     (count-pieces b (other-player p))))}
          (cond
            [(game-over? g)
             (if (positive? (diff b next))
                 (sqr sz)
                 (- (sqr sz)))]
            [else (diff b next)]))])]))

(check-expect
 (piece-counting
  (Game tb6-6 'black))
 4)
(check-expect
 (piece-counting
  (Game tb6-4 'black))
 3)
(check-expect
 (piece-counting
  (Game tb6-7 'black))
 -36)
(check-expect
 (piece-counting
  (Game tb6-7 'white))
 36)

(: single-weighted-count : Player Board Integer -> Integer)
;; will count the pieces taking into consideration the
;; input weight for the edges
(define (single-weighted-count p b weight)
  (match b
    [(Board sz loc)
     (local
       {(: count : (Listof Cell) Integer -> Integer)
        ;; will do the actual weighted counting
        (define (count loc ixacc)
          (match loc
            ['() 0]
            [(cons hd tl)
             (if (player=? hd p)
                 (if (loose-edge-test b (index->coord b ixacc))
                     (+ weight (count tl (add1 ixacc)))
                     (+ 1 (count tl (add1 ixacc))))
                 (count tl (add1 ixacc)))]))}
    (count loc 0))]))

(check-expect (single-weighted-count 'black tb6-6 2)
              12)
(check-expect (single-weighted-count 'white tb8-3 3)
              12)
(check-expect (single-weighted-count 'black tb6-3 5)
              26)

(: single-player-count : Player Board Integer -> Integer)
;; will perform the weighted count for the current player
(define (single-player-count p b x)
  (- (single-weighted-count p b x)
     (single-weighted-count (other-player p) b x)))

(check-expect (single-player-count 'black tb6-2 2)
              0)
(check-expect (single-player-count 'white tb8-2 4)
              0)
(check-expect (single-player-count 'black tb8-3 1)
              -2)
(check-expect (single-player-count 'white tb6-5 5)
              -15)

(: prefer-edges : Integer -> Evaluator)             
;; will weight every corner and edge by a certain factor
(define (prefer-edges x)
  (lambda ([g : Game])
    (match g
      [(Game b next)
       (match b
         [(Board sz cells)
          (local
            {(define win-formula : Integer
               ;; the formula for the winning score in either player's favor
               (+ (* 4 (sub1 sz) x)
                  (sqr (- sz 2))))}
            (if (game-over? g)
                (if (positive? (single-player-count next b x))
                    win-formula
                    (- win-formula))
                (single-player-count next b x)))])])))
                             
(check-expect
 ((prefer-edges 2)
  (Game tb6-6 'black))
 8)
(check-expect
 ((prefer-edges 5)
  (Game tb6-6 'black))
 20)
(check-expect
 ((prefer-edges 1)
  (Game tb6-4 'black))
 3)
(check-expect
 ((prefer-edges 4)
  (Game tb6-7 'white))
 -96)
(check-expect
 ((prefer-edges 2)
  (Game tb8-2 'white))
 0)  

(: double-weighted-count : Player Board Integer Integer -> Integer)
;; will perform a weighted count according to the different weights for corner
;; pieces and edge pieces
(define (double-weighted-count p b cw8 ew8)
  (match b
    [(Board sz loc)
     (local
       {(: count : (Listof Cell) Integer -> Integer)
        ;; will do the actual work of the weighted count
        (define (count loc ixacc)
          (match loc
            ['() 0]
            [(cons hd tl)
             (if (player=? hd p)
                 (cond
                   [(corner-test b (index->coord b ixacc))
                    (+ cw8 (count tl (add1 ixacc)))]
                   [(strict-edge-test b (index->coord b ixacc))
                    (+ ew8 (count tl (add1 ixacc)))]
                   [else (add1 (count tl (add1 ixacc)))])
                 (count tl (add1 ixacc)))]))}
       (count loc 0))]))

(check-expect (double-weighted-count 'black tb6-1 2 3)
              3)
(check-expect (double-weighted-count 'white tb8-3 5 2)
              10)
(check-expect (double-weighted-count 'black tb6-3 2 1)
              6)
(check-expect (double-weighted-count 'white tb6-6 4 2)
              4)

(: double-player-count : Player Board Integer Integer -> Integer)
;; will count the current player's score on the board
(define (double-player-count p b cw8 ew8)
  (- (double-weighted-count p b cw8 ew8)
     (double-weighted-count (other-player p) b cw8 ew8)))

(check-expect (double-player-count 'black tb6-3 5 2)
              6)
(check-expect (double-player-count 'white tb6-2 4 2)
              0)
(check-expect (double-player-count 'black tb8-3 1 2)
              -1)
(check-expect (double-player-count 'white tb6-4 3 2)
              -6)
(check-expect (double-player-count 'black tb6-6 10 2)
              8)

(: prefer-corners-and-edges : Integer Integer -> Evaluator)
;; this will return the weighted piece count based on different weights
;; for corner pieces and edge pieces
(define (prefer-corners-and-edges cw8 ew8)
  (lambda ([g : Game])
    (match g
      [(Game b next)
       (match b
         [(Board sz loc)
          (local
            {(define win-formula : Integer
               ;; the formula for the winning score in either player's favor
               (+ (* 4 cw8)
                  (* 4 (- sz 2) ew8)
                  (sqr (- sz 2))))}
             (if (game-over? g)
                (if (positive? (double-player-count next b cw8 ew8))
                    win-formula
                    (- win-formula))
                (double-player-count next b cw8 ew8)))])])))

(check-expect ((prefer-corners-and-edges 10 5)
               (Game tb6-1 'black))
              4)
(check-expect ((prefer-corners-and-edges 4 2)
               (Game tb6-1 'white))
              -1)
(check-expect ((prefer-corners-and-edges 10 5)
               (Game tb6-7 'white))
              -136)
(check-expect ((prefer-corners-and-edges 1 1)
               (Game tb8-3 'black))
              -2)

;; Implementing a Minimax Strategy =============================================

(: curry-apply : Game -> Move -> Game)
;; this will curry apply-move to use in a map in minimax eval
(define (curry-apply g)
  (lambda ([m : Move])
    (apply-move g m)))

(check-expect ((curry-apply (Game tb6-1 'black)) (Move 'black (Coord 1 2)))
              (Game (Board 6 (list '_ '_ '_ '_ '_ '_
                                   'black 'black 'black '_ '_ '_
                                   '_ '_ '_ '_ '_ '_
                                   '_ '_ '_ '_ '_ '_
                                   '_ '_ '_ '_ '_ '_
                                   '_ '_ '_ '_ '_ '_)) 'white))
(check-expect ((curry-apply (Game tb6-2 'white)) (Move 'white (Coord 2 2)))
              (Game (Board 6 (list '_ '_ '_ '_ '_ '_
                                   'black 'white '_ '_ '_ '_
                                   'white 'white 'white '_ '_ '_
                                   '_ '_ '_ '_ '_ '_
                                   '_ '_ '_ '_ '_ '_
                                   '_ '_ '_ '_ '_ '_)) 'black))
(check-expect ((curry-apply (Game tb8-2 'black)) (Move 'black (Coord 0 1)))
              (Game (Board 8 (list '_ 'black 'black 'black '_ '_ '_ '_
                                   '_ '_ 'black 'black 'black '_ '_ '_
                                   '_ '_ 'white 'black '_ '_ '_ '_
                                   '_ '_ 'black 'white '_ '_ '_ '_
                                   '_ 'white      '_ '_ '_ '_ '_ '_
                                   '_ '_ '_ '_ '_ '_ '_ '_
                                   '_ '_ '_ '_ '_ '_ '_ '_
                                   '_ '_ '_ '_ '_ '_ '_ '_)) 'white))

(: minimax-eval : Evaluator Natural -> Evaluator)
;; this will take an evaluator and a depth d and return an evaluator that scores
;; boards by performing a minimax search on them upto d plys and then uses eval
;; to evaluate boards at the leaves of the search
(define (minimax-eval eval d)
  (lambda ([g : Game])
    (local {(: minimax-search : Game Natural -> Integer)
            ;; this will do the actual work of performing the minimax
            ;; search using an accumulator to keep track of the depth
            (define (minimax-search g acc)
              (if (< 0 acc)                                   
                  (foldl (lambda ([hd : Integer]
                                  [acc : Integer])
                           (min hd acc))
                         1
                         (map eval (map (curry-apply g) (possible-moves g))))
                  (eval g)))}
      (minimax-search g (add1 d)))))

(check-expect ((minimax-eval piece-counting 2)
               (Game tb6-1 'black))
              -36)
(check-expect ((minimax-eval piece-counting 3)
               (Game tb8-1 'white))
              -8)
(check-expect ((minimax-eval (prefer-edges 5) 2)
               (Game tb6-5 'black))
              -26)
(check-expect ((minimax-eval piece-counting 2)
               (Game tb6-1 'white))
              1)
              
(: minimax-strategy : Evaluator -> Natural -> Strategy)
;; ((minimax-strategy eval) max-depth) returns a strategy that picks moves
;; by doing a minimax seatch of max-depth plys and uses the evaluator eval
;; to evaluate the leaves of the search tree.
(define (minimax-strategy eval)
  (lambda ([depth : Natural])
    (lambda ([game : Game])
      (match (possible-moves game)
        ['() 'Skip]
        [(cons mv mvs)
         (local
           {(: choose-best : (Listof Move) Integer Move -> Move)
            ;; choose the move with the highest score from a list of moves.
            ;; The second argument is the best score so far and the third
            ;; argument is the corresponding best move so far            
            (define (choose-best mvs best-score best-mv)
              (match mvs
                ['() best-mv]
                [(cons mv rest)
                 (local
                   {;; the game state resulting from the move
                    (define g (apply-move game mv))
                    ;; the score for the game state (avoid recomputation!)
                    (define score (eval g))}
                   (if (< score best-score)
                       (choose-best rest score mv)
                       (choose-best rest best-score best-mv)))]))}
           (choose-best mvs ((minimax-eval eval depth) (apply-move game mv)) mv))]))))

(check-expect (((minimax-strategy (prefer-edges 3)) 3)
               (Game tb6-4 'black))
              (Move 'black (Coord 0 5)))
(check-expect (((minimax-strategy piece-counting) 2)
               (Game tb6-1 'white))
              'Skip)
(check-expect (((minimax-strategy (prefer-edges 3)) 3)
               (Game tb6-3 'black))
              (Move 'black (Coord 1 5)))
(check-expect (((minimax-strategy (prefer-edges 4)) 4)
               (Game tb8-3 'white))
              (Move 'white (Coord 0 6)))
(check-expect (((minimax-strategy (prefer-corners-and-edges 5 3)) 5)
               (Game tb8-1 'black))
              (Move 'black (Coord 2 1)))
  
;;;;; ai.rkt API ===============================================================

;; type exports
;;
(provide
 Strategy
 Evaluator)

;; strategies and evaluators
;;
(provide
 first-move
 maximize-flips
 immediate-tactics
 winner
 piece-counting
 prefer-edges
 prefer-corners-and-edges
 minimax-strategy)

;(test)