#lang typed/racket

(require typed/test-engine/racket-tests)

(require "../include/cs151-core.rkt")
(require "../include/cs151-image.rkt")

(require "optional.rkt")
(require "loc.rkt")

;; ==== ==== ==== ====
;; external interface

(provide PieceType
         Player
         (struct-out Piece)
         Square
         Board
         (struct-out Move)
         PromoteTo
         (struct-out ChessGame)
         starting-board : Board
         new-game       : ChessGame
         board-ref      : Board Loc -> Square
         board-update   : Board Loc Square -> Board
         ;; in-check?      ; : ChessGame -> Boolean
         legal-move?    : ChessGame Move -> Boolean
         ;; moves-piece    ; : ChessGame Loc -> (Listof Move)
         ;; moves-player   ; : ChessGame -> (Listof Move)
         ;checkmate?     : ChessGame -> Boolean
         ;stalemate?     : ChessGame -> Boolean
         apply-move     : ChessGame Move -> ChessGame
         ;strings->board : (Listof String) -> Board
         )

;; ==== ==== ==== ====
;; data definitions

(define-type PieceType
  (U 'Pawn 'Bishop 'Knight 'Rook 'King 'Queen))

(define-type Player
  (U 'Black 'White))

(define-struct Piece
  ([type  : PieceType]
   [color : Player]))

(define-type Square
  (Optional Piece))

(define-type Board
  (Listof Square))

(define-type PromoteTo
  (U 'Queen 'Rook 'Bishop 'Knight))

(define-struct Move
  ([src        : Loc]
   [dst        : Loc]
   [moved      : Piece]
   [captured   : (Optional Piece)]
   [promote-to : (Optional PromoteTo)]))

(define-struct ChessGame
  ([board : Board]
   [history : (Listof Move)]))

;; Start of Definitions and Functions

(: string->board-helper : String -> (Optional Piece))
(define (string->board-helper s)
  (match s
    ["-" 'None]
    ["R" (Some (Piece 'Rook 'Black))]
    ["N" (Some (Piece 'Knight 'Black))]
    ["B" (Some (Piece 'Bishop 'Black))]
    ["Q" (Some (Piece 'Queen 'Black))]
    ["K" (Some (Piece 'King 'Black))]
    ["P" (Some (Piece 'Pawn 'Black))]
    ["r" (Some (Piece 'Rook 'White))]
    ["n" (Some (Piece 'Knight 'White))]
    ["b" (Some (Piece 'Bishop 'White))]
    ["q" (Some (Piece 'Queen 'White))]
    ["k" (Some (Piece 'King 'White))]
    ["p" (Some (Piece 'Pawn 'White))]))


(: strings->board-helper : String -> (Listof Square))
(define (strings->board-helper s)
  (match s
    ["" '()]
    [s (append (list (string->board-helper (substring s 0 1)))
               (strings->board-helper (substring s 1)))]))

(check-expect (strings->board-helper "RNBQKBNR") (list (Some (Piece 'Rook 'Black))
                                                       (Some (Piece 'Knight 'Black))
                                                       (Some (Piece 'Bishop 'Black))
                                                       (Some (Piece 'Queen 'Black))
                                                       (Some (Piece 'King 'Black))
                                                       (Some (Piece 'Bishop 'Black))
                                                       (Some (Piece 'Knight 'Black))
                                                       (Some (Piece 'Rook 'Black))))

(: strings->board : (Listof String) -> Board)
(define (strings->board los)
  (match los
    ['() '()]
    [(cons hd tl) (append (strings->board-helper hd) (strings->board tl))]))

;;------------------------------------------------------------------------------

(: starting-board : Board)
(define starting-board
  (append (list
           (Some (Piece 'Rook 'White))
           (Some (Piece 'Knight 'White))
           (Some (Piece 'Bishop 'White))
           (Some (Piece 'Queen 'White))
           (Some (Piece 'King 'White))
           (Some (Piece 'Bishop 'White))
           (Some (Piece 'Knight 'White))
           (Some (Piece 'Rook 'White)))
          (make-list 8 (Some (Piece 'Pawn 'White)))
          (make-list 32 'None)
          (make-list 8 (Some (Piece 'Pawn 'Black)))
          (list
           (Some (Piece 'Rook 'Black))
           (Some (Piece 'Knight 'Black))
           (Some (Piece 'Bishop 'Black))
           (Some (Piece 'Queen 'Black))
           (Some (Piece 'King 'Black))
           (Some (Piece 'Bishop 'Black))
           (Some (Piece 'Knight 'Black))
           (Some (Piece 'Rook 'Black)))))

;;------------------------------------------------------------------------------

(: new-game : ChessGame)
(define new-game
  (ChessGame starting-board '()))

;;------------------------------------------------------------------------------

(: file-number : File -> Integer)
(define (file-number f)
  (match f
    ['A 1]
    ['B 2]
    ['C 3]
    ['D 4]
    ['E 5]
    ['F 6]
    ['G 7]
    ['H 8]))

(check-expect (file-number 'H) 8)
(check-expect (file-number 'B) 2)
(check-expect (file-number 'A) 1)

(: target-loc : Loc -> Integer)
(define (target-loc l)
  (match l
    [(Loc f r) (sub1 (+ (file-number f) (* 8 (sub1 r))))]))

(check-expect (target-loc (Loc 'A 8)) 56)
(check-expect (target-loc (Loc 'A 7)) 48)

(: board-ref : Board Loc -> Square)
;; given a board and a location on it, it returns the contents of the specified square
(define (board-ref b l)
  (list-ref b (target-loc l)))
         
(check-expect (board-ref starting-board (Loc 'A 1)) (Some (Piece 'Rook 'White)))
(check-expect (board-ref starting-board (Loc 'H 1)) (Some (Piece 'Rook 'White)))
(check-expect (board-ref starting-board (Loc 'A 3)) 'None)
(check-expect (board-ref starting-board (Loc 'A 6)) 'None)
(check-expect (board-ref starting-board (Loc 'A 7)) (Some (Piece 'Pawn 'Black)))
(check-expect (board-ref starting-board (Loc 'A 8)) (Some (Piece 'Rook 'Black)))

;;------------------------------------------------------------------------------

(: board-update-helper : Board Integer Square -> Board)
(define (board-update-helper b i s)
  (match* (b i)
    [((cons hd tl) 0) (cons s tl)]
    [((cons hd tl) _) (cons hd (board-update-helper tl (sub1 i) s))]))
  
(: board-update : Board Loc Square -> Board)
;; function that returns an updated board where the contents of the specified square
;; are replaced with the given value.
(define (board-update b l s)
  (board-update-helper b (target-loc l) s))

;;------------------------------------------------------------------------------

(: apply-move : ChessGame Move -> ChessGame)
(define (apply-move cg m)
  (if (legal-move? cg m)
      (match* (cg m)
        [((ChessGame b '()) (Move (Loc f1 r1) (Loc f2 r2) mvd cptd prmto))
         (ChessGame (board-update (board-update b (Loc f2 r2) (Some mvd)) (Loc f1 r1) 'None)
                    (list m))]
        [((ChessGame b mh) (Move (Loc f1 r1) (Loc f2 r2) mvd cptd prmto))
         (ChessGame (board-update (board-update b (Loc f2 r2) (Some mvd)) (Loc f1 r1) 'None)
                    (append (list m) mh))])
      (error "apply-move: Illegal Move!")))

;;------------------------------------------------------------------------------

(: inv-target-loc : Integer -> Loc)
(define (inv-target-loc i)
  (match i
    [0 (Loc 'A 1)]
    [1 (Loc 'B 1)]
    [2 (Loc 'C 1)]
    [3 (Loc 'D 1)]
    [4 (Loc 'E 1)]
    [5 (Loc 'F 1)]
    [6 (Loc 'G 1)]
    [7 (Loc 'H 1)]
    [8 (Loc 'A 2)]
    [9 (Loc 'B 2)]
    [10 (Loc 'C 2)]
    [11 (Loc 'D 2)]
    [12 (Loc 'E 2)]
    [13 (Loc 'F 2)]
    [14 (Loc 'G 2)]
    [15 (Loc 'H 2)]
    [16 (Loc 'A 3)]
    [17 (Loc 'B 3)]
    [18 (Loc 'C 3)]
    [19 (Loc 'D 3)]
    [20 (Loc 'E 3)]
    [21 (Loc 'F 3)]
    [22 (Loc 'G 3)]
    [23 (Loc 'H 3)]
    [24 (Loc 'A 4)]
    [25 (Loc 'B 4)]
    [26 (Loc 'C 4)]
    [27 (Loc 'D 4)]
    [28 (Loc 'E 4)]
    [29 (Loc 'F 4)]
    [30 (Loc 'G 4)]
    [31 (Loc 'H 4)]
    [32 (Loc 'A 5)]
    [33 (Loc 'B 5)]
    [34 (Loc 'C 5)]
    [35 (Loc 'D 5)]
    [36 (Loc 'E 5)]
    [37 (Loc 'F 5)]
    [38 (Loc 'G 5)]
    [39 (Loc 'H 5)]
    [40 (Loc 'A 6)]
    [41 (Loc 'B 6)]
    [42 (Loc 'C 6)]
    [43 (Loc 'D 6)]
    [44 (Loc 'E 6)]
    [45 (Loc 'F 6)]
    [46 (Loc 'G 6)]
    [47 (Loc 'H 6)]
    [48 (Loc 'A 7)]
    [49 (Loc 'B 7)]
    [50 (Loc 'C 7)]
    [51 (Loc 'D 7)]
    [52 (Loc 'E 7)]
    [53 (Loc 'F 7)]
    [54 (Loc 'G 7)]
    [55 (Loc 'H 7)]
    [56 (Loc 'A 8)]
    [57 (Loc 'B 8)]
    [58 (Loc 'C 8)]
    [59 (Loc 'D 8)]
    [60 (Loc 'E 8)]
    [61 (Loc 'F 8)]
    [62 (Loc 'G 8)]
    [63 (Loc 'H 8)]))

;;------------------------------------------------------------------------------
;; three helper conditions (and one helper for a helper) below for the legal-move function

(: dst-on-board? : Move -> Boolean)
(define (dst-on-board? m)
  (match m
    [(Move src dst mvd cptd prmto)
     (match dst
       [(Loc f r) (and (>= 8 r)
                       (<= 1 r)
                       (>= 8 (file-number f))
                       (<= 1 (file-number f)))])]))

(: whose-turn? : ChessGame -> Boolean)
(define (whose-turn? cg)
  (match cg
    [(ChessGame b '()) #t]
    [(ChessGame b mh)
     (match mh
       [(cons hd tl)
        (match hd
          [(Move src dst mvd cptd prmto) (symbol=? (Piece-color mvd) 'Black) #t])])]))

(: my-turn? : ChessGame Move -> Boolean)
(define (my-turn? cg m)
  (match m
    [(Move src dst mvd cptd prmto)
     (match mvd
       [(Piece _ 'White) (whose-turn? cg)]
       [(Piece _ 'Black) (not (whose-turn? cg))])]))
                                              
(: land-on-own-player? : ChessGame Move -> Boolean)
(define (land-on-own-player? cg m)
  (match* (cg m)
    [((ChessGame b mh) (Move src dst mvd cptd prmto))
     (match (board-ref b dst)
       ['None #f]
       [(Some (Piece _ 'White))
        (whose-turn? cg) #t]
       [(Some (Piece _ 'Black))
        (whose-turn? cg) #f])]))

;;------------------------------------------------------------------------------
       
(: knight-moves? : ChessGame Move -> Boolean)
(define (knight-moves? cg m)
  (match m
    [(Move src dst mvd cptd prmto) (or (= (target-loc src) (- (target-loc dst) 17))
                                       (= (target-loc src) (- (target-loc dst) 15))
                                       (= (target-loc src) (+ (target-loc dst) 6))
                                       (= (target-loc src) (+ (target-loc dst) 15))
                                       (= (target-loc src) (+ (target-loc dst) 17))
                                       (= (target-loc src) (+ (target-loc dst) 10))
                                       (= (target-loc src) (- (target-loc dst) 6))
                                       (= (target-loc src) (- (target-loc dst) 10)))]))

(: pawn-moves? : ChessGame Move -> Boolean)
(define (pawn-moves? cg m)
  (match m
    [(Move src dst mvd cptd prmto)
     (cond
       [(and (<= 1 (file-number (Loc-file src)) 8) (= (Loc-rank src) 2) (whose-turn? cg) (symbol? cptd))
        (or (= (target-loc src) (- (target-loc dst) 8))
            (= (target-loc src) (- (target-loc dst) 16)))]
       [(and (<= 1 (file-number (Loc-file src)) 8) (<= 3 (Loc-rank src) 7) (whose-turn? cg) (symbol? cptd))
        (= (target-loc src) (- (target-loc dst) 8))]
       [(and (<= 1 (file-number (Loc-file src)) 8) (= (Loc-rank src) 7) (not (whose-turn? cg)) (symbol? cptd))
        (or (= (target-loc src) (+ (target-loc dst) 8))
            (= (target-loc src) (+ (target-loc dst) 16)))]
       [(and (<= 1 (file-number (Loc-file src)) 8) (<= 2 (Loc-rank src) 6) (not (whose-turn? cg)) (symbol? cptd))
        (= (target-loc src) (+ 8 (target-loc dst)))]
       [(and (<= 2 (file-number (Loc-file src)) 7) (<= 2 (Loc-rank src) 7) (whose-turn? cg) (not (symbol? cptd)))
        (or (= (target-loc src) (- (target-loc dst) 7))
            (= (target-loc src) (- (target-loc dst) 9)))]
       [(and (<= 2 (file-number (Loc-file src)) 7) (<= 2 (Loc-rank src) 7) (not (whose-turn? cg)) (not (symbol? cptd)))
        (or (= (target-loc src) (+ (target-loc dst) 7))
            (= (target-loc src) (+ (target-loc dst) 9)))]
       [(and (= 1 (file-number (Loc-file src))) (<= 2 (Loc-rank src) 7) (whose-turn? cg) (not (symbol? cptd)))
        (= (target-loc src) (- (target-loc dst) 9))]
       [(and (= 1 (file-number (Loc-file src))) (<= 2 (Loc-rank src) 7) (not (whose-turn? cg)) (not (symbol? cptd)))
        (= (target-loc src) (- (target-loc dst) 7))]
       [(and (= 8 (file-number (Loc-file src))) (<= 2 (Loc-rank src) 7) (whose-turn? cg) (not (symbol? cptd)))
        (= (target-loc src) (- (target-loc dst) 7))]
       [(and (= 8 (file-number (Loc-file src))) (<= 2 (Loc-rank src) 7) (not (whose-turn? cg)) (not (symbol? cptd)))
        (= (target-loc src) (- (target-loc dst) 9))]
       [else #f])]))


(: king-moves? : ChessGame Move -> Boolean)
(define (king-moves? cg m)
  (match m
    [(Move src dst mvd cptd prmto) (or (= (target-loc src) (- (target-loc dst) 8))
                                       (= (target-loc src) (- (target-loc dst) 9))
                                       (= (target-loc src) (- (target-loc dst) 1))
                                       (= (target-loc src) (+ (target-loc dst) 7))
                                       (= (target-loc src) (+ (target-loc dst) 8))
                                       (= (target-loc src) (+ (target-loc dst) 9))
                                       (= (target-loc src) (+ (target-loc dst) 1))
                                       (= (target-loc src) (- (target-loc dst) 7)))]))

;;------------------------------------------------------------------------------

(: rank-to-integer : Rank -> Integer)
(define (rank-to-integer r)
  (match r
    [1 1]
    [2 2]
    [3 3]
    [4 4]
    [5 5]
    [6 6]
    [7 7]
    [8 8]))

(: rook-horizontal-helper : Integer Integer -> (Listof Integer))
(define (rook-horizontal-helper i1 i2)
  (cond
    [(= i1 i2) '()]
    [else (append (list (- i2 1))
                  (rook-horizontal-helper i1 (- i2 1)))]))

(: rook-vertical-helper : Integer Integer -> (Listof Integer))
(define (rook-vertical-helper i1 i2)
  (cond
    [(= i1 i2) '()]
    [else (append (list (- i2 8))
                  (rook-vertical-helper i1 (- i2 8)))]))

(: rook-horizontal? : Loc Loc -> Boolean)
(define (rook-horizontal? l1 l2)
  (= (file-number (Loc-file l2)) (file-number (Loc-file l1))) #t)

(: intlist-to-loclist : (Listof Integer) -> (Listof Loc))
(define (intlist-to-loclist loi)
  (map inv-target-loc loi))

(: list-empty? : Board (Listof Loc) -> Boolean)
(define (list-empty? b loc)
  (match loc
    ['() #t]
    [(cons hd tl) (symbol? (board-ref b hd)) (list-empty? b tl) #f]))

(: rook-move-space? : ChessGame Move -> Boolean)
(define (rook-move-space? cg m)
  (match m
    [(Move src dst mvd cptd prmto)
     (or (= (file-number (Loc-file dst)) (file-number (Loc-file src)))
         (= (rank-to-integer (Loc-rank dst)) (rank-to-integer (Loc-rank src))))]))

(: rook-moves? : ChessGame Move -> Boolean)
(define (rook-moves? cg m)
  (if (rook-move-space? cg m)
      (match* (cg m)
        [((ChessGame b mh) (Move src dst mvd cptd prmto))
         (cond
           [(rook-horizontal? src dst) (list-empty? b (intlist-to-loclist (rook-horizontal-helper (target-loc src) (target-loc dst))))
                                       (list-empty? b (intlist-to-loclist (rook-vertical-helper (target-loc src) (target-loc dst))))]
           [else #f])])
      #f))
         
;;------------------------------------------------------------------------------

(: bishop-downtoleft : Integer Integer -> (Listof Integer))
(define (bishop-downtoleft i1 i2)
  (cond
    [(= i1 i2) '()]
    [else (append (list (- i2 9))
                  (rook-horizontal-helper i1 (- i2 9)))]))

(: bishop-downtoright : Integer Integer -> (Listof Integer))
(define (bishop-downtoright i1 i2)
  (cond
    [(= i1 i2) '()]
    [else (append (list (- i2 7))
                  (bishop-downtoright i1 (- i2 7)))]))

(: bishop-downleft? : Loc Loc -> Boolean)
(define (bishop-downleft? L1 L2)
  (match* (L1 L2)
    [((Loc f1 r1) (Loc f2 r2))
     (cond
       [(or (and (< (file-number f2) (file-number f1))          ;left
                 (< (rank-to-integer r2) (rank-to-integer r1))) ;down
            (and (> (file-number f2) (file-number f1))                 ;right
                 (> (rank-to-integer r2) (rank-to-integer r1)))) #t]
       [else #f])]))
       

(: bishop-moves-space? : ChessGame Move -> Boolean)
(define (bishop-moves-space? cg m)
  (match m
    [(Move src dst mvd cptd prmto)
     (or (= (remainder (- (target-loc dst) (target-loc src)) 9) 0)
         (= (remainder (- (target-loc dst) (target-loc src)) 7) 0))]))
         

(: bishop-moves? : ChessGame Move -> Boolean)
(define (bishop-moves? cg m)
  (if (bishop-moves-space? cg m)
      (match* (cg m)
        [((ChessGame b mh) (Move src dst mvd cptd prmto))
         (cond
           [(bishop-downleft? src dst) (list-empty? b (intlist-to-loclist (bishop-downtoleft (target-loc src) (target-loc dst))))
                                       (list-empty? b (intlist-to-loclist (bishop-downtoright (target-loc src) (target-loc dst))))]
           [else #f])])
      #f))

;;------------------------------------------------------------------------------

(: queen-moves? : ChessGame Move -> Boolean)
(define (queen-moves? cg m)
  (or (bishop-moves? cg m)
      (rook-moves? cg m)) #t)

;;------------------------------------------------------------------------------

(: legal-move? : ChessGame Move -> Boolean)
(define (legal-move? cg m)
  (and (dst-on-board? m)
       (my-turn? cg m)
       (land-on-own-player? cg m))
  (match m
    [(Move src dst mvd cptd prmto)
     (match mvd
       [(Piece 'Knight _) (knight-moves? cg m)]
       [(Piece 'King _) (king-moves? cg m)]
       [(Piece 'Pawn _) (pawn-moves? cg m)]
       [(Piece 'Rook _) (rook-moves? cg m)]
       [(Piece 'Bishop _) (bishop-moves? cg m)]
       [(Piece 'Queen _) (queen-moves? cg m)])]))

;;------------------------------------------------------------------------------

;(: moves-piece : ChessGame Loc -> (Listof Move))
;(define (moves-piece cg l)
;  (match 



;;------------------------------------------------------------------------------

;(: checkmate? : ChessGame -> Boolean)
;(define (checkmate? cg)
;  (match (moves-player cg)
;    ['() (in-check? cg)]
;    [(cons hd tl) #f]))
;
;;;------------------------------------------------------------------------------
;
;(: stalemate? : ChessGame -> Boolean)
;(define (stalemate? cg)
;  (match (moves-player cg)
;    ['() (not (in-check? cg))]
;    [(cons hd tl) #f]))

;;------------------------------------------------------------------------------


(test)



































