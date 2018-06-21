#lang typed/racket

(require typed/test-engine/racket-tests)

(require "../include/cs151-core.rkt")
(require "../include/cs151-image.rkt")

(require "optional.rkt")
(require "loc.rkt")
(require "chess-logic.rkt")

;; ==== ==== ==== ====
;; external interface

;; (provide board->image ; : Board -> Image