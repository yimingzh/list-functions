#lang racket
;; Creator: Yiming Zhong
;; Date: January 17, 2016
;; This file contains some function definitions for potentially useful functions
;;    to deal with lists of numbers.

;; (min lst) consumes a list of numbers and produces the minimum of that list
;; min: (listof Num) -> Num

(define (min lst)
  (foldr (lambda (x y)
           (cond [(< x y) x]
                 [else y])) (first lst) (rest lst)))


;; (max lst) consumes a list of numbers and produces the maximum of that list
;; max: (listof Num) -> Num

(define (max lst)
  (foldr (lambda (x y)
           (cond [(> x y) x]
                 [else y])) (first lst) (rest lst)))


;; (difference lst) consumes a list of numbers and produces the difference
;;    between the minimum and maximum value
;; difference: (listof Num) -> Num

(define (difference lst)
  (- (max lst) (min lst)))


;; (findByIndex lst index) consumes a list and an index and finds the value
;;    at that index position
;; findByIndex: (listof Num) Nat -> Num

(define (findMyIndex lst index)
  (cond [(zero? index) (first lst)]
        [else (findMyIndex (rest lst) (sub1 index))]))


