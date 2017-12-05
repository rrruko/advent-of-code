#!/usr/bin/env racket
#lang racket

(define (to-spreadsheet filepath)
  (define string-matrix
    (map
      (lambda (line) (string-split line "\t"))
      (string-split (file->string filepath) "\n")))
  (map
    (lambda (row) (map string->number row))
    string-matrix))

(define (checksum spreadsheet)
  (define (diff x y)
    (abs (- x y)))
  (apply +
    (map
      (lambda (row) (diff (apply min row) (apply max row)))
      spreadsheet)))

(define (cartesian-product ls)
  (for*/list ([x ls]
              [y ls])
    (list x y)))

(define (checksum2 spreadsheet)
  (define (div-multiples row)
    (apply / (car
      (filter
        (lambda (pair)
          (and (apply > pair)
               (= 0 (apply modulo pair))))
        (cartesian-product row)))))
  (apply +
    (map
      div-multiples
      spreadsheet)))

(define (main)
  (define spreadsheet (to-spreadsheet "2.txt"))
  (writeln (checksum spreadsheet))
  (writeln (checksum2 spreadsheet)))

(main)
