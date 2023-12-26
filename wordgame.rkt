;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-intermediate-reader.ss" "lang")((modname wordgame) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))
(require 2htdp/batch-io)


; data definitions

; A ListOf1Strings is one of:
;     - '()
;     - (cons 1String ListOf1Strings)
#;
(define (fn-on-lo1s lo1s)
  (cond
    [(empty? lo1s) ...]
    [(pred-on-1string (first lo1s)) ... (fn-on-1string (first lo1s))
                                    ... (fn-on-lo1s (rest lo1s))]
    [else ... (pred-on-1string (first lo1s))
          ... (fn-on-lo1s (rest lo1s))]))


; A ListOfListOf1Strings is one of:
;     - '()
;     - (cons ListOfStrings ListOfListOf1Strings)
#;
(define (fn-on-lolo1s lolo1s)
  (cond
    [(empty? lolo1s) ...]
    [(pred-on-lo1s (first lolo1s)) ... (fn-on-lo1s (first lolo1s))
                                   ... (fn-on-lolo1s (rest lolo1s))]
    [else ... (pred-on-lo1s (first lolo1s))
          ... (fn-on-lolo1s (rest lolo1s))]))



; constants

(define LOCATION "/usr/share/dict/words")
(define DICTIONARY (read-lines LOCATION))


  
; functions

(define (get-anagrams word)
  ; String -> [ListOf String]
  ; generate a list of all valid anagrams of the given word
  (local (
          (define (permute lo1s0 lo1s1 lo1s2)
            ; [ListOf 1String] [ListOf 1String] [ListOf 1String]
            ; -> [ListOf ListOf 1String]
            ; generate all unique permutations of elements of a given list
            (cond
              [(empty? lo1s2) lo1s0]
              [(empty? lo1s1) '()]
              [else
               (local (
                       (define (smart-merge l1 l2)
                         ; [ListOf ListOf 1String] [ListOf ListOf 1String]
                         ; -> [ListOf ListOf 1String]
                         ; decides when to create a set from a permuted
                         ; list, and when to cons it
                         (cond
                           [(cons? (first l1)) (create-set l1 l2)]
                           [else (cons l1 l2)]))
                       (define (pull-from-set ele lst)
                         ; 1String [ListOf 1String] -> [ListOf 1String]
                         ; removes an element to a set if its in there
                         ; assumes the list provided qualifies as a set
                         (cond
                           [(empty? lst) '()]
                           [(equal? ele (first lst)) (rest lst)]
                           [else (cons (first lst)
                                       (pull-from-set ele (rest lst)))]))
                       (define reduced-set
                         (pull-from-set (first lo1s1) lo1s2))
                       (define permute-first
                         (permute(cons (first lo1s1) lo1s0)
                                 reduced-set reduced-set))
                       (define permute-rest (permute lo1s0 (rest lo1s1) lo1s2)))
                 (smart-merge permute-first permute-rest))]))
          (define (recombine lolo1s)
            ; [ListOf ListOf 1String] -> [ListOf String]
            ; collapse each [ListOf 1String] into a String
            (cond
              [(empty? lolo1s) '()]
              [else (cons (implode (first lolo1s)) (recombine (rest lolo1s)))]))
          (define (permutations-to-words los)
            ; [ListOf String] [ListOf String] -> [ListOf String]
            ; purge all nonsense permutations from a list.
            ; Real words are contained in the global dictionary indicated
            (cond
              [(empty? los) '()]
              [(member? (first los) DICTIONARY)
               (cons (first los) (permutations-to-words (rest los)))]
              [else (permutations-to-words (rest los))]))
          (define exploded (explode word))
          (define raw-permutations (permute '() exploded exploded))
          (define permutations (recombine raw-permutations)))
    (permutations-to-words permutations)))
; checks
(check-satisfied (list (get-anagrams "rat")
                       (list "art" "rat" "tar" "tra")) same-set?)
(check-satisfied (list (get-anagrams "add")
                       (list "dad" "add")) same-set?)
(check-satisfied (list (get-anagrams "fxq") '()) same-set?)


(define (create-set lst1 lst2)
  ; ListOfAny ListOfAny -> ListOfAny
  ; merges two lists into a set; a list in which no items repeat.
  ; assumes the both lists provided qualify as sets
  (cond
    [(empty? lst1) lst2]
    [else (push-to-set (first lst1) (create-set (rest lst1) lst2))]))


(define (push-to-set ele lst)
  ; Any ListOfAny -> ListOfAny
  ; adds an element to a set--a list in which no items repeat.
  ; assumes the list provided qualifies as a set
  (cond
    [(empty? lst) (list ele)]
    [(equal? ele (first lst)) lst]
    [else (cons (first lst) (push-to-set ele (rest lst)))]))


(define (same-set? lsets)
  ; ListOfListOfAny  -> Boolean
  ; returns #true if two lists are setwise equivalent, else #false
  (and (= (length (first lsets)) (length (second lsets)))
       (equal? (second lsets) (create-set (first lsets) (second lsets)))
       (equal? (first lsets) (create-set (second lsets) (first lsets)))))



; actions!

(get-anagrams "live")