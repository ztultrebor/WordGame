;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-intermediate-lambda-reader.ss" "lang")((modname wordgame) (read-case-sensitive #t) (teachpacks ((lib "abstraction.rkt" "teachpack" "2htdp"))) (htdp-settings #(#t constructor repeating-decimal #f #t none #f ((lib "abstraction.rkt" "teachpack" "2htdp")) #f)))
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
          (define exploded (explode word))
          (define non-unique-permutations (permute (list '() exploded)))
          (define permutations (list-to-set non-unique-permutations)))
    ; - IN -
    (permutations-to-words permutations)))
; checks
(check-satisfied (list (get-anagrams "rat")
                       (list "art" "rat" "tar" "tra")) same-set?)
(check-satisfied (list (get-anagrams "add")
                       (list "dad" "add")) same-set?)
(check-satisfied (list (get-anagrams "fluff")
                       (list "fluff")) same-set?)
(check-satisfied (list (get-anagrams "fxq") '()) same-set?)


(define (permute candi-fractures)
  ; [ListOf [ListOf 1String]] -> [ListOf String]
  ; This function is auxiliary to get-anagrams
  ; gets all permutations of a given word that has been exploded and parsed
  ; permutations are not guaranteed to be unique
    (cond
      [(empty? (second candi-fractures)) (list (implode (first candi-fractures)))]
      [else
       (foldr append '()
              (map permute
                   (map (lambda (c)
                          (list (cons c (first candi-fractures))
                                (remove c (second candi-fractures))))
                        (second candi-fractures))))]))


#;
(define (permute lo1s0 lo1s1 lo1s2)
  ; [ListOf 1String] [ListOf 1String] [ListOf 1String]
  ; -> [ListOf ListOf 1String]
  ; hard core version, for serious computer scientologists only
  ; built from base metal
  (cond
    [(empty? lo1s2) lo1s0]
    [(empty? lo1s1) '()]
    [else
     (local (
             (define (pull-from-set ele lst)
               ; 1String [ListOf 1String] -> [ListOf 1String]
               ; removes an element to a set if its in there
               (cond
                 [(equal? ele (first lst)) (rest lst)]
                 [else (cons (first lst)
                             (pull-from-set ele (rest lst)))]))
             (define suffix (pull-from-set (first lo1s1) lo1s2))
             ; suffix is the remaining letters of the complete permutation
             (define permute-first
               (permute (cons (first lo1s1) lo1s0) suffix suffix))
             (define permute-rest (permute lo1s0 (rest lo1s1) lo1s2))
             (define (smart-merge l1 l2)
               ; [ListOf ListOf 1String] [ListOf ListOf 1String]
               ; -> [ListOf ListOf 1String]
               ; decides when to create a set from two permuted
               ; lists, and when to cons them
               (cond
                 [(cons? (first l1)) (create-set l1 l2)]
                 [else (cons l1 l2)])))
       ; - IN -
       (smart-merge permute-first permute-rest))]))


(define (permutations-to-words los)
  ; [ListOf String] [ListOf String] -> [ListOf String]
  ; purge all nonsense permutations from a list.
  ; Real words are contained in the global dictionary indicated
  (cond
    [(empty? los) '()]
    [(member? (first los) DICTIONARY)
     (cons (first los) (permutations-to-words (rest los)))]
    [else (permutations-to-words (rest los))]))



(define (list-to-set lst)
  ; [ListOf X] -> [ListOf X]
  ; purges any duplicate elements from a list, thereby creating a set
  (local (
          (define (setterator set lst)
            ; [ListOf X] [ListOf X] -> [ListOf X]
            ;iterates over a list, removing duplicates and returning a true set
            (cond
              [(empty? lst) set]
              [(member? (first lst) set) (setterator set (rest lst))]
              [else (setterator (cons (first lst) set) (rest lst))])))
    ; - IN -
    (setterator '() lst)))


(define (push-to-set ele lst)
  ; X [ListOf X] -> [ListOf X]
  ; adds an element to a set--a list in which no items repeat.
  ; assumes the list provided qualifies as a set
  (cond
    [(empty? lst) (list ele)]
    [(equal? ele (first lst)) lst]
    [else (cons (first lst) (push-to-set ele (rest lst)))]))


(define (same-set? lsets)
  ; [ListOf [ListOf X]]  -> Boolean
  ; returns #true if two lists are setwise equivalent, else #false
  (and (= (length (first lsets)) (length (second lsets)))
       (equal? (second lsets) (create-set (first lsets) (second lsets)))
       (equal? (first lsets) (create-set (second lsets) (first lsets)))))


(define (create-set lst1 lst2)
  ; [ListOf X] [ListOf X] -> [ListOf X]
  ; merges two lists into a set; a list in which no items repeat.
  ; assumes both lists provided qualify as sets
  (cond
    [(empty? lst1) lst2]
    [else (push-to-set (first lst1) (create-set (rest lst1) lst2))]))

; actions!

(get-anagrams "live")

(get-anagrams "slate")
