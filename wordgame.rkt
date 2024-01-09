;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-intermediate-lambda-reader.ss" "lang")((modname wordgame) (read-case-sensitive #t) (teachpacks ((lib "abstraction.rkt" "teachpack" "2htdp"))) (htdp-settings #(#t constructor repeating-decimal #f #t none #f ((lib "abstraction.rkt" "teachpack" "2htdp")) #f)))
(require 2htdp/batch-io)


; data definitions

; A [ListOf 1String] is one of:
;     - '()
;     - (cons 1String [ListOf 1String])
#;
(define (fn-on-lo1s lo1s)
  (cond
    [(empty? lo1s) ...]
    [(pred-on-1string (first lo1s)) ... (fn-on-1string (first lo1s))
                                    ... (fn-on-lo1s (rest lo1s))]
    [else ... (pred-on-1string (first lo1s))
          ... (fn-on-lo1s (rest lo1s))]))


; A [ListOf [ListOf 1String]] is one of:
;     - '()
;     - (cons [ListOf 1String] [ListOf [ListOf 1String]])
#;
(define (fn-on-lolo1s lolo1s)
  (cond
    [(empty? lolo1s) ...]
    [(pred-on-lo1s (first lolo1s)) ... (fn-on-lo1s (first lolo1s))
                                   ... (fn-on-lolo1s (rest lolo1s))]
    [else ... (pred-on-lo1s (first lolo1s))
          ... (fn-on-lolo1s (rest lolo1s))]))


(define-struct intermute [prefix suffix])
; An Intermute is a [[ListOf X] [ListOf X]]
; it is a tool for constructing a list of permutations


; ====================
; constants

(define LOCATION "/usr/share/dict/words")
(define DICTIONARY (read-lines LOCATION))


; ====================
; functions

(define (get-anagrams word)
  ; String -> [ListOf String]
  ; generate a list of all valid anagrams of the given word
  (local (
          (define exploded (explode word))
          (define indistinct-permutations
            (permute (make-intermute '() exploded)))
          (define permutations (list-to-set indistinct-permutations)))
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
  (match candi-fractures
    [(intermute prefix '()) (list (implode prefix))]
    [(intermute prefix suffix)
     (foldr append '()
            (map permute
                 (map (lambda (c)
                        (make-intermute (cons c prefix) (remove c suffix)))
                      suffix)))]))


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


(define (permutations-to-words los)
  ; [ListOf String] [ListOf String] -> [ListOf String]
  ; purge all nonsense permutations from a list.
  ; Real words are contained in the global dictionary indicated
  (cond
    [(empty? los) '()]
    [(member? (first los) DICTIONARY)
     (cons (first los) (permutations-to-words (rest los)))]
    [else (permutations-to-words (rest los))]))


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
  (match lst1
    ['() lst2]
    [(cons pre post) (push-to-set pre (create-set post lst2))]))


(define (push-to-set ele lst)
  ; X [ListOf X] -> [ListOf X]
  ; adds an element to a set--a list in which no items repeat.
  ; assumes the list provided qualifies as a set
  (match lst
    ['() (list ele)]
    [(cons ele post) lst]
    [(cons pre post) (cons pre (push-to-set ele post))]))



; ===================
; actions!

(get-anagrams "live")

(get-anagrams "slate")
