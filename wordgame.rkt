;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-beginner-abbr-reader.ss" "lang")((modname wordgame) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))

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


; A Dictionary is one of:
;     - '()
;     - (cons String ListOfStrings)
; Its a ListOfStrings arranged in alphabetical order
#;
(define (fn-on-dictionary d)
  (cond
    [(empty? d) ...]
    [(pred-on-string (first d)) ... (fn-on-string (first d))
                                ... (fn-on-dictionary (rest d))]
    [else ... (fn-on-string (first d))
          ... (fn-on-dictionary (rest d))]))

;constants

(define DICTIONARY (list "art" "cat" "hat" "rat" "tar" "add" "dad"))


  
; functions

(define (get-anagrams word dictionary)
  ; String -> ListOfString
  ; generate a list of all anagrams of the given word
  (permutations-to-words (recombine (shuffle (explode word))) dictionary))
(check-satisfied (list (get-anagrams "rat" DICTIONARY)
                       (list "art" "rat" "tar")) same-set?)
(check-satisfied (list (get-anagrams "add" DICTIONARY)
                       (list "dad" "add")) same-set?)


(define (shuffle lo1s)
  ; ListOf1String -> ListOfListOf1String
  ; generate a list of "words" that can be formed
  ; from the letters of the given ListOf1Strings
  (permute '() lo1s lo1s))
; checks on shuffle
(check-satisfied (list (shuffle (list "c")) (list (list "c"))) same-set?)
(check-satisfied (list (shuffle (list "a" "t"))
                       (list (list "a" "t") (list "t" "a"))) same-set?)
(check-satisfied (list (shuffle (list "c" "a" "t"))
                       (list (list "c" "a" "t") (list "c" "t" "a")
                             (list "a" "c" "t") (list "a" "t" "c")
                             (list "t" "a" "c") (list "t" "c" "a"))) same-set?)


(define (permute lo1s0 lo1s1 lo1s2)
  ; ListOfAny -> ListOfListOfAny
  ; generate all unique permutations of elements of a given list
  (cond
    [(empty? lo1s2) lo1s0]
    [(empty? lo1s1) '()]
    [else (smart-merge (permute (cons (first lo1s1) lo1s0)
                                (pull-from-set (first lo1s1) lo1s2)
                                (pull-from-set (first lo1s1) lo1s2))
                       (permute lo1s0 (rest lo1s1) lo1s2))]))


(define (smart-merge l1 l2)
  ; ListOfListOfAny ListOfListOfAny -> ListOfListOfAny
  ; an auxiliary function for permute
  ; decides when to create a set from a permited list, and when to cons it
  (cond
    [(cons? (first l1)) (create-set l1 l2)]
    [else (cons l1 l2)]))
; checks
(check-satisfied (list (smart-merge (list (list "t" "a")) (list (list "m" "v")))
                       (list (list "t" "a") (list "m" "v"))) same-set?)
(check-satisfied (list (smart-merge (list (list "a") (list "b"))
                                    (list (list "c") (list "d")))
                       (list (list "a") (list "b") (list "c") (list "d")))
                 same-set?)


(define (is-actual-word?/member? string dicto)
  ; !!!
  ; String ListOfStrings -> Boolean
  ; determines if a potential word is found in the dictionary provided
  ; member? should suffice
  (member? string dicto)
  #;
  (and
   (not (empty? dicto))
   (or
    (string=? (first dicto) string)
    (is-actual-word? string (rest dicto)))))
; checks
(check-expect (member? "cat" DICTIONARY) #t)
(check-expect (member? "atc" DICTIONARY) #f)


(define (fn-on-word/explode word)
  '())
; !!!
; String -> ListOf1String
; break down a word into a list of 1Strings
; explode should suffice


(define (recombine lolo1s)
  ; String -> ListOf1String
  ; collapse each element of a ListOfListOf1Strings into a String
  (cond
    [(empty? lolo1s) '()]
    [else (cons (implode (first lolo1s)) (recombine (rest lolo1s)))]))
; checks
(check-satisfied (list (recombine
                        (list (list "c" "a" "t") (list "c" "t" "a")
                              (list "a" "c" "t") (list "a" "t" "c")
                              (list "t" "a" "c") (list "t" "c" "a")))
                       (list "cat" "cta" "act" "atc" "tac" "tca")) same-set?)


(define (permutations-to-words los dicto)
  ; ListOfString, ListOfString -> ListOfString
  ; purge all nonsense permutations from a list. Real words are contained
  ; in the dictionary provided
  (cond
    [(empty? los) '()]
    [(member? (first los) dicto)
     (cons (first los) (permutations-to-words (rest los) dicto))]
    [else (permutations-to-words (rest los) dicto)]))
; checks
(check-satisfied (list (permutations-to-words
                        (list "rat" "rta" "art" "atr" "tar" "tra") DICTIONARY)
                      (list "rat" "art" "tar")) same-set?)


(define (create-set lst1 lst2)
  ; Any ListOfAny -> ListOfAny
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


(define (pull-from-set ele lst)
  ; Any ListOfAny -> ListOfAny
  ; removes an element to a set if its in there
  ; assumes the list provided qualifies as a set
  (cond
    [(empty? lst) '()]
    [(equal? ele (first lst)) (rest lst)]
    [else (cons (first lst) (pull-from-set ele (rest lst)))]))
(check-satisfied (list (pull-from-set "a" (list "c" "a" "t"))
                       (list "c" "t")) same-set?)
(check-satisfied (list (pull-from-set "a" (list "a")) '()) same-set?)


(define (same-set? lsets)
  ; ListOfAny ListOfAny -> Boolean
  ; returns #true if two lists are setwise equivalent, else #false
  (and (= (length (first lsets)) (length (second lsets)))
       (equal? (second lsets) (create-set (first lsets) (second lsets)))
       (equal? (first lsets) (create-set (second lsets) (first lsets)))))



; actions!

(get-anagrams "rat" DICTIONARY)