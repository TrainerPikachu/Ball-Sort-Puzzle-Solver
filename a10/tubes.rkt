;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-intermediate-lambda-reader.ss" "lang")((modname tubes) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))
(require "lib-tubes.rkt")
;;
;; ***************************************************
;; Ron Hong (20997326)
;; CS 135 Fall 2022
;; Assignment 10, Problem 1
;; ***************************************************
;;

;; A Game is (make-game Nat Nat (listof (listof Sym)))
(define-struct game (tubesize maxcolours tubes))

;;; Constants

(define emptygame
  (make-game 0 5
             (list empty empty empty empty empty)))

(define emptygame2
  (make-game 10 3 empty))

(define emptygame3
  (make-game 10 3 (list empty empty)))

(define smallgame1
  (make-game 2 2
             (list (list 'blue 'red)
                   (list 'blue 'red)
                   (list))))

(define smallgame2
  (make-game 2 3
             (list (list 'blue 'red)
                   (list 'blue 'red)
                   (list))))
(define smallgame3
  (make-game 2 2
             (list (list 'blue 'red)
                   (list 'blue 'red)
                   (list))))

(define smallinvalidgame1
  (make-game 2 1
             (list (list 'blue 'red)
                   (list 'blue 'red)
                   (list))))


(define smallinvalidgame2
  (make-game 2 2
             (list (list 'blue 'red)
                   (list 'blue 'blue)
                   (list))))

(define smallinvalidgame3
  (make-game 2 2
             (list (list 'blue 'red 'blue)
                   (list 'red)
                   (list))))


(define smallgamefinal
  (make-game 2 2
             (list (list)
                   (list 'blue 'blue)
                   (list 'red 'red))))


(define mediumgame
  (make-game 2 3
             (list (list 'blue 'red)
                   (list 'red 'yellow)
                   (list 'yellow 'blue)
                   (list))))

(define mediumgamestuck
  (make-game 2 3
             (list (list 'blue 'red)
                   (list 'red 'yellow)
                   (list 'yellow 'blue)
                   )))

(define largergame
  (make-game 3 3
             (list (list 'blue 'red 'red)
                   (list 'yellow 'blue 'yellow)
                   (list 'red 'yellow 'blue)
                   (list))))

(define biggame
  (make-game 5 3
             (list (list 'blue 'blue 'red 'red 'yellow)
                   (list 'red 'red 'yellow 'blue 'red)
                   (list 'yellow 'blue 'blue 'yellow 'yellow)
                   (list)
                   (list))))

(define biggame2
  (make-game 5 3
             (list (list 'yellow 'blue 'blue 'yellow 'yellow)
                   (list 'red 'red 'yellow 'blue 'red)
                   (list)
                   (list 'blue 'blue 'red 'red 'yellow)
                   (list))))

(define biggamesolve
  (make-game 5 3
             (list (list 'blue 'blue 'blue 'blue 'blue)
                   (list 'red 'red 'red 'red 'red)
                   (list 'yellow 'yellow 'yellow 'yellow 'yellow)
                   (list)
                   (list))))

(define hugegame
  (make-game 4 9
             (list (list 'purple 'pink 'yellow 'blue)
                   (list 'blue 'green 'purple 'white)
                   (list 'orange 'yellow 'black 'blue)
                   (list 'white 'orange 'orange 'pink)
                   (list 'pink 'red 'red 'black)
                   (list 'yellow 'green 'orange 'blue)
                   (list 'white 'purple 'red 'yellow)
                   (list 'green 'red 'green 'black)
                   (list 'purple 'black 'white 'pink)
                   (list)
                   (list))))

;;
;; Problem (a)
;;

;; (check-colour? size num los) consumes two natural numbers, size and num, and a list
;; of symbols, los, and produces true if each symbol in the list appears exactly
;; size times and if there are at most num different symbols. otherwise, check-colour?
;; will produce false. 

;; Examples: 
(check-expect (check-colour? 2 3 (list 'yellow 'green 'orange 'blue)) false)
(check-expect (check-colour? 2 4 '(a a b b c c)) true)

;; check-colour?: Nat Nat (listof Sym) -> Bool
(define (check-colour? size num los)
  (cond[(<= (length los)(* size num))
        (cond[(and (>= num 0) (empty? los)) true]
             [(= size (length (filter (lambda (x) (symbol=? x (first los))) los)))
              (check-colour? size
                             (sub1 num)
                             (filter (lambda (x) (not(symbol=? x (first los)))) los))]
             [else false])]
       [else false]))

;; Tests: 
(check-expect (check-colour? 2 4 '(a a b b c c d d)) true)
(check-expect (check-colour? 2 0 '(a a b b c c)) false)
(check-expect (check-colour? 2 2 '(a a b b c c)) false)
(check-expect (check-colour? 2 4 '(a a a b b b c c c)) false)

;;
;; Problem (b)
;;

;; (valid-game? gm) consumes a Game, gm, and produces true
;; if gm is a valid game, and false otherwise.

;; Examples: 
(check-expect (valid-game? (make-game 3 3 (list
                                           (list 'yellow 'blue 'yellow)
                                           (list 'red 'red 'red)
                                           (list 'blue 'yellow 'blue)       
                                           (list)))) true)
(check-expect (valid-game? smallinvalidgame1) false)

;; valid-game?: Game -> Bool
(define (valid-game? gm)
  (local[(define (flatten tubes)
           (cond[(empty? tubes) empty]
                [else (append (first tubes) (flatten (rest tubes)))]))
         (define (exceed? tubes tubesize)
           (cond[(empty? tubes) true]
                [(>= tubesize (length (first tubes))) (exceed? (rest tubes) tubesize)]
                [else false]))]
    (and (exceed? (game-tubes gm) (game-tubesize gm))
               (check-colour? (game-tubesize gm)
                              (game-maxcolours gm)
                              (flatten (game-tubes gm))))))

;; Tests:
(check-expect (valid-game? hugegame) true)
(check-expect (valid-game? biggame) true)
(check-expect (valid-game? (make-game 0 0 empty)) true)
(check-expect (valid-game? smallinvalidgame1) false)
(check-expect (valid-game? smallinvalidgame2) false)
(check-expect (valid-game? smallinvalidgame3) false)

;;
;; Problem (c)
;;

;; (remove-completed gm) which consumes a Game, gm, and produces
;; a Game which is similar to gm but has any completed tubes removed.

;; Examples: 
(check-expect (remove-completed (make-game 3 3 (list
                                                (list 'yellow 'blue 'yellow)
                                                (list 'red 'red 'red)
                                                (list 'blue 'yellow 'blue)       
                                                (list))))
              (make-game 3 2 (list
                              (list 'yellow 'blue 'yellow)
                              (list 'blue 'yellow 'blue)       
                              (list))))

;; remove-completed: Game -> Game
(define (remove-completed gm)
  (cond[(empty? (game-tubes gm)) (make-game (game-tubesize gm) (game-maxcolours gm) empty)]
       [(and (= (game-tubesize gm) (length (first (game-tubes gm))))
             (andmap (lambda (x) (symbol=? x (first (first (game-tubes gm)))))
                     (first (game-tubes gm))))
        (remove-completed (make-game (game-tubesize gm)
                                     (sub1 (game-maxcolours gm))
                                     (rest (game-tubes gm))))]
       [else (make-game (game-tubesize gm)
                        (game-maxcolours
                         (remove-completed (make-game (game-tubesize gm)
                                                      (game-maxcolours gm)
                                                      (rest (game-tubes gm)))))
                        (cons (first (game-tubes gm))
                              (game-tubes
                               (remove-completed (make-game (game-tubesize gm)
                                                            (game-maxcolours gm)
                                                            (rest (game-tubes gm)))))))]))
;; Tests:


;;
;; Problem (d)
;;

;; (finished-game? gm) which consumes a Game, gm, and produces
;; true if the game is finished, and false otherwise.

;; Examples: 
(check-expect (finished-game? smallgamefinal) true)

;; finished-game?: Game -> Bool
(define (finished-game? gm)
  (local[(define compgame (game-tubes (remove-completed gm)))]
    (cond[(andmap empty? compgame) true]
         [else false])))

;; Tests:

(check-expect (finished-game? smallgame1) false)

(check-expect (finished-game? hugegame) false)

;;
;; Problem (e)
;;

;; (num-blocks llos) consumes a list of lists of symbols, llos,
;; and produces the number of “blocks” contained in llos.

;; Examples: 
(check-expect (num-blocks (list empty '(a a a) '(a a b a a))) 4)

;; num-blocks: (listof (listof Sym)) -> Nat
(define (num-blocks llos)
  (local
    [(define (getsym llos) (cond[(empty? llos) empty]
                                [(empty? (first llos)) empty]
                                [(list? (first llos)) (getsym (first llos))]
                                [(symbol? (first llos)) (first llos)]))
     (define (list-counter los sym)
       (cond[(empty? los) 0]
            [(symbol=? (first los) sym) (list-counter (rest los) sym)]
            [else (add1 (list-counter (rest los) (first los)))]))
     (define (num-blocks/acc llos n sym)
       (cond[(empty? llos) n]
            [(empty? (first llos)) (num-blocks/acc (rest llos) n (getsym (rest llos)))]
            [(list? (first llos)) (+ (list-counter (first llos) sym)
                                     (num-blocks/acc (rest llos) (add1 n) (getsym (rest llos))))]))]
    (num-blocks/acc llos 0 (getsym llos))))

;; Tests: 
(check-expect (num-blocks (list (list 'blue 'red))) 2)
(check-expect (num-blocks '()) 0)
(check-expect (num-blocks (list (list 'blue 'red)
                                (list 'red 'yellow)
                                (list 'yellow 'blue)
                                (list))) 6)

;;
;; Problem (f)
;;

;; (equiv-game? gm1 gm2) consumes two Games, gm1 and gm2, and
;; produces true if gm1 and gm2 are equivalent, and false otherwise.

;; Examples: 
(check-expect (equiv-game? biggame biggame2) true)
(check-expect (equiv-game? mediumgame mediumgamestuck) false)

;; equiv-game?: Game Game -> Bool
(define (equiv-game? gm1 gm2)
  (local[(define (checktube llos1 llos2)
           (cond[(and (empty? llos1) (empty? llos2)) true]
                [(ormap (lambda (x) (equal? x (first llos1))) llos2)
                 (checktube (rest llos1) (remove (first llos1) llos2))]
                [else false]))]
    (and (= (game-maxcolours gm1) (game-maxcolours gm2))
               (= (game-tubesize gm1) (game-tubesize gm2))
               (= (length (game-tubes gm1)) (length (game-tubes gm2)))
               (checktube (game-tubes gm1) (game-tubes gm2)))))

(check-expect (equiv-game? smallgame1 smallgame2) false)
(check-expect (equiv-game? hugegame biggame) false)
(check-expect (equiv-game? biggame biggame2) true)

;;
;; Problem (g)
;;

;; (all-equiv? log1 log2) consumes two lists of Games, log1 and
;; log2, and produces true if every game in log1 has one equivalent game in log2, and
;; every game in log2 has one equivalent game in log1, and otherwise produces false.

;; Examples: 
(check-expect (all-equiv? (list biggame smallgame1)
                          (list smallgame3 biggame2)) true)

;; all-equiv?: (listof Game) (listof Game) -> Bool
(define (all-equiv? log1 log2)
  (local[(define (remove-equiv gm log)
           (cond[(empty? log) empty]
                [(equiv-game? gm (first log)) (rest log)]
                [else (cons (first log) (remove-equiv gm (rest log)))]))]
  (cond[(and (empty? log1) (empty? log2)) true]
       [(or (empty? log1) (empty? log2)) false]
       [(ormap (lambda (x) (equiv-game? x (first log1))) log2)
        (all-equiv? (rest log1) (remove-equiv (first log1) log2))]
       [else false])))

;; Tests:
(check-expect (all-equiv? (list biggame smallgame1)
                          (list smallgame3 biggame2)) true)
(check-expect (all-equiv? '()
                          '()) true)
(check-expect (all-equiv? (list smallgame1 smallgame2)
                          (list smallgame2 smallgame1)) true)
(check-expect (all-equiv? (list biggame smallgame2)
                          (list smallgame2 hugegame)) false)


;;
;; Problem (h)
;;

;; (next-games gm) consumes a Game, gm, and produces a
;; list of Games that can happen by moving one ball from gm.

;; Examples: 
(check-expect (next-games mediumgame) (list
                                       (make-game
                                        2
                                        3
                                        (list
                                         (list 'red)
                                         (list 'red 'yellow)
                                         (list 'yellow 'blue)
                                         (list 'blue)))
                                       (make-game
                                        2
                                        3
                                        (list
                                         (list 'blue 'red)
                                         (list 'yellow)
                                         (list 'yellow 'blue)
                                         (list 'red)))
                                       (make-game
                                        2
                                        3
                                        (list
                                         (list 'blue 'red)
                                         (list 'red 'yellow)
                                         (list 'blue)
                                         (list 'yellow)))))

;; next-games: Game -> (listof Game)
(define (next-games gm)
  (local[(define original-tube (game-tubes gm))]
    (local[(define (get-tubes tubes posi dest acc sym)
             (cond
                  
                  [(= acc posi) (cons (rest (first tubes))
                                      (get-tubes (rest tubes) posi dest (add1 acc) sym))]
                  [(= acc dest)
                   (cons (cons sym (first tubes))
                         (get-tubes (rest tubes) posi dest (add1 acc) sym))]
                  [(empty? tubes) empty]
                  [else (cons (first tubes) (get-tubes (rest tubes) posi dest (add1 acc) sym))]))
           
           (define (get-games gm posi dest)
               (cond[(= dest (length (game-tubes gm))) empty]
                    [(= posi dest) (get-games gm posi (add1 dest))]
                    [(= (game-tubesize gm) (length (list-ref (game-tubes gm) dest)))
                     (get-games gm posi (add1 dest))]
                    [else (cons (make-game (game-tubesize gm)
                                             (game-maxcolours gm)
                                             (get-tubes (game-tubes gm)
                                                        posi
                                                        dest
                                                        0
                                                        (first (list-ref (game-tubes gm) posi))))
                                (get-games gm posi (add1 dest)))]))
           
           (define (next-games/acc gm posi)
             (cond[(or (>= posi (length (game-tubes gm))) (empty? (game-tubes gm))) empty]
                  [(empty? (list-ref (game-tubes gm) posi)) (next-games/acc gm (add1 posi))]
                  [else (append (get-games gm posi 0) (next-games/acc gm (add1 posi)))]))]
      (next-games/acc gm 0))))

;; Tests: 
(check-expect (next-games (make-game 2 1 '((a) (a)))) (list (make-game 2 1 '(() (a a)))
                                            (make-game 2 1 '((a a) ()))))
(check-expect (next-games (make-game 2 2
                                     (list (list 'red 'blue)
                                           (list 'blue 'red)
                                           (list ))))
              (list
               (make-game
                2
                2
                (list
                 (list 'blue)
                 (list 'blue 'red)
                 (list 'red)))
               (make-game
                2
                2
                (list
                 (list 'red 'blue)
                 (list 'red)
                 (list 'blue)))))

(check-expect (test-next-games emptygame (list smallgame1)) false)
(check-expect (test-next-games largergame (list
                                           (make-game
                                            3
                                            3
                                            (list
                                             (list 'red 'red)
                                             (list 'yellow 'blue 'yellow)
                                             (list 'red 'yellow 'blue)
                                             (list 'blue)))
                                           (make-game
                                            3
                                            3
                                            (list
                                             (list 'blue 'red 'red)
                                             (list 'blue 'yellow)
                                             (list 'red 'yellow 'blue)
                                             (list 'yellow)))
                                           (make-game
                                            3
                                            3
                                            (list
                                             (list 'blue 'red 'red)
                                             (list 'yellow 'blue 'yellow)
                                             (list 'yellow 'blue)
                                             (list 'red))))) true)
(check-expect (test-next-games smallgame1 (list (make-game 2 2
                                                           (list (list 'blue 'red)
                                                                 (list 'red)
                                                                 (list 'blue)))
                                                (make-game 2 2
                                                           (list (list 'red)
                                                                 (list 'blue 'red)
                                                                 (list 'blue))))) true)

(define (test-next-games gm expected) (all-equiv? (next-games gm) expected))
(check-expect (test-next-games   (make-game 2 1 '((a) (a)))
                                (list (make-game 2 1 '(() (a a)))
                                      (make-game 2 1 '((a a) ())))) true)

(check-expect (test-next-games (make-game 2 2
                                          (list (list 'red 'blue)
                                                (list 'blue 'red)
                                                (list )))
                               (list (make-game 2 2
                                                (list (list 'blue)
                                                      (list 'blue 'red)
                                                      (list 'red)))                                
                                     (make-game 2 2
                                                (list (list 'red 'blue)
                                                      (list 'red)
                                                      (list 'blue))))) true)

(check-expect (test-next-games smallgame1

                               (list (make-game 2 2
                                                (list (list 'blue 'red)
                                                      (list 'red)
                                                      (list 'blue)))
                                     (make-game 2 2
                                                (list (list 'red)
                                                      (list 'blue 'red)
                                                      (list 'blue))))) true)
;;;;;

;; (solve gm draw-option) determines if the game gm is solveable,
;; and will also draw each possible move depending on the draw-option

;; Examples:
;; students should provide some here, or just in tests

;; solve: Game (anyof 'off 'norm 'slow 'fast) -> Bool

(define (solve gm draw-option)
  (local
    [(define setup (puzzle-setup gm draw-option))                
     (define (solve-helper to-visit visited)
       (cond
         [(empty? to-visit) false]
         [else
          (local
            [(define draw (draw-board (first to-visit) draw-option))] 
            (cond
              [(finished-game? (first to-visit)) true]
              [(member? (first to-visit) visited)
               (solve-helper (rest to-visit) visited)]
              [else
               (local [(define nbrs (next-games (first to-visit)))
                       (define new (filter (lambda (x) (not (member? x visited))) nbrs))
                       (define new-to-visit (append new (rest to-visit)))
                       (define new-visited (cons (first to-visit) visited))]
                 (solve-helper new-to-visit new-visited))]))]))]
    (solve-helper (list gm) empty)))

;; Test cases that can be uncommented as the solution is completed

(check-expect (solve smallgame 'off) true)
(check-expect (solve mediumgamestuck 'off) false)

;; Below is the format for testing and timing the solution:
;; be sure to remove any other check-expects when measuring your timing

(check-expect (time (solve mediumgame 'off)) true)
(check-expect (time (solve largergame 'off)) true)
(check-expect (time (solve biggame 'off)) true)
(check-expect (time (solve monstergame 'norm)) true)

