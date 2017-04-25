;; For homework assignment 5: Spring'17

#lang racket
(provide (all-defined-out))

(define t1
  '( (and (gt 1 2) (or (lt 1 2) (eq 1 1)))
(+ 1 2)
(+ 1 3)
))
(define t2
  '(fun ((f (x y z)) ((gt z y) x y))
(apply (f ((+ 2 2) (+ 4 4) (+ 0 1))))))

(define t3
  '(fun ((f (x y z)) ((gt z y) x y))
(apply (f ((ref 2) (+ 4 4) (+ 0 1))))))

(define t4
  '((and (gt 3 2) (or (lt (var ((x 1)) x) 2) (eq 1 1)))
x
1
))

(define heap1 '((2 400) (1 1000) (3 30) (4 0) (7 77) (6 27) (5 100) (8 19) ))
(define env1 (list (list 'x 2) (list 'y 8)))
;'(0 ((2 0) (1 1000) (3 19) (4 27) (7 100) (6 77) (5 30) (8 400)))

(define prog1
  '(fun ((swap (x y))
         (var ((t1 (deref x)) (t2 (deref y)))
              (var ((x1 (wref y t1))
                    (y1 (wref x t2)))
                   y)))
        ;; Partition the array in the heap
        (fun ((partition (i j k))
               ((lt k j)   ;; high < low
                (apply (swap (i k)))  ;; partition done
                ((not (gt (deref j) (deref i)))
                 (apply (partition (i (+ j 1) k)))
                 ((lt (deref i) (deref k))
                  (apply (partition (i j (- k 1))))
                  (var ((x (apply (swap (j k)))))
                       (apply (partition (i j k))))))))
             ;; Quicksorting
             (fun ((qsort (first last))
                   ((lt first last)
                    (var ((p (apply (partition (first (+ first 1) last)))))
                         (var ((r1 (apply (qsort (first p))))
                               (r2 (apply (qsort ((+ p 1) last)))))
                              0))
                    1))
                  (apply (qsort (x y)))))))

;2
;; '(10 ((1 10) (2 22) (3 free)))
;; chaining heap across variable assignments
(define heap2 '((1 free) (2 22) (3 free)))
(define prog2
  '(var ((x (ref 10)) (y x)) (deref y)))

;3
;; chaining heap across arithmatic operations
;'(11 ((1 10) (2 22) (3 free)))
(define prog3
  '(+ (ref 10) (var ((y (deref 1))) y)))

;4
;; chaining heap across
;;'(10 ((1 10) (2 22) (3 20)))
(define prog4
  '((or (gt (ref 10) (deref 1))
        (gt (ref 20) (deref 2)))
    (deref 2)
    (deref 1)))

;5
;; chaining of application arguments
;; '(22 ((1 10) (2 22) (3 20)))
(define prog5
  '(fun ((f (x y z w)) ((gt x y) z w))
        (apply (f ((ref 10) (ref 20) (deref 1) (deref 2))))))

; 6 7 8
;; check for free
(define heap6 '((1 free) (2 22) (3 free) (4 44)))
(define env6 '((x 2) (y 4))) ; '(4 ((1 free) (2 22) (3 free) (4 free)))
(define env6a '((x 2) (y 3))) ; '((exception fma) ((1 free) (2 22) (3 free) (4 44)))
(define env6b '((x 2) (y 5))) ; '((exception ooma) ((1 free) (2 22) (3 free) (4 44)))
(define prog6
  '((gt x y)
    (free x)
    (free y)))

; 9 10
;; check for wref
(define heap7 '((1 20) (2 free) (4 40))) ;; '(200 ((1 20) (2 200) (4 40)))
(define heap7a '((1 20) (2 40))) ;; '((exception oom) ((1 20) (2 40)))
(define prog7
  '(var ((x (ref 10)) (y x)) (wref y 200)))

; 11
;; heap7: '((exception ooma) ((1 20) (2 10) (4 40))) ;;  not added
(define heap7b '((1 20) (2 free) (3 free) (4 40))) ;; '((exception fma) ((1 20) (2 10) (3 free) (4 40)))
(define prog7a
  '(var ((x (ref 10)) (y x)) (wref (+ y 1) 200)))

; 12
;; example similar to the one from lecture
;; involving multiple heap operations
(define heap8 '((1 20) (2 free) (3 free) (4 40))) ;; '(2 ((1 20) (2 free) (3 free) (4 40)))
(define prog8
  '(var ((x (ref 10)))
        ((gt (wref x 20) (wref x 40))
         ;; then
         (deref x)
         (free x))))

; 13
;; heap8 '(22 ((1 20) (2 free) (3 free) (4 40)))
(define prog8a
  '(var ((x (ref 10)) (y (free x)) (z (ref 20))) (+ (deref z) (free z))))

; 14
(define heap8b '((1 20) (2 free) (3 2) (4 free))) ;; '((exception fma) ((1 20) (2 free) (3 2) (4 2)))
(define prog8b
  '(var ((x (ref 10)) (z (ref 20)) (y (free x))) (var ((p (wref z y))) (deref p))))

; 15
;; involving multiple heap operations with functions
(define heap9 '((1 free) (2 free) (5 50) (3 20))) ; '(50 ((1 free) (2 free) (5 50) (3 20)))
(define prog9
  '(fun ((f (x))
         (fun ((g (y))
               ((gt y 4) (deref y) (apply (f ((+ y 1))))))
              (apply (g (x))))
         ((gt x 4) (deref x) (apply (g ((+ x 2))))))
        (apply (f (0)))))

; 16
; 17
(define heap10 '((1 free) (2 20) (3 free) (5 free) (4 20) (6 free))) ;; '(24 ((1 1) (2 20) (3 2) (5 6) (4 20) (6 24)))
(define heap10a '((1 free) (2 20) (3 free) (5 free) (4 20) (6 20))) ;; '((exception oom) ((1 1) (2 20) (3 2) (5 6) (4 20) (6 20)))
(define prog10
  '(fun ((f (x))
         ((gt x 1) (var ((y (* x (apply (f ((- x 1)))))) (p (ref y))) y)
                   (var ((y (ref 1))) 1)))
         (apply (f (4)))))

; 18
;; heap10 '(1 ((1 free) (2 20) (3 free) (5 free) (4 20) (6 free)))
(define prog11
  '(fun ((f (x))
         ((gt x 0)
          (var ((y (ref x))) (var ((p (apply (f ((- x 1)))))) (free y)))
          x))
        (apply (f (2))))) 


; 19
;; heap10 '(0 ((1 2) (2 20) (3 1) (5 free) (4 20) (6 free)))
(define prog12
  '(fun ((f (x))
         ((gt x 0)
          (var ((y (ref x)) (z (wref y x))) (apply (f ((- x 1)))))
          x))
        (apply (f (2)))))
