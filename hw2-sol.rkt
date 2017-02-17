#lang racket
(provide (all-defined-out)) ;; for us to test
(require racket/trace) ;; in case you want to use tracing
(require "DB.rkt")
                   
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Ans: 3(a)
;; input params: student-table
;;               f that can be applied to student-record
;; output: whatever is the result of applying f on each
;;         student record
;; Example scenario shows: output is in the form of list
;;         f applied to student-record 
(define applyonstds
  (lambda (s-table)
    (lambda (f)
      (if (null? s-table)   
          s-table
          (cons (f (car s-table)) ((applyonstds (cdr s-table)) f))))))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Ans: 3(b)
;; input param: student-record
;; output: student-record where the course plan is replaced by
;;         number of courses in the plan
(define numberofcourses
  (lambda (student-record)
    (list (car student-record) (cadr student-record) (length (cadr (cdr student-record))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Ans: 3(c)
;; Input params: grade-table and student-record
;; Output: same format as student-record where the plan is replaced by the GPA
;; Uses: getgrades to find the list of grades obtained by the student
;;       getgpa to compute the GPA from the list of grades
(define studentgpa
  (lambda (grade-table)
    (lambda (student-record)
      (list (car student-record) (cadr student-record)
            (getgpa (getgrades grade-table (car student-record)))))))

;; Helpers

;; input params: grade-table and student-id
;; output: list of grades for the student from the grade-table.
(define (getgrades grade-table std-id)
  (if (null? grade-table)
      grade-table
      (if (equal? (cadr (car grade-table)) std-id)
          (cons (cadr (cdr (car grade-table))) (getgrades (cdr grade-table) std-id))
          (getgrades (cdr grade-table) std-id))))

;; input param: list of grades
;; output: gpa
;; - Uses myapply to convert letter grade to numbers by applying
;;   the mapping function gradetonumber
;; - Uses calcgpa to first add the numbers (calcgp) and then divide by the
;;   the number of courses to find the GPA
(define getgpa
  (lambda (lst)
    (if (null? lst)
        0
        (calcgpa ((myapply gradetonumber) lst)))))


(define (calcgp lst)
  (if (null? lst)
      0
      (+ (car lst) (calcgp (cdr lst)))))

(define (calcgpa lst)
  (if (null? lst)
      0
      (/ (round (* (/ (calcgp lst) (length lst)) 100)) 100)))  ;; presented in piazza

;; Letter grade to number mapping
(define (gradetonumber x)
  (cond
    [(equal? x 'A) 4.0]
    [(equal? x 'B) 3.0]
    [(equal? x 'C) 2.0]
    [(equal? x 'D) 1.0]
    [(equal? x 'F) 0.0]))

;; This is from class
(define myapply
  (lambda (f)
    (lambda (lst)
      (if (null? lst)
          lst
          (cons (f (car lst)) ((myapply f) (cdr lst)))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; Ans: 3(d)
;; Input params: course id, grade-table and some function that can be applied
;;               on student-id
;; Output: list format where each element is result of application of f followed by
;;         the grade f the student in that course
(define gradebook
  (lambda (course)
    (lambda (g-table)
      (lambda (f)
        (if (null? g-table)
            g-table
            (if (equal? (car (car g-table)) course)
                ;; if the course matches
                ;; append the result of application of f to the grade information and then add it to the rest of the result
                (cons (append (f (cadr (car g-table))) (list (cadr (cdr (car g-table))))) (((gradebook course) (cdr g-table)) f))
                (((gradebook course) (cdr g-table)) f)))))))









