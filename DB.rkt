#lang racket
(provide (all-defined-out)) ;; for us to test
(require racket/trace) ;; in case you want to use tracing


;; Student
(define student-table
  '( (0 Asterix (342 230 412 227))
     (1 Obelix  (342 228 227 230))))


;; Grades
(define grade-table
  '( (342 0 A)
     (227 0 B)
     (412 0 A)
     (227 1 C)
     (228 1 A)
     (342 1 B)
     ))

;; Apply on student-table
;; student-table: student-table
;; f: functions to apply one student-table. It could be numberofcourse and studentgpa
;; example: ((applyonstds student-table) numberofcourses): '((0 Asterix 4) (1 Obelix 4))
;; example: ((applyonstds student-table) (studentgpa grade-table)): '((0 Asterix 3.67) (1 Obelix 3.0))
(define applyonstds
  (lambda (student-table)
    (lambda (f)
      (if (null? student-table)
          student-table ;; nothing to do
          (cons ( f (car student-table)) ((applyonstds (cdr student-table)) f)))))) ;; recursion on student-table and add all list together


;; Return a list with student-id, student name and the number of courses the student has
;; student-record: student-record
;; example: (numberofcourses '(0 Asterix (342 230 412 227))): '(0 Asterix 4)
;; example: (numberofcourses '(1 Obelix  (342 228 227 230))): '(1 Obelix 4)
;; example: ((applyonstds student-table) numberofcourses): '((0 Asterix 4) (1 Obelix 4))
(define numberofcourses
  (lambda (student-record)
    (if (null? student-record)
        student-record ;; nothing to do
        (list (first student-record) (second student-record) (length (third student-record)))))) ;; return the list

;; Return a list with student-id, student name and student gpa
;; grade-table: grade-table
;; student-record: student-record
;; example: ((studentgpa grade-table) '(0 Asterix (342 230 412 227))): '(0 Asterix 3.67)
;; example: ((studentgpa grade-table) '(1 Obelix  (342 228 227 230))): '(1 Obelix 3.0)
;; example: ((applyonstds student-table) (studentgpa grade-table)): '((0 Asterix 3.67) (1 Obelix 3.0))
(define studentgpa
  (lambda (grade-table)
    (lambda (student-record)
      (if (null? student-record)
          student-record ;; nothing to do
          (list (first student-record) (second student-record) (gpa (first student-record) grade-table)))))) ;; return the list


;; Helper function to calculate gpa
;; Not using lambda parameters as I don’t need to
;; id: student-id
;; grade-table: grade-table
;; example: (gpa 0 grade-table): 3.67
;; example: (gpa 1 grade-table): 3.0
(define (gpa id grade-table)
  (let* ([sum 0] [count 0]) ;; let sum = 0 and count = 0
    (for ([grade-record grade-table]) ;; for loop through grade-table
      (if (not (equal? (second grade-record) id)) ;; check if the student-id is the same in the grade-record
          (set! sum (+ sum 0)) ;; nothing to do if false
          ;; add sum with gpa if true
          (if (equal? (third grade-record) 'A) ;; check if the grade is A
              (set! sum (+ sum 4.0)) ;; if true, add 4 to sum
              (if (equal? (third grade-record) 'B) ;; check if the grade is B
                  (set! sum (+ sum 3.0)) ;; if true, add 3 to sum
                  (if (equal? (third grade-record) 'C) ;; check if the grade is C
                      (set! sum (+ sum 2.0)) ;; if true, add 2 to sum
                      (if (equal? (third grade-record) 'D) ;;check is the grade is D
                          (set! sum (+ sum 1.0)) ;; if ture, add 1 to sum
                          (set! sum (+ sum 0)) ;; if false, do nothing
                          ))))))
    (for ([grade-record grade-table]) ;; for loop through grade-table
      (if (equal? (second grade-record) id) ;; check if the student-id is the same in the grade-record
          (set! count (add1 count)) ;; if true, add 1 to count
          (set! count (+ count 0)))) ;; if false, do nothing
    (/(round(*(/ sum count)100))100))) ;; return the gpa with 2 deciaml digits
              
          
;; returns the list of records for the students who have taken the course followed by the grade
;; course-id: course-id
;; grade-table: grade-table
;; f: function to apply on grade-table. It could be find student with gpa
;; example: (((gradebook 227) grade-table) ((findstudentwithgpa student-table)grade-table)): '((0 Asterix 3.67 B) (1 Obelix 3.0 C))
;; example: (((gradebook 342) grade-table) ((findstudentwithgpa student-table)grade-table)): '((0 Asterix 3.67 A) (1 Obelix 3.0 B))
(define gradebook
  (lambda (course-id)
    (lambda (grade-table)
      (lambda (f)
        (if (null? grade-table)
            grade-table ;; do nothing
            (if (equal? course-id (first (first grade-table))) ;; check if the coursre-id is the same in grade-table
                ;; if true, return the list and recursion on the rest of the grade-table
                (cons (list (car (car (f (second (first grade-table))))) (second (car (f (second (first grade-table))))) (third (car (f (second (first grade-table))))) (third (first grade-table))) (((gradebook course-id) (cdr grade-table)) f))
                ;; if false, recursion on the rest of grade-table
                (((gradebook course-id) (cdr grade-table)) f)))))))
        
    
;; return the student record with gpa
;; student-table: student-table
;; grade-table: grade-table
;; student-id: student-id
;; example: (((gradebook 227) grade-table) ((findstudentwithgpa student-table)grade-table)): '((0 Asterix 3.67 B) (1 Obelix 3.0 C))
;; example: (((findstudentwithgpa student-table)grade-table)0): '((0 Asterix 3.67))
;; example: (((findstudentwithgpa student-table)grade-table)1): '((1 Obelix 3.0))
(define findstudentwithgpa
  (lambda (student-table)
    (lambda (grade-table)
      (lambda (student-id)
        (for/list ([student ((applyonstds student-table) (studentgpa grade-table))] ;; for loop through list returned by ((applyonstds student-table) (studentgpa grade-table))
                   #:when (equal? student-id (car student))) ;; when student-id is the same in the student record
          student))))) ;; return the student record
              
                  
  
  

            
  
          