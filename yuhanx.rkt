#lang racket
(provide (all-defined-out)) ;; for us to test
(require racket/trace) ;; in case you want to use tracing
(require "DB.rkt") ;; to access the database definitions in DB.rkt


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
    (list (first student-record) (second student-record) (length (third student-record))))) ;; return the list

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
  (/(round(*(/ (countgpa id grade-table 0) (countcourses id grade-table 0))100))100))

;; Helper function to find the total number of grade
;; Not using lambda parameters as I don’t need to
;; id: student-id
;; grade-table: grade-table
;; gpa: the sum of all grade
;; example: (countgpa 0 grade-table 0): 11.0
;; example: (countgpa 1 grade-table 0): 9.0
(define (countgpa id grade-table gpa)
  (if (null? grade-table)
      gpa ;;return the sum of all grade
      (if (not (equal? id (second (car grade-table)))) ;; check if the id is the same in the grade-record
          (countgpa id (cdr grade-table) gpa) ;; if false, do nothing and recursion on the next grade-record
          (if (equal? (third (car grade-table)) 'A) ;; if true, check is the grade is A
              (countgpa id (cdr grade-table) (+ gpa 4.0)) ;; if true, add 4 to gpa and recursion on the next grade-record 
              (if (equal? (third (car grade-table)) 'B) ;; if false, check is the grade is B
                  (countgpa id (cdr grade-table) (+ gpa 3.0)) ;; if true, add 3 to gpa and recursion on the next grade-record 
                  (if (equal? (third (car grade-table)) 'C) ;; if false, check is the grade is C
                      (countgpa id (cdr grade-table) (+ gpa 2.0)) ;; if true, add 2 to gpa and recursion on the next grade-record 
                      (if (equal? (third (car grade-table)) 'D) ;; if false, check is the grade is D
                          (countgpa id (cdr grade-table) (+ gpa 1.0)) ;; if true, add 2 to gpa and recursion on the next grade-record 
                          (countgpa id (cdr grade-table) gpa) ;; if false, do nothing and recursion on the next grade-record
                          )))))))

;; Helper function to find the total number of courses
;; Not using lambda parameters as I don’t need to
;; id: student-id
;; grade-table: grade-table
;; courses: the number of course
;; example: (countcourses 0 grade-table 0): 3
;; example: (countcourses 1 grade-table 0): 3
(define (countcourses id grade-table courses)
  (if (null? grade-table)
      courses ;; return the number of course
      (if (equal? id (second (car grade-table))) ;; check if the id is the same in the grade-record
          (countcourses id (cdr grade-table) (add1 courses)) ;; if true, add 1 to courses and recursion on the next grade-record
          (countcourses id (cdr grade-table) courses)))) ;; if false, do nothing and recursion on the next grade-record
      
  
          
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
              
                  
  
  