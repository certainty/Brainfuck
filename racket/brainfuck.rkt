#! /usr/local/bin/racket

#lang racket

(require srfi/8)

(define +tokens+ (string->list "<>+-.,[]"))

(define (make-memory)
  (make-hasheq))

;; note the subtlety with regard to mod 256 instead of 255
;; because 0 has to be part of the set of possible numbers
(define (memory-update! memory ptr f)
  (let ((value (memory-ref memory ptr f)))
    (hash-set! memory ptr (modulo value 256))
    memory))

(define (memory-ref memory ptr (process values))
  (process (hash-ref memory ptr 0)))

(define (tokenize input)
  (filter (lambda (chr) (memq chr +tokens+)) (string->list input)))

(define (inspect inp)
  (display inp)
  (newline)
  inp)

(define (parse tokens)
  (let lp ((tokens tokens) (counter 1) (ast (list)))
    (cond
     ((null? tokens)
      (reverse ast))
     ((null? (cdr tokens))
      (lp (cdr tokens) counter (cons (cons (car tokens) counter) ast)))
     ((or (eq? #\[ (car tokens)) (eq? #\] (car tokens)))
      ;; do not compress loops
      (lp (cdr tokens) 1 (cons (cons (car tokens) 1) ast)))
     ((eq? (car tokens) (cadr tokens))
      (lp (cdr tokens) (add1 counter) ast))
     (else
      (lp (cdr tokens) 1 (cons (cons (car tokens) counter) ast))))))

(define (add-halt-node p)
  (append p (list '(halt . 1))))

(define (compile-jumps program)
  (do ((ip 0 (add1 ip)))
      ((= ip (vector-length program)) program)
    (cond
     ((eq? #\[ (car (vector-ref program ip)))
      (vector-set! program ip (cons #\[ (fwd-jump-addr ip program))))
     ((eq? #\] (car (vector-ref program ip)))
      (vector-set! program ip (cons #\] (back-jump-addr ip program))))
     (else program))))

(define compile (compose compile-jumps list->vector add-halt-node parse tokenize))

(define (bf-print n memory dp)
  (do ((n n (sub1 n)))
      ((zero? n) n)
    (display (memory-ref memory dp integer->char))))

(define (bf-read! n memory dp)
  (do ((n n (sub1 n)))
      ((zero? n) n)
    (let ((value (char->integer (read-char))))
      (memory-update! memory dp (const value)))))

(define (fwd-jump-addr ip program)
  (let lp ((ip (add1 ip)) (balance 0))
    (let ((token (car (vector-ref program ip))))
      (cond
       ((and (zero? balance) (eq? token #\]))
        ip)
       ((eq? token #\[)
        (lp (add1 ip) (add1 balance)))
       ((eq? token #\])
        (lp (add1 ip) (sub1 balance)))
       (else
        (lp (add1 ip) balance))))))

(define (back-jump-addr ip program)
  (let lp ((ip (sub1 ip)) (balance 0))
    (let ((token (car (vector-ref program ip))))
      (cond
       ((and (zero? balance) (eq? token #\[))
        ip)
       ((eq? token #\])
        (lp (sub1 ip) (add1 balance)))
       ((eq? token #\[)
        (lp (sub1 ip) (sub1 balance)))
       (else
        (lp (sub1 ip) balance))))))

(define (dump program memory ip dp fuel)
  (printf "\nPROGRAM DUMP\n====================================\n")
  (printf "Program: ip = ~a instr = ~a\n" ip (vector-ref program ip))
  (display (vector-copy program
                       (max 0 (- ip 3))
                       (min (sub1 (vector-length program)) (+ ip 4))))
  (printf "\n\nMemory: dp = ~a value = ~a\n" dp (memory-ref memory dp))
  (display memory)
  (printf "\n\nFuel: ~a\n=======================================\n" fuel))

;; rudimentary virtual machine
(define (run program fuel)
  (let ((memory (make-memory)))
    (let loop ((ip 0) (dp 0) (fuel fuel))
      (cond
       ((eq? 'halt (car (vector-ref program ip))) fuel)
       ((<= fuel 0)
        (newline)
        (display "PROCESS TIME OUT. KILLED!!!")
        (newline))
       (else
        (match (vector-ref program ip)
          ([cons #\+ n]
           (memory-update! memory dp (lambda (v) (+ v n)))
           (loop (add1 ip) dp (- fuel n)))

          ([cons #\- n]
           (memory-update! memory dp (lambda (v) (- v n)))
           (loop (add1 ip) dp (- fuel n)))

          ([cons #\> n]
           (loop (add1 ip) (+ dp n) (- fuel n)))

          ([cons #\< n]
           (loop (add1 ip) (- dp n) (- fuel n)))

          ([cons #\[ addr]
           (if (zero? (memory-ref memory dp))
               (loop addr dp (sub1 fuel))
               (loop (add1 ip) dp (sub1 fuel))))

          ([cons #\] addr]
           (if (zero? (memory-ref memory dp))
               (loop (add1 ip) dp (sub1 fuel))
               (loop addr dp (sub1 fuel))))

          ([cons #\. n]
           (bf-print n memory dp)
           (loop (add1 ip) dp (- fuel n)))

          ([cons #\, n]
           (bf-read! n memory dp)
           (loop (add1 ip) dp (- fuel n)))))))))

(define (read-parameters)
  (let* ((nm   (string-split    (read-line)))
         (n     (string->number (car nm)))
         (m     (string->number (cadr nm)))
         (input (substring (read-line) 0 (max 0 n))))
    (do ((line 1 (add1 line))
         (program (list) (cons (read-line) program)))
        ((> line m) (values (string-join (reverse program)) input)))))

(define (main)
  (receive (program-text input) (read-parameters)
    (with-input-from-string input
      (lambda ()
        (run (compile program-text) 100000)
        (void)))))

(main)
