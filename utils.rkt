#lang racket

(provide
 (contract-out
  [ensure-length (-> string? number? string?)]
  [log (-> string? void?)]))

(define (ensure-length s l)
  (define diff (- l (string-length s)))
  (cond
    [(< 0 diff) (ensure-length (string-append s " ") l)]
    [(= 0 diff) s]
    [(> 0 diff) (substring s 0 l)]))

(define (log s)
  (define cur-date (seconds->date (current-seconds)))
  (define date-string (string-append
                       (number->string (date-minute cur-date))
                       ":"
                       (number->string (date-second cur-date))
                       ":"
                       (number->string (- (current-milliseconds) (* 1000 (current-seconds))))))
  (println (string-append (ensure-length date-string 8) " " s))
  (flush-output (current-output-port)))