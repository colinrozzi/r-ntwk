#lang racket

(provide
 (contract-out
  [node:make (-> number? (listof number?) node?)]
  [node:run (-> node? channel? void?)]))

(define (log s)
  (define cur-date (seconds->date (current-seconds)))
  (define date-string (string-append
                       (number->string (date-hour cur-date))
                       ":"
                       (number->string (date-minute cur-date))
                       ":"
                       (number->string (date-second cur-date))))
  (println (string-append date-string " : " s))
  (flush-output (current-output-port)))

(define (connect-and-send-message neigh)
  (define-values (in out) (tcp-connect "127.0.0.1" neigh))
  (define id (number->string (random 1 10)))
  (write (string-append id ": hey!") out)
  (close-input-port in)
  (close-output-port out)
  (log (string-append "sent: " id " to " (number->string neigh))))

(define (listen-for-message listener)
  (if (tcp-accept-ready? listener)
      (accept-and-print listener)
      #f))

(define (accept-and-print listener)
  (let-values ([(in out) (tcp-accept listener)])
      (let loop ()
        (define data (read in))
        (unless (eof-object? data)
          ; Process the received data
          (if (string? data)
              (log data)
              (println data))
          (loop)))
      (close-input-port in)
      (close-output-port out)))

(define (pick-a-neighbor neighbors)
  (log "picking")
  (define rand-index (random 0 (length neighbors)))
  (list-ref neighbors rand-index))

(struct node [port neighbors])

(define (node:make port neighbors)
  (node port neighbors))

(define (node:run node ch)
  (parameterize ([current-output-port (open-output-file (string-append "log_" (number->string (node-port node)) ".txt") #:exists 'replace)])
    
    (define listener (tcp-listen (node-port node)))

    (sleep 2)
    (log "starting")

    (let loop ([rand (random 1 10)])

      (with-handlers ([exn:fail? (lambda (exn) (println exn))])
        (if (< rand 2)
            (connect-and-send-message (pick-a-neighbor (node-neighbors node)))
            (listen-for-message listener)))
      
      (flush-output)
      
      (if (equal? (channel-try-get ch) 'shutdown)
          (begin
            (tcp-close listener)
            (log "shutting down")
            (close-output-port (current-output-port)))
          (begin
            (sleep 1)
            (loop (random 1 10)))))))

    
  