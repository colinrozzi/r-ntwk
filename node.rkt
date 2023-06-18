#lang racket

(define port (make-parameter 7999))
(define neighbor (make-parameter '()))


              
(define (connect-and-send-message neigh)
  (define-values (in out) (tcp-connect "127.0.0.1" (string->number neigh)))
  (define id (number->string (random 1 10)))
  (write (string-append id ": hey!") out)
  (close-input-port in)
  (close-output-port out)
  (println (string-append "sent: " id " to " neigh)))

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
          (println data)
          (loop)))
      (close-input-port in)
      (close-output-port out)))

(define (pick-a-neighbor neighbors)
  (print "picking")
  (define rand-index (random 0 (length neighbors)))
  (list-ref neighbors rand-index))

(define neighbors (command-line #:once-each
                     [("-p" "--port") p
                                      "The port to listen on"
                                      (port p)]
                     #:args neighbors  neighbors))

(define listener (tcp-listen (string->number (port))))

(sleep 2)
(println "starting")

(let loop ([rand (random 1 10)])
  (if (< rand 2)
      (connect-and-send-message (pick-a-neighbor neighbors))
      (listen-for-message listener))
  (sleep 1)
  (loop (random 1 10)))

(tcp-close listener)