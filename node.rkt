#lang racket

(require racket/async-channel)
(require "address.rkt")
(require json)

(provide
 (contract-out
  [node-port (-> node? number?)]
  [node-incoming (-> node? async-channel?)]
  [node:make (-> port-number? async-channel? async-channel? node?)]
  [node:run (-> node? void?)]
  [node:send (-> address? jsexpr? void?)]
  ))



(struct node [port incoming outgoing shutdown-channel] #:transparent)

(define (node:make port incoming shutdown-channel)
  (node port incoming shutdown-channel))

; node:run
; Node
(define (node:run node)
  (parameterize ([current-output-port (open-output-file (string-append "log/" (number->string (node-port node)) ".txt") #:exists 'replace)])
    
    (define listener (tcp-listen (node-port node)))

    (sleep 2)
    (log (string-append "starting: " (number->string (node-port node))))

    (with-handlers ([exn:fail (λ (e) (let ([error-output-port (open-output-file (string-append "error_" (number->string (node-port node)) "_listening"))])
                                       (println e)))])
      (thread (λ () (listen-on-network node listener))))

    (if (async-channel-get (node-shutdown-channel node))
        (begin 
          (tcp-close listener)
          (log "shutting down")
          (flush-output)
          (close-output-port (current-output-port)))
        "don't know what happened, got false off the channel")))

(define (listen-on-network node listener)
  (let listening-loop ()
    (define msg (accept-from-listener listener))
    (async-channel-put (node-incoming node) msg)
    (log (string-append "rec: " msg))
    (listening-loop)))

(define (accept-from-listener listener)
  (define-values (in out) (tcp-accept/enable-break listener))
  (define msg (event:parse (read-json in)))`
  (close-output-port out)
  msg)

(define (node:send address network-message)
  (define-values (in out) (tcp-connect (address-host address) (address-port address)))
  (write-json network-message out)
  (close-input-port in)
  (close-output-port out))
;------------------------------------------utils
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

(define (ensure-length s l)
  (define diff (- l (string-length s)))
  (cond
    [(< 0 diff) (ensure-length (string-append s " ") l)]
    [(= 0 diff) s]
    [(> 0 diff) (substring s 0 l)]))