#lang racket

(require racket/async-channel)

(provide
 (contract-out
  [node:make (-> number? (listof number?) async-channel? async-channel? async-channel? node?)]
  [node:run (-> node? void?)]
  [node-port (-> node? number?)]
  [node-received-channel (-> node? async-channel?)]
  [node-sending-channel (-> node? async-channel?)]
  ))

(struct node [port neighbors received-channel sending-channel shutdown-channel] #:transparent)

(define (node:make port neighbors received-channel sending-channel shutdown-channel)
  (node port neighbors received-channel sending-channel shutdown-channel))

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
    (with-handlers ([exn:fail (λ (e) (let ([error-output-port (open-output-file (string-append "error_" (number->string (node-port node)) "_sending"))])
                                       (println e)))])
      (thread (λ () (sending-on-network node))))

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
    (async-channel-put (node-received-channel node) msg)
    (log (string-append "rec: " msg))
    (listening-loop)))

(define (accept-from-listener listener)
  (define-values (in out) (tcp-accept/enable-break listener))
  (define msg (read in))
  (close-input-port in)
  (close-output-port out)
  msg)

(define (sending-on-network node)
  (let sending-loop ()
    (define new-event (async-channel-get (node-sending-channel node)))
    (define dest (random-neighbor node))
    (neighbor:send dest new-event)
    (log (string-append "sen: " new-event " dest: " (number->string dest)))
    (sending-loop)))


(define (random-neighbor node)
  (list-ref (node-neighbors node) (random 0 (length (node-neighbors node)))))

(define (neighbor:send neighbor-port event)
  (define-values (in out) (tcp-connect "127.0.0.1" neighbor-port))
  (write event out)
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