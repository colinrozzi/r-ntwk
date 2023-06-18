#lang racket

(require "node.rkt")

(parameterize ([current-custodian (make-custodian)])

  (define ch (make-channel))
  
  (define n1 (node:make 7777 (list 7878 7979)))
  (define n2 (node:make 7878 (list 7979 7777)))
  (define n3 (node:make 7979 (list 7777 7878)))

  (thread (lambda () (node:run n1 ch)))
  (thread (lambda () (node:run n2 ch)))
  (thread (lambda () (node:run n3 ch)))
  (sleep 10)
  (print "shutting down")
  (channel-put ch 'shutdown)
  (sleep 2)
  (custodian-shutdown-all (current-custodian)))