#lang racket

(require "node.rkt")
(require racket/generator)
(require racket/async-channel)

(define port-generator (generator ()
                                  (let loop ([used-ports '()])
                                    (define free-port (let free-port-search ([choice (random 48620 49150)])
                                                        (if (false? (member choice used-ports))
                                                            choice
                                                            (free-port-search (random 48620 49150)))))
                                    (yield free-port)
                                    (loop (cons free-port used-ports)))))

(struct network [nodes shutdown-channel] #:transparent)

(define (network:make num-nodes)
  (define port-list (let port-gen-loop ([acc '()]
                                        [iter num-nodes])
                      (if (> iter 0)
                          (port-gen-loop (cons (port-generator) acc) (sub1 iter))
                          acc)))

  (define shutdown-channel (make-async-channel))

  (define nodes (for/list ([port port-list])
                  (node:make port
                             (filter (lambda (other) (not (equal? port other))) port-list)
                             (make-async-channel)
                             (make-async-channel)
                             shutdown-channel)))
  (network nodes shutdown-channel))

(define (network:num-nodes network)
  (length (network-nodes network)))

(define (network:run network)
  (parameterize ([current-custodian (make-custodian)])
    
    (define node-threads (for/list ([node (network-nodes network)])
                           (thread (lambda () (node:run node)))))
    
    (define event-thread (thread
                          (lambda ()
                            (define event-file (open-input-file "events.txt"))
                            (let event-sending-loop ([next-evt (read-line event-file)])
                              (if (eof-object? next-evt)
                                  (print "event file finished")
                                  (let ([rand (random 0 9)])
                                    (println "")
                                    (print rand)
                                    (if (< 0 rand)
                                        (let ([dest-node (list-ref (network-nodes network) (random 0 (network:num-nodes network)))])
                                          (print (string-append "evt: " next-evt ":" (number->string (node-port dest-node))))
                                          (async-channel-put (node-sending-channel dest-node) next-evt)
                                          (event-sending-loop (read-line event-file)))
                                        (begin
                                          (print "sleeping")
                                          (sleep 1)
                                          (event-sending-loop next-evt)))))))))
    
    (sleep 10)
    (println "shutting down network")
    (async-channel-put (network-shutdown-channel network) 'shutdown)
    (sleep 4)
    (custodian-shutdown-all (current-custodian))))

(define (run num)
  (network:run (network:make num)))