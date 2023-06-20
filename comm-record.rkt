#lang racket

(require "event.rkt")
(require "address.rkt")

(provide
 (contract-out
  [comm-record? contract?]
  [comm-record-origin-address (-> comm-record? address?)]
  [comm-record:make (-> event? comm-record?)]
  ))
  

(struct comm-record [event time consensus] #:transparent)

(define (comm-record:make evt)
  (comm-record evt (current-seconds) #f))

(define (comm-record-origin-address cr)
  (event-origin-address (comm-record-event cr)))