#lang racket

(require json)
(require "address.rkt")

(provide
 (contract-out
  [event? contract?]
  [event-origin-address (-> event? address?)]
  [event:parse (-> jsexpr? event?)]
  [event:to-json (-> event? jsexpr?)]
  [event:make (-> address? number? any/c event?)]
  ))

(struct event [origin-address id data] #:transparent)

(define (event:parse json-data)
  (event (hash-ref json-data 'origin-address)
         (hash-ref json-data 'id)
         (hash-ref json-data 'data)))

(define (event:make add id data)
  (event add id data))

(define (event:to-json event)
  (hash 'origin-address (event-origin-address event)
        'id (event-id event)
        'data (event-data event)))