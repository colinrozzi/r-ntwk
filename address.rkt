#lang racket

(provide
 (contract-out
  [address? contract?]
  [address-host (-> address? string?)]
  [address-port (-> address? number?)]
  ; TODO: make a more specific contract
  [address:make (-> string? port-number? address?)]))

(struct address [host port])

(define (address:make host port)
  (address host port))