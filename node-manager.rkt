#lang racket

(require "node.rkt")
(require "event.rkt")
(require "comm-record.rkt")
(require "utils.rkt")
(require racket/async-channel)

(struct node-manager [node peer-nodes] #:transparent)

(struct peer-node [address])

(define (node-manager:make peer-nodes port received-channel sending-channel shutdown-channel)
  (node-manager (node:make port received-channel sending-channel shutdown-channel)
                peer-nodes
                '()))

(define (node-manager:run node-manager)
  (parameterize ([current-output-port (open-output-file (string-append "log/" (number->string (node-port (node-manager-node node-manager))) ".txt") #:exists 'replace)])
    (define active-node (node:run (node-manager-node node-manager)))

    ;say hello! whats up other nodes
    (say-hello node-manager)

    ;be a participant on the network
    (active-network-operations node-manager)))

(define (say-hello nm)
  (for ([peer-node (node-manager-peer-nodes)])
    (node:send (peer-node-address peer-node) (hello-msg nm))))

(define (hello-msg nm)
  (event:make (node-address (node-manager-node nm))
              1
              "hello!"))

(struct network-state [active-nodes comms])
; TODO: holding past comms in both places is a parallel data structure
(struct active-node [address past-comms])

(define (active-node:make address)
  (active-node address '()))

(define (active-node:add-comm an comm)
  (struct-copy active-node an
               [past-comms (cons comm (active-node-past-comms active-node))]))

(define (node-manager-incoming node-manager)
  (node-incoming (node-manager-node node-manager)))

(define (active-network-operations node-manager)
  (let loop ([network-state (network-state:make)])
    (define new-network-state
      (cond
        [(async-channel-try-get (node-manager-incoming node-manager)) => (λ (evt) (network-state:update network-state evt))]
        [else network-state]))
    (define final-network-state (network-state:take-action new-network-state))
    (loop final-network-state)))

(define (network-state:make)
  (network-state '() '()))

(define (network-state:take-action ns)
  ns)
    
    ; it is an issue i am only taking action on the network when I am getting new info
    ; maybe split taking new info from node and updating network state from taking actions and communicate
    ; changes to the network state
    ; or just process network incoming messages when they are ready to be read - and otherwise continually loop
    ; and check if any new actions need to be taken

(define (network-state:update ns event)
  (define comm (comm-record:make event))
  (struct-copy network-state ns
               [active-nodes (network-state:update-active-nodes ns comm)]
               [comms (cons comm (network-state-comms))]))

(define (network-state:update-active-nodes ns cr)
  (map (λ (peer)
         (if (equal? (peer-node-address peer)
                     (comm-record-origin-address cr))
             (struct-copy active-node peer
                          [past-comms (cons cr (active-node-past-comms peer))])
             peer))
       (network-state-active-nodes ns)))
    
