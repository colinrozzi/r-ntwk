#lang racket

(struct node-manager [node peer-nodes] #:transparent)

(struct peer-node [address])

(define (node-manager:make peer-nodes port received-channel sending-channel shutdown-channel)
  (node-manager (node:make port received-channel sending-channel shutdown-channel)
                peer-nodes
                '()))

(define (node-manager:run node-manager)
  (define active-node (node:run (node-manager-node node-manager)))

  ;say hello! whats up other nodes
  (send-on-network (node-manager-peer-nodes) hello-message)

  ;be a participant on the network
  )

(struct network-state [active-nodes comms])
; TODO: holding past comms in both places is a parallel data structure
(struct active-node [address past-comms])

(define (active-node address)
  (active-node address past-comms))

(define (active-node:add-comm an comm)
  (struct-copy active-node an
               [past-comms (cons comm (active-node-past-comms active-node))]))

(define (node-manager-incoming node-manager)
  (node-incoming (node-manager-node node-manager)))

(define (active-network-operations node-manager)
  (let loop ([network-state (network-state:make)])
    (define new-network-state
      (cond
        [(async-channel-try-get (node-manager-incoming node-manager)) =>
                                                                      (λ (evt) (network-state:update network-state evt))]
        [else network-state]))
    (define final-network-state (network-state:take-action new-network-state))
    (loop final-network-state)))
    
    ; it is an issue i am only taking action on the network when I am getting new info
    ; maybe split taking new info from node and updating network state from taking actions and communicate
    ; changes to the network state
    ; or just process network incoming messages when they are ready to be read - and otherwise continually loop
    ; and check if any new actions need to be taken

(define (network-state:update ns event)
  (define comm (comm-record: make event))
  (struct-copy network-state ns
               [active-nodes (network-state:update-active-nodes ns comm-record)]
               [events (cons comm-record (network-state-comms))]))


    
    ;wait for a message to come in
    ;update the network state (#events, consensus, etc)
    ;check if any actions have to be taken with the new state
    ;call the loop with the new state))