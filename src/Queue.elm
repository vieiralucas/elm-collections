module Queue exposing (Queue, initialize, first, enq, deq, toList)

import List


type Queue a
    = Queue (List a)


initialize : Queue a
initialize =
    Queue []


first : Queue a -> Maybe a
first (Queue queue) =
    List.head queue


enq : a -> Queue a -> Queue a
enq a (Queue queue) =
    Queue (List.append queue [ a ])


deq : Queue a -> Queue a
deq (Queue queue) =
    let
        length =
            List.length queue
    in
        Queue (List.take (length - 1) queue)


toList : Queue a -> List a
toList (Queue queue) =
    queue
