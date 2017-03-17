module Queue exposing (Queue, empty, first, enq, deq, toList)

import List


type Queue a
    = Queue (List a)


empty : Queue a
empty =
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
