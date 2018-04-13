module QueueTests exposing (..)

import Test exposing (..)
import Fuzz exposing (intRange)
import Expect
import Queue exposing (Queue)


all : Test
all =
    describe "Queue"
        [ test "empty queue contains nothing" <|
            \() ->
                Queue.empty
                    |> Queue.first
                    |> Expect.equal Nothing
        , test "first is the first enq element" <|
            \() ->
                Queue.empty
                    |> Queue.enq 1
                    |> Queue.enq 2
                    |> Queue.first
                    |> Expect.equal (Just 1)
        , test "deq empty Queue does nothing" <|
            \() ->
                Queue.empty
                    |> Queue.deq
                    |> Expect.equal ( Nothing, Queue.empty )
        , test "deq eliminates first element" <|
            \() ->
                Queue.empty
                    |> Queue.enq 1
                    |> Queue.enq 2
                    |> Queue.deq
                    |> Expect.equal ( Just 1, (Queue.empty |> Queue.enq 2) )
        , test "toList of empty gives empty List" <|
            \() ->
                Queue.empty
                    |> Queue.toList
                    |> Expect.equal []
        , test "toList converts queue to List" <|
            \() ->
                Queue.empty
                    |> Queue.enq 1
                    |> Queue.enq 2
                    |> Queue.toList
                    |> Expect.equal [ 1, 2 ]
        , test "requeue of Empty does nothing" <|
            \() ->
                Queue.empty
                    |> Queue.requeue increment
                    |> Expect.equal Queue.empty
        , test "requeue dequeues, processes, and then queues" <|
            \() ->
                Queue.empty
                    |> Queue.enq 1
                    |> Queue.enq 3
                    |> Queue.requeue increment
                    |> Queue.toList
                    |> Expect.equal [ 3, 2 ]
        , fuzz (intRange 0 50) "enqCopies enqueus count copies of item" <|
            \(count) ->
                Queue.empty
                    |> Queue.enqMany 0 count 
                    |> Queue.toList
                    |> List.length
                    |> Expect.equal count
        , fuzz (intRange 0 50) "enqManyFromFunction enqueus calls enqueues result of function count times" <|
            \(count) ->
                Queue.empty
                    |> Queue.enqManyFromFunction (\() -> 0) count 
                    |> Queue.toList
                    |> List.length
                    |> Expect.equal count
        ]

increment: Int -> Int
increment i = 
    i + 1