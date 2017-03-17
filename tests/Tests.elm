module Tests exposing (..)

import Test exposing (..)
import Expect
import Stack exposing (Stack)
import Queue exposing (Queue)


emptyStack : Stack Int
emptyStack =
    Stack.initialize


oneElementStack : Stack Int
oneElementStack =
    Stack.push 1 emptyStack


twoElementStack : Stack Int
twoElementStack =
    Stack.push 2 oneElementStack


emptyQueue : Queue Int
emptyQueue =
    Queue.initialize


oneElementQueue : Queue Int
oneElementQueue =
    Queue.enq 1 emptyQueue


twoElementQueue : Queue Int
twoElementQueue =
    Queue.enq 2 oneElementQueue


all : Test
all =
    describe "Collections"
        [ describe "Stack"
            [ describe "top"
                [ test "Is nothing when stack is empty" <|
                    \() ->
                        Expect.equal (Stack.top emptyStack) Nothing
                , test "Is Just the top" <|
                    \() ->
                        Expect.equal (Stack.top oneElementStack) (Just 1)
                ]
            , describe "pop"
                [ test "Empty remains empty" <|
                    \() ->
                        Expect.equal (Stack.pop emptyStack) emptyStack
                , test "One element becames empty" <|
                    \() ->
                        Expect.equal (Stack.pop oneElementStack) emptyStack
                , test "Two elements becames one elements" <|
                    \() ->
                        Expect.equal (Stack.pop twoElementStack) oneElementStack
                ]
            , describe "push"
                [ test "Empty becames one" <|
                    \() ->
                        Expect.equal (Stack.push 1 emptyStack) oneElementStack
                , test "One element becames two" <|
                    \() ->
                        Expect.equal (Stack.push 2 oneElementStack) twoElementStack
                ]
            , describe "toList"
                [ test "Empty becames empty List" <|
                    \() ->
                        Expect.equal (Stack.toList emptyStack) []
                , test "One element becames one element list" <|
                    \() ->
                        Expect.equal (Stack.toList oneElementStack) [ 1 ]
                , test "Two elements becames two elements List" <|
                    \() ->
                        Expect.equal (Stack.toList twoElementStack) [ 2, 1 ]
                ]
            ]
        , describe "Queue"
            [ describe "first"
                [ test "Is nothing when queue is empty" <|
                    \() ->
                        Expect.equal (Queue.first emptyQueue) Nothing
                , test "Is Just the first element" <|
                    \() ->
                        Expect.equal (Queue.first oneElementQueue) (Just 1)
                ]
            , describe "deq"
                [ test "Empty remains empty" <|
                    \() ->
                        Expect.equal (Queue.deq emptyQueue) emptyQueue
                , test "One element becames empty" <|
                    \() ->
                        Expect.equal (Queue.deq oneElementQueue) emptyQueue
                , test "Two elements becames one elements" <|
                    \() ->
                        Expect.equal (Queue.deq twoElementQueue) oneElementQueue
                ]
            , describe "enq"
                [ test "Empty becames one" <|
                    \() ->
                        Expect.equal (Queue.enq 1 emptyQueue) oneElementQueue
                , test "One element becames two" <|
                    \() ->
                        Expect.equal (Queue.enq 2 oneElementQueue) twoElementQueue
                ]
            , describe "toList"
                [ test "Empty becames empty List" <|
                    \() ->
                        Expect.equal (Queue.toList emptyQueue) []
                , test "One element becames one element list" <|
                    \() ->
                        Expect.equal (Queue.toList oneElementQueue) [ 1 ]
                , test "Two elements becames two elements List" <|
                    \() ->
                        Expect.equal (Queue.toList twoElementQueue) [ 1, 2 ]
                ]
            ]
        ]
