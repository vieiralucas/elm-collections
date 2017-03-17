module Tests exposing (..)

import Test exposing (..)
import Expect
import Stack exposing (Stack)


emptyStack : Stack Int
emptyStack =
    Stack.initialize


oneElement : Stack Int
oneElement =
    Stack.push 1 emptyStack


twoElement : Stack Int
twoElement =
    Stack.push 2 oneElement


all : Test
all =
    describe "Stack"
        [ describe "top"
            [ test "Is nothing when stack is empty" <|
                \() ->
                    Expect.equal (Stack.top emptyStack) Nothing
            , test "Is Just the top" <|
                \() ->
                    Expect.equal (Stack.top oneElement) (Just 1)
            ]
        , describe "pop"
            [ test "Empty remains empty" <|
                \() ->
                    Expect.equal (Stack.pop emptyStack) emptyStack
            , test "One element becames empty" <|
                \() ->
                    Expect.equal (Stack.pop oneElement) emptyStack
            , test "Two elements becames one elements" <|
                \() ->
                    Expect.equal (Stack.pop twoElement) oneElement
            ]
        , describe "push"
            [ test "Empty becames one" <|
                \() ->
                    Expect.equal (Stack.push 1 emptyStack) oneElement
            , test "One element becames two" <|
                \() ->
                    Expect.equal (Stack.push 2 oneElement) twoElement
            ]
        , describe "toList"
            [ test "Empty becames empty List" <|
                \() ->
                    Expect.equal (Stack.toList emptyStack) []
            , test "One element becames one element list" <|
                \() ->
                    Expect.equal (Stack.toList oneElement) [ 1 ]
            , test "Two elements becames two elements List" <|
                \() ->
                    Expect.equal (Stack.toList twoElement) [ 2, 1 ]
            ]
        ]
