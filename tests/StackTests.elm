module StackTests exposing (..)

import Test exposing (..)
import Fuzz exposing (intRange)
import Expect
import Stack exposing (Stack)


all : Test
all =
    describe "stack"
        [ test "empty Stack contains nothing" <|
            \() ->
                Stack.empty
                    |> Stack.top
                    |> Expect.equal Nothing
        , test "top is the last pushed el" <|
            \() ->
                Stack.empty
                    |> Stack.push 1
                    |> Stack.push 2
                    |> Stack.top
                    |> Expect.equal (Just 2)
        , test "pop does nothing for empty Stack" <|
            \() ->
                Stack.empty
                    |> Stack.pop
                    |> Expect.equal ( Nothing, Stack.empty )
        , test "pop removes last pushed element" <|
            \() ->
                Stack.empty
                    |> Stack.push 1
                    |> Stack.pop
                    |> Expect.equal ( Just 1, Stack.empty )
        , test "toList of empty gives empty List" <|
            \() ->
                Stack.empty
                    |> Stack.toList
                    |> Expect.equal []
        , test "toList converts stack to List" <|
            \() ->
                Stack.empty
                    |> Stack.push 1
                    |> Stack.push 2
                    |> Stack.toList
                    |> Expect.equal [ 2, 1 ]
        , test "fromList converts list to stack" <|
            \() ->
                Stack.fromList [ 1, 2, 3 ]
                    |> Expect.equal
                        (Stack.empty
                            |> Stack.push 1
                            |> Stack.push 2
                            |> Stack.push 3
                        )
        , test "append appends a list to the stack" <|
            \() ->
                Stack.empty
                    |> Stack.push 1
                    |> Stack.append [ 2, 3 ]
                    |> Stack.toList
                    |> Expect.equal [ 3, 2, 1 ]
        , test "restack of Empty does nothing" <|
            \() ->
                Stack.empty
                    |> Stack.restack increment
                    |> Expect.equal Stack.empty
        , test "restack pop, processes, and then pushes" <|
            \() ->
                Stack.empty
                    |> Stack.push 1
                    |> Stack.push 3
                    |> Stack.restack increment
                    |> Stack.toList
                    |> Expect.equal [ 4, 1 ]
        , fuzz (intRange 0 50) "pushMany enqueus count copies of item" <|
            \(count) ->
                Stack.empty
                    |> Stack.pushMany 0 count 
                    |> Stack.toList
                    |> List.length
                    |> Expect.equal count
        , fuzz (intRange 0 50) "pushManyFromFunction enqueus calls enqueues result of function count times" <|
            \(count) ->
                Stack.empty
                    |> Stack.pushManyFromFunction (\() -> 0) count 
                    |> Stack.toList
                    |> List.length
                    |> Expect.equal count
        ]

increment: Int -> Int
increment i = 
    i + 1