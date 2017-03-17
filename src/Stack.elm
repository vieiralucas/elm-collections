module Stack exposing (Stack, empty, top, pop, push, toList)

import List


type Stack a
    = Stack (List a)


empty : Stack a
empty =
    Stack []


top : Stack a -> Maybe a
top (Stack stack) =
    List.head stack


pop : Stack a -> Stack a
pop (Stack stack) =
    Stack (List.drop 1 stack)


push : a -> Stack a -> Stack a
push a (Stack stack) =
    Stack (a :: stack)


toList : Stack a -> List a
toList (Stack stack) =
    stack
