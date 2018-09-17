This library provides Queue and Stack implementations

```bash
elm-package install ceddlyburge/elm-collections
```

## Stack

```elm
empty : Stack a

push : a -> Stack a -> Stack a

pushMany : a -> Int -> Stack a -> Stack a

pushManyFromFunction : (() -> a) -> Int -> Stack a -> Stack a

top : Stack a -> Maybe a

pop : Stack a -> ( Maybe a, Stack a)

toList : Stack a -> List a
```


#### Example

```elm
import Stack

main =
    Stack.empty
        |> Stack.push 7
        |> Stack.push 42
        |> Stack.pop |> snd
        |> Stack.top
        |> toString
        |> Html.text
            -- Shows "Just 7"
```

## Queue (FIFO)

```elm
empty : Queue a

enq : a -> Queue a -> Queue a

enqMany : a -> Int -> Queue a -> Queue a

enqManyFromFunction : (() -> a) -> Int -> Queue a -> Queue a

first : Queue a -> Maybe a

deq : Queue a -> ( Maybe a, Queue a)

toList : Queue a -> List a
```


#### Example

```elm
import Queue

main =
    Queue.empty
        |> Queue.enq 7
        |> Queue.enq 42
        |> Queue.deq |> snd
        |> Queue.first
        |> toString
        |> Html.text
            -- Shows "Just 42"
```
