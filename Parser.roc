interface Parser
    exposes [Parser, Problem, PResult, map, try, run, complete, genericError]
    imports [Bool.{true, false}]

Problem : [Failure [NoMatch]]
PResult i o : Result (i, o) Problem

Parser i o : i -> PResult i o

## Runs a parser against a given input
run : Parser i o, i -> Result o Problem
run = \parser, input ->
    parser input
        |> Result.map \(_, out) -> out

expect run oneParser [1, 2] == Ok 1

## Run a parser against a given `List` input.
## Returns the result of the parser,
complete : Parser (List i) o, (List i) -> Result o Problem
complete = \parser, input->
        when parser input is
            Ok ([], out) -> Ok out
            Ok (_, _) -> Err genericError
            Err err -> Err err

expect when complete oneParser [1] is
    Ok 1 -> true
    _ -> false
expect complete oneParser [1, 2] |> Result.isErr

map : Parser i o1, (o1 -> o2) -> Parser i o2
map = \parser, f ->
    \input ->
        parser input |> Result.map \(rest, o1) -> (rest, f o1)

expect
    parser = map oneParser \x -> x + 2
    run parser [1] == Ok 3

try : Parser i o1, (o1 -> Result o2 Problem) -> Parser i o2
try = \parser, f ->
    \input ->
        parser input
            |> Result.try \(rest, o1) -> 
                f o1 |> Result.map \o2 -> (rest, o2)

expect
    parser = try oneParser (\o1 -> Ok (o1 + 1)) 
    run parser [1, 0] == Ok 2

expect
    parser = try oneParser (\_ -> Err genericError)
    run parser [1, 0] == Err genericError

## for testing only
oneParser = \input ->
    when input is
        [1, .. as rest] -> Ok (rest, 1)
        _ -> Err genericError

genericError = Failure NoMatch
