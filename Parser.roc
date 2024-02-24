interface Parser
    exposes [Parser, Problem, PResult, map, try, run, complete, genericError]
    imports [Bool.{true, false}, Util]

Problem : [Failure [NoMatch]]
PResult i o : Result (i, o) Problem

Parser i o : i -> PResult i o

## Runs a parser against a given input
run : Parser i o, i -> Result o Problem
run = \parser, input ->
    parser input
        |> Result.map \(_, out) -> out

expect run oneParser [1, 2] |> Util.andEq 1

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

try : Parser i o1, ((i, o1) -> PResult i o2) -> Parser i o2
try = \parser, f ->
    \input ->
        parser input |> Result.try f

## for testing only
oneParser = \input ->
    when input is
        [1, .. as rest] -> Ok (rest, 1)
        _ -> Err genericError

genericError = Failure NoMatch
