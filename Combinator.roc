interface Combinator
    exposes [
        matches,
        alt,
        many,
        many0,
        surrounded,
        prefixed,
        suffixed,
        andThen,
     ]
    imports [Parser.{Parser}]

alt : List (Parser i o) -> Parser i o
alt = \parsers ->
    \input ->
        tryParser = \(input1, _), parser ->
            when parser input1 is
                Err _ -> Continue (input1, Nothing)
                Ok (currRest, out) -> Break (currRest, Just out)

        (rest, maybeOut) = List.walkUntil parsers (input, Nothing) tryParser

        when maybeOut is
            Nothing -> Err Parser.genericError
            Just out -> Ok (rest, out)

many : Parser i o -> Parser i (List o)
many = \parser ->
    \input ->
        loop = \currInput, outputs ->
            when parser currInput is
                Ok (rest, output) -> loop rest (List.append outputs output)
                _ -> Ok (currInput, outputs)

        (rest1, first) <- Result.try (parser input)

        loop rest1 [first]

many0 : Parser i o -> Parser i (List o)
many0 = \parser ->
    \input ->
        loop = \currInput, outputs ->
            when parser currInput is
                Ok (rest, output) -> loop rest (List.append outputs output)
                _ -> Ok (currInput, outputs)

        loop input []

## returns a parser which takes a `List *` as input,
## and succeeds if the first token in the input is equal to the given `token`
##
## consumes the matched token
matches : o -> Parser (List o) o
    where o implements Eq
matches = \token ->
    \input ->
        when input is
            [tok, .. as rest] if tok == token -> Ok (rest, tok)
            _ -> Err Parser.genericError

expect Parser.run (matches 0) [0, 1, 2] == Ok 0
expect Parser.run (matches 1) [0] == Err Parser.genericError

surrounded : Parser i *, Parser i o, Parser i * -> Parser i o
surrounded = \left, token, right ->
    \input ->
        (rest1, _) <- Result.try (left input)
        (rest2, out) <- Result.try (token rest1)
        (rest3, _) <- Result.map (right rest2)

        (rest3, out)

## testing only
one = matches 1
zero = matches 0

expect
    parser = surrounded one zero one  
    Parser.run parser [1, 0, 1] == Ok 0

prefixed : Parser i *, Parser i o -> Parser i o
prefixed = \prefix, token ->
    \input ->
        (rest1, _) <- Result.try (prefix input)
        token rest1

suffixed : Parser i o, Parser i * -> Parser i o
suffixed = \token, suffix ->
    \input ->
        (rest1, out) <- Result.try (token input)
        (rest2, _) <- Result.map (suffix rest1)
        (rest2, out)

andThen : Parser i o1, Parser i o2 -> Parser i (o1, o2)
andThen = \parser1, parser2 -> \input ->
    (rest1, out1) <- Result.try (parser1 input)
    (rest2, out2) <- Result.map (parser2 rest1)

    (rest2, (out1, out2))
