interface Ascii
    exposes [StrBuf, char, isDigit, isAlpha, int, charIs, tag, isWhitespace, until, until0]
    imports [Parser.{ Parser }]

StrBuf : List U8

char : U8 -> Parser StrBuf U8
char = \c -> \input ->
        when input is
            [head, .. as rest] if head == c -> Ok (rest, head)
            _ -> Err Parser.genericError

expect Parser.run (char 'a') ['a'] == Ok 'a'
expect Parser.run (char 'a') ['b'] == Err Parser.genericError

charIs : (U8 -> Bool) -> Parser StrBuf U8
charIs = \pred -> \input ->
        when input is
            [head, .. as rest] if pred head -> Ok (rest, head)
            _ -> Err Parser.genericError

## Matches until before the first element that *fulfill* the predicate
## Note that this parser cannot fail.
until0 : (i -> Bool) -> Parser (List i) (List i)
until0 = \pred -> \input ->
        count =
            input
            |> List.walkUntil 0 \i, c ->
                if !(pred c) then
                    Continue (i + 1)
                else
                    Break i

        { before, others } = List.split input count
        Ok (others, before)

expect Parser.run (until0 isDigit) ['a', 'b', '3'] == Ok ['a', 'b']
expect Parser.run (until0 isDigit) ['3', 'b'] == Ok []

until = \pred -> \input ->
        count =
            input
            |> List.walkUntil 0 \i, c ->
                if !(pred c) then
                    Continue (i + 1)
                else
                    Break i

        when input is
            [first, ..] if !(pred first) ->
                { before, others } = List.split input count
                Ok (others, before)

            _ -> Err Parser.genericError

expect Parser.run (until isDigit) ['a', 'b', '3'] == Ok ['a', 'b']
expect Parser.run (until isDigit) ['3', 'b'] |> Result.isErr

isDigit : U8 -> Bool
isDigit = \c -> c >= '0' && c <= '9'

isAlpha : U8 -> Bool
isAlpha = \c -> (c >= 'a' && c <= 'z') || (c >= 'A' && c <= 'Z')

i32 : Parser StrBuf I32
i32 = \input ->
    (rest, numStrBuf) <- int input |> Result.try

    numStr <- Str.fromUtf8 numStrBuf
        |> Result.mapErr \_ -> Parser.genericError
        |> Result.try

    when Str.toI32 numStr is
        Ok num -> Ok (rest, num)
        Err _ -> Err Parser.genericError

expect Parser.run i32 (Str.toUtf8 "12abc") == Ok 12
expect Parser.run i32 (Str.toUtf8 "acb") == Err Parser.genericError

int : Parser StrBuf StrBuf
int = \input ->
    i = firstIndex input \c -> !(isDigit c)

    if i == 0 then
        Err Parser.genericError
    else
        { before: num, others } = List.split input i
        Ok (others, num)

## returns the first index where the predicate returns true
firstIndex = \list, pred ->
    List.walkUntil list 0 \i, item ->
        if pred item then
            Break i
        else
            Continue (i + 1)

tag : Str -> Parser StrBuf StrBuf
tag = \str ->
    strBuf = Str.toUtf8 str
    \input ->
        result = List.walkWithIndexUntil strBuf Matches \_, c, i ->
            when List.get input i is
                Ok cInput if cInput == c -> Continue Matches
                _ -> Break NoMatch

        when result is
            Matches ->
                { before: token, others } = List.split input (List.len strBuf)
                Ok (others, token)

            NoMatch -> Err Parser.genericError

expect Parser.run (tag "hello") (Str.toUtf8 "hello world") == Ok (Str.toUtf8 "hello")
expect Parser.run (tag "hello") (Str.toUtf8 "hell world") |> Result.isErr

isWhitespace = \c ->
    when c is
        0x0020 | 0x000A | 0x000D | 0x0009 -> Bool.true
        _ -> Bool.false
