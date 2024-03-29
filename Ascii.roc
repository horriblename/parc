interface Ascii
    exposes [StrBuf, char, isDigit, isAlpha, int, charIs, tag, isWhitespace]
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

skipWhitespaces = \input ->
    count =
        input
        |> List.walkUntil 0 \i, c ->
            if isWhitespace c then
                Continue (i + 1)
            else
                Break i

    rest = List.dropFirst input count
    Ok (rest, {})

expect skipWhitespaces (Str.toUtf8 "  \t\nhi") == Ok (['h', 'i'], {})
