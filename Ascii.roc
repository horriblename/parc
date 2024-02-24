interface Ascii
    exposes [StrBuf, char, isDigit, int, ]
    imports [Parser.{Parser}]

StrBuf : List U8

char : U8 -> Parser StrBuf U8
char = \c -> \input ->
    when input is
        [head, .. as rest] if head == c -> Ok (rest, head)
        _ -> Err Parser.genericError

expect Parser.run (char 'a') ['a'] == Ok 'a'
expect Parser.run (char 'a') ['b'] == Err Parser.genericError

isDigit : U8 -> Bool
isDigit = \c -> c >= '0' && c <= '9'

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
        {before: num, others} = List.split input i
        Ok (others, num)

## returns the first index where the predicate returns true
firstIndex = \list, pred ->
    List.walkUntil list 0 \i, item ->
        if pred item then
            Break i
        else
            Continue (i+1)
