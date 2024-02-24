interface Util
    exposes [andEq]
    imports []

andEq : Result ok err, ok -> Bool where ok implements Eq
andEq = \res, expected ->
    when res is
        Ok ok -> ok == expected
        Err _ -> Bool.false
