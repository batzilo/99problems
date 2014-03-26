{-
    Problem 10
    
    Run-length encoding of a list.
    Use the result of problem P09 to implement the so-called run-length
    encoding data compression method. Consecutive duplicates of elements
    are encoded as lists (N E) where N is the number of duplicates of the
    element E.
    
    Example in Haskell:
    
    encode "aaaabccaadeeee"
    [(4,'a'),(1,'b'),(2,'c'),(2,'a'),(1,'d'),(4,'e')]
-}

tuplify :: Num b => [a] -> [(b, a)]
tuplify [] = []
tuplify (x:xs) = [(1,x)] ++ tuplify xs

merge_encode :: (Eq a, Num b) => [(b,a)] -> [(b,a)]
merge_encode [] = []
merge_encode [a] = [a]
merge_encode (x:y:zs) = if snd x == snd y
                        then merge_encode ( [new_x] ++ zs )
                        else [x] ++ merge_encode ( [y] ++ zs )
                        where new_x = (fst x + 1, snd x)

encode :: (Eq a, Num b) => [a] -> [(b,a)]
encode = merge_encode . tuplify
