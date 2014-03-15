{-
    Problem 3
    
    Find the K'th element of a list. The first element in the list is number 1.
    
    Example in Haskell:
    
    Prelude> elementAt [1,2,3] 2
    2
    Prelude> elementAt "haskell" 5
    'e'
-}

elementAt :: (Num b, Ord b, Eq b) => [a] -> b -> a
elementAt [] _ = error "list not long enough"
elementAt x c
            | c <= 0 = error "invalid index. index starts with 1"
elementAt (x:xs) 1 = x
elementAt (x:xs) c = elementAt xs (c-1)
