{-
    Problem 2
    
    Find the last but one element of a list.
    
    Example in Haskell:
    
    Prelude> myButLast [1,2,3,4]
    3
    Prelude> myButLast ['a'..'z']
    'y'
-}

myButLast :: [a] -> a
myButLast [] = error "empty list has no last but one element"
myButLast (x:y:[]) = x
myButLast (x:y:zs) = myButLast (y:zs)
