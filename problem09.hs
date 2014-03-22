{-
    Problem 09
    
    Pack consecutive duplicates of list elements into sublists.
    If a list contains repeated elements they should be placed in separate sublists.
    
    Example in Haskell:
    
    *Main> pack ['a', 'a', 'a', 'a', 'b', 'c', 'c', 'a', 'a', 'd', 'e', 'e', 'e', 'e']
    ["aaaa","b","cc","aa","d","eeee"]
-}

listify :: [a] -> [[a]]
listify [] = []
listify (x:xs) = [[x]] ++ listify xs

merge :: Eq a => [[a]] -> [[a]]
merge [] = []
merge [a] = [a]
merge (x:y:zs) = if head x == head y
                 then merge ( [x ++ y] ++ zs )
                 else [x] ++ merge ( [y] ++ zs )

pack :: Eq a => [a] -> [[a]]
pack = merge . listify
