{-
    Problem 11
    
    Modified run-length encoding.
    
    Modify the result of problem 10 in such a way that if an element has no
    duplicates it is simply copied into the result list. Only elements with
    duplicates are transferred as (N E) lists.
    
    Example in Haskell:
    
    P11> encodeModified "aaaabccaadeeee"
    [Multiple 4 'a',Single 'b',Multiple 2 'c',
     Multiple 2 'a',Single 'd',Multiple 4 'e']
-}

data Encoded_list a = Single a | Multiple Int a deriving (Show)

singlify :: [a] -> [ Encoded_list a ]
singlify [] = []
singlify (x:xs) = [ Single x ] ++ singlify xs

encode :: Eq a => [ Encoded_list a ] -> [ Encoded_list a ]
encode [] = []
encode [x] = [x]
encode (Single x : Single y : zs) = if x == y
                                    then encode $ [ Multiple 2 x ] ++ zs
                                    else [ Single x ] ++ ( encode $ [ Single y ] ++ zs )
encode (Multiple n x : Single y : zs ) = if x == y
                                         then encode $ [ Multiple (n+1) x ] ++ zs
                                         else [ Multiple n x ] ++ ( encode $ [ Single y ] ++ zs )

encodeModified :: Eq a => [a] -> [ Encoded_list a ]
encodeModified = encode . singlify
