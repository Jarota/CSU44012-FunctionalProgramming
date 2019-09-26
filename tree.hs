data Tree a = Leaf a
    | Node (Tree a) (Tree a)
    deriving Show

count :: Tree a -> Integer
count (Leaf _) = 1
count (Node left right) = 1 + (count left) + (count right)

depth :: Tree a -> Integer
depth (Leaf _) = 1
depth (Node left right)
    | x >= y = 1 + x
    | x < y = 1 + y
    where x = depth left
          y = depth right

flatten :: Tree a -> [a]
flatten (Leaf a) = [a]
flatten (Node left right) = (flatten left) ++ (flatten right)

numberTree :: Tree a -> Tree Integer
numberTree a = numT a 0

numT :: Tree a -> Integer -> Tree Integer
numT (Leaf a) n = Leaf n
numT (Node left right) n = Node (numT left n) (numT right (n + (toInteger $ length $ flatten left)))
