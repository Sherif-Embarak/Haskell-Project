data Tree a = Empty | Node a (Tree a) (Tree a) deriving (Show)
--Depth First Search
traverseDF :: Tree a -> [a]
traverseDF Empty        = []
traverseDF (Node a l r) = a : (traverseDF l) ++ (traverseDF r)

--Breadth First Search
traverseBF :: Tree a -> [a]
traverseBF tree = tbf [tree]
    where
        tbf [] = []
        tbf xs = map nodeValue xs ++ tbf (concat (map leftAndRightNodes xs))
        nodeValue (Node a _ _) = a
        leftAndRightNodes (Node _ Empty Empty) = []
        leftAndRightNodes (Node _ Empty b)     = [b]
        leftAndRightNodes (Node _ a Empty)     = [a]
        leftAndRightNodes (Node _ a b)         = [a,b]


