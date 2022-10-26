import Debug.Trace

data Velocity = MPS Double | FPS Double deriving (Show, Eq)

data BST a = Leaf | Node (BST a) a (BST a) deriving (Eq, Show)

singleton x = Node Leaf x Leaf

aTree = Node (singleton 5) 7 (Node (singleton 10) 15 (singleton 16))

insert :: Ord a => a -> BST a -> BST a
insert y Leaf = Node Leaf y Leaf
insert y tree@(Node left root right) = 
    case compare y root of
        EQ -> tree
        LT -> let newLeft = insert y left
              in Node newLeft root right
        GT -> let newRight = insert y right
              in Node left root newRight

element :: Ord a => a -> BST a -> Bool
element = undefined

toList :: BST a -> [a]
toList Leaf = []
toList (Node left root right) = (toList left) ++ (root:(toList right))

toTree :: (Show a, Ord a) => [a] -> BST a
toTree [] = Leaf
toTree (x:xs) = x `insert` (toTree xs)

treeB = toTree [1..10]
treeC = toTree [10,9..1]

logicalEQ :: BST Int -> BST Int -> Bool
logicalEQ treeA treeB = undefined
