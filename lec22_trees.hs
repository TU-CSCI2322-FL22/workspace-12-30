import Debug.Trace

data Velocity = MPS Double | FPS Double 

almostEq :: Double -> Double -> Bool
almostEq x y = (x - y) < 0.0001

instance Eq Velocity where
  (==) (MPS x) (MPS y) = (x `almostEq` y)
  (==) (FPS x) (FPS y) = (x `almostEq` y)
  (==) (MPS x) (FPS y) = (x `almostEq` (y / 3.28084))
  (==) (FPS x) (MPS y) = ((x/ 3.28084) `almostEq` y)

instance Show Velocity where
  show (FPS x) = (show x) ++ " ft/s"
  show (MPS x) = (show x) ++ " m/s"

data BST a = Leaf | Node (BST a) a (BST a) deriving Show

instance Eq a => Eq (BST a) where
  (==) treeA treeB = toList treeA == toList treeB

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
element x Leaf = False
element x (Node lft root rgt) = 
  case compare x root of
    EQ -> True
    LT -> x `element` lft
    GT -> x `element` rgt
--element x (Tree lft root rgt) = (x == root) || (x `element` lft) || (x `element` rgt)

toList :: BST a -> [a]
toList Leaf = []
toList (Node left root right) = (toList left) ++ (root:(toList right))

toTree :: Ord a => [a] -> BST a
{-toTree [] = Leaf
toTree (x:xs) = x `insert` (toTree xs)-}
toTree lst = foldr insert Leaf lst
--toTree lst = foldl (\acc x -> insert x acc) Leaf lst

treeB, treeC :: BST Int
treeB = toTree [1..10]
treeC = toTree [10,9..1]

logicalEQ :: Eq a => BST a -> BST a -> Bool
logicalEQ treeA treeB = (toList treeA) == (toList treeB)
