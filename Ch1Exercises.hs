type Key = String

data BST a = Leaf | Tree (BST a) Key a (BST a)
  deriving Show

empty = Leaf

insert :: (Ord a) => Key -> a -> BST a -> BST a
insert key x Leaf = Tree Leaf key x Leaf
insert key x (Tree l k v r) | key < k = Tree (insert key x l) k v r--Go left
                            | key > k = Tree l k v (insert key x r)
                            | otherwise = Tree l key x r

tx0 = insert "c" 420 $ insert "b" 2 $ insert "a" 1 $ empty
tx1 = insert "a" 5 $ insert "b" 2020 $ insert "c" 32 $ empty
{-
tx1 = insert 4 $ insert 5 $ insert 1 $ insert 2 $ insert 3 empty

tx2 = insert 8 $ insert 7 $ insert 6 $ insert 5 $ insert 4 $ insert 3 $ insert 2 $ insert 1 $ empty
tx3 = insert 1 $ insert 2 $ insert 3 $ insert 4 $ insert 5 $ insert 6 $ insert 7 $ insert 8 $ empty
-}

--Currently unbalanced

member :: Ord a => Key -> BST a -> Bool
member x (Leaf) = False
member x (Tree l k _ r) | x == k = True
                        | x < k = member x l --Check left
                        | otherwise = member x r --Check right

bstLookup :: Ord a => Key -> BST a -> Maybe a
bstLookup key (Leaf) = Nothing
bstLookup key (Tree l k v r) | key == k = Just v
                             | key < k = bstLookup key l
                             | otherwise = bstLookup key r


-- Behavior of
-- (a) t s p i p f b s t
--     t
--    s
--   p
--  i
-- f
--b
--

ex1 = zip ["t","s","p","i","p","f","b","s","t"] [0..]
ex1_t = foldr (uncurry insert) empty ex1

-- (b) a b c d e f g h i
--
--        i
--       h
--      f
--     e
--    d
--   c
--  b
-- a
