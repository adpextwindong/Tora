todo = undefined


--TODO add a to leaf constructor
data TwoThreeFour k a = Leaf
                      | TwoNode (TwoThreeFour k a) k (TwoThreeFour k a)
                      | ThreeNode (TwoThreeFour k a) k (TwoThreeFour k a) k (TwoThreeFour k a)
                      | FourNode (TwoThreeFour k a) k (TwoThreeFour k a) k (TwoThreeFour k a) k (TwoThreeFour k a)
    deriving Show

ttfMember :: Ord k => k -> TwoThreeFour k a -> Bool
ttfMember key (Leaf) = False
ttfMember key (TwoNode l k r) | key == k = True
                              | key < k = ttfMember key l
                              | otherwise = ttfMember key r
ttfMember key (ThreeNode l kIntervalL m kIntervalR r) | key == kIntervalL || key == kIntervalR = True
                                                      | key < kIntervalL = ttfMember key l
                                                      | key < kIntervalR = ttfMember key m
                                                      | otherwise        = ttfMember key r

--TODO emitting a graphviz as we trace would be cool

--t1 = ThreeNode (Leaf) 'E' (Leaf) 'R' (Leaf)
t1 = ThreeNode (Leaf) 'E' (Leaf) 'R' (TwoNode (Leaf) 'S' (Leaf))

t2 :: TwoThreeFour Char ()
t2 = FourNode (Leaf) 'A' (Leaf) 'B' (Leaf) 'C' (Leaf)

t3 :: TwoThreeFour Char ()
t3 = ttfInsert 'D' t2

--What about traversal down the tree?
--NOTE: this is wrong. it requires way more case analysis for Leafs.... https://github.com/SmythConor/2-3-4_trees/blob/master/234tree.hs
--Skipping this exercise for now jeeze.
--
ttfInsert :: Ord k => k -> TwoThreeFour k a -> TwoThreeFour k a
ttfInsert k t@(FourNode _ _ _ _ _ _ _) = ttfInsert k $ ttfSplit t
ttfInsert k (ThreeNode l lk m rk r) | k < lk    = ttfSplit $ FourNode (Leaf) k
                                                                       l lk
                                                                       m rk
                                                                       r
                                    | k < rk    = ttfSplit $ FourNode l lk
                                                                      (Leaf) k
                                                                      m rk
                                                                      r

                                    | otherwise = ttfSplit $ FourNode l lk
                                                                      m rk
                                                                      r k
                                                                      (Leaf)

ttfInsert k (TwoNode l k' r) | k < k'    = ThreeNode (Leaf) k l k' r
                             | otherwise = ThreeNode l k' r k (Leaf)

ttfSplit :: Ord k => TwoThreeFour k a -> TwoThreeFour k a
ttfSplit (FourNode l lk l' mk r' rk r) = let leftSub = TwoNode l lk l'
                                             rightSub = TwoNode r' rk r
                                             in
                                              TwoNode leftSub mk rightSub
ttfSplit t = t
