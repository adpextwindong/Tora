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

--TODO
--
--t1 = ThreeNode (Leaf) 'E' (Leaf) 'R' (Leaf)
t1 = ThreeNode (Leaf) 'E' (Leaf) 'R' (TwoNode (Leaf) 'S' (Leaf))
