data Tree a = Nil | Node (Tree a) a (Tree a) deriving Show

-- Checking Empty function
empty Nil = True
empty  _  = False

-- Insert an  element to  the Tree
insert Nil x = Node Nil x Nil
insert (Node t1 v t2) x 
    | v == x = Node t1 v t2
    | v  < x = Node t1 v (insert t2 x)
    | v  > x = Node (insert t1 x) v t2

--Contain : if element is present return true--Contain : if element is present return true
contains Nil _ = False
contains (Node t1 v t2) x 
    | x == v = True
    | x  < v = contains t1 x 
    | x  > v = contains t2 x

-- Creation of Tree from list of number 
ctree [] = Nil    
ctree (h:t) = ctree2 (Node Nil h Nil) t
    where
        ctree2 tr [] = tr
        ctree2 tr (h:t) = ctree2 (insert tr h) t