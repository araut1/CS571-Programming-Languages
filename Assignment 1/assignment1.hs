-- 5. Haskell function that computes all sublists of a list that have an odd size
osublists [] = [[]]
osublists (x:xs) = filter (odd . length ) [x:osublist | osublist <- osublists xs] ++ osublists xs 

-- 6. Haskell function that keeps every kth element of a list
keepl k lt = case drop (k-1) lt of
              (y:ys) -> y : keepl k ys
              [] -> []