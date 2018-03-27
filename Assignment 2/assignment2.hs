-- Que. 1)

replicate' :: [Int] -> [[Int]]
replicate' l = map (\n -> take n (repeat n)) l

-- Que. 2)

data Formula
    = Atom Bool -- atomic formula
    | And Formula Formula -- f /\ f
    | Or Formula Formula -- f \/ f
    | Not Formula -- not(f)
    
instance Show Formula where
    show (Atom a) = "Atom " ++ show a
    show (And a b) = "And (" ++ show a ++ ") (" ++ show b ++ ")"
    show (Or a b) = "Or (" ++ show a ++ ") (" ++ show b ++ ")"
    show (Not a) = "Not (" ++ show a ++ ")"

-- 2.1) A Haskell function collect_atoms f that computes all boolean primitives of a
-- propositional formula f

    collect_atoms (Atom x) = [Atom x]
    collect_atoms (And a b) = collect_atoms a ++ collect_atoms b
    collect_atoms (Or a b) = collect_atoms a ++ collect_atoms b
    collect_atoms (Not a) = collect_atoms a
    
-- 2.2) A Haskell function eval f to evaluate term f according to standard definitions of
-- propositional logic    
    eval (Atom a) = a
    eval (And a b) = eval a && eval b
    eval (Or a b) = eval a || eval b
    eval (Not a) = not (eval a)
    