module HC4 where

import FPPrac.Trees




-- -----------------------------------------------
-- regular expression


data FsaState = Q | R            -- start state: Q; success state R
	deriving Show

-- -----------------------------------------------





fsa0 :: FsaState -> String -> FsaState

fsa0 s "" = s

fsa0 Q ('a':xs) = fsa0 Q xs
fsa0 Q ('b':xs) = fsa0 R xs
fsa0 R ('a':xs) = fsa0 R xs
fsa0 R ('b':xs) = fsa0 Q xs


teststr = "aabaabaaabaa"




-- -----------------------------------------------


fsa1 :: FsaState -> Char -> FsaState


fsa1 Q 'a' = Q
fsa1 Q 'b' = R
fsa1 R 'a' = R
fsa1 R 'b' = Q


fsa1t = foldl fsa1 Q












isdigit x = elem x "0123456789"

fsa Q x | isdigit x   	= Q
	| x == '.'	= R

fsa R x | isdigit x	= R






-- -----------------------------------------------


-- context free grammars

{-
S -> a S A
S -> b A
A -> c
-}

data Cat = S | A
	deriving Show


data Tree = Leaf Char
       	  | Node1 Char Tree Tree
       	  | Node2 Char Tree
	  deriving Show





parse :: Cat -> String -> (Tree, String)


parse S (x:xs) | x=='a'	    = (Node1 x t1 t2 , r2)
               | x=='b'	    = (Node2 x t     , r )
               | otherwise  = error "parse error 1"
               where
                 (t1, r1) = parse S xs
                 (t2, r2) = parse A r1

                 (t , r ) = parse A xs


parse A (x:xs) | x=='c'	   = (Leaf x, xs)
               | otherwise = error "parse error 2"


pp (Leaf x)        = RoseNode "A" [ RoseNode [x] [] ]
pp (Node1 x t1 t2) = RoseNode "S" [ RoseNode [x] [] , pp t1 , pp t2 ]
pp (Node2 x t)     = RoseNode "S" [ RoseNode [x] [] , pp t ]


