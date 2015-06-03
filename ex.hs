import FPPrac.Trees
import Data.Char
import Data.List
import Data.Typeable
import qualified Data.Text

-- 1.1

data Tree1a = Leaf1a Int
            | Node1a Int Tree1a Tree1a

pp1a (Leaf1a n) = RoseNode (show n) []
pp1a (Node1a n t1 t2) = RoseNode (show n) [pp1a t1, pp1a t2]

-- showRoseTree (pp1a (Node1a 1 (Node1a 2 (Leaf1a 3) (Leaf1a 4)) (Leaf1a 5)))

data Tree1b = Leaf1b (Int, Int)
            | Node1b (Int, Int) Tree1b Tree1b

pp1b (Leaf1b n) = RoseNode (show n) []
pp1b (Node1b n t1 t2) = RoseNode (show n) [pp1b t1, pp1b t2]

-- showRoseTree (pp1b (Node1b (1,2) (Node1b (2,3) (Leaf1b (3,4)) (Leaf1b (4,5))) (Leaf1b (5,6))))

data Tree1c = Leaf1c
            | Node1c Int Tree1c Tree1c
            
pp1c (Leaf1c) = RoseNode "" []
pp1c (Node1c n t1 t2) = RoseNode (show n) [pp1c t1, pp1c t2]

-- showRoseTree (pp1c (Node1c 1 (Node1c 2 (Leaf1c) (Leaf1c)) (Node1c 3 (Node1c 4 Leaf1c Leaf1c) Leaf1c)))

data Tree1d = Leaf1d (Int, Int)
            | Node1d [Tree1d]
            
pp1d (Leaf1d n) = RoseNode (show n) []
pp1d (Node1d xs) = RoseNode "" (map pp1d xs)

-- showRoseTree (pp1d (Node1d [(Node1d [(Leaf1d (1,2)), (Leaf1d (2,3))]), (Leaf1d (3,4))]))

-- 1.2

treeAdd x (Leaf1a n) = Leaf1a (n+x)
treeAdd x (Node1a n t1 t2) = Node1a (n+x) (treeAdd x t1) (treeAdd x t2)

-- showRoseTree (pp1a (treeAdd 5 (Node1a 1 (Node1a 2 (Leaf1a 3) (Leaf1a 3)) (Leaf1a 2))))

treeSquare (Leaf1a n) = Leaf1a (n^2)
treeSquare (Node1a n t1 t2) = Node1a (n^2) (treeSquare t1) (treeSquare t2)

-- showRoseTree (pp1a (TreeSquare (Node1a 1 (Node1a 2 (Leaf1a 3) (Leaf1a 3)) (Leaf1a 2))))

mapTree f (Leaf1a n) = Leaf1a (f n)
mapTree f (Node1a n t1 t2) = Node1a (f n) (mapTree f t1) (mapTree f t2)

mapTreeAdd x tree = mapTree (+x) tree

-- showRoseTree (pp1a (mapTreeAdd 5 (Node1a 1 (Node1a 2 (Leaf1a 3) (Leaf1a 3)) (Leaf1a 2))))

mapTreeSquare tree = mapTree (^2) tree

-- showRoseTree (pp1a (mapTreeSquare (Node1a 1 (Node1a 2 (Leaf1a 3) (Leaf1a 3)) (Leaf1a 2))))

addNode (Leaf1b (a,b)) = Leaf1a (a+b)
addNode (Node1b (a,b) t1 t2) = Node1a (a+b) (addNode t1) (addNode t2)

-- showRoseTree (pp1a (addNode (Node1b (1,2) (Node1b (2,3) (Leaf1b (3,4)) (Leaf1b (4,5))) (Leaf1b (5,6)))))

mapTreeB f (Leaf1b (a,b)) = (Leaf1a (a `f` b))
mapTreeB f (Node1b (a,b) t1 t2) = Node1a (a `f` b) (mapTreeB f t1) (mapTreeB f t2)

-- showRoseTree (pp1a (mapTreeB (*) (Node1b (1,2) (Node1b (2,3) (Leaf1b (3,4)) (Leaf1b (4,5))) (Leaf1b (5,6)))))

-- 1.3

binMirror (Leaf1a n) = Leaf1a n
binMirror (Node1a n t1 t2) = Node1a n (binMirror t2) (binMirror t1)

-- showRoseTreeList [(pp1a (Node1a 1 (Node1a 2 (Leaf1a 3) (Leaf1a 4)) (Node1a 5 (Leaf1a 6) (Leaf1a 7)))), (pp1a (binMirror (Node1a 1 (Node1a 2 (Leaf1a 3) (Leaf1a 4)) (Node1a 5 (Leaf1a 6) (Leaf1a 7)))))]

binMirrorD (Leaf1d (a,b)) = Leaf1d (b,a)
binMirrorD (Node1d xs) = Node1d (map binMirrorD (reverse xs))

-- showRoseTreeList [(pp1d (Node1d [(Node1d [(Leaf1d (1,2)), (Leaf1d (2,3))]), (Leaf1d (3,4))])), (pp1d (binMirrorD (Node1d [(Node1d [(Leaf1d (1,2)), (Leaf1d (2,3))]), (Leaf1d (3,4))])))]

-- 1.4

insertTree x (Leaf1c) = Node1c x Leaf1c Leaf1c
insertTree x (Node1c n t1 t2) | n >= x = Node1c n (insertTree x t1) t2
							  | otherwise = Node1c n t1 (insertTree x t2)

-- showRoseTreeList [(pp1c (Node1c 5 (Node1c 4 Leaf1c Leaf1c) (Node1c 8 (Node1c 7 Leaf1c Leaf1c) (Leaf1c)))), (pp1c (insertTree 6 (Node1c 5 (Node1c 4 Leaf1c Leaf1c) (Node1c 8 (Node1c 7 Leaf1c Leaf1c) (Leaf1c)))))]

makeTree2 [] tree = tree
makeTree2 (x:xs) tree = makeTree2 xs (insertTree x tree)
makeTree xs = makeTree2 xs Leaf1c

-- showRoseTree (pp1c (makeTree [5,2,4,6,8,4,2,5,7,8,1]))

makeList Leaf1c = []
makeList (Node1c n t1 t2) = (makeList t1) ++ (n: (makeList t2))

-- makeList (makeTree [4,5,3,4,7,2,5,7])

sortList xs = makeList (makeTree xs)

-- sortList [4,5,3,4,7,2,5,7]

sortTree tree = makeTree (makeList tree)

-- showRoseTree (pp1c (sortTree (Node1c 1 (Node1c 2 Leaf1c Leaf1c) Leaf1c)))

--1.5

subTreeAt x Leaf1c = error "Nope. No n in there!"
subTreeAt x (Node1c n t1 t2) | n == x = Node1c n t1 t2
							 | n >= x = subTreeAt x t1
							 | otherwise = subTreeAt x t2  

-- showRoseTreeList [(pp1c (makeTree [5,1,2,3,4,6,8,7,9,10])), (pp1c (subTreeAt 6 (makeTree [5,1,2,3,4,6,8,7,9,10])))]

--1.6

cutOffAt x Leaf1c = Leaf1c
cutOffAt x (Node1c n t1 t2) | x == 0 = Leaf1c
							| otherwise = Node1c n (cutOffAt (x-1) t1) (cutOffAt (x-1) t2)

-- showRoseTreeList [(pp1c (makeTree [5,1,2,3,4,6,8,7,9,10])), (pp1c (cutOffAt 2 (makeTree [5,1,2,3,4,6,8,7,9,10])))]

--1.7

replace x [] (Leaf1a n) = Leaf1a x
replace x [] (Node1a n t1 t2) = Node1a x t1 t2
replace x (y:ys) (Node1a n t1 t2) | y == 'l' = Node1a n (replace x ys t1) t2
								  | otherwise = Node1a n t1 (replace x ys t2)

-- showRoseTreeList [(pp1a (Node1a 1 (Node1a 2 (Leaf1a 3) (Leaf1a 4)) (Node1a 5 (Leaf1a 6) (Leaf1a 7)))), (pp1a (replace 100 "lr" (Node1a 1 (Node1a 2 (Leaf1a 3) (Leaf1a 4)) (Node1a 5 (Leaf1a 6) (Leaf1a 7)))))]

subTree [] tree = tree
subTree ys (Leaf1a a) = error "Nope!"
subTree (y:ys) (Node1a n t1 t2) | y == 'l' = subTree ys t1
							    | otherwise = subTree ys t2

-- showRoseTreeList [(pp1a (Node1a 1 (Node1a 2 (Leaf1a 3) (Leaf1a 4)) (Node1a 5 (Leaf1a 6) (Leaf1a 7)))), (pp1a (subTree "l" (Node1a 1 (Node1a 2 (Leaf1a 3) (Leaf1a 4)) (Node1a 5 (Leaf1a 6) (Leaf1a 7)))))]

--1.8

getLength Leaf1c = 1
getLength (Node1c n t1 t2) = 1 + maximum [getLength t1, getLength t2]

isBalanced Leaf1c = True
isBalanced (Node1c n t1 t2) | (getLength t1) - (getLength t2) <= 1 && (getLength t1) - (getLength t2) >= -1 = isBalanced t1 && isBalanced t2
							| otherwise = False

-- isBalanced (makeTree [5,3,2,4,7,6])

balance :: Tree1c -> Tree1c
balance tree = balanceListToTree (sortList (makeList tree))

balanceListToTree :: [Int] -> Tree1c
balanceListToTree [] = Leaf1c 
balanceListToTree xs = Node1c (xs !! half) (balanceListToTree $ take half xs) (balanceListToTree $ drop (half+1) xs)
			   where
			   	half = length xs `quot` 2

-- showRoseTreeList [pp1c $ makeTree [1..25], pp1c $ balance $ makeTree [1..25]]
-- isBalanced $ balance $ makeTree [1..100]





-- 4.1

data BinTree a b = BinLeaf b
				 | BinNode a (BinTree a b) (BinTree a b)
				 deriving Show

type Unit = ()

binA :: BinTree Int Int
binA = BinNode 5 (BinNode 4 (BinLeaf 3) (BinLeaf 2)) (BinLeaf 1)

binB :: BinTree (Int, Int) (Int, Int)
binB = BinNode (1,2) (BinNode (2,3) (BinLeaf (3,4)) (BinLeaf (4,5))) (BinLeaf (5,6))

binC :: BinTree Int Unit
binC = BinNode 5 (BinNode 4 (BinLeaf ()) (BinLeaf ())) (BinLeaf ())

binConvert :: (Show a, Show b) => BinTree a b -> RoseTree
binConvert (BinLeaf x) = RoseNode (show x) []
binConvert (BinNode x bt1 bt2) =  RoseNode (show x) [binConvert bt1, binConvert bt2]

-- 4.2

isInt x = ord x >= 48 && ord x <= 57

isOperator :: Char -> Bool
isOperator x = elem x "+-*/^=><"

rep :: Char -> String -> BinTree Char Char -> BinTree Char Char
rep x [] (BinLeaf n) = BinLeaf x
rep x [] (BinNode n b1 b2) = BinNode x b1 b2

rep x (y:ys) (BinLeaf n) | y == 'l'  = BinNode n (rep x ys bl) bl
						 | otherwise = BinNode n bl (rep x ys bl)
						 where
						 	bl = BinLeaf ' '

rep x (y:ys) (BinNode n b1 b2) | y == 'l'  = BinNode n (rep x ys b1) b2
							   | otherwise = BinNode n b1 (rep x ys b2)

parse1 :: String -> BinTree Char Char -> String -> BinTree Char Char
parse1 [] tree place = tree
parse1 (x:xs) tree place | x == '(' 		= parse1 xs tree (place ++ ['l']) 
					     | isInt x 			= parse1 xs (rep x place tree) (init place)
					     | isOperator x 	= parse1 xs (rep x place tree) (place ++ ['r'])
					     | x == ')' 		= parse1 xs tree (init place)
						 | otherwise 		= error "Parsing error"
							

-- showRoseTree (binConvert (parse1 "(3+(3*5))" (BinLeaf '0') ""))

parse2 :: String -> BinTree Char Char -> String -> BinTree Char Char
parse2 [] tree place = tree
parse2 (x:xs) tree place | x == '(' 				= parse2 xs tree (place ++ ['l']) 
					     | isInt x || isMyLetter x 	= parse2 xs (rep x place tree) (init place)
					     | isOperator x 			= parse2 xs (rep x place tree) (place ++ ['r'])
					     | x == ')' 				= parse2 xs tree (init place)
						 | otherwise 				= error "Parsing error"

-- 

data Token = S | N | I | D | NEG
	deriving Show

isMyLetter x = (ord x >= 65 && ord x <= 90) || (ord x >= 97 && ord x <= 122)

tokenize _ "" = True

tokenize S (x:xs) | isInt x || x == '-' = tokenize NEG xs
				  | isMyLetter x 	= tokenize I xs
				  | otherwise		= False

tokenize N (x:xs) | isInt x 		= tokenize N xs
				  | x == '.'		= tokenize D xs
				  | otherwise 		= False

tokenize D (x:xs) | isInt x 		= tokenize D xs
				  | otherwise 		= False

tokenize I (x:xs) | isInt x || isMyLetter x = tokenize I xs
				  | otherwise 		= False

tokenize NEG (x:xs) | isInt x 		= tokenize N xs
					| otherwise 	= False

isToken x = tokenize S x

parse3 :: [String] -> BinTree String String -> String -> BinTree String String
parse3 [] tree place = tree
parse3 (x:xs) tree place | x == "(" 	= parse3 xs tree (place ++ ['l']) 
					     | isToken x 	= parse3 xs (rep2 x place tree) (init place)
					     | isOperator2 x= parse3 xs (rep2 x place tree) (place ++ ['r'])
					     | x == ")" 	= parse3 xs tree (init place)
						 | otherwise 	= error "Parsing error"

getWords [] = []
getWords (x:xs) | isToken x = x: getWords xs
				| otherwise = (split x) ++ getWords xs 

split [] = []
split (x:xs) = [x]: split xs

isOperator2 (x:xs) = elem x "+-*/^=><"

rep2 xs [] (BinLeaf n) = BinLeaf xs
rep2 xs [] (BinNode n b1 b2) = BinNode xs b1 b2

rep2 xs (y:ys) (BinLeaf n) | y == 'l'  = BinNode n (rep2 xs ys bl) bl
						   | otherwise = BinNode n bl (rep2 xs ys bl)
						   where
						 		bl = BinLeaf ""

rep2 xs (y:ys) (BinNode n b1 b2) | y == 'l'  = BinNode n (rep2 xs ys b1) b2
							   	 | otherwise = BinNode n b1 (rep2 xs ys b2)

myWords [] = ""
myWords (x:xs) | isOperator x && head xs == '-'= ' ': x: ' ': (head xs): myWords (tail xs) 
			   | isOperator x || x == ')' || x == '(' = ' ': x: ' ': myWords xs
			   | otherwise = x: myWords xs

parse3Help xs = parse3 (getWords $ words $ myWords xs) (BinLeaf "0") "" 

-- showRoseTree (binConvert (parse3Help "(123+(a1s23/a15))" ))

-- v4

-- showRoseTree (binConvert (parse3Help "(12.3+(a1sas23/(a15+-1)))" ))

op o a b | o == '+' = show(getFloat a + getFloat b)
		 | o == '-' = show(getFloat(a) - getFloat(b))
		 | o == '*' = show(getFloat(a) * getFloat(b))
		 | o == '/' = show(getFloat(a) / getFloat(b))
		 | o == '^' = show(getFloat(a) ** getFloat(b))
		 | o == '=' = show(getFloat(a) == getFloat(b)) 
		 | o == '<' = show(getFloat(a) < getFloat(b))
		 | o == '>' = show(getFloat(a) > getFloat(b))
		 | otherwise = error "Unknown operator!"

calculateTree (BinLeaf n) = n
calculateTree (BinNode (n:ns) b1 b2) | isOperator n = op n (calculateTree b1) (calculateTree b2)
									 | otherwise = error "Uncomputable"

getFloat n = read n :: Float

assign n [] = n
assign n ((a,b):xs) = Data.Text.unpack (Data.Text.replace (Data.Text.pack a) (Data.Text.pack b) (Data.Text.pack n))

eval xs [] = calculateTree (parse3Help xs)
eval xs ys = calculateTree (parse3Help (assign xs ys))

-- eval "(1+(a*4))" [("a","500")]
-- eval "(1=1)" []
-- eval "(1>1)" []
-- eval "(1<1)" []

-- Practical 5

showT 	= showRBTree
showTL 	= showRBTreeList 

createTree = RBNode NodeBlack "4" [RBNode NodeBlack "" [], RBNode NodeBlack "12" []]

rbt = RBNode NodeBlack "9" 
		[RBNode NodeRed "5" 
			[RBNode NodeBlack "4"
				[RBNode NodeRed "3" 
					[RBNode NodeBlack "" []
					,RBNode NodeBlack "" []]
				,RBNode NodeBlack "" []
				]
			,RBNode NodeBlack "7"
				[RBNode NodeRed "6"
					[RBNode NodeBlack "" []
					,RBNode NodeBlack "" []
					]
				,RBNode NodeBlack "" []
				]
			] 
		,RBNode NodeBlack "10" 
			[RBNode NodeBlack "" []
			,RBNode NodeBlack "" []
			]
		]

addToTree :: String -> RBTree -> RBTree
addToTree ins (RBNode c s []) 	 = RBNode NodeRed ins [RBNode NodeBlack "" [], RBNode NodeBlack "" []]
addToTree ins (RBNode c s [x,y]) | (read ins :: Int) <= (read s :: Int) 	= RBNode c s [(addToTree ins x), y]
								 | otherwise = RBNode c s [x, (addToTree ins y)]

checkRedViolation :: NodeColor -> RBTree -> String -> String
checkRedViolation col (RBNode c i []) path		= ""
checkRedViolation col (RBNode c i [x,y]) path	| c == NodeRed && col == NodeRed = path
												| checkRedViolation c x (path ++ "l") == "" = checkRedViolation c y (path ++ "r")
												| otherwise = checkRedViolation c x (path ++ "l")  

insertToRB :: String -> RBTree -> RBTree
insertToRB ins tree | (length path) == 0	= setColor t NodeBlack
					| otherwise 			= afterToRB t
					where 
						t = addToTree ins tree
						path = checkRedViolation NodeBlack t ""

afterToRB tree | (length path) == 0 	= setColor tree NodeBlack
			   | otherwise		 		= afterToRB t 
			   where
			   		t 	= fixRedIssue tree path
			   		path = checkRedViolation NodeBlack tree ""

fixRedIssue :: RBTree -> String -> RBTree
fixRedIssue (RBNode c i [x,y]) []		= RBNode c i [x,y]
fixRedIssue (RBNode c i [x,y]) [p1,p2]  | (getColor x == NodeRed && getColor (getChild x 0) == NodeRed && getColor y == NodeRed) || (getColor y == NodeRed && getColor (getChild y 0) == NodeRed && getColor x == NodeRed) = RBNode NodeRed i [changeColor x, changeColor y]
										| otherwise = rotate (RBNode c i [x,y]) [p1,p2] 
fixRedIssue (RBNode c i [x,y]) (p:ps) | p == 'l' = RBNode c i [fixRedIssue x ps, y]
									  | otherwise = RBNode c i [x, fixRedIssue y ps]

changeColor :: RBTree -> RBTree
changeColor (RBNode c s a) | c == NodeBlack = RBNode NodeRed s a
							   | otherwise = RBNode NodeBlack s a

setColor (RBNode c s a) col = RBNode col s a

rotate :: RBTree -> String -> RBTree
rotate node path | path == "ll" = rotate1 node
			     | path == "rl" = rotate2 node
				 | path == "lr" = rotate3 node
				 | path == "rr" = rotate4 node

getValue :: RBTree -> String
getValue (RBNode c s a) = s

getChild :: RBTree -> Int -> RBTree
getChild (RBNode c s []) i = RBNode c s []
getChild (RBNode c s [x,y]) i | i == 0 = x
							  | otherwise = y

rotate1 :: RBTree -> RBTree
rotate1 (RBNode c s [x,y]) = RBNode NodeBlack (getValue x) [getChild x 0, RBNode NodeRed s [getChild x 1, y]]

rotate2 :: RBTree -> RBTree
rotate2 (RBNode c s [x,y]) = RBNode NodeBlack (getValue $ getChild y 0) [RBNode NodeRed s [x, getChild (getChild y 0) 0], RBNode NodeRed (getValue y) [getChild (getChild y 0) 1, getChild x 0]]

rotate3 :: RBTree -> RBTree
rotate3 (RBNode c s [x,y]) = RBNode NodeBlack (getValue $ getChild x 1) [RBNode NodeRed (getValue x) [getChild x 0, getChild (getChild x 1) 0], RBNode NodeRed s [getChild (getChild x 1) 1, y]]

rotate4 :: RBTree -> RBTree
rotate4 (RBNode c s [x,y]) = RBNode NodeBlack (getValue y) [RBNode NodeRed s [x, getChild y 0], getChild y 1] 

testt1 = RBNode NodeBlack "8" [RBNode NodeRed "7" [RBNode NodeRed "6" [RBNode NodeBlack "" [], RBNode NodeBlack "" []], RBNode NodeBlack "" []], RBNode NodeBlack "" []]
testt2 = RBNode NodeBlack "8" [RBNode NodeBlack "" [], RBNode NodeRed "10" [RBNode NodeRed "9" [RBNode NodeBlack "" [], RBNode NodeBlack "" []], RBNode NodeBlack "" []]]
testt3 = RBNode NodeBlack "8" [RBNode NodeRed "6" [RBNode NodeBlack "" [], RBNode NodeRed "7" [RBNode NodeBlack "" [], RBNode NodeBlack "" []]], RBNode NodeBlack "" []]
testt4 = RBNode NodeBlack "8" [RBNode NodeBlack "" [], RBNode NodeRed "10" [RBNode NodeBlack "" [], RBNode NodeRed "11" [RBNode NodeBlack "" [], RBNode NodeBlack "" []]]]	

-- Deletion

removeLeftmostNode (RBNode c s [x,y]) | getValue x == "" && (getValue y == "" || getColor y == NodeRed) && c == NodeRed = y
									  | c == NodeBlack && getColor y == NodeRed && getValue (getChild 0 y) == "" && getValue (getChild 1 y) == "" = y
									  | c == NodeBlack && getValue x == "" && getValue y == "" = RBNode NodeGrey "" []
									  | otherwise = removeLeftmostNode x  

greyColourFlip (RBNode NodeBlack s []) = RBNode NodeBlack s []
greyColourFlip (RBNode NodeBlack p [RBNode NodeGrey g gc, RBNode NodeBlack s [RBNode NodeBlack l lc, RBNode NodeBlack r rc]]) 								= RBNode NodeGrey p [RBNode NodeBlack g gc, RBNode NodeRed s [RBNode NodeBlack l lc, RBNode NodeBlack r rc]] 
greyColourFlip (RBNode NodeBlack p [RBNode NodeBlack s [RBNode NodeBlack l lc, RBNode NodeBlack r rc], RBNode NodeGrey g gc]) 								= RBNode NodeGrey p [RBNode NodeRed s [RBNode NodeBlack l lc, RBNode NodeBlack r rc], RBNode NodeBlack g gc] 
								  
greyColourFlip (RBNode c p [RBNode NodeGrey g gc, RBNode NodeBlack s [RBNode NodeRed l [RBNode NodeBlack a ac, RBNode NodeBlack b bc], RBNode rcol r rc]]) 	= RBNode c l [RBNode NodeBlack p [RBNode NodeBlack g gc, RBNode NodeBlack a ac], RBNode NodeBlack s [RBNode NodeBlack b bc, RBNode rcol r rc]]
greyColourFlip (RBNode c p [RBNode NodeBlack s [RBNode rcol r rc, RBNode NodeRed l [RBNode NodeBlack b bc, RBNode NodeBlack a ac]], RBNode NodeGrey g gc])	= RBNode c l [RBNode NodeBlack s [RBNode rcol r rc, RBNode NodeBlack b bc], RBNode NodeBlack p [RBNode NodeBlack a ac, empty]]

greyColourFlip (RBNode NodeRed p [RBNode NodeGrey g gc, RBNode NodeBlack s [RBNode NodeBlack l lc, RBNode rcol r rc]]) 										= RBNode NodeBlack s [RBNode NodeRed p [RBNode NodeBlack g gc, RBNode NodeBlack l lc], RBNode rcol r rc]
greyColourFlip (RBNode NodeRed p [RBNode NodeBlack s [RBNode lcol l lc, RBNode NodeBlack r rc], RBNode NodeGrey g gc])										= RBNode NodeBlack s [RBNode lcol l lc, RBNode NodeRed p [RBNode NodeBlack r rc, RBNode NodeBlack g gc]]

greyColourFlip (RBNode NodeBlack p [RBNode NodeGrey g gc, RBNode NodeBlack s [RBNode NodeBlack l lc, RBNode NodeRed r rc]])									= RBNode NodeBlack s [RBNode NodeBlack p [RBNode NodeBlack g gc, RBNode NodeBlack l lc], RBNode NodeBlack r rc]
greyColourFlip (RBNode NodeBlack p [RBNode NodeBlack s [RBNode NodeRed l lc, RBNode NodeBlack r rc], RBNode NodeGrey g gc])									= RBNode NodeBlack s [RBNode NodeBlack l lc, RBNode NodeBlack p [RBNode NodeBlack r rc, RBNode NodeBlack g gc]]

greyColourFlip (RBNode NodeBlack p [RBNode NodeGrey g gc, RBNode NodeRed s [RBNode NodeBlack l lc, RBNode NodeBlack r rc]])									= RBNode NodeBlack s [RBNode NodeRed p [RBNode NodeBlack g gc, RBNode NodeBlack l lc], RBNode NodeBlack r rc]
greyColourFlip (RBNode NodeBlack p [RBNode NodeRed s [RBNode NodeBlack l lc, RBNode NodeBlack r rc], RBNode NodeGrey g gc])									= RBNode NodeBlack s [RBNode NodeBlack l lc, RBNode NodeRed p [RBNode NodeBlack g gc, RBNode NodeBlack r rc]]

greyColourFlipHelper (RBNode c s []) = RBNode c s []
greyColourFlipHelper (RBNode c s [x,y]) | getColor x == NodeGrey || getColor y == NodeGrey = greyColourFlip (RBNode c s [x,y])
										| otherwise = RBNode c s [greyColourFlipHelper x, greyColourFlipHelper y]

getColor (RBNode c _ _) = c

isGreyBalanced (RBNode c s []) = True
isGreyBalanced (RBNode c s [x,y]) | c == NodeGrey = False
								  | otherwise = isGreyBalanced x && isGreyBalanced y

greyBalance node | isGreyBalanced tree = tree
			     | otherwise = greyBalance tree
				 where
				 	tree = greyColourFlipHelper node

delete :: RBTree -> String -> RBTree
delete tree node = greyBalance $ searchAndDestroy tree node

sad node (RBNode c s [x, empty]) | node == s = grey
								 | otherwise = empty
sad node (RBNode c s [x,y]) | node == s = (RBNode (getColor newnode) (getValue newnode) [x, removeLeftmostNode y])
							| read node <= read s = sad node x
							| otherwise = sad node y
							where
								newnode = leftMostValue y

searchAndDestroy :: String -> RBTree -> RBTree
searchAndDestroy node (RBNode c s [])  = RBNode c s []
searchAndDestroy node (RBNode c s [x,y])  | node == s = (RBNode (getColor newnode) (getValue newnode) [x, removeLeftmostNode y])
							 			  | otherwise = (RBNode c s [searchAndDestroy node x, searchAndDestroy node y]) 
							 			  where
							 			  	newnode = leftMostValue y

createRBTree [x] = RBNode NodeBlack (show x) [empty, empty]
createRBTree (x:xs) = insertToRB (show x) (createRBTree xs)

checkColor :: RBTree -> NodeColor -> Bool
checkColor (RBNode c s a) col = c == col

empty = RBNode NodeBlack "" []
grey = RBNode NodeGrey "" []

testtree = createRBTree [5,4,7,8,9,6,2,1,13,45,63,27,98,65,20,12,42,75,4]

ttree1 = RBNode NodeBlack "5" [RBNode NodeBlack "3" [RBNode NodeBlack "1" [empty, empty], RBNode NodeBlack "4" [empty, empty]], RBNode NodeGrey "" []] 
ttree2 = RBNode NodeRed "5" [RBNode NodeBlack "8" [RBNode NodeBlack "9" [RBNode NodeRed "10" [empty, empty], RBNode NodeBlack "9" [empty, empty]], RBNode NodeRed "6" [RBNode NodeBlack "6" [empty, empty], RBNode NodeBlack "7" [empty, empty]]], RBNode NodeGrey "" []]
ttree3 = RBNode NodeRed "5" [RBNode NodeBlack "8" [RBNode NodeRed "9" [empty, empty], RBNode NodeBlack "7" [empty, empty]], grey]