import Data.Char
import Data.List
import Debug.Trace

-- The real program

type Program = [Clause]

data Clause = Fact String Bool
			| Rule String Bool
			deriving (Eq, Show) 

parse :: [String] -> Program
parse [] = []
parse (x:xs) 
	= parseHelp (x:xs) []

parseHelp :: [String] -> [Clause] -> Program
parseHelp [] clauses = clauses
parseHelp (x:xs) clauses
	| isFact x = parseHelp xs (clauses ++ [Fact (getFactName x) True])
	| otherwise = parseHelp xs (clauses ++ [Rule x rhs])
	where
		facts = getClauses (splitWords x) clauses
		rhs = getRHS facts clauses 

isFact :: String -> Bool
isFact [] = False
isFact (x:xs)
	| x == '.' 	= True
	| x == ':'  = False
	| otherwise = isFact xs

getRHS :: [Clause] -> Program -> Bool
getRHS [] others = True
getRHS ((Fact s b):xs) others
	= b && elem (Fact s b) others && getRHS xs others
getRHS ((Rule s b):xs) others
	= b && elem (Rule s b) others && getRHS xs others

splitWords :: String -> [String]
splitWords [] = [] 
splitWords (x:xs)
	| x == '-' 	= split xs ""
	| otherwise = splitWords xs 

split :: String -> String -> [String]
split [] ys | ys == "" = []
			| otherwise = [ys]
split (x:xs) ys	
	| isGoodName [x] 	= split xs (ys ++ [x])
	| ys == ""		= split xs ""
	| otherwise 	= ys : (split xs "")

getClauses :: [String] -> Program -> [Clause]
getClauses [] _ = []
getClauses (x:xs) prog
	= getFromProgram prog x : getClauses xs prog

getFromProgram :: Program -> String -> Clause
getFromProgram [] str = Fact str False
getFromProgram ((Fact s b):xs) str
	| s == str = Fact s b
	| otherwise = getFromProgram xs str
getFromProgram ((Rule s b):xs) str
	| getLHS s "" == str = Rule s b
	| otherwise = getFromProgram xs str

getLHS :: String -> String -> String
getLHS [] str = str
getLHS (x:xs) str
	| x == ':' || x == ' ' || x == '.' || x == '?' = str
	| otherwise = getLHS xs (str ++ [x]) 

getFactName :: String -> String
getFactName [] = ""
getFactName (x:xs)
	| isGoodName [x] = x : getFactName xs
	| otherwise = ""
	

checkFromString :: Program -> String -> Bool
checkFromString [] str = False
checkFromString ((Fact s b):xs) str
	| getLHS s "" == str = b
	| otherwise = checkFromString xs str
checkFromString ((Rule s b):xs) str
	| getLHS s "" == str = b
	| otherwise = checkFromString xs str

isGoodName :: String -> Bool
isGoodName [] = True
isGoodName (x:xs)
	= (isLetter x || isNumber x) && isGoodName xs 

evalProp :: String -> Program -> Bool
evalProp query [] = False
evalProp query ((Fact s b):xs)
	| getLHS query "" == getLHS s "" 	= b
	| otherwise 						= evalProp query xs
evalProp query ((Rule s b):xs)
	| getLHS query "" == getLHS s ""	= b
	| otherwise 					 	= evalProp query xs

-- Command Line Stuff

convert_to_list :: String -> [String] -> [String]
convert_to_list "" [] = []
convert_to_list x [] = [x]
convert_to_list x xs = x:xs

putInList :: String -> [String] -> [String]
putInList x list = list ++ [x]

main = do
	putStrLn "Please input a line for the Program. (Send an empty line to signify the end)"
	l <- getInput
	putStrLn "Program:"
	print l
	getQuery l

getQuery l = do
	putStrLn "Please input a query to apply to the program"
	line <- getLine
	if null line
		then return ()
		else do
			print $ evalProp line (parse l)
			getQuery l

getInput = do
	line <- getLine
    	if null line
        then return []
        else do  
        	if line == "standard"
        	then return standard
        	else do
				a <- getInput
				return (convert_to_list line a)

standard = ["a0.", "a1.", "a2.", "b0 :- a0, a1.", "b1 :- a1, a2.", "b2 :- a1, a2, d.", "c0 :- b0, b1.", "c1 :- b0, b1, b2."]

t1 = "a."
t2 = "b."
t3 = "c :- a,b."
t4 = "d :- c,b."
t5 = "e :- a,f."
t6 = "g :- a,e."

pTest = parse [t1, t2, t3, t4, t5, t6]