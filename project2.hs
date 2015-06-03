import Data.Char
import Data.List
import Debug.Trace

-- The real program

type Program = [Clause]

type Var = String

data Clause = Fact String Bool
			| Rule String Var Bool
			deriving (Eq, Show) 

parse :: [String] -> Program
parse [] = []
parse (x:xs) 
	= parseHelp (x:xs) []

parseHelp :: [String] -> [Clause] -> Program
parseHelp [] clauses = clauses
parseHelp (x:xs) clauses
	| isFact x = parseHelp xs (clauses ++ [Fact (getFactName x) True])
	| otherwise = parseHelp xs (clauses ++ [Rule x (getVar x) rhs])
	where
		facts = getClauses (splitWords x) clauses
		rhs = getRHS facts clauses ("","")

generateList :: Program -> String -> String -> [String]
generateList prog str name
	| facts == [] 	= genListHelp prog str (getFactsOf prog) (getRule prog name)
	| otherwise		= facts
	where 
		facts = getAllFacts prog name

genListHelp :: Program -> String -> [String] -> Clause -> [String]
genListHelp prog var []	_		= [] 
genListHelp prog var (x:xs) (Rule s v b)
	| calcRule prog s (var, x) 	= x : genListHelp prog var xs (Rule s v b)
	| otherwise 				= genListHelp prog var xs (Rule s v b)
genListHelp prog var (x:xs) (Fact s b) = []

getRule :: Program -> String -> Clause
getRule [] str 					= Fact "Nope" False
getRule ((Rule s v b):prog) str
	| getLHS s "" == str 			= Rule s v b
	| otherwise 				= getRule prog str		 
getRule ((Fact s b):prog) str 	= getRule prog str

getFactsOf :: Program -> [String]
getFactsOf [] 				 	= []
getFactsOf ((Fact s b):prog) 	
	| elem var others 			= others
	| otherwise					= var:others
	where
		var = getVar s
		others = getFactsOf prog
getFactsOf (_:prog) 			= getFactsOf prog 

getAllFacts :: Program -> String -> [String]
getAllFacts []	str 			= []
getAllFacts	((Rule s v b):prog) str
								= getAllFacts prog str
getAllFacts ((Fact s b):prog) str 
	| getName s == getName str 	= (getVar s): getAllFacts prog str
	| otherwise					= getAllFacts prog str

calcRule :: Program -> String -> (String, String) -> Bool
calcRule []	_ _ = False
calcRule prog x tup
	= calcRuleHelper prog (subVar x tup) tup

calcRuleHelper :: Program -> String -> (String,String) -> Bool
calcRuleHelper prog str tup
	| rhs == False 		= calcRuleHelper (removeFirst prog str) str tup
	| otherwise			= True
	where
		facts = getClauses (splitWords str) prog
		rhs = getRHS facts prog tup

removeFirst :: Program -> String -> Program
removeFirst [] str = []
removeFirst ((Fact s b):prog) str = (Fact s b): removeFirst prog str
removeFirst ((Rule s v b):prog) str
	| getName s == getName str 	= prog
	| otherwise 				= (Rule s v b): removeFirst prog str 

subVar :: String -> (String, String) -> String
subVar [] tup = ""
subVar (x:xs) tup
	| x == '(' 	= (x : subVarHelp (getVar (x:xs)) tup) ++ subVar (getPara xs) tup
	| otherwise = x : subVar xs tup

subVarHelp :: String -> (String, String) -> String
subVarHelp var (sVar,cons)
	| var == sVar 	= cons
	| otherwise 	= var

getPara :: String -> String
getPara [] = ""
getPara (x:xs) 
	| x == ')' = (x:xs)
	| otherwise = getPara xs

isFact :: String -> Bool
isFact [] = False
isFact (x:xs)
	| x == '.' 	= True
	| x == ':'  = False
	| otherwise = isFact xs

getVar :: String -> String
getVar [] =	""
getVar (x:xs) 
	| x == '(' = getVarHelp xs
	| otherwise = getVar xs

getVarHelp :: String -> String
getVarHelp [] = ""
getVarHelp (x:xs)
	| x == ')' = ""
	| otherwise = x : getVarHelp xs

getRHS :: [Clause] -> Program -> (String, String) -> Bool
getRHS [] others _ = True
getRHS ((Fact s b):xs) others tup
	= b && elem (Fact s b) others && getRHS xs others tup
getRHS ((Rule s v b):xs) others tup
	| b == False	= calcRule others s tup
	| otherwise 	= elem (Rule s v b) others && getRHS xs others tup

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
getFromProgram ((Rule s v b):xs) str
	| getName s == getName str = Rule s v b
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
checkFromString ((Rule s v b):xs) str
	| getLHS s "" == str = b
	| otherwise = checkFromString xs str

isGoodName :: String -> Bool
isGoodName [] = True
isGoodName (x:xs)
	= (isLetter x || isNumber x || x == '(' || x == ')') && isGoodName xs 

evalOne :: String -> Program -> Either Bool [String]
evalOne query prog
	| checkIfVar var 					= Right $ generateList prog var query
	| isRule prog query					= Left $ calcRule prog rule (getVar rule, var)
	| otherwise							= Left $ checkFact prog query
	where
		var = getVar query
		rule = getRuleFromString query prog

checkFact :: Program -> String -> Bool
checkFact [] str = False
checkFact ((Rule s v b):prog) str = checkFact prog str
checkFact ((Fact s b):prog) str
	| getVar s == getVar str 	= b
	| otherwise 				= checkFact prog str

isRule :: Program -> String -> Bool
isRule [] str = False
isRule ((Fact s b):prog) str = isRule prog str
isRule ((Rule s v b):prog) str
	| getName s == getName str 	= True
	| otherwise 				= isRule prog str

getRuleFromString :: String -> Program -> String
getRuleFromString str [] 	= ""
getRuleFromString str ((Fact s b):prog)
							= getRuleFromString str prog 
getRuleFromString str ((Rule s v b):prog)
	| getName s == getName str 	= s
	| otherwise 				= getRuleFromString str prog

getName :: String -> String
getName [] 				= ""
getName (x:xs) 
	| isLetter x 		= x : getName xs
	| otherwise 		= "" 

checkIfVar :: String -> Bool
checkIfVar (x:xs)
	= ord x >= 65 && ord x <= 90

-- Command Line Stuff

convert_to_list :: String -> [String] -> [String]
convert_to_list "" [] = []
convert_to_list x [] = [x]
convert_to_list x xs = x:xs

putInList :: String -> [String] -> [String]
putInList x list = list ++ [x]

removeQ :: String -> String
removeQ [] 		= ""
removeQ (x:xs)
	| x == '?' 	= ""
	| otherwise = x: removeQ xs 

start = do
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
			print $ evalOne (removeQ line) (parse l)
			getQuery l

getInput = do
	line <- getLine
    	if null line
        then return []
        else do  
        	if line == "standard"
        	then return s1
        	else do
				a <- getInput
				return (convert_to_list line a)

standard = ["p(a).", "p(b).", "p(c).", "q(a).", "q(b).", "s(X) :- p(X),q(X).", "r(X) :- s(X),q(X)."]
s1 = ["q(a).","q(b).","p(a).","p(b).","p(c).","k(c).","k(d).","r(X):-p(X),q(Y),k(Y).", "t(X):-q(X)."]

t1 = "a."
t2 = "b."
t3 = "c :- a,b."
t4 = "d :- c,b."
t5 = "e :- a,f."
t6 = "g :- a,e."

pTest = parse [t1, t2, t3, t4, t5, t6]
