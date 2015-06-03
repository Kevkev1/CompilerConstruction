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
		rhs = getRHS facts clauses [("","")]

generateList :: Program -> String -> String -> [String]
generateList prog str name
	| facts == [] 	= genListHelp prog str (getFactsOf prog) (getRule prog name)
	| otherwise		= facts
	where 
		facts = getAllFacts prog name

genListHelp :: Program -> String -> [String] -> Clause -> [String]
genListHelp prog var []	_		= [] 
genListHelp prog var (x:xs) (Rule s v b)
	| calcRule prog s [(var, x)] 	= x : genListHelp prog var xs (Rule s v b)
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

calcRule :: Program -> String -> [(String, String)] -> Bool
calcRule []	_ _ = False
calcRule prog x tup
	= calcRuleHelper prog (subVar x tup) tup

calcRuleHelper :: Program -> String -> [(String,String)] -> Bool
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

isCapital x
	= ord x >= 65 && ord x <= 90

subVar :: String -> [(String, String)] -> String
subVar [] tup = ""
subVar [x] _ = [x]
subVar (x:x':xs) tup
	| x == '(' || isCapital x' = (x:(subVarHelp (x:x':xs) tup)) ++ subVar (getPara xs) tup
	| otherwise = x: subVar (x':xs) tup

subVarHelp :: String -> [(String,String)] -> String
subVarHelp str []		 = ""
subVarHelp str ((sVar,rVar):rest)
	| getVar str == sVar = rVar
	| otherwise			 = subVarHelp str rest

isFact :: String -> Bool
isFact [] = False
isFact (x:xs)
	| x == '.' 	= True
	| x == ':'  = False
	| otherwise = isFact xs

getPara :: String -> String
getPara [] = ""
getPara (x:xs) 
	| x == ')' || x == ',' = (x:xs)
	| otherwise = getPara xs

getAllVar :: String -> [String]
getAllVar [] = []
getAllVar (x:xs) 
	| x == '(' || x == ','	= (getVar (x:xs)): getAllVar (getPara xs)
	| otherwise				= getAllVar xs

getVar :: String -> String
getVar [] =	""
getVar (x:xs) 
	| x == '(' || x == ',' = getVarHelp xs
	| otherwise = getVar xs

getVarHelp :: String -> String
getVarHelp [] = ""
getVarHelp (x:xs)
	| x == ')' || x == ',' = ""
	| otherwise = x : getVarHelp xs

getRHS :: [Clause] -> Program -> [(String, String)] -> Bool
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
	= (isLetter x || isNumber x || x == '(' || x == ')' || x == ',') && isGoodName xs 

evalOne :: String -> Program -> Either Bool [String]
evalOne query prog
	| checkIfVar var 					= Right $ generateList prog var query
	| isRule prog query 				= trace ("VT: " ++ show(ruleVars)) $ Left $ calcRule prog rule varTuple
	| otherwise							= Left $ checkFact prog query
	where
		var 		= getVar query
		queryVars 	= getAllVar query
		ruleVars 	= getAllVar rule
		rule 		= getRuleFromString query prog
		varTuple 	= createTupleList ruleVars queryVars

createTupleList :: [String] -> [String] -> [(String,String)]
createTupleList [] _ = []
createTupleList _ [] = []
createTupleList (rVar:rRest) (qVar:qRest)
	= (rVar,qVar): createTupleList rRest qRest

getRuleFromString :: String -> Program -> String
getRuleFromString str [] 	= ""
getRuleFromString str ((Fact s b):prog)
								= getRuleFromString str prog 
getRuleFromString str ((Rule s v b):prog)
	| getName s == getName str 	= s
	| otherwise 				= getRuleFromString str prog

checkFact :: Program -> String -> Bool
checkFact [] str = False
checkFact ((Rule s v b):prog) str = checkFact prog str
checkFact ((Fact s b):prog) str
	| getVar s == getVar str && getName s == getName str 	= b
	| otherwise 											= checkFact prog str

isRule :: Program -> String -> Bool
isRule [] str = False
isRule ((Fact s b):prog) str = isRule prog str
isRule ((Rule s v b):prog) str
	| getName s == getName str 	= True
	| otherwise 				= isRule prog str

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
        	then return standard
        	else do
				a <- getInput
				return (convert_to_list line a)

standard = ["woman(juliana).","woman(beatrix).","woman(margriet).","woman(irene).","woman(christina).",
			"man(bernhard).",
			"mother(juliana,beatrix).","mother(juliana,margriet).","mother(juliana,irene).",
			"father(bernhard,beatrix).","father(bernhard,margriet).",
			"child(K,O):-mother(O,K).","child(K,O):-father(O,K).",
			"son(Z,O):-child(Z,O),man(Z).",
			"daughter(Z,O):-child(Z,O),woman(Z).",
			"sister(X,Y):-child(X,O),woman(X),child(Y,O)."]

t1 = "a."
t2 = "b."
t3 = "c :- a,b."
t4 = "d :- c,b."
t5 = "e :- a,f."
t6 = "g :- a,e."

pTest = parse [t1, t2, t3, t4, t5, t6]
