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
		facts = map (convert clauses) (splitWords x)
		rhs = getRHS facts clauses [("","")]

generateList :: Program -> String -> [String]
generateList prog query
	| facts == [] 	= rmDup $ concat $ map (genListHelp prog (getVar query) (getFactsOf prog)) (getRules prog query)
	| otherwise		= facts
	where 
		facts = getAllFacts prog query

genListHelp :: Program -> String -> [String] -> Clause -> [String]
genListHelp _ _ [] _ = []
genListHelp prog var (f:facts) r
	| calcRule prog [(convert2 r)] [[(var,f)]]	= f: genListHelp prog var facts r
	| otherwise									= genListHelp prog var facts r

rmDup = foldl (\seen x -> if x `elem` seen then seen else seen ++ [x]) []

getRules :: Program -> String -> [Clause]
getRules [] str 					= []
getRules ((Rule s v b):prog) str
	| getName s == getName str 	= (Rule s v b): getRules prog str
	| otherwise 				= getRules prog str		 
getRules ((Fact s b):prog) str 	= getRules prog str

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

calcRule :: Program -> [String] -> [[(String,String)]] -> Bool
calcRule _ [] _			= False
calcRule _ _ []			= False
calcRule prog (s:str) (t:tup)
	| rhs == False 		= calcRule prog str tup
	| s == ""			= False
	| otherwise 		= True
	where
		facts = splitWords $ subVar s t
		rhs = getRHS (map (convert prog) facts) prog t

convert :: Program -> String -> Clause
convert [] x 	= Fact x False 
convert ((Rule s v b):prog) x
	| s == x 	= Rule s v b
	| otherwise	= convert prog x 
convert ((Fact s b):prog) x
	| s == x 	= Fact s b
	| otherwise = convert prog x

convert2 ::  Clause -> String
convert2 (Rule s v b) 	= s
convert2 (Fact s b)		= s

subVar :: String -> [(String, String)] -> String
subVar [] tup = ")"
subVar _ [] = ")"
subVar (x:xs) (t:tup)
	| x == '(' || x == ','	= (x : subVarHelp (getVar (x:xs)) t) ++ subVar (getPara xs) tup
	| otherwise 			= x : subVar xs (t:tup)

subVarHelp :: String -> (String, String) -> String
subVarHelp var (sVar,cons)
	| var == sVar 	= cons
	| otherwise 	= var

isFact :: String -> Bool
isFact [] = True
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
	| b == False	= calcRule others [s] [tup]
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
	| goodLetter 	= split xs (ys ++ [x])
	| ys == ""		= split xs ""
	| otherwise 	= ys : (split xs "")
	where
		goodLetter = isLetter x || isNumber x || x == '(' || x == ')' || x == ','

getFactName :: String -> String
getFactName [] = ""
getFactName (x:xs)
	| goodLetter = x : getFactName xs
	| otherwise = ""
	where
		goodLetter = isLetter x || isNumber x || x == '(' || x == ')' || x == ','

evalOne :: String -> Program -> Either Bool [String]
evalOne query prog
	| checkIfVar var 					= Right $ generateList prog query
	| factResult == False				= Left $ calcRule prog rule varTuple
	| otherwise							= trace "SA" $ Left $ factResult
	where
		factResult = checkFact prog query
		var = getAllVar query
		rule = getAllRules query prog
		varTuple = createTuple rule var

createTuple :: [String] -> [String] -> [[(String, String)]]
createTuple [] _ = []
createTuple (r:rules) vars
	= (cTupHelp r vars): createTuple rules vars

cTupHelp :: String -> [String] -> [(String, String)]
cTupHelp _ [] = []
cTupHelp [] _ = []
cTupHelp rule (v:vars)
	= (getVar rule, v): cTupHelp (getPara rule) vars

checkFact :: Program -> String -> Bool
checkFact [] str = False
checkFact ((Rule s v b):prog) str = checkFact prog str
checkFact ((Fact s b):prog) str
	| getAllVar s == getAllVar str && getName s == getName str 	= b
	| otherwise 				= checkFact prog str

getAllRules :: String -> Program -> [String]
getAllRules str [] 	= [""]
getAllRules str ((Fact s b):prog)
								= getAllRules str prog 
getAllRules str ((Rule s v b):prog)
	| getName s == getName str 	= s: getAllRules str prog
	| otherwise 				= getAllRules str prog

getName :: String -> String
getName [] 				= ""
getName (x:xs) 
	| isLetter x 		= x : getName xs
	| otherwise 		= "" 

checkIfVar :: [String] -> Bool
checkIfVar [] = True
checkIfVar ((x:xs):str)
	= ord x >= 65 && ord x <= 90 && checkIfVar str

-- Command Line Stuff

convert_to_list :: String -> [String] -> [String]
convert_to_list "" [] = []
convert_to_list x [] = [x]
convert_to_list x xs = x:xs

removeQ :: String -> String
removeQ [] 		= ""
removeQ (x:xs)
	| x == '?' 	= ""
	| otherwise = x: removeQ xs 

beautify :: [String] -> String
beautify [] = "\n"
beautify (x:xs)
	= "\t\t" ++  x ++ "\n" ++ beautify xs
s = do
	putStr "\n\t\tProgram:\n"
	putStr $ beautify standard
	getQuery standard

start = do
	putStrLn "Please input a line for the Program. (Send an empty line to signify the end)"
	l <- getInput
	putStrLn "Program:"
	putStr $ beautify l
	getQuery l

getQuery l = do
	putStr "Please input a query to apply to the program.\n" 
	putStr "Type 'program' to see the program again.\n"
	putStr "Use an empty line to close the program.\n"
	line <- getLine
	if null line
		then return ()
		else do
			if line == "program"
				then do
					putStr "\n\t\tProgram:\n"
					putStr $ beautify l
					getQuery l
				else do 
					putStr "\t\tResult:\n\t\t "
					putStrLn . either show show $ evalOne (removeQ line) (parse l)
					putStr "\n"
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

program = parse standard

standard = ["woman(juliana).","woman(beatrix).","woman(margriet).","woman(irene).","woman(christina).",
			"man(bernhard).",
			"mother(juliana,beatrix).","mother(juliana,margriet).","mother(juliana,irene).",
			"father(bernhard,beatrix).","father(bernhard,margriet).",
			"child(K,O):-mother(O,K).","child(K,O):-father(O,K).",
			"son(Z,O):-child(Z,O),man(Z).",
			"daughter(Z,O):-child(Z,O),woman(Z).",
			"sister(X,Y):-child(X,O),woman(X),child(Y,O)."]