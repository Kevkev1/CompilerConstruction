import Prelude
import Data.Maybe
import Data.Char (isDigit)
import Data.List ((\\), delete)


import Eventloop.EventloopCore
import Eventloop.DefaultConfiguration
import Eventloop.Types.EventTypes

import qualified Eventloop.Module.Websocket.Canvas as C
import qualified Eventloop.Module.BasicShapes as B
import qualified Eventloop.Module.Websocket.Mouse as M
import qualified Eventloop.Module.Websocket.Keyboard as K
import qualified Eventloop.Module.StdOut as S
import Eventloop.Module.Graphs




{- | Start
This function will start the eventloop system using the eventloopConfig
-}
start = startMainloop eventloopConfig

{- | The configuration of the Eventloop system
Uses the graphs module to display graphs. This module
depends on the Keyboard, Mouse, Canvas and BasicShapes modules
-}
eventloopConfig = defaultConfig { moduleConfigurations=[ defaultGraphsModuleConfiguration
                                                       , B.defaultBasicShapesModuleConfiguration
                                                       , C.defaultCanvasModuleConfiguration
                                                       , M.defaultMouseModuleConfiguration
                                                       , K.defaultKeyboardModuleConfiguration
                                                       , S.defaultStdOutModuleConfiguration
                                                       ]}
                where
                    defaultConfig = allModulesEventloopConfiguration directedProgramState eventloop -- Uses beginProgramState and eventloop to build config


{- | ProgramState
This datatype shows which variables are kept
-}
data ProgramState 
    = ProgramState { pressedKey :: [Char]
                   , node1Select :: Maybe Node
                   , node2Select :: Maybe Node
                   , graph :: Graph
                   , pathList :: [[Edge]]
                   }                

             
getPathList (ProgramState _ _ _ _  pathList) = pathList !! 0
{- | Begingraph
   This is the start state of the graph
-}
beginGraph = Graph allNodes allEdges Undirected Unweighted
           where
            allNodes = [ ('a', (50, 50), Red)
                       , ('b', (150, 50), Blue)
                       , ('c', (200, 200), Orange)
                       , ('d', (300, 50), Green)
                       , ('e', (300, 120), Purple)
                       , ('f', (400, 80), Orange)
                       ]
            allEdges = [ ('a', 'b', Green, 5, Thick)
                       , ('c', 'b', Orange, 3, Thin)
                       , ('c', 'a', Purple, 2, Thin)
                       , ('d', 'f', Red, 4, Thin)
                       , ('e', 'f', Blue, 3, Thin)
                       , ('e', 'd', Purple, 2, Thin)
                       ]

directedGraph = Graph allNodes allEdges Directed Weighted
              where
                allNodes = [ ('a', (50, 50), Red)
                       , ('b', (150, 50), Blue)
                       , ('c', (200, 200), Orange)
                       , ('d', (300, 50), Green)
                       , ('e', (300, 120), Purple)
                       , ('f', (400, 80), Orange)
                       ]
                allEdges = [ ('a', 'b', Green, 1, Thick)
                       , ('a', 'c', Orange, 5, Thin)
                       , ('b', 'd', Purple, 5, Thin)
                       , ('b', 'c', Grey, 1, Thin)
                       , ('c', 'e', Red, 2, Thin)
                       , ('d', 'f', Blue, 5, Thin)
                       , ('e', 'f', Purple, 1, Thin)
                       ]

endGraph = Graph [] [] Undirected Unweighted


{-| The beginstate of the ProgramState
-}
beginProgramState = ProgramState [] Nothing Nothing beginGraph []
directedProgramState = ProgramState [] Nothing Nothing directedGraph []
 
 
{- | Instructions
This is the list of all possible instructions
Feel free to add your own
-}
instructions = [ "Instructions"
               , "Press 'n' and click on the screen to create a new node"
               , "Press 'r', click on a node and press a letter to rename the node"
               , "Press 'e', click on two nodes to create an edge"
               , "Press 'd', click on a node to delete the node"
               , "Press 'w', click on two nodes and press a number to weight the edge in between"
               , "Press 'f', click on two nodes to delete an edge"
               , "Press 'c', click on a node to change its color to Red"
               , "Press 'u' to undo all coloring and set every node to Orange"
               , "Press 'esc' to abort the current operation and start another"  
               ]                             
          
                
{- | A variable showing which labels are used for visually added nodes
-}
automaticPossibleLabels :: [Label]
automaticPossibleLabels = ['a'..'z'] ++ ['A'..'Z'] ++ ['0'..'9']
       
       
{- | A function to determine which label can be used next
-}         
nextLabel :: [Node] -> Label
nextLabel nodes
    | null leftOverLabels = error "Did not have a leftover label to give to a node. Please do not create so many nodes!"
    | otherwise = head leftOverLabels
    where
        currentLabels = map (\(l, _, _) -> l) nodes
        leftOverLabels = automaticPossibleLabels \\ currentLabels
        
                
{- | Add a node to the graph
-}
addNode :: Graph -> Node -> Graph
addNode g@(Graph{nodes=ns}) n = g {nodes=(n:ns)}


{- | Add an edge to the graph
-}
addEdge :: Graph -> Edge -> Graph
addEdge g@(Graph{edges=es}) e = g {edges=(e:es)}


{- | Create an edge based on two nodes
Is drawn from node1 to node2
-}
createEdge :: Node -> Node -> Edge
createEdge (l1, _, c) (l2, _, _)
    = (l1, l2, c, 0, Thin)
    

{- | Finds the edge directed from the first to the second node
-}
findEdgeFromNodeToNode :: Node -> Node -> Graph -> Maybe Edge
findEdgeFromNodeToNode n1 n2 g
    | null possibleEdges = Nothing
    | otherwise = Just $ head possibleEdges
    where
        allEdges = edges g
        possibleEdges = filter (edgeRunsFromNodeToNode n1 n2) allEdges
        

{- | Finds all edges connected to this node
-}                                   
findEdgesAtNode :: Node -> Graph -> [Edge]
findEdgesAtNode (l, _, _) g
    = filter (\(el1, el2, _, _, _) -> el1 == l || el2 == l) allEdges
    where
        allEdges = edges g
      
        
{- | Finds all edges that are between two nodes
-}        
findEdgesBetweenNodes :: Node -> Node -> Graph -> [Edge]
findEdgesBetweenNodes n1 n2 g
    = filter (edgeIsBetweenNodes n1 n2)  allEdges
    where
        allEdges = edges g


{- | Conditional to check if an edge is connected to both nodes
-}      
edgeIsBetweenNodes :: Node -> Node -> Edge -> Bool
edgeIsBetweenNodes (l1, _, _) (l2, _, _) (el1, el2, _, _, _)
    = (el1 == l1 && el2 == l2) || (el1 == l2 && el2 == l1)


{- | Conditional to check if the runs is directed from the first
to the second node
-}
edgeRunsFromNodeToNode :: Node -> Node -> Edge -> Bool
edgeRunsFromNodeToNode (l1, _, _) (l2, _, _) (el1, el2, _, _, _)
    = (l1 == el1) && (l2 == el2)
        
        
{- | Removes the node from the graph
-}
removeNode :: Node -> Graph -> Graph
removeNode n g 
    = g {nodes = allNodes'}
    where
        allNodes = nodes g
        allNodes' = delete n allNodes

{- | Removes the edge from the graph
-}
removeEdge :: Edge -> Graph -> Graph
removeEdge e g
    = g {edges = allEdges'}
    where
        allEdges = edges g
        allEdges' = delete e allEdges

{- | Removes a node, and all edges connected to it,
from the graph
-}
removeNodeWithAdjoiningEdges :: Node -> Graph -> Graph
removeNodeWithAdjoiningEdges n g
    = g''
    where
        g'  = removeNode n g
        g'' = foldr removeEdge g' (findEdgesAtNode n g) 

{- | Rename a node in the edge to the new label
if the node is connected to that edge
-}        
renameNodeInEdge :: Node -> Label -> Edge -> Edge
renameNodeInEdge (oldL, _, _) newL (el1, el2, color, weight, thickness)
    | oldL == el1 = (newL, el2, color, weight, thickness)
    | oldL == el2 = (el1, newL, color, weight, thickness)
    | otherwise   = (el1, el2, color, weight, thickness)

getNeighbours :: Node -> Graph -> [Node]
getNeighbours node g
    = getNB node allNodes g
    where
        allNodes = nodes g

getNB :: Node -> [Node] -> Graph -> [Node]
getNB node [] g = []
getNB node (x:xs) g
    | length (findEdgesBetweenNodes node x g) /= 0 = x: getNB node xs g
    | otherwise = getNB node xs g

getDirectedNeighbours :: Graph -> Node -> [Node]
getDirectedNeighbours g node
    = getDNB g node allEdges
    where
        allEdges = findEdgesAtNode node g

getDNB :: Graph -> Node -> [Edge] -> [Node]
getDNB g _ [] = []
getDNB g (l,p,c) ((l1,l2,_,_,_):xs)
    | l1 == l = (getNode nodez l2): (getDNB g (l,p,c) xs)
    | isDirected g == Directed = getDNB g (l,p,c) xs
    | otherwise = (getNode nodez l2): (getDNB g (l,p,c) xs)
    where
        nodez = nodes g

getNode :: [Node] -> Label -> Node
getNode [x] l1 = x
getNode ((l,p,c):xs) l1 | l == l1 = (l,p,c)
                        | otherwise = getNode xs l1 


isDirected (Graph _ _ d _) = d

colorNode :: Color -> Node -> Node
colorNode col (l, p, _)
    = (l, p, col)

replaceNodes :: Graph -> [Node] -> [Node] -> Graph
replaceNodes g [] _ = g
replaceNodes g _ [] = g
replaceNodes g (x:xs) (y:ys)
    = (flip addNode) x $ removeNode y (replaceNodes g xs ys)

checkPath :: Node -> Node -> Graph -> Bool
checkPath n1 n2 g
    | length (findEdgesBetweenNodes n1 n2 g) /= 0 = True
    | otherwise = checkPathHelper n2 g' (getNeighbours n1 g)
    where
        g' = removeNodeWithAdjoiningEdges n1 g

checkP g n1 n2 = checkPath n1 n2 g

checkPathHelper :: Node -> Graph -> [Node] -> Bool
checkPathHelper _ _ [] = False
checkPathHelper n2 g (x:xs) 
    | x == n2 = True
    | otherwise = checkPathHelper n2 g' xs && checkPathHelper n2 g' (getNeighbours x g')
    where
        g' = removeNodeWithAdjoiningEdges x g

findPaths :: Graph -> Node -> Node -> [[Node]] 
findPaths g end start | end == start = [[start]]
                     | otherwise = map (start:) paths
                     where
                        nodes = getDirectedNeighbours g start
                        emptyPaths = map (findPaths g' end) nodes
                        paths' = removeEmpty emptyPaths
                        paths = foldr (++) [] paths'
                        g' = removeNodeWithAdjoiningEdges start g

removeEmpty []      = []
removeEmpty ([]:xs) = removeEmpty xs
removeEmpty (x:xs)  = x: removeEmpty xs

colorPathRed :: [Edge] -> [Edge] -> [Edge]
colorPathRed edgez colEdgez
    = colorE black blackPath Red
    where
      black = colorEdges edgez
      blackPath = colorEdges colEdgez

colorEdges :: [Edge] -> [Edge]
colorEdges [] = []
colorEdges (x:xs)
    = (colorEdge Black x): (colorEdges xs)

colE :: [[Edge]] -> Color -> [Edge]
colE [] _ = []
colE (x:xs) col 
    = map (colorEdge col) x ++ (colE xs (nextCol col))

colorE :: [Edge] -> [Edge] -> Color -> [Edge]
colorE alledges [] _ = alledges
colorE alledges (y:ys) col 
    = (colorEdge col y): (colorE (alledges\\[y]) ys col)

colorEdge col (l1, l2, c, l, t) = (l1, l2, col, l, t)

getEdgesList :: Graph -> [[Node]] -> [[Edge]]
getEdgesList g [] = []
getEdgesList g (x:xs) 
    = (getEdges g x): (getEdgesList g xs)
 
getEdges :: Graph -> [Node] -> [Edge]
getEdges g [x] = []
getEdges g (x:x':xs) 
    = (findEdgesBetweenNodes x x' g) ++ getEdges g (x':xs)

findSubGraph :: Graph -> [[Node]]
findSubGraph g 
    = findAllSubGraph g (nodes g)

findAllSubGraph :: Graph -> [Node] -> [[Node]]
findAllSubGraph g [] = []
findAllSubGraph g (x:xs)
    = subGraph : (findAllSubGraph g' others)
    where 
        (subGraph, others) = getSubGraph g x
        g' = g {nodes = others}

getSubGraph :: Graph -> Node -> ([Node],[Node])
getSubGraph g x 
    = ([nodez | nodez <- nodes g,
                checkPath x nodez g && checkPath nodez x g || nodez == x],
       [nodes2 | nodes2 <- nodes g,
                 (not (checkPath x nodes2 g && checkPath nodes2 x g || nodes2 == x))]
      )

colorSubGraph :: [[Node]] -> [Node]
colorSubGraph nodes 
    = colSG nodes Orange

colSG :: [[Node]] -> Color -> [Node]
colSG [] col = []
colSG (x:xs) col
    = (map (colorNode col) x) ++ colSG xs (nextCol col)

nextCol col
    | col == Orange = Red
    | col == Red = Blue
    | col == Blue = Green
    | col == Green = Yellow
    | col == Yellow = Purple
    | col == Purple = Black
    | col == Black = Grey
    | col == Grey = White
    | col == White = Orange
    | otherwise = Orange

getShortest :: [[Edge]] -> [Edge]
getShortest [x] = x
getShortest (x:xs) 
    | calculateValue x < calculateValue shortest = x
    | otherwise = shortest
    where
      shortest = getShortest xs

calculateValue :: [Edge] -> Float
calculateValue [] = 0
calculateValue ((_,_,_,l,_):xs)
    = l + calculateValue xs

{- | The eventloop
This function uses the current state and an In event to determine
the new state and what changes should be made as a list of Out events.
-}
eventloop :: ProgramState -> In -> (ProgramState, [Out])

eventloop ps Start
    = (ps, [OutGraphs SetupGraphs, OutGraphs $ DrawGraph (graph ps), OutGraphs $ Instructions instructions])

eventloop ps@(ProgramState "f" (Just node1s) _ g d) (InGraphs (Mouse (Click _) p))
    | nodeAtPosM == Nothing = (ps, [])
    | edgeM == Nothing = (ProgramState [] Nothing Nothing g d, [])
    | otherwise = (ProgramState [] Nothing Nothing g' d, [OutGraphs $ DrawGraph g', OutStdOut $ S.StdOutMessage $ "Deleted edge from '" ++ [l1] ++ "' to '" ++ [l2] ++ "'\n"])
    where
        nodeAtPosM = onNode allNodes p
        (Just nodeAtPos) = nodeAtPosM
        allNodes = nodes g
        edgeM = findEdgeFromNodeToNode node1s nodeAtPos g
        (Just edge) = edgeM
        (l1, l2, _, _, _) = edge
        g' = removeEdge edge g
                     
{- | If 'w' has been pressed, two nodes are selected and the next key
is a digit, the edge running from node1s to node2s is weighted as that
digit
-}                    
eventloop ps@(ProgramState "w" (Just node1s) (Just node2s) g d) (InGraphs (Key [key]))
    | isDigit key && edgeM /= Nothing = (ProgramState [] Nothing Nothing g' d, [OutGraphs $ DrawGraph g', OutStdOut $ S.StdOutMessage $ "Weighted edge from '" ++ [l1] ++ "' to '" ++ [l2] ++ "' with " ++ (show weight) ++ "\n"])
    | otherwise   = (ProgramState [] Nothing Nothing g d, [])
    where
        edgeM = findEdgeFromNodeToNode node1s node2s g
        (Just edge@(l1, l2, col, w, thick)) = edgeM
        weight = read [key] :: Weight
        edge' = (l1, l2, col, weight, thick)
        g' =  (flip addEdge) edge' $ removeEdge edge g

{- | If 'd' has been pressed and a node is selected
, the node is deleted from the graph
-}
eventloop ps@(ProgramState "d" _ _ g d) (InGraphs (Mouse (Click _) p))
    | nodeAtPosM == Nothing = (ps, [])
    | otherwise = (ProgramState [] Nothing Nothing g' d, [OutGraphs $ DrawGraph g', OutStdOut $ S.StdOutMessage $ "Deleted node '" ++ [l] ++ "'\n"])
    where
        (l, _, _) = nodeAtPos
        nodeAtPosM = onNode allNodes p
        (Just nodeAtPos) = nodeAtPosM
        allNodes = nodes g
        g' = removeNodeWithAdjoiningEdges nodeAtPos g

     
{- | If 'e' has been pressed, a node selected and a new node is selected
an edge is drawn between the two nodes
-}
eventloop ps@(ProgramState "e" (Just node1s) _ g d) (InGraphs (Mouse (Click _) p))
    | nodeAtPosM == Nothing = (ps, [])
    | otherwise = (ProgramState [] Nothing Nothing g' d, [OutGraphs $ DrawGraph g', OutStdOut $ S.StdOutMessage $ "Created edge from '" ++ [l1] ++ "' to '" ++ [l2] ++ "'\n"])
    where
        (l1, _, _) = node1s
        (l2, _, _) = nodeAtPos
        nodeAtPosM = onNode allNodes p
        allNodes = nodes g
        (Just nodeAtPos) = nodeAtPosM
        g' = addEdge g $ createEdge node1s nodeAtPos
                  
                  
{- | If 'r' has been pressed, a node selected and a new key stroke
comes in, the label of the selected node is changed
-}
eventloop ps@(ProgramState "r" (Just node1s) _ g d) (InGraphs (Key [l]))
    = (ProgramState [] Nothing Nothing g'' d, [OutGraphs $ DrawGraph g'', OutStdOut $ S.StdOutMessage $ "Renamed node '" ++ [oldL] ++ "' to '" ++ [l] ++ "'\n"])
    where
        allNodes = nodes g
        allEdges = edges g
        (oldL, p, color) = node1s
        node' = (l, p, color)
        allEdges' = map (renameNodeInEdge node1s l) allEdges :: [Edge]
        g'  = (flip addNode) node' $ removeNode node1s g
        g'' = g' {edges = allEdges'}

eventloop ps@(ProgramState "p" (Just node1s) _ g d) (InGraphs (Mouse (Click _) p))
    = (ProgramState [] Nothing Nothing g d, [OutStdOut $ S.StdOutMessage $ "Checked for path between '" ++ [l1] ++ "' and '" ++ [l2] ++ "'\n Found? " ++ show(result) ++ "\n"])
    where
        (l1, _, _) = node1s
        (l2, _, _) = nodeAtPos
        nodeAtPosM = onNode allNodes p
        allNodes = nodes g
        (Just nodeAtPos) = nodeAtPosM
        result = checkPath node1s nodeAtPos g



eventloop ps@(ProgramState "j" (Just node1s) _ g d) (InGraphs (Mouse (Click _) p))
    = (ProgramState [] Nothing Nothing g' newPaths, [OutGraphs $ DrawGraph g', OutStdOut $ S.StdOutMessage $ "Checked for path between '" ++ [l1] ++ "' and '" ++ [l2] ++ "'\n Found? " ++ show(result) ++ "\n"])
    where
        (l1, _, _) = node1s
        (l2, _, _) = nodeAtPos
        nodeAtPosM = onNode allNodes p
        allNodes = nodes g
        (Just nodeAtPos) = nodeAtPosM
        result = findPaths g nodeAtPos node1s
        edges' = colorPathRed (edges g) ((getEdgesList g result) !! 0)
        g' = g {edges = edges'}
        newPaths = tail $ getEdgesList g result


eventloop ps (InGraphs (Key "k"))
    | pathList ps == [] = (ps, [])
    | otherwise = (ProgramState [] Nothing Nothing g' newPaths, [OutGraphs $ DrawGraph g', OutStdOut $ S.StdOutMessage $ "Next path'\n"])
        where 
            path = (pathList ps) !! 0
            newPaths = tail (pathList ps)
            g = (graph ps)
            edges' = colorPathRed (edges g) path
            g' = g {edges=edges'}

eventloop ps@(ProgramState "l" (Just node1s) _ g d) (InGraphs (Mouse (Click _) p))
    = (ProgramState [] Nothing Nothing g' d, [OutGraphs $ DrawGraph g', OutStdOut $ S.StdOutMessage $ "Shortest path between '" ++ [l1] ++ "' and '" ++ [l2] ++ "'\n Found? " ++ show(short) ++ "\n"])
    where
        (l1, _, _) = node1s
        (l2, _, _) = nodeAtPos
        nodeAtPosM = onNode allNodes p
        allNodes = nodes g
        (Just nodeAtPos) = nodeAtPosM
        result = findPaths g nodeAtPos node1s
        short = getShortest (getEdgesList g result)
        edges' = colorPathRed (edges g) short
        g' = g {edges = edges'}

eventloop ps@(ProgramState _ _ _ g d) (InGraphs (Key "i"))
    = (ProgramState [] Nothing Nothing g d, [OutStdOut $ S.StdOutMessage $ "Strongly Connected? " ++ show(result) ++ "\n"])
    where
        (x:xs) = nodes g
        result = all (== True) (map (checkP g x) xs)

eventloop ps@(ProgramState _ _ _ g d) (InGraphs (Key "t"))
    = (ProgramState [] Nothing Nothing g' d, [OutGraphs $ DrawGraph g', OutStdOut $ S.StdOutMessage $ "Subgraphs?" ++ show(result) ++ "\n"])
    where
        result = colorSubGraph (findSubGraph g)
        g' = g {nodes = result}
                    
                    
{- | If 'n' has been pressed and the mouse has 
clicked at a position where there is no node yet,
a new node is inserted at that point
-}                    
eventloop ps@(ProgramState "n" _ _ g d) (InGraphs (Mouse (Click _) p))
    | nodeAtPosM == Nothing = (ProgramState [] Nothing Nothing g' d, [OutGraphs $ DrawGraph g', OutStdOut $ S.StdOutMessage $ "Inserted node '" ++ [nextlabel] ++ "'\n"])
    | otherwise             = (ps, [OutStdOut $ S.StdOutMessage "Tried to insert a node on another node"])
    where
        nodeAtPosM = onNode allNodes p
        allNodes = nodes g
        nextlabel = nextLabel allNodes
        newNode = (nextlabel, p, Orange)
        g' = g {nodes=(newNode:allNodes)}


eventloop ps@(ProgramState "c" _ _ g d) (InGraphs (Mouse (Click _) p))
    | nodeAtPosM == Nothing = (ps, [OutStdOut $ S.StdOutMessage "That's no node!"])
    | otherwise = (ProgramState [] Nothing Nothing g'' d, [OutGraphs $ DrawGraph g'', OutStdOut $ S.StdOutMessage $ "Colored node '" ++ [l] ++ "' red\n"])
    where
        nodeAtPosM = onNode allNodes p
        (Just nodeAtPos) = nodeAtPosM
        (l, p1, _) = nodeAtPos
        allNodes = nodes g
        neighbours = getNeighbours nodeAtPos g
        newNeighbours = map (colorNode Blue) neighbours
        g' = (flip addNode) (l, p1, Red) $ removeNode nodeAtPos g
        g'' = replaceNodes g' newNeighbours neighbours

         
{- | Buffer the last node selected if it doesn't 
trigger an event on first spot
-}
eventloop ps@(ProgramState _ Nothing _ g d) (InGraphs (Mouse (Click _) p))
    | nodeAtPosM == Nothing = (ps, [])
    | otherwise             = (ps {node1Select = Just nodeAtPos}, [OutStdOut $ S.StdOutMessage $ "[1st Select] Click on node '" ++ [l] ++ "'\n"])
    where
        (l, _, _) = nodeAtPos
        (Just nodeAtPos) = nodeAtPosM
        nodeAtPosM = onNode allNodes p
        allNodes = nodes g


eventloop ps@(ProgramState _ _ _ g d) (InGraphs (Key "u"))
    = (ProgramState [] Nothing Nothing g' d, [OutGraphs $ DrawGraph g', OutStdOut $ S.StdOutMessage $ "Removed Colors\n"])
    where
        newNodes = map (colorNode Orange) allNodes
        allNodes = nodes g
        g' = replaceNodes g newNodes allNodes

{- | Buffer the last node selected if it doesn't trigger an event on second spot -}
eventloop ps@(ProgramState _ (Just _) Nothing g d) (InGraphs (Mouse (Click _) p))
    | nodeAtPosM == Nothing = (ps, [OutStdOut $ S.StdOutMessage "Clicked on not a node\n"])
    | otherwise             = (ps {node2Select = Just nodeAtPos}, [OutStdOut $ S.StdOutMessage $ "[2nd Select] Click on node '" ++ [l] ++ "'\n"])
    where
        (l, _, _) = nodeAtPos
        (Just nodeAtPos) = nodeAtPosM
        nodeAtPosM = onNode allNodes p
        allNodes = nodes g

{- | Abort current operation and reset start on "esc" -}
eventloop ps (InGraphs (Key "esc"))
    = (ProgramState [] Nothing Nothing (graph ps) (pathList ps), [OutStdOut $ S.StdOutMessage "Aborted current operation\n"])


{- | Stop the system on "s" -}
eventloop ps (InGraphs (Key "s"))
    = (ps, [OutStdOut $ S.StdOutMessage "Stopping system...\n", Stop])
        
        
{- | Buffer the last press key if it doesn't trigger an event -}
eventloop ps@(ProgramState _ _ _ _ _) (InGraphs (Key key))
    = (ps {pressedKey = key}, [OutStdOut $ S.StdOutMessage $ "Buffered keystroke '" ++ key ++ "'\n" ])
        
         
{- | For all other In events, do nothing -}                                                                            
eventloop ps _ = (ps, [])  
