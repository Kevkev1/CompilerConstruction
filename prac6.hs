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

import Debug.Trace

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
                    defaultConfig = allModulesEventloopConfiguration beginProgramState eventloop -- Uses beginProgramState and eventloop to build config


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
putElemLast (x:xs) = xs ++ [x]         
{- | Begingraph
   This is the start state of the graph
-}
beginGraph = Graph allNodes allEdges Undirected Unweighted
           where
            allNodes = [ ('a', (50, 50), Red)
                       , ('b', (150, 50), Blue)
                       , ('c', (200, 200), Orange)
                       ]
            allEdges = [ ('a', 'b', Green, 5, Thick)
                       , ('c', 'b', Orange, 3, Thin)
                       , ('c', 'a', Purple, 2, Thin)
                       ]
nextColor color | color == Red = Blue
                | color == Blue = Orange
                | color == Orange = Green
                | color == Green = Purple
                | color == Purple = Grey
                | color == Grey = Yellow
                | color == Yellow = Black
                | color == Black = White
                | color == White = Red
                | otherwise = Orange

{-| The beginstate of the ProgramState
-}
beginProgramState = ProgramState [] Nothing Nothing beginGraph []
isDirected (Graph _ _ d _) = d
getAllNodes _ [] _ = []
getAllNodes node (x:xs) g | ((isDirected g) == Directed) &&  (findEdgeFromNodeToNode node x g) /= Nothing = x : (getAllNodes node xs g)
                          |  ((isDirected g) == Undirected) &&  (findEdgesBetweenNodes node x g) /= [] = x : (getAllNodes node xs g)
                          | otherwise =  (getAllNodes node xs g)  

getAllOtherNodes _ [] _ = []
getAllOtherNodes node (x:xs) g | ((isDirected g) == Directed) && (findEdgeFromNodeToNode node x g) /= Nothing = (getAllOtherNodes node xs g)   
                                |  ((isDirected g) == Undirected) &&  (findEdgesBetweenNodes node x g) /= [] = (getAllOtherNodes node xs g)
                          | otherwise = x :  (getAllOtherNodes node xs g)
                          
changeColor [] _ = []        
changeColor ((l, p, color):xs) c2 =   (l, p, c2) :  (changeColor xs c2) 
            
findPaths g nodeend nodestart | nodeend == nodestart = [[nodestart]]
                             | edges == [] = []
                             | otherwise = map (nodestart:) pathlist
                            where   edges = findEdgesAtNode nodestart g
                                    nodes = getNodesOnEdge g edges nodestart
                                    paths = map (findPaths g' nodeend) nodes
                                    nonEmptyPaths = removeEmptyList paths
                                    pathlist = foldr (++) [] nonEmptyPaths
                                    g' = removeNodeWithAdjoiningEdges nodestart g

removeEmptyList [] = []
removeEmptyList ([]:xs) =  removeEmptyList xs                                   
removeEmptyList (x:xs) = x:removeEmptyList xs
                    
getEdgesList [] g = []
getEdgesList (x:xs) g = (getEdges x g) : (getEdgesList xs g)                             
getEdges [x] g = []      
getEdges (node:node2:ys) g |  (findEdgeFromNodeToNode2 node node2 g)==[] && (findEdgeFromNodeToNode2 node2 node g) == [] = []
                           | not ((findEdgeFromNodeToNode2 node node2 g) == []) = ((findEdgeFromNodeToNode2 node node2 g)!!0): (getEdges (node2:ys) g)
                           | otherwise = ((findEdgeFromNodeToNode2 node2 node g)!!0): (getEdges (node2:ys) g)

findPath g nodeend nodestart | nodeend == nodestart = True
                             | edges == [] = False
                             | otherwise = elem True paths
                            where   edges = findEdgesAtNode nodestart g
                                    nodes = getNodesOnEdge g edges nodestart
                                    paths = map (findPath g' nodeend) nodes
                                    g' = removeNode nodestart g 
getNodesOnEdge _ [] _ = []                                    
getNodesOnEdge g ((l1,l2,_,_,_):xs) (l,p,c) | l1 == l = (findNode2 (nodes g) l2) :(getNodesOnEdge g xs (l,p,c))
                                            | ((isDirected g)== Directed) = (getNodesOnEdge g xs (l,p,c))
                                            | otherwise = (findNode2 (nodes g) l1) :(getNodesOnEdge g xs (l,p,c))
findNode2 [x] _ = x
findNode2 ((l,p,c):xs) l2 | l2 == l = (l,p,c)
                         | otherwise = findNode2 xs l2
                         
stronglyConnected g = stronglyConnectedHelper g (nodes g)
stronglyConnectedHelper g [] = True                         
stronglyConnectedHelper g (node:xs) | elem False paths = False
                                    | otherwise = stronglyConnectedHelper g xs
                            where   nodes2 = nodes g
                                    paths = map (findPath g node) nodes2
start3 = putStrLn (show $ findSubGraphs graph' )
                where
                    graph' = addNode beginGraph ('d', (400, 400), Red)
                    (x,y) = findSubGraphHelper2 graph' (head $ nodes graph')
                    graph'' = graph' {nodes=y}
                    
start4 = putStrLn (show $ findSubGraphHelper graph' (nodes graph'') )
                where
                    graph' = addNode beginGraph ('d', (400, 400), Red)
                    (x,y) = findSubGraphHelper2 graph' (head $ nodes graph')
                    graph'' = graph' {nodes=y}
start2 = putStrLn (show $ findSubGraphs $ graph'' )
       where
            graph' = addNode beginGraph ('d', (400, 400), Red)
            graph'' = graph' {nodes = colorSubGraphs $  findSubGraphs graph'}

findSubGraphs :: Graph -> [[Node]]                                    
findSubGraphs g = findSubGraphHelper g (nodes g)

findSubGraphHelper :: Graph -> [Node] -> [[Node]]
findSubGraphHelper g [] = []
findSubGraphHelper g (x:xs) = subGraph : (findSubGraphHelper graph' rest)
                    where   (subGraph, rest) = (findSubGraphHelper2 g x)
                            graph' = g {nodes=rest}

findSubGraphHelper2 g node = ([ nodelist | nodelist <- (nodes g), 
                                           findPath g nodelist node &&  findPath g node nodelist], 
                                           [ nodelist2 | nodelist2 <- (nodes g), (not $ (findPath g nodelist2 node && findPath g node nodelist2)) ])

colorSubGraphs :: [[Node]] -> [Node]
colorSubGraphs xs = colorSubGraphsHelper xs Orange

colorSubGraphsHelper :: [[Node]] -> Color -> [Node]
colorSubGraphsHelper [] c = []
colorSubGraphsHelper (x:xs) c = coloredNodes++(colorSubGraphsHelper xs (nextColor c))
                        where coloredNodes = changeColor x c

colorPath [] _ _ = []
colorPath ((l1, l2, c, l, t):xs) edges c2  | isElem (l1, l2, c, l, t) edges Red = (l1, l2, c2, l, t) : (colorPath xs edges c2)
                                           | otherwise = (l1, l2, Black, l, t) : (colorPath xs edges c2)
                                           
isElem (l1, l2, c, l, t) edges c2 | elem (l1, l2, c2, l, t) edges= True
                                       | c2 == White = False
                                       | otherwise = isElem (l1, l2, c, l, t) edges (nextColor c2)
                                       
getShortest (x:xs) = getShortestHelper x xs
getShortestHelper l [] = l
getShortestHelper l (x:xs) | (length x) < (length l) = getShortestHelper x xs
                            | otherwise = getShortestHelper l xs
                                       
start5 = putStrLn (show $ test )
       where
            test = isElem ((edges beginGraph) !! 0) (edges beginGraph) Red
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
        
findEdgeFromNodeToNode2 :: Node -> Node -> Graph -> [Edge]
findEdgeFromNodeToNode2 n1 n2 g
    | null possibleEdges = []
    | otherwise = [head possibleEdges]
    where
        allEdges = edges g
        possibleEdges = filter (edgeRunsFromNodeToNode n1 n2) allEdges
        
findEdgeFromNodeToNodeBool :: Node -> Node -> Graph -> Bool
findEdgeFromNodeToNodeBool n1 n2 g
    | null possibleEdges = False
    | otherwise = True
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
        
eventloop ps@(ProgramState "c" (Just node1s) _ g d) _
    = (ProgramState [] Nothing Nothing g' d, [OutGraphs $ DrawGraph g', OutStdOut $ S.StdOutMessage $ "Renamed node '" ++ [oldL] ++ "' to '" ++ [oldL] ++ "'\n"])
    where
        allNodes = nodes g
        allEdges = edges g
        (oldL, p, color) = node1s
        node' = (oldL, p, Red)
        g'  = (flip addNode) node' $ removeNode node1s g
        
eventloop ps@(ProgramState "b" (Just node1s) _ g d) _
    = trace (show allOtherNodes) (ProgramState [] Nothing Nothing g' d, [OutGraphs $ DrawGraph g', OutStdOut $ S.StdOutMessage $ "test '\n"])
    where
        allNodes = nodes g
        allEdges = edges g
        (oldL, p, color) = node1s
        allChangeNodes = getAllNodes node1s allNodes g
        allOtherNodes = getAllOtherNodes node1s allNodes g
        allChangedNodes = changeColor allChangeNodes Blue
        allNodes' = allOtherNodes ++ allChangedNodes
        g' = g {nodes = allNodes'}
        
eventloop ps@(ProgramState "p" (Just node1s) _ g d) (InGraphs (Mouse (Click _) p))
    | nodeAtPosM ==   Nothing = (ps, [])
    | path =   (ProgramState [] Nothing Nothing g d, [OutGraphs $ DrawGraph g, OutStdOut $ S.StdOutMessage $ "there is a path '\n"])
    | otherwise =   (ProgramState [] Nothing Nothing g d, [OutGraphs $ DrawGraph g, OutStdOut $ S.StdOutMessage $ "there is no path'\n"])
    where
        (l1, _, _) = node1s
        (l2, _, _) = nodeAtPos
        nodeAtPosM = onNode allNodes p
        allNodes = nodes g
        (Just nodeAtPos) = nodeAtPosM
        path = findPath g nodeAtPos node1s
        

eventloop ps (InGraphs (Key "o"))
    | connected =   (ProgramState [] Nothing Nothing g (pathList ps), [OutGraphs $ DrawGraph g, OutStdOut $ S.StdOutMessage $ "this graph is connected '\n"])
    | otherwise =   (ProgramState [] Nothing Nothing g (pathList ps), [OutGraphs $ DrawGraph g, OutStdOut $ S.StdOutMessage $ "this graph is not connected'\n"])
    where   g = (graph ps)
            connected = stronglyConnected g
            
eventloop ps (InGraphs (Key "i"))
    =   (ProgramState [] Nothing Nothing g' (pathList ps), [OutGraphs $ DrawGraph g', OutStdOut $ S.StdOutMessage $ "the subgraphs should be colored'\n"])
    where   g = (graph ps)
            g' = g {nodes = colorSubGraphs (findSubGraphs g)}


                                  
eventloop ps@(ProgramState "u" _ _ g d) _
    = (ProgramState [] Nothing Nothing g' d, [OutGraphs $ DrawGraph g', OutStdOut $ S.StdOutMessage $ "works'\n"])
    where
        allNodes = nodes g
        allEdges = edges g
        allNodes' = changeColor allNodes Orange
        g' = g {nodes = allNodes'}
 

eventloop ps@(ProgramState "g" (Just node1s) _ g d) (InGraphs (Mouse (Click _) p))
    | nodeAtPosM ==   Nothing = (ps, [])
    | trace (show (colorPath (pl!!0) (edges g) Red)) otherwise =   (ProgramState [] Nothing Nothing g' newPathList, [OutGraphs $ DrawGraph g', OutStdOut $ S.StdOutMessage $ "yolo'\n"])
    where
        (l1, _, _) = node1s
        nodeAtPosM = onNode allNodes p
        allNodes = nodes g
        (Just nodeAtPos) = nodeAtPosM
        pl = (getEdgesList (findPaths g nodeAtPos node1s) g)
        g' = g{ edges= (colorPath  (edges g) (pl!!0) Red)}
        newPathList = (putElemLast pl)
        
eventloop ps (InGraphs (Key "h"))
    | path == [] = (ps, [])
    | trace (show path) otherwise = (ProgramState [] Nothing Nothing g' newPathList, [OutGraphs $ DrawGraph g', OutStdOut $ S.StdOutMessage $ "yolo'\n"])
        where 
            path = (pathList ps) !! 0
            newPathList = putElemLast (pathList ps)
            g = (graph ps)
            g' = g{ edges= (colorPath  (edges g) path Red)}
            
eventloop ps@(ProgramState "j" (Just node1s) _ g d) (InGraphs (Mouse (Click _) p))
    | nodeAtPosM ==   Nothing = (ps, [])
    | trace (show (colorPath (pl!!0) (edges g) Red)) otherwise =   (ProgramState [] Nothing Nothing g' newPathList, [OutGraphs $ DrawGraph g', OutStdOut $ S.StdOutMessage $ "yolo'\n"])
    where
        (l1, _, _) = node1s
        nodeAtPosM = onNode allNodes p
        allNodes = nodes g
        (Just nodeAtPos) = nodeAtPosM
        pl = (getEdgesList (findPaths g nodeAtPos node1s) g)
        g' = g{ edges= (colorPath  (edges g) (getShortest pl) Red)}
        newPathList = (putElemLast pl)





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