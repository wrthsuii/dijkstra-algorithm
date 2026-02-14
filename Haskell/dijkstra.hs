
import Data.Map (Map)
import qualified Data.Map as Map
import Data.Set (Set)
import qualified Data.Set as Set
import Data.List (minimumBy)
import Data.Ord (comparing)
import Data.Maybe (fromMaybe)

-- Data types
type Vertex = String
type Weight = Int
type Graph = Map Vertex [(Vertex, Weight)]
type DistanceMap = Map Vertex Weight
type PredecessorMap = Map Vertex Vertex

intMaxValue :: Weight
intMaxValue = maxBound

-- Get distance to vertex from map, return infinity if not found
getDistance :: DistanceMap -> Vertex -> Weight
getDistance distanceMap vertex = 
    fromMaybe intMaxValue (Map.lookup vertex distanceMap)

-- Set distance to vertex in map
setDistance :: Vertex -> Weight -> DistanceMap -> DistanceMap
setDistance = Map.insert

-- Set predecessor for vertex in map
setPredecessor :: Vertex -> Vertex -> PredecessorMap -> PredecessorMap
setPredecessor = Map.insert

-- Get predecessor of vertex from map
getPredecessor :: PredecessorMap -> Vertex -> Maybe Vertex
getPredecessor = flip Map.lookup

-- Get neighbors of a vertex
getNeighbors :: Graph -> Vertex -> [(Vertex, Weight)]
getNeighbors graph vertex = fromMaybe [] (Map.lookup vertex graph)

-- Get all vertices from graph as list
getAllVertices :: Graph -> [Vertex]
getAllVertices = Map.keys

-- Initialize distances: start vertex = 0, all others = infinity
initializeDistances :: Graph -> Vertex -> DistanceMap
initializeDistances graph startVertex = 
    let allVertices = getAllVertices graph
        setInfinity vertex = (vertex, intMaxValue)
        infinityPairs = map setInfinity allVertices
        initialMap = Map.fromList infinityPairs
    in setDistance startVertex 0 initialMap

-- Find vertex with minimum distance from a list of vertices
findMinDistanceVertex :: DistanceMap -> [Vertex] -> Vertex
findMinDistanceVertex distanceMap vertices = 
    minimumBy (comparing (getDistance distanceMap)) vertices

-- Check if vertex is unreachable (distance is infinity)
isUnreachable :: DistanceMap -> Vertex -> Bool
isUnreachable distanceMap vertex = 
    getDistance distanceMap vertex == intMaxValue

-- Remove vertex from list
removeVertex :: Vertex -> [Vertex] -> [Vertex]
removeVertex vertexToRemove vertices = 
    filter (/= vertexToRemove) vertices

-- Calculate new distance through current vertex
calculateNewDistance :: Weight -> Weight -> Weight
calculateNewDistance currentDistance edgeWeight = 
    currentDistance + edgeWeight

-- Edge relaxation: update distance to neighbor if a shorter path is found
relaxEdge :: Vertex -> Weight -> (Vertex, Weight) -> (DistanceMap, PredecessorMap) -> (DistanceMap, PredecessorMap)
relaxEdge currentVertex currentDistance (neighborVertex, edgeWeight) (distanceMap, predecessorMap) =
    let oldDistance = getDistance distanceMap neighborVertex
        newDistance = calculateNewDistance currentDistance edgeWeight
        bIsNewPathShorter = newDistance < oldDistance
    in if bIsNewPathShorter
       then ( setDistance neighborVertex newDistance distanceMap
            , setPredecessor neighborVertex currentVertex predecessorMap
            )
       else (distanceMap, predecessorMap)

-- Relax all neighbors of the current vertex
relaxAllNeighbors :: Graph -> Vertex -> Weight -> (DistanceMap, PredecessorMap) -> (DistanceMap, PredecessorMap)
relaxAllNeighbors graph currentVertex currentDistance (distanceMap, predecessorMap) =
    let neighbors = getNeighbors graph currentVertex
        relaxOne neighbor state = relaxEdge currentVertex currentDistance neighbor state
    in foldr relaxOne (distanceMap, predecessorMap) neighbors

-- Main loop of Dijkstra's algorithm
dijkstraLoop :: Graph -> DistanceMap -> PredecessorMap -> [Vertex] -> (DistanceMap, PredecessorMap)
dijkstraLoop graph distanceMap predecessorMap unvisitedList
    | null unvisitedList = (distanceMap, predecessorMap)
    | otherwise =
        let currentVertex = findMinDistanceVertex distanceMap unvisitedList
            currentDistance = getDistance distanceMap currentVertex
            bAllRemainingUnreachable = isUnreachable distanceMap currentVertex
        in if bAllRemainingUnreachable
           then (distanceMap, predecessorMap)
           else
               let remainingVertices = removeVertex currentVertex unvisitedList
                   (newDistanceMap, newPredecessorMap) = 
                       relaxAllNeighbors graph currentVertex currentDistance (distanceMap, predecessorMap)
               in dijkstraLoop graph newDistanceMap newPredecessorMap remainingVertices

-- Entry point for Dijkstra's algorithm
dijkstra :: Graph -> Vertex -> (DistanceMap, PredecessorMap)
dijkstra graph startVertex =
    let initialDistanceMap = initializeDistances graph startVertex
        initialPredecessorMap = Map.empty
        allVertices = getAllVertices graph
    in dijkstraLoop graph initialDistanceMap initialPredecessorMap allVertices

-- Build path backward from end to start
buildPathBackward :: PredecessorMap -> Vertex -> Vertex -> [Vertex]
buildPathBackward predecessorMap startVertex currentVertex
    | currentVertex == startVertex = [startVertex]
    | otherwise = 
        case getPredecessor predecessorMap currentVertex of
            Just predecessor -> currentVertex : buildPathBackward predecessorMap startVertex predecessor
            Nothing -> []

-- Reconstruct path from start to end vertex
reconstructPath :: PredecessorMap -> Vertex -> Vertex -> [Vertex]
reconstructPath predecessorMap startVertex endVertex = 
    let backwardPath = buildPathBackward predecessorMap startVertex endVertex
    in reverse backwardPath

-- Find shortest path with total distance
shortestPath :: Graph -> Vertex -> Vertex -> Maybe ([Vertex], Weight)
shortestPath graph startVertex endVertex =
    let (distanceMap, predecessorMap) = dijkstra graph startVertex
        pathVertices = reconstructPath predecessorMap startVertex endVertex
        totalDistance = getDistance distanceMap endVertex
        bPathNotFound = totalDistance == intMaxValue || null pathVertices
    in if bPathNotFound
       then Nothing
       else Just (pathVertices, totalDistance)

-- Example graph
exampleGraph :: Graph
exampleGraph = Map.fromList
    [ ("A", [("B", 4), ("C", 2)])
    , ("B", [("A", 4), ("C", 1), ("D", 5)])
    , ("C", [("A", 2), ("B", 1), ("D", 8), ("E", 10)])
    , ("D", [("B", 5), ("C", 8), ("E", 2)])
    , ("E", [("C", 10), ("D", 2)])
    ]

main :: IO ()
main = do
    case shortestPath exampleGraph "A" "E" of
        Just (pathVertices, totalDistance) -> do
            putStrLn $ "Path from A to E: " ++ show pathVertices
            putStrLn $ "Distance: " ++ show totalDistance
        Nothing ->
            putStrLn "Path not found"