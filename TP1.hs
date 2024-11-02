import qualified Data.List
import qualified Data.Array
import qualified Data.Bits
import Debug.Trace

-- PFL 2024/2025 Practical assignment 1

-- Uncomment the some/all of the first three lines to import the modules, do not change the code of these lines.

type City = String
type Path = [City]
type Distance = Int

type RoadMap = [(City,City,Distance)]

type AdjList = [(City,[(City,Distance)])]
type Prelude = [(City,City)]
type PQueue = [(City,Distance)]

-- | Returns a list of all cities present in the given roadmap
-- It removes duplicates using the auxiliary function removeDuplicates
-- Arguments:
--   roadmap - A list of tuples where each tuple represents a road connecting two cities and their distance
-- Returns:
--   A list of cities present in the roadmap, without duplicates
-- Time Complexity: O(n log n)
-- Space Complexity: O(n)

cities :: RoadMap -> [City]
cities roadmap = removeDuplicates [city | (city1, city2, _) <- roadmap, city <- [city1, city2]]

-- | Auxiliary function to remove duplicates from a list using sorting and grouping
-- This functions assumes that the elements are of an Ord type to enable sorting
-- Arguments:
-- A list of elements of type a, where a is a type that has an Ord instance
-- Returns:
-- A list of unique elements
-- Time Complexity : O(n log n) due to sorting, followed by O(n) for grouping
-- Space Complexity : O(n)

removeDuplicates :: Ord a => [a] -> [a]
removeDuplicates = map head . Data.List.group . Data.List.sort

-- | Checks if two cities are directly connected in the given roadmap.
-- Arguments:
--   roadmap - A list of tuples representing the roadmap, where each tuple contains two cities and their distance.
--   c1 - The first city.
--   c2 - The second city.
-- Returns:
--   True if there is a direct road between c1 and c2, otherwise False.
-- Time Complexity : O(n)
-- Space Complexity : O(1)

areAdjacent :: RoadMap -> City -> City -> Bool
areAdjacent roadmap c1 c2 = any (\(city1,city2,_) -> (city1 == c1 && city2 == c2) || (city1 == c2 && city2 == c1)) roadmap

-- | Returns the distance between two cities if they are directly connected.
-- Arguments:
--   roadmap - A list of tuples representing the roadmap, where each tuple contains two cities and their distance.
--   c1 - The first city.
--   c2 - The second city.
-- Returns:
--   Just the distance if the cities are directly connected, otherwise Nothing.
-- Time Complexity : O(n)
-- Space Complexity : O(n)

distance :: RoadMap -> City -> City -> Maybe Distance
distance roadmap c1 c2 =
    case [distance | (city1, city2, distance) <- roadmap, (city1 == c1 && city2 == c2) || (city1 == c2 && city2 == c1)] of
    [] -> Nothing
    (distance:_) -> Just distance


-- | Returns a list of adjacent cities and distances from a given city.
-- Arguments:
--   roadmap - A list of tuples where each tuple represents a road connecting two cities and their distance.
--   city - The city whose adjacent cities we want to find.
-- Returns:
--   A list of tuples where each tuple contains a city directly connected to the input city and the distance to it.
-- Time Complexity: O(n) where n is the number of roads in the roadmap (due to the list traversal).
-- Space Complexity: O(a) where a is the number of adjacent cities.

adjacent :: RoadMap -> City -> [(City,Distance)]
--adjacent road city= [(c, d) | (c1,c,d) <- road, c1==city] ++ [(c1, d) | (c1,c,d) <- road, c==city]
adjacent [] city=[]
adjacent rd city
    | c1==city = (c2,d) : adjacent (tail rd) city
    | c2==city = (c1,d) : adjacent (tail rd) city
    | otherwise = adjacent (tail rd) city
    where (c1,c2,d) = head rd

-- | Calculates the total distance of a given path through the roadmap.
-- Arguments:
--   roadmap - A list of tuples where each tuple represents a road connecting two cities and their distance.
--   path - A list of cities representing the path.
-- Returns:
--   Just the total distance if the path is valid (i.e., all consecutive cities are connected), otherwise Nothing.
-- Time Complexity: O(p * n), where p is the length of the path and n is the number of roads (for each pair lookup).
-- Space Complexity: O(1) 

pathDistance :: RoadMap -> Path -> Maybe Distance
pathDistance road [] = Nothing
pathDistance road [_] = Just 0
pathDistance road (x:y:xs) =
        case distance road x y of
        Nothing -> Nothing
        Just dis -> case pathDistance road (y:xs) of
            Nothing->Nothing
            Just tdis -> Just (dis + tdis)

-- | Identifies all cities that have the maximum number of adjacent connections in the roadmap.
-- This function counts how many adjacent cities each city has and returns a list of cities that have the highest number of connections.
-- Arguments:
--   road - A list of tuples representing the roadmap, where each tuple contains two cities and their distance.
-- Returns:
--   A list of cities that have the highest number of adjacent connections.
-- Time Complexity: O(n), where n is the number of cities in the roadmap.
-- Space Complexity: O(n), for storing counts of adjacent connections.

rome :: RoadMap -> [City]
rome road =
    let
        counts = [(city, countAdjacent road city) | city <- cities road]
        maxCount = maximum (map snd counts)
        maxCities = [city | (city, count) <- counts, count == maxCount]

    in maxCities

-- | Counts the number of cities adjacent to a given city in the roadmap.
-- Arguments:
--   roadmap - A list of tuples representing the roadmap, where each tuple contains two cities and their distance.
--   city - The city for which adjacent connections are counted.
-- Returns:
--   The total number of adjacent cities.
-- Time Complexity: O(n), where n is the number of roads in the roadmap.
-- Space Complexity: O(1)
countAdjacent :: RoadMap -> City -> Int
countAdjacent roadmap city =
    length [() | (c1,c2,_) <- roadmap, city `elem` [c1,c2]]

-- |Performs a BFS on the given roadmap starting from a specified city.
-- This function returns a list of all the cities reachable from the start city.
-- Arguments:
--  roadmap - A list of tuples where each tuple represents a road connecting two cities and their distance.
-- startCity - The city from where the search begins.
-- Returns:
--  A list of cities reachable from startCity without duplicates.
-- Time Complexity: O(V + E), where V is the number of cities and E is the number of edges (roads).
-- Space Complexity: O(V)

bfs :: RoadMap -> City -> [City]
bfs roadmap startCity = bfs' [startCity] []
    where
        bfs' [] visited = visited
        bfs' (x:xs) visited
            | x `elem` visited = bfs' xs visited
            | otherwise =  bfs' (xs ++ unvisitedNeighbors) (x : visited)
            where
                unvisitedNeighbors = [neighbor | (neighbor, _) <- adjacent roadmap x, neighbor `notElem` visited]

-- | Checks if the given roadmap is strongly connected.
-- A roadmap is strongly connected if all cities are reachable from any starting city.
-- Arguments:
--   roadmap - A list of tuples where each tuple represents a road connecting two cities and their distance.
-- Returns:
--   True if all cities are reachable from any starting city, otherwise False.
-- Time Complexity: O(V + E), where V is the number of cities and E is the number of roads.
-- Space Complexity: O(V)

isStronglyConnected :: RoadMap -> Bool
isStronglyConnected roadmap=
    let allCities = cities roadmap
    in (null allCities || (length allCities == length (bfs roadmap (head allCities))))

initializePQueue :: RoadMap -> City -> [(City,Distance)]
initializePQueue road city = Data.List.sortOn snd ([(c,9999) | c<-cities road,c/=city] ++ [(city,0)])

addPQueue :: PQueue -> [(City,Distance)] -> PQueue
addPQueue pq newDist = Data.List.sortOn snd ([if fst p `elem` map fst newDist then (fst p,snd (head (filter (\u->fst u == fst p) newDist))) else p | p <-pq ] ++ [nd | nd <- newDist , fst nd `notElem` map fst pq])

updatePQueue :: PQueue->[(City,Distance)] ->PQueue
updatePQueue pq newV= Data.List.sortOn snd ([if fst p `elem` map fst newV then (fst p, snd (head (filter (\u->fst u == fst p) newV))) else p | p <-pq])

initializeAdjList :: RoadMap ->[City] -> AdjList
initializeAdjList road []=[]
initializeAdjList road cities= (head cities,adjacent road (head cities)) : initializeAdjList road (tail cities)

getAdj :: AdjList->City -> [(City,Distance)]
getAdj adj c= snd (head (filter (\(a,b)->a==c) adj))

getNewDist :: PQueue -> [(City,Distance)]-> [(City,Distance)] -> [(City,Distance)]
getNewDist pqueue distsToSource []=[]
getNewDist pqueue distsToSource adj
    | distTosource >= distToAdj + adjToSource = (c,distToAdj + adjToSource) : getNewDist pqueue distsToSource (tail adj)
    | otherwise = getNewDist pqueue distsToSource (tail adj)
    where
        c = fst (head adj)
        distTosource = snd (head (filter (\(a,b)->a==c) distsToSource))
        distToAdj = snd (head adj)
        adjToSource = snd (head pqueue)


updatePrelude :: Prelude -> City ->[City]->Prelude
updatePrelude prelude source cities = [if fst pr `elem` cities then (fst pr , source) else pr | pr <- prelude] ++ [(c,source) | c<-cities, c `notElem` map fst prelude]

getPathFormPrelude:: City->City ->Prelude -> Path
getPathFormPrelude source dest prelude
    | source == dest =[]
    | dest `elem` map fst prelude =  prevCity : getPathFormPrelude source prevCity prelude
    | otherwise = []
    where
        prevCity = snd (head (filter (\u-> fst u == dest) prelude))

shortestPath' :: RoadMap -> City -> City -> PQueue -> [(City,Distance)]-> AdjList -> Prelude -> Prelude

shortestPath' road source dest [] distToSource adjList prelude= prelude

shortestPath' road source dest pqueue disToSource adjList prelude = shortestPath' road source dest updatedPqueue updatedDisToSource adjList updatedPrelude
    where
        city = fst (head pqueue)
        adjs = getAdj adjList city
        newDists = getNewDist pqueue disToSource adjs
        updatedDisToSource = updatePQueue disToSource newDists
        updatedPqueue = addPQueue (tail pqueue) newDists
        cities = map fst newDists
        updatedPrelude = updatePrelude prelude city cities

pathDistanceInt :: RoadMap -> Path -> Distance
pathDistanceInt road [] = 0
pathDistanceInt road [_] = 0
pathDistanceInt road (x:y:xs) =  if x/=y then dist + pathDistanceInt road (y:xs) else 0 + pathDistanceInt road (y:xs)
    where
        dist=  head [distance | (city1, city2, distance) <- road, (city1 == x && city2 == y) || (city1 == y && city2 == x)]

allPaths:: RoadMap -> AdjList -> Distance -> City -> City -> [City] -> [Path] -> Path -> Distance -> [Path]
allPaths road adjList minCost source dest visited paths currentPath currentPathCost
    | source == dest = let pathCost =  pathDistanceInt road currentPath
        in if pathCost == minCost then  reverse currentPath : paths else  paths
    | otherwise =  foldl allPathsAux paths adjs
    where
        
        
        adjs =  getAdj adjList source 
        allPathsAux  paths (ad,dist)
            | ad `elem` visited = paths
            | currentPathCost + dist > minCost = paths
            | otherwise = allPaths road adjList minCost ad dest (ad:visited) paths (ad:currentPath) (currentPathCost+dist)


shortestPath :: RoadMap -> City -> City -> [Path]
shortestPath road source dest
    | source==dest = [[source]]
    | otherwise= if null dijkstraSP then [] else paths
    where
        pqueue = [(source,0)]
        distToSource = initializePQueue road source
        adjacentList = initializeAdjList road (cities road)
        prelude = shortestPath' road source dest pqueue distToSource adjacentList []
        dijkstraSP = getPathFormPrelude source dest prelude
        dijkstraDistance = pathDistanceInt road (dest : dijkstraSP)
        paths= allPaths road adjacentList dijkstraDistance source dest [source] [] [source] 0


travelSales :: RoadMap -> Path
travelSales = undefined

tspBruteForce :: RoadMap -> Path
tspBruteForce = undefined -- only for groups of 3 people; groups of 2 people: do not edit this function

-- Some graphs to test your work
gTest1 :: RoadMap
gTest1 = [("7","6",1),("8","2",2),("6","5",2),("0","1",4),("2","5",4),("8","6",6),("2","3",7),("7","8",7),("0","7",8),("1","2",8),("3","4",9),("5","4",10),("1","7",11),("3","5",14)]

gTest2 :: RoadMap
gTest2 = [("0","1",10),("0","2",15),("0","3",20),("1","2",35),("1","3",25),("2","3",30)]

gTest3 :: RoadMap -- unconnected graph
gTest3 = [("0","1",4),("2","3",2)]
