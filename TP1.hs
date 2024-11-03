import qualified Data.List
import qualified Data.Array
import qualified Data.Bits


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
-- Removes duplicates using the auxiliary function removeDuplicates
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
-- xs - A list of elements of type a, where a is a type that has an Ord instance
-- Returns:
-- A list of unique elements
-- Time Complexity : O(n log n) due to sorting, followed by O(n) for grouping
-- Space Complexity : O(n)

removeDuplicates :: Ord a => [a] -> [a]
removeDuplicates = map head . Data.List.group . Data.List.sort

-- | Checks if two cities are directly connected in the given roadmap.
-- Arguments:
--   roadmap - A list of tuples where each tuple represents a road connecting two cities and their distance.
--   c1 - The first city
--   c2 - The second city
-- Returns:
--   True if there is a direct road between c1 and c2, otherwise False.
-- Time Complexity : O(n)
-- Space Complexity : O(1)

areAdjacent :: RoadMap -> City -> City -> Bool
areAdjacent roadmap c1 c2 = any (\(city1,city2,_) -> (city1 == c1 && city2 == c2) || (city1 == c2 && city2 == c1)) roadmap

-- | Returns the distance between two cities if they are directly connected
-- Arguments:
--   roadmap - A list of tuples where each tuple represents a road connecting two cities and their distance
--   c1 - The first city
--   c2 - The second city
-- Returns:
--   Just the distance if the cities are directly connected, otherwise Nothing
-- Time Complexity : O(n)
-- Space Complexity : O(n)

distance :: RoadMap -> City -> City -> Maybe Distance
distance roadmap c1 c2 =
    case [distance | (city1, city2, distance) <- roadmap, (city1 == c1 && city2 == c2) || (city1 == c2 && city2 == c1)] of
    [] -> Nothing
    (distance:_) -> Just distance


-- | Returns a list of adjacent cities and distances from a given city
-- Arguments:
--   roadmap - A list of tuples where each tuple represents a road connecting two cities and their distance
--   city - The city whose adjacent cities we want to find
-- Returns:
--   A list of tuples where each tuple contains a city directly connected to the input city and the distance to it
-- Time Complexity: O(n) where n is the number of roads in the roadmap (due to the list traversal)
-- Space Complexity: O(a) where a is the number of adjacent cities

adjacent :: RoadMap -> City -> [(City,Distance)]
adjacent [] city=[]
adjacent rd city
    | c1==city = (c2,d) : adjacent (tail rd) city
    | c2==city = (c1,d) : adjacent (tail rd) city
    | otherwise = adjacent (tail rd) city
    where (c1,c2,d) = head rd

-- | Calculates the total distance of a given path through the roadmap
-- Arguments:
--   roadmap - A list of tuples where each tuple represents a road connecting two cities and their distance
--   path - A list of cities representing the path
-- Returns:
--   Just the total distance if the path is valid (i.e., all consecutive cities are connected), otherwise Nothing
-- Time Complexity: O(p * n), where p is the length of the path and n is the number of roads (for each pair lookup)
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

-- | Identifies all cities that have the maximum number of adjacent connections in the roadmap
-- This function counts how many adjacent cities each city has and returns a list of cities that have the highest number of connections
-- Arguments:
--   road - A list of tuples where each tuple represents a road connecting two cities and their distance
-- Returns:
--   A list of cities that have the highest number of adjacent connections
-- Time Complexity: O(n), where n is the number of cities in the roadmap
-- Space Complexity: O(n), for storing counts of adjacent connections

rome :: RoadMap -> [City]
rome road =
    let
        counts = [(city, countAdjacent road city) | city <- cities road]
        maxCount = maximum (map snd counts)
        maxCities = [city | (city, count) <- counts, count == maxCount]

    in maxCities

-- | Counts the number of cities adjacent to a given city in the roadmap
-- Arguments:
--   roadmap - A list of tuples where each tuple represents a road connecting two cities and their distance
--   city - The city for which adjacent connections are counted
-- Returns:
--   The total number of adjacent cities
-- Time Complexity: O(n), where n is the number of roads in the roadmap
-- Space Complexity: O(1)
countAdjacent :: RoadMap -> City -> Int
countAdjacent roadmap city =
    length [() | (c1,c2,_) <- roadmap, city `elem` [c1,c2]]

-- |Performs a BFS on the given roadmap starting from a specified city
-- This function returns a list of all the cities reachable from the start city
-- Arguments:
--  roadmap - A list of tuples where each tuple represents a road connecting two cities and their distance
-- startCity - The city from where the search begins
-- Returns:
--  A list of cities reachable from startCity without duplicates
-- Time Complexity: O(V + E), where V is the number of cities and E is the number of edges (roads)
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

-- | Checks if the given roadmap is strongly connected
-- A roadmap is strongly connected if all cities are reachable from any starting city
-- Arguments:
--   roadmap - A list of tuples where each tuple represents a road connecting two cities and their distance
-- Returns:
--   True if all cities are reachable from any starting city, otherwise False
-- Time Complexity: O(V + E), where V is the number of cities and E is the number of roads
-- Space Complexity: O(V)

isStronglyConnected :: RoadMap -> Bool
isStronglyConnected roadmap=
    let allCities = cities roadmap
    in (null allCities || (length allCities == length (bfs roadmap (head allCities))))


-- | Creates a list with all cities and their distance to the source
-- All distances start at 9999 except for the source city that starts at 0
-- Arguments:
--  road - The graph
--  city - The starting position
-- Time Complexity: O(V)
-- Space Complexity: O(V)
initializeDistToSourceList :: RoadMap -> City -> [(City,Distance)]
initializeDistToSourceList road city =  [(c,9999) | c<-cities road,c/=city] ++ [(city,0)]


-- | Adds and/or updates multiple tuples of (City,Distance) to a priority queue and ensures that the first element is the one closest to the source
-- Arguments:
--  pq - Priority queue that is going to receive/update values
--  newDist - List of tuples that are added to the priority queue or used to update it 
-- Time Complexity: O(M log M), where M is the size of the priority queue after adding/updating values, due to sorting the priority queue at the end
-- Space Complexity: O(M) for storing the updated priority queue
addPQueue :: PQueue -> [(City,Distance)] -> PQueue
addPQueue pq newDist = Data.List.sortOn snd ([if fst p `elem` map fst newDist then (fst p,snd (head (filter (\u->fst u == fst p) newDist))) else p | p <-pq ] ++ [nd | nd <- newDist , fst nd `notElem` map fst pq])

-- | Updates the list of distances to the source city
-- Arguments:
--  pq - List of cities and their current distances to the source
--  newV - Values used to update the distances in the list
-- Time Complexity: O(N^2), where N is the size of the priority queue, due to the use of `filter` within a list comprehension.
-- Space Complexity: O(1), as it produces a new list without additional storage.
updateDistToSourceList :: PQueue->[(City,Distance)] ->PQueue
updateDistToSourceList pq newV= [if fst p `elem` map fst newV then (fst p, snd (head (filter (\u->fst u == fst p) newV))) else p | p <-pq]


-- Creates an adjacency list for all the cities in a graph
-- Arguments:
-- road - The graph
-- cities - List of cities that belong to the graph
-- Time Complexity: O(V), where V is the number of cities
-- Space Complexity: O(V + E), where E is the number of edges, for storing the adjacency list
initializeAdjList :: RoadMap ->[City] -> AdjList
initializeAdjList road []=[]
initializeAdjList road cities= (head cities,adjacent road (head cities)) : initializeAdjList road (tail cities)

-- Gets a list of adjacent cities and their distances from a given adjacency list
-- Arguments:
--  adj - Adjacency list
--  c - The city for which to retrieve adjacent cities
-- Time Complexity: O(N), where N is the number of entries in the adjacency list for the given city
-- Space Complexity: O(1), as it returns a list of tuples without additional storage
getAdj :: AdjList->City -> [(City,Distance)]
getAdj adj c= snd (head (filter (\(a,b)->a==c) adj))

-- Uses the head of the priority queue and it's adjacent cities to see if their distance to the source can become smaller
-- Arguments:
--  pqueue - Priority queue
--  distsToSource - List of tuples with a city and it's distance to the source
--  adj - List of adjacents to the head of the pqueue
-- Returns a list with tuples of cities that can get a shorter distance to the source and their new distance, later used for the updateDistToSource and addPQueue functions
-- Time Complexity: O(N), where N is the number of adjacent cities.
-- Space Complexity: O(1), as it returns a new list without additional storage.
getNewDist :: PQueue -> [(City,Distance)]-> [(City,Distance)] -> [(City,Distance)]
getNewDist pqueue distsToSource []=[]
getNewDist pqueue distsToSource adj
    | distTosource >= distToAdj + adjToSource = (c,distToAdj + adjToSource) : getNewDist pqueue distsToSource (tail adj)
    | otherwise = getNewDist pqueue distsToSource (tail adj)
    where
        c = fst (head adj)
        distTosource = snd (head (filter (\(a,b)->a==c) distsToSource)) -- current distance to source of the adjacent being checked
        distToAdj = snd (head adj)
        adjToSource = snd (head pqueue)


-- Receives a prelude, a city (source), and a list of cities and uses them to update the prelude
-- Arguments:
--  prelude - Prelude to be updated
--  source - City that the other will point to
--  cities - Cities that will point to a new city or be added for the first time 
-- Time Complexity: O(N), where N is the size of the prelude
-- Space Complexity: O(M), where M is the number of cities being added to the prelude
updatePrelude :: Prelude -> City ->[City]->Prelude
updatePrelude prelude source cities = [if fst pr `elem` cities then (fst pr , source) else pr | pr <- prelude] ++ [(c,source) | c<-cities, c `notElem` map fst prelude]

-- Transforms a prelude into a path by receiving a destination, checking if it has a prelude and adding it to the path until dest == source
-- Arguments:
--  source - Starting city
--  dest - City currently being checked
--  prelude - Prelude that will be transformed
-- Time Complexity: O(N), where N is the length of the prelude.
-- Space Complexity: O(N), as it builds a new path list.
getPathFormPrelude:: City->City ->Prelude -> Path
getPathFormPrelude source dest prelude
    | source == dest =[]
    | dest `elem` map fst prelude =  prevCity : getPathFormPrelude source prevCity prelude
    | otherwise = []
    where
        prevCity = snd (head (filter (\u-> fst u == dest) prelude))

-- Auxiliary function for shortest path that uses the Dijkstra's algorithm to search for the shortest path
-- Arguments:
--  road - Graph
--  source - Starting city
--  dest - City the function is trying to reach
--  pqueque - Priority queue that always has the city closest to source as its head
--  distToSource - List with all the cities and their current distance to the source
--  adjList - List with all the cities and their adjacent cities
--  prelude - Prelude that is being constructed by the function
-- Returns a prelude that a path can be derived from
-- Time Complexity: O(E), where E is the number of edges in the graph
-- Space Complexity: O(V), where V is the number of vertices (cities) in the graph
shortestPath' :: RoadMap -> City -> City -> PQueue -> [(City,Distance)]-> AdjList -> Prelude -> Prelude
shortestPath' road source dest [] distToSource adjList prelude= prelude -- ends when the priority queue is empty witch means that no more cities can get better distances 
shortestPath' road source dest pqueue disToSource adjList prelude = shortestPath' road source dest updatedPqueue updatedDisToSource adjList updatedPrelude
    where
        city = fst (head pqueue) -- gets closes city to source
        adjs = getAdj adjList city -- adjacents of the closest city to source
        newDists = getNewDist pqueue disToSource adjs -- check if any of them can get a smaller distance
        updatedDisToSource = updateDistToSourceList disToSource newDists -- if so updates the disToSource list
        updatedPqueue = addPQueue (tail pqueue) newDists -- and the priority queue
        cities = map fst newDists -- gets the cities that get a better distance
        updatedPrelude = updatePrelude prelude city cities -- and updates the prelude with them

-- Returns the total distance of a path (only works if the path exists, aux for allPaths)
-- Arguments:
--  road - Graph
--  (x:y:xs) - Path
-- Time Complexity: O(N), where N is the length of the path.
-- Space Complexity: O(1), as it calculates the distance without additional storage.
pathDistanceInt :: RoadMap -> Path -> Distance
pathDistanceInt road [] = 0
pathDistanceInt road [_] = 0
pathDistanceInt road (x:y:xs) =  if x/=y then dist + pathDistanceInt road (y:xs) else 0 + pathDistanceInt road (y:xs)
    where
        dist=  head [distance | (city1, city2, distance) <- road, (city1 == x && city2 == y) || (city1 == y && city2 == x)]

-- Searches all paths and stops if it detects that the cost is already bigger than the minCost found by the Dijkstra's algorithm
-- Arguments:
--  road - Graph
--  adjList - List with all cities and their adjacent cities
--  minCost - Minimal cost for a path calculated by Dijkstra
--  source - Starting city / city being checked
--  dest - Destination city
--  visited - List that keeps track of cities alread visited
--  paths - List that contains all minimal cost paths
--  currentPath - Path being cheked
--  currentPathCost - Cost of the path being cheked
-- Returns a list of all paths that have the mininal cost and reach the destination
-- Time Complexity: O(V^2), where V is the number of vertices (cities)
-- Space Complexity: O(V), where V is the number of paths stored
allPaths:: RoadMap -> AdjList -> Distance -> City -> City -> [City] -> [Path] -> Path -> Distance -> [Path]
allPaths road adjList minCost source dest visited paths currentPath currentPathCost
    | source == dest = let pathCost =  pathDistanceInt road currentPath -- if city that is being checked equals dest calculate pathCost of currentPath
        in if pathCost == minCost then  reverse currentPath : paths else  paths -- if pathCost == minCost add currentPath to the paths list else return paths
    | otherwise =  foldl allPathsAux paths adjs -- continues searching for paths
    where


        adjs =  getAdj adjList source
        allPathsAux  paths (ad,dist)
            | ad `elem` visited = paths -- if adjacent as already been visited stop searching this path (avoids loops)
            | currentPathCost + dist > minCost = paths -- if the cost of the currentPath + the cost of adding the edge is bigger than minCost stop searching path
            | otherwise = allPaths road adjList minCost ad dest (ad:visited) paths (ad:currentPath) (currentPathCost+dist) -- continue searching path


-- Uses helper functions shortestPath' and allPaths to get a list of all paths with minimal cost
-- Arguments:
--  road - Graph
--  source - Starting city
--  dest - Ending city
-- Returns a list of paths with minimal cost
-- Time Complexity: O(V^2), where V is the number of vertices (cities)
-- Space Complexity: O(V), where V is the number of paths stored
shortestPath :: RoadMap -> City -> City -> [Path]
shortestPath road source dest
    | source==dest = [[source]] -- shortest path between a city and itself
    | otherwise= if null dijkstraSP then [] else paths
    where
        pqueue = [(source,0)] -- start priority queue used by the dijstra algorithm
        distToSource = initializeDistToSourceList road source
        adjacentList = initializeAdjList road (cities road)
        prelude = shortestPath' road source dest pqueue distToSource adjacentList [] -- uses dijstra's algorithm to calculete a shortest path
        dijkstraSP = getPathFormPrelude source dest prelude -- turns the prelude into a path
        dijkstraDistance = pathDistanceInt road (dest : dijkstraSP) -- gets cost of the path calculeted by dijstra's
        paths= allPaths road adjacentList dijkstraDistance source dest [source] [] [source] 0 -- calls allPath with source already visited and in the current path


-- | This function sorts tuples by their first element in ascending order and returns the smallest tuple.
-- Arguments:
--   A list of tuples, where each tuple's first element is of an Ord type.
-- Returns:
--   The tuple with the smallest first element.
-- Time Complexity: O(n) for finding the minimum in an unsorted list.
-- Space Complexity: O(1)

minByFirst :: Ord a => [(a, b)] -> (a, b)
minByFirst = Data.List.minimumBy (\(a, _) (b, _) -> compare a b)

-- | Solves the Traveling Salesperson Problem (TSP) for the given roadmap.
-- This implementation uses Dynamic Programming and Bitmasking to find he shortest path
-- that visits each city exactly once and returns to the starting city.
-- The algorithm utilizes a memoization table to store the results of subproblems, which helps avoid redundant computations.
-- Each entry in the memo table corresponds to a pair of a bitmask (representing the set of visited cities) and the current city,
-- allowing the algorithm to efficiently compute the minimum cost of visiting all cities by incrementally 
-- building solutions from smaller subproblems.
-- Arguments:
--   roadmap - A list of tuples where each tuple represents a road connecting two cities and their distance.
-- Returns:
--   A list of cities representing the shortest path that visits each city.
-- Time Complexity: O(n * 2^n), where n is the number of cities.
-- Space Complexity: O(2^n) for memoization storage.

travelSales :: RoadMap -> Path
travelSales roadmap =
    let
        allCities = cities roadmap
        n = length allCities
        maxBitmask = 2^n - 1
        memo = Data.Array.array ((0, 0), (maxBitmask, n - 1)) [((bitmask, i), Nothing) | bitmask <- [0..maxBitmask], i <- [0..n - 1]]

        tsp :: Int -> Int -> Data.Array.Array (Int, Int) (Maybe (Distance, Path)) -> (Maybe (Distance, Path), Data.Array.Array (Int, Int) (Maybe (Distance, Path)))
        tsp bitmask pos memoArr
            | bitmask == maxBitmask = 
                let returnDist = distance roadmap (allCities !! pos) (allCities !! 0)
                in case returnDist of
                    Just dist -> (Just (dist, [allCities !! pos, allCities !! 0]), memoArr)  
                    Nothing -> (Nothing, memoArr)
            | otherwise =
                case memoArr Data.Array.! (bitmask, pos) of
                    Just result -> (Just result, memoArr)
                    Nothing ->
                        let possiblePaths = [(d + nextDist, allCities !! pos : nextPath) |
                                               next <- [0 .. n - 1],
                                               next /= pos,
                                               (bitmask Data.Bits..&. (1 `Data.Bits.shiftL` next)) == 0,
                                               let (nextResult, updatedMemo)= tsp(bitmask Data.Bits..|. (1 `Data.Bits.shiftL` next)) next memoArr,
                                               Just d <- [distance roadmap (allCities !! pos) (allCities !! next)],
                                               Just (nextDist, nextPath) <- [nextResult]]
                            minPath = if null possiblePaths then Nothing else Just (minByFirst possiblePaths)
                            updatedMemo = memoArr Data.Array.// [((bitmask, pos), minPath)]
                        in (minPath, updatedMemo)

    in case tsp 1 0 memo of
        (Nothing, _) -> []
        (Just (_, path), _) -> path

tspBruteForce :: RoadMap -> Path
tspBruteForce = undefined -- only for groups of 3 people; groups of 2 people: do not edit this function

-- Some graphs to test your work
gTest1 :: RoadMap
gTest1 = [("7","6",1),("8","2",2),("6","5",2),("0","1",4),("2","5",4),("8","6",6),("2","3",7),("7","8",7),("0","7",8),("1","2",8),("3","4",9),("5","4",10),("1","7",11),("3","5",14)]

gTest2 :: RoadMap
gTest2 = [("0","1",10),("0","2",15),("0","3",20),("1","2",35),("1","3",25),("2","3",30)]

gTest3 :: RoadMap -- unconnected graph
gTest3 = [("0","1",4),("2","3",2)]
