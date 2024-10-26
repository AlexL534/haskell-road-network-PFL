import qualified Data.List
import qualified Data.Array
import qualified Data.Bits

-- PFL 2024/2025 Practical assignment 1

-- Uncomment the some/all of the first three lines to import the modules, do not change the code of these lines.

type City = String
type Path = [City]
type Distance = Int

type RoadMap = [(City,City,Distance)]

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

-- | Finds the maximum number in a list of ordered elements.
-- Arguments:
--   A list of elements of a type that has an Ord instance.
-- Returns:
--   The maximum element in the list.
-- Time Complexity: O(n)
-- Space Complexity: O(1)

maxNum :: Ord a=> [a]->a
maxNum [x]=x
maxNum (x:y:xs) = maxNum (max x y:xs)

-- | Finds the maximum number of adjacent cities any city has in the given roadmap.
-- Arguments:
--   roadmap - A list of tuples where each tuple represents a road connecting two cities and their distance.
-- Returns:
--   The maximum number of adjacent cities for any city in the roadmap.
-- Time Complexity: O(n^2), where n is the number of roads, due to multiple calls to the 'adjacent' function.
-- Space Complexity: O(n) (for storing adjacency lists during computation).

maxAdj :: RoadMap -> Int
maxAdj road= maxNum ([length (adjacent road c) | (c,c2,d)<-road] ++ [length (adjacent road c2) | (c,c2,d)<-road])

-- | Identifies all cities that have the maximum number of adjacent connections in the roadmap.
-- Arguments:
--   roadmap - A list of tuples where each tuple represents a road connecting two cities and their distance.
-- Returns:
--   A list of cities that have the highest number of adjacent connections.
-- Time Complexity: O(n^2), where n is the number of roads, due to multiple calls to the 'adjacent' function.
-- Space Complexity: O(n), for storing the resulting list of cities.

rome :: RoadMap -> [City]
rome road= Data.List.nub ( [c | (c,c2,d)<- road, length (adjacent road c)==maxAdj road] ++ [c2 | (c,c2,d)<- road, length (adjacent road c2)==maxAdj road])

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

shortestPath :: RoadMap -> City -> City -> [Path]
shortestPath = undefined

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
