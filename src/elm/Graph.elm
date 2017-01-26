module Graph exposing (Graph, empty, singleton, addNode, updateNode, updateNodeFn, getNode, removeNode, toNodeList, toEdgeList, addEdge, removeEdge, findConnected, map, updateEdge, getEdge)
import Dict exposing (Dict)
import Set exposing (Set)
import Queue exposing (Queue)

type alias Graph a b =
  { nodes : Dict Int a
  , edges : Dict Int (Dict Int b)
  , nextId : Int
  }

empty : Graph a b
empty =
  Graph (Dict.empty) (Dict.empty) 1


singleton : a -> Graph a b
singleton a =
  Graph (Dict.singleton 0 a) (Dict.empty) 1


addNode : a -> Graph a b -> (Int, Graph a b)
addNode a graph =
  let
    id = graph.nextId
    newNodes = Dict.insert id a graph.nodes
    newGraph = { graph | nodes = newNodes, nextId = id + 1 }
  in
    (id, newGraph)


updateNode : Int -> a -> Graph a b -> Graph a b
updateNode id node graph =
  case Dict.get id graph.nodes of
    Just oldNode ->
      let
        newNodes = Dict.insert id node graph.nodes
      in
        { graph | nodes = newNodes }
    Nothing ->
      graph


updateNodeFn : (a -> a) -> Int -> Graph a b -> Graph a b
updateNodeFn update id graph =
  case getNode id graph of
    Just node ->
      updateNode id (update node) graph
    Nothing ->
      graph


getNode : Int -> Graph a b -> Maybe a
getNode id graph =
  Dict.get id graph.nodes


removeNode : Int -> Graph a b -> Graph a b
removeNode id graph =
  let
    newNodes = Dict.remove id graph.nodes
    filterFn = (\anId b -> id /= anId)
    mapFn = (\from tos -> Dict.filter filterFn tos)
    truncEdges = Dict.remove id graph.edges
    newEdges = Dict.map mapFn truncEdges
  in
    { graph | nodes = newNodes, edges = newEdges }


toNodeList : Graph a b -> List (Int, a)
toNodeList graph =
  Dict.toList graph.nodes


map : (a -> a) -> Graph a b -> Graph a b
map mapFun graph =
  let
    newMapFun = (\id a -> mapFun a)
    newNodes = Dict.map newMapFun graph.nodes
  in
    { graph | nodes = newNodes }


toPairs : Int -> Dict Int b -> List (Int, b, Int)
toPairs from tos =
  let
    tosList = Dict.toList tos
    mapFn = (\(to, b) -> (from, b, to))
  in
    List.map mapFn tosList


toEdgeList : Graph a b -> List (Int, b, Int)
toEdgeList graph =
  let
    rawList = Dict.toList graph.edges
    foldFn = (\(from, tos) pairs -> List.append pairs (toPairs from tos))
  in
    List.foldl foldFn [] rawList


addEdge : Int -> b -> Int -> Graph a b -> Graph a b
addEdge from val to graph =
  case (Dict.get from graph.nodes, Dict.get to graph.nodes) of
    (Just f, Just t) ->
      let
        newTos = case Dict.get from graph.edges of
          Just tos -> Dict.insert to val tos
          Nothing -> Dict.singleton to val
        newEdges = Dict.insert from newTos graph.edges
      in
        { graph | edges = newEdges }
    _ ->
      graph


updateEdge : Int -> b -> Int -> Graph a b -> Graph a b
updateEdge from b to graph =
  case Dict.get from graph.edges of
    Just tos ->
      case Dict.get to tos of
        Just oldB ->
          let
            newTos = Dict.insert to b tos
            newEdges = Dict.insert from newTos graph.edges
          in
            { graph | edges = newEdges }

        Nothing ->
          graph
    Nothing ->
      graph


getEdge : Int -> Int -> Graph a b -> Maybe b
getEdge from to graph =
  case Dict.get from graph.edges of
    Just tos ->
      Dict.get to tos
    Nothing ->
      Nothing


removeEdge : Int -> Int -> Graph a b -> Graph a b
removeEdge from to graph =
  case Dict.get from graph.edges of
    Just tos ->
      let
        newTos = Dict.remove to tos
        newEdges = Dict.insert from newTos graph.edges
      in
        { graph | edges = newEdges }
    Nothing ->
      graph


findConnected : (Int -> b -> Int -> Bool) -> Int -> Graph a b -> Set Int
findConnected filterFn id graph =
  let
    queue = Queue.singleton id
    results = Set.singleton id
  in
    findConnectedUgly filterFn queue results graph


findConnectedUgly : (Int -> b -> Int -> Bool) -> Queue Int -> Set Int -> Graph a b -> Set Int
findConnectedUgly filterFn queue results graph =
  case Queue.deq queue of
    (Just from, smallerQueue) ->
      let
        tosDict = case Dict.get from graph.edges of
          Nothing -> Dict.empty
          Just tosDict -> tosDict
        filteredTosDict = Dict.filter (\to a -> (filterFn from a to)) tosDict
        tos = Set.fromList (Dict.keys filteredTosDict)
        unvisitedTos = Set.diff tos results
        newResults = Set.union results unvisitedTos
        listTos = Set.toList unvisitedTos
        newQueue = Queue.enqAll listTos smallerQueue
      in
        findConnectedUgly filterFn newQueue newResults graph
    (Nothing, sameQueue) ->
      results
