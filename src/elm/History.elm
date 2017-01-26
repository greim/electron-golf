module History exposing (History, push, pop, unpop, peek, init, hasItems, length, hasFuture, futureLength, break)

import Set exposing (Set)
--import Debug exposing (log)

type alias History a =
  { first : a
  , past : List (String, a)
  , future : List (String, a)
  }

init : a -> History a
init item =
  History item [] []

push : String -> a -> History a -> History a
push key item history =
  --let k = log "key" key in
  case history.past of
    (prevKey, prevItem) :: rest ->
      let
        newPast = case Set.member (prevKey, key) collapsibles of
          True -> (key, item) :: rest
          False -> (key, item) :: history.past
      in
        { history | past = newPast, future = [] }
    [] ->
      let
        newPast = (key, item) :: history.past
      in
        { history | past = newPast, future = [] }

break : History a -> History a
break history =
  case history.past of
    (prevKey, prevItem) :: rest ->
      let newPast = ("", prevItem) :: rest
      in { history | past = newPast }
    [] ->
      history

pop : History a -> History a
pop history =
  case history.past of
    first :: rest ->
      { history | past = rest, future = first :: history.future }
    [] ->
      history

unpop : History a -> History a
unpop history =
  case history.future of
    first :: newFuture ->
      let
        newPast = first :: history.past
      in
        { history | past = newPast, future = newFuture }
    [] ->
      history

peek : History a -> a
peek history =
  case List.head history.past of
    Just (key, item) ->
      item
    Nothing ->
      history.first

hasItems : History a -> Bool
hasItems history =
  List.isEmpty history.past |> not

length : History a -> Int
length history =
  List.length history.past

hasFuture : History a -> Bool
hasFuture history =
  List.isEmpty history.future |> not

futureLength : History a -> Int
futureLength history =
  List.length history.future

collapsibles : Set (String, String)
collapsibles =
  Set.fromList
    [ ("start-nothing", "end-nothing")
    , ("start-nothing", "end-node")
    , ("start-node", "end-node")
    , ("start-node", "end-nothing")
    , ("move", "move")
    , ("unmark", "mark")
    , ("mark", "mark")
    , ("mark", "sweep")
    , ("sweep", "sweep")
    , ("sweep", "done")
    , ("label", "label")
    ]
