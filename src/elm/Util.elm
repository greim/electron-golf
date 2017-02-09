module Util exposing (getByIndex)

getByIndex : Int -> List a -> Maybe a
getByIndex idx list =
  case list of
    item :: rest ->
      if idx == 0 then
        Just item
      else
        getByIndex (idx - 1) rest
    [] ->
      Nothing
