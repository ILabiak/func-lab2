module Lab2 where

import Prelude

import Data.List (List(..), (:))
import Data.Maybe (Maybe(..), fromMaybe, isNothing)
import Data.String (contains, Pattern(..))
import Effect (Effect)
import Effect.Console (log)


find:: Int -> forall a. (a -> Boolean) -> List a -> Maybe Int
find i condition (head : tail) = if (condition head) then (Just i) else (find (i + 1) (condition) tail)
find _ _ Nil = Nothing

reverse :: List ~> List
reverse = go Nil
  where
  go acc Nil = acc
  go acc (a : b) = go (a : acc) b

length :: forall a. List a -> Int
length Nil = 0
length (a : list) = length list + 1

findIndex :: forall a. (a -> Boolean) -> List a -> Maybe Int
findIndex = find 0  -- call find func with index 0, condition and list. (condition and list are omitted)

findLastIndex :: forall a. (a -> Boolean) -> List a -> Maybe Int
findLastIndex condition list = 
    let i = fromMaybe (-1) (findIndex condition (reverse list)) in
    if (i == -1)
    then Nothing
    else Just ((length list) - i - 1)


test::Effect Unit
test = do
    log $ show $ findLastIndex (contains $ Pattern "b") ("a": "bb": "f" : "b": "d": Nil)
