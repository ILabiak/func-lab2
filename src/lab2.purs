module Lab2 where

import Prelude

import Data.List (List(..), (:))
import Data.Maybe (Maybe(..))
import Data.String (contains, Pattern(..))
import Effect (Effect)
import Effect.Console (log)


find:: Int -> forall a. (a -> Boolean) -> List a -> Maybe Int
find i condition (head : tail) = if (condition head) then (Just i) else (find (i + 1) (condition) tail)
find _ _ Nil = Nothing

findIndex :: forall a. (a -> Boolean) -> List a -> Maybe Int
findIndex = find 0  -- call find func with index 0, condition and list. (condition and list are omitted)


test::Effect Unit
test = do
    log $ show $ findIndex (contains $ Pattern "b") ("a": "bb": "b": "d": Nil)
