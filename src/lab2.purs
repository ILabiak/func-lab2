module Lab2 where

import Prelude

import Data.List (List(..), (:))
import Data.Maybe (Maybe(..), fromMaybe)
import Data.String (contains, Pattern(..))
import Data.Tuple (Tuple(..), fst, snd)
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

unzipFirstEl :: forall a b. List (Tuple a b) -> List a
unzipFirstEl (a : as) = (fst a):(unzipFirstEl as)
unzipFirstEl Nil = Nil

unzipSecondEl :: forall a b. List (Tuple a b) -> List b
unzipSecondEl (b : bs) = (snd b):(unzipSecondEl bs)
unzipSecondEl Nil = Nil

findIndex :: forall a. (a -> Boolean) -> List a -> Maybe Int
findIndex = find 0  -- call find func with index 0, condition and list. (condition and list are omitted)

findLastIndex :: forall a. (a -> Boolean) -> List a -> Maybe Int
findLastIndex condition list = 
    let i = fromMaybe (-1) (findIndex condition (reverse list)) in
    if (i == -1)
    then Nothing
    else Just ((length list) - i - 1)

zip :: forall a b. List a -> List b -> List (Tuple a b)
zip (a:as) (b : bs) = (Tuple a b) : (zip as bs)
zip _ _ = Nil

unzip :: forall a b. List (Tuple a b) -> Tuple (List a) (List b)
unzip list = Tuple (unzipFirstEl(list)) (unzipSecondEl(list))

filter :: forall a. (a -> Boolean) -> List a -> List a
filter condition (a:as) | condition a = a : (filter condition as) -- use guard
filter condition (_:as) = filter condition as
filter _ (a) = Nil

optimisedFilter:: forall a. (a -> Boolean) -> List a -> List a -> List a
optimisedFilter condition (a:as) b | condition a = optimisedFilter condition as (a:b)
optimisedFilter condition (_:as) b = optimisedFilter condition as b
optimisedFilter _ _ b =  reverse b
optimisedFilter _ _ Nil = Nil

take :: forall a. Int -> List a -> List a
take n (a:as) = if(n > 0) then a:(take (n-1) as) else Nil
take _ _ = Nil

optimisedTake :: forall a. Int -> List a -> List a -> List a
optimisedTake n (a:as) b = if(n > 0) then a:(optimisedTake (n-1) as (a:b)) else Nil
optimisedTake _ _ b = b
optimisedTake _ (_:as) _ = Nil



test::Effect Unit
test = do
    log $ show $ findIndex (contains $ Pattern "b") ("a": "bb": "f" : "b": "d": Nil)
    log $ show $ findLastIndex (contains $ Pattern "b") ("a": "bb": "f" : "b": "d": Nil)
    log $ show $ zip ("a":"b":"c":Nil) ("d":"e":"f":"g":Nil)
    log $ show $ unzip ((Tuple "a" "d") : (Tuple "b" "e") : (Tuple "c" "f") : Nil)
    log $ show $ filter (contains $ Pattern "b") ("a": "bb": "f" : "b": "d": Nil)
    log $ show $ optimisedFilter (contains $ Pattern "b") ("a": "bb": "f" : "b": "d": Nil) Nil
    log $ show $ take 2 ("a": "bb": "f" : "b": "d": Nil)
    log $ show $ optimisedTake (3) ("a": "bb": "f" : "b": "d": Nil) Nil