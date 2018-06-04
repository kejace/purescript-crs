module Main where

import Prelude

import Control.Monad.Eff (Eff)
import Control.Monad.Eff.Console (CONSOLE, log, logShow)

import Data.Functor.Mu (Mu)
import Data.TacitString (TacitString)
import Matryoshka 

data ListF a t = Nil | Cons a t

derive instance functorListF :: Functor (ListF a)

instance showListF :: Show a => Show (ListF a TacitString) where
  show Nil = "Nil"
  show (Cons h t) = show h <> " : " <> show t

nil :: forall a t. Corecursive t (ListF a) => t
nil = embed Nil

cons :: forall a t. Corecursive t (ListF a) => a -> t -> t
cons h t = embed (Cons h t)

prod :: Algebra (ListF Int) Int
prod Nil = 1
prod (Cons h t) = h * t

count :: Coalgebra (ListF Int) Int
count n | n <= 0    = Nil
        | otherwise = Cons n (n - 1)

fac :: Int -> Int
fac = hylo prod count

mapf :: forall a b c d
      . Corecursive a c 
     => Recursive b d 
     => (d b -> c a)
     -> b 
     -> a
mapf f = embed <<< f <<< project

type List a = Mu (ListF a)

listExample :: forall e. Eff (console :: CONSOLE | e) Unit
listExample = do
  log "list example"
  let someList = cons 1 (cons 2 (cons 3 (cons 4 nil)))
  logShow $ cata prod (someList :: List Int)
  logShow (ana count 4 :: List Int)
  logShow (fac 6)

main :: forall e. Eff (console :: CONSOLE | e) Unit
main = do
  listExample
