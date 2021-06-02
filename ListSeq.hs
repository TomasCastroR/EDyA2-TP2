module ListSeq where

import Seq
import Par

instance Seq [] where
  emptyS = emptyS_
  singletonS = singletonS_
  lengthS = lengthS_
  nthS = nthS_
  tabulateS = tabulateS_
  mapS = mapS_
  filterS = filterS_
  appendS = appendS_
  takeS = takeS_
  dropS = dropS_
  showtS = showtS_
  showlS = showlS_
  joinS = joinS_
  --reduceS = reduceS_
  --scanS = scanS_
  fromList = fromList_

emptyS_ = []

singletonS_ a = [a]

lengthS_ s = length s

nthS_ s n = s !! n

tabulateS_ f n = [f i | i <- [0..n]]

mapS_ f [] = []
mapS_ f (x:xs) = let (y, ys) = f x ||| mapS_ f xs
               in y:ys

filterS_ p s = [a | a <-s , p a]

appendS_ [] sb = sb
appendS_ sa [] = sa
appendS_ sa sb = sa ++ sb

takeS_ _ n      | n <= 0 =  []
takeS_ [] _              =  []
takeS_ (x:xs) n          =  x : takeS_ xs (n-1)

dropS_ s n = drop n s

showtS_ [] = EMPTY
showtS_ [x] = ELT x
showtS_ xs = let l = lengthS_ xs in NODE (takeS_ xs (div l 2)) (dropS_ xs (div l 2))

showlS_ [] = NIL
showlS_ (x:xs) = CONS x xs

joinS_ [[]] = []
joinS_ [[x]] = [x]
joinS_ (xs:[xss]) = appendS_ xs (joinS_ [xss])

fromList_ as = as 