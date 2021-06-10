module ListSeq where

import Seq
import Par

instance Seq [] where
  emptyS = emptyList
  singletonS = singletonList
  lengthS = lenghtList
  nthS = nthList
  tabulateS = tabulateList
  mapS = mapList
  filterS = filterList
  appendS = appendList
  takeS = takeList
  dropS = dropList
  showtS = showtList
  showlS = showlList
  joinS = joinList
  reduceS = reduceList
  scanS = scanList
  fromList = fromList_

emptyList = []

singletonList a = [a]

lenghtList s = length s

nthList s n = s !! n
 
tabulateList f n = tabulateList' f n 0 
                 where 
                  tabulateList' f 0 _ = emptyList 
                  tabulateList' f n i = let (x, xs) = f i ||| tabulateList' f (n-1) (i+1)
                                        in x:xs
  

mapList f []     = emptyList 
mapList f (x:xs) = let (y, ys) = f x ||| mapList f xs
                   in y:ys

filterList _ []     = emptyList 
filterList p (x:xs) = let (bool, ys) = p x ||| filterList p xs
                      in if bool then x : ys else ys 

appendList [] sb     = sb
appendList sa []     = sa
appendList (a:sa) sb = a: (appendList sa sb)

takeList s n = take n s

dropList s n = drop n s

showtList []  = EMPTY
showtList [x] = ELT x
showtList xs  = let largo = div (lenghtList xs) 2
                    (l, r) = (takeList xs largo) ||| (dropList xs largo)
                in NODE l r

showlList []     = NIL
showlList (x:xs) = CONS x xs

joinList []     = emptyList 
joinList (x:xs) = appendList x (joinList xs)

contraerList :: (a -> a -> a) -> [a] -> [a]
contraerList _ []       = emptyList
contraerList _ l@[x]    = l
contraerList f (x:y:xs) = let (z,zs) = f x y ||| contraerList f xs
                          in z:zs

reduceList _ e []  = e
reduceList f e [x] = f e x
reduceList f e xs  = let ctr = contraerList f xs
                     in reduceList f e ctr

scanList _ e []  = (emptyList , e)
scanList f e [x] = (singletonList e, f e x)
scanList f e xs  = let (ys, r) = scanList f e (contraerList f xs)
                   in (expandirList f xs ys, r) 
                  where
                    expandirList _ [] _ = []
                    expandirList _ [_] ys = ys
                    expandirList f (x:_:xs) (y:ys) = let (z, zs) = (f y x) ||| expandirList f xs ys
                                                     in y:z:zs

fromList_ s = s 