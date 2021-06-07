module ArrSeq where

import Seq
import Par
import qualified Arr as A
import Arr ((!))

instance Seq A.Arr where
  emptyS = emptyArr
  singletonS = singletonArr
  lengthS = lengthArr
  nthS = nthArr
  tabulateS = tabulateArr
  mapS = mapArr
  filterS = filterArr
  appendS = appendArr
  takeS = takeArr
  dropS = dropArr
  showtS = showtArr
  showlS = showlArr
  joinS = joinArr
  reduceS = reduceArr
  scanS = scanArr
  fromList = fromListArr

emptyArr = A.empty 

singletonArr x = A.fromList [x]

lengthArr = A.length

nthArr = (!)

tabulateArr = A.tabulate

mapArr f arr = let largo = lengthArr arr 
               in  tabulateArr (\i -> (f (nthArr arr i))) largo

filterArr p arr = joinArr (mapArr (\x -> if p x then singletonArr x else emptyArr) arr)

appendArr arr brr = let n = lengthArr arr
                        m = lengthArr brr
                    in tabulateArr (\i -> if i < n then nthArr arr i else nthArr brr i) (n+m)

takeArr arr n = let largo = lengthArr arr
                in if largo < n then arr else A.subArray 0 n arr

dropArr arr n = let largo = lengthArr arr
                in if largo < n then emptyArr else A.subArray n (largo-n) arr

showtArr arr | largo == 0 = EMPTY
             | largo == 1 = ELT (nthArr arr 0)
             | otherwise = NODE (takeArr arr (div largo 2)) (dropArr arr (div largo 2))
             where largo = lengthArr arr

showlArr arr  | largo == 0 = NIL
              | otherwise = CONS (nthArr arr 0) (dropArr arr 1)
              where largo = lengthArr arr

joinArr = A.flatten

contraer :: (a->a->a)-> A.Arr a -> Int -> A.Arr a
contraer f arr largo | even largo  = tabulateArr (\i -> f (nthArr arr (2*i)) (nthArr arr ((2*i)+1))) rango
                     | otherwise   = tabulateArr (\i -> if i == rango then nthArr arr (2*i) else f (nthArr arr (2*i)) (nthArr arr ((2*i)+1))) (rango+1)
                     where rango = (div largo 2)

reduceArr f e arr | largo == 0  = e
                  | largo == 1  = f e (nthArr arr 0)
                  | otherwise = let ctr = contraer f arr largo
                                in reduceArr f e ctr
                  where largo = lengthArr arr

scanArr f e arr | largo == 0 = (emptyArr, e)
                | largo == 1 = (singletonArr e, f e (nthArr arr 0))
                | otherwise = let ctr = contraer f arr largo
                                  (brr, r) = scanArr f e ctr
                              in (expandirArr f arr brr, r)
                where
                  largo = lengthArr arr
                  expandirArr f arr brr = tabulateArr (\i -> if even i then (nthArr brr (div i 2)) else f (nthArr brr (div i 2)) (nthArr arr (i - 1))) largo

fromListArr = A.fromList