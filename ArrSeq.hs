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
  --scanS = scanArr
  fromList = fromListArr

emptyArr = A.empty 

singletonArr x = A.fromList [x]

nthArr = (!)

lengthArr = A.length

tabulateArr = A.tabulate

joinArr = A.flatten

fromListArr = A.fromList

mapArr f arr = let n = lengthArr arr 
               in  tabulateArr (\i -> (f (nthArr arr i))) n

filterArr p arr = joinArr (mapArr (\x -> if p x then singletonArr x else emptyArr) arr)

appendArr arr brr = let n = lengthArr arr
                        m = lengthArr brr
                    in tabulateArr (\i -> if i < n then nthArr arr i else nthArr brr i) (n+m)

takeArr arr n = let l = lengthArr arr
                in if l < n then arr else A.subArray 0 n arr

dropArr arr n = let l = lengthArr arr
                in if l < n then emptyArr else A.subArray n (l-n) arr

showtArr arr | largo == 0 = EMPTY
             | largo == 1 = ELT (nthArr arr 0)
             | otherwise = NODE (takeArr arr (div largo 2)) (dropArr arr (div largo 2))
            where largo = lengthArr arr

showlArr arr  | largo == 0 = NIL
              | otherwise = CONS (nthArr arr 0) (dropArr arr 1)
              where largo = lengthArr arr

contraer f arr len | even len  = tabulateArr (\i -> f (nthArr arr (2*i)) (nthArr arr ((2*i)+1))) rango
                   | otherwise = tabulateArr (\i -> if i == rango then nthArr arr (2*i) else f (nthArr arr (2*i)) (nthArr arr ((2*i)+1))) (rango+1)
                   where rango = (div len 2)

reduceArr f e arr | len == 0  = e
                  | len == 1  = f e (nthArr arr 0)
                  | otherwise = let ctr = contraer f arr len
                                in reduceArr f e ctr
                  where len = lengthArr arr