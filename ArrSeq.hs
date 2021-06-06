module ArrSeq where

import Seq
import Par
import qualified Arr as A
import Arr ((!))

instance Seq A.Arr where
  emptyS = A.empty
  singletonS = singletonArr
  lengthS = A.length
  nthS = (!)
  tabulateS = A.tabulate
  mapS = mapArr
  filterS = filterArr
  appendS = appendArr
  takeS = takeArr
  dropS = dropArr
  showtS = showtArr
  showlS = showlArr
  joinS = A.flatten
  reduceS = reduceArr
  --scanS = scanArr
  fromList = A.fromList

singletonArr x = fromList [x]

mapArr f arr = let n = lengthS arr 
               in  tabulateS (\i -> (f (nthS arr i))) n

filterArr p arr = joinS (mapArr (\x -> if p x then singletonArr x else emptyS) arr)

appendArr arr brr = let n = lengthS arr
                        m = lengthS brr
                    in tabulateS (\i -> if i < n then nthS arr i else nthS brr i) (n+m)

takeArr arr n = let l = lengthS arr
                in if l < n then arr else A.subArray 0 n arr

dropArr arr n = let l = lengthS arr
                in if l < n then emptyS else A.subArray n (l-n) arr

showtArr arr | largo == 0 = EMPTY
             | largo == 1 = ELT (nthS arr 0)
             | otherwise = NODE (takeArr arr (div largo 2)) (dropArr arr (div largo 2))
            where largo = lengthS arr

showlArr arr  | largo == 0 = NIL
              | otherwise = CONS (nthS arr 0) (dropArr arr 1)
              where largo = lengthS arr

reduceArr f e arr | len == 0  = e
                  | len == 1  = f e (nthS arr 0)
                  | otherwise = let contraer = tabulateS (\i -> f (nthS arr (2*i)) (nthS arr ((2*i)+1))) (div len 2)
                                in reduceArr f e contraer
                  where len = lengthS arr