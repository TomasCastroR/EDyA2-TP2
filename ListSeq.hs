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

tabulateS__ f 0 _ = emptyS_ 
tabulateS__ f n i = let (x, xs) = f i ||| tabulateS__ f (n-1) (i+1)
                    in x:xs

tabulateS_ f n = tabulateS__ f n 0
  

mapS_ f [] = emptyS_ 
mapS_ f (x:xs) = let (y, ys) = f x ||| mapS_ f xs
                 in y:ys

filterS_ _ [] = emptyS_ 
filterS_ p (x:xs) = let (y, ys) = p x ||| filterS_ p xs
                    in if y then x : ys else ys 

appendS_ [] sb = sb
appendS_ sa [] = sa
appendS_ (a:sa) sb = a: (appendS_ sa sb)

takeS_ _ n      | n <= 0 =  []
takeS_ [] _              =  []
takeS_ (x:xs) n          =  x : takeS_ xs (n-1)

dropS_ s n = drop n s

showtS_ [] = EMPTY
showtS_ [x] = ELT x
showtS_ xs = let largo = lengthS_ xs 
                 (l,r) = (takeS_ xs (div largo 2)) ||| (dropS_ xs (div largo 2))
             in NODE l r

showlS_ [] = NIL
showlS_ (x:xs) = CONS x xs

joinS_ [] = emptyS_ 
joinS_ (x:xs) = appendS_ x (joinS_ xs)

loga_b :: Int -> Int -> Int
loga_b a b
  | b < 0 || a < 0 = signum a * signum b * loga_b (abs a) (abs b)
  | b < a     = 0
  | b >= a     = 1 + loga_b a (b `div` a)

reduceS__ _ [x] = x
reduceS__ f xs = let n = lengthS_ xs
                     pp = 2^(loga_b 2 (n-1)) 
                 in f (reduceS__ f (takeS_ xs pp)) (reduceS__ f (dropS_ xs pp)) 
                  
reduceS_ f e xs = f e (reduceS__ f xs)
                            
-- [1,2,3,4,5] = e f (((1f2) f (3f4))f5)) 
--[1,2,3,4,5,6,7,8,9,10] = (((((1+2) + (3+4)) + (5+6)) + (7+8)) +(9+10))
-- [1+2,3+4,]
-- reduceBarato (+) 0 [1,2,3,4,5]
-- 1 (+) (2  (+) (3 + (4 + (5 + 0))))

fromList_ as = as 