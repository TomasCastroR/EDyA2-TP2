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
  reduceS = reduceS_
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

takeS_ s n = take n s

dropS_ s n = drop n s

showtS_ [] = EMPTY
showtS_ [x] = ELT x
showtS_ xs = let largo = div (lengthS_ xs) 2
                 (l, r) = (takeS_ xs largo) ||| (dropS_ xs largo)
             in NODE l r

showlS_ [] = NIL
showlS_ (x:xs) = CONS x xs

joinS_ [] = emptyS_ 
joinS_ (x:xs) = appendS_ x (joinS_ xs)

contraer :: (a -> a -> a) -> [a] -> [a]
contraer _ [] = []
contraer _ [x] = [x]
contraer f (x:y:xs) = let (z,zs) = f x y ||| contraer f xs
                      in z:zs

reduceS_ _ e [] = e
reduceS_ f e [x] = f e x
reduceS_ f e xs = reduceS_ f e (contraer f xs)

scanSContExp _ _ e [] = (singletonS_ e, e)
scanSContExp _ f e [x] = (singletonS_ e, f e x)
scanSContExp exp f e xs = exp f xs (scanSContExp exp f e (contraer f xs))

scanS_ f e xs = scanSContExp expandir f e xs

es_par i = (rem i 2) == 0

expandir f xs (ys, r) = (tabulateS_ (reconstruir f xs ys) (lengthS_ xs), r)

reconstruir f xs ys i = if es_par i then nthS_ ys (div i 2)
                                    else f (nthS_ ys (div i 2)) (nthS_ xs (i-1)) 

fromList_ s = s 