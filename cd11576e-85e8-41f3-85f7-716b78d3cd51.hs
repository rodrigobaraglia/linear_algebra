module Basics where

--Sum of vectors:
sumVec:: Num b => [b] -> [b] -> [b]
sumVec a b  =  zipWith (+) a b 
sumVec':: Num b => [b] -> [b] -> [b]
sumVec' a b  = map (\pair -> fst pair + snd pair) $ zip a b 


--Dot product:
dotProd:: Num a => [a] -> [a] -> a
dotProd a b = sum $ zipWith (*) a b

dotProd':: Num a => [a] -> [a] -> a
dotProd' a b = sum $ map (\pair -> fst pair * snd pair) $ zip a b 

--Transpose
transpose:: [[a]] -> [[a]]
transpose ([]:_) = []
transpose x = (map head x) : transpose (map tail x)

--Matrix multiplication (ordered from more to less verbose implementations)
mMult'':: Num c => [[c]] -> [[c]] -> [[c]]
mMult'' a b = map (\pair -> zipWith (*) (fst pair) (snd pair)) ab
    where ab = zip a $ transpose b

mMult':: Num c => [[c]] -> [[c]] -> [[c]]
mMult' a b = map (\pair -> zipWith (*) (fst pair) (snd pair)) $ zip a $ transpose b

mMult:: Num c => [[c]] -> [[c]] -> [[c]]
mMult a b =  zipWith (zipWith (*)) a $ transpose b