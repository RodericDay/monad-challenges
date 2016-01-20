{-# LANGUAGE MonadComprehensions #-}
{-# LANGUAGE RebindableSyntax  #-}

module Set1 where

import MCPrelude



successiveRand :: Integer -> Seed -> [Integer]
successiveRand rem seed
    | rem > 0       = n : successiveRand (rem-1) newSeed
    | otherwise     = []
        where (n, newSeed) = rand seed

fiveRands :: [Integer]
fiveRands = successiveRand 5 (mkSeed 1)



randLetter :: Gen Char
randLetter seed = (toLetter n, newSeed)
    where (n, newSeed) = rand $ seed

successiveRandLetter :: Integer -> Seed -> [Char]
successiveRandLetter rem seed
    | rem > 0       = c : successiveRandLetter (rem-1) newSeed
    | otherwise     = []
        where (c, newSeed) = randLetter seed

randString3 :: String
randString3 = successiveRandLetter 3 (mkSeed 1)



type Gen t = Seed -> (t, Seed)

generalA :: (Integer -> Integer) -> Gen Integer
generalA f =
    \seed ->
        let (n, newSeed) = rand $ seed
        in (f n, newSeed)

randOdd :: Gen Integer
randOdd seed = generalA (2*) seed

randEven :: Gen Integer
randEven seed = generalA ( (+1) . (2*) ) seed

randTen :: Gen Integer
randTen seed = generalA (10*) seed

triplet =   map (\f -> fst $ f $ mkSeed 1) [randOdd, randEven, randTen]



randPair :: Gen (Char, Integer)
randPair seed = (('c', 1), mkSeed 1)

randPair1 = fst $ randPair $ mkSeed 1
