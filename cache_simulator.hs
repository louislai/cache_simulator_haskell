-- Haskell implementation of a direct-access cache simulator --

import System.IO
import Control.Monad
import Prelude
import Numeric



main :: IO ()
main = 
    do cacheSize <- getLine
       cacheBlockSize <- getLine
       n <- getLine
       addresses <- replicateM (read n :: Int) getLine
       let numBlock = (read cacheSize :: Integer) `div` (read cacheBlockSize :: Integer)
           result   = calculate (read n :: Integer) numBlock (read cacheBlockSize :: Integer) (initCache numBlock) (0, 0) addresses
       putStrLn $ result


calculate :: Integer -> Integer -> Integer -> Cache -> HitCounter -> [String] -> String
calculate n numBlock blockSize cache hc addresses
    | n == 0        = hitP hc
    | otherwise     = uncurry (calculate (n - 1) numBlock blockSize) check (tail addresses)
                        where check = checkCache blockSize numBlock ncHC addr
                              addr  = read (head addresses) :: Integer
                              ncHC  = (cache, hc)

type Cache = [Integer]

initCache :: Integer -> Cache
initCache 0 = []
initCache n = -1 : initCache (n - 1)

getTag :: Integer -> Integer -> Integer -> Integer
getTag wordsize numBlock addr = (addr `div` wordsize) `div` numBlock

getIndex :: Integer -> Integer -> Integer -> Integer
getIndex wordsize numBlock addr = (addr `div` wordsize) `mod` numBlock

type HitCounter = (Float, Float)

hitP :: HitCounter -> String
hitP hc = showGFloat (Just 3) ((*100) (uncurry (/) hc)) "" ++ "%"


checkCache :: Integer -> Integer -> (Cache, HitCounter) -> Integer -> (Cache, HitCounter)
checkCache wordsize numBlock cnHC addr = (fst cNi, (snd cNi + fst hc, snd hc + 1))
                                            where hc    = snd cnHC
                                                  cNi   = manipulateCache (getIndex wordsize numBlock addr) (getTag wordsize numBlock addr) (fst cnHC)
                                      

manipulateCache :: Integer -> Integer -> Cache -> (Cache, Float)
manipulateCache index tag cache
        | (index == 0) && head cache == tag   = (cache, 1)
        | (index == 0) && (head cache /= tag) = (tag : tail cache, 0)
        | otherwise                           = (head cache : fst (manipulateCache (index - 1) tag (tail cache)), snd (manipulateCache (index - 1) tag (tail cache)))
