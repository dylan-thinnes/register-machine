module Register.Codeable where

import Register
import Register.Instr

import Data.Function ((&))
import Data.Numbers.Primes (primes, primeFactors)
import Data.List (groupBy, group)

-- Define Codeable values
class Codeable a where
    encode :: a -> Integer
    decode :: Integer -> a

instance Codeable Integer where
    encode = id
    decode = id

editCode :: Codeable a => (Integer -> Integer) -> a -> a
editCode f = decode . f . encode

-- Use factorPowers to implement 2^x * 3^y... etc coding functions
factorPowers :: Integer -> [(Integer, Integer)]
factorPowers x = x
               & primeFactors
               & group
               & map (\x -> (head x, fromIntegral $ length x))

-- Define generic codeable instances for most values we could want.
instance (Codeable a) => Codeable [a] where
    -- Use primes to code lists - The 2 ^ head * 3 ^ tail coding system would
    -- be much prettier, but would be prohibitive from a number size
    -- perspective even for the smallest inputs.
    encode xs = product $ zipWith (^) primes $ map encode xs
    decode x = map decode $ f (factorPowers x) primes
        where
            f [] _ = []
            f ((factor, power):factors) (prime : primes)
                | factor == prime = power : f factors primes
                | otherwise       = 0 : f ((factor, power):factors) primes

instance (Codeable a, Codeable b) => Codeable (a, b) where
    encode (a, b) = 2 ^ encode a * 3 ^ encode b
    decode x = let (a:b:_) = decode x ++ [0..]
                in (decode a, decode b)

instance (Codeable a, Codeable b) => Codeable (Either a b) where
    encode x = case x of
                 Left a -> 3 ^ encode a
                 Right a -> 2 * 3 ^ encode a
    decode x = let (a:b:_) = decode x ++ [0..]
                in if a == 0 then Left (decode b) else Right (decode b)

instance (Codeable a) => Codeable (Instr a) where
    encode (Inc (Register r)) = encode [1, encode r]
    encode (Decjz (Register r) label) = encode [2, encode r, encode label]
    decode x = let (a:b:c:_) = decode x ++ [0..]
                in if a == 1
                      then Inc $ Register $ decode b
                      else Decjz (Register $ decode b) (decode c)

-- We can define codeable instances for the Machine later...

