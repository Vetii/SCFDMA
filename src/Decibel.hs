module Decibel (Decibel,
                mkDecibel,
                toDecibel,
                fromDecibel) where

data Decibel a = Decibel a 

instance Show a => Show (Decibel a) where
    show (Decibel a) = show a

mkDecibel :: a -> Decibel a
mkDecibel x = Decibel x

-- from linear to decibels
toDecibel :: (Floating a, Fractional a) => a -> a -> Decibel a 
toDecibel p pRef = Decibel (10 * logBase 10 (p / pRef))

fromDecibel :: (Floating a, Fractional a) => Decibel a -> a
fromDecibel (Decibel x) = 10**(x / 10)
