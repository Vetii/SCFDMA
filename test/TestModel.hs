module TestModel (
    NUsers (..),
    User (..),
    NChannels (..),
    Channel (..),
    NBuckets (..),
    Bandwidth (..),
    NPSD (..),
    UserPowerLimit (..),
    PeakPowerLimit (..),
    Configuration (..),
    AlgoConfig (..)
) where

import System.Random
import Test.QuickCheck
import Model

instance Random NUsers where
    randomR ((NUsers a), (NUsers b)) g = 
        let (n, g') = randomR (a, b) g
        in (NUsers n, g')
    random g = 
        let (n, g') = random g
        in (NUsers n, g')

instance Arbitrary NUsers where
    arbitrary = do
        n <- arbitrary
        return (NUsers n)

instance Arbitrary User where
    arbitrary = do
        u <- arbitrary
        return (User u)

instance Arbitrary NChannels where
    arbitrary = do
        n <- arbitrary
        return (NChannels n)

instance Random NChannels where
    randomR ((NChannels a), (NChannels b)) g = 
        let (n, g') = randomR (a, b) g
        in (NChannels n, g')
    random g = 
        let (n, g') = random g
        in (NChannels n, g')

instance Arbitrary Channel where
    arbitrary = do
        c <- arbitrary
        return (Channel c)

instance Arbitrary NBuckets where
    arbitrary = do
        n <- arbitrary
        return (NBuckets n)

instance Random NBuckets where
    randomR ((NBuckets a), (NBuckets b)) g = 
        let (n, g') = randomR (a, b) g
        in (NBuckets n, g')
    random g = 
        let (n, g') = random g
        in (NBuckets n, g')

instance Arbitrary Bandwidth where
    arbitrary = do
        b <- arbitrary
        return (Bandwidth b)

instance Arbitrary NPSD where
    arbitrary = do
        x <- arbitrary
        return (NPSD x)

instance Arbitrary UserPowerLimit where
    arbitrary = do
        x <- arbitrary
        return (UserPowerLimit x)

instance Arbitrary PeakPowerLimit where
    arbitrary = do
        x <- arbitrary
        return (PeakPowerLimit x)

instance Arbitrary Configuration where
    arbitrary = do
        nu <- choose (2, 10)
        nc <- choose (2, 64)
        nb <- choose (1, 30)
        bandW <- arbitrary
        npsd  <- arbitrary
        uPL   <- arbitrary
        pPL   <- arbitrary
        mat   <- vectorOf nu (vector nc) :: Gen [[Float]]
        return (Configuration 
            (NUsers nu)
            (NChannels nc)
            (NBuckets nb) bandW npsd uPL pPL (GainMatrix mat))

instance Arbitrary AlgoConfig where
    arbitrary = do
        c <- arbitrary
        return (mkParams c)
