-- {-# language BangPatterns, GeneralizedNewtypeDeriving #-}

module Model 
    ( User (..),
      Channel (..),
      NUsers (..),
      NChannels (..),
      NBuckets (..),
      Bandwidth (..),
      NPSD (..),
      UserPowerLimit (..),
      PeakPowerLimit (..),
      GainMatrix (..),
      Configuration (..),
      Block (..),
      Allocation,
      AlgoConfig (..),
      UserPowerLimitF,
      PeakPowF,
      UtilityF (..),
      GainF,
      mkParams
    ) where

import Data.Set as Set
import Data.Array as Array
import Data.HashMap.Lazy as HashMap

import Decibel
import Block

newtype NBuckets = NBuckets (Int) deriving (Show)

newtype Bandwidth = Bandwidth (Float) deriving (Show)

newtype NPSD = NPSD (Decibel Float) deriving (Show)

newtype UserPowerLimit = UserPowerLimit (Float) deriving (Show)

newtype PeakPowerLimit = PeakPowerLimit (Float) deriving (Show)

newtype GainMatrix = GainMatrix ([[Decibel Float]]) deriving (Show)

data Configuration = Configuration { 
    nusers :: NUsers,
    nchannels :: NChannels,
    nbuckets  :: NBuckets,
    bandwidth :: Bandwidth,
    npsd :: NPSD,
    userPL :: UserPowerLimit,
    peakPL :: PeakPowerLimit,
    gainMT :: GainMatrix } deriving (Show)

newtype UserPowerLimitF = UserPowerLimitF { getPowerLimit :: User -> UserPowerLimit}

newtype PeakPowF = PeakPowF { getPeakPowerLimit :: Channel -> PeakPowerLimit}

newtype PowerF = PowerF { getPower :: User -> Channel -> Float }

newtype UtilityF = UtilityF { getUtility :: User -> Block -> Float }

newtype GainF = GainF { getGain :: User -> Channel -> Float }

data AlgoConfig = AlgoConfig {
    allUsers :: Set User,
    allChannels :: Set Channel,
    utilityF :: UtilityF,
    bucketSize :: NBuckets 
}

mkParams :: Configuration -> AlgoConfig 
mkParams conf = 
    let m    = mkUsers (nusers conf) 
        n    = mkChannels (nchannels conf)
        k    = nbuckets conf
        uplf = UserPowerLimitF (\_ -> userPL conf)
        pkpl = PeakPowF (\_ -> peakPL conf)
        blkP = blockPow uplf pkpl 
        gainMatrix = let (GainMatrix mat) = gainMT conf
                         trRow r = listArray (findMin n, findMax n) r 
                         rows = Prelude.map trRow mat
                     in listArray (findMin m, findMax m) rows
        gainF= GainF (\u c -> 
             fromDecibel ((gainMatrix Array.! u) Array.! c))
        uf   = utility (bandwidth conf) (npsd conf) gainF blkP
    in AlgoConfig m n uf k

-- The power each user uses on each channel
pow :: UserPowerLimitF -> PeakPowF -> Int -> PowerF
pow (UserPowerLimitF puf) (PeakPowF psf) num = 
    PowerF (\u c -> 
        let (UserPowerLimit pu) = puf u 
            (PeakPowerLimit ps) = psf c 
        in min (pu / (fromIntegral num)) (ps))

blockPow :: UserPowerLimitF -> PeakPowF -> Block -> PowerF
blockPow plf pkpf (Block b) = pow plf pkpf (Set.size b)

utility :: Bandwidth -> NPSD -> GainF -> (Block -> PowerF) -> UtilityF
utility (Bandwidth b) (NPSD npsd) gainF powF = UtilityF (\u block@(Block channels) ->
    let power   = getPower (powF block) u
        gain    = getGain gainF u
        pRatio  = fromDecibel npsd 
        f' c    = b * (logBase 2 (1 + ((power c) * (gain c)) / (pRatio * b)))
    in sum (Set.map f' channels))

-- The total utility of a given allocation 
totalUtility :: UtilityF -> Allocation -> Float
totalUtility (UtilityF uf) (Allocation a) = let utilities = HashMap.mapWithKey uf a
                                            in HashMap.foldr (+) 0 utilities 
