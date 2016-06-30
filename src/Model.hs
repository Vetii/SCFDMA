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
import Data.HashMap.Lazy as HashMap (HashMap, foldr, mapWithKey)
import Data.Array as Array

newtype NUsers = NUsers Int deriving (Show)

newtype User = User Int deriving (Ord, Eq, Show, Ix)
instance Enum User where
    toEnum = User
    fromEnum (User u) = u

mkUsers :: NUsers -> Set User
mkUsers (NUsers n) = Set.fromList (fmap (User) [1..n])

newtype NChannels = NChannels (Int) deriving (Show)

newtype Channel = Channel Int deriving (Ord, Eq, Show, Ix)

instance Enum Channel where
    toEnum = Channel
    fromEnum (Channel c) = c

mkChannels :: NChannels -> Set Channel
mkChannels (NChannels n) = Set.fromList (fmap (Channel) [1..n])

newtype NBuckets = NBuckets (Int) deriving (Show)

newtype Bandwidth = Bandwidth (Float) deriving (Show)

newtype NPSD = NPSD (Float) deriving (Show)

newtype UserPowerLimit = UserPowerLimit (Float) deriving (Show)

newtype PeakPowerLimit = PeakPowerLimit (Float) deriving (Show)

newtype GainMatrix = GainMatrix ([[Float]]) deriving (Show)

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
            (gainMatrix Array.! u) Array.! c)
        uf   = utility (bandwidth conf) (npsd conf) gainF blkP
    in AlgoConfig m n uf k

-- Conversion from dB to ratio of Watts
toLinear :: (Floating a, Fractional a) => a -> a
toLinear x = 10**(x / 10)

-- The power each user uses on each channel
pow :: UserPowerLimitF -> PeakPowF -> Int -> PowerF
pow (UserPowerLimitF puf) (PeakPowF psf) num = 
    PowerF (\u c -> 
        let (UserPowerLimit pu) = puf u 
            (PeakPowerLimit ps) = psf c 
        in min (pu / (fromIntegral num)) (ps))

blockPow :: UserPowerLimitF -> PeakPowF -> Block -> PowerF
blockPow plf pkpf (Block b) = pow plf pkpf (Set.size b)

{-- 
-- The total power each user uses on a block
blockPow :: UserPowerLimitF -> PeakPowF -> User -> Block -> Float
blockPow pu ps u (Block b) = 
    let blockSz = Set.size b
        powF    = pow pu ps blockSz
        pows    = Set.map (getPower powF u) b
    in Set.fold (+) 0 pows
    --}

utility :: Bandwidth -> NPSD -> GainF -> (Block -> PowerF) -> UtilityF
utility (Bandwidth b) (NPSD s) gainF powF = UtilityF (\u block@(Block channels) ->
    let power   = getPower (powF block) u
        gain    = getGain gainF u
        f' c    = b * (logBase 2 (1 + ((power c) * (gain c)) / (s * b)))
    in sum (Set.map f' channels))

-- The total utility of a given allocation 
totalUtility :: UtilityF -> Allocation -> Float
totalUtility (UtilityF uf) (Allocation a) = let utilities = HashMap.mapWithKey uf a
                                            in HashMap.foldr (+) 0 utilities 

newtype Block = Block (Set Channel) 

newtype ContiguousBlock = ContiguousBlock (Channel, Channel)

fromContiguous :: ContiguousBlock -> Block
fromContiguous (ContiguousBlock (a, b)) = 
    let (Channel s) = a
        (Channel e) = b
    in Block $ Set.map (Channel) $ Set.fromList [s .. e]

data InterleavedBlock= InterleavedBlock { 
    start :: Channel,
    end   :: Channel, 
    off   :: Int }

fromInterleaved :: InterleavedBlock -> Block
fromInterleaved ib = 
    let (Channel s) = start ib
        (Channel e) = end   ib 
        o = off   ib 
    in Block $ Set.map (Channel) $ Set.fromList [s,s + o .. e]

newtype Allocation = Allocation (HashMap User Block)
