module Block (NChannels (..),
              Channel (..),
              mkChannels,
              NUsers (..),
              User (..),
              mkUsers,
              Block (..),
              Allocation (..)) where

import Data.Set as Set
import Data.Ix 
import Data.HashMap.Lazy as HashMap

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
