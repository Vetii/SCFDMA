{-# language FlexibleInstances #-}

module JSON (NUsers (..),
             NChannels (..),
             NBuckets (..),
             Bandwidth (..),
             NPSD (..),
             UserPowerLimit (..),
             PeakPowerLimit (..),
             GainMatrix (..),
             Configuration (..)) where

import Model
import Decibel
import Block
import Control.Monad
import GHC.Generics
import Text.JSON
import Data.Array as  Array

instance JSON NUsers where
    readJSON (JSRational _ i) = 
        NUsers <$> pure (round i)
    readJSON _ = mzero
    showJSON (NUsers m) = showJSON m

instance JSON NChannels where
    readJSON (JSRational _ i) = 
        NChannels <$> pure (round i)
    readJSON _ = mzero
    showJSON (NChannels n) = showJSON n

instance JSON NBuckets where
    readJSON (JSRational _ i) = 
        NBuckets <$> pure (round i)
    readJSON _ = mzero
    showJSON (NBuckets n) = showJSON n

instance JSON Bandwidth where
    readJSON (JSRational _ i) = 
        Bandwidth <$> pure (fromRational i)
    readJSON _ = mzero
    showJSON (Bandwidth b) = showJSON b

instance JSON (Decibel Float) where
    readJSON (JSRational _ i) = 
        mkDecibel <$> pure (fromRational i)
    readJSON _ = mzero
    showJSON x = showJSON (fromDecibel x)

instance JSON NPSD where
    readJSON (JSRational b i) = 
        NPSD <$>  (readJSON (JSRational b i))
    readJSON _ = mzero
    showJSON (NPSD b) = showJSON b

instance JSON UserPowerLimit where
    readJSON (JSRational _ i) = UserPowerLimit <$> pure (fromRational i)
    readJSON _ = mzero
    showJSON (UserPowerLimit p) = showJSON p

instance JSON PeakPowerLimit where
    readJSON (JSRational _ i) = PeakPowerLimit <$> pure (fromRational i)
    readJSON _ = mzero
    showJSON (PeakPowerLimit b) = showJSON b

instance JSON GainMatrix where
    readJSON (JSArray arr) = 
        GainMatrix <$> mapM readJSON arr
    readJSON _ = mzero
    showJSON (GainMatrix g) = showJSON g

instance JSON Configuration where
    readJSON (JSObject obj) = 
        (Configuration <$>
        valFromObj "number of users" obj              <*>
        valFromObj "number of channels" obj           <*>
        valFromObj "number of buckets" obj            <*>
        valFromObj "bandwidth" obj                    <*>
        valFromObj "noise power spectral density" obj <*>
        valFromObj "user power limit" obj             <*>
        valFromObj "channel peak power limit" obj     <*>
        valFromObj "gain" obj                         )
        >>= checkRows >>= checkCols 
    readJSON _ = mzero
    showJSON (Configuration m n k b npsd upl pkpl g) = makeObj [
        ( "number of users"             , showJSON m    ),
        ( "number of channels"          , showJSON n    ),
        ( "number of buckets"           , showJSON k    ),
        ( "bandwidth"                   , showJSON b    ),
        ( "noise power spectral density", showJSON npsd ),
        ( "user power limit"            , showJSON upl  ),
        ( "channel peak power limit"    , showJSON pkpl ),
        ( "gain"                        , showJSON g)]                       

checkRows :: Configuration -> Result Configuration
checkRows c = do 
    let (NUsers m) = nusers c
        (GainMatrix mat) = gainMT c
    if (length mat == m) then
        return c
    else 
        let msg = "Gain matrix does not have " ++ (show m) ++ " rows"
        in Error msg

checkCols :: Configuration -> Result Configuration
checkCols c = do 
    let (NChannels n) = nchannels c
        (GainMatrix mat) = gainMT c
    if (all (\row -> length row == n) mat) then
        return c
    else 
        let msg = "Gain matrix does not have " ++ (show n) ++ " columns"
        in Error msg

