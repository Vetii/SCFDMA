{-# language BangPatterns #-}
module GLA (runGLA) where

import Debug.Trace

import Model
import Label
import Bucket
import Node

import Data.Set as Set
import Data.Array.IArray as Array
import Control.Monad
import Control.Monad.State

getBucketArr :: Int -> State NodeArr BucketArr
getBucketArr j = state (\n@(NodeArr nodes) ->
    (nodes ! j, n))

-- get Bucket in node j and bucket m
getBucket :: Int -> Int -> State NodeArr Bucket
getBucket j m = state (\n@(NodeArr nodes) -> 
    let BucketArr node = nodes ! j
    in ((node ! m), n))

-- put bucket in node j and bucket m
putBucket :: Int -> Int -> Bucket -> State NodeArr ()
putBucket j m b = state (\n@(NodeArr nodes) ->
    let BucketArr node = nodes ! j
        newnode = BucketArr $! (node // [(m, b)])
        newgraph= NodeArr   $! (nodes // [(j, newnode)])
    in ((), newgraph))


-- GLA ALGORITHM 
initGLA :: NUsers -> NChannels -> NodeArr
initGLA (NUsers m) (NChannels n) = snd (runState init start)
     where buckets = BucketArr (listArray (0, m) (repeat mempty))
           start = NodeArr (listArray (0, n) (repeat buckets))
           init = forM_ [0 .. n] (\j -> do
                    putBucket j 0 (Bucket (Set.singleton mempty)))

-- insert a label in a bucket
insert :: Label -> Bucket -> Int -> Bucket
insert l b@(Bucket labels) k =
    let sz = Set.size labels
        res= insertInBucket l b
    in if sz < k then res
       else
        -- get the label with lowest score
        let (SLabel low) = Set.findMin (Set.map (SLabel) labels) 
        in (if (score l) > (score low) then
                let withoutLow = Set.delete low labels
                in insertInBucket l (Bucket withoutLow)
            else b)

isBestLabel :: Label -> BucketArr -> Bool
isBestLabel l (BucketArr buckets) = 

    let hasNoBetterLabel (Bucket b) = (Set.null b) || (l `dominates` (findMax b))
    --not (any (\h -> h `dominates` l) b)
    in all hasNoBetterLabel buckets
    
insertLabels :: NBuckets -> Bucket -> Int -> Int -> State NodeArr ()
insertLabels (NBuckets k) (Bucket b') j m = 
    forM_ (Set.toList b') (\l -> do
        -- Get buckets at node j
        buckets <- getBucketArr j
        let match = l `isBestLabel` buckets
        if match then do
            bucket <- getBucket j m
            let newB = (GLA.insert l bucket k)
            putBucket j m newB
        else return ()
    )

allocBlock :: Block -> Bucket -> Bucket -> UtilityF -> Set User -> Bucket
allocBlock block save prevB@(Bucket prevL) uf allUsers =
    Set.fold (\l l' -> 
        let newUsers = Set.difference allUsers (users l)
        in Set.fold (\w (Bucket labels) -> 
           Bucket (Set.insert (augment uf l w block) labels)) l' newUsers
    ) save prevL

gla :: AlgoConfig -> State NodeArr ScoredLabel
gla !config = 
    let users = allUsers config
        channels = allChannels config 
        n = Set.size channels
        m = Set.size users
        u = utilityF config
        k = bucketSize config
    in do 
        forM_ [1 .. n] (\j -> do -- for all channels
            forM_ [1 .. (min j m)] (\m' -> do -- for all users allocated so far
                forM_ [0 .. (j - 1)] (\i -> do -- for all previous channels
                    l'  <- getBucket i m'
                    pre@(Bucket labels) <- getBucket i (m' - 1)
                    let startC = Channel (i + 2)
                        endC   = Channel j
                        block  = Block (Set.fromList [startC .. endC])
                        newL   = allocBlock block l' pre u users
                    insertLabels k newL j m'
                    )
                )
            )
        let userList = Prelude.map (fromEnum) (Set.toList users)
        buckets <- sequence (Prelude.map (getBucket n) userList)
        let (Bucket labels) = mconcat buckets
        let lBest = Set.findMax (Set.map (SLabel) labels)
        let tr = Debug.Trace.trace "result" lBest
        return lBest

runGLA :: Configuration -> (ScoredLabel, NodeArr)
runGLA c = 
    let m = nusers c
        n = nchannels c
    in runState (gla (mkParams c)) (initGLA m n)
