{-# language BangPatterns #-}
module GLA (runGLA) where

import Debug.Trace

import Model
import Label
import Bucket
import Node
import Block

import Data.Set as Set
import Data.Ord
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
                    putBucket j 0 (Bucket [mempty]))

-- insert a label in a bucket (With additional precautions)
insert :: Label -> Bucket -> Int -> Bucket
insert l b@(Bucket labels) k =
    let sz = Bucket.size b
        res= Bucket.insert l b
    in if sz < k then res
       else
        -- get the label with lowest score
        let low = Bucket.minimumBy (comparing score) b
        in (if (score l) > (score low) then
                let withoutLow = Bucket.delete low b
                in Bucket.insert l withoutLow
            else b)

isBestLabel :: Label -> BucketArr -> Bool
isBestLabel l (BucketArr buckets) = 
    let hasNoBetterLabel (Bucket b) = all (\h -> not (h `dominates` l)) b
    -- (Set.null b) || (l `dominates` (findMax b)) # DOMINATION IS NOT A LINEAR ORDER :(
    --not (any (\h -> h `dominates` l) b)
    in all hasNoBetterLabel buckets
    
insertLabels :: NBuckets -> Bucket -> Int -> Int -> State NodeArr ()
insertLabels (NBuckets k) b' j m = 
    forM_ (Bucket.toList b') (\l -> do
        -- Get buckets at node j
        buckets <- getBucketArr j
        -- check if l is the best label among all buckets
        let match = l `isBestLabel` buckets
        -- if yes, insert it into the current bucket
        if match then do
            bucket <- getBucket j m
            let newB = (GLA.insert l bucket k)
            putBucket j m newB
        else return ()
    )

allocBlock :: Block -> Bucket -> Bucket -> UtilityF -> Set User -> Bucket
allocBlock block save prevB uf allUsers =
    Bucket.foldr (\l l' -> 
        let newUsers = Set.difference allUsers (Set.fromList (users l))
        in Set.fold (\w b -> Bucket.insert (augment uf l w block) b) l' newUsers
    ) save prevB

gla :: AlgoConfig -> State NodeArr Label
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
        let b = mconcat buckets
        if Bucket.null b then 
            fail "Labels empty"
        else do
            let lBest = maximumBy (comparing score) b
            return lBest

runGLA :: Configuration -> (Label, NodeArr)
runGLA c = 
    let m = nusers c
        n = nchannels c
    in runState (gla (mkParams c)) (initGLA m n)
