module Bucket (Bucket (..),
               BucketArr (..),
               insertInBucket
               ) where

import Model
import Label

import Data.Set as Set
import Data.Array.IArray as Array

-- BUCKETS : Sets of Labels
newtype Bucket = Bucket (Set Label) deriving (Show)

instance Monoid Bucket where
    mempty  = Bucket (Set.empty)
    mappend (Bucket a) (Bucket b) = 
        Bucket (Set.union a b)

insertInBucket :: Label -> Bucket -> Bucket
insertInBucket l (Bucket labels) = Bucket (Set.insert l labels)

-- BUCKETARRAY is an array of buckets
-- Could also be called a Node
newtype BucketArr = BucketArr (Array Int Bucket) deriving (Show)
