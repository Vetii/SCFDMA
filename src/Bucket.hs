module Bucket (Bucket (..),
               BucketArr (..),
               Bucket.foldr,
               Bucket.null,
               insert,
               size,
               minimumBy,
               maximumBy,
               delete,
               toList
               ) where

import Model
import Label

import qualified Data.List as List
import Data.Array.IArray as Array

-- BUCKETS : Sets of Labels
newtype Bucket = Bucket ([Label]) deriving (Show)

instance Monoid Bucket where
    mempty  = Bucket ([])
    mappend (Bucket a) (Bucket b) = 
        Bucket (List.union a b)

foldr :: (Label -> b -> b) -> b -> Bucket -> b
foldr f acc (Bucket l) = List.foldr f acc l

null :: Bucket -> Bool
null (Bucket []) = True
null (Bucket _)  = False

insert :: Label -> Bucket -> Bucket
insert l (Bucket labels) = Bucket (l : labels)

size :: Bucket -> Int
size (Bucket b) = length b

sortLabelsBy :: (Label -> Label -> Ordering) -> Bucket -> Bucket
sortLabelsBy ordering (Bucket lbls) = Bucket (List.sortBy ordering lbls)

head :: Bucket -> Label
head (Bucket l) = List.head l

last :: Bucket -> Label
last (Bucket l) = List.last l

minimumBy :: (Label -> Label -> Ordering) -> Bucket -> Label
minimumBy f = Bucket.head . sortLabelsBy f

maximumBy :: (Label -> Label -> Ordering) -> Bucket -> Label
maximumBy f = Bucket.last . sortLabelsBy f

delete :: Label -> Bucket -> Bucket
delete l (Bucket b) = Bucket (List.delete l b)

toList :: Bucket -> [Label]
toList (Bucket l) = l

-- BUCKETARRAY is an array of buckets
-- Could also be called a Node
newtype BucketArr = BucketArr (Array Int Bucket) deriving (Show)
