module Node (NodeArr (..)) where

import Bucket

import Data.Array.IArray as Array

-- NODEARR is an array of nodes
newtype NodeArr = NodeArr (Array Int BucketArr) deriving (Show)


