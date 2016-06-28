{-# language BangPatterns #-} 

module Label (Label (..),
              ScoredLabel (..),
              dominates,
              augment) where

import Model
import Data.List as List

-- LABELS 
-- Labels are made to store a partial solution 
-- to the problem
-- They contain the set of users allocated along
-- the current path
data Label = Label {
    score :: Float,
    users :: [User] }
    deriving (Eq, Show)

dominates :: Label -> Label -> Bool
dominates !l !h = let c1 = (score l) >= (score h)
                      c2 = (users l) `List.isSubsequenceOf` (users h)
                  in c1 && c2

instance Ord Label where
    (<=) !l !h = h `dominates` l

instance Monoid Label where
    mempty = Label 0 []
    mappend x y = Label sc us
        where sc = (score x) + (score y)
              us = union (users x) (users y)

-- get candidates to add
candidates :: [User] -> [User] -> [User]
candidates = (\\)

-- augment a solution with one new user
augment :: UtilityF -> Label -> User -> Block -> Label
augment (UtilityF util) l u b = 
    let us = [u]
        sc = util u b
    in mappend l (Label sc us)

-- labels ordered by score only
newtype ScoredLabel = SLabel (Label) deriving (Eq, Show)

instance Ord ScoredLabel where
    compare (SLabel a) (SLabel b) = compare (score a) (score b)

