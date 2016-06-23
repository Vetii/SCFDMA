{-# language BangPatterns #-} 

module Label (Label (..),
              ScoredLabel (..),
              dominates,
              augment) where

import Model
import Data.Set as Set

-- LABELS 
-- Labels are made to store a partial solution 
-- to the problem
-- They contain the set of users allocated along
-- the current path
data Label = Label {
    score :: Float,
    users :: Set User }
    deriving (Eq, Show)

dominates :: Label -> Label -> Bool
dominates !l !h = let c1 = (score l) >= (score h)
                      c2 = (users l) `Set.isSubsetOf` (users h)
                  in c1 && c2

instance Ord Label where
    (<=) !l !h = h `dominates` l

instance Monoid Label where
    mempty = Label 0 Set.empty
    mappend x y = Label sc us
        where sc = (score x) + (score y)
              us = Set.union (users x) (users y)

-- get candidates to add
candidates :: Set User -> Set User -> Set User 
candidates = Set.difference

-- augment a solution with one new user
augment :: UtilityF -> Label -> User -> Block -> Label
augment (UtilityF util) l u b = 
    let us = Set.singleton u
        sc = util u b
    in mappend l (Label sc us)

-- labels ordered by score only
newtype ScoredLabel = SLabel (Label) deriving (Eq, Show)

instance Ord ScoredLabel where
    compare (SLabel a) (SLabel b) = compare (score a) (score b)

