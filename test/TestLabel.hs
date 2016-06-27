module TestLabel (
    prop_domination_reflexive,
    prop_domination_antisymmetric,
    prop_domination_transitive,
    prop_noBetter,
    prop_domination_total
) where

import Label

import Data.Set as Set

import Test.QuickCheck

import TestModel

-- LABELS 
-- Labels are made to store a partial solution 
-- to the problem
-- They contain the set of users allocated along
-- the current path
instance Arbitrary Label where
    arbitrary = do
        sc <- arbitrary
        us <- arbitrary
        return (Label sc us)

-- Checking that domination is an ordering relation
prop_domination_reflexive x = x `dominates` x

prop_domination_antisymmetric x y = 
    x /= y ==> x `dominates` y || y `dominates` x
    -- x `dominates` y && y `dominates` x ==> x == y

prop_domination_transitive x y z = x `dominates` y && y `dominates` z ==> x `dominates` z

-- Also checking that domination is a total order
prop_domination_total x y = x `dominates` y || y `dominates` x

prop_noBetter :: Set Label -> Label -> Bool
prop_noBetter s l = 
    let b1 = not (any (\h -> h `dominates` l) s)
        -- b2 = Set.null s || (l `dominates` (findMax s))
        b2 = all (\h -> not (h `dominates` l)) s
    in b1 == b2
    

-- Labels are a monoids because Floats are monoids under sum and 
-- set are monoids under union
