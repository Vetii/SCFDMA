import Test.QuickCheck

import TestLabel
import TestModel

-- Check that the build that I do contains only totally ordered labels


main :: IO ()
main = do
    
    quickCheck prop_domination_reflexive
    -- quickCheck prop_domination_total
    -- quickCheck prop_domination_antisymmetric
    -- quickCheck prop_domination_transitive 

