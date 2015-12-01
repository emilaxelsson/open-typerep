module Simple where



import Control.Monad

import Data.TypeRep
import Data.TypeRep.Types.Basic
import Data.TypeRep.Types.Basic.Typeable ()



-- A universe of three types
type MyUniverse = IntType :+: FloatType :+: BoolType

-- A list with dynamically typed elements
hlist :: [Dynamic MyUniverse]
hlist = [toDyn True, toDyn (1 :: Int)]

-- Dynamically typed addition for any numeric type
addDyn :: (TypeEq ts ts, PWitness Num ts ts) =>
    Dynamic ts -> Dynamic ts -> Either String (Dynamic ts)
addDyn (Dyn ta a) (Dyn tb b) = do
    Dict <- typeEqM ta tb
    Dict <- pwit pNum ta
    return (Dyn ta (a+b))

test2 = toDyn (1 :: Int)   `addDyn` toDyn (2 :: Int)
test3 = toDyn (3 :: Float) `addDyn` toDyn (4 :: Float)

main = do
    unless t1 $ fail "Test 1 failed"
    unless t2 $ fail "Test 2 failed"
    unless t3 $ fail "Test 3 failed"
    putStrLn "All tests passed"
  where
    t1 = show hlist == "[True,1]"
    t2 = show (test2 :: Either String (Dynamic MyUniverse)) == "Right 3"
    t3 = show (test3 :: Either String (Dynamic MyUniverse)) == "Right 7.0"

