import Control.Monad

import Data.TypeRep
import Data.TypeRep.Types
import Data.TypeRep.TypeableInstances ()

type MyUniverse = IntType :+: BoolType

hlist :: [Dynamic MyUniverse]
hlist = [toDyn True, toDyn (1 :: Int)]
  -- Prints: [True,1]

addDyn :: (TypeEq ts ts, PWitness Num ts ts) =>
    Dynamic ts -> Dynamic ts -> Either String (Dynamic ts)
addDyn (Dyn ta a) (Dyn tb b) = do
    Dict <- typeEq ta tb
    Dict <- pwit pNum ta
    return (Dyn ta (a+b))



test1 = toDyn (1 :: Int) `addDyn` toDyn (2 :: Int)
  -- Prints: Just 3

main = do
    unless t1 $ fail "Test 1 failed"
    unless t2 $ fail "Test 2 failed"
    putStrLn "All tests passed"
  where
    t1 = show hlist == "[True,1]"
    t2 = show (test1 :: Either String (Dynamic MyUniverse)) == "Right 3"

