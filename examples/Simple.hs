import Data.TypeRep

type MyUniverse = IntType :+: BoolType

hlist :: [Dynamic MyUniverse]
hlist = [toDyn True, toDyn (1 :: Int)]
  -- Prints: [True,1]

addDyn :: (TypeEq ts ts, PWitness Num ts ts) => Dynamic ts -> Dynamic ts -> Maybe (Dynamic ts)
addDyn (Dyn ta a) (Dyn tb b) = do
    Dict <- typeEq ta tb
    Dict <- pwit pNum ta
    return (Dyn ta (a+b))


test1 = toDyn (1 :: Int) `addDyn` toDyn (2 :: Int)
  -- Prints: Just 3

main = do
    print hlist
    print (test1 :: Maybe (Dynamic MyUniverse))

