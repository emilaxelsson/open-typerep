{-# LANGUAGE CPP #-}
{-# LANGUAGE TemplateHaskell #-}

module Data.TypeRep.TH
  ( deriveRender_forType
  , deriveTypeEq
  , deriveWitness
  , derivePWitness
  , deriveWitnessAny
  , derivePWitnessAny
  , deriveWitnessTypeable
  , derivePWitnessTypeable
  ) where



import Data.Proxy
import Language.Haskell.TH

import Control.Monad.Except
import Data.Constraint (Dict (..))

import Language.Syntactic
import Language.Syntactic.TH

import Data.TypeRep.Representation



-- | Match on a 'Pred' of the form @(t1 ~ t2)@
viewEqPred :: Pred -> Maybe (Type,Type)
#if MIN_VERSION_template_haskell(2,10,0)
viewEqPred (AppT (AppT EqualityT t1) t2) = Just (t1,t2)
#else
viewEqPred (EqualP t1 t2) = Just (t1,t2)
#endif
viewEqPred _ = Nothing
  -- This function is just here to provide compatibility with
  -- template-haskell < 2.10

-- | Construct a 'Pred' of the form @(Cl t1 t2 ...)@
mkClassPred :: Name -> [Type] -> Pred
#if MIN_VERSION_template_haskell(2,10,0)
mkClassPred cl ts = foldl1 AppT (ConT cl : ts)
#else
mkClassPred cl ts = ClassP cl ts
#endif
  -- This function is just here to provide compatibility with
  -- template-haskell < 2.10

tyVarName :: TyVarBndr -> Name
tyVarName (PlainTV v)    = v  -- Only needed on GHC < 7.10
tyVarName (KindedTV v _) = v



indent :: Int -> String -> String
indent n = unlines . map (replicate n ' ' ++) . lines

-- | Throw an error stating that the given type wasn't declared on the form
--
-- > data SymType sig where
-- >   ...
-- >   ThisSym :: SymType (a :-> ... :-> Full x)
-- >   ...
errorDerive
    :: String  -- ^ Function where error occurred
    -> Info    -- ^ Info about type
    -> a
errorDerive fun info = error $ unlines
    [ "------ " ++ fun ++ ": can only handle types declared on the form ----"
    , ""
    , "        data SymType sig where"
    , "          ..."
    , "          ThisSym :: SymType (a :-> ... :-> Full x)"
    , "          ..."
    , ""
    , "      ------ This is what I got: ------"
    , ""
    , indent 8 $ pprint info
    ]

-- | Get the arity of a symbol. If the type is not declared according to what
-- is stated for 'errorDerive', 'Nothing' is returned.
symArity
    :: Name  -- ^ Type parameter
    -> Con   -- ^ Symbol
    -> Maybe Int
symArity sigVar (ForallC _ [cxt] (NormalC _ []))
    | Just (VarT sigVar', sig) <- viewEqPred cxt
    , sigVar == sigVar'
    = count sig
  where
    count :: Type -> Maybe Int
    count (AppT (AppT arrow _) res)
        | arrow == ConT ''(:->) = fmap (+1) $ count res
    count (AppT (ConT full) _)
        | full == ''Full        = Just 0
    count _ = Nothing
symArity _ _ = Nothing

-- | Construct a pattern @v `:*` pat@
argConsP :: Name -> Pat -> Pat
argConsP v rest = InfixP (VarP v) '(:*) rest

-- | Construct a predicate proxy @`Proxy` :: `Proxy` Pred@
mkPredProxy :: Type -> Exp
mkPredProxy pred = SigE (ConE 'Proxy) (AppT (ConT ''Proxy) pred)

-- Generate an expression of the form
--
-- > case wit (Proxy :: Proxy Pred) (TypeRep v) of Dict -> result
--
-- The class is given as a 'Type' because
support
    :: Type  -- ^ Type predicate (e.g. 'Eq' or @(`Typeable` t)@)
    -> Name  -- ^ Variable for type representation
    -> Exp   -- ^ Result
    -> Exp
support pred v res = CaseE
    (foldl1 AppE [VarE 'wit, mkPredProxy pred, AppE (ConE 'TypeRep) (VarE v)])
    [Match (ConP 'Dict []) (NormalB res) []]

-- | A type variable named @t@
tVar :: Type
tVar = VarT $ mkName "t"

-- | "abc_dfg" -> "abc"
typeName :: String -> String
typeName = takeWhile (/='_')



--------------------------------------------------------------------------------
-- * Derivers
--------------------------------------------------------------------------------

-- | A version of 'deriveRender' that applies 'typeName' to each constructor
-- name. That is, e.g. the constructor @Int_t :: IntType (Full Int)@ will be
-- rendered as \"Int\".
deriveRender_forType
    :: Name  -- ^ Type name
    -> DecsQ
deriveRender_forType = deriveRender typeName

-- | Derive 'TypeEq' instance for a type representation
deriveTypeEq
    :: Name  -- ^ Type name
    -> DecsQ
deriveTypeEq ty = do
    info <- reify ty
    case info of
        TyConI (DataD _ _ [sigVarTV] cs _) -> do
            throwErrExp <- [| throwError "" |]
              -- `typeEq` will turn this into a proper error message
            let sigVar = tyVarName sigVarTV
            let maxArity = case mapM (symArity sigVar) cs of
                  Just as -> maximum (0:as)
                  Nothing -> errorDerive "deriveTypeEq" info
            let classCxt = if maxArity == 0
                  then []
                  else [mkClassPred ''TypeEq [tVar, tVar]]
            let typeEqSymFallThrough = if length cs > 1
                  then [Clause [WildP, WildP] (NormalB throwErrExp) []]
                  else []
            let mkClause c i n a = case typeEqSymClause sigVar c i n a of
                  Just clause -> clause
                  Nothing -> errorDerive "deriveTypeEq" info
            deriveClass classCxt ty
                (foldl1 AppT [ConT ''TypeEq, ConT ty, tVar])
                [MatchingMethod 'typeEqSym mkClause typeEqSymFallThrough]
        _ -> errorDerive "deriveTypeEq" info
  where
    typeEqSymClause sigVar con _ name 0 = do
        arity <- symArity sigVar con
        let vs1    = take arity varSupply
            vs2    = take arity $ drop arity varSupply
            argsP1 = foldr argConsP (ConP 'Nil []) vs1
            argsP2 = foldr argConsP (ConP 'Nil []) vs2
            checkArgs v1 v2 = foldl1 AppE
                  [ VarE 'typeEq
                  , AppE (ConE 'TypeRep) (VarE v1), AppE (ConE 'TypeRep) (VarE v2)
                  ]
              -- typeEq (TypeRep v1) (TypeRep v2)
            eqArgs  = [BindS (ConP 'Dict []) $ checkArgs v1 v2 | (v1,v2) <- zip vs1 vs2]
            retStmt = NoBindS $ AppE (VarE 'return) (ConE 'Dict)
        return $ Clause
          [ TupP [ConP name [], argsP1]
          , TupP [ConP name [], argsP2]
          ]
          (NormalB $ DoE (eqArgs ++ [retStmt]))
          []
    typeEqSymClause _ _ _ _ _ = Nothing

-- | Derive 'Witness' instance for a type representation
--
-- > instance Witness Cl t t => Witness Cl Ty t where
-- >   witSym Con1 Nil = Dict
-- >   witSym Con2 (a :* b :* Nil) =
-- >       case wit (Proxy :: Proxy Cl) (TypeRep a) of
-- >         Dict -> case wit (Proxy :: Proxy Cl) (TypeRep b) of
-- >           Dict -> Dict
deriveWitness
    :: Name  -- ^ Class name
    -> Name  -- ^ Type name
    -> DecsQ
deriveWitness cl ty = do
    info <- reify ty
    case info of
        TyConI (DataD _ _ [sigVarTV] cs _) -> do
            let sigVar = tyVarName sigVarTV
            let maxArity = case mapM (symArity sigVar) cs of
                  Just as -> maximum (0:as)
                  Nothing -> errorDerive "deriveWitness" info
            let classCxt = if maxArity == 0
                  then []
                  else [mkClassPred ''Witness [ConT cl, tVar, tVar]]
            let mkClause c i n a = case witSymClause sigVar c i n a of
                  Just clause -> clause
                  Nothing -> errorDerive "deriveWitness" info
            deriveClass classCxt ty
              (foldl1 AppT [ConT ''Witness, ConT cl, ConT ty, tVar])
              [MatchingMethod 'witSym mkClause []]
  where
    pred = ConT cl
    witSymClause sigVar con _ name 0 = do
        arity <- symArity sigVar con
        let vs    = take arity varSupply
            argsP = foldr argConsP (ConP 'Nil []) vs
        return $ Clause
          [ConP name [], argsP]
          (NormalB $ foldr (support pred) (ConE 'Dict) vs)
          []

-- | Derive 'PWitness' instance for a type representation
--
-- > instance PWitness Cl t t => PWitness Cl Ty t where
-- >   pwitSym Con1 Nil = return Dict
-- >   pwitSym Con2 (a :* b :* Nil) = do
-- >       Dict <- pwit (Proxy :: Proxy Cl) (TypeRep a)
-- >       Dict <- pwit (Proxy :: Proxy Cl) (TypeRep b)
-- >       return Dict
derivePWitness
    :: Name  -- ^ Class name
    -> Name  -- ^ Type name
    -> DecsQ
derivePWitness cl ty = do
    info <- reify ty
    case info of
        TyConI (DataD _ _ [sigVarTV] cs _) -> do
            let sigVar = tyVarName sigVarTV
            let maxArity = case mapM (symArity sigVar) cs of
                  Just as -> maximum (0:as)
                  Nothing -> errorDerive "derivePWitness" info
            let classCxt = if maxArity == 0
                  then []
                  else [mkClassPred ''PWitness [ConT cl, tVar, tVar]]
            let mkClause c i n a = case pwitSymClause sigVar c i n a of
                  Just clause -> clause
                  Nothing -> errorDerive "derivePWitness" info
            deriveClass classCxt ty
              (foldl1 AppT [ConT ''PWitness, ConT cl, ConT ty, tVar])
              [MatchingMethod 'pwitSym mkClause []]
  where
    pred = ConT cl
    pwitSymClause sigVar con _ name 0 = do
        arity <- symArity sigVar con
        let vs        = take arity varSupply
            argsP     = foldr argConsP (ConP 'Nil []) vs
            pwitArg v = foldl1 AppE [VarE 'pwit, mkPredProxy pred, AppE (ConE 'TypeRep) (VarE v)]
            pwitArgs  = [BindS (ConP 'Dict []) $ pwitArg v | v <- vs]
            retStmt   = NoBindS $ AppE (VarE 'return) (ConE 'Dict)
        return $ Clause
          [ConP name [], argsP]
          (NormalB $ DoE (pwitArgs ++ [retStmt]))
          []

-- | Derive @`Witness` `Any`@ instance for a type representation
--
-- > instance Witness Any Ty t where
-- >   witSym _ _ = Dict
-- >   witSym _ _ = Dict
deriveWitnessAny
    :: Name  -- ^ Type name
    -> DecsQ
deriveWitnessAny ty = do
    deriveClass [] ty
      (foldl1 AppT [ConT ''Witness, ConT ''Any, ConT ty, tVar])
      [MatchingMethod 'witSym witSymClause []]
  where
    witSymClause _ _ con 0 = Clause
        [WildP, WildP]
        (NormalB $ ConE 'Dict)
        []

-- | Derive @`PWitness` `Any`@ instance for a type representation
--
-- > instance PWitness Any Ty t where
-- >   pwitSym _ _ = return Dict
-- >   pwitSym _ _ = return Dict
derivePWitnessAny
    :: Name  -- ^ Type name
    -> DecsQ
derivePWitnessAny ty = do
    deriveClass [] ty
      (foldl1 AppT [ConT ''PWitness, ConT ''Any, ConT ty, tVar])
      [MatchingMethod 'pwitSym witSymClause []]
  where
    witSymClause _ _ con 0 = Clause
        [WildP, WildP]
        (NormalB $ AppE (VarE 'return) (ConE 'Dict))
        []

-- | Derive @`Witness` (`Typeable` Ty)@ instance for a type representation
--
-- > instance (Ty :<: t) => Witness (Typeable t) Ty t where
-- >   witSym Con1 Nil = Dict
-- >   witSym Con2 (a :* b :* Nil) =
-- >       case wit (Proxy :: Proxy (Typeable t)) (TypeRep a) of
-- >         Dict -> case wit (Proxy :: Proxy (Typeable t)) (TypeRep b) of
-- >           Dict -> Dict
deriveWitnessTypeable
    :: Name  -- ^ Type name
    -> DecsQ
deriveWitnessTypeable ty = do
    info <- reify ty
    case info of
        TyConI (DataD _ _ [sigVarTV] cs _) -> do
            let sigVar = tyVarName sigVarTV
            let maxArity = case mapM (symArity sigVar) cs of
                  Just as -> maximum (0:as)
                  Nothing -> errorDerive "deriveWitnessTypeable" info
            let sub = mkClassPred ''(:<:) [ConT ty, tVar]
            let classCxt = if maxArity == 0
                  then [sub]
                  else [sub, mkClassPred ''Witness [AppT (ConT cl) tVar, tVar, tVar]]
            let mkClause c i n a = case witSymClause sigVar c i n a of
                  Just clause -> clause
                  Nothing -> errorDerive "deriveWitnessTypeable" info
            deriveClass classCxt ty
              (foldl1 AppT [ConT ''Witness, AppT (ConT cl) tVar, ConT ty, tVar])
              [MatchingMethod 'witSym mkClause []]
  where
    cl   = ''Typeable
    pred = AppT (ConT cl) tVar
    witSymClause sigVar con _ name 0 = do
        arity <- symArity sigVar con
        let vs    = take arity varSupply
            argsP = foldr argConsP (ConP 'Nil []) vs
        return $ Clause
          [ConP name [], argsP]
          (NormalB $ foldr (support pred) (ConE 'Dict) vs)
          []

-- | Derive @`PWitness` (`Typeable` Ty)@ instance for a type representation
--
-- > instance (Ty :<: t) => PWitness (Typeable t) Ty t where
-- >   pwitSym Con1 Nil = return Dict
-- >   pwitSym Con2 (a :* b :* Nil) = do
-- >       Dict <- pwit (Proxy :: Proxy (Typeable t)) (TypeRep a)
-- >       Dict <- pwit (Proxy :: Proxy (Typeable t)) (TypeRep b)
-- >       return Dict
derivePWitnessTypeable
    :: Name  -- ^ Type name
    -> DecsQ
derivePWitnessTypeable ty = do
    info <- reify ty
    case info of
        TyConI (DataD _ _ [sigVarTV] cs _) -> do
            let sigVar = tyVarName sigVarTV
            let maxArity = case mapM (symArity sigVar) cs of
                  Just as -> maximum (0:as)
                  Nothing -> errorDerive "derivePWitnessTypeable" info
            let sub = mkClassPred ''(:<:) [ConT ty, tVar]
            let classCxt = if maxArity == 0
                  then [sub]
                  else [sub, mkClassPred ''PWitness [AppT (ConT cl) tVar, tVar, tVar]]
            let mkClause c i n a = case pwitSymClause sigVar c i n a of
                  Just clause -> clause
                  Nothing -> errorDerive "derivePWitnessTypeable" info
            deriveClass classCxt ty
              (foldl1 AppT [ConT ''PWitness, AppT (ConT cl) tVar, ConT ty, tVar])
              [MatchingMethod 'pwitSym mkClause []]
  where
    cl   = ''Typeable
    pred = AppT (ConT cl) tVar
    pwitSymClause sigVar con _ name 0 = do
        arity <- symArity sigVar con
        let vs        = take arity varSupply
            argsP     = foldr argConsP (ConP 'Nil []) vs
            pwitArg v = foldl1 AppE [VarE 'pwit, mkPredProxy pred, AppE (ConE 'TypeRep) (VarE v)]
            pwitArgs  = [BindS (ConP 'Dict []) $ pwitArg v | v <- vs]
            retStmt   = NoBindS $ AppE (VarE 'return) (ConE 'Dict)
        return $ Clause
          [ConP name [], argsP]
          (NormalB $ DoE (pwitArgs ++ [retStmt]))
          []

