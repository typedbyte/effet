{-# LANGUAGE TemplateHaskell #-}
-----------------------------------------------------------------------------
-- |
-- Module      :  Control.Effect.Machinery.TH
-- Copyright   :  (c) Michael Szvetits, 2020
-- License     :  BSD3 (see the file LICENSE)
-- Maintainer  :  typedbyte@qualified.name
-- Stability   :  stable
-- Portability :  portable
--
-- This module provides @TemplateHaskell@ functions to generate the handling,
-- lifting and tagging infrastructure for effect type classes.
-----------------------------------------------------------------------------
module Control.Effect.Machinery.TH
  ( -- * Common Generators
    makeEffect
  , makeHandler
  , makeLifter
    -- * Tag-based Generators
  , makeTaggedEffect
  , makeTaggedEffectWith
  , makeTagger
  , makeTaggerWith
    -- * Naming Convention
  , removeApostrophe
  ) where

-- base
import Control.Monad (forM, replicateM)
import Data.List     (isSuffixOf)
import Data.Maybe    (maybeToList)

-- monad-control
import Control.Monad.Trans.Control (liftWith, restoreT)

-- template-haskell
import Language.Haskell.TH.Lib
import Language.Haskell.TH.Syntax hiding (Lift, lift)

-- transformers
import Control.Monad.Trans.Class (lift)

import Control.Effect.Machinery.Kind   (Control, Handle, Lift)
import Control.Effect.Machinery.Tagger (Tagger, runTagger)
import Control.Effect.Machinery.Via    (G, Via(Via), runVia)

data ClassInfo = ClassInfo
  { clsCxt     :: Cxt
  , clsName     :: Name
  , clsTyVars   :: [TyVarBndr]
  , _clsFunDeps :: [FunDep]
  , clsDecs     :: [Dec]
  }

data EffectInfo = EffectInfo
  { _effCxt       :: Cxt
  , effType      :: Q Type
  , effParams    :: [TyVarBndr]
  , effMonad     :: TyVarBndr
  , effName      :: Name
  , effTrafoName :: Name
  , effSigs      :: [Signature]
  }

data TaggedInfo = TaggedInfo
  { tgTag     :: TyVarBndr
  , tgParams  :: [TyVarBndr]
  , tgMonad   :: TyVarBndr
  , tgEffName :: Name
  , tgNameMap :: String -> Q String
  , tgSigs    :: [Signature]
  }

data Signature = Signature
  { sigName :: Name
  , sigType :: Type
  }

synonymName :: TaggedInfo -> Q Name
synonymName info = mapName (tgNameMap info) (tgEffName info)

resultType :: Name -> Type -> Q Type
resultType m typ =
  case typ of
    VarT n `AppT` a | n == m -> pure a
    ArrowT `AppT` _ `AppT` r -> resultType m r
    ForallT _ _ t            -> resultType m t
    SigT t _                 -> resultType m t
    ParensT t                -> resultType m t
    other -> fail
      $  "Expected a return type of the form 'm a', but encountered: "
      ++ show other

restorables :: Bool -> Name -> Type -> [Type]
restorables neg m typ =
  case typ of
    VarT n `AppT` a | n == m && neg -> [a]
    ArrowT `AppT` a `AppT` r        -> restorables (not neg) m a ++ restorables neg m r
    ForallT _ _ t                   -> restorables neg m t
    SigT t _                        -> restorables neg m t
    ParensT t                       -> restorables neg m t
    other -> fail
      $  "Encountered an unknown term when finding restorables: "
      ++ show other

isHigherType :: TyVarBndr -> Type -> Bool
isHigherType monad = go False
  where
    m = tyVarName monad
    go negPos typ =
      case typ of
        VarT n `AppT` _ | n == m -> negPos
        ArrowT `AppT` a `AppT` r ->
          go (not negPos) a || go negPos r
        ForallT _ _ t ->
          go negPos t
        _ ->
          False

isHigherOrder :: TyVarBndr -> Signature -> Bool
isHigherOrder monad = isHigherType monad . sigType

signature :: Dec -> Q Signature
signature dec =
  case dec of
    SigD name typ ->
      pure (Signature name typ)
    other ->
      fail
         $ "The generation of the effect handling machinery currently supports"
        ++ " only signatures, but encountered: "
        ++ show other

unkindTyVar :: TyVarBndr -> TyVarBndr
unkindTyVar (KindedTV n _) = PlainTV n
unkindTyVar unkinded       = unkinded

tyVarName :: TyVarBndr -> Name
tyVarName (PlainTV  n  ) = n
tyVarName (KindedTV n _) = n

tyVarType :: TyVarBndr -> Q Type
tyVarType (PlainTV n   ) = varT n
tyVarType (KindedTV n k) = sigT (varT n) k

effectVars :: ClassInfo -> Q ([TyVarBndr], TyVarBndr)
effectVars info =
  case clsTyVars info of
    [] -> fail
            $  "The specified effect type class `"
            ++ nameBase (clsName info)
            ++ "' has no monad type variable. "
            ++ "It is expected to be the last type variable."
    vs ->
      pure
        (init vs, last vs)

effectInfo :: ClassInfo -> Q EffectInfo
effectInfo info = do
  (params, clsM) <- effectVars info
  t <- newName "t"
  sigs <- mapM signature (clsDecs info)
  pure $
    EffectInfo
      ( clsCxt info  )
      ( foldl appT (conT $ clsName info) (fmap tyVarType params) )
      ( params       )
      ( clsM         )
      ( clsName info )
      ( t            )
      ( sigs         )

extractTag :: [TyVarBndr] -> Q (TyVarBndr, [TyVarBndr])
extractTag []     = fail "The effect has no tag parameter."
extractTag (v:vs) = pure (v, vs)

-- | Extracts the untagged name from a name which is expected to end with \"\'\".
-- In other words, this function removes the suffix \"\'\" from a given name,
-- or fails otherwise.
removeApostrophe :: String -> Q String
removeApostrophe name =
  if "'" `isSuffixOf` name then
    pure $ init name
  else
    fail $ "Tagged effect and function names are expected to end with \"'\"."

mapName :: (String -> Q String) -> Name -> Q Name
mapName f = fmap mkName . f . nameBase

taggedInfo :: (String -> Q String) -> EffectInfo -> Q TaggedInfo
taggedInfo f info = do
  (tag, params) <- extractTag (effParams info)
  pure $
    TaggedInfo
      ( tag           )
      ( params        )
      ( effMonad info )
      ( effName info  )
      ( f             )
      ( effSigs info  )

classInfo :: Name -> Q ClassInfo
classInfo className = do
  info <- reify className
  case info of
    ClassI (ClassD context name tyVars funDeps decs) _ ->
      pure (ClassInfo context name tyVars funDeps decs)
    other ->
      fail
         $ "The specified name `"
        ++ nameBase className
        ++ "' is not a type class, but the following instead: "
        ++ show other

instanceCxt :: Name -> EffectInfo -> Q Cxt
instanceCxt name info = cxt
  [
    conT name
      `appT` effType info
      `appT` varT (effTrafoName info)
      `appT` tyVarType (effMonad info)
  ]

instanceHead :: Q Type -> EffectInfo -> Q Type
instanceHead eff info =
  effType info
    `appT` (
      conT ''Via
        `appT` eff
        `appT` varT (effTrafoName info)
        `appT` tyVarType (effMonad info)
      )

-- | Generates the effect handling and lifting infrastructure for an effect which
-- does not have a tag type parameter. Requires the @TemplateHaskell@ language
-- extension.
--
-- Consider the following effect type class:
--
-- @
--     class 'Monad' m => MyEffect a b c m where
--       ...
-- @
--
-- @makeEffect ''MyEffect@ then generates two instances for this effect type
-- class ('Lift' for first-order effects, 'Control' for higher-order effects):
--
-- @
--     instance 'Handle' (MyEffect a b c) t m => MyEffect a b c ('Via' (MyEffect a b c) t m) where
--       ...
--
--     instance {-\# OVERLAPPABLE \#-} 'Lift'/'Control' (MyEffect a b c) t m => MyEffect a b c ('Via' eff t m) where
--       ...
-- @
--
-- Without @TemplateHaskell@, you have to write these instances by hand. These
-- two instances can also be generated separately, see 'makeHandler' and 'makeLifter'.
makeEffect :: Name -> Q [Dec]
makeEffect className = do
  clsInfo   <- classInfo className
  effInfo   <- effectInfo clsInfo
  hInstance <- handler effInfo
  lInstance <- lifter effInfo
  pure [hInstance, lInstance]

-- | Similar to 'makeTaggedEffect', but only generates the tag-related definitions.
makeTagger :: Name -> Q [Dec]
makeTagger = makeTaggerWith removeApostrophe

-- | Similar to 'makeTaggedEffectWith', but only generates the tag-related definitions.
makeTaggerWith :: (String -> Q String) -> Name -> Q [Dec]
makeTaggerWith f className = do
  clsInfo <- classInfo className
  effInfo <- effectInfo clsInfo
  tagInfo <- taggedInfo f effInfo
  tagger tagInfo

-- | Generates the effect handling and lifting infrastructure for an effect which
-- has a tag type parameter. It is expected that the tag type parameter is the first
-- type parameter of the effect type class. It is also expected that the names of the
-- effect type class and its methods end with an apostrophe \"'\". If you want more
-- control over the naming convention, use 'makeTaggedEffectWith'.
--
-- In general, this function generates everything that 'makeEffect' does, but also some
-- additional things. Consider the following effect type class:
--
-- @
--     class 'Monad' m => MyEffect' tag a b c m where
--       methodA' :: a -> m ()
--       methodB' :: b -> m ()
--       methodC' :: c -> m ()
-- @
--
-- @'makeTaggedEffect' \'\'MyEffect'@ then generates the following additional things:
--
-- * A type synonym for the untagged version of @MyEffect'@ with the name @MyEffect@
-- (note the missing apostrophe).
-- * Untagged versions of the effect methods, namely @methodA@, @methodB@ and @methodC@
-- (note the missing apostrophes).
-- * An instance of @MyEffect'@ for the type 'Tagger' which does not handle the effect,
-- but simply tags, retags or untags the @MyEffect'@ constraint of a computation.
-- * Three functions @tagMyEffect'@, @retagMyEffect'@ and @untagMyEffect'@ which make
-- use of the 'Tagger' instance.
--
-- As a rule of thumb, whenever you see an apostrophe suffix in the name of a definition
-- somewhere in this library, it has something to do with tags. Most of the time you
-- will use such definitions in combination with the language extension @TypeApplications@,
-- like:
--
-- @
--     ... forall tag ... methodA' @tag ...
--     tagMyEffect' \@\"newTag\" program
--     retagMyEffect' \@\"oldTag\" \@\"newTag\" program
--     untagMyEffect' \@\"erasedTag\" program
-- @
--
-- All the tag-related definitions can also be generated separately (i.e., without the
-- instances generated by 'makeEffect'), see 'makeTagger' and 'makeTaggerWith'.
makeTaggedEffect :: Name -> Q [Dec]
makeTaggedEffect = makeTaggedEffectWith removeApostrophe

-- | Similar to 'makeTaggedEffect', but allows to define a naming convention function
-- for the names of the effect type class and its methods. This function is used to
-- transform the name of a tagged definition (i.e., the type class or its methods) into
-- its untagged counterpart.
--
-- The default naming convention is enforced by 'removeApostrophe', which simply
-- removes the apostrophe \"'\" at the end of a name.
makeTaggedEffectWith :: (String -> Q String) -> Name -> Q [Dec]
makeTaggedEffectWith f className = do
  clsInfo    <- classInfo className
  effInfo    <- effectInfo clsInfo
  tagInfo    <- taggedInfo f effInfo
  hInstance  <- handler effInfo
  lInstance  <- lifter effInfo
  taggerDecs <- tagger tagInfo
  pure (hInstance : lInstance : taggerDecs)

-- | Similar to 'makeEffect', but only generates the effect type class instance
-- for handling an effect.
makeHandler :: Name -> Q [Dec]
makeHandler className = do
  clsInfo   <- classInfo className
  effInfo   <- effectInfo clsInfo
  hInstance <- handler effInfo
  pure [hInstance]

-- | Similar to 'makeEffect', but only generates the effect type class instance
-- for lifting an effect.
makeLifter :: Name -> Q [Dec]
makeLifter className = do
  clsInfo   <- classInfo className
  effInfo   <- effectInfo clsInfo
  lInstance <- lifter effInfo
  pure [lInstance]

tagger :: TaggedInfo -> Q [Dec]
tagger info = do
  taggerFuns   <- taggerFunctions info
  untaggedSyn  <- untaggedSynonym info
  untaggedFuns <- untaggedFunctions info
  taggerInst   <- taggerInstance info
  pure
    $ untaggedSyn
    : taggerInst
    : taggerFuns
   ++ untaggedFuns

handler :: EffectInfo -> Q Dec
handler info = do
  funs <- handlerFunctions info
  instanceD
    ( instanceCxt ''Handle info )
    ( instanceHead (effType info) info )
    ( fmap pure funs )

lifter :: EffectInfo -> Q Dec
lifter info = do
  let
    monad = effMonad info
    context =
      if any (isHigherOrder monad) (effSigs info)
      then ''Control
      else ''Lift
  funs <- lifterFunctions info
  eff  <- newName "eff"
  instanceWithOverlapD
    ( Just Overlappable )
    ( instanceCxt context info )
    ( instanceHead (varT eff) info )
    ( fmap pure funs )

taggerFunctions :: TaggedInfo -> Q [Dec]
taggerFunctions info = do
  let params       = tgParams info
      tagVar       = tgTag info
      effectName   = tgEffName info
      nameString   = nameBase effectName
      tagFName     = mkName ("tag"   ++ nameString)
      retagFName   = mkName ("retag" ++ nameString)
      untagFName   = mkName ("untag" ++ nameString)
  tag    <- newName (nameBase $ tyVarName tagVar)
  new    <- newName "new"
  tagF   <- taggerFunction effectName tagFName Nothing (Just new) params
  retagF <- taggerFunction effectName retagFName (Just tag) (Just new) params
  untagF <- taggerFunction effectName untagFName (Just tag) Nothing params
  pure $
    tagF ++ retagF ++ untagF

taggerFunction :: Name -> Name -> Maybe Name -> Maybe Name -> [TyVarBndr] -> Q [Dec]
taggerFunction baseName funName tag new params = do
  mName <- newName "m"
  aName <- newName "a"
  let m           = varT mName
      a           = varT aName
      tagParam    = maybe [t| G |] varT tag
      newParam    = maybe [t| G |] varT new
      tagNames    = maybeToList tag ++ maybeToList new
      paramNames  = fmap tyVarName params
      paramTypes  = fmap (tyVarType . unkindTyVar) params
      forallNames = tagNames ++ paramNames ++ [mName, aName]
      forallTypes = fmap PlainTV forallNames
      effectType  = foldl appT (conT baseName) (tagParam : paramTypes)
  funSigType <- [t| ($effectType `Via` Tagger $tagParam $newParam) $m $a -> $m $a |]
  funSig     <- sigD funName $ forallT forallTypes (cxt []) (pure funSigType)
  funDef     <- [d| $(varP funName) = runTagger . runVia |]
  funInline  <- pragInlD funName Inline FunLike AllPhases
  pure (funSig : funInline : funDef)

untaggedSynonym :: TaggedInfo -> Q Dec
untaggedSynonym info = do
  synName <- synonymName info
  tySynD
    ( synName )
    ( params  )
    ( foldl appT (conT effectName) (conT ''G : fmap tyVarType params) )
  where
    effectName = tgEffName info
    params     = fmap unkindTyVar (tgParams info)

untaggedFunctions :: TaggedInfo -> Q [Dec]
untaggedFunctions info = do
  synName <- synonymName info
  fmap concat $
    forM (tgSigs info)
      $ untaggedFunction (tgNameMap info)
      $ foldl
          ( appT         )
          ( conT synName )
          ( fmap (tyVarType . unkindTyVar) $ tgParams info ++ [tgMonad info] )

untaggedFunction :: (String -> Q String) -> Q Type -> Signature -> Q [Dec]
untaggedFunction f effectType sig = do
  let originalName = sigName sig
      signatureBody = pure (unkindType $ sigType sig)
  funName   <- mapName f originalName
  funSig    <- sigD funName [t| $effectType => $signatureBody |]
  funDef    <- [d| $(varP funName) = $(varE originalName) @G |]
  funInline <- pragInlD funName Inline FunLike AllPhases
  pure (funSig : funInline : funDef)

taggerInstance :: TaggedInfo -> Q Dec
taggerInstance info = do
  newTagName <- newName "new"
  let new = varT newTagName
      monadName = tyVarName (tgMonad info)
      m = varT monadName
      tag = tyVarType (tgTag info)
      effectType = conT $ tgEffName info
      paramTypes = fmap tyVarType (tgParams info)
      taggerType = [t| Tagger $tag $new $m |]
      cxtParams  = new : paramTypes ++ [m]
      headParams = tag : paramTypes ++ [taggerType]
  funs <-
    fmap concat $
      forM (tgSigs info) $ taggerInstanceFunction new monadName
  instanceD
    ( cxt [foldl appT effectType cxtParams] )
    ( foldl appT effectType headParams )
    ( fmap pure funs )

taggerInstanceFunction :: Q Type -> Name -> Signature -> Q [Dec]
taggerInstanceFunction new monad sig = do
  let typ = sigType sig
      funName = sigName sig
      expr = derive [] [| Tagger |] [| runTagger |] monad typ
      typeAppliedName = varE funName `appTypeE` new
  funDef    <- [d| $(varP funName) = $expr $typeAppliedName |]
  funInline <- pragInlD funName Inline FunLike AllPhases
  pure (funInline : funDef)

paramCount :: Type -> Int
paramCount typ =
  case typ of
    ArrowT `AppT` _ `AppT` r -> 1 + paramCount r
    ForallT _ _ t            -> paramCount t
    _                        -> 0

invalid :: Q Exp
invalid = fail
   $ "Could not generate effect instance because the operation is "
  ++ "invalid for higher-order effects."

handlerFunctions :: EffectInfo -> Q [Dec]
handlerFunctions info =
  fmap concat $
    mapM
      ( function [| Via |] [| runVia |] (effMonad info) (effParams info) )
      ( effSigs info )

lifterFunctions :: EffectInfo -> Q [Dec]
lifterFunctions info =
  let m = effMonad info
      params = effParams info
  in
  fmap concat $
    forM (effSigs info) $ \sig ->
      if isHigherOrder m sig
      then higherFunction m params sig
      else function [| lift |] invalid m params sig

function :: Q Exp -> Q Exp -> TyVarBndr -> [TyVarBndr] -> Signature -> Q [Dec]
function f inv monad params sig = do
  let m = tyVarName monad
      funName = sigName sig
      paramTypes = fmap tyVarType params
      typeAppliedName = foldl appTypeE (varE funName) paramTypes
      expr = derive [] f inv m (sigType sig)
  funDef    <- [d| $(varP funName) = $expr $typeAppliedName |]
  funInline <- pragInlD funName Inline FunLike AllPhases
  pure (funInline : funDef)

higherFunction :: TyVarBndr -> [TyVarBndr] -> Signature -> Q [Dec]
higherFunction monad params sig = do
  let m = tyVarName monad
      typ = sigType sig
      funName = sigName sig
      paramTypes = fmap tyVarType params
      restores = restorables False m typ
      expr = derive restores [| id |] [| run . runVia |] m typ
  fParams <- replicateM (paramCount typ) (newName "x")    
  res     <- resultType m typ
  let typeAppliedName = foldl appTypeE (varE funName) paramTypes
      appliedExp = foldl appE expr (typeAppliedName : fmap varE fParams)
      body =
        [| Via $
            (liftWith $ \ $([p|run|]) -> $appliedExp)
              >>= $(traverseExp res) (restoreT . pure)
        |]
  funDef    <- funD funName [clause (fmap varP fParams) (normalB body) []]
  funInline <- pragInlD funName Inline FunLike AllPhases
  pure [funDef, funInline]

unkindType :: Type -> Type
unkindType typ =
  case typ of
    -- We could need the following line if we want to preserve foralls
    --ForallT vs ps t -> ForallT (fmap unkindTyVar vs) (fmap unkindType ps) (unkindType t)
    ForallT _ _ t -> unkindType t
    AppT l r      -> AppT (unkindType l) (unkindType r)
    SigT t _      -> t
    InfixT l n r  -> InfixT (unkindType l) n (unkindType r)
    UInfixT l n r -> UInfixT (unkindType l) n (unkindType r)
    ParensT t     -> ParensT (unkindType t)
    other         -> other

contains :: Name -> Type -> Bool
contains m typ =
  case typ of
    ForallT _ _ t -> contains m t
    AppT l r      -> contains m l || contains m r
    SigT t _      -> contains m t
    VarT n        -> n == m
    ConT n        -> n == m
    PromotedT n   -> n == m
    InfixT l n r  -> n == m || contains m l || contains m r
    UInfixT l n r -> n == m || contains m l || contains m r
    ParensT t     -> contains m t
    _             -> False

derive :: [Type] -> Q Exp -> Q Exp -> Name -> Type -> Q Exp
derive rs f inv m typ =
  -- TODO: This is missing some cases - see algorithm of DeriveFunctor.
  case typ of
    t | not (contains m t) ->
      [| id |]
    VarT n `AppT` _ | n == m ->
      f
    ArrowT `AppT` arg `AppT` res ->
      let rf = derive rs f inv m res
          af = derive rs inv f m arg
      in if elem arg rs
         then [| \x b -> $rf (((x =<<) . Via . restoreT . pure) b) |]
         else [| \x b -> $rf (x ($af b)) |]
    ForallT _ _ t ->
      derive rs f inv m t
    other -> fail
       $ "Could not generate effect instance because an unknown structure "
      ++ "was encountered: "
      ++ show other

traverseExp :: Type -> Q Exp
traverseExp typ =
  case typ of
    ForallT _ _ t -> traverseExp t
    AppT _ r      -> traverseRec r
    SigT t _      -> traverseExp t
    InfixT _ _ r  -> traverseRec r
    UInfixT _ _ r -> traverseRec r
    ParensT t     -> traverseExp t
    _             -> [| id |]
  where
    traverseRec t = [| traverse . $(traverseExp t) |]