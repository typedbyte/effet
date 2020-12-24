{-# LANGUAGE CPP             #-}
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
  , makeFinder
  , makeLifter
    -- * Tag-based Generators
  , makeTaggedEffect
  , makeTaggedEffectWith
  , makeTagger
  , makeTaggerWith
  , makeUntagged
  , makeUntaggedWith
    -- * Lifting Convenience
  , liftL
  , runL
    -- * Naming Convention
  , removeApostrophe
  ) where

-- base
import Control.Monad          (forM, replicateM)
import Control.Monad.IO.Class (MonadIO)
import Data.Coerce            (coerce)
import Data.List              (isSuffixOf)
import Data.Maybe             (catMaybes, maybeToList)

-- monad-control
import Control.Monad.Trans.Control (liftWith, restoreT)

-- template-haskell
import Language.Haskell.TH.Lib
import Language.Haskell.TH.Syntax hiding (Lift, lift)

-- transformers
import Control.Monad.Trans.Class (lift)

import Control.Effect.Machinery.Tagger (Tagger(..), runTagger)
import Control.Effect.Machinery.Via    (Control, EachVia(..), Find, G, Handle,
                                        Lift, Via, runVia)

-----------------------------------------
-- Information about effect type classes.
-----------------------------------------
data EffectInfo = EffectInfo
  { effCxts    :: [Type]
  , effName    :: Name
  , effParams  :: [TyVarBndr]
  , effMonad   :: TyVarBndr
  , effMethods :: [Signature]
  }

data Signature = Signature
  { sigName :: Name
  , sigType :: Type
  }

-- Given a type class name, extracts infos about an effect.
effectInfo :: Name -> Q EffectInfo
effectInfo className = do
  info <- reify className
  case info of
    ClassI (ClassD cxts name tyVars _ decs) _ -> do
      (params, monad) <-
        case tyVars of
          [] -> fail
                  $  "The specified effect type class `"
                  ++ nameBase name
                  ++ "' has no monad type variable. "
                  ++ "It is expected to be the last type variable."
          vs -> pure (init vs, last vs)
      let sigs = [Signature n t | SigD n t <- decs]
      pure $ EffectInfo cxts name params monad sigs
    other ->
      fail
         $ "The specified name `"
        ++ nameBase className
        ++ "' is not a type class, but the following instead: "
        ++ show other

-- Constructs the type of an effect, i.e. the type class
-- without its monad parameter.
effectType :: EffectInfo -> Q Type
effectType info =
  foldl
    ( appT )
    ( conT $ effName info )
    ( fmap tyVarType (effParams info) )

-- Extracts the super classes of an effect which have the
-- kind of effects. As an example, for the following effect ...
--
-- class (State s m, Monad m) => MyEffect s m where ...
--
-- ... this would return [State s, Monad].
superEffects :: EffectInfo -> [Type]
superEffects info =
  catMaybes $ fmap extract (effCxts info)
    where
      m = tyVarName (effMonad info)
      extract = \case
        ForallT _ _ t -> extract t
        SigT t _      -> extract t
        ParensT t     -> extract t
        t `AppT` VarT n      | n == m -> Just t
        InfixT t _ (VarT n)  | n == m -> Just t
        UInfixT t _ (VarT n) | n == m -> Just t
#if __GLASGOW_HASKELL__ >= 808
        AppKindT t _ -> extract t
        ImplicitParamT _ t -> extract t
#endif
        _ -> Nothing

-- Like superEffects, but ignores super classes from base
-- (i.e., Applicative, Functor, Monad, MonadIO).
superEffectsWithoutBase :: EffectInfo -> [Type]
superEffectsWithoutBase =
  filter (not . isBase) . superEffects 
    where
      isBase = \case
        ConT n -> n `elem` [''Applicative, ''Functor, ''Monad, ''MonadIO]
        _ -> False

-------------------------------------------------
-- Tagging information about effect type classes.
-------------------------------------------------
data TaggedInfo = TaggedInfo
  { tgTag    :: TyVarBndr
  , tgParams :: [TyVarBndr]
  }

-- Given an effect, extracts infos about the tag parameter.
taggedInfo :: EffectInfo -> Q TaggedInfo
taggedInfo info =
  case effParams info of
    []     -> fail "The effect has no tag parameter."
    (v:vs) -> pure $ TaggedInfo v vs

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
-- @makeEffect ''MyEffect@ then generates three instances for this effect type
-- class ('Lift' for first-order effects, 'Control' for higher-order effects):
--
-- @
--     instance 'Handle' (MyEffect a b c) t m => MyEffect a b c ('EachVia' (MyEffect a b c : effs) t m) where
--       ...
--
--     instance {-\# OVERLAPPABLE \#-} 'Find' (MyEffect a b c) effs t m => MyEffect a b c ('EachVia' (other : effs) t m) where
--       ...
--
--     instance 'Lift'/'Control' (MyEffect a b c) t m => MyEffect a b c ('EachVia' \'[] t m) where
--       ...
-- @
--
-- The first instance indicates that @MyEffect@ was found at the head of the type
-- level list of effects to be handled, so @MyEffect@ is delegated to @t@.
--
-- The second instance indicates that @MyEffect@ was not found at the head of the
-- type level list of effects to be handled, so we must find @MyEffect@ in the tail @effs@
-- of the type level list.
--
-- The third instance indicates that @MyEffect@ could not be found in the type level
-- list of effects to be handled, so the effect must be delegated further down the monad
-- transformer stack in order to find its corresponding effect handler.
--
-- Without @TemplateHaskell@, you have to write these three instances by hand. These
-- instances can also be generated separately, see 'makeHandler', 'makeFinder' and
-- 'makeLifter'.
makeEffect :: Name -> Q [Dec]
makeEffect className = do
  effInfo   <- effectInfo className
  hInstance <- handler effInfo
  fInstance <- finder effInfo
  lInstance <- lifter effInfo
  tInstance <- identityTaggerInstance effInfo
  pure [hInstance, fInstance, lInstance, tInstance]

-- | Similar to 'makeTaggedEffect', but only generates the tag-related definitions.
makeTagger :: Name -> Q [Dec]
makeTagger = makeTaggerWith removeApostrophe

-- | Similar to 'makeTaggedEffectWith', but only generates the tag-related definitions.
makeTaggerWith :: (String -> Q String) -> Name -> Q [Dec]
makeTaggerWith mapping className = do
  let f = fmap mkName . mapping . nameBase
  effInfo <- effectInfo className
  tagInfo <- taggedInfo effInfo
  tagger f effInfo tagInfo

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
makeTaggedEffectWith mapping className = do
  let f = fmap mkName . mapping . nameBase
  effInfo    <- effectInfo className
  tagInfo    <- taggedInfo effInfo
  hInstance  <- handler effInfo
  fInstance  <- finder effInfo
  lInstance  <- lifter effInfo
  taggerDecs <- tagger f effInfo tagInfo
  pure (hInstance : fInstance : lInstance : taggerDecs)

-- | Given a list of function names, this function generates untagged versions
-- of them, i.e. it removes the tag type parameters  from their type signatures
-- (by applying 'G') and converts tagged effect type classes found in the
-- signature to their corresponding untagged type synonyms using 'removeApostrophe'.
--
-- @since 0.4.0.0
makeUntagged :: [Name] -> Q [Dec]
makeUntagged = makeUntaggedWith removeApostrophe

-- | Similar to 'makeUntagged', but allows to define a naming convention function
-- for the names of the generated functions and the effect type classes modified
-- in the type signatures.
--
-- The default naming convention is enforced by 'removeApostrophe', which simply
-- removes the apostrophe \"'\" at the end of a name.
--
-- @since 0.4.0.0
makeUntaggedWith :: (String -> Q String) -> [Name] -> Q [Dec]
makeUntaggedWith mapping names =
  let f = fmap mkName . mapping . nameBase in
  fmap concat $ forM names $ \name -> do
    info <- reify name
    case info of
      VarI funName typ _ -> do
        tag <- findTagParameter typ
        genName <- f funName
        funSig <- sigD genName $ replaceTag f tag typ
        funDef <- [d| $(varP genName) = $(varE funName) @G |]
        funInline <- pragInlD genName Inline FunLike AllPhases
        pure (funSig : funInline : funDef)
      other ->
        fail
           $ "Expected a function for name " ++ nameBase name
          ++ ", but encountered: " ++ show other

-- | Similar to 'makeEffect', but only generates the effect type class instance
-- for handling an effect.
makeHandler :: Name -> Q [Dec]
makeHandler className = do
  effInfo   <- effectInfo className
  hInstance <- handler effInfo
  pure [hInstance]

-- | Similar to 'makeEffect', but only generates the effect type class instance
-- for finding the effect in the tail of the type level list.
--
-- @since 0.2.0.0
makeFinder :: Name -> Q [Dec]
makeFinder className = do
  effInfo   <- effectInfo className
  fInstance <- finder effInfo
  pure [fInstance]

-- | Similar to 'makeEffect', but only generates the effect type class instance
-- for lifting an effect.
makeLifter :: Name -> Q [Dec]
makeLifter className = do
  effInfo   <- effectInfo className
  lInstance <- lifter effInfo
  pure [lInstance]

tagger :: (Name -> Q Name) -> EffectInfo -> TaggedInfo -> Q [Dec]
tagger f effInfo tagInfo = do
  taggerFuns   <- taggerFunctions effInfo tagInfo
  untaggedSyn  <- untaggedSynonym f effInfo tagInfo
  untaggedFuns <- untaggedFunctions f effInfo tagInfo
  taggerInst   <- taggerInstance effInfo tagInfo
  pure
    $ untaggedSyn
    : taggerInst
    : taggerFuns
   ++ untaggedFuns

handler :: EffectInfo -> Q Dec
handler info = do
  funs   <- handlerFunctions info
  others <- newName "others"
  trafo  <- newName "t"
  instanceD
    ( instanceHandleCxt others trafo )
    ( instanceHead (promotedConsT `appT` effectType info `appT` varT others) trafo info )
    ( fmap pure funs )
  where
    instanceHandleCxt :: Name -> Name -> Q Cxt
    instanceHandleCxt others trafo = cxt
      [
        conT ''Handle
          `appT` typeLevelList (fmap pure $ superEffects info)
          `appT` effectType info
          `appT` varT others
          `appT` varT trafo
          `appT` tyVarType (effMonad info)
      ]

finder :: EffectInfo -> Q Dec
finder info = do
  funs  <- finderFunctions info
  other <- newName "other"
  effs  <- newName "effs"
  trafo <- newName "t"
  instanceWithOverlapD
    ( Just Overlappable )
    ( instanceFinderCxt other effs trafo )
    ( instanceHead (promotedConsT `appT` varT other `appT` varT effs) trafo info )
    ( fmap pure funs )
  where
    instanceFinderCxt :: Name -> Name -> Name -> Q Cxt
    instanceFinderCxt other effs trafo = cxt
      [
        conT ''Find
          `appT` typeLevelList (fmap pure $ superEffects info)
          `appT` effectType info
          `appT` varT other
          `appT` varT effs
          `appT` varT trafo
          `appT` tyVarType (effMonad info)
      ]

lifter :: EffectInfo -> Q Dec
lifter info = do
  let
    monad = effMonad info
    liftType =
      if any (isHigherOrder monad) (effMethods info)
      then ''Control
      else ''Lift
  funs  <- lifterFunctions info
  trafo <- newName "t"
  instanceD
    ( instanceLiftControlCxt liftType trafo )
    ( instanceHead promotedNilT trafo info )
    ( fmap pure funs )
  where
    instanceLiftControlCxt :: Name -> Name -> Q Cxt
    instanceLiftControlCxt name trafo = cxt
      [
        conT name
          `appT` typeLevelList (fmap pure $ superEffects info)
          `appT` effectType info
          `appT` varT trafo
          `appT` tyVarType (effMonad info)
      ]

instanceHead :: Q Type -> Name -> EffectInfo -> Q Type
instanceHead effs trafo info =
  effectType info
    `appT` (
      conT ''EachVia
        `appT` effs
        `appT` varT trafo
        `appT` tyVarType (effMonad info)
      )

taggerFunctions :: EffectInfo -> TaggedInfo -> Q [Dec]
taggerFunctions effInfo tagInfo = do
  let tagVar       = tgTag tagInfo
      nameString   = nameBase (effName effInfo)
      tagFName     = mkName ("tag"   ++ nameString)
      retagFName   = mkName ("retag" ++ nameString)
      untagFName   = mkName ("untag" ++ nameString)
  new    <- newName "new"
  tagF   <- taggerFunction tagFName effInfo tagInfo Nothing (Just new)
  retagF <- taggerFunction retagFName effInfo tagInfo (Just tagVar) (Just new)
  untagF <- taggerFunction untagFName effInfo tagInfo (Just tagVar) Nothing
  pure $
    tagF ++ retagF ++ untagF
  
taggerFunction :: Name -> EffectInfo -> TaggedInfo -> Maybe TyVarBndr -> Maybe Name -> Q [Dec]
taggerFunction funName effInfo tagInfo tag new = do
  mName <- newName "m"
  aName <- newName "a"
  gType <- [t| G |]
  let m           = varT mName
      a           = varT aName
      params      = tgParams tagInfo
      tagParam    = maybe (pure gType) (varT . tyVarName) tag
      newParam    = maybe (pure gType) varT new
      tagVars     = maybeToList tag ++ maybeToList (fmap PlainTV new)
      forallVars  = fmap unkindTyVar (tagVars ++ params) ++ [PlainTV mName, PlainTV aName]
      paramTypes  = fmap (tyVarType . unkindTyVar) params
      effType     = foldl appT (conT $ effName effInfo) (tagParam : paramTypes)
      effList     = effType : fmap pure (superEffectsWithoutBase effInfo)
      untagList =
        case tag of
            Nothing -> fmap (fmap (replace (tyVarName $ tgTag tagInfo) gType)) effList
            Just _  -> effList
      taggerType = [t| Tagger $tagParam $newParam |]
      viaType =
        case untagList of
#if __GLASGOW_HASKELL__ >= 808
          [e] -> uInfixT e ''Via taggerType
          es  -> uInfixT (typeLevelList es) ''EachVia taggerType
#else
          [e] -> conT ''Via `appT` e `appT` taggerType
          es  -> conT ''EachVia `appT` typeLevelList es `appT` taggerType
#endif
      funSigType = [t| $viaType $m $a -> $m $a |]
  funSig    <- sigD funName $ forallT forallVars (cxt []) funSigType
  funDef    <- [d| $(varP funName) = runTagger . runVia |]
  funInline <- pragInlD funName Inline FunLike AllPhases
  pure (funSig : funInline : funDef)
    where
      replace :: Name -> Type -> Type -> Type
      replace oldTag newTag = \case
        ConT n `AppT` VarT param | param == oldTag -> ConT n `AppT` newTag
        ForallT vars ctx t -> ForallT vars ctx (replace oldTag newTag t)
        AppT l r           -> AppT (replace oldTag newTag l) r
        SigT t k           -> SigT (replace oldTag newTag t) k
        InfixT l n r       -> InfixT (replace oldTag newTag l) n (replace oldTag newTag r)
        UInfixT l n r      -> UInfixT (replace oldTag newTag l) n (replace oldTag newTag r)
        ParensT t          -> ParensT (replace oldTag newTag t)
#if __GLASGOW_HASKELL__ >= 808
        AppKindT t k -> AppKindT (replace oldTag newTag t) k
        ImplicitParamT s t -> ImplicitParamT s (replace oldTag newTag t)
#endif
        other              -> other

untaggedSynonym :: (Name -> Q Name) -> EffectInfo -> TaggedInfo -> Q Dec
untaggedSynonym f effInfo tagInfo = do
  synName <- f (effName effInfo)
  tySynD
    ( synName )
    ( params  )
    ( foldl appT (conT $ effName effInfo) (conT ''G : fmap tyVarType params) )
  where
    params = fmap unkindTyVar (tgParams tagInfo)

untaggedFunctions :: (Name -> Q Name) -> EffectInfo -> TaggedInfo -> Q [Dec]
untaggedFunctions f effInfo tagInfo = do
  synName <- f (effName effInfo)
  fmap concat $
    forM (effMethods effInfo)
      $ untaggedFunction f
      $ foldl
          ( appT         )
          ( conT synName )
          ( fmap (tyVarType . unkindTyVar) $ tgParams tagInfo ++ [effMonad effInfo] )

untaggedFunction :: (Name -> Q Name) -> Q Type -> Signature -> Q [Dec]
untaggedFunction f effType sig = do
  let originalName = sigName sig
      signatureBody = pure (unkindType $ sigType sig)
  funName   <- f originalName
  funSig    <- sigD funName [t| $effType => $signatureBody |]
  funDef    <- [d| $(varP funName) = $(varE originalName) @G |]
  funInline <- pragInlD funName Inline FunLike AllPhases
  pure (funSig : funInline : funDef)

taggerInstance :: EffectInfo -> TaggedInfo -> Q Dec
taggerInstance effInfo tagInfo = do
  newTagName <- newName "new"
  let new = varT newTagName
      monadName = tyVarName (effMonad effInfo)
      m = varT monadName
      tag = tyVarType (tgTag tagInfo)
      effType = conT (effName effInfo)
      paramTypes = fmap tyVarType (tgParams tagInfo)
      taggerType = [t| Tagger $tag $new $m |]
      cxtParams  = new : paramTypes ++ [m]
      headParams = tag : paramTypes ++ [taggerType]
  funs <-
    fmap concat $
      forM (effMethods effInfo) $ taggerInstanceFunction new monadName
  instanceD
    ( cxt [foldl appT effType cxtParams] )
    ( foldl appT effType headParams )
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

identityTaggerInstance :: EffectInfo -> Q Dec
identityTaggerInstance info = do
  oldTagName <- newName "tag"
  newTagName <- newName "new"
  let old = varT oldTagName
      new = varT newTagName
      monadName = tyVarName (effMonad info)
      m = varT monadName
      effType = conT $ effName info
      paramTypes = fmap tyVarType (effParams info)
      taggerType = [t| Tagger $old $new $m |]
      cxtParams  = paramTypes ++ [m]
      headParams = paramTypes ++ [taggerType]
  funs <-
    fmap concat $
      forM (effMethods info) $
        function [| Tagger |] [| runTagger |] (effMonad info) (effParams info)
  instanceD
    ( cxt [foldl appT effType cxtParams] )
    ( foldl appT effType headParams )
    ( fmap pure funs )

handlerFunctions :: EffectInfo -> Q [Dec]
handlerFunctions info =
  fmap concat $
    mapM
      ( function [| EachVia |] [| runVia |] (effMonad info) (effParams info) )
      ( effMethods info )

finderFunctions :: EffectInfo -> Q [Dec]
finderFunctions info =
  fmap concat $
    mapM
      ( function [| liftL |] [| runL |] (effMonad info) (effParams info) )
      ( effMethods info )

lifterFunctions :: EffectInfo -> Q [Dec]
lifterFunctions info =
  let m = effMonad info
      params = effParams info
      invalid = fail
         $ "Could not generate effect instance because the operation is "
        ++ "invalid for higher-order effects."
  in
  fmap concat $
    forM (effMethods info) $ \sig ->
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
  resType <- resultType m typ
  let typeAppliedName = foldl appTypeE (varE funName) paramTypes
      appliedExp = foldl appE expr (typeAppliedName : fmap varE fParams)
      body =
        [| EachVia $
            (liftWith $ \ $([p|run|]) -> $appliedExp)
              >>= $(traverseExp resType) (restoreT . pure)
        |]
  funDef    <- funD funName [clause (fmap varP fParams) (normalB body) []]
  funInline <- pragInlD funName Inline FunLike AllPhases
  pure [funDef, funInline]
  where
    restorables :: Bool -> Name -> Type -> [Type]
    restorables neg m = \case
      VarT n `AppT` a
        | n == m && neg        -> [a]
      ArrowT `AppT` a `AppT` r -> restorables (not neg) m a ++ restorables neg m r
      ForallT _ _ t            -> restorables neg m t
      SigT t _                 -> restorables neg m t
      ParensT t                -> restorables neg m t
#if __GLASGOW_HASKELL__ >= 808
      AppKindT t _ -> restorables neg m t
      ImplicitParamT _ t -> restorables neg m t
#endif
      other -> fail
        $  "Encountered an unknown term when finding restorables: "
        ++ show other
    traverseExp :: Type -> Q Exp
    traverseExp = \case
      ForallT _ _ t -> traverseExp t
      AppT _ r      -> traverseRec r
      SigT t _      -> traverseExp t
      InfixT _ _ r  -> traverseRec r
      UInfixT _ _ r -> traverseRec r
      ParensT t     -> traverseExp t
#if __GLASGOW_HASKELL__ >= 808
      AppKindT t _ -> traverseExp t
      ImplicitParamT _ t -> traverseExp t
#endif
      _             -> [| id |]
      where
        traverseRec t = [| traverse . $(traverseExp t) |]

derive :: [Type] -> Q Exp -> Q Exp -> Name -> Type -> Q Exp
derive rs f inv m = \case
  -- TODO: This is missing some cases - see algorithm of DeriveFunctor.
  t | not (contains m t) ->
    [| id |]
  VarT n `AppT` _ | n == m ->
    f
  ArrowT `AppT` arg `AppT` res ->
    let rf = derive rs f inv m res
        af = derive rs inv f m arg
    in if elem arg rs
       then [| \x b -> $rf (((x =<<) . EachVia . restoreT . pure) b) |]
       else [| \x b -> $rf (x ($af b)) |]
  ForallT _ _ t ->
    derive rs f inv m t
#if __GLASGOW_HASKELL__ >= 808
  AppKindT t _ ->
    derive rs f inv m t
  ImplicitParamT _ t ->
    derive rs f inv m t
#endif
  other -> fail
     $ "Could not generate effect instance because an unknown structure "
    ++ "was encountered: "
    ++ show other

---------------------
-- Utility functions.
---------------------

-- Throws away all kind information and forall from a type.
unkindType :: Type -> Type
unkindType = \case
  -- We could need the following line if we want to preserve foralls
  --ForallT vs ps t -> ForallT (fmap unkindTyVar vs) (fmap unkindType ps) (unkindType t)
  ForallT _ _ t -> unkindType t
  AppT l r      -> AppT (unkindType l) (unkindType r)
  SigT t _      -> t
  InfixT l n r  -> InfixT (unkindType l) n (unkindType r)
  UInfixT l n r -> UInfixT (unkindType l) n (unkindType r)
  ParensT t     -> ParensT (unkindType t)
#if __GLASGOW_HASKELL__ >= 808
  AppKindT t _ -> unkindType t
  ImplicitParamT s t -> ImplicitParamT s (unkindType t)
#endif
  other         -> other

-- Throws away the kind information of a type variable.
unkindTyVar :: TyVarBndr -> TyVarBndr
unkindTyVar (KindedTV n _) = PlainTV n
unkindTyVar unkinded       = unkinded

-- Returns the name of a type variable.
tyVarName :: TyVarBndr -> Name
tyVarName (PlainTV  n  ) = n
tyVarName (KindedTV n _) = n

-- Converts a type variable to a type.
tyVarType :: TyVarBndr -> Q Type
tyVarType (PlainTV n   ) = varT n
tyVarType (KindedTV n k) = sigT (varT n) k

-- Counts the parameters of a type.
paramCount :: Type -> Int
paramCount = \case
  ArrowT `AppT` _ `AppT` r -> 1 + paramCount r
  ForallT _ _ t            -> paramCount t
  _                        -> 0

-- | Adds an effect @eff@ to the type level list of effects that need to be
-- handled by the transformer @t@. From a structural point of view, this is
-- analogous to @lift@ in the @mtl@ ecosystem. This function comes in handy
-- when writing the 'Find'-based instance of an effect by hand.
--
-- @since 0.2.0.0
liftL :: EachVia effs t m a -> EachVia (eff : effs) t m a
liftL = coerce
{-# INLINE liftL #-}

-- | Removes an effect @eff@ from the type level list of effects that need to be
-- handled by the transformer @t@. From a structural point of view, this is
-- analogous to the @run...@ functions in the @mtl@ ecosystem. This function
-- comes in handy when writing the 'Find'-based instance of an effect by hand.
--
-- @since 0.2.0.0
runL :: EachVia (eff : effs) t m a -> EachVia effs t m a
runL = coerce
{-# INLINE runL #-}

-- | Extracts the untagged name from a name which is expected to end with \"\'\".
-- In other words, this function removes the suffix \"\'\" from a given name,
-- or fails otherwise.
removeApostrophe :: String -> Q String
removeApostrophe name =
  if "'" `isSuffixOf` name then
    pure $ init name
  else
    fail $ "Tagged effect and function names are expected to end with \"'\"."

-- Converts a list of types to a type-level list.
typeLevelList :: [Q Type] -> Q Type
typeLevelList []     = promotedNilT
typeLevelList (t:ts) = promotedConsT `appT` t `appT` typeLevelList ts

-- Returns the result type of a monadic type m.
-- Example: X -> Y -> Z -> m a
-- Returns: a
resultType :: Name -> Type -> Q Type
resultType m = \case
  VarT n `AppT` a | n == m -> pure a
  ArrowT `AppT` _ `AppT` r -> resultType m r
  ForallT _ _ t            -> resultType m t
  SigT t _                 -> resultType m t
  ParensT t                -> resultType m t
#if __GLASGOW_HASKELL__ >= 808
  AppKindT t _ -> resultType m t
  ImplicitParamT _ t -> resultType m t
#endif
  other -> fail
    $  "Expected a return type of the form 'm a', but encountered: "
    ++ show other

-- Checks if a name m appears somewhere in a type.
contains :: Name -> Type -> Bool
contains m = \case
  ForallT _ _ t -> contains m t
  AppT l r      -> contains m l || contains m r
  SigT t _      -> contains m t
  VarT n        -> n == m
  ConT n        -> n == m
  PromotedT n   -> n == m
  InfixT l n r  -> n == m || contains m l || contains m r
  UInfixT l n r -> n == m || contains m l || contains m r
  ParensT t     -> contains m t
#if __GLASGOW_HASKELL__ >= 808
  AppKindT t _ -> contains m t
  ImplicitParamT _ t -> contains m t
#endif
  _             -> False

-- Given a monad type variable m and a type, checks if the
-- type is a higher-order type where m is in negative position.
isHigherType :: TyVarBndr -> Type -> Bool
isHigherType monad = go False
  where
    m = tyVarName monad
    go negPos = \case
      VarT n `AppT` _ | n == m -> negPos
      ArrowT `AppT` a `AppT` r ->
        go (not negPos) a || go negPos r
      ForallT _ _ t ->
        go negPos t
      _ ->
        False

-- Given a monad type variable m and a signature, checks if its
-- type is a higher-order type where m is in negative position.
isHigherOrder :: TyVarBndr -> Signature -> Bool
isHigherOrder monad = isHigherType monad . sigType

-- Finds the first ("leftmost") type parameter of a type, which
-- is expected to be the tag type parameter.
findTagParameter :: Type -> Q Name
findTagParameter typ =
  case go typ of
    Just n -> pure n
    Nothing ->
      fail $ "Cannot find the tag parameter of the type: " ++ show typ
  where
    go :: Type -> Maybe Name
    go = \case
      ForallT tyVars ctx t ->
        case filter (not . isStar) tyVars of
          (v:_) -> Just $ tyVarName v
          [] ->
            case catMaybes (fmap go ctx) of
              (n:_) -> Just n
              [] -> go t
      AppT l r ->
        case go l of
          Just n  -> Just n
          Nothing -> go r
      SigT t _ -> go t
      VarT n -> Just n
      InfixT l _ r ->
        case go l of
          Just n  -> Just n
          Nothing -> go r
      UInfixT l _ r ->
        case go l of
          Just n  -> Just n
          Nothing -> go r
      ParensT t -> go t
#if __GLASGOW_HASKELL__ >= 808
      AppKindT t _ -> go t
      ImplicitParamT _ t -> go t
#endif
      _ -> Nothing
    -- We need this because the first type parameter
    -- is often 'k' for the kind of the tag. We ignore it.
    isStar :: TyVarBndr -> Bool
    isStar (PlainTV _) = True
    isStar (KindedTV _ StarT) = True
    isStar _ = False

-- Replaces the tag parameter with its G-counterpart, simplifying
-- types to their untagged synonym if possible.
replaceTag :: (Name -> Q Name) -> Name -> Type -> Q Type
replaceTag f tag = \case
  -- We eliminate outermost forall variables completely for now,
  -- to make the type signatures more readable.
  -- If we want to preserve it, we might need something
  -- like the line below.
  -- filter (not . (== tag) . tyVarName) tyVars
  ForallT _tyVars cxts t -> go (ForallT [] cxts t)
  other -> go other
  where
    go = \case
      ForallT tyVars cxts t ->
        forallT
          ( fmap unkindTyVar tyVars )
          ( sequence $ fmap go cxts )
          ( go t )
#if __GLASGOW_HASKELL__ >= 808
      ConT n `AppT` eff `AppT` t | n == ''Via || n == ''EachVia ->
        go (UInfixT eff n t)
#endif
      ConT n `AppT` VarT t | t == tag ->
        f n >>= conT
      AppT l r ->
        appT (go l) (go r)
      SigT t _ ->
        go t -- eliminate kinds for readability.
      VarT n | n == tag -> conT ''G
             | otherwise -> varT n
      InfixT l n r ->
        infixT (go l) n (go r)
      UInfixT l n r ->
        uInfixT (go l) n (go r)
      ParensT t ->
        parensT (go t)
#if __GLASGOW_HASKELL__ >= 808
      AppKindT t k ->
        appKindT (go t) (pure k)
      ImplicitParamT s t ->
        implicitParamT s (go t)
#endif
      other ->
        pure other