{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE ViewPatterns #-}
-----------------------------------------------------------------------------
-- |
-- Module      :  Language.Haskell.GHC.ExactPrint.Transform
--
-- This module is currently under heavy development, and no promises are made
-- about API stability. Use with care.
--
-- We welcome any feedback / contributions on this, as it is the main point of
-- the library.
--
-----------------------------------------------------------------------------
module Transform
        (
        -- * The Transform Monad
          Transform
        , TransformT(..)
        , hoistTransform
        , runTransform
        , runTransformT
        , runTransformFrom
        , runTransformFromT

        -- * Transform monad operations
        , logTr
        , logDataWithAnnsTr
        , getAnnsT, putAnnsT, modifyAnnsT
        , uniqueSrcSpanT

        , cloneT
        , graftT

        , getEntryDPT
        , setEntryDPT
        , transferEntryDPT
        , setPrecedingLinesDeclT
        , setPrecedingLinesT
        , addSimpleAnnT
        , addTrailingCommaT
        , removeTrailingCommaT

        -- ** Managing declarations, in Transform monad
        , HasTransform (..)
        , HasDecls (..)
        , hasDeclsSybTransform
        , hsDeclsGeneric
        , hsDeclsPatBind, hsDeclsPatBindD
        , replaceDeclsPatBind, replaceDeclsPatBindD
        , modifyDeclsT
        , modifyValD
        -- *** Utility, does not manage layout
        , hsDeclsValBinds, replaceDeclsValbinds
        , WithWhere(..)

        -- ** New gen functions
        , noAnnSrcSpanDP
        , noAnnSrcSpanDP0
        , noAnnSrcSpanDP1
        , noAnnSrcSpanDPn
        , d0, d1, dn
        , m0, m1, mn
        , addComma

        -- ** Managing lists, Transform monad
        , insertAt
        , insertAtStart
        , insertAtEnd
        , insertAfter
        , insertBefore

        -- *** Low level operations used in 'HasDecls'
        , balanceComments
        , balanceCommentsList
        , balanceCommentsList'
        , balanceTrailingComments
        , moveTrailingComments
        , anchorEof

        -- ** Managing lists, pure functions
        , captureOrder
        , captureOrderAnnKey
        , captureLineSpacing
        , captureMatchLineSpacing
        , captureTypeSigSpacing

        -- * Operations
        , isUniqueSrcSpan

        -- * Pure functions
        , mergeAnns
        , mergeAnnList
        , setPrecedingLinesDecl
        , setPrecedingLines
        , getEntryDP
        , setEntryDP
        , setEntryDP'
        , transferEntryDP
        , transferEntryDP'
        , addTrailingComma
        , wrapSig, wrapDecl
        , decl2Sig, decl2Bind
        , deltaAnchor
        ) where

import Types
import Utils

import Control.Monad.RWS
import qualified Control.Monad.Fail as Fail

import GHC  hiding (parseModule)
import GHC.Data.Bag
import GHC.Data.FastString

-- import qualified Data.Generics as SYB

import Data.Data
import Data.List
import Data.Maybe

import qualified Data.Map as Map

import Data.Functor.Identity
import Control.Monad.State
import Control.Monad.Writer

-- import Debug.Trace

------------------------------------------------------------------------------
-- Transformation of source elements

-- | Monad type for updating the AST and managing the annotations at the same
-- time. The W state is used to generate logging information if required.
type Transform = TransformT Identity

-- |Monad transformer version of 'Transform' monad
newtype TransformT m a = TransformT { unTransformT :: RWST () [String] (Anns,Int) m a }
                deriving (Monad,Applicative,Functor
                         ,MonadReader ()
                         ,MonadWriter [String]
                         ,MonadState (Anns,Int)
                         ,MonadTrans
                         )

instance Fail.MonadFail m => Fail.MonadFail (TransformT m) where
    fail msg = TransformT $ RWST $ \_ _ -> Fail.fail msg

-- | Run a transformation in the 'Transform' monad, returning the updated
-- annotations and any logging generated via 'logTr'
runTransform :: Anns -> Transform a -> (a,(Anns,Int),[String])
runTransform ans f = runTransformFrom 0 ans f

runTransformT :: Anns -> TransformT m a -> m (a,(Anns,Int),[String])
runTransformT ans f = runTransformFromT 0 ans f

-- | Run a transformation in the 'Transform' monad, returning the updated
-- annotations and any logging generated via 'logTr', allocating any new
-- SrcSpans from the provided initial value.
runTransformFrom :: Int -> Anns -> Transform a -> (a,(Anns,Int),[String])
runTransformFrom seed ans f = runRWS (unTransformT f) () (ans,seed)

-- |Run a monad transformer stack for the 'TransformT' monad transformer
runTransformFromT :: Int -> Anns -> TransformT m a -> m (a,(Anns,Int),[String])
runTransformFromT seed ans f = runRWST (unTransformT f) () (ans,seed)

-- | Change inner monad of 'TransformT'.
hoistTransform :: (forall x. m x -> n x) -> TransformT m a -> TransformT n a
hoistTransform nt (TransformT m) = TransformT (mapRWST nt m)

-- |Log a string to the output of the Monad
logTr :: (Monad m) => String -> TransformT m ()
logTr str = tell [str]

-- |Log a representation of the given AST with annotations to the output of the
-- Monad
logDataWithAnnsTr :: (Monad m) => (Data a) => String -> a -> TransformT m ()
logDataWithAnnsTr str ast = do
  logTr $ str ++ showAst ast

-- |Access the 'Anns' being modified in this transformation
getAnnsT :: (Monad m) => TransformT m Anns
getAnnsT = gets fst

-- |Replace the 'Anns' after any changes
putAnnsT :: (Monad m) => Anns -> TransformT m ()
putAnnsT ans = do
  (_,col) <- get
  put (ans,col)

-- |Change the stored 'Anns'
modifyAnnsT :: (Monad m) => (Anns -> Anns) -> TransformT m ()
modifyAnnsT f = do
  ans <- getAnnsT
  putAnnsT (f ans)

-- ---------------------------------------------------------------------

-- |Once we have 'Anns', a 'SrcSpan' is used purely as part of an 'AnnKey'
-- to index into the 'Anns'. If we need to add new elements to the AST, they
-- need their own 'SrcSpan' for this.
uniqueSrcSpanT :: (Monad m) => TransformT m SrcSpan
uniqueSrcSpanT = do
  (an,col) <- get
  put (an,col + 1 )
  let pos = mkSrcLoc (mkFastString "ghc-exactprint") (-1) col
  return $ mkSrcSpan pos pos

-- |Test whether a given 'SrcSpan' was generated by 'uniqueSrcSpanT'
isUniqueSrcSpan :: SrcSpan -> Bool
isUniqueSrcSpan ss = srcSpanStartLine' ss == -1

srcSpanStartLine' :: SrcSpan -> Int
srcSpanStartLine' (RealSrcSpan s _) = srcSpanStartLine s
srcSpanStartLine' _ = 0

-- ---------------------------------------------------------------------
-- |Make a copy of an AST element, replacing the existing SrcSpans with new
-- ones, and duplicating the matching annotations.
cloneT :: (Data a,Monad m) => a -> TransformT m (a, [(SrcSpan, SrcSpan)])
cloneT ast = do
  runWriterT $ everywhereM (return `ext2M` replaceLocated) ast
  where
    replaceLocated :: forall loc a m. (Typeable loc,Data a,Monad m)
                    => (GenLocated loc a) -> WriterT [(SrcSpan, SrcSpan)] (TransformT m) (GenLocated loc a)
    replaceLocated (L l t) = do
      case cast l :: Maybe SrcSpan of
        Just ss -> do
          newSpan <- lift uniqueSrcSpanT
          lift $ modifyAnnsT (\anns -> case Map.lookup (mkAnnKey (L ss t)) anns of
                                  Nothing -> anns
                                  Just an -> Map.insert (mkAnnKey (L newSpan t)) an anns)
          tell [(ss, newSpan)]
          return $ fromJust . cast  $ L newSpan t
        Nothing -> return (L l t)

-- ---------------------------------------------------------------------
-- |Slightly more general form of cloneT
graftT :: (Data a,Monad m) => Anns -> a -> TransformT m a
graftT origAnns = everywhereM (return `ext2M` replaceLocated)
  where
    replaceLocated :: forall loc a m. (Typeable loc, Data a, Monad m)
                    => GenLocated loc a -> TransformT m (GenLocated loc a)
    replaceLocated (L l t) = do
      case cast l :: Maybe SrcSpan of
        Just ss -> do
          newSpan <- uniqueSrcSpanT
          modifyAnnsT (\anns -> case Map.lookup (mkAnnKey (L ss t)) origAnns of
                                  Nothing -> anns
                                  Just an -> Map.insert (mkAnnKey (L newSpan t)) an anns)
          return $ fromJust $ cast $ L newSpan t
        Nothing -> return (L l t)

-- ---------------------------------------------------------------------

-- |If a list has been re-ordered or had items added, capture the new order in
-- the appropriate 'AnnSortKey' attached to the 'Annotation' for the list.
captureOrder :: [LocatedA b] -> AnnSortKey
captureOrder ls = AnnSortKey $ map (rs . getLocA) ls

-- |If a list has been re-ordered or had items added, capture the new order in
-- the appropriate 'annSortKey' item of the supplied 'AnnKey'
captureOrderAnnKey :: AnnKey -> [LocatedA b] -> Anns -> Anns
captureOrderAnnKey parentKey ls ans = ans
-- captureOrderAnnKey parentKey ls ans = ans'
--   where
--     newList = map (rs . getLoc) ls
--     reList = Map.adjust (\an -> an {annSortKey = Just newList }) parentKey
--     ans' = reList ans

-- ---------------------------------------------------------------------

captureMatchLineSpacing :: LHsDecl GhcPs -> LHsDecl GhcPs
captureMatchLineSpacing (L l (ValD x (FunBind a b (MG c (L d ms ) e) f)))
                       = L l (ValD x (FunBind a b (MG c (L d ms') e) f))
    where
      ms' :: [LMatch GhcPs (LHsExpr GhcPs)]
      ms' = captureLineSpacing ms
captureMatchLineSpacing d = d

-- captureLineSpacing :: [LHsDecl GhcPs] -> [LHsDecl GhcPs]
captureLineSpacing [] = []
captureLineSpacing [d] = [d]
captureLineSpacing (d1:d2:ds) = d1:captureLineSpacing (d2':ds)
  where
    (l1,_) = ss2pos $ rs $ getLocA d1
    (l2,_) = ss2pos $ rs $ getLocA d2
    d2' = setEntryDP' d2 (DP (l2-l1,0))

-- ---------------------------------------------------------------------

captureTypeSigSpacing :: LHsDecl GhcPs -> LHsDecl GhcPs
captureTypeSigSpacing (L l (SigD x (TypeSig (ApiAnn anc (AnnSig dc rs) cs) ns (HsWC xw ty))))
  = (L l (SigD x (TypeSig (ApiAnn anc (AnnSig dc' rs) cs) ns (HsWC xw ty'))))
  where
    -- we want DPs for the distance from the end of the ns to the
    -- AnnDColon, and to the start of the ty
    AddApiAnn kw dca = dc
    rd = case last ns of
      L (SrcSpanAnn ApiAnnNotUsed    l) _ -> realSrcSpan l
      L (SrcSpanAnn (ApiAnn anc _ _) _) _ -> anchor anc -- TODO MovedAnchor?
    -- DP (line, col) = ss2delta (ss2pos $ anchor $ getLoc lc) r
    dc' = case dca of
      AR r -> AddApiAnn kw (AD $ ss2delta (ss2posEnd rd) r)
      AD d -> AddApiAnn kw dca

    -- ---------------------------------

    ty' = case ty of
      (L (SrcSpanAnn ApiAnnNotUsed    l) b)
        -> let
             op = case dca of
               AR r -> MovedAnchor (ss2delta (ss2posEnd r) (realSrcSpan l))
               AD _ -> MovedAnchor (DP (0,1))
           in (L (SrcSpanAnn (ApiAnn (Anchor (realSrcSpan l) op) mempty noCom) l) b)
      (L (SrcSpanAnn (ApiAnn (Anchor r op) a c) l) b)
        -> let
              op' = case op of
                MovedAnchor _ -> op
                _ -> case dca of
                  AR dcr -> MovedAnchor (ss2delta (ss2posEnd dcr) r)
                  AD _ -> MovedAnchor (DP (0,1))
           in (L (SrcSpanAnn (ApiAnn (Anchor r op') a c) l) b)


captureTypeSigSpacing s = s

-- ---------------------------------------------------------------------

-- |Pure function to convert a 'LHsDecl' to a 'LHsBind'. This does
-- nothing to any annotations that may be attached to either of the elements.
-- It is used as a utility function in 'replaceDecls'
decl2Bind :: LHsDecl GhcPs -> [LHsBind GhcPs]
decl2Bind (L l (ValD _ s)) = [L l s]
decl2Bind _                      = []

-- |Pure function to convert a 'LSig' to a 'LHsBind'. This does
-- nothing to any annotations that may be attached to either of the elements.
-- It is used as a utility function in 'replaceDecls'
decl2Sig :: LHsDecl GhcPs -> [LSig GhcPs]
decl2Sig (L l (SigD _ s)) = [L l s]
decl2Sig _                = []

-- ---------------------------------------------------------------------

-- |Convert a 'LSig' into a 'LHsDecl'
wrapSig :: LSig GhcPs -> LHsDecl GhcPs
wrapSig (L l s) = L l (SigD NoExtField s)

-- ---------------------------------------------------------------------

-- |Convert a 'LHsBind' into a 'LHsDecl'
wrapDecl :: LHsBind GhcPs -> LHsDecl GhcPs
wrapDecl (L l s) = L l (ValD NoExtField s)

-- ---------------------------------------------------------------------

-- |Create a simple 'Annotation' without comments, and attach it to the first
-- parameter.
addSimpleAnnT :: (Data a,Monad m)
              => Located a -> DeltaPos -> [(KeywordId, DeltaPos)] -> TransformT m ()
addSimpleAnnT ast dp kds = do
  let ann = annNone { annEntryDelta = dp
                    , annsDP = kds
                    }
  modifyAnnsT (Map.insert (mkAnnKey ast) ann)

-- ---------------------------------------------------------------------

-- |Add a trailing comma annotation, unless there is already one
addTrailingCommaT :: (Data a,Monad m) => Located a -> TransformT m ()
addTrailingCommaT ast = do
  modifyAnnsT (addTrailingComma ast (DP (0,0)))

-- ---------------------------------------------------------------------

-- |Remove a trailing comma annotation, if there is one one
removeTrailingCommaT :: (Data a,Monad m) => Located a -> TransformT m ()
removeTrailingCommaT ast = do
  modifyAnnsT (removeTrailingComma ast)

-- ---------------------------------------------------------------------

-- |'Transform' monad version of 'getEntryDP'
getEntryDPT :: (Data a,Monad m) => Located a -> TransformT m DeltaPos
getEntryDPT ast = do
  anns <- getAnnsT
  return (getEntryDP anns ast)

-- ---------------------------------------------------------------------

-- |'Transform' monad version of 'getEntryDP'
setEntryDPT :: (Data a,Monad m) => LocatedA a -> DeltaPos -> TransformT m ()
setEntryDPT ast dp = do
  modifyAnnsT (setEntryDP ast dp)

-- ---------------------------------------------------------------------

-- |'Transform' monad version of 'transferEntryDP'
transferEntryDPT :: (Data a,Data b,Monad m) => LocatedA a -> LocatedA b -> TransformT m (LocatedA b)
transferEntryDPT a b = do
  return b
  -- modifyAnnsT (transferEntryDP a b)

-- ---------------------------------------------------------------------

-- |'Transform' monad version of 'setPrecedingLinesDecl'
setPrecedingLinesDeclT :: (Monad m) => LHsDecl GhcPs -> Int -> Int -> TransformT m ()
setPrecedingLinesDeclT ld n c =
  modifyAnnsT (setPrecedingLinesDecl ld n c)

-- ---------------------------------------------------------------------

-- |'Transform' monad version of 'setPrecedingLines'
setPrecedingLinesT ::  (Data a,Monad m) => LocatedA a -> Int -> Int -> TransformT m ()
setPrecedingLinesT ld n c =
  modifyAnnsT (setPrecedingLines ld n c)

-- ---------------------------------------------------------------------

-- | Left bias pair union
mergeAnns :: Anns -> Anns -> Anns
mergeAnns
  = Map.union

-- |Combine a list of annotations
mergeAnnList :: [Anns] -> Anns
mergeAnnList [] = error "mergeAnnList must have at lease one entry"
mergeAnnList (x:xs) = foldr mergeAnns x xs

-- ---------------------------------------------------------------------

-- |Unwrap a HsDecl and call setPrecedingLines on it
-- ++AZ++ TODO: get rid of this, it is a synonym only
setPrecedingLinesDecl :: LHsDecl GhcPs -> Int -> Int -> Anns -> Anns
setPrecedingLinesDecl ld n c ans = setPrecedingLines ld n c ans

-- ---------------------------------------------------------------------

-- | Adjust the entry annotations to provide an `n` line preceding gap
setPrecedingLines :: (Data a) => LocatedA a -> Int -> Int -> Anns -> Anns
setPrecedingLines ast n c anne = setEntryDP ast (DP (n,c)) anne

-- ---------------------------------------------------------------------

-- |Return the true entry 'DeltaPos' from the annotation for a given AST
-- element. This is the 'DeltaPos' ignoring any comments.
getEntryDP :: (Data a) => Anns -> Located a -> DeltaPos
getEntryDP anns ast =
  case Map.lookup (mkAnnKey ast) anns of
    Nothing  -> DP (0,0)
    Just ann -> annTrueEntryDelta ann

-- ---------------------------------------------------------------------

setEntryDPDecl :: LHsDecl GhcPs -> DeltaPos -> LHsDecl GhcPs
setEntryDPDecl decl@(L l (ValD x (FunBind a b (MG c (L d ms ) e) f))) dp
                       = L l' (ValD x (FunBind a b (MG c (L d ms') e) f))
    where
      L l' _ = setEntryDP' decl dp
      ms' :: [LMatch GhcPs (LHsExpr GhcPs)]
      ms' = case ms of
        [] -> []
        (m0:ms) -> setEntryDP' m0 dp : ms
setEntryDPDecl d dp = setEntryDP' d dp

-- ---------------------------------------------------------------------

-- |Set the true entry 'DeltaPos' from the annotation for a given AST
-- element. This is the 'DeltaPos' ignoring any comments.
-- setEntryDP' :: (Data a) => LocatedA a -> DeltaPos -> LocatedA a
setEntryDP' :: (Monoid t) => LocatedAn t a -> DeltaPos -> LocatedAn t a
setEntryDP' (L (SrcSpanAnn ApiAnnNotUsed l) a) dp
  = L (SrcSpanAnn
           (ApiAnn (Anchor (realSrcSpan l) (MovedAnchor dp)) mempty noCom)
           l) a
setEntryDP' (L (SrcSpanAnn (ApiAnn (Anchor r _) an (AnnComments [])) l) a) dp
  = L (SrcSpanAnn
           (ApiAnn (Anchor r (MovedAnchor dp)) an (AnnComments []))
           l) a
setEntryDP' (L (SrcSpanAnn (ApiAnn (Anchor r _) an cs) l) a) dp
  = case sort (priorComments cs) of
      [] ->
        L (SrcSpanAnn
               (ApiAnn (Anchor r (MovedAnchor dp)) an cs)
               l) a
      (L ca c:cs') ->
        L (SrcSpanAnn
               (ApiAnn (Anchor r (MovedAnchor edp)) an cs'')
               l) a
              where
                cs'' = setPriorComments cs (L (Anchor (anchor ca) (MovedAnchor dp)) c:cs')
                lc = head $ reverse $ (L ca c:cs')
                DP (line, col) = ss2delta (ss2pos $ anchor $ getLoc lc) r
                -- TODO: this adjustment by 1 happens all over the place. Generalise it
                edp' = if line == 0 then DP (line, col)
                                    else DP (line, col - 1)
                edp = edp' `debug` ("setEntryDP' :" ++ showGhc (edp', (ss2pos $ anchor $ getLoc lc), r))
                -- edp = if line == 0 then DP (line, col)
                --                    else DP (line, col - 1)

-- |Set the true entry 'DeltaPos' from the annotation for a given AST
-- element. This is the 'DeltaPos' ignoring any comments.
setEntryDP :: (Data a) => LocatedA a -> DeltaPos -> Anns -> Anns
setEntryDP ast dp anns = anns

-- ---------------------------------------------------------------------

addAnnAnchorDelta :: LayoutStartCol -> RealSrcSpan -> AnnAnchor -> AnnAnchor
addAnnAnchorDelta off anc (AD d) = AD d
addAnnAnchorDelta off anc (AR r)
  = AD (adjustDeltaForOffset 0 off (ss2deltaEnd anc r))

-- Set the entry DP for an element coming after an existing keyword annotation
setEntryDPFromAnchor :: LayoutStartCol -> AnnAnchor -> LocatedA t -> LocatedA t
setEntryDPFromAnchor off (AD d) (L la a) = L la a
setEntryDPFromAnchor off (AR anc) ll@(L la a) = setEntryDP' ll dp'
  where
    r = case la of
      (SrcSpanAnn ApiAnnNotUsed l) -> realSrcSpan l
      (SrcSpanAnn (ApiAnn (Anchor r _) _ _) _) -> r
    dp' = adjustDeltaForOffset 0 off (ss2deltaEnd anc r)
    dp = error $ "setEntryDPFromAnchor:" ++ show (off,rs2range anc,rs2range r,dp')

-- ---------------------------------------------------------------------

-- |When setting an entryDP, the leading comment needs to be adjusted too
setCommentEntryDP :: Annotation -> DeltaPos -> Annotation
-- setCommentEntryDP ann dp = error $ "setCommentEntryDP:ann'=" ++ show ann'
setCommentEntryDP ann dp = ann'
  where
    ann' = case (annPriorComments ann) of
      [] -> ann
      [(pc,_)]     -> ann { annPriorComments = [(pc,dp)] }
      ((pc,_):pcs) -> ann { annPriorComments = ((pc,dp):pcs) }

-- ---------------------------------------------------------------------

-- |Take the annEntryDelta associated with the first item and associate it with the second.
-- Also transfer any comments occuring before it.
transferEntryDP :: (Monad m, Monoid t) => LocatedAn t a -> LocatedAn t b -> TransformT m (LocatedAn t b)
transferEntryDP (L (SrcSpanAnn ApiAnnNotUsed l1) _) (L (SrcSpanAnn ApiAnnNotUsed _) b) = do
  logTr $ "transferEntryDP': ApiAnnNotUsed,ApiAnnNotUsed"
  return (L (SrcSpanAnn ApiAnnNotUsed l1) b)
transferEntryDP (L (SrcSpanAnn (ApiAnn anc an cs) l1) _) (L (SrcSpanAnn ApiAnnNotUsed l2) b) = do
  logTr $ "transferEntryDP': ApiAnn,ApiAnnNotUsed"
  return (L (SrcSpanAnn (ApiAnn anc mempty cs) l2) b)
transferEntryDP (L (SrcSpanAnn (ApiAnn anc1 an1 cs1) l1) _) (L (SrcSpanAnn (ApiAnn anc2 an2 cs2) l2) b) = do
  logTr $ "transferEntryDP': ApiAnn,ApiAnn"
  -- Problem: if the original had preceding comments, blindly
  -- transferring the location is not correct
  case priorComments cs1 of
    [] -> return (L (SrcSpanAnn (ApiAnn anc1 an2 cs2) l2) b)
    -- TODO: what happens if the receiving side already has comments?
    (L anc _:_) -> do
      logDataWithAnnsTr "transferEntryDP':priorComments anc=" anc
      return (L (SrcSpanAnn (ApiAnn (kludgeAnchor anc) an2 cs2) l2) b)
transferEntryDP (L (SrcSpanAnn ApiAnnNotUsed l1) _) (L (SrcSpanAnn (ApiAnn anc2 an2 cs2) l2) b) = do
  logTr $ "transferEntryDP': ApiAnnNotUsed,ApiAnn"
  return (L (SrcSpanAnn (ApiAnn anc2' an2 cs2) l2) b)
    where
      anc2' = case anc2 of
        Anchor a op   -> Anchor (realSrcSpan l2) op

-- |Take the annEntryDelta associated with the first item and associate it with the second.
-- Also transfer any comments occuring before it.
-- TODO: call transferEntryDP, and use pushDeclDP
transferEntryDP' :: (Monad m) => LHsDecl GhcPs -> LHsDecl GhcPs -> TransformT m (LHsDecl GhcPs)
transferEntryDP' la lb = do
  (L l2 b) <- transferEntryDP la lb
  return (L l2 (pushDeclDP b (DP (0,0))))

-- There is an off-by-one in DPs. I *think* it has to do wether we
-- calculate the final position when applying it against the stored
-- final pos or against another RealSrcSpan.  Must get to the bottom
-- of it and come up with a canonical DP.  This function adjusts a
-- "comment space" DP to a "enterAnn" space one
kludgeAnchor :: Anchor -> Anchor
kludgeAnchor a@(Anchor _ (MovedAnchor (DP (0,_)))) = a
kludgeAnchor (Anchor a (MovedAnchor (DP (r,c)))) = (Anchor a (MovedAnchor (DP (r,c - 1))))
kludgeAnchor a = a

pushDeclDP :: HsDecl GhcPs -> DeltaPos -> HsDecl GhcPs
pushDeclDP decl@(ValD x (FunBind a b (MG c (L d  ms ) e) f)) dp
               = ValD x (FunBind a b (MG c (L d' ms') e) f)
    where
      L d' _ = setEntryDP' (L d ms) dp
      ms' :: [LMatch GhcPs (LHsExpr GhcPs)]
      ms' = case ms of
        [] -> []
        (m0:ms) -> setEntryDP' m0 dp : ms
pushDeclDP d _dp = d

-- ---------------------------------------------------------------------

addTrailingComma :: (Data a) => Located a -> DeltaPos -> Anns -> Anns
addTrailingComma a dp anns =
  case Map.lookup (mkAnnKey a) anns of
    Nothing -> anns
    Just an ->
      case find isAnnComma (annsDP an) of
        Nothing -> Map.insert (mkAnnKey a) (an { annsDP = annsDP an ++ [(G AnnComma,dp)]}) anns
        Just _  -> anns
      where
        isAnnComma (G AnnComma,_) = True
        isAnnComma _              = False

-- ---------------------------------------------------------------------

removeTrailingComma :: (Data a) => Located a -> Anns -> Anns
removeTrailingComma a anns =
  case Map.lookup (mkAnnKey a) anns of
    Nothing -> anns
    Just an ->
      case find isAnnComma (annsDP an) of
        Nothing -> anns
        Just _  -> Map.insert (mkAnnKey a) (an { annsDP = filter (not.isAnnComma) (annsDP an) }) anns
      where
        isAnnComma (G AnnComma,_) = True
        isAnnComma _              = False

-- ---------------------------------------------------------------------

balanceCommentsList :: (Monad m) => [LHsDecl GhcPs] -> TransformT m [LHsDecl GhcPs]
balanceCommentsList [] = return []
balanceCommentsList [x] = return [x]
balanceCommentsList (a:b:ls) = do
  (a',b') <- balanceComments a b
  r <- balanceCommentsList (b':ls)
  return (a':r)

-- |The relatavise phase puts all comments appearing between the end of one AST
-- item and the beginning of the next as 'annPriorComments' for the second one.
-- This function takes two adjacent AST items and moves any 'annPriorComments'
-- from the second one to the 'annFollowingComments' of the first if they belong
-- to it instead. This is typically required before deleting or duplicating
-- either of the AST elements.
balanceComments :: (Monad m)
  => LHsDecl GhcPs -> LHsDecl GhcPs
  -> TransformT m (LHsDecl GhcPs, LHsDecl GhcPs)
balanceComments first second = do
  -- ++AZ++ : replace the nested casts with appropriate gmapM
  -- logTr $ "balanceComments entered"
  -- logDataWithAnnsTr "first" first
  case first of
    (L l (ValD x fb@(FunBind{}))) -> do
      (L l' fb',second') <- balanceCommentsFB (L l fb) second
      return (L l' (ValD x fb'), second')
    _ -> balanceComments' first second

-- |Once 'balanceComments' has been called to move trailing comments to a
-- 'FunBind', these need to be pushed down from the top level to the last
-- 'Match' if that 'Match' needs to be manipulated.
balanceCommentsFB :: (Data b,Monad m)
  => LHsBind GhcPs -> LocatedA b -> TransformT m (LHsBind GhcPs, LocatedA b)
balanceCommentsFB (L lf (FunBind x n (MG mx (L lm matches) o) t)) second = do
  logTr $ "balanceCommentsFB entered: " ++ showGhc (ss2range $ locA lf)
  matches' <- balanceCommentsList' matches
  let (m:ms) = reverse matches'
  (m',second') <- balanceComments' m second
  m'' <- balanceCommentsMatch m'
  logTr $ "balanceCommentsMatch done"
  return (L lf (FunBind x n (MG mx (L lm (reverse (m'':ms))) o) t), second')
balanceCommentsFB f s = balanceComments' f s

-- | Move comments on the same line as the end of the match into the
-- GRHS, prior to the binds
balanceCommentsMatch :: (Monad m)
  => LMatch GhcPs (LHsExpr GhcPs) -> TransformT m (LMatch GhcPs (LHsExpr GhcPs))
balanceCommentsMatch (L l (Match am mctxt pats (GRHSs xg grhss binds))) = do
  logTr $ "balanceCommentsMatch: (loc1)=" ++ showGhc (ss2range (locA l))
  logTr $ "balanceCommentsMatch: (move',stay')=" ++ showAst (move',stay')
  logTr $ "balanceCommentsMatch: (logInfo)=" ++ showAst (logInfo)
  logTr $ "balanceCommentsMatch: (loc1)=" ++ showGhc (ss2range (locA l))
  logTr $ "balanceCommentsMatch: (anc1,cs1f)=" ++ showAst (anc1,cs1f)
  logTr $ "balanceCommentsMatch: (l'', grhss')=" ++ showAst (l'', grhss')
  return (L l'' (Match am mctxt pats (GRHSs xg grhss' binds')))
  where
    simpleBreak (r,_) = r /= 0
    (SrcSpanAnn an1 loc1) = l
    anc1 = addCommentOrigDeltas $ apiAnnComments an1
    cs1f = getFollowingComments anc1
    -- (move',stay') = break simpleBreak (commentsDeltas (anchorFromLocatedA (L l ())) cs1f)
    (move',stay') = break simpleBreak (trailingCommentsDeltas (anchorFromLocatedA (L l ())) cs1f)
    move = map snd move'
    stay = map snd stay'
    (l'', grhss', binds', logInfo)
      = case reverse grhss of
          [] -> (l, [], binds, (AnnComments [], SrcSpanAnn ApiAnnNotUsed noSrcSpan))
          (L lg (GRHS ag grs rhs):gs) ->
            let
              anc1' = setFollowingComments anc1 stay
              an1' = setCommentsSSA l anc1'

              -- ---------------------------------
              (moved,bindsm) = pushTrailingComments WithWhere (AnnCommentsBalanced [] move) binds
              -- ---------------------------------

              (ApiAnn anc an lgc) = ag
              lgc' = splitComments (realSrcSpan lg) $ addCommentOrigDeltas lgc
              ag' = if moved
                      then ApiAnn anc an lgc'
                      else ApiAnn anc an (lgc' <> (AnnCommentsBalanced [] move))
              -- ag' = ApiAnn anc an lgc'

            in (an1', (reverse $ (L lg (GRHS ag' grs rhs):gs)), bindsm, (anc1',an1'))

pushTrailingComments :: WithWhere -> ApiAnnComments -> HsLocalBinds GhcPs -> (Bool, HsLocalBinds GhcPs)
pushTrailingComments _ _cs b@EmptyLocalBinds{} = (False, b)
pushTrailingComments w cs lb@(HsValBinds an (ValBinds _sk binds sigs))
  = (True, HsValBinds an' vb)
  where
    (decls, _, ws1) = runTransform mempty (hsDeclsValBinds lb)
    (an', decls') = case reverse decls of
      [] -> (addCommentsToApiAnn (spanHsLocaLBinds lb) an cs, decls)
      (L la d:ds) -> (an, L (addCommentsToSSA la cs) d:ds)
    (lb'@(HsValBinds _ vb), _, ws2) = runTransform mempty (replaceDeclsValbinds w lb decls')


balanceCommentsList' :: (Monad m) => [LocatedA a] -> TransformT m [LocatedA a]
balanceCommentsList' [] = return []
balanceCommentsList' [x] = return [x]
balanceCommentsList' (a:b:ls) = do
  logTr $ "balanceCommentsList' entered"
  (a',b') <- balanceComments' a b
  r <- balanceCommentsList' (b':ls)
  return (a':r)

-- |Prior to moving an AST element, make sure any trailing comments belonging to
-- it are attached to it, and not the following element. Of necessity this is a
-- heuristic process, to be tuned later. Possibly a variant should be provided
-- with a passed-in decision function.
-- The initial situation is that all comments for a given anchor appear as prior comments
-- Many of these should in fact be following comments for the previous anchor
balanceComments' :: (Monad m) => LocatedA a -> LocatedA b -> TransformT m (LocatedA a, LocatedA b)
balanceComments' la1 la2 = do
  logTr $ "balanceComments': (loc1,loc2)=" ++ showGhc (ss2range loc1,ss2range loc2)
  logTr $ "balanceComments': (anchorFromLocatedA la1)=" ++ showGhc (anchorFromLocatedA la1)
  logTr $ "balanceComments': (sort cs2b)=" ++ showAst (sort cs2b)
  logTr $ "balanceComments': (move',stay')=" ++ showAst (move',stay')
  logTr $ "balanceComments': (move'',stay'')=" ++ showAst (move'',stay'')
  logTr $ "balanceComments': (move,stay)=" ++ showAst (move,stay)
  return (la1', la2')
  where
    simpleBreak n (r,_) = r > n
    L (SrcSpanAnn an1 loc1) f = la1
    L (SrcSpanAnn an2 loc2) s = la2
    anc1 = addCommentOrigDeltas $ apiAnnComments an1
    anc2 = addCommentOrigDeltas $ apiAnnComments an2
    cs1f = getFollowingComments anc1
    cs2b = priorComments anc2
    (stay'',move') = break (simpleBreak 1) (priorCommentsDeltas (anchorFromLocatedA la2) cs2b)
    -- Need to also check for comments more closely attached to la1,
    -- ie trailing on the same line
    (move'',stay') = break (simpleBreak 0) (trailingCommentsDeltas (anchorFromLocatedA la1) (map snd stay''))
    move = map snd (move'' ++ move')
    stay = map snd stay'
    cs1 = setFollowingComments anc1 (sort $ cs1f ++ move)
    cs2 = setPriorComments anc2 stay

    an1' = setCommentsSSA (getLoc la1) cs1
    an2' = setCommentsSSA (getLoc la2) cs2
    la1' = L an1' f
    la2' = L an2' s

-- | Like commentsDeltas, but calculates the delta from the end of the anchor, not the start
trailingCommentsDeltas :: RealSrcSpan -> [LAnnotationComment]
               -> [(Int, LAnnotationComment)]
trailingCommentsDeltas _ [] = []
trailingCommentsDeltas anc (la@(L l _):las)
  = deltaComment anc la : trailingCommentsDeltas (anchor l) las
  where
    deltaComment anc (L l c) = (abs(ll - al), L l c)
      where
        (al,_) = ss2posEnd anc
        (ll,_) = ss2pos (anchor l)

-- AZ:TODO: this is identical to commentsDeltas
priorCommentsDeltas :: RealSrcSpan -> [LAnnotationComment]
                    -> [(Int, LAnnotationComment)]
priorCommentsDeltas anc cs = go anc (reverse $ sort cs)
  where
    go :: RealSrcSpan -> [LAnnotationComment] -> [(Int, LAnnotationComment)]
    go _ [] = []
    go anc (la@(L l _):las) = deltaComment anc la : go (anchor l) las

    deltaComment :: RealSrcSpan -> LAnnotationComment -> (Int, LAnnotationComment)
    deltaComment anc (L l c) = (abs(ll - al), L l c)
      where
        (al,_) = ss2pos anc
        (ll,_) = ss2pos (anchor l)

commentsDeltas :: RealSrcSpan -> [LAnnotationComment]
               -> [(Int, LAnnotationComment)]
commentsDeltas _ [] = []
commentsDeltas anc (la@(L l _):las) = deltaComment anc la : commentsDeltas (anchor l) las
  where
    deltaComment anc (L l c) = (abs(ll - al), L l c)
      where
        (al,_) = ss2pos anc
        (ll,_) = ss2pos (anchor l)

-- | Split comments into ones occuring before the end of the reference
-- span, and those after it.
splitComments :: RealSrcSpan -> ApiAnnComments -> ApiAnnComments
splitComments p (AnnComments cs) = cs'
  where
    cmp (L (Anchor l _) _) = ss2pos l < ss2posEnd p
    (before, after) = break cmp cs
    cs' = case after of
      [] -> AnnComments cs
      _ -> AnnCommentsBalanced before after
splitComments p (AnnCommentsBalanced cs ts) = AnnCommentsBalanced cs' ts'
  where
    cmp (L (Anchor l _) _) = ss2pos l < ss2posEnd p
    (before, after) = break cmp cs
    cs' = before
    ts' = after <> ts

-- | A GHC comment includes the span of the preceding (non-comment)
-- token.  Takes an original list of comments, and converts the
-- 'Anchor's to have a have a `MovedAnchor` operation based on the
-- original locations.
commentOrigDeltas :: [LAnnotationComment] -> [LAnnotationComment]
commentOrigDeltas [] = []
commentOrigDeltas ls@(L _ (GHC.AnnComment _ p):_) = go p ls
  -- TODO:AZ: we now have deltas wrt *all* tokens, not just preceding
  -- non-comment. Simplify this.
  where
    go :: RealSrcSpan -> [LAnnotationComment] -> [LAnnotationComment]
    go _ [] = []
    go p (L (Anchor la _) (GHC.AnnComment t pp):ls)
      = L (Anchor la op) (GHC.AnnComment t pp) : go p' ls
      where
        p' = p
        (r,c) = ss2posEnd pp
        op' = if r == 0
               then MovedAnchor (ss2delta (r,c+1) la)
               else MovedAnchor (ss2delta (r,c)   la)
        op = if t == AnnEofComment && op' == MovedAnchor (DP (0,0))
               then MovedAnchor (DP (1,0))
               else op'

addCommentOrigDeltas :: ApiAnnComments -> ApiAnnComments
addCommentOrigDeltas (AnnComments cs) = AnnComments (commentOrigDeltas cs)
addCommentOrigDeltas (AnnCommentsBalanced pcs fcs)
  = AnnCommentsBalanced (commentOrigDeltas pcs) (commentOrigDeltas fcs)

addCommentOrigDeltasAnn :: (ApiAnn' a) -> (ApiAnn' a)
addCommentOrigDeltasAnn ApiAnnNotUsed   = ApiAnnNotUsed
addCommentOrigDeltasAnn (ApiAnn e a cs) = ApiAnn e a (addCommentOrigDeltas cs)

addCommentOrigDeltasSSA :: SrcSpanAnn' (ApiAnn' a) -> SrcSpanAnn' (ApiAnn' a)
addCommentOrigDeltasSSA (SrcSpanAnn an l) = SrcSpanAnn (addCommentOrigDeltasAnn an) l

-- TODO: this is replicating functionality in ExactPrint. Sort out the
-- import loop`
anchorFromLocatedA :: LocatedA a -> RealSrcSpan
anchorFromLocatedA (L (SrcSpanAnn an loc) _)
  = case an of
      ApiAnnNotUsed    -> realSrcSpan loc
      (ApiAnn anc _ _) -> anchor anc

-- ---------------------------------------------------------------------

balanceSameLineComments :: (Monad m)
  => LMatch GhcPs (LHsExpr GhcPs) -> TransformT m (LMatch GhcPs (LHsExpr GhcPs))
balanceSameLineComments (L la (Match an mctxt pats (GRHSs x grhss lb))) = do
  logTr $ "balanceSameLineComments: (la)=" ++ showGhc (ss2range $ locA la)
  logTr $ "balanceSameLineComments: [logInfo]=" ++ showAst logInfo
  return (L la' (Match an mctxt pats (GRHSs x grhss' lb)))
  where
    simpleBreak n (r,_) = r > n
    (la',grhss', logInfo) = case reverse grhss of
      [] -> (la,grhss,[])
      (L lg (GRHS ga gs rhs):grs) -> (la'',reverse $ (L lg (GRHS ga' gs rhs)):grs,[(gac,(csp,csf))])
        where
          (SrcSpanAnn an1 loc1) = la
          anc1 = addCommentOrigDeltas $ apiAnnComments an1
          (ApiAnn anc an _) = ga :: ApiAnn' GrhsAnn
          (csp,csf) = case anc1 of
            AnnComments cs -> ([],cs)
            AnnCommentsBalanced p f -> (p,f)
          (move',stay') = break (simpleBreak 0) (trailingCommentsDeltas (anchor anc) csf)
          move = map snd move'
          stay = map snd stay'
          cs1 = AnnCommentsBalanced csp stay

          gac = addCommentOrigDeltas $ apiAnnComments ga
          gfc = getFollowingComments gac
          gac' = setFollowingComments gac (sort $ gfc ++ move)
          ga' = (ApiAnn anc an gac')

          an1' = setCommentsSSA la cs1
          la'' = an1'

-- ---------------------------------------------------------------------


-- |After moving an AST element, make sure any comments that may belong
-- with the following element in fact do. Of necessity this is a heuristic
-- process, to be tuned later. Possibly a variant should be provided with a
-- passed-in decision function.
balanceTrailingComments :: (Monad m) => (Data a,Data b) => Located a -> Located b
                        -> TransformT m [(Comment, DeltaPos)]
balanceTrailingComments first second = do
  let
    k1 = mkAnnKey first
    k2 = mkAnnKey second
    moveComments p ans = (ans',move)
      where
        an1 = gfromJust "balanceTrailingComments k1" $ Map.lookup k1 ans
        an2 = gfromJust "balanceTrailingComments k2" $ Map.lookup k2 ans
        cs1f = annFollowingComments an1
        (move,stay) = break p cs1f
        an1' = an1 { annFollowingComments = stay }
        ans' = Map.insert k1 an1' $ Map.insert k2 an2 ans

    simpleBreak (_,DP (r,_c)) = r > 0

  ans <- getAnnsT
  let (ans',mov) = moveComments simpleBreak ans
  putAnnsT ans'
  return mov

-- ---------------------------------------------------------------------

-- ++AZ++ TODO: This needs to be renamed/reworked, based on what it actually gets used for
-- |Move any 'annFollowingComments' values from the 'Annotation' associated to
-- the first parameter to that of the second.
moveTrailingComments :: (Data a,Data b)
                     => Located a -> Located b -> Transform ()
moveTrailingComments first second = do
  let
    k1 = mkAnnKey first
    k2 = mkAnnKey second
    moveComments ans = ans'
      where
        an1 = gfromJust "moveTrailingComments k1" $ Map.lookup k1 ans
        an2 = gfromJust "moveTrailingComments k2" $ Map.lookup k2 ans
        cs1f = annFollowingComments an1
        cs2f = annFollowingComments an2
        an1' = an1 { annFollowingComments = [] }
        an2' = an2 { annFollowingComments = cs1f ++ cs2f }
        ans' = Map.insert k1 an1' $ Map.insert k2 an2' ans

  modifyAnnsT moveComments

-- ---------------------------------------------------------------------

anchorEof :: ParsedSource -> ParsedSource
anchorEof (L l m@(HsModule an _lo _mn _exps _imps _decls _ _)) = L l (m { hsmodAnn = an' })
  where
    an' = addCommentOrigDeltasAnn an

-- ---------------------------------------------------------------------

-- | Take an anchor and a preceding location, and generate an
-- equivalent one with a 'MovedAnchor' delta.
deltaAnchor :: Anchor -> RealSrcSpan -> Anchor
deltaAnchor (Anchor anc _) ss = Anchor anc (MovedAnchor dp)
  where
    dp = ss2delta (ss2pos anc) ss

-- ---------------------------------------------------------------------

-- | Create a @SrcSpanAnn@ with a @MovedAnchor@ operation using the
-- given @DeltaPos@.
noAnnSrcSpanDP :: (Monoid ann) => SrcSpan -> DeltaPos -> SrcSpanAnn' (ApiAnn' ann)
noAnnSrcSpanDP l dp
  = SrcSpanAnn (ApiAnn (Anchor (realSrcSpan l) (MovedAnchor dp)) mempty noCom) l

noAnnSrcSpanDP0 :: (Monoid ann) => SrcSpan -> SrcSpanAnn' (ApiAnn' ann)
noAnnSrcSpanDP0 l = noAnnSrcSpanDP l (DP (0,0))

noAnnSrcSpanDP1 :: (Monoid ann) => SrcSpan -> SrcSpanAnn' (ApiAnn' ann)
noAnnSrcSpanDP1 l = noAnnSrcSpanDP l (DP (0,1))

noAnnSrcSpanDPn :: (Monoid ann) => SrcSpan -> Int -> SrcSpanAnn' (ApiAnn' ann)
noAnnSrcSpanDPn l s = noAnnSrcSpanDP l (DP (0,s))

d0 :: AnnAnchor
d0 = AD $ DP (0,0)

d1 :: AnnAnchor
d1 = AD $ DP (0,1)

dn :: Int -> AnnAnchor
dn n = AD $ DP (0,n)

m0 :: AnchorOperation
m0 = MovedAnchor $ DP (0,0)

m1 :: AnchorOperation
m1 = MovedAnchor $ DP (0,1)

mn :: Int -> AnchorOperation
mn n = MovedAnchor $ DP (0,n)

addComma :: SrcSpanAnnA -> SrcSpanAnnA
addComma (SrcSpanAnn ApiAnnNotUsed l)
  = (SrcSpanAnn (ApiAnn (spanAsAnchor l) (AnnListItem [AddCommaAnn d0]) noCom) l)
addComma (SrcSpanAnn (ApiAnn anc (AnnListItem as) cs) l)
  = (SrcSpanAnn (ApiAnn anc (AnnListItem (AddCommaAnn d0:as)) cs) l)

-- ---------------------------------------------------------------------

-- | Insert a declaration into an AST element having sub-declarations
-- (@HasDecls@) according to the given location function.
insertAt :: (HasDecls ast)
              => (LHsDecl GhcPs
                  -> [LHsDecl GhcPs]
                  -> [LHsDecl GhcPs])
              -> ast
              -> LHsDecl GhcPs
              -> Transform ast
insertAt f t decl = do
  oldDecls <- hsDecls t
  replaceDecls t (f decl oldDecls)

-- |Insert a declaration at the beginning or end of the subdecls of the given
-- AST item
insertAtStart, insertAtEnd :: (HasDecls ast)
              => ast
              -> LHsDecl GhcPs
              -> Transform ast

insertAtStart = insertAt (:)
insertAtEnd   = insertAt (\x xs -> xs ++ [x])

-- |Insert a declaration at a specific location in the subdecls of the given
-- AST item
insertAfter, insertBefore :: (HasDecls (LocatedA ast))
                          => LocatedA old
                          -> LocatedA ast
                          -> LHsDecl GhcPs
                          -> Transform (LocatedA ast)
insertAfter (getLocA -> k) = insertAt findAfter
  where
    findAfter x xs =
      let (fs, b:bs) = span (\(L l _) -> locA l /= k) xs
      in fs ++ (b : x : bs)
insertBefore (getLocA -> k) = insertAt findBefore
  where
    findBefore x xs =
      let (fs, bs) = span (\(L l _) -> locA l /= k) xs
      in fs ++ (x : bs)

-- =====================================================================
-- start of HasDecls instances
-- =====================================================================

-- |Provide a means to get and process the immediate child declartions of a
-- given AST element.
class (Data t) => HasDecls t where
-- ++AZ++: TODO: add tests to confirm that hsDecls followed by replaceDecls is idempotent

    -- | Return the 'HsDecl's that are directly enclosed in the
    -- given syntax phrase. They are always returned in the wrapped 'HsDecl'
    -- form, even if orginating in local decls. This is safe, as annotations
    -- never attach to the wrapper, only to the wrapped item.
    hsDecls :: (Monad m) => t -> TransformT m [LHsDecl GhcPs]

    -- | Replace the directly enclosed decl list by the given
    --  decl list. Runs in the 'Transform' monad to be able to update list order
    --  annotations, and rebalance comments and other layout changes as needed.
    --
    -- For example, a call on replaceDecls for a wrapped 'FunBind' having no
    -- where clause will convert
    --
    -- @
    -- -- |This is a function
    -- foo = x -- comment1
    -- @
    -- in to
    --
    -- @
    -- -- |This is a function
    -- foo = x -- comment1
    --   where
    --     nn = 2
    -- @
    replaceDecls :: (Monad m) => t -> [LHsDecl GhcPs] -> TransformT m t

-- ---------------------------------------------------------------------

instance HasDecls ParsedSource where
  hsDecls (L _ (HsModule _ _lo _mn _exps _imps decls _ _)) = return decls
  replaceDecls m@(L l (HsModule a lo mn exps imps _decls deps haddocks)) decls
    = do
        logTr "replaceDecls LHsModule"
        -- modifyAnnsT (captureOrder m decls)
        return (L l (HsModule a lo mn exps imps decls deps haddocks))

-- ---------------------------------------------------------------------

instance HasDecls (LocatedA (Match GhcPs (LocatedA (HsExpr GhcPs)))) where
  hsDecls d@(L _ (Match _ _ _ (GRHSs _ _ lb))) = hsDeclsValBinds lb
  hsDecls (L _ (Match _ _ _ (XGRHSs _))) = return []
  hsDecls (L _ (XMatch _))               = return []

  replaceDecls m@(L l (Match xm c p (GRHSs xr rhs binds))) []
    = do
        logTr "replaceDecls LMatch empty decls"
        binds'' <- replaceDeclsValbinds WithoutWhere binds []
        return (L l (Match xm c p (GRHSs xr rhs binds'')))

  replaceDecls m@(L l (Match xm c p (GRHSs xr rhs binds))) newBinds
    = do
        logTr "replaceDecls LMatch nonempty decls"
        -- Need to throw in a fresh where clause if the binds were empty,
        -- in the annotations.
        (l', rhs') <- case binds of
          EmptyLocalBinds{} -> do
            logTr $ "replaceDecls LMatch empty binds"
            modifyAnnsT (setPrecedingLines (ghead "LMatch.replaceDecls" newBinds) 1 4)

            -- only move the comment if the original where clause was empty.
            -- toMove <- balanceTrailingComments m m
            -- insertCommentBefore (mkAnnKey m) toMove (matchApiAnn AnnWhere)
            -- TODO: move trailing comments on the same line to before the binds
            logDataWithAnnsTr "Match.replaceDecls:balancing comments:m" m
            L l' m' <- balanceSameLineComments m
            logDataWithAnnsTr "Match.replaceDecls:(m1')" (L l' m')
            return (l', grhssGRHSs $ m_grhss m')
          _ -> return (l, rhs)
        binds'' <- replaceDeclsValbinds WithWhere binds newBinds
        logDataWithAnnsTr "Match.replaceDecls:binds'" binds''
        return (L l' (Match xm c p (GRHSs xr rhs' binds'')))
  replaceDecls (L _ (Match _ _ _ (XGRHSs _))) _ = error "replaceDecls"
  replaceDecls (L _ (XMatch _)) _               = error "replaceDecls"

-- ---------------------------------------------------------------------

instance HasDecls (LocatedA (HsExpr GhcPs)) where
  hsDecls (L _ (HsLet _ decls _ex)) = hsDeclsValBinds decls
  hsDecls _                         = return []

  replaceDecls e@(L l (HsLet x binds ex)) newDecls
    = do
        logTr "replaceDecls HsLet"
        -- modifyAnnsT (captureOrder e newDecls)
        let lastAnc = realSrcSpan $ spanHsLocaLBinds binds
        -- TODO: may be an intervening comment, take account for lastAnc
        let (x', ex',newDecls') = case x of
              ApiAnnNotUsed -> (x, ex, newDecls)
              (ApiAnn a (AnnsLet l i) cs) ->
                let
                  off = case l of
                          (AR r) -> LayoutStartCol $ snd $ ss2pos r
                          (AD (DP (0,_))) -> LayoutStartCol 0
                          (AD (DP (_,c))) -> LayoutStartCol c
                  ex'' = setEntryDPFromAnchor off i ex
                  newDecls'' = case newDecls of
                    [] -> newDecls
                    -- (d:ds) -> setEntryDP' d (DP (0,0)) : ds
                    (d:ds) -> setEntryDPDecl d (DP (0,0)) : ds
                in ( ApiAnn a (AnnsLet l (addAnnAnchorDelta off lastAnc i)) cs
                   , ex''
                   , newDecls'')
        binds' <- replaceDeclsValbinds WithoutWhere binds newDecls'
        return (L l (HsLet x' binds' ex'))

  -- TODO: does this make sense? Especially as no hsDecls for HsPar
  replaceDecls (L l (HsPar x e)) newDecls
    = do
        logTr "replaceDecls HsPar"
        e' <- replaceDecls e newDecls
        return (L l (HsPar x e'))
  replaceDecls old _new = error $ "replaceDecls (LHsExpr GhcPs) undefined for:" ++ showGhc old

-- ---------------------------------------------------------------------

-- | Extract the immediate declarations for a 'PatBind' wrapped in a 'ValD'. This
-- cannot be a member of 'HasDecls' because a 'FunBind' is not idempotent
-- for 'hsDecls' \/ 'replaceDecls'. 'hsDeclsPatBindD' \/ 'replaceDeclsPatBindD' is
-- idempotent.
hsDeclsPatBindD :: (Monad m) => LHsDecl GhcPs -> TransformT m [LHsDecl GhcPs]
hsDeclsPatBindD (L l (ValD _ d)) = hsDeclsPatBind (L l d)
hsDeclsPatBindD x = error $ "hsDeclsPatBindD called for:" ++ showGhc x

-- | Extract the immediate declarations for a 'PatBind'. This
-- cannot be a member of 'HasDecls' because a 'FunBind' is not idempotent
-- for 'hsDecls' \/ 'replaceDecls'. 'hsDeclsPatBind' \/ 'replaceDeclsPatBind' is
-- idempotent.
hsDeclsPatBind :: (Monad m) => LHsBind GhcPs -> TransformT m [LHsDecl GhcPs]
hsDeclsPatBind d@(L _ (PatBind _ _ (GRHSs _ _grhs lb) _)) = hsDeclsValBinds lb
hsDeclsPatBind x = error $ "hsDeclsPatBind called for:" ++ showGhc x

-- -------------------------------------

-- | Replace the immediate declarations for a 'PatBind' wrapped in a 'ValD'. This
-- cannot be a member of 'HasDecls' because a 'FunBind' is not idempotent
-- for 'hsDecls' \/ 'replaceDecls'. 'hsDeclsPatBindD' \/ 'replaceDeclsPatBindD' is
-- idempotent.
replaceDeclsPatBindD :: (Monad m) => LHsDecl GhcPs -> [LHsDecl GhcPs]
                     -> TransformT m (LHsDecl GhcPs)
replaceDeclsPatBindD (L l (ValD x d)) newDecls = do
  (L _ d') <- replaceDeclsPatBind (L l d) newDecls
  return (L l (ValD x d'))
replaceDeclsPatBindD x _ = error $ "replaceDeclsPatBindD called for:" ++ showGhc x

-- | Replace the immediate declarations for a 'PatBind'. This
-- cannot be a member of 'HasDecls' because a 'FunBind' is not idempotent
-- for 'hsDecls' \/ 'replaceDecls'. 'hsDeclsPatBind' \/ 'replaceDeclsPatBind' is
-- idempotent.
replaceDeclsPatBind :: (Monad m) => LHsBind GhcPs -> [LHsDecl GhcPs]
                    -> TransformT m (LHsBind GhcPs)
replaceDeclsPatBind p@(L l (PatBind x a (GRHSs xr rhss binds) b)) newDecls
    = do
        logTr "replaceDecls PatBind"
        -- Need to throw in a fresh where clause if the binds were empty,
        -- in the annotations.
        case binds of
          EmptyLocalBinds{} -> do
            let
              addWhere mkds =
                error "TBD"
                -- case Map.lookup (mkAnnKey p) mkds of
                --   Nothing -> error "wtf"
                --   Just ann -> Map.insert (mkAnnKey p) ann1 mkds
                --     where
                --       ann1 = ann { annsDP = annsDP ann ++ [(G AnnWhere,DP (1,2))]
                --                  }
            modifyAnnsT addWhere
            modifyAnnsT (setPrecedingLines (ghead "LMatch.replaceDecls" newDecls) 1 4)

          _ -> return ()

        -- modifyAnnsT (captureOrderAnnKey (mkAnnKey p) newDecls)
        binds'' <- replaceDeclsValbinds WithWhere binds newDecls
        -- let binds' = L (getLoc binds) binds''
        return (L l (PatBind x a (GRHSs xr rhss binds'') b))
replaceDeclsPatBind x _ = error $ "replaceDeclsPatBind called for:" ++ showGhc x

-- ---------------------------------------------------------------------

instance HasDecls (LocatedA (Stmt GhcPs (LocatedA (HsExpr GhcPs)))) where
  hsDecls ls@(L _ (LetStmt _ lb)) =  hsDeclsValBinds lb
  hsDecls (L _ (LastStmt _ e _ _))    = hsDecls e
  hsDecls (L _ (BindStmt _ _pat e))   = hsDecls e
  hsDecls (L _ (BodyStmt _ e _ _))    = hsDecls e
  hsDecls _                                   = return []

  replaceDecls s@(L l (LetStmt x lb)) newDecls
    = do
        -- modifyAnnsT (captureOrder s newDecls)
        lb'' <- replaceDeclsValbinds WithWhere lb newDecls
        -- let lb' = L (getLoc lb) lb''
        return (L l (LetStmt x lb''))
  replaceDecls (L l (LastStmt x e d se)) newDecls
    = do
        e' <- replaceDecls e newDecls
        return (L l (LastStmt x e' d se))
  replaceDecls (L l (BindStmt x pat e)) newDecls
    = do
      e' <- replaceDecls e newDecls
      return (L l (BindStmt x pat e'))

  replaceDecls (L l (BodyStmt x e a b)) newDecls
    = do
      e' <- replaceDecls e newDecls
      return (L l (BodyStmt x e' a b))
  replaceDecls x _newDecls = return x

-- =====================================================================
-- end of HasDecls instances
-- =====================================================================

-- ---------------------------------------------------------------------

-- |Do a transformation on an AST fragment by providing a function to process
-- the general case and one specific for a 'LHsBind'. This is required
-- because a 'FunBind' may have multiple 'Match' items, so we cannot
-- gurantee that 'replaceDecls' after 'hsDecls' is idempotent.
hasDeclsSybTransform :: (Data t2,Monad m)
       => (forall t. HasDecls t => t -> m t)
             -- ^Worker function for the general case
       -> (LHsBind GhcPs -> m (LHsBind GhcPs))
             -- ^Worker function for FunBind/PatBind
       -> t2 -- ^Item to be updated
       -> m t2
hasDeclsSybTransform workerHasDecls workerBind t = trf t
  where
    trf = mkM   parsedSource
         `extM` lmatch
         `extM` lexpr
         `extM` lstmt
         `extM` lhsbind
         `extM` lvald

    parsedSource (p::ParsedSource) = workerHasDecls p

    lmatch (lm::LMatch GhcPs (LHsExpr GhcPs))
      = workerHasDecls lm

    lexpr (le::LHsExpr GhcPs)
      = workerHasDecls le

    lstmt (d::LStmt GhcPs (LHsExpr GhcPs))
      = workerHasDecls d

    lhsbind (b@(L _ FunBind{}):: LHsBind GhcPs)
      = workerBind b
    lhsbind b@(L _ PatBind{})
      = workerBind b
    lhsbind x = return x

    lvald (L l (ValD x d)) = do
      (L _ d') <- lhsbind (L l d)
      return (L l (ValD x d'))
    lvald x = return x

-- ---------------------------------------------------------------------

-- |A 'FunBind' wraps up one or more 'Match' items. 'hsDecls' cannot
-- return anything for these as there is not meaningful 'replaceDecls' for it.
-- This function provides a version of 'hsDecls' that returns the 'FunBind'
-- decls too, where they are needed for analysis only.
hsDeclsGeneric :: (Data t,Monad m) => t -> TransformT m [LHsDecl GhcPs]
hsDeclsGeneric t = q t
  where
    q = return []
        `mkQ`  parsedSource
        `extQ` lmatch
        `extQ` lexpr
        `extQ` lstmt
        `extQ` lhsbind
        `extQ` lhsbindd
        `extQ` llocalbinds
        `extQ` localbinds

    parsedSource (p::ParsedSource) = hsDecls p

    lmatch (lm::LMatch GhcPs (LHsExpr GhcPs)) = hsDecls lm

    lexpr (le::LHsExpr GhcPs) = hsDecls le

    lstmt (d::LStmt GhcPs (LHsExpr GhcPs)) = hsDecls d

    -- ---------------------------------

    lhsbind :: (Monad m) => LHsBind GhcPs -> TransformT m [LHsDecl GhcPs]
    lhsbind (L _ (FunBind _ _ (MG _ (L _ matches) _) _)) = do
        dss <- mapM hsDecls matches
        return (concat dss)
    lhsbind p@(L _ (PatBind{})) = do
      hsDeclsPatBind p
    lhsbind _ = return []

    -- ---------------------------------

    lhsbindd (L l (ValD _ d)) = lhsbind (L l d)
    lhsbindd _ = return []

    -- ---------------------------------

    llocalbinds :: (Monad m) => Located (HsLocalBinds GhcPs) -> TransformT m [LHsDecl GhcPs]
    llocalbinds (L _ ds) = localbinds ds

    -- ---------------------------------

    localbinds :: (Monad m) => HsLocalBinds GhcPs -> TransformT m [LHsDecl GhcPs]
    localbinds d = hsDeclsValBinds d

-- ---------------------------------------------------------------------

-- |Look up the annotated order and sort the decls accordingly
-- TODO:AZ: this should be pure
orderedDecls :: (Monad m)
             => AnnSortKey -> [LHsDecl GhcPs] -> TransformT m [LHsDecl GhcPs]
orderedDecls sortKey decls = do
  case sortKey of
    NoAnnSortKey -> do
      -- return decls
      return $ sortBy (\a b -> compare (realSrcSpan $ getLocA a) (realSrcSpan $ getLocA b)) decls
    AnnSortKey keys -> do
      let ds = map (\s -> (rs $ getLocA s,s)) decls
          ordered = map snd $ orderByKey ds keys
      return ordered

-- ---------------------------------------------------------------------

hsDeclsValBinds :: (Monad m) => HsLocalBinds GhcPs -> TransformT m [LHsDecl GhcPs]
hsDeclsValBinds lb = case lb of
    HsValBinds _ (ValBinds sortKey bs sigs) -> do
      let
        bds = map wrapDecl (bagToList bs)
        sds = map wrapSig sigs
      orderedDecls sortKey (bds ++ sds)
    HsValBinds _ (XValBindsLR _) -> error $ "hsDecls.XValBindsLR not valid"
    HsIPBinds {}       -> return []
    EmptyLocalBinds {} -> return []

data WithWhere = WithWhere
               | WithoutWhere
               deriving (Eq,Show)

-- | Utility function for returning decls to 'HsLocalBinds'. Use with
-- care, as this does not manage the declaration order, the
-- ordering should be done by the calling function from the 'HsLocalBinds'
-- context in the AST.
replaceDeclsValbinds :: (Monad m)
                     => WithWhere
                     -> HsLocalBinds GhcPs -> [LHsDecl GhcPs]
                     -> TransformT m (HsLocalBinds GhcPs)
replaceDeclsValbinds _ _ [] = do
  return (EmptyLocalBinds NoExtField)
replaceDeclsValbinds w b@(HsValBinds a _) new
    = do
        logTr "replaceDeclsValbinds"
        let oldSpan = spanHsLocaLBinds b
        an <- oldWhereAnnotation a w (realSrcSpan oldSpan)
        let decs = listToBag $ concatMap decl2Bind new
        let sigs = concatMap decl2Sig new
        let sortKey = captureOrder new
        return (HsValBinds an (ValBinds sortKey decs sigs))
replaceDeclsValbinds _ (HsIPBinds {}) _new    = error "undefined replaceDecls HsIPBinds"
replaceDeclsValbinds w (EmptyLocalBinds _) new
    = do
        logTr "replaceDecls HsLocalBinds"
        an <- newWhereAnnotation w
        let newBinds = concatMap decl2Bind new
            newSigs  = concatMap decl2Sig  new
        let decs = listToBag $ newBinds
        let sigs = newSigs
        let sortKey = captureOrder new
        return (HsValBinds an (ValBinds sortKey decs sigs))
replaceDeclsValbinds _ (XHsLocalBindsLR _) _ = error "replaceDeclsValbinds. XHsLocalBindsLR"

oldWhereAnnotation :: (Monad m)
  => ApiAnn' AnnList -> WithWhere -> RealSrcSpan -> TransformT m (ApiAnn' AnnList)
oldWhereAnnotation ApiAnnNotUsed ww _oldSpan = do
  newSpan <- uniqueSrcSpanT
  let w = case ww of
        WithWhere -> [AddApiAnn AnnWhere (AD (DP (0,0)))]
        WithoutWhere -> []
  let anc2' = Anchor (rs newSpan) (MovedAnchor (DP (0,1)))
  (anc, anc2) <- do
          newSpan <- uniqueSrcSpanT
          return ( Anchor (rs newSpan) (MovedAnchor (DP (1,2)))
                 , anc2')
  let an = ApiAnn anc
                  (AnnList (Just anc2) Nothing Nothing w [])
                  noCom
  return an
oldWhereAnnotation (ApiAnn anc an cs) ww oldSpan = do
  -- TODO: when we set DP (0,0) for the HsValBinds ApiAnnAnchor, change the AnnList anchor to have the correct DP too
  let (AnnList ancl o c r t) = an
  let w = case ww of
        WithWhere -> [AddApiAnn AnnWhere (AD (DP (0,0)))]
        WithoutWhere -> []
  -- let anc2 = Anchor (anchor anc) (MovedAnchor (DP (0,1)))
  let anc2 = anc
  -- TODO: updated ancl to have MovedAnchor based on oldSpan
  -- let dp = ss2delta (ss2pos $ anchor $ getLoc lc) oldSpan
  -- let ancl' = case ancl of
  --       Nothing -> anc
  --       Just aa@(Anchor a (MovedAnchor _)) -> aa
  --       Just (Anchor a _) -> Anchor a (MovedAnchor dp)
  (anc', ancl') <- do
        case ww of
          WithWhere -> return (anc, ancl)
          WithoutWhere -> return (anc, ancl)
          -- WithoutWhere -> return ( Anchor (anchor anc)  (MovedAnchor (DP (0,0)))
          --                        , ancl'')
          --   where
          --     ancl'' = case ancl of
          --       Nothing -> Nothing
          --       Just a -> Just (Anchor (anchor a) (MovedAnchor (DP (0,0))))
  let an = ApiAnn anc'
                  (AnnList ancl' o c w t)
                  cs
  return an

newWhereAnnotation :: (Monad m) => WithWhere -> TransformT m (ApiAnn' AnnList)
newWhereAnnotation ww = do
  newSpan <- uniqueSrcSpanT
  let anc  = Anchor (rs newSpan) (MovedAnchor (DP (1,2)))
  let anc2 = Anchor (rs newSpan) (MovedAnchor (DP (1,4)))
  let w = case ww of
        WithWhere -> [AddApiAnn AnnWhere (AD (DP (0,0)))]
        WithoutWhere -> []
  let an = ApiAnn anc
                  (AnnList (Just anc2) Nothing Nothing w [])
                  noCom
  return an

-- ---------------------------------------------------------------------

type Decl  = LHsDecl GhcPs
type PMatch = LMatch GhcPs (LHsExpr GhcPs)

-- |Modify a 'LHsBind' wrapped in a 'ValD'. For a 'PatBind' the
-- declarations are extracted and returned after modification. For a
-- 'FunBind' the supplied 'SrcSpan' is used to identify the specific
-- 'Match' to be transformed, for when there are multiple of them.
modifyValD :: forall m t. (HasTransform m)
                => SrcSpan
                -> Decl
                -> (PMatch -> [Decl] -> m ([Decl], Maybe t))
                -> m (Decl,Maybe t)
modifyValD p pb@(L ss (ValD _ (PatBind {} ))) f =
  if (locA ss) == p
     then do
       ds <- liftT $ hsDeclsPatBindD pb
       (ds',r) <- f (error "modifyValD.PatBind should not touch Match") ds
       pb' <- liftT $ replaceDeclsPatBindD pb ds'
       return (pb',r)
     else return (pb,Nothing)
modifyValD p ast f = do
  (ast',r) <- runStateT (everywhereM (mkM doModLocal) ast) Nothing
  return (ast',r)
  where
    doModLocal :: PMatch -> StateT (Maybe t) m PMatch
    doModLocal  (match@(L ss _) :: PMatch) = do
         if (locA ss) == p
           then do
             ds <- lift $ liftT $ hsDecls match
             (ds',r) <- lift $ f match ds
             put r
             match' <- lift $ liftT $ replaceDecls match ds'
             return match'
           else return match

-- ---------------------------------------------------------------------

-- |Used to integrate a @Transform@ into other Monad stacks
class (Monad m) => (HasTransform m) where
  liftT :: Transform a -> m a

instance Monad m => HasTransform (TransformT m) where
  liftT = hoistTransform (return . runIdentity)

-- ---------------------------------------------------------------------

-- | Apply a transformation to the decls contained in @t@
modifyDeclsT :: (HasDecls t,HasTransform m)
             => ([LHsDecl GhcPs] -> m [LHsDecl GhcPs])
             -> t -> m t
modifyDeclsT action t = do
  decls <- liftT $ hsDecls t
  decls' <- action decls
  liftT $ replaceDecls t decls'

-- ---------------------------------------------------------------------

matchApiAnn :: AnnKeywordId -> (KeywordId,DeltaPos) -> Bool
matchApiAnn mkw (kw,_)
  = case kw of
     (G akw) -> mkw == akw
     _       -> False


-- We comments extracted from annPriorComments or annFollowingComments, which
-- need to move to just before the item identified by the predicate, if it
-- fires, else at the end of the annotations.
insertCommentBefore :: (Monad m) => AnnKey -> [(Comment, DeltaPos)]
                    -> ((KeywordId, DeltaPos) -> Bool) -> TransformT m ()
insertCommentBefore key toMove p = do
  -- let
  --   doInsert ans =
  --     case Map.lookup key ans of
  --       Nothing -> error $ "insertCommentBefore:no AnnKey for:" ++ showGhc key
  --       Just ann -> Map.insert key ann' ans
  --         where
  --           (before,after) = break p (annsDP ann)
  --           ann' = ann { annsDP = before ++ (map comment2dp toMove) ++ after}

  -- modifyAnnsT doInsert
  return ()

-- ---------------------------------------------------------------------
