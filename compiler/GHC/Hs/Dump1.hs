{-
(c) The University of Glasgow 2006
(c) The GRASP/AQUA Project, Glasgow University, 1992-1998
-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}

-- | Contains a debug function to dump parts of the GHC.Hs AST. It uses a syb
-- traversal which falls back to displaying based on the constructor name, so
-- can be used to dump anything having a @Data.Data@ instance.

module GHC.Hs.Dump1 (
        -- * Dumping ASTs
        showAstData,
        BlankApiAnnotations(..),
    ) where

import GHC.Prelude (Maybe(..), maybe)

import GHC.Hs (SrcSpanAnn', ApiAnn', AnnListItem, SrcSpanAnn'(SrcSpanAnn))

import GHC.Utils.Outputable (SDoc, vcat, text )

import Data.Data (Data,Typeable, gmapQ, cast)

data BlankApiAnnotations = NoBlankApiAnnotations

-- | Show a GHC syntax tree. This parameterised because it is also used for
-- comparing ASTs in ppr roundtripping tests, where the SrcSpan's are blanked
-- out, to avoid comparing locations, only structure
showAstData :: Data a => BlankApiAnnotations -> a -> SDoc
showAstData ba a0 = showAstData' a0
  where
    showAstData' :: Data a => a -> SDoc
    showAstData' =
      generic
              `extQ` srcSpanAnnA

      where generic :: Data a => a -> SDoc
            generic t = vcat (gmapQ showAstData' t)

            -- -------------------------

            srcSpanAnnA :: SrcSpanAnn' (ApiAnn' AnnListItem) -> SDoc
            srcSpanAnnA = locatedAnn''

            locatedAnn'' :: forall a. (Typeable a, Data a)
              => SrcSpanAnn' a -> SDoc
            locatedAnn'' ss =
              case cast ss of
                Just ((SrcSpanAnn ann _) :: SrcSpanAnn' a) ->
                  case ba of
                    NoBlankApiAnnotations
                      -> showAstData' ann
                Nothing -> text "locatedAnn:unmatched"



{-
************************************************************************
*                                                                      *
* Copied from syb
*                                                                      *
************************************************************************
-}


-- | Extend a generic query by a type-specific case
extQ :: ( Typeable a
        , Typeable b
        )
     => (a -> q)
     -> (b -> q)
     -> a
     -> q
extQ f g a = maybe (f a) g (cast a)

