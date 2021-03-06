{-# LANGUAGE MagicHash, UnboxedSums, NoImplicitPrelude #-}
{-# LANGUAGE TypeFamilies #-}
-- {-# LANGUAGE PolyKinds #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE TypeInType #-}
{-# LANGUAGE GADTs ,ExplicitNamespaces#-}
{-# LANGUAGE UnboxedTuples #-}

module T14111 where

import GHC.Exts
import GHC.Types
import Prelude (undefined)
import Data.Kind
import Data.Void

data family Maybe(x :: TYPE (r :: RuntimeRep))

data instance Maybe (a :: Type ) where
  MaybeSum :: (# a  | (# #) #) -> Maybe a

data instance Maybe (x :: TYPE 'UnliftedRep) where
  MaybeSumU :: (# x | (# #) #) -> Maybe x
