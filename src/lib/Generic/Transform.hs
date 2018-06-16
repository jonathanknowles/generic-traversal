{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE ScopedTypeVariables       #-}

module Generic.Transform
  ( Transformer (..)
  , TransformerM (..)
  , transform
  , transformM
  , transformWith
  , transformWithM
  ) where

import Data.Data
import Data.Maybe

data Transformer    = forall t . Typeable t => Transformer  (t -> t)
data TransformerM m = forall t . Typeable t => TransformerM (t -> m t)

transform :: Data d => [Transformer] -> d -> d
transform transformers = transformItem
  where
    transformItem :: Data e => e -> e
    transformItem d = loop transformers
      where
        loop (t : ts) = fromMaybe (loop ts) (applyTransformer t d)
        loop []       = transformChildren d

    transformChildren :: Data e => e -> e
    transformChildren = gmapT transformItem

transformM
  :: forall m d . Monad m => Typeable m => Data d
  => [TransformerM m] -> d -> m d
transformM transformers = transformItem
  where

    transformItem :: forall e . Data e => e -> m e
    transformItem d = loop transformers
      where
        loop (t : ts) = fromMaybe (loop ts) (applyTransformerM t d)
        loop []       = transformChildren d

    transformChildren :: forall e . Data e => e -> m e
    transformChildren = gmapM transformItem

transformWith :: Data d => d -> [Transformer] -> d
transformWith = flip transform

transformWithM
  :: forall m d . Monad m => Typeable m => Data d
  => d -> [TransformerM m] -> m d
transformWithM = flip transformM

applyTransformer
  :: Typeable d
  => Transformer -> d -> Maybe d
applyTransformer (Transformer f) d = cast . f =<< cast d

applyTransformerM
  :: Typeable m => Typeable d
  => TransformerM m -> d -> Maybe (m d)
applyTransformerM (TransformerM f) d = cast . f =<< cast d

