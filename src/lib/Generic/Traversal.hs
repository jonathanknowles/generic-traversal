{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE ScopedTypeVariables       #-}

module Generic.Traversal
  ( Action (..)
  , Transformation (..)
  , genericMap
  , genericMapWith
  , genericTraverse
  , genericTraverseWith
  ) where

import Data.Data
import Data.Maybe

data Transformation = forall t . Typeable t => Transformation (t ->   t)
data Action m       = forall t . Typeable t => Action         (t -> m t)

genericMap :: Data d => [Transformation] -> d -> d
genericMap transformations = visitItem
  where
    visitItem :: Data e => e -> e
    visitItem d = loop transformations
      where
        loop (t : ts) = fromMaybe (loop ts) (applyTransformation t d)
        loop []       = visitChildren d

    visitChildren :: Data e => e -> e
    visitChildren = gmapT visitItem

genericTraverse
  :: forall m d . Monad m => Typeable m => Data d
  => [Action m] -> d -> m d
genericTraverse actions = visitItem
  where
    visitItem :: forall e . Data e => e -> m e
    visitItem d = loop actions
      where
        loop (t : ts) = fromMaybe (loop ts) (applyAction t d)
        loop []       = visitChildren d

    visitChildren :: forall e . Data e => e -> m e
    visitChildren = gmapM visitItem

genericMapWith :: Data d => d -> [Transformation] -> d
genericMapWith = flip genericMap

genericTraverseWith
  :: forall m d . Monad m => Typeable m => Data d
  => d -> [Action m] -> m d
genericTraverseWith = flip genericTraverse

applyTransformation
  :: Typeable d
  => Transformation -> d -> Maybe d
applyTransformation (Transformation f) d = cast . f =<< cast d

applyAction
  :: Typeable m => Typeable d
  => Action m -> d -> Maybe (m d)
applyAction (Action f) d = cast . f =<< cast d

