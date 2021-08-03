{-# LANGUAGE FlexibleInstances #-}

module CoRulesAlgebra where

import Control.Comonad

data Lens a b = Lens
  { get :: a -> b,
    set :: a -> b -> a
  }

data Rule s a = Rule {fn :: s -> s -> a, current :: s, prev :: s}

instance Functor (Rule s) where
  fmap g (Rule f s s') = Rule (\s s' -> g (f s s')) s s'

instance Comonad (Rule s) where
  extract (Rule f s s') = f s s'
  duplicate (Rule f s s') = Rule (Rule f) s s'

