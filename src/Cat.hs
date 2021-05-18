{-# LANGUAGE LambdaCase #-}
module Cat where

import           Data.Functor.Foldable
import           Lambda
import           Lib


-- Résolution des types

resolveC :: Cat -> Type
resolveC = cata resC

resC :: Algebra CatF Type
resC = \case
  IdF t -> t ~> t
  ComposeF (TFun tc td) (TFun ta tb ) | tb == tc -> ta ~> td

  AppCF (TProd (TFun x y) a) | x == a -> ((x ~> y) × x) ~> y
  CurryF (TFun (TProd a b) c) -> a ~> b ~> c
  UnCurryF (TFun a (TFun b c)) -> a × b ~> c

  ForkF (TFun tx ty) (TFun tx' ty') | tx == tx' -> tx ~> (ty × ty')
  PiLeftF (TProd a b) -> a × b ~> a
  PiRightF (TProd a b) -> a × b ~> b

  AddCF -> (TInt × TInt) ~> TInt
  CstCF _ t -> t ~> TInt

  _ -> error "Bug"

-- Convertion STLC -> CCC

convert :: Term -> Cat

convert (Lambda tx x e) = elimAbs tx x e
convert _ = error "Seules les abstractions peuvent être converties"

-- Elimination des abstractions

-- TODO ? utiliser un paramorphisme ?

elimAbs :: Type -> Char -> Term -> Cat

elimAbs tx x (Var v)
  | v == x = Id tx
  | otherwise = error $ "Variable non liée : " ++ show v

elimAbs tx x (App u v) =
    Compose (AppC tapp) (Fork u' v')
  where tapp = undefined
        u' = elimAbs tx x u
        v' = elimAbs tx x v

elimAbs tx x (Lambda ty y u) = Curry $ elimAbs (tx × ty) x u'
  where u' = (substituteTerm (Var x) '!') .
             (substituteTerm (PLeft $ Var '!') x) .
             (substituteTerm (PRight $ Var '!') y) $ u

elimAbs tx x (Add u v) =
    Compose AddC (Fork u' v')
  where tapp = undefined
        u' = elimAbs tx x u
        v' = elimAbs tx x v

elimAbs tx x (Lit n) = CstC n tx

elimAbs tx x (Pair u v) = Fork u' v'
  where u' = elimAbs tx x u
        v' = elimAbs tx x v

elimAbs tx x (PLeft u) = Compose (PiLeft tu) u'
  where u' = elimAbs tx x u
        tu = resolveWithCtx u [(x,tu)]

elimAbs tx x (PRight u) = Compose (PiRight tu) u'
  where u' = elimAbs tx x u
        tu = resolveWithCtx u [(x,tx)]


