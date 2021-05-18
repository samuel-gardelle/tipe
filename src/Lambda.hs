{-# LANGUAGE LambdaCase #-}
module Lambda (substituteTerm, resolve, resolveWithCtx) where

import           Data.Functor.Foldable
import           Data.List             (intersperse)
import           Lib

{- Definitions #-}

-- Substitution des termes

substituteTerm :: Term -> Char -> Term -> Term
substituteTerm t x = cata (subTerm t x)

subTerm :: Term -> Char -> Algebra TermF Term
subTerm e x = \case
  VarF x' | x == x' -> e
  LambdaF t a b | x == a -> error "Substitution de terme ambigue"
  t -> embed t

-- Typage des termes

resolve :: Term -> Type
resolve t = (cata typer t) []

resolveWithCtx :: Term -> Gamma -> Type
resolveWithCtx = cata typer

-- TODO ? réécrire en lambda-case

typer :: Algebra TermF Typer

typer (VarF x) = \l -> case lookup x l of
    Just t -> t
    _      -> error $ "La variable " ++ show x ++ " est libre."
                      ++ "\nPeut être confondez-vous avec les variables suivantes : "
                      ++ (intersperse ',' (map fst l))

typer (AddF a b) = \l -> case (a l,b l) of
                  (TInt, TInt) -> TInt
                  _ -> error "L'addition attend des opérandes entières"

typer (LitF k) = const TInt

typer (LambdaF t x y) = \l -> t ~> y ((x,t):l)

typer (AppF f x) = \l ->
  let tf = f l
      tx = x l in
  case tf of
    TFun a b | tx == a -> b
    _ -> error $ "Application illégale de " ++ show tf ++ " à  " ++ show tx

typer (PairF a b) = \l -> (a l) × (b l)

typer (PLeftF p) = \l ->
  case p l of
    TProd a _ -> a
    _         -> error "La projection gauche attend un type produit"

typer (PRightF p) = \l ->
  case p l of
    TProd _ b -> b
    _         -> error "La projection droite attend un type produit"

