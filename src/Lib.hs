{-# LANGUAGE DeriveFoldable    #-}
{-# LANGUAGE DeriveFunctor     #-}
{-# LANGUAGE DeriveTraversable #-}
{-# LANGUAGE LambdaCase        #-}
{-# LANGUAGE NamedFieldPuns    #-}
{-# LANGUAGE TemplateHaskell   #-}
{-# LANGUAGE TypeFamilies      #-}
module Lib where

import           Data.List                (intercalate)

-- Recursion schemes

import           Data.Functor.Foldable.TH

type Algebra f a = f a -> a

-- Types

type Gamma = [(Char,Type)]
type Typer = Gamma -> Type -- Une fonction qui résoud un type à partir d'un contexte


data Type = TFun Type Type
    | TInt
    | TProd Type Type
    deriving Eq

instance Show Type where
  show TInt = "N"
  show (TProd a b) = show a ++ "×" ++ show b
  show (TFun a b) = show a ++ "->" ++ show b

(~>) a b = TFun a b
(×) a b = TProd a b
infixr 6 ~>
infixr 7 ×

makeBaseFunctor ''Type

-- Lambda-termes

data Term = Lambda Type Char Term
    | Var Char
    | Add Term Term
    | Lit Int
    | App Term Term
    | Pair Term Term
    | PLeft Term
    | PRight Term
    deriving (Eq, Show)

makeBaseFunctor ''Term

-- Language des catégories

data Cat = Id Type
    | Compose Cat Cat
    | PiLeft Type
    | PiRight Type
    | Fork Cat Cat
    | AddC
    | CstC Int Type
    | AppC Type
    | Curry Cat
    | UnCurry Cat
    deriving (Show, Eq)

makeBaseFunctor ''Cat

-- Assembleur

type Register = (Int,Type)
data Arrow = Arrow
    { domain    :: Register
    , procedure :: [IST]
    , codomain  :: Register
    }

instance Show Arrow where
  show Arrow{domain,procedure,codomain} =
    "domain:" ++ showReg domain
    ++ "\n" ++ intercalate "\n" (map show procedure)
    ++ "\ncodomain:" ++ showReg codomain

identity :: Register -> Arrow
identity r = Arrow r [NoOp] r

instance Semigroup Arrow where
  Arrow d1 p1 c1 <> Arrow d2 p2 c2 = Arrow d1 (p1 ++ [Move c1 d2] ++ p2) c2

data Val = Constant Int
    | Couple Val Val
    | RegContent Register
    | Parameter
    deriving Show

type LLVMFun = Int
type Declaration = (LLVMFun,[IST])
type FormFun = (LLVMFun,Int)

data IST = Assign Register Val
    | Move Register Register
    | Return Val
    | CallInto Val Val Register
    | AllocInto Register Type Register
    | DerefPointerInto Register Register
    | ProjLeftInto Register Register
    | ProjRightInto Register Register
    | AddInto Register Register Register
    | NoOp

showReg :: Register -> String
showReg (n,t) = "reg" ++ show n ++ ":" ++ show t

instance Show IST where
  show = \case
    Assign reg val -> showReg reg ++ " <- " ++ show val
    Move reg1 reg2 -> showReg reg1 ++ " -> " ++ showReg reg2
    Return val -> "return " ++ show val
    CallInto val1 val2 reg -> "call " ++ show val1
                ++ show " " ++ show val2 ++ " " ++ showReg reg
    AllocInto reg1 t reg2 -> "alloc " ++ showReg reg1
                ++ " " ++ show t ++ show " "++ showReg reg2
    DerefPointerInto reg1 reg2 -> "*" ++ showReg reg1 ++ " -> " ++ show reg2
    ProjLeftInto reg1 reg2 -> "pi_gauche(" ++ showReg reg1 ++ ") -> " ++ showReg reg2
    ProjRightInto reg1 reg2 -> "pi_droit(" ++ showReg reg1 ++ ") -> " ++ showReg reg2
    AddInto reg1 reg2 reg3 -> showReg reg1 ++ " + " ++ showReg reg2 ++ " -> " ++ showReg reg3
    NoOp -> "no_op"

type Program = (Arrow, [Declaration])

