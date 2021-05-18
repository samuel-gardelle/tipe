{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE LambdaCase       #-}
{-# LANGUAGE RecordWildCards  #-}
module CodeGen where

import           Data.Functor.Foldable

import           Control.Monad.State
import           Control.Monad.Writer
import           Control.Monad.State.Lazy
import           Control.Monad.Writer.Lazy

import           Cat
import           Lib

codegen :: Cat -> Program
codegen c =
  let g = gen c in
  let g' = evalStateT g (0,0) in
  let (arr,w) = runWriter g' in
  (arr,w)

type MyState = (Int,Int)
-- (i,j) | i = dernier registre, j = dernier nom de fonction

-- Un DSL pour la génération de code

createRegister :: MonadState MyState m => Type -> m Register
createRegister t = do
    modify (\(i,j) -> (i+1,j))
    i <- fst <$> get
    return $ (i,t)

createRegPair :: MonadState MyState m => Type -> Type -> m (Register,Register)
createRegPair tau mu = (,) <$> createRegister tau <*> createRegister mu

declareLLVMFun :: (MonadWriter [Declaration] m, MonadState MyState m) =>
                  [IST] -> m LLVMFun
declareLLVMFun proc = do
    modify (\(i,j) -> (i,j+1))
    j <- snd <$> get
    tell [(j,proc)]
    return j

createArrow :: MonadState MyState m =>
               Type -> Type -> (Register -> Register -> m [IST]) -> m Arrow
createArrow domType codomType builder = do
    (dom, codom) <- createRegPair domType codomType
    procedure <- builder dom codom
    return $ Arrow dom procedure codom

registerType :: Register -> Type
registerType = snd

-- Génération du code

gen :: (MonadWriter [Declaration] m, MonadState MyState m) => Cat -> m Arrow
gen = cata genM

genM :: (MonadWriter [Declaration] m, MonadState MyState m) => Algebra CatF (m Arrow)
genM = \case

  IdF t -> identity <$> createRegister t
  ComposeF g f -> (<>) <$> f <*> g

  PiLeftF (TProd a b) ->
      createArrow (TProd a b) a $ \dom codom ->
          return [ProjLeftInto dom codom]
  PiRightF (TProd a b) ->
      createArrow (TProd a b) b $ \dom codom ->
          return [ProjRightInto dom codom]

  ForkF f g -> do

    Arrow domF procF codomF <- f
    Arrow domG procG codomG <- g

    let a = registerType domF
        b = registerType codomF
        c = registerType codomG

    createArrow a (b × c) $ \dom codom ->
        return $ [Move dom domF, Move dom domG]
                 ++ procF ++ procG
                 ++ [Assign codom (Couple (RegContent codomF) (RegContent codomG))]

  AddCF -> createArrow (TInt × TInt) TInt $ \dom codom -> do
              (regA, regB) <- createRegPair TInt TInt
              return $
                    [ProjLeftInto dom regA,
                    ProjLeftInto dom regB, AddInto regA regB codom]

  CstCF k t -> createArrow t TInt $ \dom codom -> return [Assign codom (Constant k)]

  AppCF t@(TProd (TFun _ b) a) ->
    createArrow t b $ \dom codom -> do
      formFun <- createRegister (TInt × TInt)
      (fnAddr, ptrAddr) <- createRegPair TInt TInt
      param <- createRegister a

      return [ ProjLeftInto dom formFun,
               ProjRightInto dom param,

               ProjLeftInto formFun fnAddr,
               ProjRightInto formFun ptrAddr,

               CallInto (RegContent fnAddr) (Couple (RegContent param)
                    (RegContent ptrAddr)) codom
             ]

  CurryF c -> do
    Arrow dom proc codom <- c

    let (TProd a b) = registerType dom
        c = registerType codom

    (ra,rb) <- createRegPair a b -- TODO
    ptr <- createRegister TInt
    param <- createRegister (a × TInt)

    llvmfun <- declareLLVMFun $ [

          Assign param Parameter,
          ProjLeftInto param ra,
          ProjRightInto param ptr,

          DerefPointerInto ptr rb,
          Assign dom (Couple (RegContent ra) (RegContent rb))
        ] ++ proc ++ [ Return (RegContent codom) ]

    createArrow a (b ~> c) $ \dom codom -> do
          param <- createRegister a
          ptr <- createRegister TInt
          ret <- createRegister (TInt × TInt)
          return $ [
              Assign param Parameter,
              AllocInto param a ptr,
              Assign ret (Couple (Constant llvmfun) (RegContent ptr))
              ]

  UnCurryF c -> do

    Arrow dom proc codom <- c
    let a = registerType dom
        (TFun b c) = registerType codom

    Arrow dom' proc' codom' <- gen $ AppC ((b ~> c) × b)
    param <- createRegister (a × b)
    rb <- createRegister b

    createArrow (a × b) c $ \dom codom -> return $ [
        Assign param Parameter,
        ProjLeftInto param dom,
        ProjRightInto param rb
      ] ++ proc ++
      [ Assign dom' (Couple (RegContent codom) (RegContent rb)),
        Return (RegContent codom)
      ]

