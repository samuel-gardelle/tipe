module Main where

import Lib
import Lambda
import Parsing
import Parser
import Cat
import CodeGen

import Control.Monad (forM)

-- TODO : de meilleurs messages d'erreur

main :: IO ()
main = do
  program <- getLine
  putStrLn ""

  let parsed = case parse expr program of
                Right (parsed,_) -> parsed
                Left e -> error e

  print parsed
  putStrLn "Parsing : OK.\n"

  putStrLn "Vérification du typage..."
  let ptype = resolve parsed
  print ptype
  putStrLn "Vérification du typage : OK.\n"

  putStrLn "Conversion..."
  let cat = convert parsed
  print cat
  putStrLn "Conversion : OK.\n"

  putStrLn "Génération du code..."
  let (arr,decls) = codegen cat

  putStrLn $ show arr
  putStrLn ""

  forM decls $ \d -> do
      putStrLn ""
      putStrLn $ show d

  putStrLn "Génération du code : OK.\n"

