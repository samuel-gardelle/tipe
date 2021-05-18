{-# LANGUAGE LambdaCase #-}
module Parser where

import           Control.Applicative ((<|>))
import           Lambda
import           Lib
import           Parsing
import           Prelude             hiding (lambda, not)


{-
 - TODO: trouver une syntaxe pour l'abstraction de type
 - TODO: trouver une syntaxe pour l'application de type
 -}


expr :: Parser Term
expr = Parser $ \case
    [] -> Left "Fin de chaine atteinte"
    x -> parse work x
  where work = do
          (chainl1 (unary <|> nullary) parseOp) <|> paren expr


parseType :: Parser Type
parseType = (chainr1 simpleKinds (char '>' >> return TFun))
  where simpleKinds = (char 'I' >> return TInt)
                      <|> (do { char '(' ; a <- parseType
                                ; char ',' ; b <- parseType
                                ; char ')' ; return $ a × b})
                      <|> paren parseType


-- Constructions nullaires

nullary :: Parser Term
nullary = nmbr <|> var

nmbr = Lit <$> natural
var = Var <$> alpha

-- Constructions unaires

unary :: Parser Term
unary = lambda <|> pleft <|> pright <|> paren expr

pleft = prefix "<" >> PLeft <$> expr
pright = prefix ">" >> PRight <$> expr
lambda = paren $ do
  char '\\'
  x <- alpha
  char ':'
  t <- parseType
  char '.'
  e <- expr
  return $ Lambda t x e

-- Constructions binaires

operations = [
    (Add,'+'),(App,' '),(Pair,',')
  ]

parseOp = anyOf $
  map (\(c,symb) -> char symb >> return c) operations

