{-# LANGUAGE LambdaCase #-}
module Parsing where

import           Control.Applicative
import           Control.Monad
import           Data.Bifunctor

-- Un parseur monadique classique
-- cf. "Parsec: Direct Style Monadic Parser Combinators For The Real World"


newtype Parser a = Parser { parse :: String -> Either String (a,String) }

instance Functor Parser where
  fmap f p = Parser $ second (first f) . parse p

instance Applicative Parser where
  pure x = Parser $ \s -> Right (x,s)
  a <*> b = Parser $ \s ->
    case parse a s of
      Right (f,rem) -> parse (f <$> b) rem
      Left err      -> Left err

instance Monad Parser where
  a >>= f = Parser $ \s ->
    case parse a s of
      Right (x,rem) -> parse (f x) rem
      Left err      -> Left err

instance Alternative Parser where
  empty = Parser . const $ Left "Erreur"
  a <|> b = Parser $ \s ->
    case parse a s of
      Left err -> parse b s
      x        -> x

instance MonadFail Parser where
  fail err = Parser $ \s -> Left err

instance MonadPlus Parser

prefix :: String -> Parser String
prefix pattern = Parser $
  \s -> case splitAt (length pattern) s of
          ([],[])              -> Left "Fin de chaine atteinte"
          (a,r) | a == pattern-> Right (a,r)
          _                    -> Left $ "Erreur, il était attendu : " ++ pattern

char :: Char -> Parser Char
char c = head <$> prefix [c]

anyOf :: [Parser a] -> Parser a
anyOf (x:xs) = x <|> anyOf xs
anyOf []     = empty

anyChar :: Parser Char
anyChar = Parser $ \case
  [] -> Left "Aucun charactère restant"
  x:xs -> Right (x,xs)

option :: Parser a -> Parser (Maybe a)
option p = (Just <$> p) <|> (pure Nothing)

charOf :: String -> Parser Char
charOf l = (anyOf . (map char) $ l) <*< (l ++ " étaients attendus")
digit = (charOf $ map (head.show) [0..9]) <*< "Un chiffre était attendu"
alpha = (charOf $ ['a'..'z'] ++ ['A'..'Z']) <*< "Une lettre était attendue"
alphaNum = (alpha <|> digit) <*< "Une lettre ou un chiffre était attendu"

many1 :: Parser a -> Parser [a]
many1 p = Parser $ \s ->
  case parse (many p) s of
    Right ([],_) -> Left "Élément non trouvé"
    x            -> x


natural :: Parser Int
natural = (read <$> many1 digit) <*< "Un entier naturel était attendu"

paren :: Parser a -> Parser a
paren p = do { char '(' ; r <- p ; char ')' ; return r }

identifier = many1 (digit <|> (charOf $ ['a'..'z']))

(<*<) :: Parser a -> String -> Parser a
(<*<) p err = Parser $ \s -> first (const err) (parse p s)
infixr 9 <*<

-- TODO: vérif + sourcer

chainr1 p op = scan
  where scan = do { x <- p ; rest x }
        rest x = do { f <- op ;
                      y <- scan ;
                      ; return (f x y) } <|> return x

chainl1 p op = do { x <- p ; rest x }
  where rest x = do {
                  f <- op ;
                  y <- p ;
                  rest (f x y) } <|> return x


