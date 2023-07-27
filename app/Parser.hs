{-# LANGUAGE OverloadedStrings #-}
module Parser (Atom(..), Env, parseForm, parse) where

import qualified Data.Text as T
import           Data.Text (Text)
import qualified Data.Map as Map
import qualified Text.Parsec as P
import           Control.Applicative
import           UnliftIO.IORef

type Env = IORef (Map.Map Text Atom)

data Atom = Nil
          | Symbol Text
          | Pair [Atom]
          | PrimitiveFunction ([Atom] -> Atom)
          | Function { params :: [Text], body :: Atom, env :: Env  }
          | Number Integer
          | Boolean Bool
          | Str Text

instance Show Atom where
  show (PrimitiveFunction f) = "<function>"
  show (Function p b e) = "<function>"

  show (Symbol t) = show t
  show (Number n) = show n
  show (Boolean b) = show b
  show (Str s) = show "\"" ++ show s ++ "\""
  show (Pair p) = "(" ++ (T.unpack (T.intercalate " " (map (T.pack . show) p))) ++ ")"

  show Nil = show "Nil"
parseNil :: P.Parsec Text () Atom
parseNil = do
  P.string "#nil"

  return Nil
parseSymbol :: P.Parsec Text () Atom
parseSymbol = do
  symbol <- P.many1 (P.noneOf "'#() ")
  return $ Symbol (T.pack symbol)

parseBoolean :: P.Parsec Text () Atom
parseBoolean = do
  bool <- P.try (P.string "#t") <|> P.string "#f"
  return $ case bool of
    "#t" -> Boolean True
    "#f" -> Boolean False

parseNumber :: P.Parsec Text () Atom
parseNumber = do
  number <- (P.many1 P.digit)
  return $ Number (read number :: Integer)

parseString :: P.Parsec Text () Atom
parseString = do
  P.char '"'
  string <- P.many $ P.noneOf "\""
  P.char '"'
  return $ Str (T.pack string)

parseAtom :: P.Parsec Text () Atom
parseAtom = do
  atom <- P.try parseNil <|> parseBoolean <|> parseNumber <|> parseString <|> parseSymbol
  return atom

parseForm :: P.Parsec Text () Atom
parseForm = do
  P.char '('
  P.spaces

  formName <- parseAtom
  P.spaces
  args <- many $ (parseForm <|> parseAtom) >>= (\x -> P.spaces >> return x)

  P.spaces
  P.char ')'

  return $ Pair $ [formName] ++ args

preParse :: Text -> Either P.ParseError Atom
preParse s = P.parse parseForm "(source)" $ s
parse :: Text -> Atom
parse s = case preParse s of
  Right p -> p
  Left e -> Nil

{-
prefix add (rank 1)
Nil
#nil
list
(list x1 x2 x3)
(pair x y)
(fst list)
(snd list)

form
(function args ...)
symbol
<name> -> <value>

label
(def <symbol> <value>)

function

(fn (x y z ...) <form>) -> function
((fn (x) (* x 2)) 2)
predicates
#t, #f -> bool
(= x y) -> bool
(not x) -> bool
(number? x) -> bool


control flow
(if <predicate> x y)

time
(wait sec <form>) ->

moderation
(ban user)
(unban user)
(kick user)
(mute user)
(unmute user)
(add-role user role)
(rm-role user rol)

discord
  channel
  (get-channel-messages channel-id)
  (get-channel-message message-id)
  (get-message-reactions channel-id message-id)

  user
(get-user id)
  guild
(get-guild-channels guild-id)
(create-guild-channel guild-id str ...)
(list-guild-members guild-id)
(add-user-role guild-id user-id role-id)
(remove-user-role guild-id user-id role-id)
(get-guild-bans guild-id)
(create-ban guild-id user-id ...)
(remove-ban guild-id user-id)
(get-guild-roles guild-id)
(create-guild-role guild-id opts...)


-}
