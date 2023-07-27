{-# LANGUAGE OverloadedStrings #-}
module Evaluation (evaluate, run) where

import           Parser
import qualified Data.Map as Map
import           Data.Text (Text)
import qualified Data.Text as T
import           Discord
import           Discord.Types
import qualified Discord.Requests as R
import           UnliftIO.IORef

type Predicate = Atom -> Bool
booleanp :: Predicate
booleanp (Boolean _) = True
booleanp _ = False

symbolp :: Predicate
symbolp (Symbol _) = True
symbolp _ = False

numberp :: Predicate
numberp (Number _) = True
numberp _ = False

strp :: Predicate
strp (Str _) = True
strp _ = False

functionp :: Predicate
functionp (Function _ _ _) = True
functionp (PrimitiveFunction _) = True
functionp _ = False

pairp :: Predicate
pairp (Pair _) = True
pairp _ = False

nilp :: Predicate
nilp Nil = True
nilp _ = False


numericBinOp :: (Integer -> Integer -> Integer) -> [Atom] -> Atom
numericBinOp op params = Number $ foldl1 op $ map unPackNum params

predicateNumBinOp :: (Integer -> Integer -> Bool) -> [Atom] -> Atom
predicateNumBinOp op params =  Boolean $ (\xs -> op (xs !! 0) (xs !! 1)) $ take 2 $ map unPackNum params

predicateBoolBinOp :: (Bool -> Bool -> Bool) -> [Atom] -> Atom
predicateBoolBinOp op params = Boolean . foldl1 op $ map unPackBool params

unPackNum (Number n) = n
unPackNum (Pair [(Number n)]) = n

unPackBool (Boolean b) = b
unPackSymbol (Symbol s) = s
unPackPair (Pair s) = s

listFunctions = Map.fromList [
              ("fst", PrimitiveFunction $ head . unPackPair . head)
            , ("snd", PrimitiveFunction $ Pair . tail . unPackPair . head)
            , ("pair", PrimitiveFunction $ Pair . (\xs -> (head xs):(tail xs)) . unPackPair . head)
            , ("len", PrimitiveFunction $ Number . toInteger . length . unPackPair . head)
                             ]

numberFunctions = Map.fromList [
                 ("+", PrimitiveFunction $ numericBinOp (+))
                , ("-", PrimitiveFunction $ numericBinOp (-))
                , ("*", PrimitiveFunction $ numericBinOp (*))
                , ("/", PrimitiveFunction $ numericBinOp div)
                , ("abs", PrimitiveFunction $ Number . abs . unPackNum . head)
                , ("expt", PrimitiveFunction $ numericBinOp (^))
                --, ("modulo", numericBinOp . rem)
                ]
predicateFunctions :: Map.Map Text Atom
predicateFunctions = Map.fromList [
        ("Nil?", PrimitiveFunction $ Boolean . nilp . head)
        , ("Number?", PrimitiveFunction $ Boolean . numberp . head)
        , ("Boolean?", PrimitiveFunction $ Boolean . booleanp . head)
        , ("Str?", PrimitiveFunction $ Boolean . strp . head)
        , ("Symbol?", PrimitiveFunction $ Boolean . symbolp . head)
        , ("Function?", PrimitiveFunction $ Boolean . functionp . head)
        , ("=", PrimitiveFunction $ predicateNumBinOp (==))
        , (">", PrimitiveFunction $ predicateNumBinOp (>))
        , ("<", PrimitiveFunction $ predicateNumBinOp (<))
        , (">=", PrimitiveFunction $ predicateNumBinOp (>=))
        , ("<=", PrimitiveFunction $ predicateNumBinOp (<=))
        , ("and", PrimitiveFunction $ predicateBoolBinOp (&&))
        , ("or", PrimitiveFunction $ predicateBoolBinOp (||))
                                  ]

baseFunctions = Map.unions [
                numberFunctions
              , predicateFunctions
              , listFunctions
                     ]

apply :: Atom -> [Atom] -> IO Atom
apply (PrimitiveFunction f) params = return $ f params
apply (Function params body env) args = if (length params) /= (length args)
  then return Nil
  else do
        newEnv <- bindVars env (zip params args)
        evaluate newEnv body

isBound :: Env -> Text -> IO Bool
isBound env var = do
  table <- readIORef env
  case (Map.lookup var table) of
        (Just _) -> return True
        Nothing -> return False

defineVariable :: Env -> Text -> Atom -> IO ()
defineVariable env name val = do
 envTable <- readIORef env
 let newEnv = Map.insert name val envTable

 writeIORef env newEnv

bindVars :: Env -> [(Text, Atom)] -> IO (Env)
bindVars env vars = do
  table <- readIORef env
  newEnv <- newIORef table
  mapM (\v -> defineVariable newEnv (fst v) (snd v)) vars
  return newEnv
setVariable :: Env -> Text -> Atom -> IO ()
setVariable env name val = return ()

readVariable :: Env -> Text -> IO (Atom)
readVariable env name = do
  envTable <- readIORef env
  bound <- isBound env name
  if bound then
    return $ envTable Map.! name
    else return Nil

evaluate :: Env -> Atom -> IO (Atom)
evaluate env (Symbol s) = do
  val <- readVariable env s
  return val

evaluate env val@(Number _) = return val
evaluate env val@(Str _) = return val
evaluate env val@(Boolean _) = return val
evaluate env Nil = return Nil

evaluate env (Pair ((Symbol "do"):args)) = do
         res <- mapM (evaluate env) args
         return $ last res

evaluate env (Pair [Symbol "quote", val]) = return val
evaluate env (Pair [Symbol "if", pred, conseq, alt]) = do
  result <- evaluate env pred
  case result of
    Boolean True -> evaluate env conseq
    Boolean False -> evaluate env alt
evaluate env (Pair [Symbol "def", Symbol name, val]) = evaluate env val >>= defineVariable env name >> return Nil
evaluate env (Pair [Symbol "fn", Pair params, body]) = return $ Function {params = (map unPackSymbol params), body = body, env=env}

evaluate env (Pair ((Symbol s):args)) = do
         fnName <- evaluate env (Symbol s)
         params <- mapM (evaluate env) args
         evaluate env (Pair (fnName:params))

evaluate env (Pair ((PrimitiveFunction func):args)) = apply (PrimitiveFunction func) args

evaluate env (Pair ((Function p b e):args)) = apply func args
         where func = (Function p b e)

evaluate env val@(Pair _) = return val

run exp = do
    env <- newIORef baseFunctions
    res <- evaluate env exp
    return $ show res
