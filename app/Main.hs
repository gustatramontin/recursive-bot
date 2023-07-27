{-# LANGUAGE OverloadedStrings #-}
module Main where

import           Prelude hiding (words)
import           Control.Monad (when, void)
import           Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.IO as TIO
import           Data.List hiding (isPrefixOf, intercalate, words)

import           UnliftIO.IORef
import           UnliftIO (liftIO)

import           Discord
import           Discord.Types
import qualified Discord.Requests as R
import           Discord.Internal.Types.Embed (CreateEmbed, EmbedField)

import           Parser
import           Evaluation
import           Token (token)
{-
 - Commands
 - deck add <word>
 - deck list
 - deck export
 -
 - define <word>
 - 
 -}
main :: IO ()
main = do
        err <- runDiscord $ def
          { discordToken = token
          , discordOnEvent = eventHandler
          , discordOnLog = \s -> TIO.putStrLn s >> TIO.putStrLn ""
          }

        TIO.putStrLn err

eventHandler :: Event -> DiscordHandler ()
eventHandler (MessageCreate m) = when (isCommand m && not (fromBot m)) $ do
   let code = '(' `T.cons` (T.tail (messageContent m)) `T.snoc` ')'
   res <- liftIO $ run $ parse code
   void $ restCall (R.CreateMessage (messageChannelId m) (T.pack res))

eventHandler _ = return ()

fromBot :: Message -> Bool
fromBot = userIsBot . messageAuthor

isCommand :: Message -> Bool
isCommand = ("ð" `T.isPrefixOf`) . messageContent
