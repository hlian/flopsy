{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Main where

import qualified Web.Scotty as Scotty
import qualified Data.Aeson as Aeson

import           BasePrelude hiding (lazy)
import           Control.Lens
import           Control.Monad.Trans (liftIO)
import           Data.Aeson ((.:))
import           Data.Text (Text)
import           Data.Text.Strict.Lens

data Event = MergeWhen Text Text deriving Show

instance Aeson.FromJSON Event where
  parseJSON = Aeson.withObject "MergeWhen" $ \o -> do
    (action :: Text) <- o .: "action"
    when (action /= "created") (fail ("bad action: " <> show action))
    comment <- o .: "comment" >>= (.: "body")
    commentsURL <- o .: "issue" >>= (.: "comments_url")
    case words body of
      [] ->
        fail "empty body"
      ("@flopsy-the-rabbit":"merge":"when":rest) ->
        pure $ MergeWhen ((view packed . unwords) rest) commentsURL

present :: Show a => a -> Text
present = view packed . show

raise :: Text -> Scotty.ActionM a
raise = Scotty.raise . view lazy

mergeWhen :: Event -> IO (Either Text ())
mergeWhen (MergeWhen body commentsURL) = do
  

main :: IO ()
main =
  Scotty.scotty 8080 slash
  where
    slash = Scotty.post "/" $ do
      json <- Scotty.param "payload"
      case Aeson.eitherDecodeStrict (json ^. re utf8) of
        Left e ->
          raise ("bad decode: " <> present e)
        Right event -> do
          mergeWhen event
