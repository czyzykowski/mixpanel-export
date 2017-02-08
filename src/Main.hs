{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleInstances #-}

-- TODO: Ensure that passed data is valid: the right format and that it
--       represents valid date

module Main where

import           Control.Exception (throwIO)
import           Control.Monad.IO.Class
import           Control.Monad.Trans.Resource (ResourceT)
import qualified Data.Aeson as A
import qualified Data.ByteString.Char8 as C8
import           Data.Conduit
import qualified Data.Conduit.Combinators as C
import qualified Data.Conduit.Binary as CB
import qualified Data.Conduit.BZlib as BZ
import qualified Data.Conduit.JSON.NewlineDelimited as JSONL
import qualified Data.HashMap.Lazy as M
import qualified Data.Text as T
import           Network.HTTP.Req
import           Network.HTTP.Req.Conduit
import           Options.Applicative


mixpanelExportURL :: Url 'Https
mixpanelExportURL = https "data.mixpanel.com" /: "api" /: "2.0" /: "export"


data Options = Options
  { fromDate       :: String
  , toDate         :: String
  , mixpanelSecret :: String
  , outputFile     :: FilePath
  , timeout        :: Int
  , removeProps    :: [T.Text]
  , compress       :: Bool
  } deriving (Show, Eq)


instance MonadHttp (ConduitM i o (ResourceT IO)) where
  handleHttpException = liftIO . throwIO


removeEmails :: [T.Text] -> M.HashMap T.Text A.Value -> M.HashMap T.Text A.Value
removeEmails remove = M.adjust f "properties"
  where f (A.Object m) = A.Object $ M.filterWithKey g m
        f x            = x
        g k _          = T.toLower k `notElem` remove


strip :: [T.Text] -> A.Value -> A.Value
strip remove (A.Object m) = A.Object $ removeEmails remove m
strip _ x                 = x


run :: Options -> IO ()
run options = runConduitRes $ do
  let qs     = "from_date" =: fromDate options <> "to_date" =: toDate options
  let auth   = basicAuth (C8.pack $ mixpanelSecret options) ""
  let time   = responseTimeout (timeout options)
  let remove = T.toLower <$> removeProps options
  let output = outputFile options ++ (if compress options then ".bz2" else "")
  let destination = CB.sinkFile output

  req' GET mixpanelExportURL NoReqBody httpSource (qs <> auth <> time)
    .| JSONL.parser
    .| C.map (strip remove)
    .| JSONL.serializer
    .| if compress options then BZ.bzip2 .| destination else destination


secondsOption :: Mod OptionFields Int -> Parser Int
secondsOption = fmap (*1000000) . option auto


removePropsOption :: Mod OptionFields String -> Parser [T.Text]
removePropsOption = fmap (T.splitOn "," . T.pack) . strOption

main :: IO ()
main = execParser opts >>= run
  where
    parser = Options
      <$> strOption         (long "from"         <> help "from date")
      <*> strOption         (long "to"           <> help "to date")
      <*> strOption         (long "secret"       <> help "mixpanel api secret")
      <*> strOption         (long "output"       <> help "output file")
      <*> secondsOption     (long "timeout"      <> help "response timeout" <> value 3000) -- 50 minutes should be enough
      <*> removePropsOption (long "remove-props" <> help "list of properties to remove (matched regardless of the case)" <> value [])
      <*> switch            (long "compress"     <> help "should the output be bzip2 compressed")

    opts   = info (helper <*> parser) (fullDesc <> progDesc "export data from mixpanel")
