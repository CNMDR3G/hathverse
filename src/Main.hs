{-# LANGUAGE OverloadedStrings #-}
import System.Environment (lookupEnv)
import Control.Monad.IO.Class (liftIO)
import Web.Scotty
import Network.Wai.Middleware.Static
import Lucid
import Db
import Css
import View

main :: IO ()
main = do
  port <- maybe 3000 read <$> lookupEnv "PORT"

  scotty port $ do

    middleware $ staticPolicy $ addBase "static"

    get "/css/default.css" $ do
      addHeader "Content-Type" "text/css"
      text defaultCss

    get "/" $ do
      idTitles <- liftIO allProblemIdTitles
      html . renderText $ homepage idTitles
