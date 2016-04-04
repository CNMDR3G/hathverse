{-# LANGUAGE OverloadedStrings #-}
import Control.Monad.IO.Class (liftIO)
import Web.Scotty
import Text.Blaze.Html.Renderer.Text
import Network.Wai.Middleware.Static
import Db
import View

main :: IO ()
main = scotty 3000 $ do
    middleware $ staticPolicy $ addBase "static"
    get "/" $ do
      idTitles <- liftIO allProblemIdTitles
      html . renderHtml $ homepage idTitles
