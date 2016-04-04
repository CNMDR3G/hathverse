{-# LANGUAGE OverloadedStrings #-}
import Web.Scotty
import Text.Blaze.Html.Renderer.Text
import View.Homepage

main :: IO ()
main = scotty 3000 $ do
  get "/" $ html . renderHtml $ homepage
