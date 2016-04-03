{-# LANGUAGE OverloadedStrings #-}
import qualified Data.Text as T
import Web.Scotty as S
import Text.Blaze.Html5 as H hiding (main)
import qualified Text.Blaze.Html5.Attributes as A
import Text.Blaze.Html.Renderer.Text

index :: Html
index = H.docTypeHtml $ do
  H.head $ title "home"
  H.body $
    H.form ! A.method "post" ! A.action "/check" $ do
      textarea ! A.name "solution" $
        "main = putStrLn \"Hello world!\""
      H.input ! A.type_ "submit" ! A.value "submit"

check :: Bool -> Html
check result = H.docTypeHtml $ do
  H.head $ title "check"
  H.body $ H.toHtml $ show result

main :: IO ()
main = scotty 3000 $ do
  get "/" $ S.html . renderHtml $ index
  post "/check" $ do
    solution <- S.param "solution"
    let right_solution = "main = putStrLn \"Hello world!\"" :: T.Text
    S.html . renderHtml . check $ solution == right_solution
