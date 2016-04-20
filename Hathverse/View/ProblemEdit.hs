{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
module Hathverse.View.ProblemEdit where

import Data.Int (Int64)
import Data.Monoid ((<>))
import qualified Data.Text as T
import Hathverse.View.Common
import Hathverse.Db (Problem(..))
import Lucid

problemEditView :: Maybe (Int64, Problem) -> HtmlGen
problemEditView maybeProblem =
  let (pageTitle, probTitle, probDesc, probMod,
       probTemp, probSol, probCheck) =
        case maybeProblem of
          Nothing ->
            ("New Problem" , "", "", "", "", "", "")
          Just (pid, Problem{..}) ->
            ( "Edit #" <> T.pack (show pid) <> " " <> problemTitle
            , problemTitle, problemDescription, problemModuleName
            , problemTemplate, problemSolution, problemCheckProgram
            )
  in  withTitleBody (toHtml pageTitle) $ do
    h1_ (toHtml pageTitle)
    form_ [action_ "#"] $ do

      div_ [class_ "form-group"] $ do
        _ <- label_ [for_ "titleedit"] "Title:"
        input_ [id_ "titleeidt", class_ "form-control", type_ "text", value_ probTitle]

      div_ [class_ "form-group"] $ do
        _ <- label_ [for_ "descedit"] "Description:"
        textarea_ [id_ "descedit", class_ "form-control"] $ toHtml probDesc

      div_ [class_ "form-group"] $ do
        _ <- label_ [for_ "modulename"] "Module name used in checking:"
        input_ [id_ "modulename", class_ "form-control", type_ "text", value_ probMod]

      div_ [class_ "form-group"] $ do
        _ <- label_ [for_ "template"] "Solution template:"
        div_ [id_ "template"] $ toHtml probTemp

      div_ [class_ "form-group"] $ do
        _ <- label_ [for_ "solution"] "Solution:"
        div_ [id_ "solution"] $ toHtml probSol

      div_ [class_ "form-group"] $ do
        _ <- label_ [for_ "checkprogram"] "Check program:"
        div_ [id_ "checkprogram"] $ toHtml probCheck

      div_ [id_ "buttons"] $ do
        button_ [id_ "run", class_ "btn btn-primary"]
          $ toHtml ("test run" :: String)
        button_ [id_ "submit", class_ "btn btn-success", style_ "margin-left: 10px"]
          $ toHtml ("submit" :: String)

      div_ [id_ "results", class_ "alert", style_ "display: none"] $
        pre_ [id_ "result"] $ toHtml ("" :: String)

    script_ [src_ "/js/problemedit.js"] ("" :: String)
