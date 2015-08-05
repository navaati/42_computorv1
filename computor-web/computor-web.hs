{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE QuasiQuotes           #-}
{-# LANGUAGE TemplateHaskell       #-}
{-# LANGUAGE TypeFamilies          #-}
{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MultiParamTypeClasses #-}

import Yesod
import Text.Hamlet
import Text.Blaze
import Data.Text hiding (map, zip)
import Data.Maybe
import Data.Complex

import Computor
import Computor.Math
import Computor.Error
import Computor.Displaying

data ComputorWeb = ComputorWeb

mkYesod "ComputorWeb" [parseRoutes|
/ HomeR GET
/computor ComputorR GET
/bootstrap.min.css BootstrapR GET
|]

instance Yesod ComputorWeb where
  defaultLayout contents = do
    PageContent _ headTags bodyTags <- widgetToPageContent contents
    withUrlRenderer $(hamletFile "computor-web/layout.hamlet")

getBootstrapR :: Handler TypedContent
getBootstrapR = sendFile typeCss "computor-web/bootstrap.min.css"

instance RenderMessage ComputorWeb FormMessage where
  renderMessage _ _ = defaultFormMessage

form :: Maybe Text -> Widget
form value = [whamlet|
  <div .panel.panel-default>
    <form .panel-body method=get action=@{ComputorR}>
      <div .input-group>
        <input .form-control type=text name=input :isJust value:value=#{fromJust value}>
        <span .input-group-btn>
          <button .btn.btn-default>Résoudre
  |]

getHomeR :: Handler Html
getHomeR = do
  defaultLayout $ form Nothing

tableHelper :: ToMarkup a => [(Markup, a)] -> Markup
tableHelper pairs = [shamlet|
  <table .table.table-bordered>
    $forall (a, b) <- pairs
      <tr>
        <td>#{a}
        <td>#{b}
  |]

xSub :: Int -> Markup
xSub i = [shamlet|x<sub>#{i}</sub>|]

instance ToMarkup (Complex Double) where
  toMarkup = toMarkup . displayComplex

instance ToMarkup Solution where
  toMarkup (OneRoot x) = [shamlet|
  <p>The equation has one solution:
  ^{tableHelper [("x", x)]}
  |]
  toMarkup (TwoRoots (x1 :+ 0) (x2 :+ 0)) = [shamlet|
  <p>Discriminant is strictly positive, the equation has two real solutions:
  ^{tableHelper [(xSub 1, x1), (xSub 2, x2)]}
  |]
  toMarkup (TwoRoots x1 x2) = [shamlet|
  <p>Discriminant is strictly negative, the equation has two complex solutions:
  ^{tableHelper [(xSub 1, x1), (xSub 2, x2)]}
  |]
  toMarkup (DoubleRoot x) = let table = tableHelper [([shamlet|#{xSub 1} = #{xSub 2}|], x)]
                            in [shamlet|
  <p>Discriminant is null, the equation has one double solution:
  ^{table}
  |]
  toMarkup AllReals = [shamlet|
  <p>The equation holds true for all numbers
  |]

getComputorR :: Handler Html
getComputorR = do
  input <- runInputGet $ ireq textField "input"
  let (res, messages) = runComputorM $ computor [unpack input]
  defaultLayout $(whamletFile "computor-web/computor.hamlet")

main :: IO ()
main = do
  warp 3000 ComputorWeb
