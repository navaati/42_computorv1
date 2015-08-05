{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE QuasiQuotes           #-}
{-# LANGUAGE TemplateHaskell       #-}
{-# LANGUAGE TypeFamilies          #-}
{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MultiParamTypeClasses #-}

import Yesod
import Yesod.Form.Bootstrap3
import Text.Hamlet
import Text.Blaze
import Data.Text hiding (map, zip)
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

equationForm :: Html -> MForm Handler (FormResult Text, Widget)
equationForm = renderBootstrap3 BootstrapBasicForm $ areq textField (bfs ("Equation: " :: Text)) Nothing

formTemplate ((result, formWidget), enctype) = (result, [whamlet|
    <div .panel.panel-default>
      <form .panel-body method=get action=@{ComputorR} enctype=#{enctype}>
        ^{formWidget}
        <button>Résoudre
  |])

getHomeR :: Handler Html
getHomeR = do
  (_, formWidget) <- formTemplate <$> runFormGet equationForm
  defaultLayout formWidget

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
  (formResult, formWidget) <- formTemplate <$> runFormGet equationForm
  let input = case formResult of
        FormSuccess equation -> [unpack equation]
        _ -> []
  let (res, messages) = runComputorM $ computor input
  defaultLayout $(whamletFile "computor-web/computor.hamlet")

main :: IO ()
main = do
  warp 3000 ComputorWeb
