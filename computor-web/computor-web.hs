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
import Data.Text hiding (map, zipWith, intersperse)
import Data.Maybe
import Data.Complex
import Data.List

import Computor
import Computor.Math
import Computor.Error

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
  let eqs = [
        "4 * X^0 = 0 * X^0 + 0 * X^1 + 1 * X^2",
        "1 * X^0 = 2 * X^0",
        "0 * X^0 + 1 * X^1 = 0 * X^0 + 1 * X^1"]
  defaultLayout [whamlet|
  ^{form Nothing}
  <ul>
    $forall eq <- eqs
      <li>
        <a href=@?{(ComputorR,[("input", eq)])}>#{eq}
  |]

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

instance ToMarkup Message where
  toMarkup (ReducedForm rp) = [shamlet|
  Reduced form: ^{rp_html} = 0
  |]
    where rp_html = case rp of
            RP [] -> "0"
            RP rp' -> sequence_ $ intersperse " + " $ zipWith3 monome rp' (cycle colors) [0..]
          monome :: Double -> Text -> Int -> Html
          monome coeff color deg = [shamlet|<span style="color: #{color}">#{show coeff} * X^#{deg}|]
          colors = ["#86C6EF", "#F16F55", "#7D71B2", "#9CCE82", "#D67BB0"]
  toMarkup (PolynomialDegree d) = [shamlet|
  Polynomial degree: #
  $case d
    $of Degree deg
      #{show deg}
    $of MinusInf
      -∞
  |]

getComputorR :: Handler Html
getComputorR = do
  input <- runInputGet $ ireq textField "input"
  let (res, messages) = runComputorM $ computor (unpack input)
  defaultLayout $(whamletFile "computor-web/computor.hamlet")

main :: IO ()
main = do
  warp 3000 ComputorWeb
