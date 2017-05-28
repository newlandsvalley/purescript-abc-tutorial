module View.CSS where

import Prelude (discard, ($), (#))
import Text.Smolder.Markup (Attribute)
import Data.NonEmpty (singleton)
import Pux.DOM.HTML.Attributes (style)
import CSS.Background (backgroundColor)
import CSS.Color (rgb, red, lightgrey, darkgrey)
import CSS.Common (auto)
import CSS.String (fromString)
import CSS.Display (display, block, float, floatLeft)
import CSS.Font (GenericFontFamily(..), color, fontSize, fontFamily)
import CSS.Geometry (height, width, padding, margin, marginLeft, marginRight)
import CSS.Size (px, em)
import CSS.TextAlign (textAlign, leftTextAlign, center)

monospace :: GenericFontFamily
monospace = GenericFontFamily $ fromString "monospace"

taStyle :: Attribute
taStyle =
    style do
      padding (px 10.0) (px 0.0) (px 10.0) (px 0.0)
      fontSize (em 1.5)
      fontFamily [ "Times New Roman" ] (singleton monospace)
      backgroundColor (rgb 243 246 198)
      textAlign leftTextAlign
      margin (px 0.0) (px 2.0) (px 0.0) (px 2.0)
      display block

instructionStyle :: Attribute
instructionStyle =
    style do
      fontSize (em 1.3)
      fontFamily [ "Times New Roman" ] (singleton monospace)
      -- backgroundColor white
      textAlign leftTextAlign
      margin (px 10.0) (px 80.0) (px 20.0) (px 40.0)
      height (em 4.0)
      display block

hintStyle :: Attribute
hintStyle =
    style do
      fontSize (em 1.3)
      fontFamily [ "Times New Roman" ] (singleton monospace)
      -- backgroundColor white
      textAlign leftTextAlign
      margin (px 10.0) (px 80.0) (px 20.0) (px 40.0)
      width (em 25.0)
      display block

centreStyle :: Attribute
centreStyle =
  style do
    textAlign center
    -- margin auto auto auto auto
    marginLeft auto
    marginRight auto

leftPaneStyle :: Attribute
leftPaneStyle =
  style do
    width (px 800.0)
    float floatLeft

floatLeftStyle :: Attribute
floatLeftStyle =
  style do
    float floatLeft

leftComponentStyle :: Attribute
leftComponentStyle =
  style do
    float floatLeft
    margin (px 10.0) (px 0.0) (px 10.0) (px 0.0)


leftMarginStyle :: Attribute
leftMarginStyle =
    style $ do
      margin (10.0 # px) (px 0.0) (px 20.0) (px 40.0)

errorHighlightStyle :: Attribute
errorHighlightStyle =
  style do
    color red

buttonStyle :: Boolean -> Attribute
buttonStyle enabled =
  if enabled then
    style do
      margin (px 0.0) (px 0.0) (px 0.0) (px 10.0)
      fontSize (em 1.0)
  else
    style do
      margin (px 0.0) (px 0.0) (px 0.0) (px 10.0)
      fontSize (em 1.0)
      backgroundColor lightgrey
      color darkgrey
