module Style (cssText) where

import Data.Text.Lazy (Text)
import Clay

cssText :: Text
cssText = render formCss

formCss :: Css
formCss = do
  formContainerCss
  formLabelCss
  inputCss
  advertisementCss

formContainerCss :: Css
formContainerCss = "#formContainer" ?
 do background niceGrey
    display    inlineBlock
    border     solid (px 1) "#CDCDCD"
    borderRadius (px 5) (px 5) (px 5) (px 5)
    margin     (em 0.75) (em 0.75) (em 0.75) (em 0.75)
    padding    (px 10) (px 10) (px 10) (px 10)

inputCss :: Css
inputCss = "input" ?
  do padding    (px 9) (px 9) (px 9) (px 9)
     border     solid (px 1) "#E5E5E5"
     outline    solid (px 0) (rgba 0 0 0 255)
     fontFamily ["Verdana", "Tahoma"] [sansSerif]
     fontSize   (px 12)
     background ("#FFFFFF" :: Color)
     boxShadow  (px 0) (px 0) (px 8) (rgba 0 0 0 10)
     background $
       linearGradient (straight sideTop)
	 [ ("#ffffff", pct 0.1)
	 , ("#eeeeee", pct 14.9)
	 , ("#ffffff", pct 85.0)
	 ]

formLabelCss :: Css
formLabelCss = "form label" ?
  do marginLeft (px 10)
     fontSize (px 13)
     color coolBreezeGrey

advertisementCss :: Css
advertisementCss = "#ad" ?
  do marginLeft (px 5)
     fontSize (px 10)
     fontStyle italic
     color coolBreezeGrey

coolBreezeGrey :: Color
coolBreezeGrey = "#999999"

niceGrey :: Color
niceGrey = "#F5F5F5"
