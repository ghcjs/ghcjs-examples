{-# LANGUAGE OverloadedStrings, ScopedTypeVariables      #-}

module Main where

import           Control.Applicative
import           Control.Monad
import           Data.Default
import           Data.Text (Text)
import qualified Data.Text as T
import           Text.Read

import           JavaScript.JQuery

import           FRP.Sodium

main :: IO ()
main = do
  body <-       select "body"
  [op1, op2] <- replicateM 2 $
     fmap (fmap (readMaybe . T.unpack)) (reactiveTextInput "0" body)
  let items = [ ("add"     , arithBehaviour op1 op2 (+))
              , ("multiply", arithBehaviour op1 op2 (*))
              ]
  sel <- reactiveSelect items body
  output <- select "<div />"
  appendJQuery output body
  sync $ do
    result <- switch sel
    listen (values result) $ \v -> void $
      setText (maybe "invalid input" (T.pack.show) v) output
  return ()

arithBehaviour :: Behaviour (Maybe Integer)
               -> Behaviour (Maybe Integer)
               -> (Integer -> Integer -> Integer)
               -> Behaviour (Maybe Integer)
arithBehaviour op1 op2 f = liftA2 f <$> op1 <*> op2

reactiveTextInput :: Text -> JQuery -> IO (Behaviour Text)
reactiveTextInput value parent = do
  (b, a) <- sync (newBehaviour value)
  input <- select "<input type='text' />"
  setVal value input
  appendJQuery input parent
  let handler _ = sync . a =<< getVal input
  on handler "keyup change" def input
  return b

reactiveSelect :: [(Text,a)] -> JQuery -> IO (Behaviour a)
reactiveSelect items parent = do
  (b, a) <- sync (newBehaviour . snd . head $ items)
  sel <- select "<select />"
  forM_ (zip [(0::Int)..] items) $ \(n,(name,_)) -> do
    opt <- select "<option />"
    setAttr "value" (T.pack . show $ n) opt
    when (n == 0) $ void (setAttr "selected" "true" opt)
    setText name opt
    appendJQuery opt sel
  appendJQuery sel parent
  let handler _ = sync . a =<< snd.(items !!).read.T.unpack <$> getVal sel
  on handler "change" def sel
  return b
