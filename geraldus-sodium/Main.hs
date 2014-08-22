module Main where

import Control.Applicative( pure )
import Control.Concurrent( forkIO )
import Control.Concurrent.Chan
import Control.Concurrent.MVar
import Control.Monad( foldM, forever, liftM, mapM, forM_, replicateM, when, void, join, liftM2 )
import Data.Default
import qualified Data.Text as T

import GHCJS.Foreign
import GHCJS.Types
import FRP.Sodium

import JavaScript.JQuery hiding (Event, not)
import qualified JavaScript.JQuery as JQ

data PopupTab    = PopupTab { exTrigger :: JQuery
                            , inTrigger :: JQuery
                            , content   :: JQuery
                            }

data PopupWidget = PopupWidget { window :: JQuery
                               , closeB :: JQuery
                               , tabs   :: [PopupTab]
                               }

foreign import javascript unsafe "$1.length" jq_length :: JQuery -> IO Int

elems :: JQuery -> IO [JQuery]
elems a = do
  len <- jq_length a
  mapM (flip eq a) [0..len - 1]

bldWidget :: IO PopupWidget
bldWidget = do
    pppWnd <- select "#popup"
    pppCls <- select "#ppp-close"
    tabs   <- find ".content" pppWnd >>= elems
    pppTbs <- gatherTabs tabs
    return $ PopupWidget pppWnd pppCls pppTbs
    where gatherTabs :: [JQuery] -> IO [PopupTab]
          gatherTabs = mapM (\t -> getAttr "id" t >>= bldTab . (T.drop 8))
          bldTab eId = do
              exT <- select (T.append "#ppp-show-" eId)
              inT <- select (T.append "#item-"     eId)
              cnt <- select (T.append "#content-"  eId)
              return $ PopupTab exT inT cnt

mkEvents :: Int -> IO [(Event a, a -> Reactive ())]
mkEvents n = sync $ replicateM n newEvent

mkReactiveUi :: PopupWidget
               -> [(Event a, a -> Reactive ())]
               -> Reactive (Behavior (Maybe PopupTab))
mkReactiveUi pppw es = do
    let closeBhv = pure (Nothing :: Maybe PopupTab)
    items' <- mapM (\t -> newBehavior (Just t) >>= return . fst) $ tabs pppw
    let items = closeBhv:items'
    events <- mapM (return . fst) es
    let eItems = zipWith (\i e -> fmap (const i) e) items events
    sel    <- hold closeBhv $ mergeFold eItems
    switch sel
  where mergeFold :: [Event a] -> Event a
        mergeFold (e:es) = foldl merge e es

render :: PopupWidget -> Maybe PopupTab -> IO ()
render w Nothing  = (popupClose w) >> (releaseTabs w)
render w (Just t) = (popupOpen w) >> (renderTabs w t)

popupClose :: PopupWidget -> IO ()
popupClose w = void $ addClass "Dn" (window w)

popupOpen  :: PopupWidget -> IO ()
popupOpen  w = void $ removeClass "Dn" (window w)

releaseTabs :: PopupWidget -> IO ()
releaseTabs w = mapM_ (\t -> do
                          removeClass "hold" (exTrigger t)
                          removeClass "hold" (inTrigger t)
                          ) (tabs w)

toggleClass :: Bool -> T.Text -> JQuery -> IO JQuery
toggleClass p = if p then addClass else removeClass

renderTabs :: PopupWidget -> PopupTab -> IO ()
renderTabs w t = forM_ (tabs w) (\t' -> do
                   cur <- liftM2 (==) (getId t) (getId t')
                   toggleClass (not cur) "dn" (content t')
                   toggleClass cur "hold" (exTrigger t')
                   toggleClass cur "hold" (inTrigger t'))
    where getId t' = getAttr "id" (content t')

onClick :: (() -> Reactive ()) -> JQuery -> IO () 
onClick f = void . click (const $ sync $ f ()) def

main :: IO ()
main = do
    w <- bldWidget
    es <- mkEvents $ length (tabs w) + 1
    rui  <- sync $ mkReactiveUi w es
    kill <- sync $ listen (value rui) (render w)

    let (_, pushClose):ls = es
    forM_ (zip (tabs w) ls) (\(t,(_,f)) -> do
                               onClick f (exTrigger t)
                               onClick f (inTrigger t))

    onClick pushClose (closeB w)
