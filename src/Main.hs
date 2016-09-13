{-# LANGUAGE FlexibleContexts     #-}
{-# LANGUAGE FlexibleInstances    #-}
{-# LANGUAGE LambdaCase           #-}
{-# LANGUAGE OverloadedStrings    #-}
{-# LANGUAGE ScopedTypeVariables  #-}
{-# LANGUAGE TypeFamilies         #-}
{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE ViewPatterns         #-}

module Main where

------------------------------------------------------------------------------
import           Control.Lens
import           Control.Monad
import           Control.Monad.Reader
import           Data.Default
import           Data.Foldable
import           Data.Maybe
import           Data.Monoid
import           Data.RNG
import           Data.Text (Text)
import qualified Data.Text as T
import           Linear (_w, _x, _y, _z)
import qualified Linear as L
import           Reflex
import           Reflex.Dom
import           Reflex.Dom.Contrib.KeyEvent
import           Reflex.Dom.Contrib.Utils
------------------------------------------------------------------------------
import           Lib
------------------------------------------------------------------------------


main :: IO ()
main = appMain "approot" gameWidget

initBoard :: Board
initBoard = def & _y . _y .~ Just (Sum 2)
                & _w . _z .~ Just (Sum 2)

addNumber :: (Int,Int) -> Board -> Board
addNumber (i,j) b = b & (indToLens i) . (indToLens j) .~ Just (Sum 2)
  where
    indToLens 0 = _x
    indToLens 1 = _y
    indToLens 2 = _z
    indToLens 3 = _w
    indToLens _ = error "bad ind"


procKeyPress :: (KeyEvent, Double) -> Board -> Board
procKeyPress (ke,r) b = if b2 == b then b else addNumber newSquare b2
  where
    newSquare = emptySquares !! (floor $ r * fromIntegral (length emptySquares))
    b2 = next b
    emptySquares = getEmptyInds b2
    next = case keKeyCode ke of
             37 -> mkMove MoveLeft
             38 -> mkMove MoveUp
             39 -> mkMove MoveRight
             40 -> mkMove MoveDown
             _ -> id

addRandomVal :: MonadWidget t m => RNG -> Event t a -> m (Event t (a,Double))
addRandomVal rng e = performEvent (add <$> e)
  where
    add a = do
      r <- liftIO $ withRNG rng uniform
      return (a,r - 2**(-53))

gameWidget :: MonadWidget t m => App t m ()
gameWidget = do
  rng <- liftIO mkRNG
  putDebugLn "in gameWidget"
  kp <- asks bsKeyDown
  putDebugLnE kp (show . keKeyCode)
  kpWithRand <- addRandomVal rng kp
  b <- foldDyn ($) initBoard (procKeyPress <$> kpWithRand)
  elAttr "link" ("href" =: "css/main.css" <>
                 "rel" =: "stylesheet" <>
                 "type" =: "text/css"
                ) $ return ()
  divClass "container" $ do
    divClass "heading" $ do
      elClass "h1" "title" $ elAttr "a" ("href" =: "/") $ text "2048"
      divClass "scores-container" $ do
        divClass "score-container" $ do
          text "0"
          divClass "score-addition" $ text "+4"
        divClass "best-container" $ text "0"
    divClass "heading" $ do
      elClass "a" "restart-button" $ text "New Game"
      elClass "h2" "subtitle" $ do
        text "Play "
        el "strong" $ text "2048 Game"
        text " Online"
      divClass "above-game" $ el "p" $ do
        text "Join the numbers and get to the "
        el "strong" $ text "2048 tile!"
    divClass "game-container" $ do
      divClass "game-message" $ return ()
      divClass "grid-container" $ do
        replicateM_ 4 row
      _ <- widgetHoldHelper boardWidget initBoard (updated b)
      return ()
  where
    row = divClass "grid-row" $ replicateM_ 4 cell
    cell = divClass "grid-cell" $ return ()

boardWidget :: MonadWidget t m => Board -> m ()
boardWidget b = do
    divClass "tile-container" $ do
      void $ itraverse lineWidget $ toList b

lineWidget :: MonadWidget t m => Int -> L.V4 (Maybe (Sum Integer)) -> m ()
lineWidget i a = void $ itraverse (cellWidget i) $ toList a

cellWidget :: MonadWidget t m => Int -> Int -> Maybe (Sum Integer) -> m ()
cellWidget _ _ Nothing = return ()
cellWidget i j (Just v) = do
    divClass cls $
      divClass "tile-inner" $ text $ tshow (getSum v)
  where
    cls = T.unwords
            [ "tile"
            , "tile-" <> tshow (getSum v)
            , T.intercalate "-" ["tile-position", tshow (j+1), tshow (i+1)]
            ]

tshow :: Show a => a -> Text
tshow = T.pack . show

data Board = Board
    { boardSquares :: L.M44 (Maybe (Sum Integer))
    , boardScore   :: Int
    } deriving (Eq,Ord,Show,Read)

instance Default Board where
  def = L.V4 naught naught naught naught
    where naught = L.V4 Nothing Nothing Nothing Nothing

data Move = MoveUp | MoveDown | MoveLeft | MoveRight
  deriving (Eq,Ord,Show,Read)

mkMove :: Move -> Board -> Board
mkMove MoveUp = cols %~ mergeLine
mkMove MoveDown = locs %~ mergeLine
mkMove MoveLeft = rows %~ mergeLine
mkMove MoveRight = wors %~ mergeLine

instance Reversing (L.V4 a) where
  reversing v = L.V4 (v ^. _w) (v ^. _z) (v ^. _y) (v ^. _x)

mergeLine :: (Eq a, Monoid a) => [a] -> [a]
mergeLine (x:x':xs) | x == x' = (x <> x') : mergeLine xs
mergeLine (x:xs) = x : mergeLine xs
mergeLine [] = []

rows, wors, cols, locs :: Traversal' (L.M44 (Maybe  a)) [a]
rows = traverse . vecList
wors = traverse . reversed . vecList
cols = transposed . rows
locs = transposed . wors

transposed :: Iso' (L.M44 a) (L.M44 a)
transposed = iso L.transpose L.transpose

vecList :: Iso' (L.V4 (Maybe a)) [a]
vecList = iso vecToList vecFromList
  where
    vecToList v = reverse $ catMaybes $ foldl (flip (:)) [] v
    vecFromList (xs :: [a]) = L.V4 (xs^?ix 0) (xs^?ix 1) (xs^?ix 2) (xs^?ix 3)

getEmptyInds :: L.M44 (Maybe a) -> [(Int, Int)]
getEmptyInds = map dropLast . filter isEmpty . concat . imap f . toList . fmap (toList)
  where
    f i = imap (\j a -> (i,j,a))
    isEmpty (_,_,Nothing) = True
    isEmpty (_,_,_) = False
    dropLast (a,b,_) = (a,b)
