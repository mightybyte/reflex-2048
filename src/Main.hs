{-# LANGUAGE FlexibleContexts     #-}
{-# LANGUAGE FlexibleInstances    #-}
{-# LANGUAGE LambdaCase           #-}
{-# LANGUAGE OverloadedStrings    #-}
{-# LANGUAGE RecursiveDo          #-}
{-# LANGUAGE ScopedTypeVariables  #-}
{-# LANGUAGE TemplateHaskell      #-}
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


data SquareStatus = SquareMerged | SquareNew
  deriving (Eq,Ord,Show,Read)

data Square = Square
    { _squareNumber :: Maybe (Sum Int)
    , _squareStatus :: Maybe SquareStatus
    } deriving (Eq,Ord,Show,Read)

makeLenses ''Square

instance Default Square where
    def = Square Nothing Nothing

mergeSquares :: Square -> Square -> Square
mergeSquares a b  = Square (_squareNumber a <> _squareNumber b) (Just SquareMerged)

data Board = Board
    { _boardSquares :: L.M44 Square
    , _boardScore   :: Int
    } deriving (Eq,Ord,Show,Read)

makeLenses ''Board

instance Default Board where
  def = Board (L.V4 naught naught naught naught) 0
    where naught = L.V4 def def def def

data Move = MoveUp | MoveDown | MoveLeft | MoveRight
  deriving (Eq,Ord,Show,Read)

initBoard :: Board
initBoard = def & boardSquares . _y . _y .~ square2
                & boardSquares . _w . _z .~ square2

square2 :: Square
square2 = Square (Just (Sum 2)) Nothing

clearStatus :: Board -> Board
clearStatus = boardSquares . each . each . squareStatus .~ Nothing

updateScore :: Board -> Board
updateScore b = b & boardScore %~ (+newPoints)
  where
    isMerged s = _squareStatus s == Just SquareMerged
    newPoints =
      sum $ map (maybe 0 getSum . _squareNumber)
          $ filter isMerged $ concat $ toList
          $ fmap (toList) $ _boardSquares b

addNumber :: (Int,Int) -> Board -> Board
addNumber (i,j) b = b & boardSquares . (indToLens i) . (indToLens j) .~ s2
  where
    s2 = Square (Just (Sum 2)) (Just SquareNew)
    indToLens 0 = _x
    indToLens 1 = _y
    indToLens 2 = _z
    indToLens 3 = _w
    indToLens _ = error "bad ind"


procKeyPress :: (KeyEvent, Double) -> Board -> Board
procKeyPress (ke,r) b = updateScore b3
  where
    newSquare = emptySquares !! (floor $ r * fromIntegral (length emptySquares))
    b1 = clearStatus b
    b2 = next b1
    b3 = if b2 == b1 then b1 else addNumber newSquare b2
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
    kp <- asks bsKeyDown
    kpWithRand <- addRandomVal rng kp
    rec b <- foldDyn ($) initBoard $ leftmost
               [ procKeyPress <$> kpWithRand
               , const initBoard <$ newGame
               ]
        elAttr "link" ("href" =: "css/main.css" <>
                       "rel" =: "stylesheet" <>
                       "type" =: "text/css"
                      ) $ return ()
        newGame <- divClass "container" $ do
          divClass "heading" $ do
            elClass "h1" "title" $ elAttr "a" ("href" =: "/") $ text "2048"
            divClass "scores-container" $ do
              divClass "score-container" $ do
                display $ _boardScore <$> b
                divClass "score-addition" $ text "+4"
              divClass "best-container" $ text "0"
          restart <- divClass "heading" $ do
            (buttonEl,_) <- elClass' "a" "restart-button" $ text "New Game"
            elClass "h2" "subtitle" $ do
              text "Play "
              el "strong" $ text "2048 Game"
              text " Online"
            divClass "above-game" $ el "p" $ do
              text "Join the numbers and get to the "
              el "strong" $ text "2048 tile!"
            return $ domEvent Click buttonEl
          divClass "game-container" $ do
            divClass "game-message" $ return ()
            divClass "grid-container" $ do
              replicateM_ 4 row
            _ <- widgetHoldHelper boardWidget initBoard (updated b)
            return ()
          return restart
        return ()
    return ()
  where
    row = divClass "grid-row" $ replicateM_ 4 cell
    cell = divClass "grid-cell" $ return ()

boardWidget :: MonadWidget t m => Board -> m ()
boardWidget b = do
    divClass "tile-container" $ do
      void $ itraverse lineWidget $ toList $ _boardSquares b

lineWidget :: MonadWidget t m => Int -> L.V4 Square -> m ()
lineWidget i a = void $ itraverse (cellWidget i) $ toList a

cellWidget :: MonadWidget t m => Int -> Int -> Square -> m ()
cellWidget i j s =
    case _squareNumber s of
      Nothing -> return ()
      Just v -> do
        let cls = addNewOrMerged $ T.unwords
                    [ "tile"
                    , "tile-" <> tshow (getSum v)
                    , T.intercalate "-" ["tile-position", tshow (j+1), tshow (i+1)]
                    ]
        divClass cls $ divClass "tile-inner" $ text $ tshow (getSum v)
  where
    addNewOrMerged = case _squareStatus s of
                       Nothing -> id
                       Just SquareMerged -> (<> " tile-merged")
                       Just SquareNew -> (<> " tile-new")

mkMove :: Move -> Board -> Board
mkMove MoveUp = over boardSquares (cols %~ mergeLine)
mkMove MoveDown = over boardSquares (locs %~ mergeLine)
mkMove MoveLeft = over boardSquares (rows %~ mergeLine)
mkMove MoveRight = over boardSquares (wors %~ mergeLine)

instance Reversing (L.V4 a) where
  reversing v = L.V4 (v ^. _w) (v ^. _z) (v ^. _y) (v ^. _x)

mergeLine :: [Square] -> [Square]
mergeLine (x:x':xs) | _squareNumber x == _squareNumber x' =
  (mergeSquares x x') : mergeLine xs
mergeLine (x:xs) = x : mergeLine xs
mergeLine [] = []

rows, wors, cols, locs :: Traversal' (L.M44 Square) [Square]
rows = traverse . vecList
wors = traverse . reversed . vecList
cols = transposed . rows
locs = transposed . wors

transposed :: Iso' (L.M44 a) (L.M44 a)
transposed = iso L.transpose L.transpose

isSquareEmpty :: Square -> Bool
isSquareEmpty (Square Nothing _) = True
isSquareEmpty _ = False

vecList :: Iso' (L.V4 Square) [Square]
vecList = iso vecToList vecFromList

vecToList :: L.V4 Square -> [Square]
vecToList v = reverse $ filter (not . isSquareEmpty) $ foldl (flip (:)) [] v

vecFromList :: [Square] -> L.V4 Square
vecFromList xs = L.V4 (mk $ xs^?ix 0) (mk $ xs^?ix 1)
                      (mk $ xs^?ix 2) (mk $ xs^?ix 3)
  where
    mk :: Maybe Square -> Square
    mk = fromMaybe def

getEmptyInds :: Board -> [(Int, Int)]
getEmptyInds =
    map dropLast . filter isEmpty . concat . imap f . toList . fmap (toList)
                 . _boardSquares
  where
    f i = imap (\j a -> (i,j,a))
    isEmpty (_,_,s) = isSquareEmpty s
    dropLast (a,b,_) = (a,b)

main :: IO ()
main = appMain "approot" gameWidget
