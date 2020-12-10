{-|
Module      : Common.Object.Types
Description : Some useful object types.
Copyright   : (c) Xie Ruifeng, 2020
License     : AGPL-3
Maintainer  : krantz.xrf@outlook.com
Stability   : experimental
Portability : portable
-}
{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE OverloadedStrings         #-}
{-# LANGUAGE QuasiQuotes               #-}
{-# LANGUAGE TypeFamilies              #-}
module Common.Object.Types where

import qualified Data.Text as T

import Control.Lens         ((.~), (?~))
import Control.Monad.State  (MonadState (..), evalState)
import Data.Foldable        (Foldable (toList))
import Data.Function        ((&))
import Data.Functor.Compose (Compose (Compose))
import GHC.Exts             (IsList (..), IsString (..))

import Common.HexColour              (rgba)
import Common.Object.SyntaxHighlight
import Common.Object.Transform       (GroupRender (..))

import Graphics.SvgTree hiding (Line)
import Reanimate
import Reanimate.LaTeX
import Reanimate.Scene  hiding (rectHeight, rectWidth)

-- |Center a group of objects.
centerAsGroup :: (Functor t, Foldable t) => t SVG -> t SVG
centerAsGroup xs = let g = mkGroup (Data.Foldable.toList xs) in fmap (centerUsing g) xs

-- * Any Renderable Object

-- |Any 'Renderable' object.
data AnyRenderable = forall a . Renderable a => AnyRenderable a

instance Renderable AnyRenderable where
  toSVG (AnyRenderable x) = toSVG x

-- * Groups

-- |A group of renderable objects, rendered using 'renderGroup'.
newtype ObjectGroup a = ObjectGroup { unwrapObjectGroup :: [a] }
  deriving stock (Show, Eq)

instance IsList (ObjectGroup a) where
  type Item (ObjectGroup a) = a
  fromList = ObjectGroup
  toList = unwrapObjectGroup

instance GroupRender a => Renderable (ObjectGroup a) where
  toSVG = mkGroup . renderGroup . unwrapObjectGroup

-- * Code Chunks

-- |Code chunks typeset by XeLaTeX in type writer font.
newtype CodeChunk = CodeChunk { unwrapCodeChunk :: T.Text }
  deriving stock (Show, Eq, Ord)
  deriving newtype (IsString)

instance GroupRender CodeChunk where
  renderGroup = centerAsGroup
    . fmap (withFillColorPixel [rgba|000|] . withStrokeWidth 0 . withFillOpacity 1)
    . latexCfgChunksTrans texCfg code . fmap unwrapCodeChunk
    where
      texCfg = TexConfig
        { texConfigEngine = XeLaTeX
        , texConfigHeaders =
          [ "\\usepackage{fontspec}"
          , "\\setmonofont{JetBrains Mono}[Contextuals=Alternate]" ]
        , texConfigPostScript = []
        }
      code x = "\\texttt{" <> x <> "}"

-- * Code Blocks.

-- |Code blocks in some programming language. Highlighted by Skylighting.
data CodeBlock = CodeBlock
  { codeLanguage :: T.Text
  , codeText     :: T.Text
  } deriving stock (Show, Eq, Ord)

instance Renderable CodeBlock where
  toSVG (CodeBlock lang txt) = mkGroup $ concat $ highlightIn lang txt

-- * TeX Codes

-- |TeX codes.
newtype TeX = TeX { unwrapTeXCode :: T.Text }
  deriving stock (Show, Eq, Ord)
  deriving newtype (IsString)

instance Renderable TeX where
  toSVG = withFillColorPixel [rgba|000|]
        . withStrokeWidth 0
        . withFillOpacity 1
        . ctexWithHeaders
          [ "\\setsansfont{Source Serif Pro}"
          , "\\setsansfont{Source Sans 3}"
          , "\\setCJKmainfont{Source Han Serif SC}"
          , "\\setCJKsansfont{Source Han Sans SC}" ]
        . unwrapTeXCode

-- * Paragraphs

-- |Renderable object, treated as a line.
data Line a
  = LeftAligned a
  | Centered a
  | RightAligned a
  deriving stock (Show, Eq, Ord)
  deriving stock (Functor, Traversable, Foldable)

instance IsString a => IsString (Line a) where
  fromString = LeftAligned . fromString

-- |Lines typeset by XeLaTeX in normal font.
type TextLine = Line TeX

-- |Lines wrapping any 'Renderable'.
type AnyLine = Line AnyRenderable

layoutLines :: Traversable t => t (Line SVG) -> t SVG
layoutLines xs = go xs where
  go = flip evalState 0 . traverse stackLine . fmap alignLine
  w0 = maximum $ svgWidth <$> Compose xs
  xMax a = let (x, _, w, _) = boundingBox a in x + w
  xMin a = let (x, _, _, _) = boundingBox a in x
  xCenter a = let (x, _, w, _) = boundingBox a in x + w / 2
  alignLine (LeftAligned x)  = translate (- xMin x) 0 x
  alignLine (RightAligned x) = translate (w0 - xMax x) 0 x
  alignLine (Centered x)     = translate (w0 / 2 - xCenter x) 0 x
  stackLine thisLine = get >>= \h0 -> do
    let (_, y, _, h) = boundingBox thisLine
    let y' = h0 - h - 0.5
    put y'
    pure (translate 0 (y' - y) thisLine)

instance Renderable a => GroupRender (Line a) where
  renderGroup = centerAsGroup . layoutLines . fmap (fmap toSVG)

-- * Text Bubbles.

-- |Make a rounded rectangle.
roundedRect :: Double -> Double -> Double -> SVG
roundedRect w h r = RectangleTree
  $ defaultSvg
  & rectWidth ?~ Num w
  & rectHeight ?~ Num h
  & rectUpperLeftCorner .~ (Num $ -w / 2, Num $ -h / 2)
  & rectCornerRadius .~ (Just $ Num r, Just $ Num r)

-- |Make a rounded square.
roundedSquare :: Double -> Double -> SVG
roundedSquare a = roundedRect a a

-- |Text bubble (tips), basically lines surrounded by a rounded rectangle.
newtype Bubble a = Bubble { unwrapBubble :: [a] }
  deriving stock (Show, Eq)

instance GroupRender a => Renderable (Bubble a) where
  toSVG = center . mkGroup . addRect . renderGroup . unwrapBubble
    where
      addRect s =
        let (x, y, w, h) = boundingBox (mkGroup s)
            (x', y', w', h') = (x + w / 2, y + h / 2, w + 1.2, h + 1.2)
            r = withFillOpacity 0.08
              $ withFillColorPixel [rgba|000|]
              $ roundedRect w' h' 0.36
        in translate x' y' r : s

-- |Create a new text bubble, attached to an existing object.
oNewBubble :: forall x a s . GroupRender x => Object s a -> [x] -> Scene s (Object s SVG)
oNewBubble o (scale 0.3 . toSVG . Bubble -> b) = do
  x <- oRead o oTranslateX
  y <- oRead o oBottomY
  res <- oNew b
  -- in NumInstance: tuples implement 'Num', 'Fractional', 'Floating', etc.
  -- fromInteger/fromFractional simply duplicates the number to all fields.
  oModify res (oMargin .~ 0)
  oModify res (oTranslateX .~ x)
  oModify res (oTopY .~ y)
  pure res

-- |Pop out a new text bubble (for a given duration), attached to an existing object.
oPopBubble :: forall x a s . GroupRender x => Duration -> Object s a -> [x] -> Scene s ()
oPopBubble dt o b = spriteScope $ do
  bub <- oNewBubble o b
  oShowWith bub oFadeIn
  wait dt
  oHideWith bub oFadeOut

-- |'CodeBlock' in a 'Bubble'.
type CodeBubble = Bubble (Line CodeBlock)

-- |Construct a 'CodeBubble'.
pattern CodeBubble :: T.Text -> T.Text -> CodeBubble
pattern CodeBubble lang t = Bubble [LeftAligned (CodeBlock lang t)]

-- |Construct a 'CodeBubble' in Haskell language.
pattern HaskellBubble :: T.Text -> CodeBubble
pattern HaskellBubble t = CodeBubble "haskell" t
