{-|
Module      : Common.Object.SyntaxHighlight
Description : Syntax highlighting support for Reanimate through Skylighting.
Copyright   : (c) Xie Ruifeng, 2020
License     : AGPL-3
Maintainer  : krantz.xrf@outlook.com
Stability   : experimental
Portability : portable
-}
{-# LANGUAGE OverloadedStrings #-}
module Common.Object.SyntaxHighlight where

import qualified Data.Text as T

import Control.Arrow ((>>>))
import Data.Function ((&))
import Data.Maybe    (fromJust)

import Control.Lens.Internal.Getter (AlongsideRight (..))
import Data.Bifunctor               (Bifunctor (second))
import Data.Coerce                  (Coercible, coerce)
import Data.Functor.Compose         (Compose (..))

import qualified Data.Map as M

import Common.Object.SyntaxHighlight.Theme (tomorrow)

import Codec.Picture              (PixelRGBA8 (..))
import Reanimate                  (SVG)
import Reanimate.LaTeX
import Reanimate.Svg.Constructors (withFillColorPixel)
import Skylighting.Core           (lookupSyntax)
import Skylighting.Syntax         (defaultSyntaxMap)
import Skylighting.Tokenizer      (TokenizerConfig (..), tokenize)
import Skylighting.Types

-- |Evaluate to the first, with the second as a type hint.
asIf :: a -> a -> a
asIf x _ = x
{-# INLINE asIf #-}

-- |Coerce the data, as if it were converted using the given converter.
coerceAsIf :: Coercible a b => (a -> b) -> (a -> b)
coerceAsIf = asIf coerce

-- |Default tokenizer config: with default syntax map, without trace output.
defaultTokenizerConfig :: TokenizerConfig
defaultTokenizerConfig = TokenizerConfig
  { syntaxMap = defaultSyntaxMap
  , traceOutput = False
  }

-- |Highlight with a specified language syntax.
-- Use 'lookupSyntax' to find a proper 'Syntax'.
highlight :: Syntax -> T.Text -> [[SVG]]
highlight syn = highlightWith defaultTokenizerConfig tomorrow syn >>> \case
  Left err  -> error ("syntax highlight error: " <> err)
  Right res -> res

-- |Highlight code in some language.
highlightIn :: T.Text -> T.Text -> [[SVG]]
highlightIn lang = highlight $ fromJust $ lookupSyntax lang defaultSyntaxMap

-- |Highlight Haskell code.
highlightHs :: T.Text -> [[SVG]]
highlightHs = highlightIn "haskell"

-- |Custom highlight with maximum flexibility.
highlightWith :: TokenizerConfig -> Style -> Syntax -> T.Text -> Either String [[SVG]]
highlightWith cfg style syn code = do
  ls <- tokenize cfg syn code
  let applyOtherStyles (tp, txt) =
        (tp, case M.lookup tp (tokenStyles style) of
          Nothing -> txt
          Just TokenStyle{..} ->
            txt & styled tokenBold "textbf"
                & styled tokenItalic "textit"
                & styled tokenUnderline "underline")
      styled b t x = if b then "\\" <> t <> "{" <> x <> "}" else x
  let gs = coerceAsIf (map getAlongsideRight . getCompose)
         $ latexCfgChunksTrans texCfg setFont
         $ coerceAsIf (Compose . map AlongsideRight)
         $ map ( (<> [(OtherTok, "\\\\\n")])
               . map (applyOtherStyles . second escapeLaTeX) ) ls
      texCfg = TexConfig XeLaTeX headers []
      headers =
        [ "\\usepackage{fontspec}"
        , "\\setmonofont{JetBrains Mono}[Contextuals=Alternate]" ]
      -- fix "there is no new line to end" error.
      setFont "\\\\\n" = ""
      setFont t        = "\\tt " <> t
  let applyColour (tp, svg) =
        svg & case (M.lookup tp (tokenStyles style), defaultColor style) of
          (Just TokenStyle{tokenColor = Just (RGB r g b)}, _) ->
            withFillColorPixel (PixelRGBA8 r g b 0xFF)
          (_, Just (RGB r g b)) -> withFillColorPixel (PixelRGBA8 r g b 0xFF)
          _ -> id
  pure (map (map applyColour) gs)

-- |Escape LaTeX text. Stolen from @Skylighting.Format.LaTeX.escapeLaTeX@.
escapeLaTeX :: T.Text -> T.Text
escapeLaTeX = T.concatMap $ \case
  '\\' -> "\\textbackslash{}"
  '{'  -> "\\{"
  '}'  -> "\\}"
  '_'  -> "\\_"
  '&'  -> "\\&"
  '%'  -> "\\%"
  '#'  -> "\\#"
  '`'  -> "\\textasciigrave{}"
  '\'' -> "\\textquotesingle{}"
  -- we don't want to prevent ligatures
  -- '-'  -> "{-}"
  '~'  -> "\\textasciitilde{}"
  '^'  -> "\\^{}"
  '>'  -> "\\textgreater{}"
  '<'  -> "\\textless{}"
  c    -> T.singleton c
