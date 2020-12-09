{-|
Module      : Common.Object.SyntaxHighlight.Theme
Description : Tomorrow theme for Skylighting.
Copyright   : (c) Xie Ruifeng, 2020
License     : AGPL-3
Maintainer  : krantz.xrf@outlook.com
Stability   : experimental
Portability : portable
-}
{-# LANGUAGE QuasiQuotes #-}
module Common.Object.SyntaxHighlight.Theme (tomorrow) where

import qualified Data.Map as M

import Common.HexColour
import Skylighting.Types

normal :: Color -> TokenStyle
normal c = defStyle { tokenColor = Just c }

bold :: Color -> TokenStyle
bold c = (normal c) {tokenBold = True}

foreground, background, comment, red, orange, yellow, green, aqua, blue, purple
  :: FromRGBA8 c => c
foreground = [rgba|4d4d4c|]
background = [rgba|f7f7f7|]
comment    = [rgba|8e908c|]
red        = [rgba|c82829|]
orange     = [rgba|f5871f|]
yellow     = [rgba|eab700|]
green      = [rgba|718c00|]
aqua       = [rgba|3e999f|]
blue       = [rgba|4271ae|]
purple     = [rgba|8959a8|]

-- | Tomorrow theme.
tomorrow :: Style
tomorrow = Style{
  tokenStyles = M.fromList
    [ ( KeywordTok,        bold   purple)
    , ( DataTypeTok,       normal yellow)
    , ( DecValTok,         normal orange)
    , ( BaseNTok,          normal orange)
    , ( FloatTok,          normal orange)
    , ( ConstantTok,       normal red)
    , ( CharTok,           normal foreground)
    , ( SpecialCharTok,    normal foreground)
    , ( StringTok,         normal green)
    , ( VerbatimStringTok, normal green)
    , ( SpecialStringTok,  normal green)
    , ( ImportTok,         normal purple)
    , ( CommentTok,        normal comment)
    , ( DocumentationTok,  normal comment)
    , ( AnnotationTok,     normal foreground)
    , ( CommentVarTok,     normal comment)
    , ( OtherTok,          normal aqua)
    , ( FunctionTok,       normal blue)
    , ( VariableTok,       normal red)
    , ( ControlFlowTok,    bold   purple)
    , ( OperatorTok,       normal aqua)
    , ( BuiltInTok,        bold   purple)
    , ( ExtensionTok,      bold   foreground)
    , ( PreprocessorTok,   normal comment)
    , ( AttributeTok,      normal blue)
    , ( RegionMarkerTok,   normal foreground)
    , ( InformationTok,    normal foreground)
    , ( WarningTok,        normal foreground)
    , ( AlertTok,          normal foreground)
    , ( ErrorTok,          normal foreground)
    , ( NormalTok,         normal foreground)
    ]
  , defaultColor              = foreground
  , backgroundColor           = background
  , lineNumberColor           = foreground
  , lineNumberBackgroundColor = background
  }
