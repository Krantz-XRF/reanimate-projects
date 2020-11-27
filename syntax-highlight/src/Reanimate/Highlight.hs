{-|
Module      : Reanimate.Highlight
Description : Syntax highlighting support for Reanimate.
Copyright   : (c) Xie Ruifeng, 2020
License     : AGPL-3-or-later
Maintainer  : krantz.xrf@outlook.com
Stability   : experimental
Portability : portable
-}
module Reanimate.Highlight (module Reanimate.Highlight.Skylighting) where

import Reanimate.Highlight.Skylighting (defaultTokenizerConfig, highlight,
                                        highlightHs, highlightWith)
