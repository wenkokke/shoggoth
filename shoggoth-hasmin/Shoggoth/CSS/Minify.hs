{-# OPTIONS_GHC -fno-warn-orphans #-}

module Shoggoth.CSS.Minify
  ( minifyCSS,
    minifyCSSWith,
    minifyCssDefaultConfig,
    Hasmin.Config (..),
    Hasmin.ColorSettings (..),
    Hasmin.DimensionSettings (..),
    Hasmin.GradientSettings (..),
    Hasmin.FontWeightSettings (..),
    Hasmin.LetterCase (..),
    Hasmin.SortingMethod (..),
    Hasmin.RulesMergeSettings (..),
  )
where

import Shoggoth.Prelude (Action, liftEither)
import Data.Text (Text)
import Hasmin qualified

minifyCssDefaultConfig :: Hasmin.Config
minifyCssDefaultConfig = Hasmin.defaultConfig

-- | Minify CSS using 'Hasmin'.
minifyCSS :: Text -> Action Text
minifyCSS = minifyCSSWith Hasmin.defaultConfig

-- | Minify CSS with options.
minifyCSSWith :: Hasmin.Config -> Text -> Action Text
minifyCSSWith opts css = liftEither id (Hasmin.minifyCSSWith opts css)
