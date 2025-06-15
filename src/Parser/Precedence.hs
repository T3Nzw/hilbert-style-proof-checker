{-# LANGUAGE MultiParamTypeClasses #-}

-- precedence parsing combinators using Pratt parsing

-- resources:
-- https://matthewmanela.com/blog/a-monadic-pratt-parser/
-- https://matklad.github.io/2020/04/13/simple-but-powerful-pratt-parsing.html
-- great video: https://www.youtube.com/watch?v=0c8b7YfsBKs

module Precedence where

import Control.Applicative ((<|>))
import Parser

-- TODO implement precedence parsing
