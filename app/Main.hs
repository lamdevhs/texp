{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FlexibleContexts #-}
module Main where

-- import Control.Applicative (liftA2, (<|>))
-- import Data.Monoid ((<>))
-- import qualified Text.Parsec as P

import System.Environment (getArgs)

import Lib

main :: IO ()
main = do
  (input:output:_) <- getArgs
  parseFile input output