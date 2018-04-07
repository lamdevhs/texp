{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FlexibleContexts #-}
module Lib where

import Control.Applicative (liftA2, (<|>))
import Data.Monoid ((<>), mempty)
import Data.Foldable (foldMap)


import qualified Text.Parsec as P
import qualified Data.Text as T
import qualified Data.Text.IO as T

------------------
-- UTILITIES

(<:>) :: (Monad m) => m a -> m [a] -> m [a]
ma <:> mas = liftA2 (:) ma mas
infixr 5 <:>

feed :: P.Parsec s u b -> s -> P.Parsec s u b
feed p x = P.setInput x *> p

--

eof :: Monoid m => P.Parsec String u m
eof = P.eof *> pure mempty

notEOF :: P.Parsec String u Char
notEOF = P.lookAhead P.anyChar

eol :: P.Parsec String u String
eol = oneChar '\n' <|> eof

oneChar :: Char -> P.Parsec String u String
oneChar c = wrap (P.char c)

wrap :: Monad m => m a -> m [a]
wrap = fmap (:[])

manyOf :: String -> P.Parsec String u String
manyOf = P.many . P.oneOf

moreOf :: String -> P.Parsec String u String
moreOf = P.many1 . P.oneOf

manyNoneOf :: String -> P.Parsec String u String
manyNoneOf = P.many . P.noneOf

moreNoneOf :: String -> P.Parsec String u String
moreNoneOf = P.many1 . P.noneOf

manycat :: Monoid a => P.Parsec String u a -> P.Parsec String u a
manycat p = fmap mconcat (P.many p)

morecat :: Monoid a => P.Parsec String u a -> P.Parsec String u a
morecat p = fmap mconcat (P.many1 p)

--

a''z = ['a'..'z'] ++ ['A'..'Z']

spacing :: String
spacing = " \t"

white :: String
white = '\n' : spacing

escaped :: P.Parsec String u String
escaped = P.char '\\' <:> wrap (P.noneOf "\n")

oneLine :: P.Parsec String u String
oneLine = P.try (moreNoneOf "\n" <> eol)
  -- <|> (manyNoneOf "\n" <> oneChar '\n')
  <|> oneChar '\n'

------------------
------------------

data LineType
  = Plain String
  | MathBlock String
  | MathSpan String
  deriving (Show)

foo :: (Monoid (m o), Monad m, Monoid o) => (a -> m o) -> m [a] -> m o
foo k mas = mas >>= foldMap k


parser :: P.Parsec String u String
parser = divider >>= foldMap fp
  where
    fp (Plain x) = pure x
    fp (MathBlock x) = feed (latexBlock toLatex) x
    fp (MathSpan x) = feed (latexSpan toLatex) x

-- mathParser :: P.Parsec String u String
-- mathParser = 

latexBlock :: P.Parsec s u String -> P.Parsec s u String
latexBlock p = pure "\\[" <> p <> pure "\\]\n"

latexSpan :: P.Parsec s u String -> P.Parsec s u String
latexSpan p = pure "$\\:" <> p <> pure "\\:$"

--

divider :: P.Parsec String u [LineType]
divider = manycat (P.try mathBlock <|> normalLine)
  -- <> (eof <|> divider)

normalLine :: P.Parsec String u [LineType]
normalLine = notEOF
    *> (fmap Plain (manyOf spacing)
    <:> manycat p
    <> wrap (fmap Plain eol))
  where
    p = P.try (fmap MathSpan mathSpan
        <:> wrap (fmap Plain (manyOf spacing)))
      <|> wrap (fmap Plain plainLexeme)

    -- plainParser :: P.Parsec String u String
    -- plainParser = manyOf spacing <> morecat inside
    --   where
    --     inside = eol
    --         <|> plainLexeme
    --         <|> mathSpan

plainLexeme :: P.Parsec String u String
plainLexeme =  P.noneOf "\n"
    <:> manyNoneOf white
    <> manyOf spacing

-- mathSpan = undefined
mathSpan :: P.Parsec String u String
mathSpan = P.char '%'
    *> manycat inside
  where
    inside = moreNoneOf ("[" ++ white)
      <|> brackets

brackets :: P.Parsec String u String
brackets = P.char '['
    <:> manycat inside
    <> oneChar ']'
  where
    inside = brackets <|> wrap (P.noneOf "]")

mathBlock :: P.Parsec String u [LineType]
mathBlock = wrap (fmap MathBlock p)
  where
    p = start <> (P.try end <|> inside)
    start = manyOf spacing
      <> P.string "%..."
      *> manyOf spacing
      <> oneChar '\n'
    inside = oneLine <> (P.try end <|> inside)
    end = manyOf spacing
      <> P.string "...%"
      *> manyOf spacing
      <* eol

--

toLatex :: P.Parsec String u String
toLatex = manycat it
  where
    it = escaped
      <|> replace '{' "\\{"
      <|> replace '}' "\\}"
      <|> replace '[' "{"
      <|> replace '.' "_"
      <|> replace '_' "."
      <|> replace ']' "}"
      <|> replace ']' "}"

      <|> P.try literal
      <|> word
      <|> wrap P.anyChar

replace :: Char -> String -> P.Parsec String u String
replace c str = P.char c *> pure str

literal :: P.Parsec String û String
literal = P.char '@' *> P.many1 (P.oneOf a''z)

word :: P.Parsec String u String
word = fmap f (P.many1 $ P.oneOf a''z)
  where
    f [c] = c : [] -- is literal letter
    f str = '\\' : str -- is command

parseFile :: FilePath -> FilePath -> IO ()
parseFile input output = do
  contents <- fmap T.unpack (T.readFile input)
  case P.parse parser "~file~" contents of
    Left e -> putStr $ show e
    Right result -> do
      T.writeFile output (T.pack result)


---- ###############################
---- ###############################
---- ###############################

-- lala :: P.Parsec String () String
-- lala = do
--     xs <- bar
--     -- input <- P.getInput
--     res <- foldMap f xs
--     -- P.setInput input
--     pure res

--   where
--     bar = P.many (P.string "ab" <|> P.string "cd")
--     bla = manyOf "a"
--     blo = manyOf "c"
--     f "ab" = feed bla "ab"
--     f "cd" = feed blo "cd"