{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FlexibleContexts #-}
module Lib2 where

import Control.Applicative (liftA2, (<|>))
import Data.Monoid ((<>))
import qualified Text.Parsec as P

-- lexeme :: P.Parsec String () a -> P.Parsec String () (a, String)
-- lexeme p = do
--   a <- p
--   spc <- space
--   return (a, spc)

spacing :: String
spacing = " \t"

white :: String
white = "\n" ++ spacing

mathSpanSymbol :: P.Parsec String () String
mathSpanSymbol = P.string "%"

eof :: P.Parsec String () String
eof = P.eof *> pure ""

oneChar :: Char -> P.Parsec String () String
oneChar c = wrap (P.char c)

data Bit
  = Plain String
  | Span String
  | Div String
  deriving (Show)

data Lines
  = PlainL String
  | MathL String
  deriving (Show)

plainLexeme :: P.Parsec String () Bit
plainLexeme = fmap Plain p
  where
    p = P.satisfy (/= '%')
      <:> P.many (P.noneOf white)
      <+> (P.try (P.many1 (P.oneOf spacing)) <|> eof)
      <+> (P.try p <|> pure "")

escaped :: P.Parsec String () String
escaped = P.char '\\' <:> P.anyChar <:> pure "" -- ) *> pure "waaa"

mathSpan :: P.Parsec String () Bit
mathSpan = fmap Span p
  where
    p = (P.char '%' *> pure '&')
      <:> inside
      <+> (P.try (P.many1 (P.oneOf spacing)) <|> eof)
      -- <+> (P.try p <|> pure "")
    inside = let
        it = escaped
          <|> P.many1 (P.noneOf $ "[\\" ++ white)
          <|> brackets
      in
        it <+> (P.try inside <|> pure "")

brackets :: P.Parsec String () String
brackets = (P.char '[' *> pure '{')
    <:> inside
  where
    inside = ((escaped <|> brackets <|> wrap (P.noneOf "]\\"))
      <+> P.try inside)
      <|> wrap (P.char ']' *> pure '}')

mathDiv :: P.Parsec String () Bit
mathDiv = fmap Div p
  where
    p = starter <+> inside
    inside = P.try ender <|> (q <+> inside)
    q = oneChar '\n'
      <|> P.many1 (P.noneOf "\n")

    starter = P.string "\n%___"
      <+> P.many (P.oneOf spacing)
    ender = P.string "\n___%"
      <+> P.many (P.oneOf spacing)
      <+> oneChar '\n'
      <+> P.many (P.oneOf spacing)

wrap :: Monad m => m a -> m [a]
wrap = fmap (:[])

inside :: P.Parsec String () [Bit]
inside = (P.try mathDiv
    <|> fmap Plain (oneChar '\n')
    <|> plainLexeme
    <|> mathSpan)
  <:> (P.try inside <|> pure [])

file' :: P.Parsec String () [Bit]
file' =
  fmap Plain (P.many (P.oneOf spacing))
  <:> inside
  

(<+>) :: (Monad m, Monoid o) => m o -> m o -> m o
ma <+> mb = liftA2 (<>) ma mb
infixr 6 <+>

(<:>) :: (Monad m) => m a -> m [a] -> m [a]
ma <:> mas = liftA2 (:) ma mas

infixr 5 <:>


eol :: P.Parsec String () String
eol = oneChar '\n' <|> eof

oneLine :: P.Parsec String () String
oneLine
  = P.try (P.many1 (P.noneOf "\n")
    <+> eof)
  <|> (P.many (P.noneOf "\n")
    <+> oneChar '\n')

mathLines :: P.Parsec String () Lines
mathLines = fmap MathL p
  where
    p = start
      <+> (end <|> inside)
    start = P.many (P.oneOf spacing)
      <+> P.string "%..."
      <+> P.many (P.oneOf spacing)
      <+> oneChar '\n'
    inside = oneLine <+> (end <|> inside)
    end = P.many (P.oneOf spacing)
      <+> P.string "...%"
      <+> P.many (P.oneOf spacing)
      <+> eol

plainLine :: P.Parsec String () Lines
plainLine = fmap PlainL oneLine

preparser :: P.Parsec String () [Lines]
preparser = (P.try mathLines <|> plainLine)
  <:> ((eof *> pure []) <|> preparser)

linesToLatex :: [Lines] -> Either   P.ParseError String
linesToLatex [] = Right ""
linesToLatex (MathL x:rest) =
    P.parse mathParser "_" x <+> linesToLatex rest
linesToLatex (PlainL x:rest) =
    P.parse plainParser "_" x <+> linesToLatex rest

plainParser :: P.Parsec String () String
plainParser = P.many (P.oneOf spacing)
    <+> inside
  where
    inside = (eol
        <|> plainLexeme'
        <|> mathSpan')
      <+> inside

plainLexeme' :: P.Parsec String () String
plainLexeme' =  P.satisfy (/= '%')
    <:> P.many (P.noneOf white)
    <+> (P.try (P.many1 (P.oneOf spacing)) <|> eol)

mathSpan' :: P.Parsec String () String
mathSpan' =
    (P.char '%' *> pure '&')
    <:> inside
    <+> (P.try (P.many1 (P.oneOf spacing)) <|> eol)
  where
    inside = let
        it = escaped
          <|> P.many1 (P.noneOf $ "[\\" ++ white)
          <|> brackets
      in
        it <+> (P.try inside <|> pure "")

mathParser = undefined

{-
oneFile :: GenParser Char st [String]
oneFile = do
  bef <- untouched
  maybeIt <- optionMaybe special
  rest <- oneFile <|> pure []
  pure $ maybe
    (bef : rest)
    (\it -> bef : it : rest)
    maybeIt

untouched :: GenParser Char st String
untouched = many1 (noneOf "%")
  -- before <- many1 (noneOf "%")
  -- if last before != ' '
    -- then char '%'

special :: GenParser Char st String
special = App.liftA2 (:) (char '%') (many (noneOf " \n\t"))

eol :: GenParser Char st Char
eol = char '\n'
-}

-- doParse :: String -> Either ParseError [[String]]
-- doParse input = parse csvFile "(unknown)" input

replace :: Char -> String -> P.Parsec String () String
replace c str = P.char c *> pure str

a''z = ['a'..'z'] :: String

literal :: P.Parsec String () String
literal = P.char '@' *> P.many1 (P.oneOf a''z)

word :: P.Parsec String () String
word = fmap f (P.many1 $ P.oneOf a''z)
  where
    f [c] = c : [] -- is literal letter
    f str = '\\' : str -- is command

-- symbol :: P.Parsec String () String
-- symbol = wrap P.anyChar

toLatex :: P.Parsec String () String
toLatex = P.many it
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

test = "test\ntest\naa"

