module ParserAEBE where

import Text.Parsec (parserTrace)
import Text.ParserCombinators.Parsec
import Syntax

{-
For expressions E and B, it is only allowed to have white spaces inside the expressions.  In other
words, in the beginning and in the end of the expressions no white space is allowed.
-}


--START: Parser for E--
--function to test parseE 
testParseE :: String -> Either ParseError E
testParseE s = parse parseE "(unknown)" s

-- Parses a E expression which can have certain white spaces before and after
parseE :: GenParser Char st E
parseE = do
  entersOnly
  x <- parseESelect
  spacesAndEnters
  eof
  return x

-- parser of E expressions which "forwards" the input to another parser, which it selects according
-- to the input
parseESelect :: GenParser Char st E
parseESelect = try(parsePlus)
               <|> try(parseNum)
               <|> try(parseId)
               <|> parsePar

-- parses identifiers (which can only have alphabetic or numeric Unicode characters, or underscores)
parseId :: GenParser Char st E 
parseId = do
    notFollowedBy (reservedWord)
    i <- (try(startUnderscore) <|> startLetter) -- the identifier can either start with an underscore or with an alphabetic Unicode character  
    return (Id i) 

-- parses identifiers (which can only have alphabetic or numeric Unicode characters, or underscores)
parseIdeStr :: GenParser Char st String 
parseIdeStr = do
    notFollowedBy (reservedWord)
    i <- (try(startUnderscore) <|> startLetter) -- the identifier can either start with an underscore or with an alphabetic Unicode character  
    return i

-- parses a natural number (a sequence of digits)
parseNum :: GenParser Char st E 
parseNum = do
  minus <- option "" (string "-")  
  x <- many1 digit
  return (Num (read (minus ++ x) :: Integer))

-- previous version of parseNum without negative numbers
-- parseNum :: GenParser Char st E 
-- parseNum = do
--         x <- many1 digit
--         return (Nat (read x :: Integer))

-- parses E expressions between parentheses 
parsePar :: GenParser Char st E --1+(1+2); (1+(2))
parsePar = do
        x <- between (char '(') (char ')') parseESelect
        return x

-- parses a term in a Plus expression: either a natural number, or a variable, or a term inside
-- parentheses
termPlus :: GenParser Char st E
termPlus =  try(parseNum)  <|> try(parseId) <|> parsePar 

-- parser for Plus expressions
parsePlus :: GenParser Char st E
parsePlus = do
  x <- termPlus
  spaces
  char '+'
  spaces
  y <- termPlus
  loop (PlusE x y)
    where
      f x = do
        spaces
        char '+'
        spaces
        y <- termPlus
        spaces
        loop (PlusE x y)
      loop t = try(f t) <|> return t
--END: Parser for E--


--START: Parser for B--
-- function to test parseB 
testParseB :: String -> Either ParseError B
testParseB s = parse parseB "(unknown)" s

-- Parses a B expression which can have certain white spaces before and after
parseB :: GenParser Char st B
parseB = do
  entersOnly
  x <- parseBSelect
  spacesAndEnters
  eof
  return x

-- parser of B expressions which "forwards" the input to another parser, which it selects according
-- to the input
parseBSelect :: GenParser Char st B
parseBSelect = try(parseAnd)
               <|> try(parseNot)
               <|> try(parseEq)
               <|> try(parseLeq)
               <|> try(parseGeq)
               <|> try(parseLess)
               <|> try(parseGre)
               <|> try(parseTrue)
               <|> try(parseFalse)
               <|> parseParB

parseTrue :: GenParser Char st B
parseTrue = do
  string "true"
  return BTrue

parseFalse :: GenParser Char st B
parseFalse = do
  string "false"
  return BFalse  

-- parses a term in an And expression: either a negation, or a <=, or a true, or a false, or a term
-- inside parentheses
termAnd :: GenParser Char st B
termAnd =  try(parseNot)
           <|> try(parseEq)
           <|> try(parseLeq)
           <|> try(parseGeq)
           <|> try(parseLess)
           <|> try(parseGre)
           <|> try(parseTrue)
           <|> try(parseFalse)
           <|> parseParB

-- parser for And expressions
parseAnd :: GenParser Char st B
parseAnd = do
  x <- termAnd
  spaces
  char '&'
  spaces
  y <- termAnd
  loop (And x y)
    where
      f x = do
        spaces
        char '&'
        spaces
        y <- termAnd
        spaces
        loop (And x y)
      loop t = try(f t) <|> return t  

bNot :: GenParser Char st B
bNot =  termAnd

-- parsing a negation
parseNot :: GenParser Char st B
parseNot = do
  char '-'
  x <- bNot
  return (Not x)

-- parsing an equality 
parseEq :: GenParser Char st B
parseEq = do
  x <- parseESelect
  spacesOnly
  string "=="
  spacesOnly
  y <- parseESelect
  return (Equ x y)

-- parsing an inequality 
parseLeq :: GenParser Char st B
parseLeq = do
  x <- parseESelect
  spacesOnly
  string "<="
  spacesOnly
  y <- parseESelect
  return (Leq x y)

-- parsing an inequality 
parseGeq :: GenParser Char st B
parseGeq = do
  x <- parseESelect
  spacesOnly
  string ">="
  spacesOnly
  y <- parseESelect
  return (Geq x y)  

-- parsing an inequality 
parseLess :: GenParser Char st B
parseLess = do
  x <- parseESelect
  spacesOnly
  string "<"
  spacesOnly
  y <- parseESelect
  return (Less x y)

-- parsing an inequality 
parseGre :: GenParser Char st B
parseGre = do
  x <- parseESelect
  spacesOnly
  string ">"
  spacesOnly
  y <- parseESelect
  return (Gre x y)  

-- parsing B expressions inside parentheses
parseParB :: GenParser Char st B 
parseParB = do
  char '('
  spacesOnly
  x <- parseBSelect
  spacesOnly
  char ')'
  return x
--END: Parser for B--


--START: Parsers for the identifier--
-- reservedWord parses a reserved word ("skip","if","then","else","while","do","true" or "false"),
-- as long as it is followed by neither an alphabetic or numeric Unicode character nor an underscore
reservedWord :: GenParser Char st String
reservedWord = do
    x <- ( try (string "skip")
           <|> try (string "if")
           <|> try (string "then")
           <|> try (string "else")
           <|> try (string "while")
           <|> try (string "do")
           <|> try (string "true")
           <|> (string "false")
           <|> (string "await"))
    notFollowedBy (try(alphaNum) <|> parseUnderscore)
    return x

-- parses identifiers that start with an underscore
startUnderscore :: GenParser Char st String 
startUnderscore = do
    underscores <- many1 (parseUnderscore) -- "underscores" is a string with underscores only
    a <- alphaNum -- there cannot only be underscores in an identifier (a is an alphabetic or numeric Unicode character)
    b <- many (try(alphaNum) <|> parseUnderscore) -- "b" is either a string that can be composed of alphabetic and numeric Unicode characters and underscores, or an empty string
    return (underscores ++ (a:b))

-- parses identifiers that start with a letter
startLetter :: GenParser Char st String 
startLetter = do
    h <- letter -- h is an alphabetic Unicode character (which is a letter)
    t <- many (try(alphaNum) <|> parseUnderscore) -- "t" is either a string that can be composed of alphabetic and numeric Unicode characters and underscores, or an empty string
    return (h:t)

-- parses underscores only
parseUnderscore :: GenParser Char st Char 
parseUnderscore = satisfy (isUnderscore)

-- returns True iff it receives an underscore
isUnderscore :: Char -> Bool 
isUnderscore c = if (c=='_') then True else False
--END: Parsers for the identifier--


--START: Others parsers for spaces--
-- Functions for defining white spaces between elements of the language

--separateOrJoined parses 0 or more spaces followed by 0 or more '\n' characters
separateOrJoined :: GenParser Char st String 
separateOrJoined = do
  spacesOnly
  entersOnly

-- parses 1 or more spaces followed by 0 or more '\n', OR parses 1 or more '\n'
separateElems :: GenParser Char st String
separateElems = try(atLeastOneSpace >> entersOnly) <|> atLeastOneEnter

-- parses 1 or more spaces
atLeastOneSpace :: GenParser Char st String 
atLeastOneSpace = many1 parseSpace

-- parses 1 or more '\n' characters
atLeastOneEnter :: GenParser Char st String 
atLeastOneEnter = many1 parseEnter

-- parses 0 or more characters that are either a space or a '\n' character
spacesAndEnters :: GenParser Char st String 
spacesAndEnters = many parseSpaceOrEnter

-- succeeds for space characters and '\n' characters
parseSpaceOrEnter :: GenParser Char st Char 
parseSpaceOrEnter = satisfy isSpaceOrEnter

isSpaceOrEnter :: Char -> Bool
isSpaceOrEnter c = (c == ' ' || c == '\n')

-- parses 0 or more spaces
spacesOnly :: GenParser Char st String 
spacesOnly = many parseSpace

-- parses 0 or more '\n' characters
entersOnly :: GenParser Char st String 
entersOnly = many parseEnter

-- succeeds for space characters (' ')
parseSpace :: GenParser Char st Char 
parseSpace = satisfy isSpace

isSpace :: Char -> Bool
isSpace c = (c == ' ')

-- succeeds for '\n' characters
parseEnter :: GenParser Char st Char 
parseEnter = satisfy isEnter

isEnter :: Char -> Bool
isEnter c = (c == '\n')
--END: Others parsers for spaces--

