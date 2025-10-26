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

testParseId :: String -> Either ParseError E
testParseId s = parse parseId "(unknown)" s

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
parseESelect = try(parseMultDiv)
               <|> try(parsePlusMinus)
               <|> try(parseNum)
               <|> try(parseId)
               <|> try(parseMod)
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

-- parses an integer number (a sequence of digits)
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
  char '('
  spaces
  x <- parseESelect
  spaces
  char ')'
  return x

-- parses a term in a Plus or Minus expression: either an integer number, or a variable, or a term inside
-- parentheses
termPlusMinus :: GenParser Char st E
termPlusMinus = try(parseNum)
                <|> try(parseId)
                <|> parsePar 

-- parser for Plus or Minus expressions
parsePlusMinus :: GenParser Char st E
parsePlusMinus = do
  x <- termPlusMinus
  spaces
  op <- try(char '+') <|> (char '-')
  spaces
  y <- termPlusMinus
  case op of
    '+' -> loop (PlusE x y)
    '-' -> loop (MinusE x y)
    where
      f x = do
        spaces
        op <- try(char '+') <|> (char '-')
        spaces
        y <- termPlusMinus
        spaces
        case op of
          '+' -> loop (PlusE x y)
          '-' -> loop (MinusE x y)
      loop t = try(f t) <|> return t

-- parses a term in a Mult or Div expression: either an integer number, or a variable, or a term inside
-- parentheses
termMultDiv :: GenParser Char st E
termMultDiv =  try(parsePlusMinus)
               <|> try(parseNum)
               <|> try(parseId)
               <|> parsePar 

-- parser for Mult or Div expressions
parseMultDiv :: GenParser Char st E
parseMultDiv = do
  x <- termMultDiv
  spaces
  op <- try(char '*') <|> (char '/')
  spaces
  y <- termMultDiv
  case op of
    '*' -> loop (MultE x y)
    '/' -> loop (DivE x y) -- <----- atencao à possivel divisao por zero
    where
      f x = do
        spaces
        op <- try(char '*') <|> (char '/')
        spaces
        y <- termMultDiv
        spaces
        case op of
          '*' -> loop (MultE x y)
          '/' -> loop (DivE x y)
      loop t = try(f t) <|> return t

-- parser for modular operations
parseMod :: GenParser Char st E
parseMod = do
  _  <- string "mod"
  spaces
  _ <- char '('
  spaces
  n <- many1 digit
  spaces
  _ <- char ','
  spaces
  aexp <- parseESelect
  spaces
  _ <- char ')'
  return (ModE (read n :: Integer) aexp)
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
parseBSelect = try(parseAndOr)
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
termAndOr :: GenParser Char st B
termAndOr =  try(parseNot)
           <|> try(parseEq)
           <|> try(parseLeq)
           <|> try(parseGeq)
           <|> try(parseLess)
           <|> try(parseGre)
           <|> try(parseTrue)
           <|> try(parseFalse)
           <|> parseParB

-- parser for And expressions
parseAndOr :: GenParser Char st B
parseAndOr = do
  x <- termAndOr
  spaces
  op <- try(char '&') <|> (char '|')
  spaces
  y <- termAndOr
  case op of
    '&' -> loop (And x y)
    '|' -> loop (OrB x y)
    where
      f x = do
        spaces
        op <- try(char '&') <|> (char '|')
        spaces
        y <- termAndOr
        spaces
        case op of
          '&' -> loop (And x y)
          '|' -> loop (OrB x y)
      loop t = try(f t) <|> return t  

bNot :: GenParser Char st B
bNot =  termAndOr

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
-- parseParB :: GenParser Char st B 
-- parseParB = do
--   x <- between (char '(') (char ')') parseBSelect
--   return x
parseParB :: GenParser Char st B 
parseParB = do
  char '('
  spaces
  x <- parseBSelect
  spaces
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
           <|> (string "await")
           <|> (string "mod")
         )
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



--START: Parsers for Arg---
--function to test parseArg
testParseArgSelect :: String -> Either ParseError Arg
testParseArgSelect s = parse parseArgSelect "(unknown)" s

-- parser of P expressions which "forwards" the input to another parser, which it selects according
-- to the input
parseArgSelect :: GenParser Char st Arg
parseArgSelect = try(parseMultDivArg)
                 <|> try(parsePlusMinusArg)
                 <|> try(parseNumArg)
                 <|> try(parsePi)
                 <|> try(parseSqrt)
                 <|> parseParArg

-- parses an integer number (a sequence of digits)
parseNumArg :: GenParser Char st Arg
parseNumArg = do
  minus <- option "" (string "-")  
  x <- many1 digit
  dot <- option "" (string ".")
  y <- option "" (many1 digit)
  case dot of
    "." -> return (NumP (read (minus ++ x ++ dot ++ y) :: Double))
    otherwise -> return (NumP (read (minus ++ x) :: Double))

-- parses pi
parsePi :: GenParser Char st Arg
parsePi = do
  _ <- string "pi"
  return PiP

-- parses sqrt
parseSqrt :: GenParser Char st Arg
parseSqrt = do
  _ <- string "sqrt"
  spacesOnly
  char '('
  spacesOnly
  exp <- parseArgSelect
  spacesOnly
  char ')'
  return (SqrtP exp)
  

-- parses Arg expressions between parentheses 
parseParArg :: GenParser Char st Arg --1+(1+2); (1+(2))
parseParArg = do
  char '('
  spaces
  x <- parseArgSelect
  spaces
  char ')'
  return x
-- parseParArg :: GenParser Char st Arg --1+(1+2); (1+(2))
-- parseParArg = do
--         x <- between (char '(') (char ')') parseArgSelect
--         return x

-- parses a term in a Plus or Minus expression: either an integer number, or a variable, or a term
-- inside parentheses
termPlusMinusArg :: GenParser Char st Arg
termPlusMinusArg = try(parseNumArg)
                   <|> try(parsePi)
                   <|> try(parseSqrt)
                   <|> parseParArg 

-- parser for Plus or Minus expressions
parsePlusMinusArg :: GenParser Char st Arg
parsePlusMinusArg = do
  x <- termPlusMinusArg
  spaces
  op <- try(char '+') <|> (char '-')
  spaces
  y <- termPlusMinusArg
  case op of
    '+' -> loop (PlusP x y)
    '-' -> loop (MinusP x y)
    where
      f x = do
        spaces
        op <- try(char '+') <|> (char '-')
        spaces
        y <- termPlusMinusArg
        spaces
        case op of
          '+' -> loop (PlusP x y)
          '-' -> loop (MinusP x y)
      loop t = try(f t) <|> return t

-- parses a term in a Mult or Div expression: either an integer number, or a variable, or a term inside
-- parentheses
termMultDivArg :: GenParser Char st Arg
termMultDivArg =  try(parsePlusMinusArg)
                  <|> try(parseNumArg)
                  <|> try(parsePi)
                  <|> try(parseSqrt)
                  <|> parseParArg

-- parser for Mult or Div expressions
parseMultDivArg :: GenParser Char st Arg
parseMultDivArg = do
  x <- termMultDivArg
  spaces
  op <- try(char '*') <|> (char '/')
  spaces
  y <- termMultDivArg
  case op of
    '*' -> loop (MultP x y)
    '/' -> loop (DivP x y) -- <----- atencao à possivel divisao por zero
    where
      f x = do
        spaces
        op <- try(char '*') <|> (char '/')
        spaces
        y <- termMultDivArg
        spaces
        case op of
          '*' -> loop (MultP x y)
          '/' -> loop (DivP x y)
      loop t = try(f t) <|> return t
--END: Parsers for Arg---


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

