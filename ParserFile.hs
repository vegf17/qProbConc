{-
Parser of the concurrent quantum language,
C = Skip 
    | x:=e
    | Reset(q)
    | U(q1, ..., qn)
    | Meas(c, q) 
    | C;C 
    | C or C 
    | C || C 
    | C +(p) C 
    | if b then {C} else {C} 
    | while b do {C}
    | await b do {C}
    | Atom(C)
and using left associativity and no auxiliary data types.

-}


module ParserFile where

import Text.Parsec (parserTrace)
import Text.ParserCombinators.Parsec -- import functions from Parsec (this module exports modules Text.ParserCombinators.Parsec.Prim, Text.ParserCombinators.Parsec.Combinator and Text.ParserCombinators.Parsec.Char)
import Text.Parsec.String (Parser)
import Data.Matrix hiding((<|>))
import Data.Complex

import Syntax
--import ParserBE_Brookes -- module created in ParserBE_Brookes.hs
import ParserAEBE
import ParserCom
import QuantumCalc

{-
Important functions of this module:
· main1 : parses to C a file with only one program

· main: parses to C a file with one or more programs

· mainRun: parses to Config a file with one or more programs  

· parseInputC :: String -> Either ParseError C
  (parseInputC s) where:
  - s is a string corresponding to a command
  returns either an error or the parsing of s

· parseConf ::  String -> Either ParseError (C,SC,L,SQ)
  (parseConf s) where:
  - s is a string corresponding to a configuration
  returns either an error or the parsing of s
-}

--START: Test parsers--
-- test parser for Doubles
testParserDouble :: Parser Double -> String -> Either ParseError Double
testParserDouble parser input = parse parser "(unknown)" input

--test parser for classical states
testpCS :: String -> Either ParseError SC
testpCS input = parse pClassicStates "(unknown)" input

-- test parser for quantum states
testpOpDen :: String -> Either ParseError SQ
testpOpDen input = parse pOpDen "(unknown)" input

--test parser for complex numbers
testpComplex :: String -> Either ParseError (Complex Double)
testpComplex input = parse pComplex "(unknown)" input

--test parser for configurations
-- <command, classical states, linking function, quantum states>
-- <skip, [(x,0)], [(q,1)], (1.0 + i0.0)|0><0|>
parseConf ::  String -> Either ParseError (C,SC,L,SQ)
parseConf input = parse pConf "(unknown)" input

-- function to test the parser splitRun
parseRun :: String -> Either ParseError [((String, (Int, [[String]]), Int), (C,SC,L,SQ))]
parseRun input = parse splitRun "(unknown)" input
--END: Test parsers--

--START: Read from a file--
-- parses to C a file with only one program
main1 = do
    putStr "Name of the file to be parsed: " -- print this string to the terminal
    fileName <- getLine -- get name of the file to be read
    program <- readFile (fileName ++ ".txt") -- "program" is a String containing the program in fileName
    print (parseInputC program)-- apply to "program" function parseInputC and print the result

-- parses to C a file with one or more programs  
mainC :: IO ()
mainC = do
  putStr "Name of the file to be parsed: "  
  fileName <- getLine                      
  text <- readFile (fileName ++ ".txt")    
  case parse splitProgs "(unknown)" text of
    Left err -> print err  -- Print error if parsing fails
    Right progs -> mapM_ print progs

-- parses to Config a file with one or more programs
mainRun :: String -> IO ()
mainRun path = do
  fileContent <- readFile path                      
  case parseRun fileContent of
    Left err -> print err  -- Print error if parsing fails
    Right progs -> mapM_ print progs
    --Right progs -> putStrLn "success"
    


-- mainRun :: IO ()
-- mainRun = do
--   putStr "Name of the file to be parsed: "  
--   fileName <- getLine                      
--   text <- readFile (fileName ++ ".txt")    
--   case parse splitRun "(unknown)" text of
--     Left err -> print err  -- Print error if parsing fails
--     Right progs -> mapM_ print progs    

-- separates different programs and returns a list of C
splitProgs :: GenParser Char st [C]
splitProgs = do
  string "---v---"
  char '\n'
  c <- parseCSelect -- type C
  separateOrJoined
  string "---v---"
  separateOrJoined
  listC <- option [] splitProgs -- type [C]
  return (c:listC)

-- separates different programs and returns a list of ((rep hist, k, name),(prog, sc, l, sq))
splitRun :: GenParser Char st [((String, (Int, [[String]]), Int), (C,SC,L,SQ))]
splitRun = do
  string "---"
  name <- many1 alphaNum
  string "---"
  char '\n'
  string "hist:"
  spacesOnly
  (rep, llvar) <- parseHist
  char '\n'
  string "k:"
  spacesOnly
  k <- many1 digit
  char '\n'
  (c,sc,l,sq) <- pConf
  separateOrJoined
  string "---"
  many1 alphaNum
  string "---"
  separateOrJoined
  listRun <- option [] splitRun -- type [(C,SC,L,SQ)]
  return (((name, (rep, llvar), read k :: Int),(c,sc,l,sq)):listRun)
--END: Read from a file--


---START: Parsers for States----
-- parses classical states
-- regular expression for parsing classical states:
-- [(\(var,int\))((,) (\n)* \(var,int\))*] | []
pClassicStates :: GenParser Char st SC
pClassicStates = do
  cs <- option [] (between (char '[') (char ']') (sepBy state separator))
  return cs
  where state = do
          char '('
          x <- parseIdeStr
          char ','
          v <- parseNum
          char ')'
          return (x, rmvNat v)
        separator = char ',' <* spaces  -- allows spaces/newlines after `,`
{-
since Parsec is a monad, it is part of the Applicative instance, which has the operators *> and <*;
<* keeps only the first result; hence it ignores the spaces that appear after the comma
-}

-- remove the constructor Nat from a number
rmvNat :: E -> Integer
rmvNat (Num n) = n
rmvNat x = error "did not receive a Num n"

--Parser for Linking function (is there a way to automatize this?)--
parserL :: GenParser Char st L
parserL = do
  l <- option [] (between (char '[') (char ']') (sepBy state separator))
  return l
  where state = do
          char '('
          q <- parseQVar
          char ','
          v <- parseNum
          char ')'
          return (q, fromIntegral (rmvNat v) :: Int)
        separator = char ',' <* spaces  -- allows spaces/newlines after `,`  

--parses quantum states (density operators)
-- regular expression for parsing quantum states:
--  (c \|(0|1)+ \> \<(01)+\|)(\+ (\n)* c \|(0|1)+ \> \<(01)+\|)*
--  |
--  c \( (\|(0|1)+ \> \<(01)+\|) (\+ (\n)* \|(0|1)+ \> \<(01)+\|)+ \)
--  |
--  c
-- where c is a complex number
pQuantumStates :: GenParser Char st SQ
pQuantumStates = try(pOpDen1) <|> pOpDen2

-- parses a quantum state following this regular expression:
--for (complex \|(0|1)+ \> \<(01)+\|)(\+ (\n)* complex \|(0|1)+ \> \<(01)+\|)*
-- intuition: parses a density operator with a complex in each state
pOpDen1 :: GenParser Char st SQ
pOpDen1 = do
  complex <- between (char '(') (char ')') pComplex
  char '|'
  bin1 <- many1 (oneOf "01")
  char '>'
  char '<'
  bin2 <- many1 (oneOf "01")
  char '|'
  loop (scaleMatrix complex (tensorProduct [stringToSQ bin1, dagger $ stringToSQ bin2]))
    where
      f x = do
        spacesOnly
        char '+'
        spaces
        complex1 <- between (char '(') (char ')') pComplex
        char '|'
        bin3 <- many1 (oneOf "01")
        char '>'
        char '<'
        bin4 <- many1 (oneOf "01")
        char '|'
        loop (sumMatrices x (scaleMatrix complex1 (tensorProduct [stringToSQ bin3, dagger $ stringToSQ bin4])))
      loop t = try(f t) <|> return t

-- parses a quantum state following this regular expression:
-- c \( (\|(0|1)+ \> \<(0|1)+\|) (\+ (\n)* \|(0|1)+ \> \<(0|1)+\|)+ \)
-- where c is a complex number
-- intuition: parses a density operator multiplied all by the same complex
pOpDen2 :: GenParser Char st SQ
pOpDen2 = do
  complex <- between (char '(') (char ')') pComplex
  opDen <- between (char '(') (char ')') pOpDen2Aux
  return (scaleMatrix complex opDen)

-- parser for the regular expression:
-- \|(0|1)+ \> \<(0|1)+\|) (\+ (\n)* \|(0|1)+ \> \<(0|1)+\|
-- intuition: parses a density operator 
pOpDen2Aux :: GenParser Char st SQ
pOpDen2Aux = do
  char '|'
  bin1 <- many1 (oneOf "01")
  char '>'
  char '<'
  bin2 <- many1 (oneOf "01")
  char '|'
  loop (tensorProduct [stringToSQ bin1, dagger $ stringToSQ bin2])
    where
      f x = do
        spacesOnly
        char '+'
        spaces
        char '|'
        bin3 <- many1 (oneOf "01")
        char '>'
        char '<'
        bin4 <- many1 (oneOf "01")
        char '|'
        loop (sumMatrices x (tensorProduct [stringToSQ bin3, dagger $ stringToSQ bin4]))
      loop t = try(f t) <|> return t

-- given a binary string, returns its associated density operator
-- example: stringToSQ "01" returns a column vector corresponding to [0,1,0,0]
stringToSQ :: String -> SQ
stringToSQ s
  | (length s == 1) = char01ToSQ $ head s
  | (length s > 1) = tensorProduct $ map (\e -> char01ToSQ e) s
  | otherwise = fromLists [[]]

-- returns the vector |0> or |1> if the char is '0' or '1', respectively
char01ToSQ :: Char -> SQ
char01ToSQ c 
  | c=='0' = fromLists [[1.0 :+ 00],[0.0 :+ 0.0]]
  | c=='1' = fromLists [[0.0 :+ 00],[1.0 :+ 0.0]]

-- parser for density operators
-- if the string is empty, we return an empty matrix
pOpDen :: GenParser Char st SQ
pOpDen = option (fromLists [[]]) (try(pOpDen1) <|> pOpDen2)


{-Some expressions and its respective output when using pComplex with testpComplex
"1" ++ string --> 1.0 :+ 0.0
"2i" ++ string --> 0.0 :+ 2.0
"1.23 + -(2.3)i" --> 1.23 :+ 0.0
"1.23 + (-2.3)i" --> 1.23 :+ (-2.3)
"1.23 + -2.3i" --> 1.23 :+ (-2.3)
"-(1.23) + -2.3i" --> error
"(-1.23) + -2.3i" --> (-1.23) :+ (-2.3)
"1.23 + -2.3i" ++ string --> 1.23 :+ (-2.3)
-}
-- parses complex numbers following this regular expression:
{-
real1 (\. real2)? \+ (\n)* imag1 (\. imag2)? i
|
imag1 (\. imag2)? i
|
real1 (\. real2)?
-}
-- 
pComplex :: GenParser Char st (Complex Double)
pComplex = try(pRealImg) <|> try(pImgOnly) <|> pRealOnly

--parser a complex number with both real and imaginary parts
pRealImg :: GenParser Char st (Complex Double)
pRealImg = do
  real <- pNumParen --Double
  spacesOnly
  char '+'
  separateOrJoined
  img <- pNumParen
  char 'i'
  return (real :+ img)

--parses a complex number only with the imaginary part
pImgOnly :: GenParser Char st (Complex Double)
pImgOnly = do
  img <- pNumParen
  char 'i'
  return (0.0 :+ img)

--parses a complex number composed only by the real part
pRealOnly :: GenParser Char st (Complex Double)
pRealOnly = do
  real <- pNumParen
  return (real :+ 0.0)
  
--parser for doubles, which can be negative
pDoubNeg :: GenParser Char st Double
pDoubNeg = do
  minus <- option "" (string "-")
  num <- many1 digit
  dec <- option "" pDec
  return (read (minus ++ num ++ dec) :: Double)

--parser for decimal numbers, i.e., given a number 1.23 we want to parse .23
pDec :: GenParser Char st String
pDec = do
  char '.'
  numb <- many1 digit
  return ('.' : numb)

--parsers number possibly between parentheses
pNumParen :: GenParser Char st Double
pNumParen = try(between (char '(') (char ')') pDoubNeg) <|> pDoubNeg

{-PREVIOUS VERSION OF pComplex
-- parses complex numbers following this regular expression:
-- real1 \. real2 \+ (\n)* (i) imag1 \. imag2
pComplex :: GenParser Char st (Complex Double)
pComplex = do
  real1 <- many1 digit
  char '.'
  real2 <- many1 digit
  string " +"
  separateOrJoined
  i <- char 'i'
  imag1 <- many1 digit
  char '.'
  imag2 <- many1 digit
  return ((read (real1 ++ "." ++ real2) :: Double) :+ (read (imag1 ++ "." ++ imag2) :: Double))
-}

---END: Parsers for States----

--START: Parser Histogram data--
parseHist :: GenParser Char st (Int, [[String]])
parseHist = do
  char '('
  rep <- many1 digit
  string ", "
  lvar <- parseListListVarHist
  char ')'
  return (read rep :: Int, lvar)

parseListListVarHist :: GenParser Char st [[String]]
parseListListVarHist = try (between (char '[') (char ']') (sepBy parseListVarHist (char ',')))
                       <|> fmap (:[]) parseListVarHist

parseListVarHist :: GenParser Char st [String]
parseListVarHist = between (char '[') (char ']') (sepBy (many1 alphaNum) (char ','))  
--END: Parser Histogram data--


--START: Parser for configurations--
pConf :: GenParser Char st (C,SC,L,SQ)
pConf = do
  char '<'
  spacesAndEnters
  com <- parseCSelect
  char ','
  spacesAndEnters
  sc <- pClassicStates
  (l,sq) <- option ([],fromLists [[]]) (do
                                           char ','
                                           spacesAndEnters
                                           pLSQ
                                       )
  spacesAndEnters
  char '>'
  return (com,sc,l,sq)

pLSQ :: GenParser Char st (L,SQ)
pLSQ = do
  l <- parserL
  char ','
  spacesAndEnters
  sq <- pOpDen
  return (l,sq)
--END: Parser for configurations--


-- pConf :: GenParser Char st (C,SC,L,SQ)
-- pConf = do
--   char '<'
--   spacesAndEnters
--   com <- parseCSelect
--   char ','
--   spacesAndEnters
--   sc <- pClassicStates
--   char ','
--   spacesAndEnters
--   l <- parserL
--   char ','
--   spacesAndEnters
--   sq <- pOpDen
--   spacesAndEnters
--   char '>'
--   return (com,sc,l,sq)
  
