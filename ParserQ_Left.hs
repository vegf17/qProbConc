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
and using left associativity and no auxiliary data types.

-}


module ParserQ_Left where

import Syntax
--import ParserBE_Brookes -- module created in ParserBE_Brookes.hs
import ParserAEBE
import Text.Parsec (parserTrace)
import Text.ParserCombinators.Parsec -- import functions from Parsec (this module exports modules Text.ParserCombinators.Parsec.Prim, Text.ParserCombinators.Parsec.Combinator and Text.ParserCombinators.Parsec.Char)
import Text.Parsec.String (Parser)
import Data.Matrix hiding((<|>))
import Data.Complex
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

-- test parseC (apply to an input given as argument a parser for commands)
parseInputC :: String -> Either ParseError C 
parseInputC input = parse parseC "(unknown)" input

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
parseRun :: String -> Either ParseError [((String, Int, Int), (C,SC,L,SQ))]
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
main :: IO ()
main = do
  putStr "Name of the file to be parsed: "  
  fileName <- getLine                      
  text <- readFile (fileName ++ ".txt")    
  case parse splitProgs "(unknown)" text of
    Left err -> print err  -- Print error if parsing fails
    Right progs -> mapM_ print progs

-- parses to Config a file with one or more programs  
mainRun :: IO ()
mainRun = do
  putStr "Name of the file to be parsed: "  
  fileName <- getLine                      
  text <- readFile (fileName ++ ".txt")    
  case parse splitRun "(unknown)" text of
    Left err -> print err  -- Print error if parsing fails
    Right progs -> mapM_ print progs    

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
splitRun :: GenParser Char st [((String, Int, Int), (C,SC,L,SQ))]
splitRun = do
  string "---"
  name <- many1 alphaNum
  string "---"
  char '\n'
  string "rep: "
  rep <- many1 digit
  char '\n'
  string "k: "
  k <- many1 digit
  char '\n'
  (c,sc,l,sq) <- pConf
  separateOrJoined
  string "---"
  many1 alphaNum
  string "---"
  separateOrJoined
  listRun <- option [] splitRun -- type [(C,SC,L,SQ)]
  return (((name, read rep :: Int, read k :: Int),(c,sc,l,sq)):listRun)
--END: Read from a file--

--START: Main parsers-- 
-- parser for commands (which can have certain white spaces before and after) and returns the
-- corresponding value of type C
parseC :: GenParser Char st C 
parseC = do
    entersOnly
    c <- parseCSelect
    spacesAndEnters
    eof
    return c

-- parser which "forwards" the input (which is a command) to another parser, which it selects
-- according to the input
parseCSelect :: GenParser Char st C 
parseCSelect = try(parseParal)
               <|> try(parseSeq)
               <|> try(parseOr)
               <|> try(parseProb)
               <|> try(parseSkip)
               <|> try(parseGate)
               <|> try(parseAsg)
               <|> try(parseMeas)
               <|> try(parseReset)
               <|> try(parseIf)
               <|> try(parseWhile)
               <|> try(parseAwait)
               <|> parseParen
--END: Main parsers-- 


--START: Parsers for Commands--
{-
For commands C, it is only allowed to have white spaces inside the commands.  In other words, in the
beginning and in the end of commands no white space is allowed.
-}
-- parses commands with value "skip", i.e "Skip" commands
parseSkip :: GenParser Char st C 
parseSkip = do
    string "skip"
    return Skip

-- parses commands with value "I:=E", i.e "Asg I E" commands
parseAsg :: GenParser Char st C 
parseAsg = do
    i <- parseIdeStr
    spacesOnly
    string ":="
    spacesOnly
    e <- parseESelect 
    return (Asg i e) 

-- parses the reset command
parseReset :: GenParser Char st C
parseReset = do
  string "Reset"
  separateOrJoined
  char '('
  separateOrJoined
  q <- parseQVar
  separateOrJoined
  char ')'
  return (Reset q)

-- parses Meas commands
parseMeas :: GenParser Char st C 
parseMeas = do
    string "Meas"
    separateOrJoined
    char '('
    separateOrJoined
    c <- parseIdeStr
    spacesOnly
    char ','
    separateOrJoined
    q <- parseQVar
    separateOrJoined
    char ')'
    return (Meas (c,q))

-- parses the if command
parseIf :: GenParser Char st C
parseIf = do
    string "if"
    separateElems -- 1 or + spaces followed by 0 or + '\n' / 1 or + '\n'
    b <- parseBSelect
    separateElems 
    string "then"
    separateOrJoined -- there can be no separation between "then" and "{"
    char '{' -- 1st command begins
    separateOrJoined
    c1 <- parseCSelect
    separateOrJoined
    char '}' -- 1st command ends
    separateOrJoined
    string "else"
    separateOrJoined
    char '{' -- 2nd command begins
    separateOrJoined
    c2 <- parseCSelect
    separateOrJoined
    char '}' -- 2nd command ends
    return (IfC b c1 c2)

-- parses While commands
parseWhile :: GenParser Char st C
parseWhile = do
  string "while"
  separateElems
  b <- parseBSelect
  separateElems 
  string "do"
  separateOrJoined -- there can be no separation between "do" and "{"
  char '{' -- command begins
  separateOrJoined
  c <- parseCSelect
  separateOrJoined
  char '}' -- command ends
  return (Whl b c)

-- parses commands with value "(C)"  
parseParen :: GenParser Char st C 
parseParen = do
    char '('
    spacesOnly
    cInsideParen <- parseCSelect
    spacesOnly
    char ')'
    return cInsideParen

-- parses U commands
parseGate :: GenParser Char st C 
parseGate = try(pGate1Q) <|> try(pGate2Q) <|> pGate3Q

-- parses U commands with 1-qubit gates
pGate1Q :: GenParser Char st C 
pGate1Q = do
    g <- gate1Q
    separateOrJoined
    char '('
    separateOrJoined -- there can be no separation between "(" and the name of the quantum variable
    q <- parseQVar -- q is a quantum variable
    qs <- (try(parseQVars) <|> return []) -- qs is either a list of quantum variables or an empty list
    separateOrJoined
    char ')'
    return (U g (q:qs))

-- parses U commands with 2-qubit gates
pGate2Q :: GenParser Char st C 
pGate2Q = do
    g <- gate2Q
    separateOrJoined
    char '('
    separateOrJoined -- there can be no separation between "(" and the name of the quantum variable
    q1 <- parseQVar -- q1 is a quantum variable
    spacesOnly
    char ','
    separateOrJoined
    q2 <- parseQVar -- q2 is another quantum variable
    separateOrJoined
    char ')'
    return (U g [q1,q2])

-- parses U commands with 3-qubit gates
pGate3Q :: GenParser Char st C 
pGate3Q = do
    g <- gate3Q
    separateOrJoined
    char '('
    separateOrJoined -- there can be no separation between "(" and the name of the quantum variable
    q1 <- parseQVar -- q1 is a quantum variable
    spacesOnly
    char ','
    separateOrJoined
    q2 <- parseQVar -- q2 is another quantum variable
    spacesOnly
    char ','
    separateOrJoined
    q3 <- parseQVar -- q1 is a quantum variable
    separateOrJoined
    char ')'
    return (U g [q1,q2,q3])    

-- parses the name of a 1-qubit gate belonging to this language ("H", "I", "X", "Y" or "Z")
gate1Q :: GenParser Char st G 
gate1Q = do
    s <- ( try (string "H")
           <|> try (string "I")
           <|> try (string "X")
           <|> try (string "Y")
           <|> (string "Z") )
    return (strToG s)

-- parses the name of a 2-qubit gate belonging to this language ("CNOT" or "CZ")
gate2Q :: GenParser Char st G 
gate2Q = do
    s <- ( try (string "CNOT") <|> (string "CZ") )
    return (strToG s)

-- parses the name of a 3-qubit gate belonging to this language ("TOF")
gate3Q :: GenParser Char st G 
gate3Q = do
    s <- string "TOF"
    return (strToG s)

-- (strToG s) = value of type G corresponding to s, if there is one
strToG :: String -> G 
strToG "H" = H
strToG "I" = I
strToG "X" = X
strToG "Y" = Y
strToG "Z" = Z
strToG "CNOT" = CNOT
strToG "CZ" = CZ
strToG "TOF" = TOF
strToG s = error ("No value of type G corresponds to " ++ s)

-- reservedWordQ parses a reserved word ("skip", "H", "I", "X", "Y", "Z", "CNOT", "CZ", "TOF", "or",
-- "Meas", "Reset", or "while"), as long as it is followed by neither an alphabetic or numeric
-- Unicode character nor an underscore
reservedWordQ :: GenParser Char st String
reservedWordQ = do
    x <- ( try (string "skip")
           <|> try (string "H")
           <|> try (string "I")
           <|> try (string "X")
           <|> try (string "Y")
           <|> try (string "Z")
           <|> try (string "CNOT")
           <|> try (string "CZ")
           <|> try (string "TOF")
           <|> try (string "or")
           <|> try (string "Meas")
           <|> try (string "while")
           <|> try (string "await")
           <|> (string "Reset") )
    notFollowedBy (try(alphaNum) <|> parseUnderscore)
    return x

-- parses quantum variables (corresponding to values of type QVar)
parseQVar :: GenParser Char st QVar 
parseQVar = do
    notFollowedBy (reservedWordQ)
    i <- (try(startUnderscore) <|> startLetter) -- the quantum variable can either start with an underscore or with an alphabetic Unicode character  
    return i -- this "i" will not have any white space

-- parseQVars parses a comma followed by a list of quantum variables given in the input, and returns
-- that same list
parseQVars :: GenParser Char st QVarList 
parseQVars = do
    spacesOnly -- 0 or + spaces
    char ','
    separateOrJoined
    q <- parseQVar -- q is a quantum variable
    qs <- (try (parseQVars) <|> return []) -- qs is either a list of quantum variables or an empty list
    return (q:qs)


--Parsers for the probabilistic command
--parser for the probabilities that will be associated to the probabilistic command
parseProbRat :: GenParser Char st Rational
parseProbRat = try(parseRat) <|> try(parse1Rat) <|> parse0Rat

-- parses 0 into a rational 0%1
parse0Rat :: GenParser Char st Rational
parse0Rat = do
  x <- string "0"
  return (read (x ++ "%1") :: Rational)

-- parses 1 into a rational 1%1
parse1Rat :: GenParser Char st Rational
parse1Rat = do
  x <- string "1"
  return (read (x ++ "%1") :: Rational)

-- parses rational numbers
-- separator is %
-- example: 1%2
parseRat :: GenParser Char st Rational
parseRat = do
  num <- many1 digit
  _ <- char '%'
  den <- many1 digit
  return (read (num ++ "%" ++ den) :: Rational)

{-
--parser for the probabilities that will be associated to the probabilistic command
parseProbDouble :: GenParser Char st Double
parseProbDouble = try(parse1Double) <|> try(parse0DotDouble) <|> parse0Double

-- parses 1 into a double 1.0
parse1Double :: GenParser Char st Double
parse1Double = do
  x <- string "1"
  return (read x :: Double)

-- parses 0 into a double 0.0
parse0Double :: GenParser Char st Double
parse0Double = do
  x <- string "0"
  return (read x :: Double)  

-- parses probabilities p between 0 and 1, ie when 0<p<1 
-- separator is the dot
-- examples: 0.125, 0.1234567890987654323456789876543
parse0DotDouble :: GenParser Char st Double
parse0DotDouble = do
  _ <- char '0'
  _ <- char '.'
  z <- many1 digit
  return (read ("0" ++ "." ++ z) :: Double)
-}

-- parses a term in a probabilistic command: any other command except parallel, sequential, and
-- non-deterministic commands
comProb :: GenParser Char st C 
comProb = do
  com <- try(parseSkip)
         <|> try(parseGate)
         <|> try(parseAsg)
         <|> try(parseMeas)
         <|> try(parseReset)
         <|> try(parseIf)
         <|> try(parseWhile)
         <|> try(parseAwait)
         <|> parseParen
  return com

-- parses a probabilistic command
parseProb :: GenParser Char st C
parseProb = do
  x <- comProb   -- a;b (+)(p) c||d --> a;(b (+)(p) c)||d OR (a;b) (+)(p) (c||d)
  spacesOnly
  string "(+)("
  --prob <- parseProbDouble
  prob <- parseProbRat
  char ')'
  --separateOrJoined
  spacesOnly -- 0 or more spaces
  entersOnly -- 0 or more '\n'
  spacesOnly -- 0 or more spaces
  y <- comProb
  loop (P prob x y)
    where
      f x = do
        spacesOnly
        string "(+)("
        --prob <- parseProbDouble
        prob <- parseProbRat
        char ')'
        --separateOrJoined
        spacesOnly -- 0 or more spaces
        entersOnly -- 0 or more '\n'
        spacesOnly -- 0 or more spaces
        y <- comProb
        loop (P prob x y)
      loop t = try(f t) <|> return t


-- Parsers for the non-deterministic command 

-- parses a term in an or command: any other command except parallel and sequential commands
comOr :: GenParser Char st C 
comOr = do
  com <- try(parseProb)
         <|> try(parseSkip)
         <|> try(parseGate)
         <|> try(parseAsg)
         <|> try(parseMeas)
         <|> try(parseReset)
         <|> try(parseIf)
         <|> try(parseWhile)
         <|> try(parseAwait)
         <|> parseParen  
  return com

-- parses an Or command
parseOr :: GenParser Char st C
parseOr = do
  x <- comOr   -- a;b or c||d --> a;(b or c)||d OR (a;b) or (c||d)
  spacesOnly
  string "or"
  --separateOrJoined
  spacesOnly -- 0 or more spaces
  entersOnly -- 0 or more '\n'
  spacesOnly -- 0 or more spaces
  y <- comOr
  loop (Or x y)
    where
      f x = do
        spacesOnly
        string "or"
        --separateOrJoined
        spacesOnly -- 0 or more spaces
        entersOnly -- 0 or more '\n'
        spacesOnly -- 0 or more spaces
        y <- comOr
        loop (Or x y)
      loop t = try(f t) <|> return t


-- Parsers for the sequential command 

-- parses a term in a sequential command: any other command except parallel commands
comSeq :: GenParser Char st C 
comSeq = do
  com <- try(parseOr)
         <|> try(parseProb)
         <|> try(parseSkip)
         <|> try(parseGate)
         <|> try(parseAsg)
         <|> try(parseMeas)
         <|> try(parseReset)
         <|> try(parseIf)
         <|> try(parseWhile)
         <|> try(parseAwait)
         <|> parseParen  
  return com

-- parses a sequential command
parseSeq :: GenParser Char st C
parseSeq = do
  x <- comSeq 
  spacesOnly
  string ";"
  --separateOrJoined
  spacesOnly -- 0 or more spaces
  entersOnly -- 0 or more '\n'
  spacesOnly -- 0 or more spaces
  y <- comSeq
  loop (Seq x y)
    where
      f x = do
        spacesOnly
        string ";"
        --separateOrJoined
        spacesOnly -- 0 or more spaces
        entersOnly -- 0 or more '\n'
        spacesOnly -- 0 or more spaces
        y <- comSeq
        loop (Seq x y)
      loop t = try(f t) <|> return t

-- Parsers for the parallel command
-- parses a term in a parallel command
comParal :: GenParser Char st C
comParal = do
  com <- try(parseSeq)
         <|> try(parseOr)
         <|> try(parseProb)         
         <|> try(parseSkip)
         <|> try(parseGate)
         <|> try(parseAsg)
         <|> try(parseMeas)
         <|> try(parseReset)
         <|> try(parseIf)
         <|> try(parseWhile)
         <|> try(parseAwait)
         <|> parseParen    
  return com

-- parses a parallel command 
parseParal :: GenParser Char st C
parseParal = do
  x <- comParal 
  spacesOnly
  string "||"
  --separateOrJoined
  spacesOnly -- 0 or more spaces
  entersOnly -- 0 or more '\n'
  spacesOnly -- 0 or more spaces
  y <- comParal
  loop (Paral x y)
    where
      f x = do
        spacesOnly
        string "||"
        --separateOrJoined
        spacesOnly -- 0 or more spaces
        entersOnly -- 0 or more '\n'
        spacesOnly -- 0 or more spaces
        y <- comParal
        loop (Paral x y)
      loop t = try(f t) <|> return t

--Parsers for the await command
--parses the await command
parseAwait :: GenParser Char st C
parseAwait = do
  string "await"
  separateElems
  b <- parseBSelect
  separateElems 
  string "do"
  separateOrJoined -- there can be no separation between "do" and "{"
  char '{' -- command begins
  separateOrJoined
  ca <- parseCASelect
  separateOrJoined
  char '}' -- command ends
  return (Await b ca)

-- parser which "forwards" the input (which is a command) to another parser, which it selects
-- according to the input
parseCASelect :: GenParser Char st CAwait
parseCASelect = try(parseSeqA)
               <|> try(parseSkipA)
               <|> try(parseGateA)
               <|> try(parseAsgA)
               <|> try(parseMeasA)
               <|> try(parseResetA)
               <|> try(parseIfA)
               <|> parseParenA


-- parses commands with value "skip", i.e "Skip" commands
parseSkipA :: GenParser Char st CAwait
parseSkipA = do
    string "skip"
    return SkipA

-- parses commands with value "I:=E", i.e "Asg I E" commands
parseAsgA :: GenParser Char st CAwait
parseAsgA = do
    i <- parseIdeStr
    spacesOnly
    string ":="
    spacesOnly
    e <- parseESelect 
    return (AsgA i e) 

-- parses the reset command
parseResetA :: GenParser Char st CAwait
parseResetA = do
  string "Reset"
  separateOrJoined
  char '('
  separateOrJoined
  q <- parseQVar
  separateOrJoined
  char ')'
  return (ResetA q)

-- parses Meas commands
parseMeasA :: GenParser Char st CAwait
parseMeasA = do
    string "Meas"
    separateOrJoined
    char '('
    separateOrJoined
    c <- parseIdeStr
    spacesOnly
    char ','
    separateOrJoined
    q <- parseQVar
    separateOrJoined
    char ')'
    return (MeasA (c,q))

-- parses the if command
parseIfA :: GenParser Char st CAwait
parseIfA = do
    string "if"
    separateElems -- 1 or + spaces followed by 0 or + '\n' / 1 or + '\n'
    b <- parseBSelect
    separateElems 
    string "then"
    separateOrJoined -- there can be no separation between "then" and "{"
    char '{' -- 1st command begins
    separateOrJoined
    c1 <- parseCASelect
    separateOrJoined
    char '}' -- 1st command ends
    separateOrJoined
    string "else"
    separateOrJoined
    char '{' -- 2nd command begins
    separateOrJoined
    c2 <- parseCASelect
    separateOrJoined
    char '}' -- 2nd command ends
    return (IfCA b c1 c2)

-- parses commands with value "(C)"  
parseParenA :: GenParser Char st CAwait
parseParenA = do
    char '('
    spacesOnly
    cInsideParen <- parseCASelect
    spacesOnly
    char ')'
    return cInsideParen

-- parses U commands
parseGateA :: GenParser Char st CAwait 
parseGateA = try(pGate1QA) <|> try(pGate2QA) <|> pGate3QA

-- parses U commands with 1-qubit gates
pGate1QA :: GenParser Char st CAwait 
pGate1QA = do
    g <- gate1Q
    separateOrJoined
    char '('
    separateOrJoined -- there can be no separation between "(" and the name of the quantum variable
    q <- parseQVar -- q is a quantum variable
    qs <- (try(parseQVars) <|> return []) -- qs is either a list of quantum variables or an empty list
    separateOrJoined
    char ')'
    return (UA g (q:qs))

-- parses U commands with 2-qubit gates
pGate2QA :: GenParser Char st CAwait
pGate2QA = do
    g <- gate2Q
    separateOrJoined
    char '('
    separateOrJoined -- there can be no separation between "(" and the name of the quantum variable
    q1 <- parseQVar -- q1 is a quantum variable
    spacesOnly
    char ','
    separateOrJoined
    q2 <- parseQVar -- q2 is another quantum variable
    separateOrJoined
    char ')'
    return (UA g [q1,q2])

-- parses U commands with 3-qubit gates
pGate3QA :: GenParser Char st CAwait
pGate3QA = do
    g <- gate3Q
    separateOrJoined
    char '('
    separateOrJoined -- there can be no separation between "(" and the name of the quantum variable
    q1 <- parseQVar -- q1 is a quantum variable
    spacesOnly
    char ','
    separateOrJoined
    q2 <- parseQVar -- q2 is another quantum variable
    spacesOnly
    char ','
    separateOrJoined
    q3 <- parseQVar -- q1 is a quantum variable
    separateOrJoined
    char ')'
    return (UA g [q1,q2,q3])

-- Parsers for the sequential command inside await command
-- parses a term in a sequential command: any other command except parallel commands
comSeqA :: GenParser Char st CAwait
comSeqA = do
  com <- try(parseSkipA)
         <|> try(parseGateA)
         <|> try(parseAsgA)
         <|> try(parseMeasA)
         <|> try(parseResetA)
         <|> try(parseIfA)
         <|> parseParenA
  return com


-- parses a sequential command
parseSeqA :: GenParser Char st CAwait
parseSeqA = do
  x <- comSeqA 
  spacesOnly
  string ";"
  --separateOrJoined
  spacesOnly -- 0 or more spaces
  entersOnly -- 0 or more '\n'
  spacesOnly -- 0 or more spaces
  y <- comSeqA
  loop (SeqA x y)
    where
      f x = do
        spacesOnly
        string ";"
        --separateOrJoined
        spacesOnly -- 0 or more spaces
        entersOnly -- 0 or more '\n'
        spacesOnly -- 0 or more spaces
        y <- comSeqA
        loop (SeqA x y)
      loop t = try(f t) <|> return t    
--END: Parsers for Commands--

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


--START: Parser for configurations--
pConf :: GenParser Char st (C,SC,L,SQ)
pConf = do
  char '<'
  spacesAndEnters
  com <- parseCSelect
  char ','
  spacesAndEnters
  sc <- pClassicStates
  char ','
  spacesAndEnters
  l <- parserL
  char ','
  spacesAndEnters
  sq <- pOpDen
  spacesAndEnters
  char '>'
  return (com,sc,l,sq)
--END: Parser for configurations--

