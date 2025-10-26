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


module ParserCom where

import Text.Parsec (parserTrace)
import Text.ParserCombinators.Parsec -- import functions from Parsec (this module exports modules Text.ParserCombinators.Parsec.Prim, Text.ParserCombinators.Parsec.Combinator and Text.ParserCombinators.Parsec.Char)
import Text.Parsec.String (Parser)
import Data.Matrix hiding((<|>))
import Data.Complex

import Syntax
--import ParserBE_Brookes -- module created in ParserBE_Brookes.hs
import ParserAEBE
import QuantumCalc

{-
Important functions of this module:
· parseInputC :: String -> Either ParseError C
  (parseInputC s) where:
  - s is a string corresponding to a command
  returns either an error or the parsing of s
-}

--START: Test parsers--
-- test parseC (apply to an input given as argument a parser for commands)
parseInputC :: String -> Either ParseError C 
parseInputC input = parse parseC "(unknown)" input
--END: Test parsers--

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
               <|> try(parseAtom)
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
  --separateOrJoined
  spacesOnly
  char '('
  --separateOrJoined
  spacesOnly
  q <- parseQVar
  --separateOrJoined
  spacesOnly
  char ')'
  return (Reset q)

-- parses Meas commands
parseMeas :: GenParser Char st C 
parseMeas = do
    string "Meas"
    --separateOrJoined
    spacesOnly
    char '('
  --separateOrJoined
    spacesOnly
    c <- parseIdeStr
    spacesOnly
    char ','
    --separateOrJoined
    spacesOnly
    q <- parseQVar
    --separateOrJoined
    spacesOnly
    char ')'
    return (Meas (c,q))

-- parses the if command
parseIf :: GenParser Char st C
parseIf = do
    string "if"
    --separateElems -- 1 or + spaces followed by 0 or + '\n' / 1 or + '\n'
    spacesOnly
    b <- parseBSelect
    --separateElems
    spaces
    string "then"
    --separateOrJoined -- there can be no separation between "then" and "{"
    spaces
    char '{' -- 1st command begins
    --separateOrJoined
    spaces
    c1 <- parseCSelect
    --separateOrJoined
    spaces
    char '}' -- 1st command ends
    --separateOrJoined
    spaces
    string "else"
    --separateOrJoined
    spaces
    char '{' -- 2nd command begins
    --separateOrJoined
    spaces
    c2 <- parseCSelect
    --separateOrJoined
    spaces
    char '}' -- 2nd command ends
    return (IfC b c1 c2)

-- parses While commands
parseWhile :: GenParser Char st C
parseWhile = do
  string "while"
  --separateElems
  spacesOnly
  b <- parseBSelect
  --separateElems
  spacesOnly
  string "do"
  --separateOrJoined -- there can be no separation between "do" and "{"
  spaces
  char '{' -- command begins
  --separateOrJoined
  spaces
  c <- parseCSelect
  --separateOrJoined
  spaces
  char '}' -- command ends
  return (Whl b c)

-- parses commands with value "(C)"  
parseParen :: GenParser Char st C 
parseParen = do
    char '('
    --spacesOnly
    spaces
    cInsideParen <- parseCSelect
    --spacesOnly
    spaces
    char ')'
    return cInsideParen

-- parses U commands
parseGate :: GenParser Char st C 
parseGate = try(pGate1Q)
            <|> try(pGate1QArg)
            <|> try(pGate2Q)
            <|> try(pGate2QArg)            
            <|> pGate3Q

-- parses U commands with 1-qubit gates
pGate1Q :: GenParser Char st C 
pGate1Q = do
    g <- gate1Q
    --separateOrJoined
    spacesOnly
    char '('
    --separateOrJoined -- there can be no separation between "(" and the name of the quantum variable
    spacesOnly
    q <- parseQVar -- q is a quantum variable
    qs <- (try(parseQVars) <|> return []) -- qs is either a list of quantum variables or an empty list
    --separateOrJoined
    spacesOnly
    char ')'
    return (U g (q:qs))

-- parses U parameterized commands with 1-qubit gates
--Ph(pi/4, q1)
pGate1QArg :: GenParser Char st C 
pGate1QArg = do
    _ <- string "Ph"
    --separateOrJoined
    spacesOnly
    char '('
    --separateOrJoined -- there can be no separation between "(" and the name of the quantum variable
    spacesOnly
    arg <- parseArgSelect
    spacesOnly
    char ','
    spacesOnly
    q <- parseQVar -- q is a quantum variable
    qs <- (try(parseQVars) <|> return []) -- qs is either a list of quantum variables or an empty list
    --separateOrJoined
    spacesOnly
    char ')'
    return (U (Ph arg) (q:qs))

-- parses U commands with 2-qubit gates
pGate2Q :: GenParser Char st C 
pGate2Q = do
    g <- gate2Q
    --separateOrJoined
    spacesOnly
    char '('
    --separateOrJoined -- there can be no separation between "(" and the name of the quantum variable
    spacesOnly
    q1 <- parseQVar -- q1 is a quantum variable
    spacesOnly
    char ','
    --separateOrJoined
    spacesOnly
    q2 <- parseQVar -- q2 is another quantum variable
    --separateOrJoined
    spacesOnly
    char ')'
    return (U g [q1,q2])

-- parses U parameterized commands with 2-qubit gates
-- CPh(pi/4, q1, q2)
pGate2QArg :: GenParser Char st C 
pGate2QArg = do
    _ <- string "CPh"
    --separateOrJoined
    spacesOnly
    char '('
    --separateOrJoined -- there can be no separation between "(" and the name of the quantum variable
    spacesOnly
    arg <- parseArgSelect
    spacesOnly
    char ','
    spacesOnly
    q1 <- parseQVar -- q1 is a quantum variable
    spacesOnly
    char ','
    --separateOrJoined
    spacesOnly
    q2 <- parseQVar -- q2 is another quantum variable
    --separateOrJoined
    spacesOnly
    char ')'
    return (U (CPh arg) [q1,q2])


-- parses U commands with 3-qubit gates
pGate3Q :: GenParser Char st C 
pGate3Q = do
    g <- gate3Q
    --separateOrJoined
    spacesOnly
    char '('
    --separateOrJoined -- there can be no separation between "(" and the name of the quantum variable
    spacesOnly
    q1 <- parseQVar -- q1 is a quantum variable
    spacesOnly
    char ','
    --separateOrJoined
    spacesOnly
    q2 <- parseQVar -- q2 is another quantum variable
    spacesOnly
    char ','
    --separateOrJoined
    spacesOnly
    q3 <- parseQVar -- q1 is a quantum variable
    --separateOrJoined
    spacesOnly
    char ')'
    return (U g [q1,q2,q3])    

-- parses the name of a 1-qubit gate belonging to this language ("H", "I", "X", "Y", "Z", "S", or "Ph")
gate1Q :: GenParser Char st G
gate1Q = do
    s <- ( try (string "H")
           <|> try (string "I")
           <|> try (string "X")
           <|> try (string "Y")
           <|> try (string "S")
           <|> try (string "Umag2")
           <|> (string "Z") )
    return (strToG s)

-- parses the name of a 2-qubit gate belonging to this language ("CNOT" or "CZ")
gate2Q :: GenParser Char st G 
gate2Q = do
    s <- (
      try (string "SWAP")
      <|> try (string "CNOT")
      <|> try (string "Vmag3")
      <|> (string "CZ") )
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
strToG "S" = Sgt
strToG "SWAP" = SWAP
strToG "CNOT" = CNOT
strToG "CZ" = CZ
strToG "TOF" = TOF
strToG "Umag2" = Umag2
strToG "Vmag3" = Vmag3
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
           <|> try (string "Ph")
           <|> try (string "CPh")
           <|> try (string "SWAP")
           <|> try (string "Umag2")
           <|> try (string "Vmag3")           
           <|> try (string "or")
           <|> try (string "Meas")
           <|> try (string "while")
           <|> try (string "await")
           <|> try (string "mod")
           <|> try (string "Atom")
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
         <|> try(parseAtom)
         <|> parseParen
  return com

-- parses a probabilistic command
parseProb :: GenParser Char st C
parseProb = do
  x <- comProb   -- a;b (+)(p) c||d --> a;(b (+)(p) c)||d OR (a;b) (+)(p) (c||d)
  spaces
  string "(+)("
  --prob <- parseProbDouble
  prob <- parseProbRat
  char ')'
  spaces
  y <- comProb
  loop (P prob x y)
    where
      f x = do
        spaces
        string "(+)("
        --prob <- parseProbDouble
        prob <- parseProbRat
        char ')'
        spaces
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
         <|> try(parseAtom)
         <|> parseParen  
  return com

-- parses an Or command
parseOr :: GenParser Char st C
parseOr = do
  x <- comOr   -- a;b or c||d --> a;(b or c)||d OR (a;b) or (c||d)
  spaces
  string "or"
  spaces
  y <- comOr
  loop (Or x y)
    where
      f x = do
        spaces
        string "or"
        spaces
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
         <|> try(parseAtom)
         <|> parseParen  
  return com

-- parses a sequential command
parseSeq :: GenParser Char st C
parseSeq = do
  x <- comSeq 
  spaces
  string ";"
  spaces
  y <- comSeq
  loop (Seq x y)
    where
      f x = do
        spaces
        string ";"
        spaces
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
         <|> try(parseAtom)
         <|> parseParen    
  return com

-- parses a parallel command 
parseParal :: GenParser Char st C
parseParal = do
  x <- comParal 
  spaces
  string "||"
  spaces
  y <- comParal
  loop (Paral x y)
    where
      f x = do
        spaces
        string "||"
        spaces
        y <- comParal
        loop (Paral x y)
      loop t = try(f t) <|> return t

--Parsers for the await command
--parses the await command
parseAwait :: GenParser Char st C
parseAwait = do
  string "await"
  --separateElems
  spacesOnly
  b <- parseBSelect
  --separateElems
  spacesOnly
  string "do"
  --separateOrJoined -- there can be no separation between "do" and "{"
  spaces
  char '{' -- command begins
  --separateOrJoined
  spaces
  c <- parseCSelect
  --separateOrJoined
  spaces
  char '}' -- command ends
  return (Await b c)

-- parses the atom command
parseAtom :: GenParser Char st C
parseAtom = do
  string "Atom"
  --separateOrJoined
  spacesOnly
  char '('
  --separateOrJoined
  spaces
  c <- parseCSelect
  --separateOrJoined
  spaces
  char ')'
  return (Atom c)
--END: Parsers for Commands--
