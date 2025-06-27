module Syntax where

import Data.Matrix
import Data.Complex

--classical memory
type SC = [(String, Integer)]

--quantum memory
type QVar = String
type QVarList = [QVar]
type Prob = Double
type Op = Matrix (Complex Double)-- operators are represented by matrices of complex numbers
type L = [(QVar,Int)] 
type SQ = Matrix (Complex Double) --quantum state, represented by a density operator 

--memory
type Mem = (SC,SQ)
type LMem = (SC,L,SQ) -- has the linking function, useful for SQ

--ProbPath: (X x V(S+X))* x X ==> ([(X, V(S + X))], X)
-- X = (C,LMem)
-- V(S+X) = [(Either S X, Double)]
-- ([((C,LMem), [(Either LMem (C,LMem), Double)])], (C,LMem))
type ProbPath = ([((C,LMem), [(Either LMem (C,LMem),Double)])], (C,LMem))

--E expressions
data E = Num Integer
       | Id String
       | PlusE E E
       deriving (Show, Eq)

--B expressions
data B = BTrue
       | BFalse
       | Not B
       | And B B
       | Equ E E
       | Leq E E
       | Geq E E
       | Less E E
       | Gre E E
       deriving (Show, Eq)

--Gates considered in the language
data G = H | I | X | Y | Z | CNOT | CZ | TOF | Sgt
       deriving (Show, Eq)

--C expressions
data C = Skip 
       | Asg String E
       | Reset QVar
       | U G QVarList
       | Meas (String, QVar) 
       | Seq C C 
       | Or C C 
       | Paral C C 
       | P Prob C C 
       | IfC B C C 
       | Whl B C
       | Await B CAwait
       deriving (Show, Eq)

--CAwait expressions
data CAwait = SkipA
            | AsgA String E
            | ResetA QVar
            | UA G QVarList
            | MeasA (String, QVar) 
            | SeqA CAwait CAwait
            | IfCA B CAwait CAwait
            deriving (Show, Eq)
