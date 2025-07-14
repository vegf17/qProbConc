module SemBE_Brookes where

import Syntax
import ParserAEBE

evalE :: String -> Integer
evalE exp =
  case testParseE exp of
    Right e -> bigStepExp e []
    otherwise -> undefined

-- changeSt i n s is the state [s|i=n] (here, we are not allowing attributions to non-declared
-- identifiers - see section 11.2 from the article) 
changeSt :: String -> Integer -> SC -> SC
changeSt i n [] = []
changeSt i n ((i',n'):t) = if (i'==i) then (i',n):(changeAll i n t) else (i',n'):(changeSt i n t)

-- Auxiliary function to changeSt (changeAll i n s changes the value of all occurrences of i in s)
changeAll :: String -> Integer -> SC -> SC 
changeAll i n [] = []
changeAll i n ((i',n'):t)
    | (i==i') = (i',n):(changeAll i n t)
    | otherwise = (i',n'):(changeAll i n t)

--START: evaluate arithmetic expressions--
-- bigStepExp e s = n means that <e,s> ->* n (see Fig 3 from the article)
bigStepExp :: E -> SC -> Integer 
bigStepExp (Num n) s = n -- 1st rule
bigStepExp (Id i) s = getValue s i -- 3rd rule
bigStepExp (PlusE e1 e2) s = (bigStepExp e1 s) + (bigStepExp e2 s) -- final rules
bigStepExp (MinusE e1 e2) s = (bigStepExp e1 s) - (bigStepExp e2 s) -- final rules
bigStepExp (MultE e1 e2) s = (bigStepExp e1 s) * (bigStepExp e2 s) -- final rules
bigStepExp (DivE e1 e2) s = (bigStepExp e1 s) `div` (bigStepExp e2 s) -- final rules

-- getValue s i = the value that state s attributes to identifier i
getValue :: SC -> String -> Integer 
getValue [] i = error ("No value attributed to identifier \""++i++"\" was found.")
getValue ((i',n'):t) i = if (i==i') then n' else getValue t i
--END: evaluate arithmetic expressions--

--START: evaluate Boolean expressions--
-- bigStepBExp b s is the final result obtained by evaluating b in state s
bigStepBExp :: B -> SC -> Bool 
bigStepBExp (BTrue) s = True
bigStepBExp (BFalse) s = False
bigStepBExp (Not b) s = not (bigStepBExp b s)
bigStepBExp (And b1 b2) s = (bigStepBExp b1 s) && (bigStepBExp b2 s)
bigStepBExp (Equ e1 e2) s = (bigStepExp e1 s) == (bigStepExp e2 s)
bigStepBExp (Leq e1 e2) s = (bigStepExp e1 s) <= (bigStepExp e2 s)
bigStepBExp (Geq e1 e2) s = (bigStepExp e1 s) >= (bigStepExp e2 s)
bigStepBExp (Less e1 e2) s = (bigStepExp e1 s) < (bigStepExp e2 s)
bigStepBExp (Gre e1 e2) s = (bigStepExp e1 s) > (bigStepExp e2 s)
--END: evaluate Boolean expressions--

--START: calculate free variables--
-- (freeE e) is the set of free identifiers in e
freeE :: E -> [String] 
freeE (Num n) = []
freeE (Id i) = [i]
freeE (PlusE e1 e2) = (freeE e1) ++ (freeE e2)

-- (freeB b) is the set of free identifiers in b
freeB :: B -> [String] 
freeB BTrue = []
freeB BFalse = []
freeB (Not b) = freeB b
freeB (And b1 b2) = (freeB b1) ++ (freeB b2)
freeB (Equ e1 e2) = (freeE e1) ++ (freeE e2)
freeB (Leq e1 e2) = (freeE e1) ++ (freeE e2)
freeB (Geq e1 e2) = (freeE e1) ++ (freeE e2)
freeB (Less e1 e2) = (freeE e1) ++ (freeE e2)
freeB (Gre e1 e2) = (freeE e1) ++ (freeE e2)

-- (belong ids s) is only True if no identifier in list ids is missing in state s
belong :: [String] -> SC -> Bool 
belong [] s = True
belong (i:t) s = if (declared i s) then belong t s else False 

-- (declared i s) is True if identifier i is part of state s
declared :: String -> SC -> Bool 
declared i [] = False
declared i ((i',n'):t) = if (i' == i) then True else (declared i t)
--END: calculate free variables--
