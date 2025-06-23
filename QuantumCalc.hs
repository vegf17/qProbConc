module QuantumCalc where

import Data.Matrix
import Data.Complex
import Data.List 
import Data.Ratio
import Data.Char
import Data.Fixed
import Data.Maybe
import Numeric (showFFloat)

import Syntax

--START: Calculations for quantum states as vectors--
-- Some useful constants:
h = 1/(sqrt 2)
hC = h :+ 0 -- h as a Complex Double value
c1 = 1.0 :+ 0 -- 1 as a Complex Double value
c0 = 0.0 :+ 0 -- 0 as a Complex Double value
i = 0.0 :+ 1 -- i as a Complex Double value
oneHalf = realToComp (1/2) -- 1/2 as a Complex Double value

-- some unitary gates in matrix form:
had = fromLists [[hC,hC],[hC,-hC]] -- Hadamard gate
ident = fromLists [[c1,c0],[c0,c1]] -- Identity gate
x = fromLists [[c0,c1],[c1,c0]] -- Pauli X gate
y = fromLists [[c0,-i],[i,c0]] -- Pauli Y gate
z = fromLists [[c1,c0],[c0,-c1]] -- Pauli Z gate
sgt = fromLists[[c1, c0],[c0,i]] -- S gate
tof = fromLists [[1,0,0,0,0,0,0,0],
                 [0,1,0,0,0,0,0,0],
                 [0,0,1,0,0,0,0,0],
                 [0,0,0,1,0,0,0,0],
                 [0,0,0,0,1,0,0,0],
                 [0,0,0,0,0,1,0,0],
                 [0,0,0,0,0,0,0,1],
                 [0,0,0,0,0,0,1,0]] -- Toffoli


m0 = fromLists [[1,0],[0,0]] -- measurement operator M0 = |0><0| 
m1 = fromLists [[0,0],[0,1]] -- measurement operator M1 = |1><1|
m2 = fromLists [[0,0],[0,2]] -- 2|1><1|

-- applyGate g nums s = output state that results from applying gate g to qubits whose number is in
-- nums, when the input state is s When g is a 2-qubit gate, the 1st number in nums corresponds to
-- the control qubit, and the 2nd one to the target one.
applyGate :: G -> [Int] -> SQ -> SQ
applyGate g nums s
    | g == H = applyH nums s 
    | g == I = s
    | g == X = applyX nums s
    | g == Y = applyY nums s
    | g == Z = applyZ nums s
    | g == CNOT = applyCNOT nums s
    | g == TOF = applyTOF nums s
    | g == Sgt = applySgt nums s
    | otherwise = applyCZ nums s

-- qNums vars l = list of integers corresponding to vars, according to linking function l
{- --Ines
qNums :: QVarList -> L -> [Int]
qNums [] l = []
qNums (h:t) l = l(h) : (qNums t l)
--Ines -}

-- Diferente da Inês pq se mudou a linking function de uma funcao para uma lista de tuplos
-- (supostamente para tornar as coisas mais gerais possiveis)
qNums :: QVarList -> L -> [Int]
qNums [] _ = []
qNums _ [] = []
qNums (h:t) l = qNumsAux h l : qNums t l

qNumsAux :: QVar -> L -> Int
qNumsAux q [] = error "qbit not initialized"
qNumsAux q ((q',n):t) = if q==q' then n else qNumsAux q t

-- prob i n s = probability of measuring qubit number n in state |i>, with i=0,1, if the initial
-- state of the system is s.
prob :: Int -> Int -> SQ -> Prob
prob i n s
    | (i == 0 || i == 1) = realPart $ matrixToElem $ mult mToStateDagger mToState -- equation (2.92) - Quantum Computation and Quantum Information (Nielson & Chuang)
    | otherwise = error ((show i)++" cannot be the first argument of function prob.")
        where nqubits = numQubits s
              m = if (i==0) then applyToSomeQ m0 [n] nqubits else applyToSomeQ m1 [n] nqubits
              mToState = mult m s
              mToStateDagger = dagger mToState

-- matrixToDouble m returns the only element of matrix m, if it only contains one element
matrixToElem :: Matrix a -> a
matrixToElem m = if (length l == 1) then (head l) else error "Function matrixToDouble is only meant to receive matrices with only 1 element as argument."
    where l = toList m 

-- dagger m = Hermitian conjugate/ adjoint/ conjugate transpose of matrix m, obtained by complex
-- conjugating and transposing m
dagger :: Matrix (Complex Double) -> Matrix (Complex Double)
dagger m = Data.Matrix.transpose $ complexConjugate m

-- state i n s = state of the system whose initial state is s, after measuring its n-th qubit to be
-- in state |i>, with i=0,1
stateMeas :: Int -> Int -> SQ -> SQ
stateMeas i n s
    | (i == 0 || i == 1) = fromLists (finalState) -- equation (2.93) - Quantum Computation and Quantum Information (Nielson & Chuang)
    | otherwise = error ((show i)++" cannot be the first argument of function state.")
        where nqubits = numQubits s
              m = if (i==0) then applyToSomeQ m0 [n] nqubits else applyToSomeQ m1 [n] nqubits
              mToState = mult m s
              mToStateL = toLists mToState -- matrix mToState in the form of a list of lists of Complex Double values
              p = prob i n s
              finalState = map ( map ( divideBy (realToComp (sqrt p)) ) ) mToStateL

-- mult a b = a * b
mult :: Matrix (Complex Double) -> Matrix (Complex Double) -> Matrix (Complex Double)  
mult a b = multStd2 a b

divideBy :: Complex Double -> Complex Double -> Complex Double
divideBy a b = b/a

-- realToComp turns a Double value into its corresponding Complex Double value
realToComp :: Double -> Complex Double
realToComp a = a :+ 0

-- (applyH nums s) is the output state that results from applying an Hadamard gate to the qubits
-- whose number is in nums, when the input state is s
applyH :: [Int] -> SQ -> SQ
applyH nums s = mult matrix s
    where matrix = applyToSomeQ had nums (numQubits s)

-- (applyX nums s) is the output state that results from applying a Pauli X gate to the qubits whose
-- number is in nums, when the input state is s
applyX :: [Int] -> SQ -> SQ
applyX nums s = mult matrix s
    where matrix = applyToSomeQ x nums (numQubits s)

-- (applyY nums s) is the output state that results from applying a Pauli Y gate to the qubits whose
-- number is in nums, when the input state is s
applyY :: [Int] -> SQ -> SQ
applyY nums s = mult matrix s
    where matrix = applyToSomeQ y nums (numQubits s)

-- (applyZ nums s) is the output state that results from applying a Pauli Z gate to the qubits whose
-- number is in nums, when the input state is s
applyZ :: [Int] -> SQ -> SQ
applyZ nums s = mult matrix s
    where matrix = applyToSomeQ z nums (numQubits s)

-- (applyCNOT nums s) is the output state that results from applying a CNOT gate to the two qubits
-- whose number is in list nums (the 1st one is the control one and the 2nd one is the target one),
-- when the input state is s. If list nums does not contain only two elements, there is an error.
applyCNOT :: [Int] -> SQ -> SQ
applyCNOT l s
    | (length l /= 2) = error "First argument of function applyCNOT must be a list with two elements."
    | otherwise = if (control /= target) then mult matrix s else error "The control and target qubits given as argument to function applyCNOT cannot be the same."
        where control = head l
              target = last l
              nqubits = numQubits s
              listId = gateList ident nqubits
              matrix0 = applyToSomeQ m0 [control] nqubits 
              matrix1 = tensorProduct $ replaceByGate x [target] (replaceByGate m1 [control] listId)
              matrix = sumMatrices matrix0 matrix1 -- equation (6.17) - Quantum Information (Barnett)

-- (applyTOF nums s) is the output state that results from applying a TOF gate to the three qubits
-- whose number is in list nums (the 1st and 2nd one are the control one and the 3rd one is the
-- target one), when the input state is s. If list nums does not contain only three elements, there
-- is an error.
applyTOF :: [Int] -> SQ -> SQ
applyTOF l s
  | (length l /= 3) = error "First argument of function applyTOF must be a list with three elements."
  | otherwise = if ((ctrl1 /= ctrl2) && (ctrl1 /= target) && (ctrl2 /= target))
                then mult matrix s
                else error "Control and/or target qubits given as argument to function applyTOF cannot be the same."
  where ctrl1 = head l
        ctrl2 = l!!1
        target = last l
        nqubits = numQubits s
        listId = gateList ident nqubits -- list with nqubits numbers of identity gates
        matrix00 = applyToSomeQ m0 [ctrl1,ctrl2] nqubits
        matrix01 = subtMatrices (applyToSomeQ m0 [ctrl1] nqubits) (applyToSomeQ m0 [ctrl1,ctrl2] nqubits)
        matrix10 = subtMatrices (applyToSomeQ m1 [ctrl1] nqubits) (applyToSomeQ m1 [ctrl1,ctrl2] nqubits)
        matrix11 = tensorProduct $ replaceByGate x [target] (replaceByGate m1 [ctrl1,ctrl2] listId)
        matrix = sumMatrices (sumMatrices matrix00 matrix01) (sumMatrices matrix10 matrix11)

-- (applySgt nums s) is the output state that results from applying a S gate to the qubits whose
-- number is in nums, when the input state is s
applySgt :: [Int] -> SQ -> SQ
applySgt nums s = mult matrix s
    where matrix = applyToSomeQ sgt nums (numQubits s)


-- (applyCZ nums s) is the output state that results from applying a CZ gate to the two qubits whose
-- number is in list nums (the 1st one is the control one and the 2nd one is the target one), when
-- the input state is s. If list nums does not contain only two elements, there is an error.
applyCZ :: [Int] -> SQ -> SQ
applyCZ l s
    | (length l /= 2) = error "First argument of function applyCZ must be a list with two elements."
    | otherwise = if (control /= target) then mult matrix s else error "The control and target qubits given as argument to function applyCZ cannot be the same."
        where control = head l
              target = last l
              nqubits = numQubits s
              listId = gateList ident nqubits
              matrix1 = tensorProduct listId 
              matrix2 = tensorProduct $ replaceByGate m1 [target] (replaceByGate m2 [control] listId)
              matrix = subtMatrices matrix1 matrix2 -- equation (6.22) - Quantum Information (Barnett)

-- (applyToSomeQ op nums n) = transformation matrix that corresponds to applying operator op to
-- qubits whose number is in nums, when the total number of qubits is n
applyToSomeQ :: Op -> [Int] -> Int -> Op
applyToSomeQ op nums nqubits
    | (nqubits == 1) = if (nums == [1]) then op else error "The second argument of function applyToSomeQ can only be [1], if its third argument is 1."
    | otherwise = tensorProduct (replaceByGate op nums listId)
        where listId = gateList ident nqubits

-- sumMatrices a b = sum of matrices a and b
sumMatrices :: Op -> Op -> Op
sumMatrices a b = elementwise (+) a b

-- subtMatrices a b = a - b, where a and b are matrices
subtMatrices :: Op -> Op -> Op
subtMatrices a b = elementwise (-) a b 

-- (numQubits s) is equal to the number of qubits of a system in state s
numQubits :: SQ -> Int
numQubits s = if log2IntToDouble == log2 then log2Int else error "The matrix given as argument to function numQubits is not a valid quantum state."
    where log2IntToDouble = (fromIntegral log2Int) :: Double -- log2Int converted to a Double value
          log2Int = round log2 :: Int -- log2 rounded to an Int value
          log2 = logBase 2.0 numElemsDouble -- logarithm base 2 of numElemsDouble (log2 is of type Double)
          numElemsDouble = (fromIntegral numElems) :: Double -- numElems converted to a Double value
          numElems = length (toList s) -- number of elements in matrix s

-- (gateList op n) = list with n elements, all equal to op
gateList :: Op -> Int -> [Op]
gateList op 0 = []
gateList op n = op : (gateList op (n-1))

-- (replaceByGate op nums l) = list l, but replacing by op the elements of l whose indexes are in list nums
-- E.g. replaceByGate A [1,3] [B,C,D] = [A,C,A] 
replaceByGate :: Op -> [Int] -> [Op] -> [Op]
replaceByGate op [] l = l
replaceByGate op (h:t) l
    | (n==0) = error ("Empty operators list received as argument by function replaceByGate.")
    | (h > n) = error ("List of operators given as argument to function replaceByGate does not contain " ++ (show h) ++ " elements.") 
    | (h < 1) = error ("The list of indexes received as argument by function replaceByGate cannot contain integers less than 1.")
    | otherwise = replaceByGate op t nextl
        where n = length l
              first = [op] ++ ( if (n==1) then [] else (elements 2 n l) )
              last = (elements 1 (n-1) l) ++ [op]
              middle = (elements 1 (h-1) l) ++ [op] ++ (elements (h+1) n l)
              nextl = if (h==1) then first else (if (h==n) then last else middle)

-- (elements a b l) = list with elements from l, from the a-th to the b-th, if (a >= 1), (b >= a), (b <= length l), and l is non-empty.
-- Otherwise, an error message is displayed.
-- E.g. elements 1 2 [1,2,3] = [1,2] 
elements :: Int -> Int -> [a] -> [a] 
elements a b [] = error "Unsuitable argument(s) given to function elements."
elements a b (h:t)
    | (a < 1 || a > b) = error "Unsuitable argument(s) given to function elements."
    | (a==1) && (b==1) = [h]
    | (a==1) = h:(elements 1 (b-1) t)
    | otherwise = elements (a-1) (b-1) t

-- tensorProduct l = result of the tensor product between all elements of l (e.g tensorProduct [A,B,C] = A tensor B tensor C)
tensorProduct :: [Op] -> Op
tensorProduct [] = error "No matrices given for the calculation of their tensor product."
tensorProduct [a] = error "Not enough matrices given for the calculation of their tensor product."
tensorProduct [a,b] = fromLists (tensorProductLists (toLists a) (toLists b))
tensorProduct (a:b:t) = tensorProduct [a, (tensorProduct (b:t)) ]

-- (tensorProductLists a b) is the tensor product of a and b, if a and b correspond to matrices
tensorProductLists :: [[Complex Double]] -> [[Complex Double]] -> [[Complex Double]]
tensorProductLists a [] = []
tensorProductLists [] b = []
tensorProductLists (h:t) b = (map (getLineTensor h) b) ++ (tensorProductLists t b)

-- getLineTensor [a1,...,an] b is the result of concatenating (multElemLine a1 b), ..., (multElemLine an b) 
getLineTensor :: [Complex Double] -> [Complex Double] -> [Complex Double]
getLineTensor [] l = []
getLineTensor l [] = []
getLineTensor (h:t) l = (multElemLine h l) ++ (getLineTensor t l)

-- (multElemLine x l) is the list that results from multiplying every element of l by x
multElemLine :: Complex Double -> [Complex Double] -> [Complex Double]
multElemLine x [] = []
multElemLine x (h:t) = xh : (multElemLine x t)
    where (a,theta) = polar x
          (b,phi) = polar h
          xh = mkPolar (a*b) (theta + phi)

-- complexConjugate m is the conjugate of matrix m
complexConjugate :: Matrix (Complex Double) -> Matrix (Complex Double)
complexConjugate m = fromLists (conjugated)
    where mList = toLists m -- matrix m in the form of a list of lists of Complex Double values
          conjugated = map (map conjugate) mList -- mList after complex conjugating all the elements in its lists
--END: Calculations for quantum states as vectors--

--Multiply a matrix by a scalar
multElem :: Double -> Matrix (Complex Double) -> Matrix (Complex Double)
multElem n m = fromLists [[e*(realToComp n) | e <- l] | l <- m']
  where m' = toLists m


--START: Calculations for density operators--
-- prob i n s = probability of measuring qubit number n in state |i>, with i=0,1, if the initial
-- state of the system is s.
appGateOpDen :: G -> [Int] -> SQ -> SQ
appGateOpDen g nums s
    | g == H = applyOpDenH nums s
    | g == I = s
    | g == X = applyOpDenX nums s
    | g == Y = applyOpDenY nums s
    | g == Z = applyOpDenZ nums s
    | g == CNOT = applyOpDenCNOT nums s
    | g == TOF = applyOpDenTOF nums s
    | g == Sgt = applyOpDenSgt nums s
    | otherwise = applyOpDenCZ nums s

applyOpDenH :: [Int] -> SQ -> SQ
applyOpDenH  nums s = mult (mult matrix s) (dagger matrix)
    where matrix = applyToSomeQ had nums (div (numQubits s) 2)

applyOpDenX :: [Int] -> SQ -> SQ
applyOpDenX nums s = mult (mult matrix s) (dagger matrix)
    where matrix = applyToSomeQ x nums (div (numQubits s) 2)

-- (applyY nums s) is the output state that results from applying a Pauli Y gate to the qubits whose
-- number is in nums, when the input state is s
applyOpDenY :: [Int] -> SQ -> SQ
applyOpDenY nums s = mult (mult matrix s) (dagger matrix)
    where matrix = applyToSomeQ y nums (div (numQubits s) 2)

-- (applyZ nums s) is the output state that results from applying a Pauli Z gate to the qubits whose
-- number is in nums, when the input state is s
applyOpDenZ :: [Int] -> SQ -> SQ
applyOpDenZ nums s = mult (mult matrix s) (dagger matrix)
    where matrix = applyToSomeQ z nums (div (numQubits s) 2)

-- (applyCNOT nums s) is the output state that results from applying a CNOT gate to the two qubits
-- whose number is in list nums (the 1st one is the control one and the 2nd one is the target one),
-- when the input state is s. If list nums does not contain only two elements, there is an error.
applyOpDenCNOT :: [Int] -> SQ -> SQ
applyOpDenCNOT l s
    | (length l /= 2) = error "First argument of function applyCNOT must be a list with two elements."
    | otherwise =
      if (control /= target)
      then mult (mult matrix s) (dagger matrix)
      else error "The control and target qubits given as argument to function applyCNOT cannot be the same."
  where control = head l
        target = last l
        nqubits = div (numQubits s) 2
        listId = gateList ident nqubits
        matrix0 = applyToSomeQ m0 [control] nqubits 
        matrix1 = tensorProduct $ replaceByGate x [target] (replaceByGate m1 [control] listId)
        matrix = sumMatrices matrix0 matrix1 -- equation (6.17) - Quantum Information (Barnett)

-- (applyTOF nums s) is the output state that results from applying a TOF gate to the three qubits
-- whose number is in list nums (the 1st and 2nd one are the control one and the 3rd one is the
-- target one), when the input state is s. If list nums does not contain only three elements, there
-- is an error.
applyOpDenTOF :: [Int] -> SQ -> SQ
applyOpDenTOF l s
  | (length l /= 3) = error "First argument of function applyTOF must be a list with three elements."
  | otherwise = if ((ctrl1 /= ctrl2) && (ctrl1 /= target) && (ctrl2 /= target))
                then mult (mult matrix s) (dagger matrix)
                else error "Control and/or target qubits given as argument to function applyTOF cannot be the same."
  where ctrl1 = head l
        ctrl2 = l!!1
        target = last l
        nqubits = div (numQubits s) 2
        listId = gateList ident nqubits -- list with nqubits numbers of identity gates
        matrix00 = applyToSomeQ m0 [ctrl1,ctrl2] nqubits
        matrix01 = subtMatrices (applyToSomeQ m0 [ctrl1] nqubits) (applyToSomeQ m0 [ctrl1,ctrl2] nqubits)
        matrix10 = subtMatrices (applyToSomeQ m1 [ctrl1] nqubits) (applyToSomeQ m1 [ctrl1,ctrl2] nqubits)
        matrix11 = tensorProduct $ replaceByGate x [target] (replaceByGate m1 [ctrl1,ctrl2] listId)
        matrix = sumMatrices (sumMatrices matrix00 matrix01) (sumMatrices matrix10 matrix11)

-- (applySgt nums s) is the output state that results from applying a S gate to the qubits whose
-- number is in nums, when the input state is s
applyOpDenSgt :: [Int] -> SQ -> SQ
applyOpDenSgt nums s = mult (mult matrix s) (dagger matrix)
    where matrix = applyToSomeQ sgt nums (div (numQubits s) 2)

-- (applyCZ nums s) is the output state that results from applying a CZ gate to the two qubits whose
-- number is in list nums (the 1st one is the control one and the 2nd one is the target one), when
-- the input state is s. If list nums does not contain only two elements, there is an error.
applyOpDenCZ :: [Int] -> SQ -> SQ
applyOpDenCZ l s
    | (length l /= 2) = error "First argument of function applyCZ must be a list with two elements."
    | otherwise =
      if (control /= target)
      then mult (mult matrix s) (dagger matrix)
      else error "The control and target qubits given as argument to function applyCZ cannot be the same."
  where control = head l
        target = last l
        nqubits = div (numQubits s) 2
        listId = gateList ident nqubits
        matrix1 = tensorProduct listId 
        matrix2 = tensorProduct $ replaceByGate m1 [target] (replaceByGate m2 [control] listId)
        matrix = subtMatrices matrix1 matrix2 -- equation (6.22) - Quantum Information (Barnett)

-- prob i n s = probability of measuring qubit number n in state |i>, with i=0,1, if the initial
-- state of the system is s.
probOpDen :: Int -> Int -> SQ -> Prob
probOpDen i n s
    | (i == 0 || i == 1) = realPart $ trace $  mToState 
    | otherwise = error ((show i)++" cannot be the first argument of function prob.")
        where nqubits = div (numQubits s) 2
              p0 = mult (dagger m0) m0
              p1 = mult (dagger m1) m1
              m = if (i==0)
                  then applyToSomeQ p0 [n] nqubits
                  else applyToSomeQ p1 [n] nqubits
              mToState = mult m s
              -- mToStateDagger = dagger mToState

-- state i n s = state of the system whose initial state is s, after measuring its n-th qubit to be
-- in state |i>, with i=0,1
stateMeasOpDen :: Int -> Int -> SQ -> SQ
stateMeasOpDen i n s
    | (i == 0 || i == 1) = finalState 
    | otherwise = error ((show i)++" cannot be the first argument of function state.")
        where nqubits = div (numQubits s) 2
              m = if (i==0)
                  then applyToSomeQ m0 [n] nqubits
                  else applyToSomeQ m1 [n] nqubits
              opden = mult (mult m s) m -- matrix 
              p = probOpDen i n s -- 
              finalState = multElem (1/p) opden

resetOpDen :: Int -> SQ -> SQ
resetOpDen n s = reset
  where nqubits = div (numQubits s) 2
        s0 = applyToSomeQ m0 [n] nqubits 
        s00 = mult (mult s0 s) s0 --  |0><0| \rho |0><0|
        s1 = applyToSomeQ (fromLists [[0,1],[0,0]]) [n] nqubits 
        s01 = mult (mult s1 s) (dagger s1) --  |0><1| \rho |1><0|
        reset = sumMatrices s00 s01 --Foundations of quantum programming
--END: Calculations for density operators--


