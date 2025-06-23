module HistogramSem where

import Syntax
import Beautify
import Data.Complex -- module for complex numbers
import Data.Matrix -- module for matrix datatype and operations
import Data.Char -- intToDigit function
import Graphics.Histogram
import qualified Graphics.Gnuplot.Frame.OptionSet as Opt
import System.Exit -- in order to get type ExitCode 


-- Building a histogram with integer data, with one column for each integer, whose caption is the
-- respective state for each integer (x-1)

-- (histogramInt dataSet t) plots an histogram whose input is dataSet and whose title is t, with the following features:
---- dataSet is a list formed by tuples (i, mem_i), where 1 <= i <= n, with n being the number of different memories in dataSet
---- the bin size is 1, so there is one column for each integer; 
---- the range of the x-axis is from -1 to (n+1);
---- in the x-axis only integers from 0 to (n-1) are labeled;
---- for each x, there is an associated mem_i which is then used as the caption

-- The elements of dataSet are all expected to be integers.
histogramInt :: [(Double,Mem)] -> String -> IO ExitCode
histogramInt [] title = error "Empty input."
histogramInt dataSet title = plotAdv "" options hist -- the filename (1st argument of plotAdv) is empty, so the histogram will appear on a new window
    where newDataSet = [(d-1,st) | (d,st) <- dataSet] -- moving the x coordinates of the labels to the left by one
          doubles =  [d | (d,st) <- newDataSet]
          max = round (maximum doubles) :: Int -- (maximum doubles) is of type Double, but max is of type Int; max is n
          hist = histogramBinSize 1 doubles -- (histogramBinSize 1 doubles) creates a histogram with bin size 1 and with doubles as its input
          options = Opt.title title $ Opt.xRange2d (-1,max) $ Opt.xTicks2d (xTicksData newDataSet) (defOpts hist) -- options for the histogram (title, range of the x-axis and labels of the x-axis)

-- xTicksData [(0.0, mem_1),(1.0, mem_2), ..., (n-1, mem_n)] = [("mem_1",0), ("mem_2",1), ... ("mem_n", n-1)]
xTicksData :: [(Double, Mem)] -> [(String, Int)]
xTicksData [] = []
xTicksData ((d,mem):t) = (memToString mem, round d :: Int) : xTicksData t



-- Consultei, para fazer este código:

-- http://learnyouahaskell.com/
-- https://hackage.haskell.org/
-- especificamente:
-- https://hackage.haskell.org/package/Histogram-0.1.0.2/docs/Graphics-Histogram.html (contains a useful example)
-- https://hackage.haskell.org/package/gnuplot-0.5.7/src/src/Demo.hs (also contains some useful examples)

-- https://wiki.haskell.org/Converting_numbers (mentions useful functions for converting numbers)
