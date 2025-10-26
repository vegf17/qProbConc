import System.Environment

{-
max: maximal number of iterations
nc: maximal number of concurrent randWalks
ni: i-th randWalk; starts at 1
-}

randWalk :: (Int,Int) -> Int -> String
randWalk (ni, nc) max = if (ni > nc)
                 then ""
                      else if (ni==nc)
                           then randWalkAux ni max
                           else (randWalkAux ni max) ++ "\n ||\n" ++ (randWalk (ni+1,nc) max)

randWalkAux :: Int -> Int -> String
randWalkAux ni max =
  " while (i" ++ (show ni) ++ "<" ++ (show max)
  ++ ") do {x" ++ (show ni) ++ ":=x" ++ (show ni) ++ "+1 "
  ++ "(+)(1%2) x" ++ (show ni) ++ ":=x" ++ (show ni) ++ "-1; "
  ++ "i" ++ (show ni) ++ ":=i" ++ (show ni) ++ "+1}"

stRandWalk :: Int -> String
stRandWalk  nc = let init = [ "(" ++ var ++ show id ++ "," ++ show 0 ++ ")" | var <- ["x","i"], id <- [1 .. nc]]
               in "[" ++ stRandWalkAux init ++ "]"
                  
stRandWalkAux :: [String] -> String
stRandWalkAux [] = ""
stRandWalkAux [h] = h
stRandWalkAux (h:t) = h ++ "," ++ stRandWalkAux t  


lazy :: Int -> Int -> String
lazy nc max =
  "---RandWalk" ++ show nc ++ "---\n"
  ++"hist: (1, [])\n"
  ++"k: " ++ (show (nc*(2 + 3*max)))
  ++"\n<\n"
  ++ (randWalk (1,nc) max)
  ++",\n "
  ++ (stRandWalk nc)
  ++"\n>\n---RandWalk" ++ show nc ++ "---\n"  

main :: IO()
main = do
  args <- getArgs
  let nc = (read (head args) :: Int)
      max = (read (last args) :: Int)
  putStrLn $ lazy nc max
