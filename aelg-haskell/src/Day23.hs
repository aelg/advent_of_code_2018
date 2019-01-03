module Day23 ( solve ) where

import           Control.Arrow
import           Control.Monad
import           Data.List
import           Data.Monoid
import qualified Data.Map.Strict as M
import qualified Data.Set as S
import           Data.Maybe
import           Data.Char
import           Text.ParserCombinators.ReadP
import qualified Parsing as P
import qualified Utils as U
import Debug.Trace

parsePos = do
  string "pos=<"
  x <- P.integer
  char ','
  y <- P.integer
  char ','
  z <- P.integer
  string ">, r="
  r <- P.integer
  eof
  return (r,x,y,z)

parse = map (P.run parsePos)

test = ["pos=<0,0,0>, r=4"
       , "pos=<1,0,0>, r=1"
       , "pos=<4,0,0>, r=3"
       , "pos=<0,2,0>, r=1"
       , "pos=<0,5,0>, r=3"
       , "pos=<0,0,3>, r=1"
       , "pos=<1,1,1>, r=1"
       , "pos=<1,1,2>, r=1"
       , "pos=<1,3,1>, r=1"
       ]
test2 = [ "pos=<10,12,12>, r=2"
        , "pos=<12,14,12>, r=2"
        , "pos=<16,12,12>, r=4"
        , "pos=<14,14,14>, r=6"
        , "pos=<50,50,50>, r=200"
        , "pos=<10,10,10>, r=5"
        ]

distance (_,x1,y1,z1) (_,x2,y2,z2) = abs (x1-x2) + abs (y1-y2) + abs (z1-z2)

inRange xs p = (length . filter (\(r,x,y,z) -> distance p (0,x,y,z) <= r) $ xs, distance (0,0,0,0) p, p)

corners (r,x,y,z) = [(0,x-r,y,z), (0,x+r,y,z), (0,x,y-r,z), (0,x,y+r,z), (0,x,y,z-r), (0,x,y,z+r)]

--solve1 :: [String] -> String
solve1 i = show . length . filter ((<= r) . distance (r,x,y,z)) $ i
  where 
    (r, x,y,z) = maximum i

--solve2 :: [String] -> String
solve2 xs = show res
  where
    interesting = concatMap corners xs
    (n, _, p) = last . sort $ map (inRange xs) interesting
    goBack (_,x,y,z) = filter ((==n) . pick . traceShowId . inRange xs) [(0,x-1,y,z), (0,x,y-1,z), (0,x,y,z-1), (0,x-1,y-1,z), (0,x,y-1,z-1), (0,x-1,y,z-1), (0, x-1, y-1,z-1)]
      where 
        pick (a,_,_) = a
    search = U.bfs goBack [p]
    res = minimum . map (distance (0,0,0,0)) $ M.keys search

solve :: [String] -> (String, String)
solve = parse ^>> solve1 &&& solve2
