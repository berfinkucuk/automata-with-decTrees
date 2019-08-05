import System.IO
import Data.Char
import Dta

main = do
  contents <- readFile "UnicodeData.txt"
  --putStr ( branchUnicode ["56","6"] )
  linebyline <- readTxtUnicode contents
  let uniLines = toUnicodeClass linebyline
  let branches = extractBranches uniLines --no memory issue till here as expected
  putStr( show( branches))
