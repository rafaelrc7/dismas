module Main where
import           Settings (parseCLIArgs)

main :: IO ()
main = do
  settings <- parseCLIArgs
  print settings

