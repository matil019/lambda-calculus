-- | The REPL for interactive term reduction.
module LambdaCalculus.Main.Repl where

import Conduit ((.|), mapM_C, runConduit, yield)
import Control.Monad.Extra (unlessM)
import LambdaCalculus.SimplyTyped.DeBruijn (formatTerm, parseTerm, reduceSteps)
import System.IO (hFlush, hPutStrLn, isEOF, stderr, stdout)

-- | The REPL.
--
-- Parses each line input ('parseTerm'), reduces it until termination
-- ('reduceSteps') and pretty-prints it ('formatTerm').
main :: IO ()
main = do
  putStr "> "
  hFlush stdout
  unlessM isEOF $ do
    s <- getLine
    case s of
      "" -> pure ()
      _ | Just m <- parseTerm s -> runConduit $ (yield m >> reduceSteps m) .| mapM_C (putStrLn . formatTerm)
        | otherwise -> hPutStrLn stderr $ "cannot parse: " <> s
    main
