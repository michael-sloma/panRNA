{-# LANGUAGE DeriveDataTypeable #-}
module Main where
 
import System.Console.CmdArgs
import qualified PanRNA as P

data Args = Args {inFormat :: String,
                  outFormat :: String,
                  commentChar :: String
                  } deriving (Show, Data, Typeable)

convert inp outp = fmap (writeOut outp) . P.parse (readIn inp)
  where readIn "fasta" = P.fasta
        readIn "seq" = P.dotSeq
        readIn "vienna" = P.viennaOutput
        readIn "ct" = P.ct
        readIn e = error $ "unrecognized input format: '" ++ e ++ "'"
        writeOut "fasta" = concatMap P.writeFaSeq
        writeOut "seq" = concatMap P.writeDotSeq
        writeOut "ct" = concatMap P.writeCt
        writeOut "db" = concatMap P.writeDb
        writeOut e = error $ "unrecognized output format: '" ++ e ++ "'"

convFun (Right f) = f
convFun (Left e) = error $ "parse error: " ++ show e

preprocess a | "" <- commentChar a = id
             | (x:[]) <- commentChar a = P.removeComments x
             | otherwise = error "only a single character to indicate a comment is supported"

defaults = Args {inFormat = "fasta" &= help "input format: one of plain, fasta, seq, ct, vienna (default fasta)",
                 outFormat = "fasta" &= help "output format: one of plain, fasta, seq, ct, vienna (default fasta)",
                 commentChar = "" &= help "comment char: a character to indicate the beginning of a comment (default none)" &= opt ""
                 }

main = do a <- cmdArgs defaults
          interact $ convFun . convert (inFormat a) (outFormat a) . preprocess a
