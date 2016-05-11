{-# LANGUAGE DeriveDataTypeable #-}
module Main where
 
import System.Console.CmdArgs
import PanRNA

data Args = Args {inFormat :: String,
                  outFormat :: String}
                  deriving (Show, Data, Typeable)

convert inp outp = fmap (writeOut outp) . parse (readIn inp)
  where readIn "fasta" = fasta
        readIn "seq" = dotSeq
        readIn "vienna" = viennaOutput
        readIn "ct" = ct
        readIn e = error $ "unrecognized input format: '" ++ e ++ "'"
        writeOut "fasta" = concatMap writeFaSeq
        writeOut "seq" = concatMap writeDotSeq
        writeOut "ct" = concatMap writeCt
        writeOut "db" = concatMap writeDb
        writeOut e = error $ "unrecognized output format: '" ++ e ++ "'"

convFun (Right f) = f
convFun (Left e) = error $ "parse error: " ++ show e

defaults = Args {inFormat = "fasta" &= help "input format: one of plain, fasta, seq, ct, vienna (default fasta)",
                 outFormat = "fasta" &= help "output format: one of plain, fasta, seq, ct, vienna (default fasta)"}

main = do a <- cmdArgs defaults
          interact $ convFun . convert (inFormat a) (outFormat a)
