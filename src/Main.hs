{-# LANGUAGE DeriveDataTypeable #-}
module Main where
 
import System.Console.CmdArgs
import qualified PanRNA as P

data Args = Args {inFormat :: String,
                  outFormat :: String,
                  commentChar :: String,
                  select :: String
                  } deriving (Show, Data, Typeable)

convert args = fmap (writeOut $ outFormat args) . fmap (getIndex args) . P.parse (readIn $ inFormat args)
  where readIn "fasta" = P.fasta
        readIn "seq" = P.dotSeq
        readIn "vienna" = P.viennaOutput
        readIn "ct" = P.ct
        readIn "plain" = P.plain
        readIn e = error $ "unrecognized input format: '" ++ e ++ "'"
        writeOut "fasta" = concatMap P.writeFaSeq
        writeOut "seq" = concatMap P.writeDotSeq
        writeOut "ct" = concatMap P.writeCt
        writeOut "vienna" = concatMap P.writeDb
        writeOut "plain" = concatMap P.writePlain
        writeOut e = error $ "unrecognized output format: '" ++ e ++ "'"
        getIndex a | "" <- select a = id
                   | otherwise = P.filterByIndex (read $ select a)

convFun (Right f) = f
convFun (Left e) = error $ "parse error: " ++ show e

preprocess a = foldl (.) id [comment a]
  where comment a | "" <- commentChar a = id
                  | (x:[]) <- commentChar a = P.removeComments x
                  | otherwise = error "only a single character to indicate a comment is supported"

defaults = Args {inFormat = "fasta" &= help "input format: one of plain, fasta, seq, ct, vienna (default fasta)",
                 outFormat = "fasta" &= help "output format: one of plain, fasta, seq, ct, vienna (default fasta)",
                 commentChar = "" &= help "comment char: a character to indicate the beginning of a comment (default none)" &= opt "",
                 select = "" &= help "the select of a particular RNA to convert, starting from 1 (default: convert all RNAs in the file)" &= opt ""
                 }

main = do a <- cmdArgs defaults
          interact $ convFun . convert a . preprocess a
