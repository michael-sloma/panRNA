{-# LANGUAGE DeriveDataTypeable #-}
module Main where
 
import System.Console.CmdArgs
import qualified PanRNA as P

data Args = Args {inFormat :: String,
                  outFormat :: String,
                  commentChar :: String,
                  removeNoncanonical :: Bool,
                  convertAmbiguous :: String,
                  select :: String
                  } deriving (Show, Data, Typeable)

convert args = getResult .
               fmap (writeOut $ outFormat args) .
               fmap (map (postprocess args)) .
               fmap (getIndex args) . 
               P.parse (readIn $ inFormat args)
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

getResult (Right f) = f
getResult (Left e) = error $ "parse error: " ++ show e

preprocess a = foldl (.) id [comment a]
  where comment a | "" <- commentChar a = id
                  | (x:[]) <- commentChar a = P.removeComments x
                  | otherwise = error "only a single character to indicate a comment is supported"

postprocess a = foldl (.) id [rmvNc a, ambiguous a]
  where rmvNc a | False <- removeNoncanonical a = id
                | otherwise  = P.removeNoncanonical
        ambiguous a | "" <- convertAmbiguous a = id
                    | (x:[]) <- convertAmbiguous a = P.convertAmbiguous x
                    | otherwise = error "only a single character to indicate an ambigous nucleotide is supported"

defaults = Args {inFormat = "fasta" &= help "input format: one of plain, fasta, seq, ct, vienna (default fasta)",
                 outFormat = "fasta" &= help "output format: one of plain, fasta, seq, ct, vienna (default fasta)",
                 commentChar = "" &= help "comment char: a character to indicate the beginning of a comment (default none)" &= opt "",
                 removeNoncanonical = False &= help "remove noncanonical: remove pairs that are not AU, GC or GU",
                 convertAmbiguous = "" &= help ": convert ambiguous: convert nucleotide ambiguity codes (N, X, Y, R, S, W, K, M, B, D, H, V) to the specified character",
                 select = "" &= help "the select of a particular RNA to convert, starting from 1 (default: convert all RNAs in the file)"
                 }

main = do a <- cmdArgs defaults
          interact $ convert a . preprocess a
