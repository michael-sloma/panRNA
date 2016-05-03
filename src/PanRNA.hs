module PanRNA where

import qualified Text.Parsec as P
import Text.Parsec ((<?>), (<|>))
import Data.Maybe
import Data.List
import qualified Data.HashMap.Strict as HM

data Tag = Tag String deriving Show
data Sequence = Sequence String deriving Show
data Structure = Structure [(Int,Int)] deriving Show
data RNA = RNA Tag Sequence Structure deriving Show
data Pair = Unpaired | Pair Int deriving (Show, Eq)
data Index = Index Int deriving (Show, Eq)

parse rule text = P.parse rule "(debug)" text

opt parser defaultVal = P.try parser <|> (return defaultVal)

nucLine :: P.Parsec String () String
nucLine = P.many $ oneNucleotide

oneNucleotide = P.oneOf "AUGCTNXaugctnx"

whitespace = P.oneOf " \t"

whitespaces = P.many whitespace
whitespaces1 = P.many1 whitespace

eol = whitespaces >> P.newline

int = P.many1 P.digit

endSequence = eol <|> whitespace <|> P.char '1'

emptyStructure = Structure []
emptyTag = Tag ""

nucleotides :: P.Parsec String () Sequence
nucleotides = do s <- P.sepEndBy nucLine endSequence
                 return . Sequence $ concat s

faTag :: P.Parsec String () Tag                 
faTag = do P.char '>' >> P.spaces
           t <- P.manyTill P.anyChar eol
           return $ Tag t

dotSeqTag :: P.Parsec String () Tag
dotSeqTag = do P.manyTill P.anyChar (P.char ';') >> eol
               t <- P.manyTill P.anyChar eol
               return $ Tag t

parseSequence :: P.Parsec String () Tag -> P.Parsec String () RNA
parseSequence tagParser = do t <- tagParser
                             s <- nucleotides
                             return $ RNA t s emptyStructure

ctLine :: P.Parsec String st (Index, Char, Pair)
ctLine = do i <- int
            n <- whitespaces1  >> oneNucleotide
            whitespaces1  >> int >> whitespaces1  >> int
            j <- whitespaces1  >> int
            whitespaces1  >> int >> eol
            return (Index $ read i,
                    n,
                    if j=="0" then Unpaired else Pair $ read j)

ctTag :: P.Parsec String st Tag
ctTag = do (P.try $ P.spaces >> int >> whitespaces1) <|> whitespaces
           t <- P.manyTill (P.anyChar <|> P.digit) eol
           return (Tag t)

fasta = P.many1 $ parseSequence faTag

dotSeq = P.many1 $ parseSequence dotSeqTag

ct = P.many1 $
       do t <- ctTag
          c <- P.many1 ctLine
          return $ RNA t (toSeq c) (toStructure c)
            where toPair (Index i, _, Pair j) = Just (i,j)
                  toPair (Index i, _, Unpaired) = Nothing
                  toSeq = Sequence . map (\(_, n, _) -> n) 
                  toStructure = Structure . mapMaybe toPair

dbOpen = "(>{["
dbClose = ")<}]"
dbOther = ".,_-"
dbChar = P.oneOf (dbOpen ++ dbClose ++ dbOther)

db :: P.Parsec String st Structure
db = do l <- P.manyTill dbChar (P.skipMany1 eol <|> P.eof)
        return $ toStructure l
          where
          toStructure = Structure . getF . foldl' step ([], 1, [])
          getF (a, _, _) = a
          step (pair, pos, stk) c
           | c `elem` dbClose, null stk = error "unbalanced parens in db"
           | c `elem` dbOpen = (pair, pos+1, pos:stk)
           | c `elem` dbOther = (pair, pos+1, stk)
           | c `elem` dbClose = ( (head stk, pos):pair, pos+1,  tail stk)
           | otherwise = error $ "unrecognized character " ++ [c] ++ " in dot-bracket string"

viennaOutput = P.many1 $ do
                  t <- opt faTag emptyTag
                  n <- nucleotides
                  s <- db
                  return $ RNA t n s

writeDotSeq (RNA (Tag t) (Sequence s) _) = unlines [";", t, s]

writeFaSeq (RNA (Tag t) (Sequence s) _) = unlines [">"++t, s]

pairs :: [(Int,Int)] -> HM.HashMap Int Int
pairs = HM.fromList
