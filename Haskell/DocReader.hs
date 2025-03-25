module DocReader where

import Control.Monad
import Data.Text (Text, unpack)
import Pdf.Document

-- :set -package text
type PdfText = IO Text

tokenizeDoc :: String -> IO ([String])
tokenizeDoc nameOfFile = withPdfFile nameOfFile $ \pdf -> do
    doc <- document pdf
    catalog <- documentCatalog doc
    rootNode <- catalogPageNode catalog
    count <- pageNodeNKids rootNode
    page <- pageNodePageByNum rootNode 0
    text <- pageExtractText page
    return (map (remove_string "\"") (map (remove_string_sequence ["\\", "n"]) (tokenizer $ show text)))

concat_to_first_in_last :: Char -> [String] -> [String]
concat_to_first_in_last c [] = [[c]]
concat_to_first_in_last c (x:xs) = ([c] ++ x) : xs

tokenizer :: String -> [String]
tokenizer "" = []
tokenizer (' ':[]) = []
tokenizer (' ':xs) = if ((head xs) == ' ') then tokenizer xs else []:(tokenizer xs)
tokenizer (x:xs) = concat_to_first_in_last x (tokenizer xs)

fits_at_ys_start :: String -> String -> Bool
fits_at_ys_start [] _ = True
fits_at_ys_start _ [] = False
fits_at_ys_start (x:xs) (y:ys) = if x == y then fits_at_ys_start xs ys else False

remove_string :: String -> String -> String
remove_string [] xs = xs
remove_string _ [] = []
remove_string (x:[]) (y:ys) = if x == y then ys else y:(remove_string (x:[]) ys)
remove_string (x:xs) (y:ys) = if fits_at_ys_start (x:xs) (y:ys)
                              then remove_string xs ys else y:(remove_string (x:xs) ys)

remove_string_sequence :: [String] -> String -> String
remove_string_sequence [] xs = xs
remove_string_sequence _ [] = []
remove_string_sequence ((x':[]):[]) (y:ys) = if x' == y then ys else y:(remove_string_sequence ((x':[]):[]) ys)
remove_string_sequence ((x':[]):xs) (y:ys) = if x' == y then
                remove_string_sequence xs ys else y:(remove_string_sequence ((x':[]):xs) ys)
remove_string_sequence ((x':xs'):[]) (y:ys) = if x' == y then ys else y:(remove_string_sequence (xs':[]) ys)
remove_string_sequence ((x':xs'):xs) (y:ys) = if fits_at_ys_start (x':xs') (y:ys)
                then remove_string_sequence (xs':xs) ys else y:(remove_string_sequence ((x':xs'):xs) ys)