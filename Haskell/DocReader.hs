module DocReader where

import Control.Monad
import Data.Text (Text, unpack)
import Pdf.Document

-- Obs: you gotta use ':set -package text' before loading

------------ External

type PdfText = IO Text
--type PageNode = Pdf.Document.PageNode

tokenizeDoc :: String -> IO ([String])
tokenizeDoc nameOfFile = withPdfFile nameOfFile $ \pdf -> do
    doc <- document pdf
    catalog <- documentCatalog doc
    rootNode <- catalogPageNode catalog
    count <- pageNodeNKids rootNode
    text <- tokenizePages rootNode (count - 1)
    return $ map clean_str (tokenizer $ show text)

tokenizer :: String -> [String]
tokenizer "" = []
tokenizer (' ':[]) = []
tokenizer (' ':xs) = if ((head xs) == ' ') then tokenizer xs else []:(tokenizer xs)
tokenizer (x:xs) = conct_to_head x (tokenizer xs)

------------ Internal

clean_str :: String -> String
clean_str s = remove_str "\\" $ remove_str "\"" $ (remove_sequence_of_str ["\\", "n"] s)

conct_to_head :: Char -> [String] -> [String]
conct_to_head c [] = [[c]]
conct_to_head c (x:xs) = ([c] ++ x) : xs

-- whether X appears as a substring in the start of Y
fits_at_ys_start :: String -> String -> Bool
fits_at_ys_start [] _ = True
fits_at_ys_start _ [] = False
fits_at_ys_start (x:xs) (y:ys) = if x == y then fits_at_ys_start xs ys else False

remove_str :: String -> String -> String
remove_str [] xs = xs
remove_str _ [] = []
remove_str (x:[]) (y:ys) = if x == y then ys else y:(remove_str (x:[]) ys)
remove_str (x:xs) (y:ys) = if fits_at_ys_start (x:xs) (y:ys)
                              then remove_str xs ys else y:(remove_str (x:xs) ys)

remove_sequence_of_str :: [String] -> String -> String
remove_sequence_of_str [] xs = xs
remove_sequence_of_str _ [] = []
remove_sequence_of_str ((x':[]):[]) (y:ys) = if x' == y then ys else y:(remove_sequence_of_str ((x':[]):[]) ys)
remove_sequence_of_str ((x':[]):xs) (y:ys) = if x' == y then
                remove_sequence_of_str xs ys else y:(remove_sequence_of_str ((x':[]):xs) ys)
remove_sequence_of_str ((x':xs'):[]) (y:ys) = if x' == y then ys else y:(remove_sequence_of_str (xs':[]) ys)
remove_sequence_of_str ((x':xs'):xs) (y:ys) = if fits_at_ys_start (x':xs') (y:ys)
                then remove_sequence_of_str (xs':xs) ys else y:(remove_sequence_of_str ((x':xs'):xs) ys)

tokenizePages :: PageNode -> Int -> IO String
tokenizePages rootNode (-1) = return ""
tokenizePages rootNode count = do
                                txt1 <- (tokenizePages rootNode (count-1))
                                page <- pageNodePageByNum rootNode count
                                txt2 <- (pageExtractText page)
                                return $ txt1 ++ (show txt2)
