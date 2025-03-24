module DocReader where

import Control.Monad
import Data.Text (Text, unpack)
import Pdf.Document

type PdfText = IO Text

readDoc :: String -> IO ()
readDoc nameOfFile = withPdfFile nameOfFile $ \pdf -> do
    doc <- document pdf
    catalog <- documentCatalog doc
    rootNode <- catalogPageNode catalog
    count <- pageNodeNKids rootNode
    page <- pageNodePageByNum rootNode 0
    text <- pageExtractText page
    print $ tokenizer (show text)

concat_to_first_in_last :: Char -> [String] -> [String]
concat_to_first_in_last c [] = [[c]]
concat_to_first_in_last c (x:xs) = ([c] ++ x) : xs

tokenizer :: String -> [String]
tokenizer "" = []
tokenizer (' ':xs) = []:(tokenizer xs)
tokenizer (x:xs) = concat_to_first_in_last x (tokenizer xs)
