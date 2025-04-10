module Utils where

import Control.Concurrent

---------- MVARS

printMVar var = takeMVar var >>= putStrLn

create_emtpy_mvars :: Int -> IO [MVar a]
create_emtpy_mvars 0 = return []
create_emtpy_mvars n = do
       new_empty_mvar <- newEmptyMVar
       list <- create_emtpy_mvars (n - 1)
       let return_list = (new_empty_mvar):(list)
       return return_list

mvar_list_to_list :: [MVar a] -> IO ([a])
mvar_list_to_list [] = return []
mvar_list_to_list (mvar_x:mvar_xs) = do
       x <- takeMVar mvar_x
       xs <- mvar_list_to_list mvar_xs
       return (x:xs)

---------- FILES

is_file_type :: String -> String -> Bool
is_file_type [] _ = True
is_file_type _ [] = False
is_file_type xs ys = fits_at_ys_start (reverse $ "." ++ xs) (reverse ys)

---------- STRING MANIPULATION

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

remove_sequence_of_str = rsstr
rsstr :: [String] -> String -> String
rsstr [] xs = xs
rsstr _ [] = []
rsstr ((x':[]):[]) (y:ys) = if x' == y then ys else y:(rsstr ((x':[]):[]) ys)
rsstr ((x':[]):xs) (y:ys) = if x' == y then
                rsstr xs ys else y:(rsstr ((x':[]):xs) ys)
rsstr ((x':xs'):[]) (y:ys) = if x' == y then ys else y:(rsstr (xs':[]) ys)
rsstr ((x':xs'):xs) (y:ys) = if fits_at_ys_start (x':xs') (y:ys)
                then rsstr (xs':xs) ys else y:(rsstr ((x':xs'):xs) ys)

---------- OTHERS

not_empty = not . null :: [a] -> Bool