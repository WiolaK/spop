import Excel

import System.IO
import System.Exit
import System.Directory
import Data.List
import Text.Printf

main = do
  putStrLn "Arkusz kalkulacyjny"
  menu

menu = do
  putStrLn ""
  putStrLn "Menu"
  putStrLn "Wybierz jedną z poniższych opcji: "
  putStrLn " 1 - utwórz nowy arkusz"
  putStrLn " 2 - otwórz istniejący arkusz"
  putStrLn " 3 - usuń arkusz"
  putStrLn " 4 - wyświetl arkusz"
  putStrLn " e - wyjście z programu"
  putStr   "$ "
  a <- getLine
  let x = if length a > 0 then (a !! 0) else ' '
  case x of
    '1' -> do 
      putStrLn "Nazwa arkusza, który chcesz utworzyć:"
      temp <- getLine
      let name = temp ++ ".spop"
      writeFile name ""
      putStrLn ("Arkusz o nazwie " ++ name ++ " został utworzony")
      return 0
    '2' -> do 
      putStrLn "Nazwa istniejącego arkusza, który chcesz otworzyć:"
      temp <- getLine
      let name = temp ++ ".spop"
      print name
-- wywyłanie funkcji do otwierania arkusza
      arkusz <- otworzArkusz name 
      print arkusz
      arkusz_menu <- menuArkusz name arkusz
      print arkusz_menu
      return 0
    '3' -> do
      putStrLn "Nazwa arkusza, który chcesz usunąć:"
      temp <- getLine
      let name = temp ++ ".spop"
      removeFile name
      putStrLn ("Arkusz o nazwie " ++ name ++ "został usunięty")
      return 0
    '4' -> do
      putStrLn "Dostępne do wyświetlenia arkusze: "
      allindirectory <- getDirectoryContents "."
      let filtered = filter (isSuffixOf ".spop") allindirectory
      print filtered
      return 0
    'e' -> do 
      putStrLn  "Dziękujemy za skorzystanie z programu. Do zobaczenia :)"
      return 0
    _ -> do 
      putStrLn "Przepraszamy - nie ma takiej opcji. "
      return 0
  if x /= 'e' then 
    menu
  else
    return 0

menuArkusz :: String -> Arkusz -> IO (Arkusz)
menuArkusz nazwa arkusz = do
  putStrLn ""
  putStrLn ("Menu - dostępne opcje edycji arkusza o nazwie: " ++ nazwa)
  putStrLn "Wybierz jedną z poniższych opcji: "
  putStrLn "1 - wyświetl arkusz"
  putStrLn "2 - edytuj dane z podanej komórki"
  putStrLn "3 - zapisz arkusz"
  putStrLn "x - wróć do menu głównego"
  a <- getLine
  let x = if length a > 0 then (a !! 0) else ' '
  case x of
    '1' -> do
      print arkusz
      return 0
    '2' -> do
      putStrLn "Podaj wiersz: "
      wiersz <- getLine
      putStrLn "Podaj kolumnę: "
      kolumna <- getLine
 -- wywołanie funkjci do edycji komórki arkusza
      return 0
    '3' -> do
      return 0
    'x' -> do
      return 0
    _ -> do
      putStrLn "Przepraszamy - nie ma takiej opcji."
      return 0
  if x /= 'x' then 
    menuArkusz nazwa arkusz
  else
    return arkusz
