module Main where

import System.IO
import Lib
import Excel

main = do
    putStrLn "Hi"
    line <- getLine
    case line of
        "q" -> do
            putStrLn "bye"
        "n" -> do
            loop $ newS []
            main
        "l" -> do
            putStrLn "file name: "
            line <- getLine
            handle <- openFile line ReadMode
            contents <- hGetContents handle
            let a = read contents :: Spreadsheet
            loop a
            hClose handle
        otherwise -> do
            putStrLn "no"
            main


loop state = do
    line <- getLine
    case line of
        "a" -> do
            line <- getLine
            loop (newS [((0,0), cStr "a")])
        "s" -> do
            putStrLn $ show state
            loop state
        "w" -> do
            putStrLn "file name: "
            line <- getLine
            handle <- openFile line WriteMode
            hPutStr handle $ show state
            putStrLn "saved"
            hClose handle
            loop state
        "q" -> do
            putStrLn "bye"
        otherwise -> do
            putStrLn "no"
            loop state
