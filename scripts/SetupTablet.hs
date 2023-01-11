module Main where

import Control.Monad (forM_)
import System.Process (spawnProcess)
import System.Environment (getArgs)

---

tabletLayout :: [String]
tabletLayout =
    [ "ctrl -" , "e" , "ctrl ="
    , "ctrl z" , "e" , "ctrl shift z"
    ]

---

setupMappings :: String -> IO ()
setupMappings deviceName = do
    forM_ mappings $ \(button, key) -> do
        let args = ["--set", deviceName, "Button", show button, "key " ++ key]
        spawnProcess "xsetwacom" args
  where
    mappings = [1, 2, 3, 8, 9, 10] `zip` tabletLayout

main :: IO ()
main = do
    -- get first command line argument
    args <- getArgs
    case args of
        [deviceName] -> setupMappings deviceName
        _ -> putStrLn "usage: SetupTablet [deviceName]"
