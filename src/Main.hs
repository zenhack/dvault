module Main where

import Control.Monad      (void)
import System.Directory   (createDirectoryIfMissing, listDirectory)
import System.Environment (getEnv)
import System.IO          (hClose, hPutStr)
import System.Process
    ( CreateProcess(..)
    , StdStream(CreatePipe)
    , createProcess
    , proc
    , readProcess
    , waitForProcess
    )

import qualified Data.ByteString as B

vaultDir :: IO FilePath
vaultDir = do
    home <- getEnv "HOME"
    let path = home ++ "/.local/dvault"
    createDirectoryIfMissing True path
    return path

dmenu :: [String] -> IO String
dmenu options = fmap stripNewline $ readProcess "dmenu" [] $ unlines options
  where
    stripNewline = takeWhile (/= '\n')

xclip :: String -> IO ()
xclip contents = do
    xclipI ["-sel", "clip"]
    xclipI []
  where
    xclipI args = do
        -- For some reason I don't understand, just doing readProcess hangs.
        let p = (proc "xclip" ("-i":args)) { std_in = CreatePipe }
        (Just inpipe, Nothing, Nothing, pid) <- createProcess p
        hPutStr inpipe contents
        hClose inpipe
        void $ waitForProcess pid

notify :: String -> IO ()
notify summary = void $ readProcess "notify-send" ["-t", "2000", summary] ""

main = do
    dir <- vaultDir
    sites <- map (`dropSuffix` theSuffix)
                <$> filter (`endsWith` theSuffix)
                <$> listDirectory dir
    selection <- dmenu sites
    ciphertext <- readFile (dir ++ "/" ++ selection ++ theSuffix)
    plaintext <- readProcess "gpg" ["-a", "-d"] ciphertext
    xclip plaintext
    notify $ "Password for " ++ selection ++ " copied to clipboard."
  where
    theSuffix = ".pass.asc"
    str    `endsWith` suffix | str == suffix = True
    []     `endsWith` _ = False
    (c:cs) `endsWith` suffix = cs `endsWith` suffix

str    `dropSuffix` suffix | str == suffix = []
[]     `dropSuffix` _ = []
(c:cs) `dropSuffix` suffix = c:(cs `dropSuffix` suffix)
