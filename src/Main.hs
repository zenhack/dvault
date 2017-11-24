{-|
Module: Main
Description: dvault command line tool.
-}
module Main where

import Control.Monad      (void)
import Data.Default       (def)
import Generate           (generate)
import System.Directory   (createDirectoryIfMissing, listDirectory)
import System.Environment (getArgs, getEnv)
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

-- | Get the directory in which to store our data.
getVaultDir :: IO FilePath
getVaultDir = do
    -- It's not entirely obvious to me(zenhack) what's the "right" place to
    -- put our data; the xdg base spec mentions an $XDG_DATA_HOME that falls
    -- back to $HOME/.local/share, but other programs put stuff in ~/.local
    -- outside of ~/.local/share, and ~/.local/share doesn't feel right.
    -- If this were system wide, the right place would probably be /var/lib,
    -- but there's no xdg homedir equivalent. For now, we just do ~/.local
    home <- getEnv "HOME"
    let path = home ++ "/.local/dvault"
    createDirectoryIfMissing True path
    return path

-- | @'dmenu' options@ launches the dmenu(1) command, supplying it with
-- @options@, and returning the user's choice.
dmenu :: [String] -> IO String
dmenu options = fmap stripNewline $ readProcess "dmenu" [] $ unlines options
  where
    stripNewline = takeWhile (/= '\n')

-- | @'xclip' foo@ copies the string @foo@ to both x11 clipboards.
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

-- | Send a desktop notification
notify :: String -> IO ()
notify summary = void $ readProcess "notify-send" ["-t", "2000", summary] ""

main = do
    args <- getArgs
    dir <- getVaultDir
    case args of
        []            -> fetchPass dir
        ["gen", name] -> newPass $ passFilename dir name
        _             -> putStrLn "Usage: dvault [ gen <tag> ]"

-- | Generate a new password and save it (encrypted) to the supplied path.
newPass :: FilePath -> IO ()
newPass path = do
    plaintext <- generate def
    ciphertext <- readProcess "gpg" ["-a", "-e"] plaintext
    writeFile path ciphertext

-- | Prompt the user for a password label given the available passwords in
-- directory @dir@, then decrypt then user's choice and copy it to the
-- x11 clipboards.
fetchPass dir = do
    sites <- map (`dropSuffix` theSuffix)
                <$> filter (`endsWith` theSuffix)
                <$> listDirectory dir
    selection <- dmenu sites
    ciphertext <- readFile (dir ++ "/" ++ selection ++ theSuffix)
    plaintext <- readProcess "gpg" ["-a", "-d"] ciphertext
    xclip plaintext
    notify $ "Password for " ++ selection ++ " copied to clipboard."
  where
    str    `endsWith` suffix | str == suffix = True
    []     `endsWith` _ = False
    (c:cs) `endsWith` suffix = cs `endsWith` suffix

str    `dropSuffix` suffix | str == suffix = []
[]     `dropSuffix` _ = []
(c:cs) `dropSuffix` suffix = c:(cs `dropSuffix` suffix)

-- | Suffix to append to the password's label to form a filename.
theSuffix = ".pass.asc"

-- | @'passFilename' vaultdir label@ is the file in which the password for @label@
-- is stored, given the storage directory @vaultdir@.
passFilename :: String -> String -> FilePath
passFilename dir name = dir ++ "/" ++ name ++ theSuffix
