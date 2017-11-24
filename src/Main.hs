{-|
Module: Main
Description: dvault command line tool.
-}
module Main where

import Generate

import Control.Monad      (void)
import Data.Default       (def)
import Data.Maybe         (fromMaybe)
import System.Directory   (createDirectoryIfMissing, listDirectory)
import System.Environment (getArgs, getEnv)
import System.Exit        (exitFailure, exitSuccess)
import System.IO          (hClose, hPutStr)
import System.Process
    ( CreateProcess(..)
    , StdStream(CreatePipe)
    , createProcess
    , proc
    , readProcess
    , waitForProcess
    )
import Text.Read          (readMaybe)

import qualified Data.ByteString as B
import qualified Data.Set        as S

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
        ["-h"] -> usage >> exitSuccess
        ["--help"] -> usage >> exitSuccess
        []                -> fetchPass dir
        [arg] | arg `elem` ["-h", "--help"] -> usage >> exitSuccess
        ("gen":name:args)
            | name `elem` ["-h", "--help"] -> usage >> exitSuccess
            | otherwise -> do
                opts <- genCmd Nothing S.empty args
                newPass opts (passFilename dir name)
        _ -> usage >> exitFailure

usage = putStrLn $ unlines
    [ "Usage: dvault [ gen <tag> [ CHARSET... ] | -h | --help ]"
    , ""
    , "where CHARSET is one of:"
    , ""
    , "    --letters"
    , "    --upper"
    , "    --lower"
    , "    --digits"
    , "    --symbols"
    , ""
    , "Note that --letters cannot be combined with --upper or --lower."
    ]

genCmd :: Maybe Int -> S.Set (S.Set Char) -> [String] -> IO Options
genCmd _ sets _ | seq sets False = undefined -- force `sets` to be evaluated.
genCmd argSize sets []
    | letter `S.member` sets &&
        (lower `S.member` sets || upper `S.member` sets)
        = do
            putStrLn $ "Error : You cannot specify both --letters and " ++
                "either --upper or --lower."
            usage >> exitFailure
    | otherwise = return
        Options
            { size = size def `fromMaybe` argSize
            , charSets = if S.null sets
                then charSets def
                else S.toList sets
            }
genCmd Nothing sets ("--size":size:args) = case readMaybe size of
    Just size' -> genCmd (Just size') sets args
    Nothing    -> usage >> exitFailure
genCmd (Just size) _ ("--size":_) = usage >> exitFailure
genCmd size sets ("--letters":args) =
    genCmd size (S.insert letter sets) args
genCmd size sets ("--upper":args) =
    genCmd size (S.insert upper sets) args
genCmd size sets ("--lower":args) =
    genCmd size (S.insert lower sets) args
genCmd size sets ("--digits":args) =
    genCmd size (S.insert digit sets) args
genCmd size sets ("--symbols":args) =
    genCmd size (S.insert symbol sets) args
genCmd _ _ ("--help":_) = usage >> exitSuccess
genCmd _ _ ("-h":_) = usage >> exitSuccess
genCmd _ _ _ = usage >> exitFailure


-- | Generate a new password and save it (encrypted) to the supplied path.
newPass :: Options -> FilePath -> IO ()
newPass opts path = do
    plaintext <- generate opts
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
