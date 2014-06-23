--------------------------------------------------------------------------------
-- Module      :  XMonad.Prompt.Terminator
-- Copyright   :  Rui Damas, 2014
-- License     :  GNU/GLP3
--
-- Maintainer  :  rui.damas@gmail.com
-- Stability   :  unstable
-- Portability :  portable
--
-- | Provides .
--------------------------------------------------------------------------------
module XMonad.Prompt.Terminator (abManPrompt, abRunInTerm, Terminator) where
--
import XMonad (io, spawn, terminal, X)
--
import XMonad.Prompt (mkComplFunFromList, mkXPrompt, showXPrompt, uniqSort, XPrompt, XPConfig)
import XMonad.Prompt.Man (getCommandOutput)
import XMonad.Prompt.Shell (split)
--
import System.Directory (doesDirectoryExist, getDirectoryContents)
import qualified Control.Exception.Extensible as E
import Control.Monad (forM)
import Data.List (isSuffixOf)
import Data.Maybe (catMaybes)
--
import XMonad.Config.ABC (abCRDrawer, abCRTallFloatCE)
--
data Man = Man

instance XPrompt Man where
    showXPrompt Man = "Manual: "

abTDisplayProfile = "abxm-display"

data Terminator = Terminator {
    tExec    :: String
  , tRole    :: String
  , tProfile :: String
  , tOptions :: String
  }
--
terminator = Terminator {
    tExec    = "terminator"
  , tRole    = "default"
  , tProfile = tRole terminator
  , tOptions = ""
  }
--
gksuTerminator = terminator {tExec = "gksu " ++ tExec terminator}
--
--
terminatorInDrawer = terminator {
    tRole    = abCRDrawer
  , tProfile = tRole terminatorInDrawer
  }
--
gksuTerminatorInDrawer = terminatorInDrawer {tExec = tExec gksuTerminator}
--
--
terminatorDisplay = terminator {
    tRole    = abCRTallFloatCE
  , tProfile = abTDisplayProfile
  , tOptions = ""
  }
--
gksuTerminatorDisplay = terminatorDisplay {tExec = tExec gksuTerminator}
--
--
inTerminator t o c = inTerminatorPrefix t o ++ "\" -e \"" ++ c ++ "\""
--
inTerminatorSu t o c = inTerminatorPrefix t o ++ "\" -x su -c \"" ++ c ++ "\""
--
inTerminatorPrefix t o = tExec t ++ " " ++ tOptions t ++ " " ++ o
  ++ " -r \"" ++ tRole t
  ++ "\" -p \""  ++ tProfile t
--
--
runInTerminator :: Terminator -> String -> String -> X ()
runInTerminator t o c = spawn $ inTerminator t o c
--
abRunInTerm = runInTerminator
--
--
-- abSSHPrompt :: [String] -> XPConfig -> X ()
-- abInTerminatorPrompt :: [String] -> XPConfig -> X ()
-- abInTerminatorSuPrompt :: [String] -> XPConfig -> X ()
--
abManPrompt :: XPConfig -> X ()
abManPrompt c = do
  mans <- io getMans
  mkXPrompt Man c (manCompl mans) $
    runInTerminator terminatorDisplay "" . (++) "man "
--
-------------------------------------------------------------------------------
-- not exported...
--
-- copied from source: XMonad.Prompt.Man (wheezy)
--
getMans :: IO [String]
getMans = do
  paths <- getCommandOutput "manpath -g 2>/dev/null" `E.catch`
            \(E.SomeException _) -> return []
  let sects    = ["man" ++ show n | n <- [1..9 :: Int]]
      dirs     = [d ++ "/" ++ s | d <- split ':' paths, s <- sects]
  mans <- forM dirs $ \d -> do
            exists <- doesDirectoryExist d
            if exists
              then map (stripExt . stripSuffixes [".gz", ".bz2"]) `fmap`
                   getDirectoryContents d
              else return []
  return $ uniqSort $ concat mans

manCompl :: [String] -> String -> IO [String]
manCompl mans s | s == "" || last s == ' ' = return []
                | otherwise                = do
  -- XXX readline instead of bash's compgen?
  f <- lines `fmap` getCommandOutput ("bash -c 'compgen -A file " ++ s ++ "'")
  mkComplFunFromList (f ++ mans) s

stripExt :: String -> String
stripExt = reverse . drop 1 . dropWhile (/= '.') . reverse

stripSuffixes :: Eq a => [[a]] -> [a] -> [a]
stripSuffixes sufs fn =
    head . catMaybes $ map (flip rstrip fn) sufs ++ [Just fn]

rstrip :: Eq a => [a] -> [a] -> Maybe [a]
rstrip suf lst
    | suf `isSuffixOf` lst = Just $ take (length lst - length suf) lst
    | otherwise            = Nothing

