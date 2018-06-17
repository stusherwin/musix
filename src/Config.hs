module Config (
  getConfig
) where

import System.Directory ( doesFileExist )
import Text.Read ( readMaybe )
import Data.Maybe ( catMaybes )
import Data.List.Split ( splitOn )

type Config = [(String, (Int, Int))]

parseConfig :: String -> Config
parseConfig = catMaybes . map (parseLine . splitOn "|") . lines
  where
  parseLine (name:firstKey:lastKey:_) =
    case (readMaybe firstKey :: Maybe Int, readMaybe lastKey :: Maybe Int) of
      (Just fk, Just lk) -> Just (name, (fk, lk))
      _ -> Nothing
  parseLine _ = Nothing

getConfig :: String -> IO Config
getConfig configFile = do
  fileExists <- doesFileExist configFile
  if fileExists then do
    config <- readFile configFile
    return $ parseConfig config
  else
    return []

  -- when (not fileExists) $ do
  --   writeFile "config.txt" "" -- $ unlines $ map name sources

  -- 
  -- let sourceConfigs = map (\s -> (s, lookup (name s) (parseConfig config))) sources
  -- let source = case trace ("Midi sources: " ++ (unlines $ map show sourceConfigs)) sourceConfigs of
  --                (s, Just c):_ -> Just (s, c)
  --                _ -> Nothing
