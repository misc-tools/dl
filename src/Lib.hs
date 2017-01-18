module Lib
    ( someFunc
    ) where

import Options.Applicative
import Network.HTTP
import Text.HTML.TagSoup
import Data.Maybe
import System.FilePath

data Option = Option
  { url :: String
  , explore :: Bool }

type URL = String
type Link = String

sample :: Parser Option
sample = Option
     <$> strOption
         ( long "url"
        <> short 'u'
        <> metavar "URL"
        <> help "URL from which we want to download" )
     <*> switch
         ( long "explore"
        <> short 'e'
        <> help "Whether to explore the URL and report the resources available" )

process :: Option -> IO ()
process (Option u False) = putStrLn $ "So, you want to download from " ++ u
process (Option u True) = getStatistics u

sampleUrl :: URL
sampleUrl = "http://stanford.edu/~pyzhang/publication.html"

-- | count how many files of each type from a website 
getStatistics :: URL -> IO ()
getStatistics url = do
  src <- getResponseBody =<< simpleHTTP (getRequest url)
  let tags = parseTags src
  let a_tags = getATags tags
  let links = map (fromJust) $ filter isJust $ map extractLink a_tags
  let extensions = filter (\e -> ((length e) < 7) && ((length e) > 0)) $ map takeExtension links 
  putStrLn $ "Getting statistics from " ++ url ++ "..."
  putStrLn $ "Display all links..."
  mapM_ putStrLn links
  mapM_ putStrLn extensions

-- | get all tags <a ...>
getATags :: [Tag String] -> [Tag String]
getATags tags = filter f tags
  where f (TagOpen "a" _) = True
        f _ = False 

-- | extract link from <a ...> tag -- heavy template matching
extractLink :: Tag String -> Maybe String
extractLink (TagOpen "a" listparams) = go listparams
  where go lps = case lps of
          [] -> Nothing
          p : ps -> case p of
            ("href", link) -> Just link
            _ -> go ps          
extractLink _ = Nothing

someFunc :: IO ()
someFunc = execParser opts >>= process
  where
    opts = info (helper <*> sample)
      ( fullDesc
     <> progDesc "Download resources from a website"
     <> header "dl - Download resources from a website" )
         
