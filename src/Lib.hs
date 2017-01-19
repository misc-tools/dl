{-# LANGUAGE OverloadedStrings #-}

module Lib
    ( someFunc
    ) where

import Options.Applicative
import Network.HTTP
import Text.HTML.TagSoup
import Data.Maybe
import System.FilePath
import Network.HTTP.Conduit
import qualified Data.ByteString as B
import qualified Data.ByteString.Lazy as L
import Control.Applicative
import Control.Concurrent.Async

data Option = Option
  { url :: String
  , filetype :: String
  , explore :: Bool}

type URL = String
type Link = String
type FileType = String 

sample :: Parser Option
sample = Option
     <$> strOption
         ( long "url"
        <> short 'u'
        <> metavar "URL"
        <> value sampleUrl
        <> help "URL from which we want to download" )
     <*> strOption
         ( long "filetype"
        <> short 'f'
        <> value "pdf"
        <> metavar "FILETYPE"
        <> help "filetype to download (default: PDF)" )
     <*> switch
         ( long "explore"
        <> short 'e'
        <> help "Whether to explore the URL and report the resources available" )

process :: Option -> IO ()
process (Option u f False) = downloadResources u f 
process (Option u f True) = getStatistics u

sampleUrl :: URL
sampleUrl = "http://stanford.edu/~pyzhang/publication.html"

-- | download resources of a certain type from the website 
downloadResources :: URL -> FileType -> IO ()
downloadResources url ft = do
  putStrLn $ "Start download from " ++ url
  putStrLn $ "File type to download: " ++ ft 
  allLinks <- getResources url 
  let linksToDownload = map (normalizeLink (takeDirectory url)) $ filter f allLinks
  putStrLn "Links to download: "
  mapM_ putStrLn linksToDownload
  putStrLn "Begin to download..."
  xs <- foldr conc (return []) (map downloadResource linksToDownload)
  print (map B.length xs)
  where
    f link = (takeExtension link) == ("." ++ ft)
    conc ioa ioas = do
      (a,as) <- concurrently ioa ioas
      return (a:as)
-- | download a single resource 
downloadResource :: URL -> IO B.ByteString
downloadResource url = do
  res <- L.toStrict <$> simpleHttp url
  putStrLn $ "Finished: " ++ url
  return res

-- | get all resources from the website
getResources :: URL  -> IO [Link]
getResources url= do
  src <- getResponseBody =<< simpleHTTP (getRequest url)
  let tags = parseTags src
  let a_tags = getATags tags
  let links = map (fromJust) $ filter isJust $ map extractLink a_tags
  return links 
  
-- | count how many files of each type from a website 
getStatistics :: URL -> IO ()
getStatistics url = do
  links <- getResources url
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

-- | normalize a link, if it's relative (start with "./") then replace it with its absolute link.
normalizeLink :: URL -> URL -> URL
normalizeLink baseUrl url = if head url == '.'
  then baseUrl ++ tail url 
  else url
  

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
         
