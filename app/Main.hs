{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DeriveAnyClass #-}

import Web.Scotty
import Data.Aeson
import Data.Aeson.Lens
import Control.Lens
import Data.Text (Text)
import Control.Applicative
import Control.Monad
import GHC.Generics
import Network.Wai.Middleware.Static
import qualified Data.ByteString.Lazy as B

import Data.Monoid (mconcat)

data Post = Post { _id :: Text
                 , title :: Text
                 , url :: Text
                 , created :: Text
                 , subject :: Text
                 , summary :: Text
                 , content :: Text
                 , likes :: Int } deriving (Show, Generic)

instance FromJSON Post
instance ToJSON Post

artJson :: [Post]
artJson = [ Post { _id = "fhpzah1lCBUGt9dS4rhM", title = "Mlog - Meeting Action Tracker", url = "lorem-ipsum-dolor-sit-amet-1", created = "2017-03-28T12:42:24.915Z", subject = "JavaScript", summary = "Lorem ipsum dolor sit amet, consectetur adipiscing elit, sed do eiusmod tempor incididunt ut labore et dolore magna aliqua. Ut enim ad minim veniam, quis nostrud exercitation ullamco laboris nisi ut aliquip ex ea commodo consequat.", content = "Hello", likes = 1 }, Post { _id = "fhpzah1lCBUGt9dS4rhZ", title = "Mlog - Meeting Action Tracker", url = "lorem-ipsum-dolor-sit-amet-2", created = "2017-03-28T12:42:24.915Z", subject = "JavaScript", summary = "Lorem ipsum dolor sit amet, consectetur adipiscing elit, sed do eiusmod tempor incididunt ut labore et dolore magna aliqua. Ut enim ad minim veniam, quis nostrud exercitation ullamco laboris nisi ut aliquip ex ea commodo consequat.", content = "Hello", likes = 1 }, Post { _id = "fhpzah1lCBUGt9dS4rhh", title = "Another - Meeting Action Tracker", url = "lorem-ipsum-dolor-sit-amet", created = "2017-03-28T12:42:24.915Z", subject = "JavaScript", summary = "Lorem ipsum dolor sit amet, consectetur adipiscing elit, sed do eiusmod tempor incididunt ut labore et dolore magna aliqua. Ut enim ad minim veniam, quis nostrud exercitation ullamco laboris nisi ut aliquip ex ea commodo consequat.", content = "Hello", likes = 1 } ]

-- projJson :: [Post]
-- projJson = [ Post { _id = "fhpzah1lCBUGt9dS4rhM", title = "Woop - Meeting Action Tracker", url = "lorem-ipsum-dolor-sit-amet-2", created = "2017-03-28T12:42:24.915Z", subject = "JavaScript", summary = "Lorem ipsum dolor sit amet, consectetur adipiscing elit, sed do eiusmod tempor incididunt ut labore et dolore magna aliqua. Ut enim ad minim veniam, quis nostrud exercitation ullamco laboris nisi ut aliquip ex ea commodo consequat.", content = "Hello", likes = 1 }, Post { _id = "fhpzah1lCBUGt9dS4rhZ", title = "Mlog - Meeting Action Tracker", url = "lorem-ipsum-dolor-sit-amet-1", created = "2017-03-28T12:42:24.915Z", subject = "JavaScript", summary = "Lorem ipsum dolor sit amet, consectetur adipiscing elit, sed do eiusmod tempor incididunt ut labore et dolore magna aliqua. Ut enim ad minim veniam, quis nostrud exercitation ullamco laboris nisi ut aliquip ex ea commodo consequat.", content = "Hello", likes = 1 }, Post { _id = "fhpzah1lCBUGt9dS4rhh", title = "Another - Meeting Action Tracker", url = "lorem-ipsum-dolor-sit-amet", created = "2017-03-28T12:42:24.915Z", subject = "JavaScript", summary = "Lorem ipsum dolor sit amet, consectetur adipiscing elit, sed do eiusmod tempor incididunt ut labore et dolore magna aliqua. Ut enim ad minim veniam, quis nostrud exercitation ullamco laboris nisi ut aliquip ex ea commodo consequat.", content = "Hello", likes = 1 } ]

jsonFile :: FilePath
jsonFile = "projects.json"

getJSON :: IO B.ByteString
getJSON = B.readFile jsonFile

staticServe :: ScottyM ()
staticServe = do
  middleware $ staticPolicy (addBase "./dist")

matchesUrl :: Text -> Post -> Bool
matchesUrl fUrl post = url post == fUrl

-- getArticle url = do
--   let filter = "url" :: Text
--   let article = artJson ^? key filter == Just (String url)
--   Web.Scotty.json $ article

serve :: ScottyM ()
serve = do
  get "/api/getarticle" $ do
    fUrl <- param "url"
    Web.Scotty.json $ (filter (matchesUrl fUrl) artJson) !!0
  get "/api/getproject" $ do
    fUrl <- param "url"
    d <- (eitherDecode <$> getJSON) :: IO (Either String [Post])
    case d of
      Left err -> putStrLn err
      Right ps -> Web.Scotty.json $ (filter (matchesUrl fUrl) ps) !!0
    -- let projects = (decode projJson) :: [Post]
  -- get "/articles" $ do
  --   Web.Scotty.json $ artJson
  -- get "/projects" $ do
  --   Web.Scotty.json $ projJson
  get "/api/getarticles" $ do
    Web.Scotty.json $ artJson
  get "/api/getprojects" $ do
    Web.Scotty.json $ projJson
  get "" $ do
    file $ "./dist/index.html"

main = scotty 4000 (serve >> staticServe)