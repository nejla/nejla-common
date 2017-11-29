{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module RestExample where

import           Control.Monad.Except
import           Control.Monad.Reader
import           Data.Text            (Text)
import           Network.Wai          (Application)
import           Rest
import           Rest.Api
import           Rest.Driver.Wai
import qualified Rest.Resource        as R

newtype BlogApi a = BlogApi { unBlogApi :: IO a }
  deriving ( Applicative
           , Functor
           , Monad
           , MonadIO
           )

runBlogApi :: BlogApi a -> IO a
runBlogApi = unBlogApi

-- | Defines the /user api end-point.
resource :: Resource BlogApi (ReaderT () BlogApi) () () Void
resource = mkResourceReader
  { R.name   = "user" -- Name of the HTTP path segment.
  , R.schema = withListing () $ named []
  , R.list = const list
  }

list :: ListHandler BlogApi
list = mkListing jsonO handler
  where
    handler :: Range -> ExceptT Reason_ BlogApi [Text]
    handler r =
      let usrs = ["user1", "user2"]
      in return .take (count r) . drop (offset r) $ usrs

api :: Application
api = apiToApplication runBlogApi $ Versioned [(mkVersion 1 0 0, Some1 user)]

user :: Router BlogApi BlogApi
user  =
  root -/ route resource
