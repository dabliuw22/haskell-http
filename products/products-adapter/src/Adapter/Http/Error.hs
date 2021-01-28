{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}

module Adapter.Http.Error
  ( error400,
    error404,
    error500,
  )
where

import Control.Monad.IO.Class (MonadIO, liftIO)
import Data.Aeson (ToJSON, encode)
import Data.ByteString.Lazy (ByteString)
import Data.Text (Text)
import Data.Time (ZonedTime, getZonedTime)
import GHC.Generics (Generic)
import Network.HTTP.Types (hContentType)
import Network.HTTP.Types.Header (Header)
import Servant
  ( Handler,
    ServerError (errBody, errHeaders),
    err400,
    err404,
    err500,
    throwError,
  )

data ErrorDto = ErrorDto
  { message :: Text,
    timestamp :: ZonedTime
  }
  deriving (Show, Generic)

instance ToJSON ErrorDto

class Json a where
  toJson :: a -> ByteString

instance Json ErrorDto where
  toJson dto = encode dto

error400 :: Text -> Handler a
error400 msg = do
  errorDto <- makeError msg
  throwError
    err400
      { errBody = toJson errorDto,
        errHeaders = headers
      }

error404 :: Text -> Handler a
error404 msg = do
  errorDto <- makeError msg
  throwError
    err404
      { errBody = toJson errorDto,
        errHeaders = headers
      }

error500 :: Text -> Handler a
error500 msg = do
  errorDto <- makeError msg
  throwError
    err500
      { errBody = toJson errorDto,
        errHeaders = headers
      }

makeError :: (MonadIO m) => Text -> m ErrorDto
makeError msg = do
  now <- liftIO getZonedTime
  return $ ErrorDto msg now

headers :: [Header]
headers = [(hContentType, "application/json;charset=utf-8")]
