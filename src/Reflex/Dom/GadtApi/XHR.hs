{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuantifiedConstraints #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
module Reflex.Dom.GadtApi.XHR where

import Control.Concurrent (forkIO, newEmptyMVar, putMVar, takeMVar)
import Control.Monad.IO.Class (MonadIO, liftIO)
import Data.Aeson
import qualified Data.ByteString.Lazy as LBS
import Data.Constraint.Extras (Has, has)
import Data.Functor (void)
import Data.Text (Text)
import qualified Data.Text.Encoding as T
import Language.Javascript.JSaddle (MonadJSM)
import Language.Javascript.JSaddle.Monad (runJSM, askJSM)
import Reflex.Dom.Core

type ApiEndpoint = Text

-- | Takes the output of a 'RequesterT' widget and issues that
-- output as API requests. The result of this function can be
-- fed back into the requester as responses. For example:
--
-- @
-- rec (appResult, requests) <- runRequesterT myApplication responses
--     responses <- performXhrRequests myApiEndpoint requests
-- @
--
performXhrRequests
  :: forall t m e api.
     ( FromJSON e
     , Has FromJSON api
     , forall a. ToJSON (api a)
     , Prerender t m
     , Applicative m
     )
  => ApiEndpoint
  -> (Text -> e)
  -> Event t (RequesterData api)
  -> m (Event t (RequesterData (Either e)))
performXhrRequests apiUrl req = fmap switchPromptlyDyn $ prerender (pure never) $ do
  performEventAsync $ ffor req $ \r yield -> do
    ctx <- askJSM
    void $ liftIO $ forkIO $ flip runJSM ctx $
      liftIO . yield =<< apiRequestXhr apiUrl errFn r

-- | Encodes an API request as JSON and issues an 'XhrRequest',
-- and attempts to decode the response.
apiRequestXhr
  :: forall e, api m.
     ( MonadIO m
     , MonadJSM m
     , Has FromJSON e
     , Has FromJSON api
     , forall a. ToJSON (api a)
     )
  => ApiEndpoint
  -> (Text -> e)
  -> RequesterData api
  -> m (RequesterData (Either e))
apiRequestXhr apiUrl errFn = traverseRequesterData $ \x ->
  has @FromJSON @api x $ mkRequest x
  where
    mkRequest
      :: FromJSON b
      => api b
      -> m (Either e b)
    mkRequest req = do
      response <- liftIO newEmptyMVar
      _ <- newXMLHttpRequest (postJson apiUrl req) $
        liftIO . putMVar response
      xhrResp <- liftIO $ takeMVar response
      case decodeXhrResponse xhrResp of
        Nothing -> pure $ Left . errFn $
          "Response could not be decoded for request: " <>
            T.decodeUtf8 (LBS.toStrict $ encode req)
        Just r -> pure r
