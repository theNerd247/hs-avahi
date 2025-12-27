{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

module Network.Avahi.Announce where

import DBus.Client
import DBus.Internal.Types
import Data.Char
import Data.Int
import Data.Maybe
import Data.Word
import Network.Avahi.Common

-- | Announce network service
announce
  :: Service
  -- ^ Service to announce
  -> IO ()
announce (Service {..}) = do
  client <- connectSystem
  [newGroup] <- call' client "/" serverInterface "EntryGroupNew" []
  let path = fromJust $ fromVariant newGroup
  let text' = [map (fromIntegral . ord) serviceText] :: [[Word8]]
  call'
    client
    path
    entryGroupInterface
    "AddService"
    [ toVariant (-1 :: Int32) -- IF_UNSPEC
    , proto2variant serviceProtocol
    , flags_empty
    , toVariant serviceName
    , toVariant serviceType
    , toVariant serviceDomain
    , toVariant serviceHost
    , toVariant servicePort
    , toVariant text'
    ]
  call' client path entryGroupInterface "Commit" []
  return ()
