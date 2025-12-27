{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

module Network.Avahi.Browse
  ( browse
  , dispatch
  )
where

import Control.Concurrent
import Control.Monad
import DBus.Client as C
import DBus.Internal.Message
import DBus.Internal.Types
import Data.ByteString (ByteString)
import Data.Char
import Data.Int
import Data.Text (Text)
import Data.Text.Encoding (decodeUtf8)
import Data.Word
import Network.Avahi.Common

import qualified Data.Text as Text

listenAvahi :: Maybe BusName -> Maybe MemberName -> C.MatchRule
listenAvahi name member = matchAny {matchSender = name, matchMember = member}

-- | Browse for specified service
browse :: BrowseQuery -> IO ()
browse (BrowseQuery {..}) = do
  client <- connectSystem
  -- We have to set up callback for ItemNew signal before we actually create a browser.
  -- Otherwise, the signal can arrive sooner then we managed to set up a callback for it.
  -- See also https://github.com/cocagne/txdbus/issues/8, https://github.com/lathiat/avahi/issues/9
  addMatch client (listenAvahi Nothing Nothing) (handler client lookupCallback)
  [sb] <-
    call'
      client
      "/"
      serverInterface
      "ServiceBrowserNew"
      [ iface_unspec
      , proto2variant lookupProtocol
      , toVariant lookupServiceName
      , toVariant lookupDomain
      , flags_empty
      ]
  -- print sb
  addMatch client (listenAvahi (Just serviceResolver) (Just "Found")) (handler client lookupCallback)
  return ()

-- | Dispatch signal and call corresponding function.
dispatch :: [(String, Signal -> IO b)] -> Signal -> IO ()
dispatch pairs signal = do
  let signame = signalMember signal
  -- putStrLn $ "signame: " ++ show signame ++ ", sender: " ++ show (signalSender signal)
  let good = [callback | (name, callback) <- pairs, memberName_ name == signame]
  forM_ good $ \callback ->
    callback signal

handler :: Client -> (Service -> IO ()) -> Signal -> IO ()
handler client callback signal = do
  dispatch
    [ ("ItemNew", on_new_item client)
    , ("Found", on_service_found callback)
    , ("ItemRemove", on_remove_item client)
    ]
    signal

on_new_item :: Client -> Signal -> IO ()
on_new_item client signal = do
  let body = signalBody signal
      [iface, proto, name, stype, domain, flags] = body
  call'
    client
    "/"
    serverInterface
    "ServiceResolverNew"
    [ iface
    , proto
    , name
    , stype
    , domain
    , proto2variant PROTO_UNSPEC
    , flags_empty
    ]
  return ()

on_remove_item :: Client -> Signal -> IO ()
on_remove_item client signal = do
  let body = signalBody signal
      [iface, proto, name, stype, domain, flags] = body
  call'
    client
    "/"
    serviceBrowserInterface
    "Free"
    [ iface
    , proto
    , name
    , stype
    , domain
    , proto2variant PROTO_UNSPEC
    , flags_empty
    ]
  return ()

on_service_found :: (Service -> IO ()) -> Signal -> IO ()
on_service_found callback signal = do
  let body = signalBody signal
      [iface, proto, name, stype, domain, host, aproto, addr, port, text, flags] = body
      service =
        Service
          { serviceProtocol = variant2proto proto
          , serviceName = fromVariant_ "service name" name
          , serviceType = fromVariant_ "service type" stype
          , serviceDomain = fromVariant_ "domain" domain
          , serviceHost = fromVariant_ "service host" host
          , serviceAddress = fromVariant addr
          , servicePort = fromVariant_ "service port" port
          , serviceText = maybe "" toString (fromVariant text :: Maybe [ByteString])
          }
  callback service

toString :: [ByteString] -> String
toString = Text.unpack . Text.concat . fmap (decodeUtf8)
