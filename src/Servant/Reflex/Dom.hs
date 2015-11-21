{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE IncoherentInstances #-}
{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE UndecidableInstances #-}

module Servant.Reflex.Dom where

import           Control.Arrow
import           Data.Aeson
import           Data.Aeson.Parser
import           Data.Aeson.Types
import           Data.Attoparsec.ByteString
import           Data.ByteString (ByteString)
import           Data.List
import qualified Data.Map as Map
import           Data.String.Conversions
import           GHC.TypeLits
import           Reflex.Dom hiding (Value)
import           Reflex.Dom.Xhr
import           Reflex.Spider.Internal (SpiderHostFrame)
import           Servant
import           Servant.API

-- fixme: use the one from servant-client?
data BaseUrl
  = BaseUrl Scheme String Int

data Scheme
  = Http
  | Https

renderBaseUrl :: BaseUrl -> String
renderBaseUrl (BaseUrl scheme host port) =
  schemeS ++ "://" ++ host ++ ":" ++ show port
  where
    schemeS = case scheme of
      Http -> "http"
      Https -> "https"

client :: HasClients api => Proxy api -> BaseUrl -> Clients api
client = ex

type SpiderM = Widget Spider (Gui Spider (WithWebView SpiderHost) SpiderHostFrame)

type family Clients a :: * where
  Clients (a :<|> b) = Clients a :<|> Clients b
  Clients a = Event Spider (Input a) -> SpiderM (Event Spider (Either String (Output a)))

class HasClients api where
  ex :: Proxy api -> BaseUrl -> Clients api

instance (HasClients a, HasClients b) => HasClients (a :<|> b) where
  ex proxy base = ex aProxy base :<|> ex bProxy base
    where
      aProxy = Proxy :: Proxy a
      bProxy = Proxy :: Proxy b

instance {-# OVERLAPPING #-}
  (HasClient a,
   Clients a ~ (Event Spider (Input a) -> SpiderM (Event Spider (Either String (Output a))))) =>
  HasClients a where

  ex proxy base inputEvent = run proxy (fmap (initialRequest, ) inputEvent)
    where
      initialRequest :: Request
      initialRequest = Request
        (xhrRequest "" (renderBaseUrl base) def) []

class HasClient api where
  type Input api :: *
  type Output api :: *
  run :: Proxy api -> MkClient (Input api) (Output api)

data Request
  = Request {
    xhr :: XhrRequest,
    queryParams :: [(String, Maybe String)]
  }

modXhr :: (XhrRequest -> XhrRequest) -> Request -> Request
modXhr f (Request xhr queryParams) = Request (f xhr) queryParams

modXhrConfig :: (XhrRequestConfig -> XhrRequestConfig) -> XhrRequest -> XhrRequest
modXhrConfig f xhr = xhr {
  _xhrRequest_config = f (_xhrRequest_config xhr)
}

toXhr :: Request -> XhrRequest
toXhr (Request xhr queryParams) = xhr {
  _xhrRequest_url = _xhrRequest_url xhr ++ toQueryParamString queryParams
}
  where
    toQueryParamString :: [(String, Maybe String)] -> String
    toQueryParamString [] = ""
    toQueryParamString ps = "?" ++ intercalate "&" (map format ps)
    format (key, Nothing) = key
    format (key, Just value) = key ++ "=" ++ value

type MkClient input output =
  Event Spider (Request, input) -> SpiderM (Event Spider (Either String output))

instance (KnownSymbol path, HasClient subApi) =>
  HasClient ((path :: Symbol) :> subApi) where

  type Input (path :> subApi) = Input subApi
  type Output (path :> subApi) = Output subApi
  run Proxy event = run (Proxy :: Proxy subApi) (fmap (first addPath) event)
    where
      path = symbolVal (Proxy :: Proxy path)
      addPath = modXhr $ \ request -> request {
        _xhrRequest_url = _xhrRequest_url request ++ "/" ++ path
      }

instance (HasClient subApi, Input subApi ~ input, SplitInput body input, ToJSON body) =>
  HasClient (ReqBody '[JSON] (body :: *) :> subApi) where

  type Input (ReqBody '[JSON] body :> subApi) =
    AddInput body (Input subApi)
  type Output (ReqBody '[JSON] body :> subApi) =
    Output subApi
  run :: Proxy (ReqBody '[JSON] body :> subApi)
    -> MkClient (AddInput body (Input subApi)) (Output subApi)
  run Proxy inputEvent = run subProxy (popInput setBody inputEvent)
    where
      subProxy = Proxy :: Proxy subApi
      setBody :: body -> Request -> Request
      setBody body =
        let bytes = cs $ encode body
            setSendData = modXhrConfig $ \ config -> config {
              _xhrRequestConfig_sendData =
                Just bytes
              }
            setContentLength = setHeader "Content-Length" (show (length (bytes :: String)))
 {-           setResponseType = modXhrConfig $ \ config -> config {
              _xhrRequestConfig_responseType = Just XMLHttpRequestResponseTypeJson
            } -}
        in modXhr (setContentLength . setSendData)

instance (ToText capture, HasClient subApi, SplitInput capture (Input subApi)) =>
  HasClient (Capture path (capture :: *) :> subApi) where

  type Input (Capture path capture :> subApi) =
    AddInput capture (Input subApi)
  type Output (Capture path capture :> subApi) =
    Output subApi
  run :: Proxy (Capture path capture :> subApi)
    -> MkClient (AddInput capture (Input subApi)) (Output subApi)
  run Proxy inputEvent = run subProxy (popInput addCapture inputEvent)
    where
      subProxy = Proxy :: Proxy subApi
      addCapture :: ToText capture => capture -> Request -> Request
      addCapture capture = modXhr $ \ request -> request {
        _xhrRequest_url = _xhrRequest_url request ++ "/" ++ cs (toText capture)
      }

instance (HasClient subApi, SplitInput param (Input subApi), KnownSymbol name, ToText param) =>
  HasClient (QueryParam name (param :: *) :> subApi) where

  type Input (QueryParam name param :> subApi) =
    AddInput param (Input subApi)
  type Output (QueryParam name param :> subApi) =
    Output subApi
  run Proxy inputEvent = run subProxy (popInput addQueryParam inputEvent)
    where
      subProxy = Proxy :: Proxy subApi
      name = symbolVal (Proxy :: Proxy name)
      addQueryParam :: param -> Request -> Request
      addQueryParam param (Request xhr queryParams) =
        Request xhr (queryParams ++ [(name, Just $ cs $ toText param)])

popInput :: (SplitInput added inner, Functor f) => (added -> request -> request)
  -> f (request, AddInput added inner) -> f (request, inner)
popInput modRequest = fmap $ \ (request, outer) ->
  let (added, inner) = splitInput outer
  in (modRequest added request, inner)

instance FromJSON a => HasClient (Get '[JSON] a) where
  type Input (Get '[JSON] a) = ()
  type Output (Get '[JSON] a) = a
  run Proxy = verb "GET"

instance FromJSON a => HasClient (Post '[JSON] a) where
  type Input (Post '[JSON] a) = ()
  type Output (Post '[JSON] a) = a
  run Proxy = verb "POST"

verb :: forall api a . FromJSON a => String -> Event Spider (Request, ())
  -> SpiderM (Event Spider (Either String a))
verb method inputEvent = do
  let requestEvent = fmap transformRequest inputEvent
  responses <- performRequestAsync (traceEvent "req" requestEvent)
  return $ fmap parse responses
  where
    transformRequest :: (Request, ()) -> XhrRequest
    transformRequest =
      fst >>>
      toXhr >>>
      setMethod >>>
      setContentType

    setMethod r = r { _xhrRequest_method = method }

    setContentType = setHeader "Accept" "application/json"

    parse :: XhrResponse -> Either String a
    parse response = do
     case _xhrResponse_body response of
      Nothing -> Left "no response body found"
      Just body -> case decodeLenient $ cs body of
        Left err -> Left (err ++ ": " ++ cs body)
        Right a -> Right a

setHeader key value = modXhrConfig $ \ r ->
    r {
      _xhrRequestConfig_headers =
        Map.insert key value (_xhrRequestConfig_headers r)
    }

-- | Like 'Data.Aeson.decode' but allows all JSON values instead of just
-- objects and arrays.
decodeLenient :: FromJSON a => ByteString -> Either String a
decodeLenient input = do
  v :: Value <- parseOnly (Data.Aeson.Parser.value <* endOfInput) (cs input)
  parseEither parseJSON v

-- * AddInput

type family AddInput input inner where
  AddInput input () = input
  AddInput input (a, b) = (input, a, b)
  AddInput input (a, b, c) = (input, a, b, c)
  AddInput input (a, b, c, d) = (input, a, b, c, d)
  AddInput input (a, b, c, d, e) = (input, a, b, c, d, e)
  AddInput input (a, b, c, d, e, f) = (input, a, b, c, d, e, f)
  AddInput input other = (input, other)

class SplitInput input inner where
  splitInput :: AddInput input inner -> (input, inner)

instance SplitInput a () where
  splitInput a = (a, ())

instance (AddInput input other ~ (input, other)) => SplitInput input other where
  splitInput (input, other) = (input, other)
