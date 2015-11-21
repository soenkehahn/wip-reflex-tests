{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeOperators #-}

module Servant.Reflex.DomSpec where

import           Control.Concurrent
import           Control.Exception
import           Control.Monad.IO.Class
import           Data.Foldable
import           Data.Functor.Identity
import           Data.Maybe
import           GHCJS.DOM
import qualified Graphics.UI.Gtk as Gtk
import           Network.Wai
import           Reflex.Dom
import           Reflex.Spider.Internal (SpiderHostFrame)
import           Servant
import           System.Exit
import           Test.Hspec
import           Test.Hspec.Wai.Server

import           Servant.Reflex.Dom

type TestApi =
  "a" :> Get '[JSON] ([Int], String) :<|>
  "b" :> "b" :> Post '[JSON] String :<|>
  "c" :> ReqBody '[JSON] Bool :> Get '[JSON] Bool :<|>
  "d" :> Capture "d" String :> Get '[JSON] String :<|>
  "e" :> Capture "e" String :> ReqBody '[JSON] Int :> Get '[JSON] [String] :<|>
  "f" :> QueryParam "f" String :> Get '[JSON] String

testApi :: Proxy TestApi
testApi = Proxy

app :: Application
app request respond = do
  print request
  let respond' response = do
        print (responseHeaders response)
        print (responseStatus response)
        respond response
  serve testApi server request respond'

-- fixme: nat?
server :: Server TestApi
server =
  return ([42], "a") :<|>
  return "b" :<|>
  return :<|>
  return . reverse :<|>
  (\ s i -> return (replicate i s)) :<|>
  (\ s -> return (reverse $ fromMaybe "!!!gnorw" s))

spec :: Spec
spec = do
  describe "getFirstEvent" $ do
    it "works" $ do
      getFirstEvent (return . fmap (const 42)) `shouldReturn` 42

    it "works multiple times" $ do
      forM_ [1 .. 10] $ \ i -> do
        getFirstEvent (return . fmap (const i)) `shouldReturn` i

  describe "xhrRequest" $ do
    it "allows to query a server" $ withServer app $ \ port -> do
      let a :<|> _ = client testApi (BaseUrl Http "localhost" port)
      r <- getFirstEvent a
      r `shouldBe` Right ([42], "a")

    it "allows to send POST requests" $ withServer app $ \ port -> do
      let _ :<|> b :<|> _ = client testApi (BaseUrl Http "localhost" port)
      r <- getFirstEvent b
      r `shouldBe` Right "b"

    it "allows to send request bodies" $ withServer app $ \ port -> do
      let _ :<|> _ :<|> c :<|> _ = client testApi (BaseUrl Http "localhost" port)
      r <- getFirstEvent $ \ tick -> c (fmap (const True) tick)
      r `shouldBe` Right True

    it "allows captures" $ withServer app $ \ port -> do
      let _ :<|> _ :<|> _ :<|> d :<|> _ = client testApi (BaseUrl Http "localhost" port)
      r <- getFirstEvent $ \ tick -> d (fmap (const "foo") tick)
      r `shouldBe` Right "oof"

    it "allows multiple inputs" $ withServer app $ \ port -> do
      let _ :<|> _ :<|> _ :<|> _ :<|> e :<|> _ = client testApi (BaseUrl Http "localhost" port)
      r <- getFirstEvent $ \ tick -> e (fmap (const ("foo", 7)) tick)
      r `shouldBe` Right (replicate 7 "foo")

    it "allows QueryParams" $ withServer app $ \ port -> do
      let _ :<|> _ :<|> _ :<|> _ :<|> _ :<|> f = client testApi (BaseUrl Http "localhost" port)
      r <- getFirstEvent $ \ tick -> f (fmap (const "bar") tick)
      r `shouldBe` Right "rab"

-- fixme: around or before

    it "returns proper parse errors" $ do
      pending

getFirstEvent :: (MonadWidget t SpiderM, MonadIO (PushM t)) =>
  (Event t () -> SpiderM (Event t a)) -> IO a
getFirstEvent action = do
  mvar <- newEmptyMVar
  _ <- mainWidget $ do
    tick <- getPostBuild
    event <- action tick
    webView <- askWebView
    _ <- (\ inner -> foldDynM inner () event) $ \ a () -> liftIO $ do
      haltGui webView
      putMVar mvar a
    return ()
  readMVar mvar

haltGui :: WebView -> IO ()
haltGui wv = Gtk.postGUIAsync $ do
    w <- Gtk.widgetGetToplevel wv
    Gtk.widgetDestroy w

ensureOnce :: Spec -> Spec
ensureOnce spec = do
  mvar <- runIO $ newMVar True
  around (ensure mvar) spec
  where
    ensure mvar spec = modifyMVar_ mvar $ \ isFirst -> do
      if isFirst
        then do
          spec ()
        else do
          throwIO $ ErrorCall "only run one at a time"
      return False
