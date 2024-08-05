{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}

-- |
--
-- Module      : UnliftIO.Servant.Server
-- Copyright   : (C) 2024 Bellroy Pty Ltd
-- License     : BSD-3-Clause
-- Maintainer  : Bellroy Tech Team <haskell@bellroy.com>
-- Stability   : experimental
--
-- The functions in this module make it easier to serve a Servant API,
-- when its API endpoints are implemented in a monad that has an
-- 'MonadUnliftIO' instance. Many monad transformers are morally just
-- 'ReaderT' or 'IdentityT' over some kind of base monad, and these
-- monads can be unlifted.
--
-- This isn't magic—you'll still have to unwrap the outer transformers
-- to get at the returned 'Application'—but for simpler application
-- monads it frees you from fiddling around with rank-2 functions just
-- to serve your API.
--
-- Example use:
--
-- @
-- import Network.Wai.Handler.Warp (runEnv)
--
-- -- Some kind of Servant API
-- type MyApi = ... :\<|\> ... :\<|\> ...
--
-- -- API implemented in terms of some application monad, which has a 'MonadUnliftIO' instance.
-- myApi :: ServerT MyApi SomeApplicationMonad
-- myApi = undefined -- details unimportant
--
-- main :: IO ()
-- main = runSomeApplicationMonad $ do
--   app <- 'serve' myApi
--   liftIO $ runEnv 3000 app
-- @
module UnliftIO.Servant.Server
  ( -- * Helpers for traditional-style APIs
    serve,
    serveExceptT,
    serveWithContext,
    serveExceptTWithContext,

    -- * Helpers for Generic/records-based APIs
    genericServe,
    genericServeExceptT,
    genericServeWithContext,
    genericServeExceptTWithContext,
  )
where

import Control.Monad ((>=>))
import Control.Monad.Except (ExceptT, runExceptT, throwError)
import Control.Monad.IO.Unlift (MonadUnliftIO (..), liftIO)
import Data.Proxy (Proxy)
import Servant.API.Generic (AsApi, GenericServant, ToServant, ToServantApi)
import Servant.Server
  ( Application,
    Context,
    HasServer,
    ServerContext,
    ServerT,
  )
import qualified Servant.Server as Servant
import Servant.Server.Generic (AsServerT)
import qualified Servant.Server.Generic as Servant

-- | Convert a Servant API into an 'Application', by unlifting the
-- monad in which it runs.
serve ::
  (MonadUnliftIO m, HasServer api '[]) =>
  Proxy api ->
  ServerT api m ->
  m Application
serve proxy = serveWithContext proxy Servant.EmptyContext

-- | Convert a Servant API which uses 'ExceptT' above an unliftable
-- monad, by converting its errors into Servant's
-- 'Servant.ServerError' and returning them to the API caller.
serveExceptT ::
  (MonadUnliftIO m, HasServer api '[]) =>
  Proxy api ->
  (e -> Servant.ServerError) ->
  ServerT api (ExceptT e m) ->
  m Application
serveExceptT proxy toServerError =
  serveExceptTWithContext proxy toServerError Servant.EmptyContext

-- | As 'serve', with an additional 'Context' parameter.
serveWithContext ::
  (HasServer api context, ServerContext context, MonadUnliftIO m) =>
  Proxy api ->
  Context context ->
  ServerT api m ->
  m Application
serveWithContext proxy context api =
  withRunInIO $ \runInIO ->
    pure $ Servant.serveWithContextT proxy context (liftIO . runInIO) api

-- | As 'serveExceptT', with an additional 'Context' parameter.
serveExceptTWithContext ::
  (HasServer api context, ServerContext context, MonadUnliftIO m) =>
  Proxy api ->
  (e -> Servant.ServerError) ->
  Context context ->
  ServerT api (ExceptT e m) ->
  m Application
serveExceptTWithContext proxy toServerError context api =
  withRunInIO $ \runInIO ->
    pure $
      Servant.serveWithContextT
        proxy
        context
        ( liftIO . runInIO . runExceptT
            >=> either (throwError . toServerError) pure
        )
        api

-- | As 'serve', but for Servant's generic records.
--
-- /See:/ "Servant.API.Generic"
genericServe ::
  ( GenericServant routes (AsServerT m),
    GenericServant routes AsApi,
    HasServer (ToServantApi routes) '[],
    ServerT (ToServantApi routes) m ~ ToServant routes (AsServerT m),
    MonadUnliftIO m
  ) =>
  routes (AsServerT m) ->
  m Application
genericServe routes = withRunInIO $ \runInIO ->
  pure $ Servant.genericServeT (liftIO . runInIO) routes

-- | As 'genericServe', but for when you have an 'ExceptT' above the
-- unliftable monad. As with 'serveExceptT', errors are returned to
-- the API caller.
genericServeExceptT ::
  ( GenericServant routes (AsServerT (ExceptT e m)),
    GenericServant routes AsApi,
    HasServer (ToServantApi routes) '[],
    ServerT (ToServantApi routes) (ExceptT e m)
      ~ ToServant routes (AsServerT (ExceptT e m)),
    MonadUnliftIO m
  ) =>
  (e -> Servant.ServerError) ->
  routes (AsServerT (ExceptT e m)) ->
  m Application
genericServeExceptT toServerError routes =
  genericServeExceptTWithContext toServerError routes Servant.EmptyContext

-- | As 'genericServe', but with an additional 'Context' parameter.
genericServeWithContext ::
  ( GenericServant routes (AsServerT m),
    GenericServant routes AsApi,
    HasServer (ToServantApi routes) context,
    ServerContext context,
    ServerT (ToServantApi routes) m ~ ToServant routes (AsServerT m),
    MonadUnliftIO m
  ) =>
  routes (AsServerT m) ->
  Context context ->
  m Application
genericServeWithContext routes context = withRunInIO $ \runInIO ->
  pure $ Servant.genericServeTWithContext (liftIO . runInIO) routes context

-- | As 'genericServeExceptT', but with an additional 'Context' parameter.
genericServeExceptTWithContext ::
  ( GenericServant routes (AsServerT (ExceptT e m)),
    GenericServant routes AsApi,
    HasServer (ToServantApi routes) context,
    ServerContext context,
    ServerT (ToServantApi routes) (ExceptT e m)
      ~ ToServant routes (AsServerT (ExceptT e m)),
    MonadUnliftIO m
  ) =>
  (e -> Servant.ServerError) ->
  routes (AsServerT (ExceptT e m)) ->
  Context context ->
  m Application
genericServeExceptTWithContext toServerError routes context =
  withRunInIO $ \runInIO ->
    pure $
      Servant.genericServeTWithContext
        ( liftIO . runInIO . runExceptT
            >=> either (throwError . toServerError) pure
        )
        routes
        context
