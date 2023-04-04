{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}

module UnliftIO.Servant.Server where

import Control.Monad ((>=>))
import Control.Monad.Except (ExceptT, runExceptT, throwError)
import Data.Proxy (Proxy)
import Servant.API.Generic (AsApi, GenericServant, ToServant, ToServantApi)
import Servant.Server
  ( Application,
    Context,
    HasServer,
    ServerContext,
    ServerError,
    ServerT,
  )
import qualified Servant.Server as Servant
import Servant.Server.Generic (AsServerT)
import qualified Servant.Server.Generic as Servant
import UnliftIO (MonadUnliftIO (..), liftIO)

serve ::
  (MonadUnliftIO m, HasServer api '[]) =>
  Proxy api ->
  ServerT api m ->
  m Application
serve proxy = serveWithContext proxy Servant.EmptyContext

serveExceptT ::
  (MonadUnliftIO m, HasServer api '[]) =>
  Proxy api ->
  (e -> Servant.ServerError) ->
  ServerT api (ExceptT e m) ->
  m Application
serveExceptT proxy toServerError =
  serveExceptTWithContext proxy toServerError Servant.EmptyContext

serveWithContext ::
  (HasServer api context, ServerContext context, MonadUnliftIO m) =>
  Proxy api ->
  Context context ->
  ServerT api m ->
  m Application
serveWithContext proxy context api =
  withRunInIO $ \runInIO ->
    pure $ Servant.serveWithContextT proxy context (liftIO . runInIO) api

serveExceptTWithContext ::
  (HasServer api context, ServerContext context, MonadUnliftIO m) =>
  Proxy api ->
  (e -> ServerError) ->
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

genericServeExceptT ::
  ( GenericServant routes (AsServerT (ExceptT e m)),
    GenericServant routes AsApi,
    HasServer (ToServantApi routes) '[],
    ServerT (ToServantApi routes) (ExceptT e m)
      ~ ToServant routes (AsServerT (ExceptT e m)),
    MonadUnliftIO m
  ) =>
  (e -> ServerError) ->
  routes (AsServerT (ExceptT e m)) ->
  m Application
genericServeExceptT toServerError routes =
  genericServeExceptTWithContext toServerError routes Servant.EmptyContext

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

genericServeExceptTWithContext ::
  ( GenericServant routes (AsServerT (ExceptT e m)),
    GenericServant routes AsApi,
    HasServer (ToServantApi routes) context,
    ServerContext context,
    ServerT (ToServantApi routes) (ExceptT e m)
      ~ ToServant routes (AsServerT (ExceptT e m)),
    MonadUnliftIO m
  ) =>
  (e -> ServerError) ->
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
