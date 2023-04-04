# UnliftIO Support for `servant-server`

APIs written in any `MonadUnliftIO m` can be converted to `wai`
`Application`s without writing the natural transformation by
hand. These functions will return `m Application` which means that either:

1. You will have to serve from inside your `MonadUnliftIO m` monad, or
2. You will have to unwrap your application monad to `IO`, bind the
   `Application`, and then serve it in `IO`.

If you choose the second option, beware `ResourceT` --- trying to use
the returned `Application` outside of a containing `ResourceT` risks
attempting to interact with closed resources.
