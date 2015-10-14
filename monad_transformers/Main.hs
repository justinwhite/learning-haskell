{-- 
    http://book.realworldhaskell.org/read/monad-transformers.html
    started 13 OCT at 2030
--}


-- monad combines the effect of carrying state with early exit
-- monad transformer (mt) modifies behavior of underlying monad

-- IO is always on bottom of monad stack

-- mt -> monad -> monad
-- stack mt on monad you get back a monad

-- ReaderT for read-only data
-- StateT for mutable global state
-- WriterT for logging events during execution


-- fmap elevates a pure function to the level of functors
-- liftM takes a pure function to the level of monads
-- lift raises a monadic action from one level beneath in the transformer stack to the current one

implicitGet :: App AppState
implicitGet = get

explicitGet :: App AppState
explicitGet = lift get

-- One case in which we must use lift is when we create a monad transformer stack in which instances of the same typeclass appear at multiple levels
