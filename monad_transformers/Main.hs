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


