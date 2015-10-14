{-# LANGUAGE TypeFamilies,
             GeneralizedNewtypeDeriving #-}

-- QUESTION
-- seems GeneralizednewtypeDeriving pragma is 'unsafe'
-- due to implementation assuming `newtype` implies 
-- not necessarily true equalities
-- i.e. `a -> b` and `b -> a` does not mean `forall f. f a -> f b`

import System.Directory
import System.FilePath
import Control.Monad.Reader
import Control.Monad.State
import Control.Monad.Writer

import CountEntries (listDirectory)

data AppConfig = AppConfig {
      cfgMaxDepth :: Int
    } deriving (Show)

data AppState = AppState {
      stDeepestReached :: Int
    } deriving (Show)

data AppLog = AppLog {
      file :: FilePath
      , depth :: Int
      } deriving (Show)

-- ReaderT -> StateT -> IO 

-- QUESTION
-- no need to include type param `a`.. 

--type App = ReaderT AppConfig (StateT AppState IO)
--type App = StateT AppState (ReaderT AppConfig IO)
type App = WriterT Log (StateT AppState (ReaderT AppConfig IO))
type Entry = (FilePath, Int)
type Log = [Entry]

runApp :: App a -> Int -> IO ((a, Log), AppState)
runApp k maxDepth =
  let config = AppConfig maxDepth
      state = AppState 0
      --in runStateT (runReaderT k config) state
      --in runReaderT (runStateT k state) config
  in runReaderT (runStateT (runWriterT k) state) config

constrainedCount :: Int -> FilePath -> App Log
constrainedCount curDepth path = do
  contents <- liftIO . listDirectory $ path
  cfg <- ask
  tell [(path, (length contents))]
  rest <- forM contents $ \name -> do
            let newPath = path </> name
            isDir <- liftIO $ doesDirectoryExist newPath
            if isDir && curDepth < cfgMaxDepth cfg
              then do
                let newDepth = curDepth + 1
                st <- get
                when (stDeepestReached st < newDepth) $
                  put st { stDeepestReached = newDepth }
                constrainedCount newDepth newPath
              else return []
  return $ (path, length contents) : concat rest

-- QUESTION 
-- need help seeing the big picture here:
-- Our use of monad transformers here is admittedly a little contrived. Because we're writing a single straightforward function, we're not really winning anything. What's useful about this approach, though, is that it scales to bigger programs. No comments
-- We can write most of an application's imperative-style code in a monad stack similar to our App monad. In a real program, we'd carry around more complex configuration data, but we'd still use ReaderT to keep it read-only and hidden except when needed. We'd have more mutable state to manage, but we'd still use StateT to encapsulate it

newtype MyApp a = MyA {
      --runA :: ReaderT AppConfig (StateT AppState IO) a
      --runA :: StateT AppState (ReaderT AppConfig IO) a
      runA :: WriterT Log (StateT AppState (ReaderT AppConfig IO)) a
    } deriving (Monad, MonadIO, MonadReader AppConfig,
                MonadState AppState)

runMyApp :: MyApp a -> Int -> IO ((a, Log), AppState)
runMyApp k maxDepth =
    let config = AppConfig maxDepth
        state = AppState 0
    --in runStateT (runReaderT (runA k) config) state
    --in runReaderT (runStateT (runA k) state) config
    in runReaderT (runStateT (runWriterT (runA k)) state) config

-- QUESTION
-- If we export the MyApp type constructor and the runMyApp execution function from a module, client code will not be able to tell that the internals of our monad is a stack of monad transformers

-- to run
-- runMyApp (MyA $ constrainedCount 0 "/home/jwhite/learning-haskell") 2
-- runApp (constrainedCount 0 "..") 1

-- QUESTION
-- does `tell` explicitly append lists?
