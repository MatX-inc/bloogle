{-# LANGUAGE RecordWildCards #-}

module General.Timing (Timing, withTiming, timed, timedOverwrite) where

import Control.Monad.Extra
import Control.Monad.IO.Class
import Data.IORef
import Data.List.Extra
import General.Util
import System.IO
import System.Time.Extra

-- | A mutable object to keep timing information
data Timing = Timing
  { -- | Get time since the initialization of this 'Timing'.
    timingOffset :: IO Seconds,
    -- | Record timings for writing to a file
    timingStore :: IORef [(String, Seconds)],
    -- | If you are below T you may overwrite N characters
    -- at the end of the current terminal output.
    -- Only used iff @timingTerminal == True@.
    timingOverwrite :: IORef (Maybe (Seconds, Int)),
    -- | whether is this a terminal
    timingTerminal :: Bool,
    -- | Whether to print progress messages and timings to stdout.
    -- Timings are always recorded in 'timingStore' regardless.
    timingVerbose :: Bool
  }

-- | Time an action, optionally printing timing information to the terminal
withTiming ::
  -- | Whether to print progress messages and timings to stdout
  Bool ->
  -- | A file to optionally write all timings to, after the action is finished
  Maybe FilePath ->
  -- | An action that can write timings into 'Timing'
  (Timing -> IO a) ->
  IO a
withTiming timingVerbose writeTimingsTo act = do
  timingOffset <- offsetTime
  timingStore <- newIORef []
  timingOverwrite <- newIORef Nothing
  timingTerminal <- hIsTerminalDevice stdout

  res <- act Timing {..}
  total <- timingOffset
  whenJust writeTimingsTo $ \file -> do
    xs <- readIORef timingStore
    -- Expecting unrecorded of ~2s
    -- Most of that comes from the pipeline - we get occasional 0.01 between items as one flushes
    -- Then at the end there is ~0.5 while the final item flushes
    xs' <- pure $ sortOn (negate . snd) $ ("Unrecorded", total - sum (map snd xs)) : xs
    writeFile file $ unlines $ prettyTable 2 "Secs" xs'
  when timingVerbose $ putStrLn $ "Took " ++ showDuration total
  pure res

-- skip it if have written out in the last 1s and takes < 0.1

-- | Time the given action, writing the message and duration to stdout
--   if the 'Timing' is verbose
timed :: (MonadIO m) => Timing -> String -> m a -> m a
timed = timedEx False

-- | Like 'timed', but overwriting a previous message if it was marked as
--   overwritable
timedOverwrite :: (MonadIO m) => Timing -> String -> m a -> m a
timedOverwrite = timedEx True

timedEx :: (MonadIO m) => Bool -> Timing -> String -> m a -> m a
timedEx overwrite Timing {..} msg act
  -- quiet: record the timing, but print nothing
  | not timingVerbose = do
      start <- liftIO timingOffset
      res <- act
      end <- liftIO timingOffset
      liftIO $ modifyIORef timingStore ((msg, end - start) :)
      pure res
  | otherwise = do
      start <- liftIO timingOffset
      liftIO $ whenJustM (readIORef timingOverwrite) $ \(t, n) ->
        if overwrite && start < t
          then
            putStr $ replicate n '\b' ++ replicate n ' ' ++ replicate n '\b'
          else
            putStrLn ""

      let out msg' = liftIO $ putStr msg' >> pure (length msg')
      undo1 <- out $ msg ++ "... "
      liftIO $ hFlush stdout

      res <- act
      end <- liftIO timingOffset
      let time = end - start
      liftIO $ modifyIORef timingStore ((msg, time) :)

      s <- maybe "" (\x -> " (" ++ x ++ ")") <$> liftIO getStatsPeakAllocBytes
      undo2 <- out $ showDuration time ++ s

      old <- liftIO $ readIORef timingOverwrite
      let next = maybe (start + 1.0) fst old
      liftIO $
        if timingTerminal && overwrite && end < next
          then
            writeIORef timingOverwrite $ Just (next, undo1 + undo2)
          else do
            writeIORef timingOverwrite Nothing
            putStrLn ""
      pure res
