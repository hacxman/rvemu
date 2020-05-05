module Emu.Devices where

import Control.Concurrent
import Control.Concurrent.STM
import Control.Concurrent.STM.TBChan
import Control.Monad
import Control.Monad.IO.Class
import Emu.EmuM
import System.IO


startSuperStupidTimer :: TBChan Int -> EmuM ()
startSuperStupidTimer irqchan = do
      liftIO $ forkIO $ forever $ do
        threadDelay $ 2*100000
        atomically $ writeTBChan irqchan 0
      return ()


