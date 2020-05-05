{-# language RankNTypes #-}
{-# language ScopedTypeVariables #-}
{-# language OverloadedStrings #-}
{-# language BlockArguments #-}
{-# language FlexibleContexts #-}
module Emu.GDBStub where

import Control.Concurrent
import Control.Concurrent.STM
import Control.Concurrent.STM.TBChan
import Control.Exception
import Control.Monad
import Control.Monad.IO.Class
import Control.Monad.State
import Data.ByteString.Lazy as BL hiding (putStrLn, putStr)
import qualified Data.ByteString.Lazy as BL
import qualified Data.ByteString as BS
import qualified Data.ByteString.Char8 as BS8
import Data.Char
import Data.Maybe
import Data.Set
import Emu.EmuM
import Network.Simple.TCP
import Numeric
import System.IO
import Text.Printf
import Emu.Utils

data DbgCmd = DbgGetRegs (BS.ByteString -> IO ())
            | DbgGetReg Int (BS.ByteString -> IO ())
            | DbgReadMem Int Int (BS.ByteString -> IO ())
            | DbgAddBreakpoint Int (BS.ByteString -> IO ())
            | DbgDelBreakpoint Int (BS.ByteString -> IO ())
            | DbgWriteRegs
            | DbgStatus (BS.ByteString -> IO ())
            | DbgPause Bool (BS.ByteString -> IO ())
            | DbgReset

crc :: [Char] -> String
crc bs = '$' : bs ++ (printf "#%02x" (sum' :: Int))
  where sum' = (Prelude.foldl (+) 0 $ Prelude.map (Data.Char.ord) (bs)) `mod` 256


processDbg emuRunning irqchan dbgchan = do
  cmd <- liftIO $ atomically $ tryReadTBChan dbgchan
  case cmd of
    Nothing -> return ()
    Just DbgReset -> do
      setPC 0
      mapM_ (setReg 0) [0..31]
      return ()
    Just (DbgStatus sndfn) -> do
      notpaused <- liftIO $! atomically $ readTVar emuRunning
      liftIO $ print $ crc "1"
      when notpaused $ liftIO $ sndfn $ BS8.pack $ crc "1"
      when (not notpaused) $ liftIO $ sndfn $ BS8.pack $ crc ""
    Just (DbgGetRegs sndfn) -> do
      regs <- getRegs
      --let rgs = fmap (printf "%08x") regs
      let rgs = fmap registerToHex regs
      liftIO $ sndfn $ BS8.pack $ crc $ Prelude.concat rgs
      return ()
    Just (DbgGetReg num sndfn) -> do
      reg <- getReg num
--      let rgs = printf "%08x" reg
      liftIO $ sndfn $ BS8.pack $ crc $ registerToHex reg -- rgs
      return ()
    Just (DbgReadMem loc sz sndfn) -> do
      s <- ramSize
      if loc + sz < s then do
        b <- getRamAt loc
        let nums = [fromIntegral $ BL.head $ (BL.drop (fromIntegral cc) b) | cc <- [0..sz-1]] :: [Int]
            rgs = Prelude.concat $ Prelude.map (\n -> printf "%02x" n) nums
        liftIO $ sndfn $ BS8.pack $ crc $ rgs
      else
        liftIO $ sndfn $ BS8.pack $ crc $ "E00"
      return ()
    Just (DbgPause p sndfn) -> do
      liftIO $ atomically $ writeTVar emuRunning (not p)
      liftIO $ print "pauzaiaiaiaiaing"
      liftIO $ print (not p)
      liftIO $ sndfn $ BS8.pack $ crc $ "OK"
    Just (DbgAddBreakpoint addr sndfn) -> do
      s <- get
      let brs = breakpoints s
      put $ s { breakpoints = addr `Data.Set.insert` brs
              , replyFn = sndfn }
      liftIO $ sndfn $ BS8.pack $ crc $ "OK"
      return ()
    Just (DbgDelBreakpoint addr sndfn) -> do
      s <- get
      let brs = breakpoints s
      put $ s { breakpoints = addr `Data.Set.delete` brs }
      liftIO $ sndfn $ BS8.pack $ crc $ "OK"
      return ()

startGDBServer :: EmuM (TBChan DbgCmd)
startGDBServer = do
  liftIO $ do
    chan <- atomically $ newTBChan 1
    forkIO $ do
      colorize Green
      putStrLn "starting gdbserver thread"
      colorize ResetAll
      serve (Host "127.0.0.1") "4243" $ \(connectionSocket, remoteAddr) -> do
        putStrLn $ "TCP connection established from " ++ show remoteAddr
        forever $ do
          mbs <- recv connectionSocket 1024
          case mbs of
            Just bs' -> do
              let bs = BS.dropWhile (\x->x `BL.elem` "+-") bs'
              BS.putStr bs'
              case bs of
                bs | "$" `BS.isPrefixOf` bs -> do
                  putStrLn "got GDB packet"
                  debugFn bs chan connectionSocket
                "\3" -> do
                  send connectionSocket "+"
                  atomically $ writeTBChan chan (DbgPause True (send connectionSocket))
                  send connectionSocket $ BS8.pack $ crc $ "T05core:0;" --20:" ++ (registerToHex pc) ++ ";core:0;"
                _ -> do
                  colorize Red
                  BL.putStr $ "got NOT GDB packet: "
--                  BS.putStr bs
                  print (BS.unpack bs)
                  colorize ResetAll
            Nothing -> return ()
    return chan
  where
    debugFn s chan connectionSocket | "$g" `BS.isPrefixOf` s = do
      colorize Yellow
      putStrLn "got $g"
      colorize ResetAll
      send connectionSocket "+"
      atomically $ writeTBChan chan (DbgGetRegs (send connectionSocket))
    debugFn s chan connectionSocket | "$!" `BS.isPrefixOf` s = do
      send connectionSocket "+"
      send connectionSocket $ BS8.pack $ crc "OK"
    debugFn s chan connectionSocket | "$?" `BS.isPrefixOf` s = do
      send connectionSocket "+"
      send connectionSocket $ BS8.pack $ crc "T08thread:01;"
    debugFn s chan connectionSocket | "$R" `BS.isPrefixOf` s = do
      colorize Yellow
      putStrLn "got $R"
      colorize ResetAll
      atomically $ writeTBChan chan (DbgReset)
--    debugFn s chan connectionSocket | "$v" `BS.isPrefixOf` s = do
--      print "ERPLY"
--      send connectionSocket "+"
--      send connectionSocket $ BS8.pack $ crc ""
    debugFn s chan connectionSocket | "$m" `BS.isPrefixOf` s = do
      colorize Yellow
      putStrLn "got $m"
      colorize ResetAll
      let dropped = Prelude.drop 2 $ BS8.unpack s
          [(loc, rest')] = readHex dropped
          [(size, _)] = readHex $ Prelude.drop 1 rest'
      send connectionSocket "+"
      atomically $ writeTBChan chan (DbgReadMem loc size (send connectionSocket))

    debugFn s chan connectionSocket | "$p" `BS.isPrefixOf` s = do
      colorize Yellow
      putStrLn "got $p"
      colorize ResetAll
      let [(rn, rest')] = readHex $ BS8.unpack $ BS8.drop 2 s
      send connectionSocket "+"
      atomically $ writeTBChan chan (DbgGetReg rn (send connectionSocket))

    debugFn s chan connectionSocket | "$c" `BS.isPrefixOf` s = do
      colorize Yellow
      putStrLn "got $c"
      colorize ResetAll
      send connectionSocket "+"
      atomically $ writeTBChan chan (DbgPause False (send connectionSocket))

    debugFn s chan connectionSocket | "$C" `BS.isPrefixOf` s = do
      colorize Yellow
      putStrLn "got $C"
      colorize ResetAll
      send connectionSocket "+"
      atomically $ writeTBChan chan (DbgPause False (send connectionSocket))

    debugFn s chan connectionSocket | "$H" `BS.isPrefixOf` s = do
      let pckt = BS.drop 2 s
      send connectionSocket "+"
      send connectionSocket $ BS8.pack $ crc "OK"
      --debugFn (BS.concat ["$",pckt]) chan connectionSocket


    debugFn s chan connectionSocket | "$Z0," `BS.isPrefixOf` s = debugFn (BS.concat ["$Z1",(BS.drop 3 s)]) chan connectionSocket
    debugFn s chan connectionSocket | "$z0," `BS.isPrefixOf` s = debugFn (BS.concat ["$z1",(BS.drop 3 s)]) chan connectionSocket
    debugFn s chan connectionSocket | "$Z1," `BS.isPrefixOf` s = do
      colorize Yellow
      putStrLn "got $Z1"
      colorize ResetAll
      send connectionSocket "+"
      let dropped = Prelude.drop 4 $ BS8.unpack s
          [(addr, rest')] = readHex dropped
      atomically $ writeTBChan chan (DbgAddBreakpoint addr (send connectionSocket))

    debugFn s chan connectionSocket | "$z1," `BS.isPrefixOf` s = do
      colorize Yellow
      putStrLn "got $z1"
      colorize ResetAll
      send connectionSocket "+"
      let dropped = Prelude.drop 4 $ BS8.unpack s
          [(addr, rest')] = readHex dropped
      atomically $ writeTBChan chan (DbgDelBreakpoint addr (send connectionSocket))


    debugFn s chan connectionSocket | "$vCont?#" `BS.isPrefixOf` s = do
      send connectionSocket "+"
      send connectionSocket $ BS8.pack $ crc "vCont;cs"

    debugFn s chan connectionSocket | "$qfThreadInfo" `BS.isPrefixOf` s = do
      send connectionSocket "+"
      send connectionSocket $ BS8.pack $ crc "m01"

    debugFn s chan connectionSocket | "$qsThreadInfo" `BS.isPrefixOf` s = do
      send connectionSocket "+"
      send connectionSocket $ BS8.pack $ crc "l"

    debugFn s chan connectionSocket | "$qOffsets" `BS.isPrefixOf` s = do
      send connectionSocket "+"
      send connectionSocket $ BS8.pack $ crc "TextSeg=0"

    debugFn s chan connectionSocket | "$qAttached" `BS.isPrefixOf` s = do
      send connectionSocket "+"
      send connectionSocket $ BS8.pack $ crc "1"

    debugFn s chan connectionSocket | "$qSupported" `BS.isPrefixOf` s = do
      send connectionSocket "+"
      send connectionSocket $ BS8.pack $ crc "PacketSize=1000;qXfer:features:read+;hwbreak+"
      --send connectionSocket $ BS8.pack $ crc "PacketSize=1000;qXfer:features:read+;multiprocess+;hwbreak+"

    debugFn s chan connectionSocket | "$qTStatus#" `BS.isPrefixOf` s = do
      send connectionSocket "+"
      atomically $ writeTBChan chan (DbgStatus (send connectionSocket))

    debugFn s chan connectionSocket |"$qXfer:features:read:target.xml" `BS.isPrefixOf` s = do
      send connectionSocket "+"
      xml <- System.IO.readFile "target.xml"
      send connectionSocket $ BS8.pack $ crc "l<?xml version=\"1.0\"?><!DOCTYPE target SYSTEM \"gdb-target.dtd\"><target><xi:include href=\"riscv-32bit-cpu.xml\"/><xi:include href=\"riscv-32bit-fpu.xml\"/><xi:include href=\"riscv-32bit-csr.xml\"/></target>" -- #bb"
--      send connectionSocket $ BS8.pack $ crc $ (T.unpack $ T.strip $ T.pack xml)

    debugFn s chan connectionSocket |"$qXfer:features:read:riscv-32bit-cpu.xml" `BS.isPrefixOf` s = do
      send connectionSocket "+"
      xml <- System.IO.readFile "riscv-32bit-cpu.xml"
      send connectionSocket $ BS8.pack $ crc $ 'l' : xml

    debugFn s chan connectionSocket |"$qXfer:features:read:riscv-32bit-csr.xml" `BS.isPrefixOf` s = do
      send connectionSocket "+"
      xml <- System.IO.readFile "riscv-32bit-csr.xml"
      let dropped = Prelude.drop 41 $ BS8.unpack s
          [(first', rest')] = readHex dropped
          [(second', _)] = readHex $ Prelude.drop 1 rest'
      liftIO $ print dropped
      liftIO $ print first'
      liftIO $ print second'
      fsize <- getFileSize "riscv-32bit-csr.xml"
      let msgprefix = if ((fromMaybe 0 fsize) - (fromIntegral first')) < (fromIntegral second') then 'l'
                                                    else 'm'
      send connectionSocket $ BS8.pack $ crc $ msgprefix : (Prelude.take (second') (Prelude.drop first' xml))

    debugFn s chan connectionSocket |"$qXfer:features:read:riscv-32bit-fpu.xml" `BS.isPrefixOf` s = do
      send connectionSocket "+"
      xml <- System.IO.readFile "riscv-32bit-fpu.xml"
      send connectionSocket $ BS8.pack $ crc $ 'l' : xml

    debugFn s chan connectionSocket = do
                  colorize Red
                  BL.putStr $ "got unknown GDB packet: "
                  BS.putStrLn s
                  colorize ResetAll
                  send connectionSocket "+"
                  send connectionSocket $ BS8.pack $ crc $ ""


getFileSize :: FilePath -> IO (Maybe Integer)
getFileSize path = handle handler
                   $ bracket (openFile path ReadMode) (hClose) (\h -> do size <- hFileSize h
                                                                         return $ Just size)
  where
    handler :: SomeException -> IO (Maybe Integer)
    handler _ = return Nothing


