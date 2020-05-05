{-# language RankNTypes #-}
{-# language ScopedTypeVariables #-}
{-# language OverloadedStrings #-}
{-# language BlockArguments #-}
{-# language FlexibleContexts #-}
module Rvemu where

import Control.Concurrent
import Control.Concurrent.STM
import Control.Concurrent.STM.TBChan
import Control.Concurrent.STM.TVar
import Control.Exception
import Control.Monad
import Control.Monad.IO.Class
import Control.Monad.State
-- import Control.Monad.Trans
-- import Control.Monad.Trans.State
import Data.Binary.Get
import Data.Binary.Put
import Data.Bits
import Data.ByteString.Lazy as BL hiding (putStrLn, putStr)
import qualified Data.ByteString.Lazy as BL
import qualified Data.ByteString as BS
import qualified Data.ByteString.Char8 as BS8
import Data.Char
import Data.Map.Strict
import Data.Maybe
import Data.Set
import qualified Data.Text as T
import Data.Word
import Foreign.Marshal.Alloc
import Foreign.Ptr
import Network.Simple.TCP
import Numeric
import System.Environment
import System.IO
import System.Mem
import Text.Printf

getOpcode insn = insn .&. 63
getOp6'2 insn = insn `shiftR` 2 .&. 31
getRd insn = insn `shiftR` 7 .&. 31
getFunct3 insn = insn `shiftR` 12 .&. 7
getRs1 insn = insn `shiftR` 15 .&. 31
getRs2 insn = insn `shiftR` 20 .&. 31
getFunct7 insn = insn `shiftR` 25 .&. 127
getImm11'0 insn = insn `shiftR` 20 .&. 4095
getImm4'0 insn = insn `shiftR` 4 .&. 31
getImm11'5 insn = insn `shiftR` 25 .&. 127
getImm31'12 insn = insn `shiftR` 12 .&. 1048575
getShamt insn = (getImm11'0 insn) .&. 63
getShamt31'26 insn = (getImm11'0 insn) `shiftR` 6

getJImm20 insn = insn `shiftR` 31
getJImm10'1 insn = insn `shiftR` 21 .&. 1023
getJImm11 insn = insn `shiftR` 20 .&. 1
getJImm19'12 insn = insn `shiftR` 12 .&. 255



getRform insn = (getOpcode insn, getRd insn, getFunct3 insn, getRs1 insn, getRs2 insn, getFunct7 insn)
getIform insn = (getOpcode insn, getRd insn, getFunct3 insn, getRs1 insn, getImm11'0 insn, getShamt insn, getShamt31'26 insn)
getSform insn = (getOpcode insn, getImm4'0 insn, getFunct3 insn, getRs1 insn, getRs2 insn, getImm11'5 insn)
getUform insn = (getOpcode insn, getRd insn, getImm31'12 insn)

              -- pos -> data
              -- write is of arbitrary size, we are an emulator :D
type WrAccessFn = Int -> ByteString -> IO ()
              -- read access is always 32 bit
type RdAccessFn = Int -> IO ByteString

data MemRegion = MemRegion
               { memstart :: Int
               , memsize :: Int } deriving (Eq, Show)

instance Ord MemRegion where
  compare a b = compare (memstart a) (memstart b)

data RegionAccessFns = RegionAccessFns
                     { wrfn :: WrAccessFn
                     , rdfn :: RdAccessFn }
instance Show RegionAccessFns where
  show x = "RegionAccessFns"

type Addr = Int

data VMst = EmuS
          { regs :: [Int]
          , pc :: Int
          , ram :: ByteString
          , ramsize :: Int
          , iomaps :: Map MemRegion RegionAccessFns
          , breakpoints :: Set Addr
          , replyFn :: (BS.ByteString -> IO ())
          }

emptyVMst = EmuS { regs = [0 | _ <- [0..31]]
                 , ram = BL.empty
                 , ramsize = 0
                 , pc = 0
                 , iomaps = Data.Map.Strict.empty
                 , breakpoints = Data.Set.empty
                 , replyFn = \_ -> return ()
                 }

type EmuM a = StateT VMst IO a

data DbgCmd = DbgGetRegs (BS.ByteString -> IO ())
            | DbgGetReg Int (BS.ByteString -> IO ())
            | DbgReadMem Int Int (BS.ByteString -> IO ())
            | DbgAddBreakpoint Int (BS.ByteString -> IO ())
            | DbgDelBreakpoint Int (BS.ByteString -> IO ())
            | DbgWriteRegs
            | DbgStatus (BS.ByteString -> IO ())
            | DbgPause Bool (BS.ByteString -> IO ())
            | DbgReset

instance Show VMst where
  show x = "Machine state: " ++ (show $ regs x)

-- newtype EmuT m a = EmuT {
--     runEmuT :: m (EmuM a)
--   }
-- 
-- instance MonadTrans EmuT where
--   lift m = EmuT $ liftM m

getRegs :: EmuM [Int]
getRegs = do
  s <- get
  return $ regs s

initRegs :: EmuM ()
initRegs = do
  s <- get
  put $ s { regs = [0 | _ <- [0..31]] }

initRam :: Int -> EmuM ()
initRam size = do
  s <- get
  put $ s { ram = BL.pack [0 | _ <- [1..size]]
          , ramsize = size }

loadBSToRam :: BL.ByteString -> Int -> EmuM ()
loadBSToRam bs pos = do
  s <- get
  let rm = ram s
      maps = iomaps s
--  liftIO $ print "loading BS to"
--  liftIO $ print pos
--  liftIO $ print bs
--  liftIO $ print maps

  case Data.Map.Strict.lookup (MemRegion pos 4 ) maps of
    Just (RegionAccessFns wr rd) -> liftIO $ wr pos bs
    Nothing -> put $ s { ram = BL.concat [ BL.take (fromIntegral pos) rm
                            , bs
                            , (BL.drop ((fromIntegral $ pos)+(BL.length bs)) rm) ] }

getReg :: Int -> EmuM Int
getReg x = do
  s <- get
  case x of
    _ | (x == 0x20) -> getPC
    _ | (x > 31) -> do
      liftIO $ print $ "invalid register number " ++ show x
      return 0
    _ | (x <= 31) -> return $ (regs s) !! x

setReg :: Int -> Int -> EmuM ()
setReg x val = do
  s <- get
  let rgs = regs s
  when (x > 31) $ fail "invalid register number"
  when (x > 0) $ put $ s { regs = Prelude.take (x) rgs ++ (val : Prelude.drop (x+1) rgs) }
  return ()

printRegs :: EmuM ()
printRegs = do
  r <- getRegs
  liftIO $ putStrLn $ "Machine registers: " ++ show r

printRam :: EmuM ()
printRam = do
  r <- get
  liftIO $ putStrLn $ "Machine RAM: " ++ (show $ ram r)

ramSize :: EmuM Int
ramSize = do
  s <- get
  return $ ramsize s


signExtend :: Word32 -> Int -> Int
signExtend word signbit = fromMaybe 0 $ toIntegralSized word

getPC :: EmuM Int
getPC = do
  s <- get
  return $ pc s

setPC :: Int -> EmuM ()
setPC pc = do
  s <- get
  put $ s {pc = pc}
  return ()

advancePC :: EmuM ()
advancePC = do
  pc <- getPC
  rs <- ramSize
--  liftIO $ System.IO.putStr $ "advancing PC from " ++ (showHex pc "")
  setPC $ (pc + 4) `mod` rs
  pc' <- getPC
--  liftIO $ System.IO.putStrLn $ " to " ++ (showHex pc' "")
  return ()

getRamAtPC :: EmuM ByteString
getRamAtPC = do
  pc <- getPC
  s <- get
  return $ BL.drop (fromIntegral pc) $ ram s
  
getRamAt :: Int -> EmuM ByteString
getRamAt a = do
  s <- get
  let addr = (fromIntegral a)
  return $ BL.drop addr $ ram s


---- read mem bracket
--readMemBracket :: forall a. Int -> (ByteString -> a) -> EmuM a
--readMemBracket pos getter = do
--  bs <- getRamAt pos
--  return $! runGet $ getter bs
--
---- write mem bracket
--writeMemBracket :: Int -> (a -> ByteString -> ByteString) -> EmuM a
--writeMemBracket pos getter = do
--  bs <- getRamAt pos
--  return $! runGet $ getter bs

registerDevice :: MemRegion -> RegionAccessFns -> EmuM ()
registerDevice mr mafs = do
  s <- get
  let maps = iomaps s
  let newmaps = alter (\_ -> Just mafs) mr maps
  put $ s { iomaps = newmaps }
  return ()

unregisterDevice :: MemRegion -> EmuM ()
unregisterDevice mr = do
  s <- get
  let maps = iomaps s
  let newmaps = alter (\_ -> Nothing ) mr maps
  put $ s { iomaps = newmaps }
  return ()

dummyIOWr pos b = return ()

printVerboseIOWr pos b = System.IO.putStr "write at " >> print pos >> print b >> return ()
printIOWr pos b = BL.putStr b >> return ()

dummyIORd pos = return $ BL.pack [0,0,0,0]

type IRQNum = Int

createIrqController :: EmuM (TBChan IRQNum)
createIrqController = do
  chan <- liftIO $ atomically $ newTBChan 1
  return chan

processIrqs :: TBChan IRQNum -> EmuM ()
processIrqs chan = do
  irq <- liftIO $ atomically $ tryReadTBChan chan
  case irq of
    Just irqnum -> do
      bs <- getRamAt $ (irqnum * 4) + (256*1024)
      let addr = fromIntegral $ runGet getWord32le bs
      pc <- getPC
      setReg 5 pc
      setPC addr

    Nothing -> return ()

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



processBreakPoints :: TVar Bool -> EmuM ()
processBreakPoints emuRunning = do
  s <- get
  let brs = breakpoints s
  pc <- getPC
  case pc `Data.Set.member` brs of
    True -> liftIO $ do
      print $ BS8.pack $ crc $ "T0520:" ++ (registerToHex pc) ++ ";core:0;"
      replyFn s $ BS8.pack $ crc $ "T0520:" ++ (registerToHex pc) ++ ";core:0;"
      atomically $ writeTVar emuRunning False
    False -> return ()

registerToHex :: Int -> String
registerToHex reg =
     (o reg 3)
  ++ (o reg 2)
  ++ (o reg 1)
  ++ (o reg 0)
  where
    o :: Int -> Int -> String
    o reg x = Prelude.take 2 $ Prelude.drop (x*2) $ (printf "%08x" reg)

cpuLoop :: TVar Bool -> TBChan Int -> TBChan DbgCmd -> EmuM ()
cpuLoop emuRunning irqchan dbgchan = do
    notpaused <- liftIO $! atomically $ readTVar emuRunning
    processDbg emuRunning irqchan dbgchan
    when notpaused $ do
--          r <- getRamAt 0
--          liftIO $ print $ BL.length r
        processIrqs irqchan
--        printRam
        processBreakPoints emuRunning
        pc <- getPC
        --liftIO $ putStrLn $ "PC: " ++ (show pc)
        cc <- getRamAtPC
        insn <- return $! runGet getWord32le cc
        
--        liftIO $ print $ getOp6'2 insn
        case getOp6'2 insn of
--        0x18 -- beq, bne, blt, bge, bltu, bgeu,
          0x18 -> do
            let (op, imm4, fun3, rs1, rs2, imm11) = getSform insn
                imm = fromIntegral $ imm4 .|. (imm11 `shiftL` 4)
                addr = fromIntegral $ pc + imm
            in1 <- getReg $ fromIntegral rs1
            in2 <- getReg $ fromIntegral rs2
            case fun3 of
              0 -> do -- beq
                when (in1 == in2) $ setPC addr
              1 -> do -- bne
                when (in1 /= in2) $ setPC addr
              4 -> do -- blt
                when (in1 < in2) $ setPC addr
              5 -> do -- bge
                when (in1 >= in2) $ setPC addr
              6 -> do -- bltu
                when (in1 < in2) $ setPC addr
              7 -> do -- bgeu
                when (in1 >= in2) $ setPC addr
          --printRegs
            return ()

--        0x19 -- jalr
          0x19 -> do
--            liftIO $ print "JALR"
            let (op, rd, fun3, rs1, imm11'0, shamt, upshamt) = getIform insn
            val <- getReg $ fromIntegral rs1
            setReg (fromIntegral rd) $ fromIntegral pc + 4
            setPC $ fromIntegral (val + (fromIntegral imm11'0)) .&. (-2)
          --printRegs
            return ()

--        0x1b -- jal
          0x1b -> do
--            liftIO $ print "JAL"
            let (op, rd, imm) = getUform insn
            let jimm20 = getImm31'12 insn
            setReg (fromIntegral rd) $ fromIntegral pc + 4
            --liftIO $ print jimm20
            let immm = getJImm10'1 insn `shiftL` 1
                     + getJImm11 insn `shiftL` 11
                     + getJImm19'12 insn `shiftL` 12

            --liftIO $ print $ getJImm20 insn
            --liftIO $ print imm
            let signedimm :: Int = case getJImm20 insn of
                    0 -> fromIntegral immm
                    1 -> (-1) * fromIntegral (1+complement immm .&. (1048575))
--            liftIO $ print signedimm
            setPC (pc + signedimm)
          --  printRegs
            return ()
--        0x0d -- lui
          0x0d -> do
            let (op, rd, imm) = getUform insn
--            liftIO $ print "LUI"
            setReg (fromIntegral rd) (fromIntegral $ imm `shiftL` 12)
            advancePC
          --printRegs
            return ()
--        0x05 -- auipc
          0x05 -> do
            let (op, rd, imm) = getUform insn
--            liftIO $ print "AUIPC"
            setReg (fromIntegral rd) (fromIntegral $ imm `shiftL` 12 + fromIntegral pc)
            advancePC
          --printRegs
            return ()
          -- addi, slli, slti, sltiu, xori, srli, srai, ori, andi
          0x04 -> do 
            let (op, rd, fun3, rs1, imm11'0, shamt, upshamt) = getIform insn
--            liftIO $ print rd
--            liftIO $ putStrLn $ "fun3:" ++ show fun3
            val <- getReg $ fromIntegral rs1
            let in1 = fromIntegral imm11'0
            case fun3 of
              -- addi
              0 -> do
--                liftIO $ print "ADDI"
                setReg (fromIntegral rd) (val + in1)
                advancePC
              --  printRegs
                return ()
              -- slli
              1 -> do
                liftIO $ print "SLLI"
                setReg (fromIntegral rd) $ val `shiftL` (fromIntegral shamt)
                advancePC
              --printRegs
                return ()
              -- sltu
              2 -> do
--                liftIO $ print "SLTI"
                when (val < in1) $ setReg (fromIntegral rd) $ val `shiftL` (fromIntegral imm11'0)
                when (val >= in1) $ setReg (fromIntegral rd) $ val `shiftL` (fromIntegral imm11'0)
                advancePC
              --printRegs
                return ()
              -- sltu
              3 -> do
--                liftIO $ print "SLTIU"
                when (val < in1) $ setReg (fromIntegral rd) $ val `shiftL` (fromIntegral imm11'0)
                when (val >= in1) $ setReg (fromIntegral rd) $ val `shiftL` (fromIntegral imm11'0)
                advancePC
              --printRegs
                return ()
              -- xori
              4 -> do
--                liftIO $ print "XORI"
                setReg (fromIntegral rd) (val `xor` fromIntegral imm11'0)
                advancePC
              --printRegs
                return ()
              -- srli, srai
              5 -> do
                case upshamt of
                  0 -> do
                    liftIO $ print "SRLI"
                    setReg (fromIntegral rd) $ val `shiftR` (fromIntegral shamt)
                  16 -> do
--                    liftIO $ print "SRAI"
                    setReg (fromIntegral rd) $ val `shiftR` (fromIntegral shamt)
                advancePC
              --printRegs
                return ()
              -- ori
              6 -> do
--                liftIO $ print "ORI"
                setReg (fromIntegral rd) (val .|. fromIntegral imm11'0)
                advancePC
              --printRegs
                return ()
              -- andi
              7 -> do
--                liftIO $ print "ANDI"
                setReg (fromIntegral rd) (val .&. fromIntegral imm11'0)
                advancePC
              --printRegs
                return ()
--        0x0c -- add, sub, sll, slt, sltu, xor, srl, sra, or, and
          0x0c -> do 
            let (op, rd, fun3, rs1, rs2, fun7) = getRform insn
--            liftIO $ print rd
--            liftIO $ putStrLn $ "fun3:" ++ show fun3
            in1 <- getReg $ fromIntegral rs1
            in2 <- getReg $ fromIntegral rs2
            case fun3 of
              -- add / sub
              0 -> do
                case fun7 of
                      0 -> do
--                        liftIO $ print "ADD"
                        setReg (fromIntegral rd) (in1 + in2)
                      32 -> do
--                        liftIO $ print "SUB"
                        setReg (fromIntegral rd) (in1 - in2)
                advancePC
              --printRegs

                return ()
              -- sll
              1 -> do
--                liftIO $ print "SLL"
                setReg (fromIntegral rd) $ in1 `shiftL` in2
                advancePC
              --printRegs
                return ()
              -- sltu
              2 -> do
--                liftIO $ print "SLT"
                when (in1 < in2) $ setReg (fromIntegral rd) 1
                when (in1 >= in2) $ setReg (fromIntegral rd) 0
                advancePC
              --printRegs
                return ()
              -- sltu
              3 -> do
--                liftIO $ print "SLTU"
                when (in1 < in2) $ setReg (fromIntegral rd) 1
                when (in1 >= in2) $ setReg (fromIntegral rd) 0
                advancePC
              --printRegs
                return ()
              -- xor
              4 -> do
--                liftIO $ print "XOR"
                setReg (fromIntegral rd) (in1 `xor` in2)
              --printRegs
                advancePC
                return ()
              -- srl / sra
              5 -> do
                case fun7 of
                      0 -> do
--                        liftIO $ print "SRL"
                        setReg (fromIntegral rd) (in1 `shiftR` in2)
                      32 -> do
--                        liftIO $ print "SRA"
                        setReg (fromIntegral rd) (in1 `shiftR` in2)
                advancePC
              --printRegs

                return ()
              -- or
              6 -> do
--                liftIO $ print "OR"
                setReg (fromIntegral rd) (in1 .|. in2)
              --printRegs
                advancePC
                return ()

              -- and
              7 -> do
--                liftIO $ print "AND"
                setReg (fromIntegral rd) (in1 .&. in2)
              --printRegs
                advancePC
                return ()

  --      0x00 -- lb, lh, lw, lbu, lhu
          0x00 -> do
            let (op, rd, fun3, rs1, imm11'0, shamt, upshamt) = getIform insn
            let in1 = fromIntegral imm11'0
--            liftIO $ print rd
--            liftIO $ putStrLn $ "fun3:" ++ show fun3
            val <- getReg $ fromIntegral rs1
            b <- getRamAt $ val + in1
            let loaded = case fun3 of
                           0 -> -- lb
                             fromIntegral $ runGet getWord8 b
                           1 -> do -- lh
                             fromIntegral $ runGet getWord16le b
                           2 -> do -- lw
                             runGet getWord32le b
                           4 -> do -- lbu
                             fromIntegral $ runGet getWord8 b
                           5 -> do -- lhu
                             fromIntegral $ runGet getWord16le b
            setReg (fromIntegral rd) $ fromIntegral loaded
            advancePC
          --printRegs
  --      0x08 -- sb, sh, sw
          0x08 -> do
            let (op, imm4, fun3, rs1, rs2, imm11) = getSform insn
--                imm = fromIntegral $ imm4 .|. (imm11 `shiftL` 5)
                imm = fromIntegral $ imm4 .|. (imm11 `shiftL` 4)
                addr = fromIntegral $ pc + imm
            valadd <- getReg $ fromIntegral rs1
            val <- getReg $ fromIntegral rs2
  --          let addr = fromIntegral $ imm11
            liftIO $ print addr
            liftIO $ print val
            liftIO $ print valadd
--            liftIO $ print "SB,SH,SW"
            case fun3 of
              0 -> do -- sb
                liftIO $ print "SB"
                let bs = runPut $ putWord8 $ fromIntegral val
                loadBSToRam bs addr
              1 -> do -- sh
                liftIO $ print "SH"
                let bs = runPut $ putWord16le $ fromIntegral val
                loadBSToRam bs addr
              2 -> do -- sw
                liftIO $ print "SW"
                let bs = runPut $ putWord32le $ fromIntegral val
                loadBSToRam bs addr
            advancePC
          --printRegs
          x -> liftIO $ do
            let err = "Unknown OPcode " ++ (showHex x " at addr ") ++ (showHex pc "")

            fail err

  --      0x03 -- fences

        return ()
    liftIO $ performGC
    cpuLoop emuRunning irqchan dbgchan





startSuperStupidTimer :: TBChan Int -> EmuM ()
startSuperStupidTimer irqchan = do
      liftIO $ forkIO $ forever $ do
        threadDelay $ 2*100000
        atomically $ writeTBChan irqchan 0
      return ()

rvemu = do
  args <- getArgs
  let ramsize = 256*1025*1024
  let filename = args !! 0
  withBinaryFile filename ReadMode $ \hnd -> do
    putStrLn $ "loadling file " ++ filename
    c <- BL.hGetContents hnd
    flip runStateT emptyVMst $  do

      -- BOOT SEQUENCE :D
    --printRegs
      liftIO $ colorize Green
      liftIO $ putStrLn $ "initializing RAM " ++ (show ramsize) ++ " (0x" ++ (showHex ramsize ") bytes")
      liftIO $ colorize ResetAll
      initRam $ 256*1024*1024 -- 1 MiB
      --printRam
      setPC 0x0 -- 4000000
      pc <- getPC
      liftIO $ colorize Green
      liftIO $ putStrLn $ "starting at address: 0x" ++ (showHex (fromIntegral pc) "")
      liftIO $ colorize ResetAll
      loadBSToRam c pc
      --printRam

      registerDevice (MemRegion {memstart = 1024, memsize = 4})
                     (RegionAccessFns { wrfn = printIOWr
                                      , rdfn = dummyIORd})

      irqchan <- createIrqController

--      startSuperStupidTimer irqchan

      emuRunning <- liftIO $ atomically $ newTVar False

      dbgchan <- startGDBServer

      liftIO $ colorize Green
      liftIO $ putStrLn "starting CPU loop"
      liftIO $ colorize ResetAll
      --liftIO $ threadDelay 2000000
      -- MAIN RUN
      cpuLoop emuRunning irqchan dbgchan

      return ()
    return ()
    
data Colors = Default
            | Red
            | Green
            | Yellow
            | ResetAll
            | Bold
            | ResetBold

colorize Default    = putStr "\x1B[39m"
colorize Red        = putStr "\x1B[31m"
colorize Green      = putStr "\x1B[32m"
colorize Yellow     = putStr "\x1B[33m"

colorize Bold       = putStr "\x1B[1m"
colorize ResetBold  = putStr "\x1B[21m"

colorize ResetAll   = putStr "\x1B[0m"

