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

import Emu.EmuM
import Emu.Devices
import Emu.GDBStub
import Emu.Utils



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






