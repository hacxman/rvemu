module Rvemu where

import Control.Exception
import Control.Monad
import Control.Monad.IO.Class
import Control.Monad.State
-- import Control.Monad.Trans
-- import Control.Monad.Trans.State
import Data.Binary.Get
import Data.Bits
import Data.ByteString.Lazy as BL hiding (putStrLn)
import Data.Maybe
import Data.Word
import Foreign.Marshal.Alloc
import Foreign.Ptr
import System.Environment
import System.IO


getOpcode insn = insn .&. 63
getOp6'2 insn = insn `shiftR` 2 .&. 31
getRd insn = insn `shiftR` 7 .&. 31
getFunct3 insn = insn `shiftR` 14 .&. 7
getRs1 insn = insn `shiftR` 15 .&. 31
getRs2 insn = insn `shiftR` 20 .&. 31
getFunct7 insn = insn `shiftR` 25 .&. 127
getImm11'0 insn = insn `shiftR` 20 .&. 4095
getImm4'0 insn = insn `shiftR` 25 .&. 31
getImm11'5 insn = insn `shiftR` 25 .&. 127
getImm31'12 insn = insn `shiftR` 25 .&. 1048575

getRform insn = (getOpcode insn, getRd insn, getFunct3 insn, getRs1 insn, getRs2 insn, getFunct7 insn)
getIform insn = (getOpcode insn, getRd insn, getFunct3 insn, getRs1 insn, getImm11'0 insn)
getSform insn = (getOpcode insn, getImm4'0 insn, getFunct3 insn, getRs1 insn, getRs2 insn, getImm11'5 insn)
getUform insn = (getOpcode insn, getRd insn, getImm31'12 insn)

data VMst = EmuS
          { regs :: [Int]
          , ram :: ByteString
          , ramsize :: Int}
emptyVMst = EmuS {regs = [0 | _ <- [0..31]], ram = BL.empty, ramsize = 0}
type EmuM a = StateT VMst IO a

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
  put $ s { ram = BL.concat [ BL.take (fromIntegral pos) rm
                            , bs
                            , (BL.drop ((fromIntegral $ pos)+(BL.length bs)) rm) ] }

getReg :: Int -> EmuM Int
getReg x = do
  s <- get
  when (x > 31) $ fail "invalid register number"
  return $ (regs s) !! x

setReg :: Int -> Int -> EmuM ()
setReg x val = do
  s <- get
  let rgs = regs s
  when (x > 31) $ fail "invalid register number"
  put $ s { regs = Prelude.take (x) rgs ++ (val : Prelude.drop (x+1) rgs) }
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
  r <- getReg 10
  return $ r

setPC :: Int -> EmuM ()
setPC pc = do
  r <- setReg 10 pc
  return ()

advancePC :: EmuM ()
advancePC = do
  pc <- getPC
  rs <- ramSize
  setPC $ (pc + 4) `mod` rs
  return ()

getRamAtPC :: EmuM ByteString
getRamAtPC = do
  pc <- getPC
  s <- get
  return $ BL.drop (fromIntegral pc) $ ram s
  

rvemu = do
  args <- getArgs
  let filename = args !! 0
  withBinaryFile filename ReadMode $ \hnd -> do
    putStrLn $ "loadling file " ++ filename
    c <- BL.hGetContents hnd
    flip runStateT emptyVMst $  do

      -- BOOT SEQUENCE :D
      printRegs
      initRam $ 256 -- 1 MiB
      printRam
      pc <- getPC
      liftIO $ print pc
      loadBSToRam c 0
      printRam

      -- MAIN RUN
      forever $ do
        printRam
        pc <- getPC
        liftIO $ putStrLn $ "PC: " ++ (show pc)
        cc <- getRamAtPC
        insn <- return $ runGet getWord32le cc
        
        liftIO $ print $ getOp6'2 insn
        case getOp6'2 insn of
    --      0x18 -- beq, bne, blt, bge, bltu, bgeu,
    --      0x19 -- jalr
    --      0x1b -- jal
    --      0x0d -- lui
    --      0x05 -- auipc
          -- addi, slli, slti, sltiu, xori, srli, srai, ori, andi
          0x04 -> do 
            let (op, rd, fun3, rs1, imm11'0) = getIform insn
            liftIO $ print rd
            liftIO $ putStrLn $ "fun3:" ++ show fun3
            val <- getReg $ fromIntegral rd
            case fun3 of
              -- addi
              0 -> do
                liftIO $ print "ADDI"
                setReg (fromIntegral rd) (val + fromIntegral imm11'0)
                advancePC
                printRegs
                return ()

  --          1 -- slli
  --          2 -- slti
  --          3 -- sltiu
              -- xori
              4 -> do
                liftIO $ print "XORI"
                setReg (fromIntegral rd) (val `xor` fromIntegral imm11'0)
                advancePC
                printRegs
                return ()
  --          5 -- srli, srai
              -- ori
              6 -> do
                liftIO $ print "ORI"
                setReg (fromIntegral rd) (val .|. fromIntegral imm11'0)
                advancePC
                printRegs
                return ()
              -- andi
              7 -> do
                liftIO $ print "ANDI"
                setReg (fromIntegral rd) (val .&. fromIntegral imm11'0)
                advancePC
                printRegs
                return ()


  --       0x0c -- add, sub, sll, slt, sltu, xor, srl, sra, or, and
          0x0c -> do 
            let (op, rd, fun3, rs1, rs2, fun7) = getRform insn
            liftIO $ print rd
            liftIO $ putStrLn $ "fun3:" ++ show fun3
            in1 <- getReg $ fromIntegral rs1
            in2 <- getReg $ fromIntegral rs2
            case fun3 of
              -- add / sub
              0 -> do
                case fun7 of
                      0 -> do
                        liftIO $ print "ADD"
                        setReg (fromIntegral rd) (in1 + in2)
                      32 -> do
                        liftIO $ print "SUB"
                        setReg (fromIntegral rd) (in1 - in2)
                advancePC
                printRegs

                return ()
              -- xor
              4 -> do
                liftIO $ print "XOR"
                setReg (fromIntegral rd) (in1 `xor` in2)
                printRegs
                advancePC
                return ()


  --      0x00 -- lb, lh, lw, lbu, lhu
  --      0x08 -- sb, sh, sw
  --      0x03 -- fences

    return ()
    
