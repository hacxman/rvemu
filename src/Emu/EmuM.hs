module Emu.EmuM where

--import Control.Concurrent.STM.TBChan
--
--import Control.Monad
--import Control.Monad.IO.Class
--import Control.Monad.State
--
--import Data.Binary.Get
--import Data.Binary.Put
--import Data.Bits
--
--import Data.ByteString.Lazy as BL hiding (putStrLn, putStr)
--import qualified Data.ByteString.Lazy as BL
--import qualified Data.ByteString as BS
--import qualified Data.ByteString.Char8 as BS8
--
--import Data.Map.Strict
--import Data.Maybe
--import Data.Set
--import Data.Word
--import Numeric

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

type EmuM a = StateT VMst IO a

data VMst = EmuS
          { regs :: [Int]
          , pc :: Int
          , ram :: ByteString
          , ramsize :: Int
          , iomaps :: Map MemRegion RegionAccessFns
          , breakpoints :: Set Addr
          , replyFn :: (BS.ByteString -> IO ())
          }


instance Show VMst where
  show x = "Machine state: " ++ (show $ regs x)

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

-- newtype EmuT m a = EmuT {
--     runEmuT :: m (EmuM a)
--   }
-- 
-- instance MonadTrans EmuT where
--   lift m = EmuT $ liftM m

emptyVMst = EmuS { regs = [0 | _ <- [0..31]]
                 , ram = BL.empty
                 , ramsize = 0
                 , pc = 0
                 , iomaps = Data.Map.Strict.empty
                 , breakpoints = Data.Set.empty
                 , replyFn = \_ -> return ()
                 }

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
