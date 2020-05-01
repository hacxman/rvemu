import System.Environment

import Rvemu

main :: IO ()
main = testAddi

testAddi :: IO ()
testAddi = do
  withArgs ["/home/hexo/src/rvemu/rvemu/test/add-addi.text"] rvemu
