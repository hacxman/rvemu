{-# language RankNTypes #-}
{-# language ScopedTypeVariables #-}
{-# language OverloadedStrings #-}
{-# language BlockArguments #-}
{-# language FlexibleContexts #-}
module Emu.Utils where

import Text.Printf

registerToHex :: Int -> String
registerToHex reg =
     (o reg 3)
  ++ (o reg 2)
  ++ (o reg 1)
  ++ (o reg 0)
  where
    o :: Int -> Int -> String
    o reg x = Prelude.take 2 $ Prelude.drop (x*2) $ (printf "%08x" reg)

    
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

