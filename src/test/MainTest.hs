module Main where

import Test.Framework.Runners.Console (defaultMain)
import Test.Framework.Providers.HUnit (testCase)
import Test.HUnit.Base (assertEqual)
import System.Posix.Process (getProcessID)
import Temper (temperatureFrom)
import qualified Data.ByteString as BS

main :: IO ()
main = defaultMain [
        testCase "temperatureFromShouldShiftBitsCorrectly0" temperatureFromShouldShiftBitsCorrectly,
        testCase "temperatureFromShouldShiftBitsCorrectly1" aValueOfOneShouldBeAlmostZeroCelcius,
        testCase "liveExampleTest" temperatureFromShouldWorkForRealExample
    ]

--------------------------------
-- Unit tests
--------------------------------

-- This test should simply continue as afterPid self should immediately return
temperatureFromShouldShiftBitsCorrectly = assertEqual "Zero is measured as zero" 0.0 (temperatureFrom (BS.pack [0x00, 0x00, 0x00, 0x00]))

aValueOfOneShouldBeAlmostZeroCelcius = assertEqual "One should work" 0.00390625 (temperatureFrom (BS.pack [0x00, 0x00, 0x00, 0x01]))

temperatureFromShouldWorkForRealExample = assertEqual "One should work" 23.0 (temperatureFrom (BS.pack [128, 2, 23, 0, 101, 114, 70, 49]))