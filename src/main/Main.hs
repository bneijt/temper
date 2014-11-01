module Main where
import qualified Temper as T

import System.USB.Enumeration (getDevices, Device)
import System.USB.Initialization (newCtx)
import Control.Monad (filterM)
import Data.Vector (toList)
import System.USB.DeviceHandling (withDeviceHandle)

main:: IO()
main = do
    usbContext <- newCtx
    devices <- getDevices usbContext
    temperDevices <- filterM T.isTemperDevice (toList devices)
    print (head temperDevices)
    withDeviceHandle (head temperDevices) $ \device -> do
        temp <- T.readTemperature device
        print temp
