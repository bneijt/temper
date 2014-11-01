module Temper where

import qualified Data.ByteString as BS
import System.USB.IO (Size, writeControl, noTimeout, Recipient(ToInterface), RequestType(Class), readInterrupt, Status)
import System.USB.Enumeration (getDevices, Device)
import System.USB.Initialization (newCtx)
import System.USB.Descriptors (getDeviceDesc, DeviceDesc(deviceProductId, deviceVendorId), EndpointAddress(..), TransferDirection(..))
import System.USB.DeviceHandling (withDetachedKernelDriver, withDeviceHandle, DeviceHandle, InterfaceNumber)
import Data.Word (Word16, Word8)

import Control.Monad (filterM)
import Data.Vector (toList)


temperatureFrom :: BS.ByteString -> Float
temperatureFrom msg = (125.0 / 32000.0) * fromIntegral measurement
    where
        byte2Int = fromIntegral (BS.index msg 2) :: Integer
        byte3Int = fromIntegral (BS.index msg 3) :: Integer
        measurement = (256 :: Integer) * byte2Int + byte3Int

temperVendorId :: Word16
temperVendorId = 0x0c45
temperProductId :: Word16
temperProductId = 0x7401

interface0 :: InterfaceNumber
interface0 = 0x00
interface1 :: InterfaceNumber
interface1 = 0x01

temperatureCommand :: [Word8]
temperatureCommand = [ 0x01, 0x80, 0x33, 0x01, 0x00, 0x00, 0x00, 0x00 ]
ini1Command :: [Word8]
ini1Command = [ 0x01, 0x82, 0x77, 0x01, 0x00, 0x00, 0x00, 0x00 ]
ini2Command :: [Word8]
ini2Command = [ 0x01, 0x86, 0xff, 0x01, 0x00, 0x00, 0x00, 0x00 ]


isTemperDevice :: Device -> IO Bool
isTemperDevice device = do
    devDesc <- getDeviceDesc device
    return $ (deviceVendorId devDesc == temperVendorId) && (deviceProductId devDesc == temperProductId)


interruptRead :: DeviceHandle -> IO (BS.ByteString, Status)
interruptRead deviceHandle =
    readInterrupt deviceHandle EndpointAddress {endpointNumber = 0x82, transferDirection = In} 8 noTimeout

controlTransfer :: DeviceHandle -> [Word8] -> IO (Size, Status)
controlTransfer deviceHandle msg = writeControl deviceHandle Class ToInterface setConfigurationRequest 0x0200 0x01 (BS.pack msg) noTimeout

setConfigurationRequest :: Word8
setConfigurationRequest = 0x09


readTemperature :: DeviceHandle -> IO Float
readTemperature deviceHandle =
    --Release the kernel driver
    withDetachedKernelDriver deviceHandle 0 $
        withDetachedKernelDriver deviceHandle 1 $ do
            _ <- writeControl deviceHandle Class ToInterface setConfigurationRequest 0x0201 0x00 (BS.pack [0x01, 0x01]) noTimeout
            _ <- controlTransfer deviceHandle temperatureCommand
            _ <- interruptRead deviceHandle
            _ <- controlTransfer deviceHandle ini1Command
            _ <- interruptRead deviceHandle
            _ <- controlTransfer deviceHandle ini2Command
            _ <- interruptRead deviceHandle
            _ <- interruptRead deviceHandle
            _ <- controlTransfer deviceHandle temperatureCommand
            (msg, _) <- interruptRead deviceHandle
            return (temperatureFrom msg)

readTemperatureFromFirstAvailableDevice :: IO Float
readTemperatureFromFirstAvailableDevice = do
    usbContext <- newCtx
    devices <- getDevices usbContext
    temperDevices <- filterM isTemperDevice (toList devices)
    let firstDevice = head temperDevices
    withDeviceHandle firstDevice readTemperature


