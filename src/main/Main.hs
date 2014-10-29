module Main where
import System.USB.IO (control, writeControl, noTimeout, Recipient(ToDevice, ToInterface), RequestType(Standard, Class), readInterrupt, Status)
import System.USB.Enumeration (getDevices, Device)
import System.USB.Initialization (newCtx)
import System.USB.Descriptors (getDeviceDesc, DeviceDesc(deviceProductId, deviceVendorId), EndpointAddress(..), TransferDirection(..))
import System.USB.DeviceHandling (setConfig, withDeviceHandle, withDetachedKernelDriver, DeviceHandle, resetDevice, detachKernelDriver, claimInterface, InterfaceNumber)
import Control.Monad (filterM)
import Data.Vector (toList)
import Data.Word (Word16, Word8)
import qualified Data.ByteString as BS
import Data.Binary (decode)
import Data.Int (Int16)
import Data.Char (ord)
import qualified Temper as T

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
    return $ ((deviceVendorId devDesc) == temperVendorId) && ((deviceProductId devDesc) == temperProductId)


interruptRead :: DeviceHandle -> IO (BS.ByteString, Status)
interruptRead deviceHandle = do
    readInterrupt deviceHandle (EndpointAddress {endpointNumber = 0x82, transferDirection = In}) 8 noTimeout

controlTransfer deviceHandle msg = writeControl deviceHandle Class ToInterface setConfigurationRequest 0x0200 0x01 (BS.pack msg) noTimeout

setConfigurationRequest :: Word8
setConfigurationRequest = 0x09


readTemperature :: DeviceHandle -> IO (Float)
readTemperature deviceHandle = do
    --Release the kernel driver
    withDetachedKernelDriver deviceHandle 0 $ do
        withDetachedKernelDriver deviceHandle 1 $ do
            writeControl deviceHandle Class ToInterface setConfigurationRequest 0x0201 0x00 (BS.pack [0x01, 0x01]) noTimeout
            controlTransfer deviceHandle temperatureCommand
            interruptRead deviceHandle
            controlTransfer deviceHandle ini1Command
            interruptRead deviceHandle
            controlTransfer deviceHandle ini2Command
            interruptRead deviceHandle
            interruptRead deviceHandle
            controlTransfer deviceHandle temperatureCommand
            (msg, status) <- interruptRead deviceHandle
            return (T.temperatureFrom msg)



main:: IO()
main = do
    usbContext <- newCtx
    devices <- getDevices usbContext
    temperDevices <- filterM isTemperDevice (toList devices)
    putStrLn (show (head temperDevices))
    withDeviceHandle (head temperDevices) $ \device -> do
        temp <- readTemperature device
        putStrLn (show temp)
