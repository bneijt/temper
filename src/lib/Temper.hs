module Temper where

import qualified Data.ByteString as BS


temperatureFrom :: BS.ByteString -> Float
temperatureFrom msg = (125.0 / 32000.0) * (fromIntegral measurement)
    where
        byte2Int = fromIntegral (BS.index msg 2) :: Integer
        byte3Int = fromIntegral (BS.index msg 3) :: Integer
        measurement = (256 :: Integer) * byte2Int + byte3Int