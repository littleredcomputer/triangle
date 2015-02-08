module Targa (
  TargaImage(..),
  load_targa_image
) where

import Data.Binary.Get
import Data.Word
import System.IO
import qualified Data.ByteString.Lazy as BL
import qualified Data.ByteString as B

-- Read an uncompressed (no RLE) Targa file, for the purposes of using
-- it on OpenGL. We only parse a subset of the format; this code is very
-- far from general purpose

data TargaImage =
    TargaImage {
      idlen::Word8,
      cmap_type::Word8,
      img_type::Word8,
      cmap_org::Word16,
      cmap_len::Word16,
      cmap_size::Word8,
      xo::Word16,
      yo::Word16,
      width::Word16,
      height::Word16,
      depth::Word8,
      img_desc::Word8,
      comment::B.ByteString,
      img_data::BL.ByteString
    }

parse_targa = do
  idlen     <- getWord8
  cmap_type <- getWord8
  img_type  <- getWord8
  cmap_org  <- getWord16le
  cmap_len  <- getWord16le
  cmap_size <- getWord8
  xo        <- getWord16le
  yo        <- getWord16le
  width     <- getWord16le
  height    <- getWord16le
  depth     <- getWord8
  img_desc  <- getWord8
  comment   <- getByteString $ fromIntegral idlen
  img_data  <- getRemainingLazyByteString

  return $ TargaImage idlen cmap_type img_type cmap_org cmap_len cmap_size
                      xo yo width height depth img_desc comment img_data

load_targa_image fname =
  openBinaryFile fname ReadMode
    >>= BL.hGetContents >>= (return . runGet parse_targa)
