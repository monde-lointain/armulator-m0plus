module Main where

import Data.Bits
import qualified Data.ByteString.Lazy as L
import Data.Word
import Text.Printf

decodeInstruction16 :: Word16 -> String
decodeInstruction16 inst
    | (inst .&. 0x7E00) == 0x1C00 = addImm1
    | (inst .&. 0x7800) == 0x3000 = addImm2
    | (inst .&. 0x7E00) == 0x1800 = addReg1
    | (inst .&. 0xFF00) == 0x4400 = addReg2
    | (inst .&. 0xF800) == 0x2800 = cmpImm
    | (inst .&. 0xFFC0) == 0x4280 = cmpReg1
    | (inst .&. 0xFF00) == 0x4500 = cmpReg2
    | (inst .&. 0xF800) == 0x2000 = movImm
    | (inst .&. 0xFFC0) == 0x0000 = movReg1
    | (inst .&. 0xFF00) == 0x4600 = movReg2
    | (inst .&. 0x7E00) == 0x1E00 = subImm1
    | (inst .&. 0xF800) == 0x3800 = subImm2
    | (inst .&. 0x7E00) == 0x1A00 = subReg1
    | otherwise = "Unimplemented 16-bit instruction"
  where
    addImm1 = printf "adds r%u, r%u, #%u" rd rn rb
      where
        rd = inst .&. 0x7
        rn = (inst `shiftR` 3) .&. 0x7
        rb = (inst `shiftR` 6) .&. 0x7

    addImm2 = printf "adds r%u, #%u" rd rb
      where
        rb = inst .&. 0xFF
        rd = (inst `shiftR` 8) .&. 0x7

    addReg1 = printf "adds r%u, r%u, r%u" rd rn rm
      where
        rd = inst .&. 0x7
        rn = (inst `shiftR` 3) .&. 0x7
        rm = (inst `shiftR` 6) .&. 0x7

    addReg2 = printf "add r%u, r%u" rdn rm
      where
        rdn = (inst .&. 0x7) .|. ((inst `shiftR` 4) .&. 0x8)
        rm = (inst `shiftR` 3) .&. 0xF

    cmpImm = printf "cmp r%u, #%u" rn rb
      where
        rb = inst .&. 0xFF
        rn = (inst `shiftR` 8) .&. 0x7

    cmpReg1 = printf "cmp r%u, r%u" rn rm
      where
        rn = inst .&. 0x7
        rm = (inst `shiftR` 3) .&. 0x7

    cmpReg2 = printf "cmp r%u, r%u" rn rm
      where
        rn = (inst .&. 0x7) .|. ((inst `shiftR` 4) .&. 0x8)
        rm = (inst `shiftR` 3) .&. 0xF

    movImm = printf "movs r%u, #%u" rd rb
      where
        rb = inst .&. 0xFF
        rd = (inst `shiftR` 8) .&. 0x7

    movReg1 = printf "movs r%u, r%u" rd rn
      where
        rd = inst .&. 0x7
        rn = (inst `shiftR` 3) .&. 0x7

    movReg2 = printf "movs r%u, r%u" rdn rm
      where
        rdn = (inst .&. 0x7) .|. ((inst `shiftR` 4) .&. 0x8)
        rm = (inst `shiftR` 3) .&. 0xF

    subImm1 = printf "subs r%u, r%u, #%u" rd rn rb
      where
        rd = inst .&. 0x7
        rn = (inst `shiftR` 3) .&. 0x7
        rb = (inst `shiftR` 6) .&. 0x7

    subImm2 = printf "subs r%u, #%u" rd rb
      where
        rb = inst .&. 0xFF
        rd = (inst `shiftR` 8) .&. 0x7

    subReg1 = printf "subs r%u, r%u, r%u" rd rn rm
      where
        rd = inst .&. 0x7
        rn = (inst `shiftR` 3) .&. 0x7
        rm = (inst `shiftR` 6) .&. 0x7

decodeInstruction32 :: Word32 -> String
decodeInstruction32 inst = "Unimplemented 32-bit instruction"

main :: IO ()
main = do
    byteStream <- L.readFile "fixed_mul.bin"
    let instructions = decodeInstructions byteStream []
    mapM_ putStrLn (reverse instructions)

decodeInstructions :: L.ByteString -> [String] -> [String]
decodeInstructions byteStream accum
    | L.null byteStream = accum
    | otherwise =
        let (firstHalfword, rest) = L.splitAt 2 byteStream
            inst = readHalfWordLE firstHalfword
         in if is32BitInstruction inst
                then do
                    let (secondHalfword, remaining) = L.splitAt 2 rest
                        inst32 = readHalfWordLE secondHalfword
                    decodeInstructions remaining (decodeInstruction32 inst32 : accum)
                else do
                    decodeInstructions rest (decodeInstruction16 inst : accum)

readHalfWordLE :: (Bits a, Num a) => L.ByteString -> a
readHalfWordLE = L.foldr (\x acc -> (acc `shiftL` 8) .|. fromIntegral x) 0

is32BitInstruction :: Word16 -> Bool
is32BitInstruction inst = (inst .&. 0xF800) `elem` [0xE800, 0xF000, 0xF800]
