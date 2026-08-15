module SQLite.Record

import SQLite.Value
import SQLite.Varint

%default covering

validByte : Integer -> Bool
validByte byte = byte >= 0 && byte <= 255

takeBytes : Nat -> List Integer -> Either String (List Integer, List Integer)
takeBytes Z bytes = Right ([], bytes)
takeBytes (S amount) [] = Left "record payload is truncated"
takeBytes (S amount) (byte :: rest) =
  if validByte byte
    then do
      (taken, remaining) <- takeBytes amount rest
      pure (byte :: taken, remaining)
    else Left "record payload contains a value outside the byte range"

takePayload : Integer -> List Integer -> Either String (List Integer, List Integer)
takePayload amount bytes =
  if amount < 0
    then Left "record payload length is negative"
    else if amount > cast (length bytes)
      then Left
        ("record payload needs " ++ show amount
          ++ " bytes but only " ++ show (length bytes) ++ " remain")
      else takeBytes (cast amount) bytes

unsignedBigEndian : List Integer -> Integer
unsignedBigEndian = foldl (\answer, byte => answer * 256 + byte) 0

signedBigEndian : List Integer -> Integer
signedBigEndian [] = 0
signedBigEndian bytes@(first :: _) =
  let unsigned = unsignedBigEndian bytes
      modulus = powInteger 256 (length bytes)
   in if first >= 128 then unsigned - modulus else unsigned
  where
    powInteger : Integer -> Nat -> Integer
    powInteger _ Z = 1
    powInteger base (S exponent) = base * powInteger base exponent

applySign : Bool -> Double -> Double
applySign False value = value
applySign True value = negate value

ieee754Double : List Integer -> Either String Double
ieee754Double bytes = do
  (eight, remaining) <- takeBytes 8 bytes
  case remaining of
    [] =>
      let bits = unsignedBigEndian eight
          negative = bits >= 9223372036854775808
          exponent = (bits `div` 4503599627370496) `mod` 2048
          fraction = bits `mod` 4503599627370496
          fractionAsDouble : Double
          fractionAsDouble = cast fraction
          magnitude =
            if exponent == 2047
              then if fraction == 0 then 1.0 / 0.0 else 0.0 / 0.0
              else if exponent == 0
                then fractionAsDouble * pow 2.0 (-1074.0)
                else
                  (1.0 + fractionAsDouble / 4503599627370496.0)
                    * pow 2.0 (cast (exponent - 1023))
       in Right (applySign negative magnitude)
    _ => Left "an IEEE-754 value must contain exactly eight bytes"

continuationByte : List Integer -> Either String (Integer, List Integer)
continuationByte [] = Left "truncated UTF-8 sequence"
continuationByte (byte :: rest) =
  if not (validByte byte)
    then Left "UTF-8 input contains a value outside the byte range"
    else if byte < 128 || byte > 191
      then Left ("invalid UTF-8 continuation byte " ++ show byte)
      else Right (byte, rest)

utf8Characters : List Integer -> Either String (List Char)
utf8Characters [] = Right []
utf8Characters (first :: rest) =
  if not (validByte first)
    then Left "UTF-8 input contains a value outside the byte range"
    else if first <= 127
      then do
        later <- utf8Characters rest
        pure (chr (cast first) :: later)
      else if first >= 194 && first <= 223
        then do
          (second, afterSecond) <- continuationByte rest
          let codepoint = (first - 192) * 64 + second - 128
          later <- utf8Characters afterSecond
          pure (chr (cast codepoint) :: later)
        else if first >= 224 && first <= 239
          then do
            (second, afterSecond) <- continuationByte rest
            (third, afterThird) <- continuationByte afterSecond
            if first == 224 && second < 160
              then Left "overlong three-byte UTF-8 sequence"
              else if first == 237 && second > 159
                then Left "UTF-8 sequence encodes a surrogate code point"
                else do
                  let codepoint =
                        (first - 224) * 4096
                          + (second - 128) * 64
                          + third - 128
                  later <- utf8Characters afterThird
                  pure (chr (cast codepoint) :: later)
          else if first >= 240 && first <= 244
            then do
              (second, afterSecond) <- continuationByte rest
              (third, afterThird) <- continuationByte afterSecond
              (fourth, afterFourth) <- continuationByte afterThird
              if first == 240 && second < 144
                then Left "overlong four-byte UTF-8 sequence"
                else if first == 244 && second > 143
                  then Left "UTF-8 sequence exceeds U+10FFFF"
                  else do
                    let codepoint =
                          (first - 240) * 262144
                            + (second - 128) * 4096
                            + (third - 128) * 64
                            + fourth - 128
                    later <- utf8Characters afterFourth
                    pure (chr (cast codepoint) :: later)
            else Left ("invalid UTF-8 leading byte " ++ show first)

public export
decodeUtf8 : List Integer -> Either String String
decodeUtf8 bytes = map pack (utf8Characters bytes)

headerTypes : List Integer -> Either String (List Integer)
headerTypes [] = Right []
headerTypes bytes = do
  decoded <- SQLite.Varint.decode bytes
  later <- headerTypes decoded.rest
  pure (decoded.value :: later)

integerValue : Integer -> List Integer -> Either String (SqlValue, List Integer)
integerValue amount input = do
  (encoded, remaining) <- takePayload amount input
  pure (SqlInteger (signedBigEndian encoded), remaining)

decodeSerialValue : Integer -> List Integer -> Either String (SqlValue, List Integer)
decodeSerialValue 0 bytes = Right (SqlNull, bytes)
decodeSerialValue 1 bytes = integerValue 1 bytes
decodeSerialValue 2 bytes = integerValue 2 bytes
decodeSerialValue 3 bytes = integerValue 3 bytes
decodeSerialValue 4 bytes = integerValue 4 bytes
decodeSerialValue 5 bytes = integerValue 6 bytes
decodeSerialValue 6 bytes = integerValue 8 bytes
decodeSerialValue 7 bytes = do
  (encoded, remaining) <- takePayload 8 bytes
  value <- ieee754Double encoded
  pure (if value == value then SqlReal value else SqlNull, remaining)
decodeSerialValue 8 bytes = Right (SqlInteger 0, bytes)
decodeSerialValue 9 bytes = Right (SqlInteger 1, bytes)
decodeSerialValue 10 _ = Left "SQLite serial type 10 is reserved"
decodeSerialValue 11 _ = Left "SQLite serial type 11 is reserved"
decodeSerialValue serialType bytes =
  if serialType < 12
    then Left ("invalid SQLite serial type " ++ show serialType)
    else if serialType `mod` 2 == 0
      then do
        let byteCount = (serialType - 12) `div` 2
        (blob, remaining) <- takePayload byteCount bytes
        pure (SqlBlob blob, remaining)
      else do
        let byteCount = (serialType - 13) `div` 2
        (encoded, remaining) <- takePayload byteCount bytes
        let value = case decodeUtf8 encoded of
              Right text => SqlText text
              Left _ => SqlTextBytes encoded
        pure (value, remaining)

decodeValues : List Integer -> List Integer -> Either String (List SqlValue, List Integer)
decodeValues [] bytes = Right ([], bytes)
decodeValues (serialType :: laterTypes) bytes = do
  (value, remaining) <- decodeSerialValue serialType bytes
  (laterValues, trailing) <- decodeValues laterTypes remaining
  pure (value :: laterValues, trailing)

||| Decode one complete SQLite record payload. The first varint is the total
||| header size, including that varint; the remaining header varints are the
||| serial types whose values are stored consecutively in the record body.
public export
decodeRecord : List Integer -> Either String (List SqlValue)
decodeRecord payload = do
  headerSize <- SQLite.Varint.decode payload
  let minimumHeaderSize : Integer = cast headerSize.bytesRead
  if headerSize.value < minimumHeaderSize
    then Left "record header size is smaller than its own varint"
    else if headerSize.value > cast (length payload)
      then Left
        ("record header claims " ++ show headerSize.value
          ++ " bytes but the payload has only " ++ show (length payload))
      else do
        let typeBytes = headerSize.value - minimumHeaderSize
        (encodedTypes, body) <- takePayload typeBytes headerSize.rest
        serialTypes <- headerTypes encodedTypes
        (values, trailing) <- decodeValues serialTypes body
        case trailing of
          [] => Right values
          _ => Left
            ("record body has " ++ show (length trailing)
              ++ " unclaimed bytes")
