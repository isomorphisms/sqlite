module SQLite.Varint

%default covering

public export
record DecodedVarint where
  constructor MkDecodedVarint
  value : Integer
  bytesRead : Nat
  rest : List Integer

public export
Eq DecodedVarint where
  left == right =
    left.value == right.value
      && left.bytesRead == right.bytesRead
      && left.rest == right.rest

public export
Show DecodedVarint where
  show decoded =
    "varint(" ++ show decoded.value
      ++ ", " ++ show decoded.bytesRead ++ " bytes)"

maximumUnsigned64 : Integer
maximumUnsigned64 = 18446744073709551615

maximumEightGroupPayload : Integer
maximumEightGroupPayload = 72057594037927935

littleGroups : Nat → Integer → List Integer
littleGroups Z value = [value `mod` 128]
littleGroups (S fuel) value =
  let low = value `mod` 128
      high = value `div` 128
   in if high == 0
        then [low]
        else low :: littleGroups fuel high

markContinuation : List Integer → List Integer
markContinuation [] = []
markContinuation [last] = [last]
markContinuation (first :: rest) = first + 128 :: markContinuation rest

exactGroups : Nat → Integer → List Integer
exactGroups Z _ = []
exactGroups (S amount) value =
  value `mod` 128 :: exactGroups amount (value `div` 128)

public export
encode : Integer → Either String (List Integer)
encode value =
  if value < 0
    then Left "a SQLite varint cannot encode a negative integer"
    else if value > maximumUnsigned64
      then Left "integer exceeds SQLite's unsigned 64-bit varint range"
      else if value <= maximumEightGroupPayload
        then Right (markContinuation (reverse (littleGroups 7 value)))
        else
          let final = value `mod` 256
              prefixValue = value `div` 256
              leadingBytes = map (+ 128) (reverse (exactGroups 8 prefixValue))
           in Right (leadingBytes ++ [final])

validByte : Integer → Bool
validByte byte = byte >= 0 && byte <= 255

decodeStep : Nat → Nat → Integer → List Integer → Either String DecodedVarint
decodeStep _ _ _ [] = Left "truncated SQLite varint"
decodeStep Z count accumulated (byte :: rest) =
  if validByte byte
    then
      let answer = accumulated * 256 + byte
       in if answer <= maximumUnsigned64
            then Right (MkDecodedVarint answer (S count) rest)
            else Left "decoded value exceeds unsigned 64-bit range"
    else Left "varint input contains a value outside the byte range"
decodeStep (S prefixSlots) count accumulated (byte :: rest) =
  if not (validByte byte)
    then Left "varint input contains a value outside the byte range"
    else
      let payload = if byte >= 128 then byte - 128 else byte
          answer = accumulated * 128 + payload
       in if byte < 128
            then Right (MkDecodedVarint answer (S count) rest)
            else decodeStep prefixSlots (S count) answer rest

public export
decode : List Integer → Either String DecodedVarint
decode = decodeStep 8 0 0
