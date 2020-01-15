{-# LANGUAGE DeriveGeneric, MultiParamTypeClasses, FlexibleInstances #-}
module EVM.Bytecode where

import Prelude (Show, Eq, Ord, concatMap, (++), (==), error)
import GHC.Generics
--import Test.SmallCheck.Series
import qualified Data.ByteString as B
import Data.Word

data Opcode
  = INVALID Word8
  | STOP
  | ADD
  | MUL
  | SUB
  | DIV
  | SDIV
  | MOD
  | SMOD
  | ADDMOD
  | MULMOD
  | EXP
  | SIGNEXTEND
  | LT
  | GT
  | SLT
  | SGT
  | EQ
  | ISZERO
  | AND
  | OR
  | XOR
  | NOT
  | BYTE
  | SHL
  | SHR
  | SAR
  | SHA3
  | ADDRESS
  | BALANCE
  | ORIGIN
  | CALLER
  | CALLVALUE
  | CALLDATALOAD
  | CALLDATASIZE
  | CALLDATACOPY
  | CODESIZE
  | CODECOPY
  | GASPRICE
  | EXTCODESIZE
  | EXTCODECOPY
  | RETURNDATASIZE
  | RETURNDATACOPY
  | EXTCODEHASH
  | BLOCKHASH
  | COINBASE
  | TIMESTAMP
  | NUMBER
  | DIFFICULTY
  | GASLIMIT
  | POP
  | MLOAD
  | MSTORE
  | MSTORE8
  | SLOAD
  | SSTORE
  | JUMP
  | JUMPI
  | PC
  | MSIZE
  | GAS
  | JUMPDEST
  | PUSH1 B.ByteString
  | PUSH2 B.ByteString
  | PUSH3 B.ByteString
  | PUSH4 B.ByteString
  | PUSH5 B.ByteString
  | PUSH6 B.ByteString
  | PUSH7 B.ByteString
  | PUSH8 B.ByteString
  | PUSH9 B.ByteString
  | PUSH10 B.ByteString
  | PUSH11 B.ByteString
  | PUSH12 B.ByteString
  | PUSH13 B.ByteString
  | PUSH14 B.ByteString
  | PUSH15 B.ByteString
  | PUSH16 B.ByteString
  | PUSH17 B.ByteString
  | PUSH18 B.ByteString
  | PUSH19 B.ByteString
  | PUSH20 B.ByteString
  | PUSH21 B.ByteString
  | PUSH22 B.ByteString
  | PUSH23 B.ByteString
  | PUSH24 B.ByteString
  | PUSH25 B.ByteString
  | PUSH26 B.ByteString
  | PUSH27 B.ByteString
  | PUSH28 B.ByteString
  | PUSH29 B.ByteString
  | PUSH30 B.ByteString
  | PUSH31 B.ByteString
  | PUSH32 B.ByteString
  | DUP1
  | DUP2
  | DUP3
  | DUP4
  | DUP5
  | DUP6
  | DUP7
  | DUP8
  | DUP9
  | DUP10
  | DUP11
  | DUP12
  | DUP13
  | DUP14
  | DUP15
  | DUP16
  | SWAP1
  | SWAP2
  | SWAP3
  | SWAP4
  | SWAP5
  | SWAP6
  | SWAP7
  | SWAP8
  | SWAP9
  | SWAP10
  | SWAP11
  | SWAP12
  | SWAP13
  | SWAP14
  | SWAP15
  | SWAP16
  | LOG0
  | LOG1
  | LOG2
  | LOG3
  | LOG4
  | CREATE
  | CALL
  | CALLCODE
  | RETURN
  | DELEGATECALL
  | CREATE2
  | STATICCALL
  | REVERT
  | SELFDESTRUCT  
  deriving (Show, Eq, Ord, Generic)

--instance Monad m => Serial m Opcode

--instance Generic B.ByteString where

--instance Monad m => Serial m B.ByteString

type Bytecode = [Opcode]

decode :: B.ByteString -> Bytecode
decode bs =
  if B.null bs then
    []
  else
    case opc of
      0x00 -> STOP : more
      0x01 -> ADD : more
      0x02 -> MUL : more
      0x03 -> SUB : more
      0x04 -> DIV : more
      0x05 -> SDIV : more
      0x06 -> MOD : more
      0x07 -> SMOD : more
      0x08 -> ADDMOD : more
      0x09 -> MULMOD : more
      0x0A -> EXP : more
      0x0B -> SIGNEXTEND : more
      0x10 -> LT : more
      0x11 -> GT : more
      0x12 -> SLT : more
      0x13 -> SGT : more
      0x14 -> EQ : more
      0x15 -> ISZERO : more
      0x16 -> AND : more
      0x17 -> OR : more
      0x18 -> XOR : more
      0x19 -> NOT : more
      0x1A -> BYTE : more
      0x1B -> SHL : more
      0x1C -> SHR : more
      0x1D -> SAR : more
      0x20 -> SHA3 : more
      0x30 -> ADDRESS : more
      0x31 -> BALANCE : more
      0x32 -> ORIGIN : more
      0x33 -> CALLER : more
      0x34 -> CALLVALUE : more
      0x35 -> CALLDATALOAD : more
      0x36 -> CALLDATASIZE : more
      0x37 -> CALLDATACOPY : more
      0x38 -> CODESIZE : more
      0x39 -> CODECOPY : more
      0x3A -> GASPRICE : more
      0x3B -> EXTCODESIZE : more
      0x3C -> EXTCODECOPY : more
      0x3D -> RETURNDATASIZE : more
      0x3E -> RETURNDATACOPY : more
      0x3F -> EXTCODEHASH : more
      0x40 -> BLOCKHASH : more
      0x41 -> COINBASE : more
      0x42 -> TIMESTAMP : more
      0x43 -> NUMBER : more
      0x44 -> DIFFICULTY : more
      0x45 -> GASLIMIT : more
      0x50 -> POP : more
      0x51 -> MLOAD : more
      0x52 -> MSTORE : more
      0x53 -> MSTORE8 : more
      0x54 -> SLOAD : more
      0x55 -> SSTORE : more
      0x56 -> JUMP : more
      0x57 -> JUMPI : more
      0x58 -> PC : more
      0x59 -> MSIZE : more
      0x5A -> GAS : more
      0x5B -> JUMPDEST : more
      0x60 -> next PUSH1 1
      0x61 -> next PUSH2 2
      0x62 -> next PUSH3 3
      0x63 -> next PUSH4 4
      0x64 -> next PUSH5 5
      0x65 -> next PUSH6 6
      0x66 -> next PUSH7 7
      0x67 -> next PUSH8 8
      0x68 -> next PUSH9 9
      0x69 -> next PUSH10 10
      0x6A -> next PUSH11 11
      0x6B -> next PUSH12 12
      0x6C -> next PUSH13 13
      0x6D -> next PUSH14 14
      0x6E -> next PUSH15 15
      0x6F -> next PUSH16 16
      0x70 -> next PUSH17 17
      0x71 -> next PUSH18 18
      0x72 -> next PUSH19 19
      0x73 -> next PUSH20 20
      0x74 -> next PUSH21 21
      0x75 -> next PUSH22 22
      0x76 -> next PUSH23 23
      0x77 -> next PUSH24 24
      0x78 -> next PUSH25 25
      0x79 -> next PUSH26 26
      0x7A -> next PUSH27 27
      0x7B -> next PUSH28 28
      0x7C -> next PUSH29 29
      0x7D -> next PUSH30 30
      0x7E -> next PUSH31 31
      0x7F -> next PUSH32 32
      0x80 -> DUP1 : more
      0x81 -> DUP2 : more
      0x82 -> DUP3 : more
      0x83 -> DUP4 : more
      0x84 -> DUP5 : more
      0x85 -> DUP6 : more
      0x86 -> DUP7 : more
      0x87 -> DUP8 : more
      0x88 -> DUP9 : more
      0x89 -> DUP10 : more
      0x8A -> DUP11 : more
      0x8B -> DUP12 : more
      0x8C -> DUP13 : more
      0x8D -> DUP14 : more
      0x8E -> DUP15 : more
      0x8F -> DUP16 : more
      0x90 -> SWAP1 : more
      0x91 -> SWAP2 : more
      0x92 -> SWAP3 : more
      0x93 -> SWAP4 : more
      0x94 -> SWAP5 : more
      0x95 -> SWAP6 : more
      0x96 -> SWAP7 : more
      0x97 -> SWAP8 : more
      0x98 -> SWAP9 : more
      0x99 -> SWAP10 : more
      0x9A -> SWAP11 : more
      0x9B -> SWAP12 : more
      0x9C -> SWAP13 : more
      0x9D -> SWAP14 : more
      0x9E -> SWAP15 : more
      0x9F -> SWAP16 : more
      0xA0 -> LOG0 : more
      0xA1 -> LOG1 : more
      0xA2 -> LOG2 : more
      0xA3 -> LOG3 : more
      0xA4 -> LOG4 : more
      0xF0 -> CREATE : more
      0xF1 -> CALL : more
      0xF2 -> CALLCODE : more
      0xF3 -> RETURN : more
      0xF4 -> DELEGATECALL : more
      0xF5 -> CREATE2 : more
      0xFA -> STATICCALL : more
      0xFD -> REVERT : more
      0xFF -> SELFDESTRUCT : more
      _ -> INVALID opc : more
  where opc = B.head bs
        r = B.tail bs
        more = decode r
        next o k = o ks : decode rks
          where (ks, rks) = B.splitAt k r

encode :: Bytecode -> B.ByteString
encode bc = B.pack (concatMap encode1 bc)

encode1 :: Opcode -> [Word8]
encode1 op =
  case op of
      STOP -> [0x00]
      ADD -> [0x01]
      MUL -> [0x02]
      SUB -> [0x03]
      DIV -> [0x04]
      SDIV -> [0x05]
      MOD -> [0x06]
      SMOD -> [0x07]
      ADDMOD -> [0x08]
      MULMOD -> [0x09]
      EXP -> [0x0A]
      SIGNEXTEND -> [0x0B]
      LT -> [0x10]
      GT -> [0x11]
      SLT -> [0x12]
      SGT -> [0x13]
      EQ -> [0x14]
      ISZERO -> [0x15]
      AND -> [0x16]
      OR -> [0x17]
      XOR -> [0x18]
      NOT -> [0x19]
      BYTE -> [0x1A]
      SHL -> [0x1B]
      SHR -> [0x1C]
      SAR -> [0x1D]
      SHA3 -> [0x20]
      ADDRESS -> [0x30]
      BALANCE -> [0x31]
      ORIGIN -> [0x32]
      CALLER -> [0x33]
      CALLVALUE -> [0x34]
      CALLDATALOAD -> [0x35]
      CALLDATASIZE -> [0x36]
      CALLDATACOPY -> [0x37]
      CODESIZE -> [0x38]
      CODECOPY -> [0x39]
      GASPRICE -> [0x3A]
      EXTCODESIZE -> [0x3B]
      EXTCODECOPY -> [0x3C]
      RETURNDATASIZE -> [0x3D]
      RETURNDATACOPY -> [0x3E]
      EXTCODEHASH -> [0x3F]
      BLOCKHASH -> [0x40]
      COINBASE -> [0x41]
      TIMESTAMP -> [0x42]
      NUMBER -> [0x43]
      DIFFICULTY -> [0x44]
      GASLIMIT -> [0x45]
      POP -> [0x50]
      MLOAD -> [0x51]
      MSTORE -> [0x52]
      MSTORE8 -> [0x53]
      SLOAD -> [0x54]
      SSTORE -> [0x55]
      JUMP -> [0x56]
      JUMPI -> [0x57]
      PC -> [0x58]
      MSIZE -> [0x59]
      GAS -> [0x5A]
      JUMPDEST -> [0x5B]
      PUSH1 n -> [0x60] ++ next n 1
      PUSH2 n -> [0x61] ++ next n 2
      PUSH3 n -> [0x62] ++ next n 3
      PUSH4 n -> [0x63] ++ next n 4
      PUSH5 n -> [0x64] ++ next n 5
      PUSH6 n -> [0x65] ++ next n 6
      PUSH7 n -> [0x66] ++ next n 7
      PUSH8 n -> [0x67] ++ next n 8
      PUSH9 n -> [0x68] ++ next n 9
      PUSH10 n -> [0x69] ++ next n 10
      PUSH11 n -> [0x6A] ++ next n 11
      PUSH12 n -> [0x6B] ++ next n 12
      PUSH13 n -> [0x6C] ++ next n 13
      PUSH14 n -> [0x6D] ++ next n 14
      PUSH15 n -> [0x6E] ++ next n 15
      PUSH16 n -> [0x6F] ++ next n 16
      PUSH17 n -> [0x70] ++ next n 17
      PUSH18 n -> [0x71] ++ next n 18
      PUSH19 n -> [0x72] ++ next n 19
      PUSH20 n -> [0x73] ++ next n 20
      PUSH21 n -> [0x74] ++ next n 21
      PUSH22 n -> [0x75] ++ next n 22
      PUSH23 n -> [0x76] ++ next n 23
      PUSH24 n -> [0x77] ++ next n 24
      PUSH25 n -> [0x78] ++ next n 25
      PUSH26 n -> [0x79] ++ next n 26
      PUSH27 n -> [0x7A] ++ next n 27
      PUSH28 n -> [0x7B] ++ next n 28
      PUSH29 n -> [0x7C] ++ next n 29
      PUSH30 n -> [0x7D] ++ next n 30
      PUSH31 n -> [0x7E] ++ next n 31
      PUSH32 n -> [0x7F] ++ next n 32
      DUP1 -> [0x80]
      DUP2 -> [0x81]
      DUP3 -> [0x82]
      DUP4 -> [0x83]
      DUP5 -> [0x84]
      DUP6 -> [0x85]
      DUP7 -> [0x86]
      DUP8 -> [0x87]
      DUP9 -> [0x88]
      DUP10 -> [0x89]
      DUP11 -> [0x8A]
      DUP12 -> [0x8B]
      DUP13 -> [0x8C]
      DUP14 -> [0x8D]
      DUP15 -> [0x8E]
      DUP16 -> [0x8F]
      SWAP1 -> [0x90]
      SWAP2 -> [0x91]
      SWAP3 -> [0x92]
      SWAP4 -> [0x93]
      SWAP5 -> [0x94]
      SWAP6 -> [0x95]
      SWAP7 -> [0x96]
      SWAP8 -> [0x97]
      SWAP9 -> [0x98]
      SWAP10 -> [0x99]
      SWAP11 -> [0x9A]
      SWAP12 -> [0x9B]
      SWAP13 -> [0x9C]
      SWAP14 -> [0x9D]
      SWAP15 -> [0x9E]
      SWAP16 -> [0x9F]
      LOG0 -> [0xA0]
      LOG1 -> [0xA1]
      LOG2 -> [0xA2]
      LOG3 -> [0xA3]
      LOG4 -> [0xA4]
      CREATE -> [0xF0]
      CALL -> [0xF1]
      CALLCODE -> [0xF2]
      RETURN -> [0xF3]
      DELEGATECALL -> [0xF4]
      CREATE2 -> [0xF5]
      STATICCALL -> [0xFA]
      REVERT -> [0xFD]
      SELFDESTRUCT -> [0xFF]
      INVALID opc -> [opc]
  where next n k =
          if B.length n == k then B.unpack n
          else error "encode: illegal lengthed constant"
