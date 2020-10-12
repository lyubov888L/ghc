
module GHC.Platform.Regs
       (callerSaves, activeStgRegs, haveRegBase, globalRegMaybe, freeReg)
       where

import GHC.Prelude

import GHC.Cmm.Expr
import GHC.Platform
import GHC.Platform.Reg

import qualified GHC.Platform.ARM         as ARM
import qualified GHC.Platform.ARM64       as ARM64
import qualified GHC.Platform.PPC         as PPC
import qualified GHC.Platform.S390X       as S390X
import qualified GHC.Platform.SPARC       as SPARC
import qualified GHC.Platform.X86         as X86
import qualified GHC.Platform.X86_64      as X86_64
import qualified GHC.Platform.RiscV64     as RiscV64
import qualified GHC.Platform.NoRegs      as NoRegs

-- | Returns 'True' if this global register is stored in a caller-saves
-- machine register.

callerSaves :: Platform -> GlobalReg -> Bool
callerSaves platform
 | platformUnregisterised platform = NoRegs.callerSaves
 | otherwise
 = case platformArch platform of
   ArchX86    -> X86.callerSaves
   ArchX86_64 -> X86_64.callerSaves
   ArchS390X  -> S390X.callerSaves
   ArchSPARC  -> SPARC.callerSaves
   ArchARM {} -> ARM.callerSaves
   ArchARM64  -> ARM64.callerSaves
   ArchRiscV64 -> RiscV64.callerSaves
   arch
    | arch `elem` [ArchPPC, ArchPPC_64 ELF_V1, ArchPPC_64 ELF_V2] ->
        PPC.callerSaves

    | otherwise -> NoRegs.callerSaves

-- | Here is where the STG register map is defined for each target arch.
-- The order matters (for the llvm backend anyway)! We must make sure to
-- maintain the order here with the order used in the LLVM calling conventions.
-- Note that also, this isn't all registers, just the ones that are currently
-- possibly mapped to real registers.
activeStgRegs :: Platform -> [GlobalReg]
activeStgRegs platform
 | platformUnregisterised platform = NoRegs.activeStgRegs
 | otherwise
 = case platformArch platform of
   ArchX86    -> X86.activeStgRegs
   ArchX86_64 -> X86_64.activeStgRegs
   ArchS390X  -> S390X.activeStgRegs
   ArchSPARC  -> SPARC.activeStgRegs
   ArchARM {} -> ARM.activeStgRegs
   ArchARM64  -> ARM64.activeStgRegs
   ArchRiscV64 -> RiscV64.activeStgRegs
   arch
    | arch `elem` [ArchPPC, ArchPPC_64 ELF_V1, ArchPPC_64 ELF_V2] ->
        PPC.activeStgRegs

    | otherwise -> NoRegs.activeStgRegs

haveRegBase :: Platform -> Bool
haveRegBase platform
 | platformUnregisterised platform = NoRegs.haveRegBase
 | otherwise
 = case platformArch platform of
   ArchX86    -> X86.haveRegBase
   ArchX86_64 -> X86_64.haveRegBase
   ArchS390X  -> S390X.haveRegBase
   ArchSPARC  -> SPARC.haveRegBase
   ArchARM {} -> ARM.haveRegBase
   ArchARM64  -> ARM64.haveRegBase
   ArchRiscV64 -> RiscV64.haveRegBase
   arch
    | arch `elem` [ArchPPC, ArchPPC_64 ELF_V1, ArchPPC_64 ELF_V2] ->
        PPC.haveRegBase

    | otherwise -> NoRegs.haveRegBase

globalRegMaybe :: Platform -> GlobalReg -> Maybe RealReg
globalRegMaybe platform
 | platformUnregisterised platform = NoRegs.globalRegMaybe
 | otherwise
 = case platformArch platform of
   ArchX86    -> X86.globalRegMaybe
   ArchX86_64 -> X86_64.globalRegMaybe
   ArchS390X  -> S390X.globalRegMaybe
   ArchSPARC  -> SPARC.globalRegMaybe
   ArchARM {} -> ARM.globalRegMaybe
   ArchARM64  -> ARM64.globalRegMaybe
   ArchRiscV64 -> RiscV64.globalRegMaybe
   arch
    | arch `elem` [ArchPPC, ArchPPC_64 ELF_V1, ArchPPC_64 ELF_V2] ->
        PPC.globalRegMaybe

    | otherwise -> NoRegs.globalRegMaybe

freeReg :: Platform -> RegNo -> Bool
freeReg platform
 | platformUnregisterised platform = NoRegs.freeReg
 | otherwise
 = case platformArch platform of
   ArchX86    -> X86.freeReg
   ArchX86_64 -> X86_64.freeReg
   ArchS390X  -> S390X.freeReg
   ArchSPARC  -> SPARC.freeReg
   ArchARM {} -> ARM.freeReg
   ArchARM64  -> ARM64.freeReg
   ArchRiscV64 -> RiscV64.freeReg
   arch
    | arch `elem` [ArchPPC, ArchPPC_64 ELF_V1, ArchPPC_64 ELF_V2] ->
        PPC.freeReg

    | otherwise -> NoRegs.freeReg
