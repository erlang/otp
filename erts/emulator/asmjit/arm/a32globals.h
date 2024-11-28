// This file is part of AsmJit project <https://asmjit.com>
//
// See asmjit.h or LICENSE.md for license and copyright information
// SPDX-License-Identifier: Zlib

#ifndef ASMJIT_ARM_A32GLOBALS_H_INCLUDED
#define ASMJIT_ARM_A32GLOBALS_H_INCLUDED

#include "../arm/armglobals.h"

//! \namespace asmjit::a32
//! \ingroup asmjit_a32
//!
//! AArch32 backend.

ASMJIT_BEGIN_SUB_NAMESPACE(a32)

//! \addtogroup asmjit_a32
//! \{

//! ARM32/THUMB instruction.
//!
//! \note Only used to hold ARM-specific enumerations and static functions.
struct Inst {
  //! Instruction id.
  enum Id : uint32_t {
    // ${a32::InstId:Begin}
    // ------------------- Automatically generated, do not edit -------------------
    kIdNone = 0,  //!< Instruction '<none>'.
    kIdAdc,       //!< Instruction 'adc'.
    kIdAdcs,      //!< Instruction 'adcs'.
    kIdAdd,       //!< Instruction 'add'.
    kIdAdds,      //!< Instruction 'adds'.
    kIdAdr,       //!< Instruction 'adr'.
    kIdAesd,      //!< Instruction 'aesd' {AES}.
    kIdAese,      //!< Instruction 'aese' {AES}.
    kIdAesimc,    //!< Instruction 'aesimc' {AES}.
    kIdAesmc,     //!< Instruction 'aesmc' {AES}.
    kIdAnd,       //!< Instruction 'and'.
    kIdAnds,      //!< Instruction 'ands'.
    kIdAsr,       //!< Instruction 'asr'.
    kIdAsrs,      //!< Instruction 'asrs'.
    kIdB,         //!< Instruction 'b'.
    kIdBfc,       //!< Instruction 'bfc'.
    kIdBfi,       //!< Instruction 'bfi'.
    kIdBic,       //!< Instruction 'bic'.
    kIdBics,      //!< Instruction 'bics'.
    kIdBkpt,      //!< Instruction 'bkpt'.
    kIdBl,        //!< Instruction 'bl'.
    kIdBlx,       //!< Instruction 'blx'.
    kIdBx,        //!< Instruction 'bx'.
    kIdBxj,       //!< Instruction 'bxj'.
    kIdCbnz,      //!< Instruction 'cbnz' (THUMB).
    kIdCbz,       //!< Instruction 'cbz' (THUMB).
    kIdClrbhb,    //!< Instruction 'clrbhb' {CLRBHB}.
    kIdClrex,     //!< Instruction 'clrex'.
    kIdClz,       //!< Instruction 'clz'.
    kIdCmn,       //!< Instruction 'cmn'.
    kIdCmp,       //!< Instruction 'cmp'.
    kIdCps,       //!< Instruction 'cps' (ARM).
    kIdCpsid,     //!< Instruction 'cpsid' (ARM).
    kIdCpsie,     //!< Instruction 'cpsie' (ARM).
    kIdCrc32b,    //!< Instruction 'crc32b' {CRC32}.
    kIdCrc32cb,   //!< Instruction 'crc32cb' {CRC32}.
    kIdCrc32ch,   //!< Instruction 'crc32ch' {CRC32}.
    kIdCrc32cw,   //!< Instruction 'crc32cw' {CRC32}.
    kIdCrc32h,    //!< Instruction 'crc32h' {CRC32}.
    kIdCrc32w,    //!< Instruction 'crc32w' {CRC32}.
    kIdCsdb,      //!< Instruction 'csdb'.
    kIdDbg,       //!< Instruction 'dbg' {ARMv8-}.
    kIdDcps1,     //!< Instruction 'dcps1' (THUMB).
    kIdDcps2,     //!< Instruction 'dcps2' (THUMB).
    kIdDcps3,     //!< Instruction 'dcps3' (THUMB).
    kIdDmb,       //!< Instruction 'dmb'.
    kIdDsb,       //!< Instruction 'dsb'.
    kIdEor,       //!< Instruction 'eor'.
    kIdEors,      //!< Instruction 'eors'.
    kIdEret,      //!< Instruction 'eret' (ARM).
    kIdEsb,       //!< Instruction 'esb' {RAS}.
    kIdHlt,       //!< Instruction 'hlt'.
    kIdHvc,       //!< Instruction 'hvc' (ARM).
    kIdIsb,       //!< Instruction 'isb'.
    kIdIt,        //!< Instruction 'it' (THUMB).
    kIdIte,       //!< Instruction 'ite' (THUMB).
    kIdItee,      //!< Instruction 'itee' (THUMB).
    kIdIteee,     //!< Instruction 'iteee' (THUMB).
    kIdIteet,     //!< Instruction 'iteet' (THUMB).
    kIdItet,      //!< Instruction 'itet' (THUMB).
    kIdItete,     //!< Instruction 'itete' (THUMB).
    kIdItett,     //!< Instruction 'itett' (THUMB).
    kIdItt,       //!< Instruction 'itt' (THUMB).
    kIdItte,      //!< Instruction 'itte' (THUMB).
    kIdIttee,     //!< Instruction 'ittee' (THUMB).
    kIdIttet,     //!< Instruction 'ittet' (THUMB).
    kIdIttt,      //!< Instruction 'ittt' (THUMB).
    kIdIttte,     //!< Instruction 'ittte' (THUMB).
    kIdItttt,     //!< Instruction 'itttt' (THUMB).
    kIdLda,       //!< Instruction 'lda'.
    kIdLdab,      //!< Instruction 'ldab'.
    kIdLdaex,     //!< Instruction 'ldaex'.
    kIdLdaexb,    //!< Instruction 'ldaexb'.
    kIdLdaexd,    //!< Instruction 'ldaexd'.
    kIdLdaexh,    //!< Instruction 'ldaexh'.
    kIdLdah,      //!< Instruction 'ldah'.
    kIdLdmda,     //!< Instruction 'ldmda' (ARM).
    kIdLdmdb,     //!< Instruction 'ldmdb'.
    kIdLdmia,     //!< Instruction 'ldmia'.
    kIdLdmib,     //!< Instruction 'ldmib' (ARM).
    kIdLdr,       //!< Instruction 'ldr'.
    kIdLdrb,      //!< Instruction 'ldrb'.
    kIdLdrbt,     //!< Instruction 'ldrbt'.
    kIdLdrd,      //!< Instruction 'ldrd'.
    kIdLdrex,     //!< Instruction 'ldrex'.
    kIdLdrexb,    //!< Instruction 'ldrexb'.
    kIdLdrexd,    //!< Instruction 'ldrexd'.
    kIdLdrexh,    //!< Instruction 'ldrexh'.
    kIdLdrh,      //!< Instruction 'ldrh'.
    kIdLdrht,     //!< Instruction 'ldrht'.
    kIdLdrsb,     //!< Instruction 'ldrsb'.
    kIdLdrsbt,    //!< Instruction 'ldrsbt'.
    kIdLdrsh,     //!< Instruction 'ldrsh'.
    kIdLdrsht,    //!< Instruction 'ldrsht'.
    kIdLdrt,      //!< Instruction 'ldrt'.
    kIdLsl,       //!< Instruction 'lsl'.
    kIdLsls,      //!< Instruction 'lsls'.
    kIdLsr,       //!< Instruction 'lsr'.
    kIdLsrs,      //!< Instruction 'lsrs'.
    kIdMcr,       //!< Instruction 'mcr' {ARMv6T2+}.
    kIdMcr2,      //!< Instruction 'mcr2' {ARMv6T2+}.
    kIdMcrr,      //!< Instruction 'mcrr' {ARMv6T2+}.
    kIdMcrr2,     //!< Instruction 'mcrr2' {ARMv6T2+}.
    kIdMla,       //!< Instruction 'mla'.
    kIdMlas,      //!< Instruction 'mlas' (ARM).
    kIdMls,       //!< Instruction 'mls'.
    kIdMov,       //!< Instruction 'mov'.
    kIdMovs,      //!< Instruction 'movs'.
    kIdMovt,      //!< Instruction 'movt'.
    kIdMovw,      //!< Instruction 'movw'.
    kIdMrc,       //!< Instruction 'mrc'.
    kIdMrc2,      //!< Instruction 'mrc2'.
    kIdMrrc,      //!< Instruction 'mrrc'.
    kIdMrrc2,     //!< Instruction 'mrrc2'.
    kIdMrs,       //!< Instruction 'mrs'.
    kIdMsr,       //!< Instruction 'msr'.
    kIdMul,       //!< Instruction 'mul'.
    kIdMuls,      //!< Instruction 'muls'.
    kIdMvn,       //!< Instruction 'mvn'.
    kIdMvns,      //!< Instruction 'mvns'.
    kIdNop,       //!< Instruction 'nop'.
    kIdOrn,       //!< Instruction 'orn' (THUMB).
    kIdOrns,      //!< Instruction 'orns' (THUMB).
    kIdOrr,       //!< Instruction 'orr'.
    kIdOrrs,      //!< Instruction 'orrs'.
    kIdPkhbt,     //!< Instruction 'pkhbt'.
    kIdPkhtb,     //!< Instruction 'pkhtb'.
    kIdPld,       //!< Instruction 'pld'.
    kIdPldw,      //!< Instruction 'pldw' {MP}.
    kIdPli,       //!< Instruction 'pli'.
    kIdPop,       //!< Instruction 'pop'.
    kIdPssbb,     //!< Instruction 'pssbb'.
    kIdPush,      //!< Instruction 'push'.
    kIdQadd,      //!< Instruction 'qadd'.
    kIdQadd16,    //!< Instruction 'qadd16'.
    kIdQadd8,     //!< Instruction 'qadd8'.
    kIdQasx,      //!< Instruction 'qasx'.
    kIdQdadd,     //!< Instruction 'qdadd'.
    kIdQdsub,     //!< Instruction 'qdsub'.
    kIdQsax,      //!< Instruction 'qsax'.
    kIdQsub,      //!< Instruction 'qsub'.
    kIdQsub16,    //!< Instruction 'qsub16'.
    kIdQsub8,     //!< Instruction 'qsub8'.
    kIdRbit,      //!< Instruction 'rbit'.
    kIdRev,       //!< Instruction 'rev'.
    kIdRev16,     //!< Instruction 'rev16'.
    kIdRevsh,     //!< Instruction 'revsh'.
    kIdRfeda,     //!< Instruction 'rfeda' (ARM).
    kIdRfedb,     //!< Instruction 'rfedb' (ARM).
    kIdRfeia,     //!< Instruction 'rfeia' (ARM).
    kIdRfeib,     //!< Instruction 'rfeib' (ARM).
    kIdRor,       //!< Instruction 'ror'.
    kIdRors,      //!< Instruction 'rors'.
    kIdRrx,       //!< Instruction 'rrx'.
    kIdRrxs,      //!< Instruction 'rrxs'.
    kIdRsb,       //!< Instruction 'rsb'.
    kIdRsbs,      //!< Instruction 'rsbs'.
    kIdRsc,       //!< Instruction 'rsc' (ARM).
    kIdRscs,      //!< Instruction 'rscs' (ARM).
    kIdSadd16,    //!< Instruction 'sadd16'.
    kIdSadd8,     //!< Instruction 'sadd8'.
    kIdSasx,      //!< Instruction 'sasx'.
    kIdSb,        //!< Instruction 'sb' {SB}.
    kIdSbc,       //!< Instruction 'sbc'.
    kIdSbcs,      //!< Instruction 'sbcs'.
    kIdSbfx,      //!< Instruction 'sbfx'.
    kIdSdiv,      //!< Instruction 'sdiv' {IDIVA & IDIVT}.
    kIdSel,       //!< Instruction 'sel'.
    kIdSetend,    //!< Instruction 'setend' {ARMv8-}.
    kIdSetpan,    //!< Instruction 'setpan' {PAN}.
    kIdSev,       //!< Instruction 'sev'.
    kIdSevl,      //!< Instruction 'sevl'.
    kIdSha1c,     //!< Instruction 'sha1c' {SHA1}.
    kIdSha1h,     //!< Instruction 'sha1h' {SHA1}.
    kIdSha1m,     //!< Instruction 'sha1m' {SHA1}.
    kIdSha1p,     //!< Instruction 'sha1p' {SHA1}.
    kIdSha1su0,   //!< Instruction 'sha1su0' {SHA1}.
    kIdSha1su1,   //!< Instruction 'sha1su1' {SHA1}.
    kIdSha256h,   //!< Instruction 'sha256h' {SHA256}.
    kIdSha256h2,  //!< Instruction 'sha256h2' {SHA256}.
    kIdSha256su0, //!< Instruction 'sha256su0' {SHA256}.
    kIdSha256su1, //!< Instruction 'sha256su1' {SHA256}.
    kIdShadd16,   //!< Instruction 'shadd16'.
    kIdShadd8,    //!< Instruction 'shadd8'.
    kIdShasx,     //!< Instruction 'shasx'.
    kIdShsax,     //!< Instruction 'shsax'.
    kIdShsub16,   //!< Instruction 'shsub16'.
    kIdShsub8,    //!< Instruction 'shsub8'.
    kIdSmc,       //!< Instruction 'smc' {SECURITY}.
    kIdSmlabb,    //!< Instruction 'smlabb'.
    kIdSmlabt,    //!< Instruction 'smlabt'.
    kIdSmlad,     //!< Instruction 'smlad'.
    kIdSmladx,    //!< Instruction 'smladx'.
    kIdSmlal,     //!< Instruction 'smlal'.
    kIdSmlalbb,   //!< Instruction 'smlalbb'.
    kIdSmlalbt,   //!< Instruction 'smlalbt'.
    kIdSmlald,    //!< Instruction 'smlald'.
    kIdSmlaldx,   //!< Instruction 'smlaldx'.
    kIdSmlals,    //!< Instruction 'smlals' (ARM).
    kIdSmlaltb,   //!< Instruction 'smlaltb'.
    kIdSmlaltt,   //!< Instruction 'smlaltt'.
    kIdSmlatb,    //!< Instruction 'smlatb'.
    kIdSmlatt,    //!< Instruction 'smlatt'.
    kIdSmlawb,    //!< Instruction 'smlawb'.
    kIdSmlawt,    //!< Instruction 'smlawt'.
    kIdSmlsd,     //!< Instruction 'smlsd'.
    kIdSmlsdx,    //!< Instruction 'smlsdx'.
    kIdSmlsld,    //!< Instruction 'smlsld'.
    kIdSmlsldx,   //!< Instruction 'smlsldx'.
    kIdSmmla,     //!< Instruction 'smmla'.
    kIdSmmlar,    //!< Instruction 'smmlar'.
    kIdSmmls,     //!< Instruction 'smmls'.
    kIdSmmlsr,    //!< Instruction 'smmlsr'.
    kIdSmmul,     //!< Instruction 'smmul'.
    kIdSmmulr,    //!< Instruction 'smmulr'.
    kIdSmuad,     //!< Instruction 'smuad'.
    kIdSmuadx,    //!< Instruction 'smuadx'.
    kIdSmulbb,    //!< Instruction 'smulbb'.
    kIdSmulbt,    //!< Instruction 'smulbt'.
    kIdSmull,     //!< Instruction 'smull'.
    kIdSmulls,    //!< Instruction 'smulls' (ARM).
    kIdSmultb,    //!< Instruction 'smultb'.
    kIdSmultt,    //!< Instruction 'smultt'.
    kIdSmulwb,    //!< Instruction 'smulwb'.
    kIdSmulwt,    //!< Instruction 'smulwt'.
    kIdSmusd,     //!< Instruction 'smusd'.
    kIdSmusdx,    //!< Instruction 'smusdx'.
    kIdSrsda,     //!< Instruction 'srsda' (ARM).
    kIdSrsdb,     //!< Instruction 'srsdb' (ARM).
    kIdSrsia,     //!< Instruction 'srsia' (ARM).
    kIdSrsib,     //!< Instruction 'srsib' (ARM).
    kIdSsat,      //!< Instruction 'ssat'.
    kIdSsat16,    //!< Instruction 'ssat16'.
    kIdSsax,      //!< Instruction 'ssax'.
    kIdSsbb,      //!< Instruction 'ssbb'.
    kIdSsub16,    //!< Instruction 'ssub16'.
    kIdSsub8,     //!< Instruction 'ssub8'.
    kIdStl,       //!< Instruction 'stl'.
    kIdStlb,      //!< Instruction 'stlb'.
    kIdStlex,     //!< Instruction 'stlex'.
    kIdStlexb,    //!< Instruction 'stlexb'.
    kIdStlexd,    //!< Instruction 'stlexd'.
    kIdStlexh,    //!< Instruction 'stlexh'.
    kIdStlh,      //!< Instruction 'stlh'.
    kIdStmda,     //!< Instruction 'stmda' (ARM).
    kIdStmdb,     //!< Instruction 'stmdb'.
    kIdStmia,     //!< Instruction 'stmia'.
    kIdStmib,     //!< Instruction 'stmib' (ARM).
    kIdStr,       //!< Instruction 'str'.
    kIdStrb,      //!< Instruction 'strb'.
    kIdStrbt,     //!< Instruction 'strbt'.
    kIdStrd,      //!< Instruction 'strd'.
    kIdStrex,     //!< Instruction 'strex'.
    kIdStrexb,    //!< Instruction 'strexb'.
    kIdStrexd,    //!< Instruction 'strexd'.
    kIdStrexh,    //!< Instruction 'strexh'.
    kIdStrh,      //!< Instruction 'strh'.
    kIdStrht,     //!< Instruction 'strht'.
    kIdStrt,      //!< Instruction 'strt'.
    kIdSub,       //!< Instruction 'sub'.
    kIdSubs,      //!< Instruction 'subs'.
    kIdSvc,       //!< Instruction 'svc'.
    kIdSxtab,     //!< Instruction 'sxtab'.
    kIdSxtab16,   //!< Instruction 'sxtab16'.
    kIdSxtah,     //!< Instruction 'sxtah'.
    kIdSxtb,      //!< Instruction 'sxtb'.
    kIdSxtb16,    //!< Instruction 'sxtb16'.
    kIdSxth,      //!< Instruction 'sxth'.
    kIdTbb,       //!< Instruction 'tbb' (THUMB).
    kIdTbh,       //!< Instruction 'tbh' (THUMB).
    kIdTeq,       //!< Instruction 'teq'.
    kIdTst,       //!< Instruction 'tst'.
    kIdUadd16,    //!< Instruction 'uadd16'.
    kIdUadd8,     //!< Instruction 'uadd8'.
    kIdUasx,      //!< Instruction 'uasx'.
    kIdUbfx,      //!< Instruction 'ubfx'.
    kIdUdf,       //!< Instruction 'udf' (ARM).
    kIdUdiv,      //!< Instruction 'udiv' {IDIVA & IDIVT}.
    kIdUhadd16,   //!< Instruction 'uhadd16'.
    kIdUhadd8,    //!< Instruction 'uhadd8'.
    kIdUhasx,     //!< Instruction 'uhasx'.
    kIdUhsax,     //!< Instruction 'uhsax'.
    kIdUhsub16,   //!< Instruction 'uhsub16'.
    kIdUhsub8,    //!< Instruction 'uhsub8'.
    kIdUmaal,     //!< Instruction 'umaal'.
    kIdUmlal,     //!< Instruction 'umlal'.
    kIdUmlals,    //!< Instruction 'umlals' (ARM).
    kIdUmull,     //!< Instruction 'umull'.
    kIdUmulls,    //!< Instruction 'umulls' (ARM).
    kIdUqadd16,   //!< Instruction 'uqadd16'.
    kIdUqadd8,    //!< Instruction 'uqadd8'.
    kIdUqasx,     //!< Instruction 'uqasx'.
    kIdUqsax,     //!< Instruction 'uqsax'.
    kIdUqsub16,   //!< Instruction 'uqsub16'.
    kIdUqsub8,    //!< Instruction 'uqsub8'.
    kIdUsad8,     //!< Instruction 'usad8'.
    kIdUsada8,    //!< Instruction 'usada8'.
    kIdUsat,      //!< Instruction 'usat'.
    kIdUsat16,    //!< Instruction 'usat16'.
    kIdUsax,      //!< Instruction 'usax'.
    kIdUsub16,    //!< Instruction 'usub16'.
    kIdUsub8,     //!< Instruction 'usub8'.
    kIdUxtab,     //!< Instruction 'uxtab'.
    kIdUxtab16,   //!< Instruction 'uxtab16'.
    kIdUxtah,     //!< Instruction 'uxtah'.
    kIdUxtb,      //!< Instruction 'uxtb'.
    kIdUxtb16,    //!< Instruction 'uxtb16'.
    kIdUxth,      //!< Instruction 'uxth'.
    kIdVaba,      //!< Instruction 'vaba' {ASIMD}.
    kIdVabal,     //!< Instruction 'vabal' {ASIMD}.
    kIdVabd,      //!< Instruction 'vabd' {ASIMD ~FP16}.
    kIdVabdl,     //!< Instruction 'vabdl' {ASIMD}.
    kIdVabs,      //!< Instruction 'vabs' {ASIMD & FP ~FP16}.
    kIdVacge,     //!< Instruction 'vacge' {ASIMD ~FP16}.
    kIdVacgt,     //!< Instruction 'vacgt' {ASIMD ~FP16}.
    kIdVacle,     //!< Instruction 'vacle' {ASIMD ~FP16}.
    kIdVaclt,     //!< Instruction 'vaclt' {ASIMD ~FP16}.
    kIdVadd,      //!< Instruction 'vadd' {ASIMD & FP ~FP16}.
    kIdVaddhn,    //!< Instruction 'vaddhn' {ASIMD}.
    kIdVaddl,     //!< Instruction 'vaddl' {ASIMD}.
    kIdVaddw,     //!< Instruction 'vaddw' {ASIMD}.
    kIdVand,      //!< Instruction 'vand' {ASIMD}.
    kIdVbic,      //!< Instruction 'vbic' {ASIMD}.
    kIdVbif,      //!< Instruction 'vbif' {ASIMD}.
    kIdVbit,      //!< Instruction 'vbit' {ASIMD}.
    kIdVbsl,      //!< Instruction 'vbsl' {ASIMD}.
    kIdVcadd,     //!< Instruction 'vcadd' {FCMA ~FP16}.
    kIdVceq,      //!< Instruction 'vceq' {ASIMD ~FP16}.
    kIdVcge,      //!< Instruction 'vcge' {ASIMD ~FP16}.
    kIdVcgt,      //!< Instruction 'vcgt' {ASIMD ~FP16}.
    kIdVcle,      //!< Instruction 'vcle' {ASIMD ~FP16}.
    kIdVcls,      //!< Instruction 'vcls' {ASIMD}.
    kIdVclt,      //!< Instruction 'vclt' {ASIMD ~FP16}.
    kIdVclz,      //!< Instruction 'vclz' {ASIMD}.
    kIdVcmla,     //!< Instruction 'vcmla' {FCMA ~FP16}.
    kIdVcmp,      //!< Instruction 'vcmp' {FP ~FP16}.
    kIdVcmpe,     //!< Instruction 'vcmpe' {FP ~FP16}.
    kIdVcnt,      //!< Instruction 'vcnt' {ASIMD}.
    kIdVcvt,      //!< Instruction 'vcvt' {ASIMD & FP & FP16CONV}.
    kIdVcvta,     //!< Instruction 'vcvta' {ASIMD & FP16CONV}.
    kIdVcvtb,     //!< Instruction 'vcvtb' {BF16 & FP16CONV}.
    kIdVcvtm,     //!< Instruction 'vcvtm' {ASIMD & FP16CONV}.
    kIdVcvtn,     //!< Instruction 'vcvtn' {ASIMD & FP16CONV}.
    kIdVcvtp,     //!< Instruction 'vcvtp' {ASIMD & FP16CONV}.
    kIdVcvtr,     //!< Instruction 'vcvtr' {FP & FP16CONV}.
    kIdVcvtt,     //!< Instruction 'vcvtt' {BF16 & FP16CONV}.
    kIdVdiv,      //!< Instruction 'vdiv' {FP ~FP16}.
    kIdVdot,      //!< Instruction 'vdot' {BF16}.
    kIdVdup,      //!< Instruction 'vdup' {ASIMD}.
    kIdVeor,      //!< Instruction 'veor' {ASIMD}.
    kIdVext,      //!< Instruction 'vext' {ASIMD}.
    kIdVfma,      //!< Instruction 'vfma' {ASIMD & VFPv4 ~FP16}.
    kIdVfmab,     //!< Instruction 'vfmab' {BF16}.
    kIdVfmal,     //!< Instruction 'vfmal' {FHM}.
    kIdVfmat,     //!< Instruction 'vfmat' {BF16}.
    kIdVfms,      //!< Instruction 'vfms' {ASIMD & VFPv4 ~FP16}.
    kIdVfmsl,     //!< Instruction 'vfmsl' {FHM}.
    kIdVfnma,     //!< Instruction 'vfnma' {VFPv4 ~FP16}.
    kIdVfnms,     //!< Instruction 'vfnms' {VFPv4 ~FP16}.
    kIdVhadd,     //!< Instruction 'vhadd' {ASIMD}.
    kIdVhsub,     //!< Instruction 'vhsub' {ASIMD}.
    kIdVins,      //!< Instruction 'vins' {ASIMD ~FP16}.
    kIdVjcvt,     //!< Instruction 'vjcvt' {JSCVT}.
    kIdVld1,      //!< Instruction 'vld1' {ASIMD}.
    kIdVld1r,     //!< Instruction 'vld1r' {ASIMD}.
    kIdVld2,      //!< Instruction 'vld2' {ASIMD}.
    kIdVld2r,     //!< Instruction 'vld2r' {ASIMD}.
    kIdVld3,      //!< Instruction 'vld3' {ASIMD}.
    kIdVld3r,     //!< Instruction 'vld3r' {ASIMD}.
    kIdVld4,      //!< Instruction 'vld4' {ASIMD}.
    kIdVld4r,     //!< Instruction 'vld4r' {ASIMD}.
    kIdVldmdb,    //!< Instruction 'vldmdb' {ASIMD}.
    kIdVldmia,    //!< Instruction 'vldmia' {ASIMD}.
    kIdVldr,      //!< Instruction 'vldr' {FP ~FP16}.
    kIdVmax,      //!< Instruction 'vmax' {ASIMD ~FP16}.
    kIdVmaxnm,    //!< Instruction 'vmaxnm' {ASIMD ~FP16}.
    kIdVmin,      //!< Instruction 'vmin' {ASIMD ~FP16}.
    kIdVminnm,    //!< Instruction 'vminnm' {ASIMD ~FP16}.
    kIdVmla,      //!< Instruction 'vmla' {ASIMD & FP ~FP16}.
    kIdVmlal,     //!< Instruction 'vmlal' {ASIMD}.
    kIdVmls,      //!< Instruction 'vmls' {ASIMD & FP ~FP16}.
    kIdVmlsl,     //!< Instruction 'vmlsl' {ASIMD}.
    kIdVmmla,     //!< Instruction 'vmmla' {BF16}.
    kIdVmov,      //!< Instruction 'vmov' {ASIMD & FP ~FP16}.
    kIdVmovl,     //!< Instruction 'vmovl' {ASIMD}.
    kIdVmovn,     //!< Instruction 'vmovn' {ASIMD}.
    kIdVmovx,     //!< Instruction 'vmovx' {FP ~FP16}.
    kIdVmul,      //!< Instruction 'vmul' {ASIMD & FP ~FP16}.
    kIdVmull,     //!< Instruction 'vmull' {ASIMD}.
    kIdVmvn,      //!< Instruction 'vmvn' {ASIMD}.
    kIdVneg,      //!< Instruction 'vneg' {ASIMD & FP ~FP16}.
    kIdVnmla,     //!< Instruction 'vnmla' {FP ~FP16}.
    kIdVnmls,     //!< Instruction 'vnmls' {FP ~FP16}.
    kIdVnmul,     //!< Instruction 'vnmul' {FP ~FP16}.
    kIdVorn,      //!< Instruction 'vorn' {ASIMD}.
    kIdVorr,      //!< Instruction 'vorr' {ASIMD}.
    kIdVpadal,    //!< Instruction 'vpadal' {ASIMD}.
    kIdVpadd,     //!< Instruction 'vpadd' {ASIMD ~FP16}.
    kIdVpaddl,    //!< Instruction 'vpaddl' {ASIMD}.
    kIdVpmax,     //!< Instruction 'vpmax' {ASIMD ~FP16}.
    kIdVpmin,     //!< Instruction 'vpmin' {ASIMD ~FP16}.
    kIdVpop,      //!< Instruction 'vpop' {ASIMD}.
    kIdVpush,     //!< Instruction 'vpush' {ASIMD}.
    kIdVqabs,     //!< Instruction 'vqabs' {ASIMD}.
    kIdVqadd,     //!< Instruction 'vqadd' {ASIMD}.
    kIdVqdmlal,   //!< Instruction 'vqdmlal' {ASIMD}.
    kIdVqdmlsl,   //!< Instruction 'vqdmlsl' {ASIMD}.
    kIdVqdmulh,   //!< Instruction 'vqdmulh' {ASIMD}.
    kIdVqdmull,   //!< Instruction 'vqdmull' {ASIMD}.
    kIdVqmovn,    //!< Instruction 'vqmovn' {ASIMD}.
    kIdVqmovun,   //!< Instruction 'vqmovun' {ASIMD}.
    kIdVqneg,     //!< Instruction 'vqneg' {ASIMD}.
    kIdVqrdmlah,  //!< Instruction 'vqrdmlah' {RDM}.
    kIdVqrdmlsh,  //!< Instruction 'vqrdmlsh' {RDM}.
    kIdVqrdmulh,  //!< Instruction 'vqrdmulh' {ASIMD}.
    kIdVqrshl,    //!< Instruction 'vqrshl' {ASIMD}.
    kIdVqrshrn,   //!< Instruction 'vqrshrn' {ASIMD}.
    kIdVqrshrun,  //!< Instruction 'vqrshrun' {ASIMD}.
    kIdVqshl,     //!< Instruction 'vqshl' {ASIMD}.
    kIdVqshlu,    //!< Instruction 'vqshlu' {ASIMD}.
    kIdVqshrn,    //!< Instruction 'vqshrn' {ASIMD}.
    kIdVqshrun,   //!< Instruction 'vqshrun' {ASIMD}.
    kIdVqsub,     //!< Instruction 'vqsub' {ASIMD}.
    kIdVraddhn,   //!< Instruction 'vraddhn' {ASIMD}.
    kIdVrecpe,    //!< Instruction 'vrecpe' {ASIMD ~FP16}.
    kIdVrecps,    //!< Instruction 'vrecps' {ASIMD ~FP16}.
    kIdVrev16,    //!< Instruction 'vrev16' {ASIMD}.
    kIdVrev32,    //!< Instruction 'vrev32' {ASIMD}.
    kIdVrev64,    //!< Instruction 'vrev64' {ASIMD}.
    kIdVrhadd,    //!< Instruction 'vrhadd' {ASIMD}.
    kIdVrinta,    //!< Instruction 'vrinta' {ASIMD ~FP16}.
    kIdVrintm,    //!< Instruction 'vrintm' {ASIMD ~FP16}.
    kIdVrintn,    //!< Instruction 'vrintn' {ASIMD ~FP16}.
    kIdVrintp,    //!< Instruction 'vrintp' {ASIMD ~FP16}.
    kIdVrintr,    //!< Instruction 'vrintr' {ASIMD ~FP16}.
    kIdVrintx,    //!< Instruction 'vrintx' {ASIMD ~FP16}.
    kIdVrintz,    //!< Instruction 'vrintz' {ASIMD ~FP16}.
    kIdVrshl,     //!< Instruction 'vrshl' {ASIMD}.
    kIdVrshr,     //!< Instruction 'vrshr' {ASIMD}.
    kIdVrshrn,    //!< Instruction 'vrshrn' {ASIMD}.
    kIdVrsqrte,   //!< Instruction 'vrsqrte' {ASIMD ~FP16}.
    kIdVrsqrts,   //!< Instruction 'vrsqrts' {ASIMD ~FP16}.
    kIdVrsra,     //!< Instruction 'vrsra' {ASIMD}.
    kIdVrsubhn,   //!< Instruction 'vrsubhn' {ASIMD}.
    kIdVsdot,     //!< Instruction 'vsdot' {DOTPROD}.
    kIdVseleq,    //!< Instruction 'vseleq' {ASIMD ~FP16}.
    kIdVselge,    //!< Instruction 'vselge' {ASIMD ~FP16}.
    kIdVselgt,    //!< Instruction 'vselgt' {ASIMD ~FP16}.
    kIdVselvs,    //!< Instruction 'vselvs' {ASIMD ~FP16}.
    kIdVshl,      //!< Instruction 'vshl' {ASIMD}.
    kIdVshll,     //!< Instruction 'vshll' {ASIMD}.
    kIdVshr,      //!< Instruction 'vshr' {ASIMD}.
    kIdVshrn,     //!< Instruction 'vshrn' {ASIMD}.
    kIdVsli,      //!< Instruction 'vsli' {ASIMD}.
    kIdVsmmla,    //!< Instruction 'vsmmla' {I8MM}.
    kIdVsqrt,     //!< Instruction 'vsqrt' {FP ~FP16}.
    kIdVsra,      //!< Instruction 'vsra' {ASIMD}.
    kIdVsri,      //!< Instruction 'vsri' {ASIMD}.
    kIdVst1,      //!< Instruction 'vst1' {ASIMD}.
    kIdVst2,      //!< Instruction 'vst2' {ASIMD}.
    kIdVst3,      //!< Instruction 'vst3' {ASIMD}.
    kIdVst4,      //!< Instruction 'vst4' {ASIMD}.
    kIdVstmdb,    //!< Instruction 'vstmdb' {ASIMD}.
    kIdVstmia,    //!< Instruction 'vstmia' {ASIMD}.
    kIdVstr,      //!< Instruction 'vstr' {FP ~FP16}.
    kIdVsub,      //!< Instruction 'vsub' {ASIMD & FP ~FP16}.
    kIdVsubhn,    //!< Instruction 'vsubhn' {ASIMD}.
    kIdVsubl,     //!< Instruction 'vsubl' {ASIMD}.
    kIdVsubw,     //!< Instruction 'vsubw' {ASIMD}.
    kIdVsudot,    //!< Instruction 'vsudot' {I8MM}.
    kIdVswp,      //!< Instruction 'vswp' {ASIMD}.
    kIdVtbl,      //!< Instruction 'vtbl' {ASIMD}.
    kIdVtbx,      //!< Instruction 'vtbx' {ASIMD}.
    kIdVtrn,      //!< Instruction 'vtrn' {ASIMD}.
    kIdVtst,      //!< Instruction 'vtst' {ASIMD}.
    kIdVudot,     //!< Instruction 'vudot' {DOTPROD}.
    kIdVummla,    //!< Instruction 'vummla' {I8MM}.
    kIdVusdot,    //!< Instruction 'vusdot' {I8MM}.
    kIdVusmmla,   //!< Instruction 'vusmmla' {I8MM}.
    kIdVuzp,      //!< Instruction 'vuzp' {ASIMD}.
    kIdVzip,      //!< Instruction 'vzip' {ASIMD}.
    kIdWfe,       //!< Instruction 'wfe'.
    kIdWfi,       //!< Instruction 'wfi'.
    kIdYield,     //!< Instruction 'yield'.
    _kIdCount
    // ----------------------------------------------------------------------------
    // ${a32::InstId:End}
  };

  //! Tests whether the `instId` is defined (counts also Inst::kIdNone, which must be zero).
  //!
  //! \note This function required identifier to be without modifiers. If the given instruction id contains modifiers
  //! it would return false as modifiers overflow `_kIdCount`.
  static constexpr inline bool isDefinedId(InstId instId) noexcept {
    return instId < _kIdCount;
  }
};

//! \}

ASMJIT_END_SUB_NAMESPACE

#endif // ASMJIT_ARM_A32GLOBALS_H_INCLUDED
