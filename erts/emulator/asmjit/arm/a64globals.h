// AsmJit - Machine code generation for C++
//
//  * Official AsmJit Home Page: https://asmjit.com
//  * Official Github Repository: https://github.com/asmjit/asmjit
//
// Copyright (c) 2008-2020 The AsmJit Authors
//
// This software is provided 'as-is', without any express or implied
// warranty. In no event will the authors be held liable for any damages
// arising from the use of this software.
//
// Permission is granted to anyone to use this software for any purpose,
// including commercial applications, and to alter it and redistribute it
// freely, subject to the following restrictions:
//
// 1. The origin of this software must not be misrepresented; you must not
//    claim that you wrote the original software. If you use this software
//    in a product, an acknowledgment in the product documentation would be
//    appreciated but is not required.
// 2. Altered source versions must be plainly marked as such, and must not be
//    misrepresented as being the original software.
// 3. This notice may not be removed or altered from any source distribution.

#ifndef ASMJIT_ARM_A64GLOBALS_H_INCLUDED
#define ASMJIT_ARM_A64GLOBALS_H_INCLUDED

#include "../arm/armglobals.h"

ASMJIT_BEGIN_SUB_NAMESPACE(a64)

// a64 uses everything from arm namespace and adds into it.
using namespace arm;

//! \addtogroup asmjit_a64
//! \{

// ============================================================================
// [asmjit::a64::Inst]
// ============================================================================

//! AArch64 instruction.
//!
//! \note Only used to hold ARM-specific enumerations and static functions.
struct Inst : public BaseInst {
  //! Instruction id.
  enum Id : uint32_t {
    // ${InstId:Begin}
    kIdNone = 0,
    kIdAdc,
    kIdAdcs,
    kIdAdd,
    kIdAddg,
    kIdAdds,
    kIdAdr,
    kIdAdrp,
    kIdAnd,
    kIdAnds,
    kIdAsr,
    kIdAsrv,
    kIdAt,
    kIdAutda,
    kIdAutdza,
    kIdAutdb,
    kIdAutdzb,
    kIdAutia,
    kIdAutia1716,
    kIdAutiasp,
    kIdAutiaz,
    kIdAutib,
    kIdAutib1716,
    kIdAutibsp,
    kIdAutibz,
    kIdAutiza,
    kIdAutizb,
    kIdAxflag,
    kIdB,
    kIdBfc,
    kIdBfi,
    kIdBfm,
    kIdBfxil,
    kIdBic,
    kIdBics,
    kIdBl,
    kIdBlr,
    kIdBr,
    kIdBrk,
    kIdCas,
    kIdCasa,
    kIdCasab,
    kIdCasah,
    kIdCasal,
    kIdCasalb,
    kIdCasalh,
    kIdCasb,
    kIdCash,
    kIdCasl,
    kIdCaslb,
    kIdCaslh,
    kIdCasp,
    kIdCaspa,
    kIdCaspal,
    kIdCaspl,
    kIdCbnz,
    kIdCbz,
    kIdCcmn,
    kIdCcmp,
    kIdCfinv,
    kIdCinc,
    kIdCinv,
    kIdClrex,
    kIdCls,
    kIdClz,
    kIdCmn,
    kIdCmp,
    kIdCmpp,
    kIdCneg,
    kIdCrc32b,
    kIdCrc32cb,
    kIdCrc32ch,
    kIdCrc32cw,
    kIdCrc32cx,
    kIdCrc32h,
    kIdCrc32w,
    kIdCrc32x,
    kIdCsdb,
    kIdCsel,
    kIdCset,
    kIdCsetm,
    kIdCsinc,
    kIdCsinv,
    kIdCsneg,
    kIdDc,
    kIdDcps1,
    kIdDcps2,
    kIdDcps3,
    kIdDgh,
    kIdDmb,
    kIdDrps,
    kIdDsb,
    kIdEon,
    kIdEor,
    kIdEsb,
    kIdExtr,
    kIdEret,
    kIdGmi,
    kIdHint,
    kIdHlt,
    kIdHvc,
    kIdIc,
    kIdIsb,
    kIdLdadd,
    kIdLdadda,
    kIdLdaddab,
    kIdLdaddah,
    kIdLdaddal,
    kIdLdaddalb,
    kIdLdaddalh,
    kIdLdaddb,
    kIdLdaddh,
    kIdLdaddl,
    kIdLdaddlb,
    kIdLdaddlh,
    kIdLdar,
    kIdLdarb,
    kIdLdarh,
    kIdLdaxp,
    kIdLdaxr,
    kIdLdaxrb,
    kIdLdaxrh,
    kIdLdclr,
    kIdLdclra,
    kIdLdclrab,
    kIdLdclrah,
    kIdLdclral,
    kIdLdclralb,
    kIdLdclralh,
    kIdLdclrb,
    kIdLdclrh,
    kIdLdclrl,
    kIdLdclrlb,
    kIdLdclrlh,
    kIdLdeor,
    kIdLdeora,
    kIdLdeorab,
    kIdLdeorah,
    kIdLdeoral,
    kIdLdeoralb,
    kIdLdeoralh,
    kIdLdeorb,
    kIdLdeorh,
    kIdLdeorl,
    kIdLdeorlb,
    kIdLdeorlh,
    kIdLdg,
    kIdLdgm,
    kIdLdlar,
    kIdLdlarb,
    kIdLdlarh,
    kIdLdnp,
    kIdLdp,
    kIdLdpsw,
    kIdLdr,
    kIdLdraa,
    kIdLdrab,
    kIdLdrb,
    kIdLdrh,
    kIdLdrsb,
    kIdLdrsh,
    kIdLdrsw,
    kIdLdset,
    kIdLdseta,
    kIdLdsetab,
    kIdLdsetah,
    kIdLdsetal,
    kIdLdsetalb,
    kIdLdsetalh,
    kIdLdsetb,
    kIdLdseth,
    kIdLdsetl,
    kIdLdsetlb,
    kIdLdsetlh,
    kIdLdsmax,
    kIdLdsmaxa,
    kIdLdsmaxab,
    kIdLdsmaxah,
    kIdLdsmaxal,
    kIdLdsmaxalb,
    kIdLdsmaxalh,
    kIdLdsmaxb,
    kIdLdsmaxh,
    kIdLdsmaxl,
    kIdLdsmaxlb,
    kIdLdsmaxlh,
    kIdLdsmin,
    kIdLdsmina,
    kIdLdsminab,
    kIdLdsminah,
    kIdLdsminal,
    kIdLdsminalb,
    kIdLdsminalh,
    kIdLdsminb,
    kIdLdsminh,
    kIdLdsminl,
    kIdLdsminlb,
    kIdLdsminlh,
    kIdLdtr,
    kIdLdtrb,
    kIdLdtrh,
    kIdLdtrsb,
    kIdLdtrsh,
    kIdLdtrsw,
    kIdLdumax,
    kIdLdumaxa,
    kIdLdumaxab,
    kIdLdumaxah,
    kIdLdumaxal,
    kIdLdumaxalb,
    kIdLdumaxalh,
    kIdLdumaxb,
    kIdLdumaxh,
    kIdLdumaxl,
    kIdLdumaxlb,
    kIdLdumaxlh,
    kIdLdumin,
    kIdLdumina,
    kIdLduminab,
    kIdLduminah,
    kIdLduminal,
    kIdLduminalb,
    kIdLduminalh,
    kIdLduminb,
    kIdLduminh,
    kIdLduminl,
    kIdLduminlb,
    kIdLduminlh,
    kIdLdur,
    kIdLdurb,
    kIdLdurh,
    kIdLdursb,
    kIdLdursh,
    kIdLdursw,
    kIdLdxp,
    kIdLdxr,
    kIdLdxrb,
    kIdLdxrh,
    kIdLsl,
    kIdLslv,
    kIdLsr,
    kIdLsrv,
    kIdMadd,
    kIdMneg,
    kIdMov,
    kIdMovk,
    kIdMovn,
    kIdMovz,
    kIdMrs,
    kIdMsr,
    kIdMsub,
    kIdMul,
    kIdMvn,
    kIdNeg,
    kIdNegs,
    kIdNgc,
    kIdNgcs,
    kIdNop,
    kIdOrn,
    kIdOrr,
    kIdPacda,
    kIdPacdb,
    kIdPacdza,
    kIdPacdzb,
    kIdPacga,
    kIdPssbb,
    kIdRbit,
    kIdRet,
    kIdRev,
    kIdRev16,
    kIdRev32,
    kIdRev64,
    kIdRor,
    kIdRorv,
    kIdSbc,
    kIdSbcs,
    kIdSbfiz,
    kIdSbfm,
    kIdSbfx,
    kIdSdiv,
    kIdSetf8,
    kIdSetf16,
    kIdSev,
    kIdSevl,
    kIdSmaddl,
    kIdSmc,
    kIdSmnegl,
    kIdSmsubl,
    kIdSmulh,
    kIdSmull,
    kIdSsbb,
    kIdSt2g,
    kIdStadd,
    kIdStaddl,
    kIdStaddb,
    kIdStaddlb,
    kIdStaddh,
    kIdStaddlh,
    kIdStclr,
    kIdStclrl,
    kIdStclrb,
    kIdStclrlb,
    kIdStclrh,
    kIdStclrlh,
    kIdSteor,
    kIdSteorl,
    kIdSteorb,
    kIdSteorlb,
    kIdSteorh,
    kIdSteorlh,
    kIdStg,
    kIdStgm,
    kIdStgp,
    kIdStllr,
    kIdStllrb,
    kIdStllrh,
    kIdStlr,
    kIdStlrb,
    kIdStlrh,
    kIdStlxp,
    kIdStlxr,
    kIdStlxrb,
    kIdStlxrh,
    kIdStnp,
    kIdStp,
    kIdStr,
    kIdStrb,
    kIdStrh,
    kIdStset,
    kIdStsetl,
    kIdStsetb,
    kIdStsetlb,
    kIdStseth,
    kIdStsetlh,
    kIdStsmax,
    kIdStsmaxl,
    kIdStsmaxb,
    kIdStsmaxlb,
    kIdStsmaxh,
    kIdStsmaxlh,
    kIdStsmin,
    kIdStsminl,
    kIdStsminb,
    kIdStsminlb,
    kIdStsminh,
    kIdStsminlh,
    kIdSttr,
    kIdSttrb,
    kIdSttrh,
    kIdStumax,
    kIdStumaxl,
    kIdStumaxb,
    kIdStumaxlb,
    kIdStumaxh,
    kIdStumaxlh,
    kIdStumin,
    kIdStuminl,
    kIdStuminb,
    kIdStuminlb,
    kIdStuminh,
    kIdStuminlh,
    kIdStur,
    kIdSturb,
    kIdSturh,
    kIdStxp,
    kIdStxr,
    kIdStxrb,
    kIdStxrh,
    kIdStz2g,
    kIdStzg,
    kIdStzgm,
    kIdSub,
    kIdSubg,
    kIdSubp,
    kIdSubps,
    kIdSubs,
    kIdSvc,
    kIdSwp,
    kIdSwpa,
    kIdSwpab,
    kIdSwpah,
    kIdSwpal,
    kIdSwpalb,
    kIdSwpalh,
    kIdSwpb,
    kIdSwph,
    kIdSwpl,
    kIdSwplb,
    kIdSwplh,
    kIdSxtb,
    kIdSxth,
    kIdSxtw,
    kIdSys,
    kIdTlbi,
    kIdTst,
    kIdTbnz,
    kIdTbz,
    kIdUbfiz,
    kIdUbfm,
    kIdUbfx,
    kIdUdf,
    kIdUdiv,
    kIdUmaddl,
    kIdUmnegl,
    kIdUmull,
    kIdUmulh,
    kIdUmsubl,
    kIdUxtb,
    kIdUxth,
    kIdWfe,
    kIdWfi,
    kIdXaflag,
    kIdXpacd,
    kIdXpaci,
    kIdXpaclri,
    kIdYield,
    kIdAbs_v,
    kIdAdd_v,
    kIdAddhn_v,
    kIdAddhn2_v,
    kIdAddp_v,
    kIdAddv_v,
    kIdAesd_v,
    kIdAese_v,
    kIdAesimc_v,
    kIdAesmc_v,
    kIdAnd_v,
    kIdBcax_v,
    kIdBfcvt_v,
    kIdBfcvtn_v,
    kIdBfcvtn2_v,
    kIdBfdot_v,
    kIdBfmlalb_v,
    kIdBfmlalt_v,
    kIdBfmmla_v,
    kIdBic_v,
    kIdBif_v,
    kIdBit_v,
    kIdBsl_v,
    kIdCls_v,
    kIdClz_v,
    kIdCmeq_v,
    kIdCmge_v,
    kIdCmgt_v,
    kIdCmhi_v,
    kIdCmhs_v,
    kIdCmle_v,
    kIdCmlt_v,
    kIdCmtst_v,
    kIdCnt_v,
    kIdDup_v,
    kIdEor_v,
    kIdEor3_v,
    kIdExt_v,
    kIdFabd_v,
    kIdFabs_v,
    kIdFacge_v,
    kIdFacgt_v,
    kIdFadd_v,
    kIdFaddp_v,
    kIdFcadd_v,
    kIdFccmp_v,
    kIdFccmpe_v,
    kIdFcmeq_v,
    kIdFcmge_v,
    kIdFcmgt_v,
    kIdFcmla_v,
    kIdFcmle_v,
    kIdFcmlt_v,
    kIdFcmp_v,
    kIdFcmpe_v,
    kIdFcsel_v,
    kIdFcvt_v,
    kIdFcvtas_v,
    kIdFcvtau_v,
    kIdFcvtl_v,
    kIdFcvtl2_v,
    kIdFcvtms_v,
    kIdFcvtmu_v,
    kIdFcvtn_v,
    kIdFcvtn2_v,
    kIdFcvtns_v,
    kIdFcvtnu_v,
    kIdFcvtps_v,
    kIdFcvtpu_v,
    kIdFcvtxn_v,
    kIdFcvtxn2_v,
    kIdFcvtzs_v,
    kIdFcvtzu_v,
    kIdFdiv_v,
    kIdFjcvtzs_v,
    kIdFmadd_v,
    kIdFmax_v,
    kIdFmaxnm_v,
    kIdFmaxnmp_v,
    kIdFmaxnmv_v,
    kIdFmaxp_v,
    kIdFmaxv_v,
    kIdFmin_v,
    kIdFminnm_v,
    kIdFminnmp_v,
    kIdFminnmv_v,
    kIdFminp_v,
    kIdFminv_v,
    kIdFmla_v,
    kIdFmlal_v,
    kIdFmlal2_v,
    kIdFmls_v,
    kIdFmlsl_v,
    kIdFmlsl2_v,
    kIdFmov_v,
    kIdFmsub_v,
    kIdFmul_v,
    kIdFmulx_v,
    kIdFneg_v,
    kIdFnmadd_v,
    kIdFnmsub_v,
    kIdFnmul_v,
    kIdFrecpe_v,
    kIdFrecps_v,
    kIdFrecpx_v,
    kIdFrint32x_v,
    kIdFrint32z_v,
    kIdFrint64x_v,
    kIdFrint64z_v,
    kIdFrinta_v,
    kIdFrinti_v,
    kIdFrintm_v,
    kIdFrintn_v,
    kIdFrintp_v,
    kIdFrintx_v,
    kIdFrintz_v,
    kIdFrsqrte_v,
    kIdFrsqrts_v,
    kIdFsqrt_v,
    kIdFsub_v,
    kIdIns_v,
    kIdLd1_v,
    kIdLd1r_v,
    kIdLd2_v,
    kIdLd2r_v,
    kIdLd3_v,
    kIdLd3r_v,
    kIdLd4_v,
    kIdLd4r_v,
    kIdLdnp_v,
    kIdLdp_v,
    kIdLdr_v,
    kIdLdur_v,
    kIdMla_v,
    kIdMls_v,
    kIdMov_v,
    kIdMovi_v,
    kIdMul_v,
    kIdMvn_v,
    kIdMvni_v,
    kIdNeg_v,
    kIdNot_v,
    kIdOrn_v,
    kIdOrr_v,
    kIdPmul_v,
    kIdPmull_v,
    kIdPmull2_v,
    kIdRaddhn_v,
    kIdRaddhn2_v,
    kIdRax1_v,
    kIdRbit_v,
    kIdRev16_v,
    kIdRev32_v,
    kIdRev64_v,
    kIdRshrn_v,
    kIdRshrn2_v,
    kIdRsubhn_v,
    kIdRsubhn2_v,
    kIdSaba_v,
    kIdSabal_v,
    kIdSabal2_v,
    kIdSabd_v,
    kIdSabdl_v,
    kIdSabdl2_v,
    kIdSadalp_v,
    kIdSaddl_v,
    kIdSaddl2_v,
    kIdSaddlp_v,
    kIdSaddlv_v,
    kIdSaddw_v,
    kIdSaddw2_v,
    kIdScvtf_v,
    kIdSdot_v,
    kIdSha1c_v,
    kIdSha1h_v,
    kIdSha1m_v,
    kIdSha1p_v,
    kIdSha1su0_v,
    kIdSha1su1_v,
    kIdSha256h_v,
    kIdSha256h2_v,
    kIdSha256su0_v,
    kIdSha256su1_v,
    kIdSha512h_v,
    kIdSha512h2_v,
    kIdSha512su0_v,
    kIdSha512su1_v,
    kIdShadd_v,
    kIdShl_v,
    kIdShll_v,
    kIdShll2_v,
    kIdShrn_v,
    kIdShrn2_v,
    kIdShsub_v,
    kIdSli_v,
    kIdSm3partw1_v,
    kIdSm3partw2_v,
    kIdSm3ss1_v,
    kIdSm3tt1a_v,
    kIdSm3tt1b_v,
    kIdSm3tt2a_v,
    kIdSm3tt2b_v,
    kIdSm4e_v,
    kIdSm4ekey_v,
    kIdSmax_v,
    kIdSmaxp_v,
    kIdSmaxv_v,
    kIdSmin_v,
    kIdSminp_v,
    kIdSminv_v,
    kIdSmlal_v,
    kIdSmlal2_v,
    kIdSmlsl_v,
    kIdSmlsl2_v,
    kIdSmmla_v,
    kIdSmov_v,
    kIdSmull_v,
    kIdSmull2_v,
    kIdSqabs_v,
    kIdSqadd_v,
    kIdSqdmlal_v,
    kIdSqdmlal2_v,
    kIdSqdmlsl_v,
    kIdSqdmlsl2_v,
    kIdSqdmulh_v,
    kIdSqdmull_v,
    kIdSqdmull2_v,
    kIdSqneg_v,
    kIdSqrdmlah_v,
    kIdSqrdmlsh_v,
    kIdSqrdmulh_v,
    kIdSqrshl_v,
    kIdSqrshrn_v,
    kIdSqrshrn2_v,
    kIdSqrshrun_v,
    kIdSqrshrun2_v,
    kIdSqshl_v,
    kIdSqshlu_v,
    kIdSqshrn_v,
    kIdSqshrn2_v,
    kIdSqshrun_v,
    kIdSqshrun2_v,
    kIdSqsub_v,
    kIdSqxtn_v,
    kIdSqxtn2_v,
    kIdSqxtun_v,
    kIdSqxtun2_v,
    kIdSrhadd_v,
    kIdSri_v,
    kIdSrshl_v,
    kIdSrshr_v,
    kIdSrsra_v,
    kIdSshl_v,
    kIdSshll_v,
    kIdSshll2_v,
    kIdSshr_v,
    kIdSsra_v,
    kIdSsubl_v,
    kIdSsubl2_v,
    kIdSsubw_v,
    kIdSsubw2_v,
    kIdSt1_v,
    kIdSt2_v,
    kIdSt3_v,
    kIdSt4_v,
    kIdStnp_v,
    kIdStp_v,
    kIdStr_v,
    kIdStur_v,
    kIdSub_v,
    kIdSubhn_v,
    kIdSubhn2_v,
    kIdSudot_v,
    kIdSuqadd_v,
    kIdSxtl_v,
    kIdSxtl2_v,
    kIdTbl_v,
    kIdTbx_v,
    kIdTrn1_v,
    kIdTrn2_v,
    kIdUaba_v,
    kIdUabal_v,
    kIdUabal2_v,
    kIdUabd_v,
    kIdUabdl_v,
    kIdUabdl2_v,
    kIdUadalp_v,
    kIdUaddl_v,
    kIdUaddl2_v,
    kIdUaddlp_v,
    kIdUaddlv_v,
    kIdUaddw_v,
    kIdUaddw2_v,
    kIdUcvtf_v,
    kIdUdot_v,
    kIdUhadd_v,
    kIdUhsub_v,
    kIdUmax_v,
    kIdUmaxp_v,
    kIdUmaxv_v,
    kIdUmin_v,
    kIdUminp_v,
    kIdUminv_v,
    kIdUmlal_v,
    kIdUmlal2_v,
    kIdUmlsl_v,
    kIdUmlsl2_v,
    kIdUmmla_v,
    kIdUmov_v,
    kIdUmull_v,
    kIdUmull2_v,
    kIdUqadd_v,
    kIdUqrshl_v,
    kIdUqrshrn_v,
    kIdUqrshrn2_v,
    kIdUqshl_v,
    kIdUqshrn_v,
    kIdUqshrn2_v,
    kIdUqsub_v,
    kIdUqxtn_v,
    kIdUqxtn2_v,
    kIdUrecpe_v,
    kIdUrhadd_v,
    kIdUrshl_v,
    kIdUrshr_v,
    kIdUrsqrte_v,
    kIdUrsra_v,
    kIdUsdot_v,
    kIdUshl_v,
    kIdUshll_v,
    kIdUshll2_v,
    kIdUshr_v,
    kIdUsmmla_v,
    kIdUsqadd_v,
    kIdUsra_v,
    kIdUsubl_v,
    kIdUsubl2_v,
    kIdUsubw_v,
    kIdUsubw2_v,
    kIdUxtl_v,
    kIdUxtl2_v,
    kIdUzp1_v,
    kIdUzp2_v,
    kIdXar_v,
    kIdXtn_v,
    kIdXtn2_v,
    kIdZip1_v,
    kIdZip2_v,
    _kIdCount
    // ${InstId:End}
  };

  //! Instruction options
  enum Options : uint32_t {
    //! Condition code flag shift
    kOptionCondFlagShift = 27,
    //! Condition code flag shift
    kOptionCondFlagMask = 1u << kOptionCondFlagShift,

    //! Condition code value shift.
    kOptionCondCodeShift = 28u,
    //! Condition code value mask.
    kOptionCondCodeMask = 0xFu << kOptionCondCodeShift
  };

  // --------------------------------------------------------------------------
  // [Statics]
  // --------------------------------------------------------------------------

  //! Tests whether the `instId` is defined (counts also Inst::kIdNone, which must be zero).
  static inline bool isDefinedId(uint32_t instId) noexcept { return instId < _kIdCount; }
};

// ============================================================================
// [asmjit::a64::Predicate]
// ============================================================================

namespace Predicate {

//! Address translate options (AT).
namespace AT {
  static inline constexpr uint32_t encode(uint32_t op1, uint32_t cRn, uint32_t cRm, uint32_t op2) noexcept {
    return (op1 << 11) | (cRn << 7) | (cRm << 3) | (op2 << 0);
  }

  enum Value : uint32_t {
    kS1E1R  = encode(0b000, 0b0111, 0b1000, 0b000),
    kS1E2R  = encode(0b100, 0b0111, 0b1000, 0b000),
    kS1E3R  = encode(0b110, 0b0111, 0b1000, 0b000),
    kS1E1W  = encode(0b000, 0b0111, 0b1000, 0b001),
    kS1E2W  = encode(0b100, 0b0111, 0b1000, 0b001),
    kS1E3W  = encode(0b110, 0b0111, 0b1000, 0b001),
    kS1E0R  = encode(0b000, 0b0111, 0b1000, 0b010),
    kS1E0W  = encode(0b000, 0b0111, 0b1000, 0b011),
    kS12E1R = encode(0b100, 0b0111, 0b1000, 0b100),
    kS12E1W = encode(0b100, 0b0111, 0b1000, 0b101),
    kS12E0R = encode(0b100, 0b0111, 0b1000, 0b110),
    kS12E0W = encode(0b100, 0b0111, 0b1000, 0b111),
    kS1E1RP = encode(0b000, 0b0111, 0b1001, 0b000),
    kS1E1WP = encode(0b000, 0b0111, 0b1001, 0b001)
  };
}

//! Data barrier options (DMB/DSB).
namespace DB {
  //! Data barrier immediate values.
  enum Value : uint32_t {
    //! Waits only for loads to complete, and only applies to the outer shareable domain.
    kOSHLD = 0x01u,
    //! Waits only for stores to complete, and only applies to the outer shareable domain.
    kOSHST = 0x02u,
    //! Only applies to the outer shareable domain.
    kOSH = 0x03u,

    //! Waits only for loads to complete and only applies out to the point of unification.
    kNSHLD = 0x05u,
    //! Waits only for stores to complete and only applies out to the point of unification.
    kNSHST = 0x06u,
    //! Only applies out to the point of unification.
    kNSH = 0x07u,

    //! Waits only for loads to complete, and only applies to the inner shareable domain.
    kISHLD = 0x09u,
    //! Waits only for stores to complete, and only applies to the inner shareable domain.
    kISHST = 0x0Au,
    //! Only applies to the inner shareable domain.
    kISH = 0x0Bu,

    //! Waits only for loads to complete.
    kLD = 0x0Du,
    //! Waits only for stores to complete.
    kST = 0x0Eu,
    //! Full system memory barrier operation.
    kSY = 0x0Fu
  };
}

//! Data cache maintenance options.
namespace DC {
  static inline constexpr uint32_t encode(uint32_t op1, uint32_t cRn, uint32_t cRm, uint32_t op2) noexcept {
    return (op1 << 11) | (cRn << 7) | (cRm << 3) | (op2 << 0);
  }

  //! Data cache maintenance immediate values.
  enum Value : uint32_t {
    kZVA     = encode(0b011, 0b0111, 0b0100, 0b001),
    kIVAC    = encode(0b000, 0b0111, 0b0110, 0b001),
    kISW     = encode(0b000, 0b0111, 0b0110, 0b010),
    kCVAC    = encode(0b011, 0b0111, 0b1010, 0b001),
    kCSW     = encode(0b000, 0b0111, 0b1010, 0b010),
    kCVAU    = encode(0b011, 0b0111, 0b1011, 0b001),
    kCIVAC   = encode(0b011, 0b0111, 0b1110, 0b001),
    kCISW    = encode(0b000, 0b0111, 0b1110, 0b010),
    kCVAP    = encode(0b011, 0b0111, 0b1100, 0b001),
    kCVADP   = encode(0b011, 0b0111, 0b1101, 0b001),
    kIGVAC   = encode(0b000, 0b0111, 0b0110, 0b011),
    kIGSW    = encode(0b000, 0b0111, 0b0110, 0b100),
    kCGSW    = encode(0b000, 0b0111, 0b1010, 0b100),
    kCIGSW   = encode(0b000, 0b0111, 0b1110, 0b100),
    kCGVAC   = encode(0b011, 0b0111, 0b1010, 0b011),
    kCGVAP   = encode(0b011, 0b0111, 0b1100, 0b011),
    kCGVADP  = encode(0b011, 0b0111, 0b1101, 0b011),
    kCIGVAC  = encode(0b011, 0b0111, 0b1110, 0b011),
    kGVA     = encode(0b011, 0b0111, 0b0100, 0b011),
    kIGDVAC  = encode(0b000, 0b0111, 0b0110, 0b101),
    kIGDSW   = encode(0b000, 0b0111, 0b0110, 0b110),
    kCGDSW   = encode(0b000, 0b0111, 0b1010, 0b110),
    kCIGDSW  = encode(0b000, 0b0111, 0b1110, 0b110),
    kCGDVAC  = encode(0b011, 0b0111, 0b1010, 0b101),
    kCGDVAP  = encode(0b011, 0b0111, 0b1100, 0b101),
    kCGDVADP = encode(0b011, 0b0111, 0b1101, 0b101),
    kCIGDVAC = encode(0b011, 0b0111, 0b1110, 0b101),
    kGZVA    = encode(0b011, 0b0111, 0b0100, 0b100)
  };
}

//! Instruction cache maintenance options.
namespace IC {
  static inline constexpr uint32_t encode(uint32_t op1, uint32_t cRn, uint32_t cRm, uint32_t op2) noexcept {
    return (op1 << 11) | (cRn << 7) | (cRm << 3) | (op2 << 0);
  }

  //! Instruction cache maintenance immediate values.
  enum Value : uint32_t {
    kIALLUIS = encode(0b000, 0b0111, 0b0001, 0b000),
    kIALLU   = encode(0b000, 0b0111, 0b0101, 0b000),
    kIVAU    = encode(0b011, 0b0111, 0b0101, 0b001)
  };
}

//! Instruction-fetch barrier options.
namespace ISB {
  //! Instruction-fetch barrier immediate values.
  enum Value : uint32_t {
    kSY = 0xF
  };
}

//! Prefetch options.
namespace PRFOp {
  //! Prefetch immediate values.
  enum Value : uint32_t {
    kPLDL1KEEP = 0x00,
    kPLDL1STRM = 0x01,
    kPLDL2KEEP = 0x02,
    kPLDL2STRM = 0x03,
    kPLDL3KEEP = 0x04,
    kPLDL3STRM = 0x05,
    kPLIL1KEEP = 0x08,
    kPLIL1STRM = 0x09,
    kPLIL2KEEP = 0x0A,
    kPLIL2STRM = 0x0B,
    kPLIL3KEEP = 0x0C,
    kPLIL3STRM = 0x0D,
    kPSTL1KEEP = 0x10,
    kPSTL1STRM = 0x11,
    kPSTL2KEEP = 0x12,
    kPSTL2STRM = 0x13,
    kPSTL3KEEP = 0x14,
    kPSTL3STRM = 0x15
  };
}

//! PSB instruction options.
namespace PSB {
  //! PSB immediate values.
  enum Value : uint32_t {
    kCSYNC = 0x11u
  };
}

namespace TLBI {
  static inline constexpr uint32_t encode(uint32_t op1, uint32_t cRn, uint32_t cRm, uint32_t op2) noexcept {
    return (op1 << 11) | (cRn << 7) | (cRm << 3) | (op2 << 0);
  }

  enum Value : uint32_t {
    kIPAS2E1IS    = encode(0b100, 0b1000, 0b0000, 0b001),
    kIPAS2LE1IS   = encode(0b100, 0b1000, 0b0000, 0b101),
    kVMALLE1IS    = encode(0b000, 0b1000, 0b0011, 0b000),
    kALLE2IS      = encode(0b100, 0b1000, 0b0011, 0b000),
    kALLE3IS      = encode(0b110, 0b1000, 0b0011, 0b000),
    kVAE1IS       = encode(0b000, 0b1000, 0b0011, 0b001),
    kVAE2IS       = encode(0b100, 0b1000, 0b0011, 0b001),
    kVAE3IS       = encode(0b110, 0b1000, 0b0011, 0b001),
    kASIDE1IS     = encode(0b000, 0b1000, 0b0011, 0b010),
    kVAAE1IS      = encode(0b000, 0b1000, 0b0011, 0b011),
    kALLE1IS      = encode(0b100, 0b1000, 0b0011, 0b100),
    kVALE1IS      = encode(0b000, 0b1000, 0b0011, 0b101),
    kVALE2IS      = encode(0b100, 0b1000, 0b0011, 0b101),
    kVALE3IS      = encode(0b110, 0b1000, 0b0011, 0b101),
    kVMALLS12E1IS = encode(0b100, 0b1000, 0b0011, 0b110),
    kVAALE1IS     = encode(0b000, 0b1000, 0b0011, 0b111),
    kIPAS2E1      = encode(0b100, 0b1000, 0b0100, 0b001),
    kIPAS2LE1     = encode(0b100, 0b1000, 0b0100, 0b101),
    kVMALLE1      = encode(0b000, 0b1000, 0b0111, 0b000),
    kALLE2        = encode(0b100, 0b1000, 0b0111, 0b000),
    kALLE3        = encode(0b110, 0b1000, 0b0111, 0b000),
    kVAE1         = encode(0b000, 0b1000, 0b0111, 0b001),
    kVAE2         = encode(0b100, 0b1000, 0b0111, 0b001),
    kVAE3         = encode(0b110, 0b1000, 0b0111, 0b001),
    kASIDE1       = encode(0b000, 0b1000, 0b0111, 0b010),
    kVAAE1        = encode(0b000, 0b1000, 0b0111, 0b011),
    kALLE1        = encode(0b100, 0b1000, 0b0111, 0b100),
    kVALE1        = encode(0b000, 0b1000, 0b0111, 0b101),
    kVALE2        = encode(0b100, 0b1000, 0b0111, 0b101),
    kVALE3        = encode(0b110, 0b1000, 0b0111, 0b101),
    kVMALLS12E1   = encode(0b100, 0b1000, 0b0111, 0b110),
    kVAALE1       = encode(0b000, 0b1000, 0b0111, 0b111),

    kVMALLE1OS    = encode(0b000, 0b1000, 0b0001, 0b000),
    kVAE1OS       = encode(0b000, 0b1000, 0b0001, 0b001),
    kASIDE1OS     = encode(0b000, 0b1000, 0b0001, 0b010),
    kVAAE1OS      = encode(0b000, 0b1000, 0b0001, 0b011),
    kVALE1OS      = encode(0b000, 0b1000, 0b0001, 0b101),
    kVAALE1OS     = encode(0b000, 0b1000, 0b0001, 0b111),
    kIPAS2E1OS    = encode(0b100, 0b1000, 0b0100, 0b000),
    kIPAS2LE1OS   = encode(0b100, 0b1000, 0b0100, 0b100),
    kVAE2OS       = encode(0b100, 0b1000, 0b0001, 0b001),
    kVALE2OS      = encode(0b100, 0b1000, 0b0001, 0b101),
    kVMALLS12E1OS = encode(0b100, 0b1000, 0b0001, 0b110),
    kVAE3OS       = encode(0b110, 0b1000, 0b0001, 0b001),
    kVALE3OS      = encode(0b110, 0b1000, 0b0001, 0b101),
    kALLE2OS      = encode(0b100, 0b1000, 0b0001, 0b000),
    kALLE1OS      = encode(0b100, 0b1000, 0b0001, 0b100),
    kALLE3OS      = encode(0b110, 0b1000, 0b0001, 0b000),

    kRVAE1        = encode(0b000, 0b1000, 0b0110, 0b001),
    kRVAAE1       = encode(0b000, 0b1000, 0b0110, 0b011),
    kRVALE1       = encode(0b000, 0b1000, 0b0110, 0b101),
    kRVAALE1      = encode(0b000, 0b1000, 0b0110, 0b111),
    kRVAE1IS      = encode(0b000, 0b1000, 0b0010, 0b001),
    kRVAAE1IS     = encode(0b000, 0b1000, 0b0010, 0b011),
    kRVALE1IS     = encode(0b000, 0b1000, 0b0010, 0b101),
    kRVAALE1IS    = encode(0b000, 0b1000, 0b0010, 0b111),
    kRVAE1OS      = encode(0b000, 0b1000, 0b0101, 0b001),
    kRVAAE1OS     = encode(0b000, 0b1000, 0b0101, 0b011),
    kRVALE1OS     = encode(0b000, 0b1000, 0b0101, 0b101),
    kRVAALE1OS    = encode(0b000, 0b1000, 0b0101, 0b111),
    kRIPAS2E1IS   = encode(0b100, 0b1000, 0b0000, 0b010),
    kRIPAS2LE1IS  = encode(0b100, 0b1000, 0b0000, 0b110),
    kRIPAS2E1     = encode(0b100, 0b1000, 0b0100, 0b010),
    kRIPAS2LE1    = encode(0b100, 0b1000, 0b0100, 0b110),
    kRIPAS2E1OS   = encode(0b100, 0b1000, 0b0100, 0b011),
    kRIPAS2LE1OS  = encode(0b100, 0b1000, 0b0100, 0b111),
    kRVAE2        = encode(0b100, 0b1000, 0b0110, 0b001),
    kRVALE2       = encode(0b100, 0b1000, 0b0110, 0b101),
    kRVAE2IS      = encode(0b100, 0b1000, 0b0010, 0b001),
    kRVALE2IS     = encode(0b100, 0b1000, 0b0010, 0b101),
    kRVAE2OS      = encode(0b100, 0b1000, 0b0101, 0b001),
    kRVALE2OS     = encode(0b100, 0b1000, 0b0101, 0b101),
    kRVAE3        = encode(0b110, 0b1000, 0b0110, 0b001),
    kRVALE3       = encode(0b110, 0b1000, 0b0110, 0b101),
    kRVAE3IS      = encode(0b110, 0b1000, 0b0010, 0b001),
    kRVALE3IS     = encode(0b110, 0b1000, 0b0010, 0b101),
    kRVAE3OS      = encode(0b110, 0b1000, 0b0101, 0b001),
    kRVALE3OS     = encode(0b110, 0b1000, 0b0101, 0b101),
  };
}

//! Trace synchronization barrier options.
namespace TSB {
  //! Trace synchronization immediate values.
  enum Value : uint32_t {
    kCSYNC = 0
  };
}

//! Processor state access through MSR.
namespace PState {
  //! Encodes a pstate from `op0` and `op1`.
  static inline constexpr uint32_t encode(uint32_t op0, uint32_t op1) noexcept {
    return (op0 << 3) | (op1 << 0);
  }

  //! Processor state access immediates.
  enum Value : uint32_t {
    kSPSel   = encode(0b000, 0b101),
    kDAIFSet = encode(0b011, 0b110),
    kDAIFClr = encode(0b011, 0b111),
    kPAN     = encode(0b000, 0b100),
    kUAO     = encode(0b000, 0b011),
    kDIT     = encode(0b011, 0b010),
    kSSBS    = encode(0b011, 0b001),
    kTCO     = encode(0b011, 0b100)
  };
};

//! System register identifiers and utilities (MSR/MRS).
namespace SysReg {
  //! System register fields.
  struct Fields {
    uint8_t op0;
    uint8_t op1;
    uint8_t cRn;
    uint8_t cRm;
    uint8_t op2;
  };

  //! Encodes a system register from `op0`, `op1`, `cRn`, `cRm`, and `op2` fields.
  static inline constexpr uint32_t encode(uint32_t op0, uint32_t op1, uint32_t cRn, uint32_t cRm, uint32_t op2) noexcept {
    return (op0 << 14) | (op1 << 11) | (cRn << 7) | (cRm << 3) | (op2 << 0);
  }

  //! Encodes a system register from `fields`.
  static inline constexpr uint32_t encode(const Fields& fields) noexcept {
    return encode(fields.op0, fields.op1, fields.cRn, fields.cRm, fields.op2);
  }

  //! Decodes a system register to \ref Fields.
  static inline constexpr Fields decode(uint32_t id) noexcept {
    return Fields {
      uint8_t((id >> 14) & 0x3u),
      uint8_t((id >> 11) & 0x7u),
      uint8_t((id >>  7) & 0xFu),
      uint8_t((id >>  3) & 0xFu),
      uint8_t((id >>  0) & 0x7u)
    };
  }

  //! System register identifiers.
  enum Id : uint32_t {
    kACTLR_EL1            = encode(0b11, 0b000, 0b0001, 0b0000, 0b001), // RW
    kACTLR_EL2            = encode(0b11, 0b100, 0b0001, 0b0000, 0b001), // RW
    kACTLR_EL3            = encode(0b11, 0b110, 0b0001, 0b0000, 0b001), // RW
    kAFSR0_EL1            = encode(0b11, 0b000, 0b0101, 0b0001, 0b000), // RW
    kAFSR0_EL12           = encode(0b11, 0b101, 0b0101, 0b0001, 0b000), // RW
    kAFSR0_EL2            = encode(0b11, 0b100, 0b0101, 0b0001, 0b000), // RW
    kAFSR0_EL3            = encode(0b11, 0b110, 0b0101, 0b0001, 0b000), // RW
    kAFSR1_EL1            = encode(0b11, 0b000, 0b0101, 0b0001, 0b001), // RW
    kAFSR1_EL12           = encode(0b11, 0b101, 0b0101, 0b0001, 0b001), // RW
    kAFSR1_EL2            = encode(0b11, 0b100, 0b0101, 0b0001, 0b001), // RW
    kAFSR1_EL3            = encode(0b11, 0b110, 0b0101, 0b0001, 0b001), // RW
    kAIDR_EL1             = encode(0b11, 0b001, 0b0000, 0b0000, 0b111), // RO
    kAMAIR_EL1            = encode(0b11, 0b000, 0b1010, 0b0011, 0b000), // RW
    kAMAIR_EL12           = encode(0b11, 0b101, 0b1010, 0b0011, 0b000), // RW
    kAMAIR_EL2            = encode(0b11, 0b100, 0b1010, 0b0011, 0b000), // RW
    kAMAIR_EL3            = encode(0b11, 0b110, 0b1010, 0b0011, 0b000), // RW
    kAMCFGR_EL0           = encode(0b11, 0b011, 0b1101, 0b0010, 0b001), // RO
    kAMCGCR_EL0           = encode(0b11, 0b011, 0b1101, 0b0010, 0b010), // RO
    kAMCNTENCLR0_EL0      = encode(0b11, 0b011, 0b1101, 0b0010, 0b100), // RW
    kAMCNTENCLR1_EL0      = encode(0b11, 0b011, 0b1101, 0b0011, 0b000), // RW
    kAMCNTENSET0_EL0      = encode(0b11, 0b011, 0b1101, 0b0010, 0b101), // RW
    kAMCNTENSET1_EL0      = encode(0b11, 0b011, 0b1101, 0b0011, 0b001), // RW
    kAMCR_EL0             = encode(0b11, 0b011, 0b1101, 0b0010, 0b000), // RW
    kAMEVCNTR00_EL0       = encode(0b11, 0b011, 0b1101, 0b0100, 0b000), // RW
    kAMEVCNTR01_EL0       = encode(0b11, 0b011, 0b1101, 0b0100, 0b001), // RW
    kAMEVCNTR02_EL0       = encode(0b11, 0b011, 0b1101, 0b0100, 0b010), // RW
    kAMEVCNTR03_EL0       = encode(0b11, 0b011, 0b1101, 0b0100, 0b011), // RW
    kAMEVCNTR10_EL0       = encode(0b11, 0b011, 0b1101, 0b1100, 0b000), // RW
    kAMEVCNTR110_EL0      = encode(0b11, 0b011, 0b1101, 0b1101, 0b010), // RW
    kAMEVCNTR111_EL0      = encode(0b11, 0b011, 0b1101, 0b1101, 0b011), // RW
    kAMEVCNTR112_EL0      = encode(0b11, 0b011, 0b1101, 0b1101, 0b100), // RW
    kAMEVCNTR113_EL0      = encode(0b11, 0b011, 0b1101, 0b1101, 0b101), // RW
    kAMEVCNTR114_EL0      = encode(0b11, 0b011, 0b1101, 0b1101, 0b110), // RW
    kAMEVCNTR115_EL0      = encode(0b11, 0b011, 0b1101, 0b1101, 0b111), // RW
    kAMEVCNTR11_EL0       = encode(0b11, 0b011, 0b1101, 0b1100, 0b001), // RW
    kAMEVCNTR12_EL0       = encode(0b11, 0b011, 0b1101, 0b1100, 0b010), // RW
    kAMEVCNTR13_EL0       = encode(0b11, 0b011, 0b1101, 0b1100, 0b011), // RW
    kAMEVCNTR14_EL0       = encode(0b11, 0b011, 0b1101, 0b1100, 0b100), // RW
    kAMEVCNTR15_EL0       = encode(0b11, 0b011, 0b1101, 0b1100, 0b101), // RW
    kAMEVCNTR16_EL0       = encode(0b11, 0b011, 0b1101, 0b1100, 0b110), // RW
    kAMEVCNTR17_EL0       = encode(0b11, 0b011, 0b1101, 0b1100, 0b111), // RW
    kAMEVCNTR18_EL0       = encode(0b11, 0b011, 0b1101, 0b1101, 0b000), // RW
    kAMEVCNTR19_EL0       = encode(0b11, 0b011, 0b1101, 0b1101, 0b001), // RW
    kAMEVTYPER00_EL0      = encode(0b11, 0b011, 0b1101, 0b0110, 0b000), // RO
    kAMEVTYPER01_EL0      = encode(0b11, 0b011, 0b1101, 0b0110, 0b001), // RO
    kAMEVTYPER02_EL0      = encode(0b11, 0b011, 0b1101, 0b0110, 0b010), // RO
    kAMEVTYPER03_EL0      = encode(0b11, 0b011, 0b1101, 0b0110, 0b011), // RO
    kAMEVTYPER10_EL0      = encode(0b11, 0b011, 0b1101, 0b1110, 0b000), // RW
    kAMEVTYPER110_EL0     = encode(0b11, 0b011, 0b1101, 0b1111, 0b010), // RW
    kAMEVTYPER111_EL0     = encode(0b11, 0b011, 0b1101, 0b1111, 0b011), // RW
    kAMEVTYPER112_EL0     = encode(0b11, 0b011, 0b1101, 0b1111, 0b100), // RW
    kAMEVTYPER113_EL0     = encode(0b11, 0b011, 0b1101, 0b1111, 0b101), // RW
    kAMEVTYPER114_EL0     = encode(0b11, 0b011, 0b1101, 0b1111, 0b110), // RW
    kAMEVTYPER115_EL0     = encode(0b11, 0b011, 0b1101, 0b1111, 0b111), // RW
    kAMEVTYPER11_EL0      = encode(0b11, 0b011, 0b1101, 0b1110, 0b001), // RW
    kAMEVTYPER12_EL0      = encode(0b11, 0b011, 0b1101, 0b1110, 0b010), // RW
    kAMEVTYPER13_EL0      = encode(0b11, 0b011, 0b1101, 0b1110, 0b011), // RW
    kAMEVTYPER14_EL0      = encode(0b11, 0b011, 0b1101, 0b1110, 0b100), // RW
    kAMEVTYPER15_EL0      = encode(0b11, 0b011, 0b1101, 0b1110, 0b101), // RW
    kAMEVTYPER16_EL0      = encode(0b11, 0b011, 0b1101, 0b1110, 0b110), // RW
    kAMEVTYPER17_EL0      = encode(0b11, 0b011, 0b1101, 0b1110, 0b111), // RW
    kAMEVTYPER18_EL0      = encode(0b11, 0b011, 0b1101, 0b1111, 0b000), // RW
    kAMEVTYPER19_EL0      = encode(0b11, 0b011, 0b1101, 0b1111, 0b001), // RW
    kAMUSERENR_EL0        = encode(0b11, 0b011, 0b1101, 0b0010, 0b011), // RW
    kAPDAKeyHi_EL1        = encode(0b11, 0b000, 0b0010, 0b0010, 0b001), // RW
    kAPDAKeyLo_EL1        = encode(0b11, 0b000, 0b0010, 0b0010, 0b000), // RW
    kAPDBKeyHi_EL1        = encode(0b11, 0b000, 0b0010, 0b0010, 0b011), // RW
    kAPDBKeyLo_EL1        = encode(0b11, 0b000, 0b0010, 0b0010, 0b010), // RW
    kAPGAKeyHi_EL1        = encode(0b11, 0b000, 0b0010, 0b0011, 0b001), // RW
    kAPGAKeyLo_EL1        = encode(0b11, 0b000, 0b0010, 0b0011, 0b000), // RW
    kAPIAKeyHi_EL1        = encode(0b11, 0b000, 0b0010, 0b0001, 0b001), // RW
    kAPIAKeyLo_EL1        = encode(0b11, 0b000, 0b0010, 0b0001, 0b000), // RW
    kAPIBKeyHi_EL1        = encode(0b11, 0b000, 0b0010, 0b0001, 0b011), // RW
    kAPIBKeyLo_EL1        = encode(0b11, 0b000, 0b0010, 0b0001, 0b010), // RW
    kCCSIDR2_EL1          = encode(0b11, 0b001, 0b0000, 0b0000, 0b010), // RO
    kCCSIDR_EL1           = encode(0b11, 0b001, 0b0000, 0b0000, 0b000), // RO
    kCLIDR_EL1            = encode(0b11, 0b001, 0b0000, 0b0000, 0b001), // RO
    kCNTFRQ_EL0           = encode(0b11, 0b011, 0b1110, 0b0000, 0b000), // RW
    kCNTHCTL_EL2          = encode(0b11, 0b100, 0b1110, 0b0001, 0b000), // RW
    kCNTHPS_CTL_EL2       = encode(0b11, 0b100, 0b1110, 0b0101, 0b001), // RW
    kCNTHPS_CVAL_EL2      = encode(0b11, 0b100, 0b1110, 0b0101, 0b010), // RW
    kCNTHPS_TVAL_EL2      = encode(0b11, 0b100, 0b1110, 0b0101, 0b000), // RW
    kCNTHP_CTL_EL2        = encode(0b11, 0b100, 0b1110, 0b0010, 0b001), // RW
    kCNTHP_CVAL_EL2       = encode(0b11, 0b100, 0b1110, 0b0010, 0b010), // RW
    kCNTHP_TVAL_EL2       = encode(0b11, 0b100, 0b1110, 0b0010, 0b000), // RW
    kCNTHVS_CTL_EL2       = encode(0b11, 0b100, 0b1110, 0b0100, 0b001), // RW
    kCNTHVS_CVAL_EL2      = encode(0b11, 0b100, 0b1110, 0b0100, 0b010), // RW
    kCNTHVS_TVAL_EL2      = encode(0b11, 0b100, 0b1110, 0b0100, 0b000), // RW
    kCNTHV_CTL_EL2        = encode(0b11, 0b100, 0b1110, 0b0011, 0b001), // RW
    kCNTHV_CVAL_EL2       = encode(0b11, 0b100, 0b1110, 0b0011, 0b010), // RW
    kCNTHV_TVAL_EL2       = encode(0b11, 0b100, 0b1110, 0b0011, 0b000), // RW
    kCNTISCALE_EL2        = encode(0b11, 0b100, 0b1110, 0b0000, 0b101), // RW
    kCNTKCTL_EL1          = encode(0b11, 0b000, 0b1110, 0b0001, 0b000), // RW
    kCNTKCTL_EL12         = encode(0b11, 0b101, 0b1110, 0b0001, 0b000), // RW
    kCNTPCTSS_EL0         = encode(0b11, 0b011, 0b1110, 0b0000, 0b101), // RW
    kCNTPCT_EL0           = encode(0b11, 0b011, 0b1110, 0b0000, 0b001), // RO
    kCNTPOFF_EL2          = encode(0b11, 0b100, 0b1110, 0b0000, 0b110), // RW
    kCNTPS_CTL_EL1        = encode(0b11, 0b111, 0b1110, 0b0010, 0b001), // RW
    kCNTPS_CVAL_EL1       = encode(0b11, 0b111, 0b1110, 0b0010, 0b010), // RW
    kCNTPS_TVAL_EL1       = encode(0b11, 0b111, 0b1110, 0b0010, 0b000), // RW
    kCNTP_CTL_EL0         = encode(0b11, 0b011, 0b1110, 0b0010, 0b001), // RW
    kCNTP_CTL_EL02        = encode(0b11, 0b101, 0b1110, 0b0010, 0b001), // RW
    kCNTP_CVAL_EL0        = encode(0b11, 0b011, 0b1110, 0b0010, 0b010), // RW
    kCNTP_CVAL_EL02       = encode(0b11, 0b101, 0b1110, 0b0010, 0b010), // RW
    kCNTP_TVAL_EL0        = encode(0b11, 0b011, 0b1110, 0b0010, 0b000), // RW
    kCNTP_TVAL_EL02       = encode(0b11, 0b101, 0b1110, 0b0010, 0b000), // RW
    kCNTSCALE_EL2         = encode(0b11, 0b100, 0b1110, 0b0000, 0b100), // RW
    kCNTVCTSS_EL0         = encode(0b11, 0b011, 0b1110, 0b0000, 0b110), // RW
    kCNTVCT_EL0           = encode(0b11, 0b011, 0b1110, 0b0000, 0b010), // RO
    kCNTVFRQ_EL2          = encode(0b11, 0b100, 0b1110, 0b0000, 0b111), // RW
    kCNTVOFF_EL2          = encode(0b11, 0b100, 0b1110, 0b0000, 0b011), // RW
    kCNTV_CTL_EL0         = encode(0b11, 0b011, 0b1110, 0b0011, 0b001), // RW
    kCNTV_CTL_EL02        = encode(0b11, 0b101, 0b1110, 0b0011, 0b001), // RW
    kCNTV_CVAL_EL0        = encode(0b11, 0b011, 0b1110, 0b0011, 0b010), // RW
    kCNTV_CVAL_EL02       = encode(0b11, 0b101, 0b1110, 0b0011, 0b010), // RW
    kCNTV_TVAL_EL0        = encode(0b11, 0b011, 0b1110, 0b0011, 0b000), // RW
    kCNTV_TVAL_EL02       = encode(0b11, 0b101, 0b1110, 0b0011, 0b000), // RW
    kCONTEXTIDR_EL1       = encode(0b11, 0b000, 0b1101, 0b0000, 0b001), // RW
    kCONTEXTIDR_EL12      = encode(0b11, 0b101, 0b1101, 0b0000, 0b001), // RW
    kCONTEXTIDR_EL2       = encode(0b11, 0b100, 0b1101, 0b0000, 0b001), // RW
    kCPACR_EL1            = encode(0b11, 0b000, 0b0001, 0b0000, 0b010), // RW
    kCPACR_EL12           = encode(0b11, 0b101, 0b0001, 0b0000, 0b010), // RW
    kCPM_IOACC_CTL_EL3    = encode(0b11, 0b111, 0b1111, 0b0010, 0b000), // RW
    kCPTR_EL2             = encode(0b11, 0b100, 0b0001, 0b0001, 0b010), // RW
    kCPTR_EL3             = encode(0b11, 0b110, 0b0001, 0b0001, 0b010), // RW
    kCSSELR_EL1           = encode(0b11, 0b010, 0b0000, 0b0000, 0b000), // RW
    kCTR_EL0              = encode(0b11, 0b011, 0b0000, 0b0000, 0b001), // RO
    kCurrentEL            = encode(0b11, 0b000, 0b0100, 0b0010, 0b010), // RO
    kDACR32_EL2           = encode(0b11, 0b100, 0b0011, 0b0000, 0b000), // RW
    kDAIF                 = encode(0b11, 0b011, 0b0100, 0b0010, 0b001), // RW
    kDBGAUTHSTATUS_EL1    = encode(0b10, 0b000, 0b0111, 0b1110, 0b110), // RO
    kDBGBCR0_EL1          = encode(0b10, 0b000, 0b0000, 0b0000, 0b101), // RW
    kDBGBCR10_EL1         = encode(0b10, 0b000, 0b0000, 0b1010, 0b101), // RW
    kDBGBCR11_EL1         = encode(0b10, 0b000, 0b0000, 0b1011, 0b101), // RW
    kDBGBCR12_EL1         = encode(0b10, 0b000, 0b0000, 0b1100, 0b101), // RW
    kDBGBCR13_EL1         = encode(0b10, 0b000, 0b0000, 0b1101, 0b101), // RW
    kDBGBCR14_EL1         = encode(0b10, 0b000, 0b0000, 0b1110, 0b101), // RW
    kDBGBCR15_EL1         = encode(0b10, 0b000, 0b0000, 0b1111, 0b101), // RW
    kDBGBCR1_EL1          = encode(0b10, 0b000, 0b0000, 0b0001, 0b101), // RW
    kDBGBCR2_EL1          = encode(0b10, 0b000, 0b0000, 0b0010, 0b101), // RW
    kDBGBCR3_EL1          = encode(0b10, 0b000, 0b0000, 0b0011, 0b101), // RW
    kDBGBCR4_EL1          = encode(0b10, 0b000, 0b0000, 0b0100, 0b101), // RW
    kDBGBCR5_EL1          = encode(0b10, 0b000, 0b0000, 0b0101, 0b101), // RW
    kDBGBCR6_EL1          = encode(0b10, 0b000, 0b0000, 0b0110, 0b101), // RW
    kDBGBCR7_EL1          = encode(0b10, 0b000, 0b0000, 0b0111, 0b101), // RW
    kDBGBCR8_EL1          = encode(0b10, 0b000, 0b0000, 0b1000, 0b101), // RW
    kDBGBCR9_EL1          = encode(0b10, 0b000, 0b0000, 0b1001, 0b101), // RW
    kDBGBVR0_EL1          = encode(0b10, 0b000, 0b0000, 0b0000, 0b100), // RW
    kDBGBVR10_EL1         = encode(0b10, 0b000, 0b0000, 0b1010, 0b100), // RW
    kDBGBVR11_EL1         = encode(0b10, 0b000, 0b0000, 0b1011, 0b100), // RW
    kDBGBVR12_EL1         = encode(0b10, 0b000, 0b0000, 0b1100, 0b100), // RW
    kDBGBVR13_EL1         = encode(0b10, 0b000, 0b0000, 0b1101, 0b100), // RW
    kDBGBVR14_EL1         = encode(0b10, 0b000, 0b0000, 0b1110, 0b100), // RW
    kDBGBVR15_EL1         = encode(0b10, 0b000, 0b0000, 0b1111, 0b100), // RW
    kDBGBVR1_EL1          = encode(0b10, 0b000, 0b0000, 0b0001, 0b100), // RW
    kDBGBVR2_EL1          = encode(0b10, 0b000, 0b0000, 0b0010, 0b100), // RW
    kDBGBVR3_EL1          = encode(0b10, 0b000, 0b0000, 0b0011, 0b100), // RW
    kDBGBVR4_EL1          = encode(0b10, 0b000, 0b0000, 0b0100, 0b100), // RW
    kDBGBVR5_EL1          = encode(0b10, 0b000, 0b0000, 0b0101, 0b100), // RW
    kDBGBVR6_EL1          = encode(0b10, 0b000, 0b0000, 0b0110, 0b100), // RW
    kDBGBVR7_EL1          = encode(0b10, 0b000, 0b0000, 0b0111, 0b100), // RW
    kDBGBVR8_EL1          = encode(0b10, 0b000, 0b0000, 0b1000, 0b100), // RW
    kDBGBVR9_EL1          = encode(0b10, 0b000, 0b0000, 0b1001, 0b100), // RW
    kDBGCLAIMCLR_EL1      = encode(0b10, 0b000, 0b0111, 0b1001, 0b110), // RW
    kDBGCLAIMSET_EL1      = encode(0b10, 0b000, 0b0111, 0b1000, 0b110), // RW
    kDBGDTRRX_EL0         = encode(0b10, 0b011, 0b0000, 0b0101, 0b000), // RO
    kDBGDTRTX_EL0         = encode(0b10, 0b011, 0b0000, 0b0101, 0b000), // WO
    kDBGDTR_EL0           = encode(0b10, 0b011, 0b0000, 0b0100, 0b000), // RW
    kDBGPRCR_EL1          = encode(0b10, 0b000, 0b0001, 0b0100, 0b100), // RW
    kDBGVCR32_EL2         = encode(0b10, 0b100, 0b0000, 0b0111, 0b000), // RW
    kDBGWCR0_EL1          = encode(0b10, 0b000, 0b0000, 0b0000, 0b111), // RW
    kDBGWCR10_EL1         = encode(0b10, 0b000, 0b0000, 0b1010, 0b111), // RW
    kDBGWCR11_EL1         = encode(0b10, 0b000, 0b0000, 0b1011, 0b111), // RW
    kDBGWCR12_EL1         = encode(0b10, 0b000, 0b0000, 0b1100, 0b111), // RW
    kDBGWCR13_EL1         = encode(0b10, 0b000, 0b0000, 0b1101, 0b111), // RW
    kDBGWCR14_EL1         = encode(0b10, 0b000, 0b0000, 0b1110, 0b111), // RW
    kDBGWCR15_EL1         = encode(0b10, 0b000, 0b0000, 0b1111, 0b111), // RW
    kDBGWCR1_EL1          = encode(0b10, 0b000, 0b0000, 0b0001, 0b111), // RW
    kDBGWCR2_EL1          = encode(0b10, 0b000, 0b0000, 0b0010, 0b111), // RW
    kDBGWCR3_EL1          = encode(0b10, 0b000, 0b0000, 0b0011, 0b111), // RW
    kDBGWCR4_EL1          = encode(0b10, 0b000, 0b0000, 0b0100, 0b111), // RW
    kDBGWCR5_EL1          = encode(0b10, 0b000, 0b0000, 0b0101, 0b111), // RW
    kDBGWCR6_EL1          = encode(0b10, 0b000, 0b0000, 0b0110, 0b111), // RW
    kDBGWCR7_EL1          = encode(0b10, 0b000, 0b0000, 0b0111, 0b111), // RW
    kDBGWCR8_EL1          = encode(0b10, 0b000, 0b0000, 0b1000, 0b111), // RW
    kDBGWCR9_EL1          = encode(0b10, 0b000, 0b0000, 0b1001, 0b111), // RW
    kDBGWVR0_EL1          = encode(0b10, 0b000, 0b0000, 0b0000, 0b110), // RW
    kDBGWVR10_EL1         = encode(0b10, 0b000, 0b0000, 0b1010, 0b110), // RW
    kDBGWVR11_EL1         = encode(0b10, 0b000, 0b0000, 0b1011, 0b110), // RW
    kDBGWVR12_EL1         = encode(0b10, 0b000, 0b0000, 0b1100, 0b110), // RW
    kDBGWVR13_EL1         = encode(0b10, 0b000, 0b0000, 0b1101, 0b110), // RW
    kDBGWVR14_EL1         = encode(0b10, 0b000, 0b0000, 0b1110, 0b110), // RW
    kDBGWVR15_EL1         = encode(0b10, 0b000, 0b0000, 0b1111, 0b110), // RW
    kDBGWVR1_EL1          = encode(0b10, 0b000, 0b0000, 0b0001, 0b110), // RW
    kDBGWVR2_EL1          = encode(0b10, 0b000, 0b0000, 0b0010, 0b110), // RW
    kDBGWVR3_EL1          = encode(0b10, 0b000, 0b0000, 0b0011, 0b110), // RW
    kDBGWVR4_EL1          = encode(0b10, 0b000, 0b0000, 0b0100, 0b110), // RW
    kDBGWVR5_EL1          = encode(0b10, 0b000, 0b0000, 0b0101, 0b110), // RW
    kDBGWVR6_EL1          = encode(0b10, 0b000, 0b0000, 0b0110, 0b110), // RW
    kDBGWVR7_EL1          = encode(0b10, 0b000, 0b0000, 0b0111, 0b110), // RW
    kDBGWVR8_EL1          = encode(0b10, 0b000, 0b0000, 0b1000, 0b110), // RW
    kDBGWVR9_EL1          = encode(0b10, 0b000, 0b0000, 0b1001, 0b110), // RW
    kDCZID_EL0            = encode(0b11, 0b011, 0b0000, 0b0000, 0b111), // RO
    kDISR_EL1             = encode(0b11, 0b000, 0b1100, 0b0001, 0b001), // RW
    kDIT                  = encode(0b11, 0b011, 0b0100, 0b0010, 0b101), // RW
    kDLR_EL0              = encode(0b11, 0b011, 0b0100, 0b0101, 0b001), // RW
    kDSPSR_EL0            = encode(0b11, 0b011, 0b0100, 0b0101, 0b000), // RW
    kELR_EL1              = encode(0b11, 0b000, 0b0100, 0b0000, 0b001), // RW
    kELR_EL12             = encode(0b11, 0b101, 0b0100, 0b0000, 0b001), // RW
    kELR_EL2              = encode(0b11, 0b100, 0b0100, 0b0000, 0b001), // RW
    kELR_EL3              = encode(0b11, 0b110, 0b0100, 0b0000, 0b001), // RW
    kERRIDR_EL1           = encode(0b11, 0b000, 0b0101, 0b0011, 0b000), // RO
    kERRSELR_EL1          = encode(0b11, 0b000, 0b0101, 0b0011, 0b001), // RW
    kERXADDR_EL1          = encode(0b11, 0b000, 0b0101, 0b0100, 0b011), // RW
    kERXCTLR_EL1          = encode(0b11, 0b000, 0b0101, 0b0100, 0b001), // RW
    kERXFR_EL1            = encode(0b11, 0b000, 0b0101, 0b0100, 0b000), // RO
    kERXMISC0_EL1         = encode(0b11, 0b000, 0b0101, 0b0101, 0b000), // RW
    kERXMISC1_EL1         = encode(0b11, 0b000, 0b0101, 0b0101, 0b001), // RW
    kERXMISC2_EL1         = encode(0b11, 0b000, 0b0101, 0b0101, 0b010), // RW
    kERXMISC3_EL1         = encode(0b11, 0b000, 0b0101, 0b0101, 0b011), // RW
    kERXPFGCDN_EL1        = encode(0b11, 0b000, 0b0101, 0b0100, 0b110), // RW
    kERXPFGCTL_EL1        = encode(0b11, 0b000, 0b0101, 0b0100, 0b101), // RW
    kERXPFGF_EL1          = encode(0b11, 0b000, 0b0101, 0b0100, 0b100), // RO
    kERXSTATUS_EL1        = encode(0b11, 0b000, 0b0101, 0b0100, 0b010), // RW
    kESR_EL1              = encode(0b11, 0b000, 0b0101, 0b0010, 0b000), // RW
    kESR_EL12             = encode(0b11, 0b101, 0b0101, 0b0010, 0b000), // RW
    kESR_EL2              = encode(0b11, 0b100, 0b0101, 0b0010, 0b000), // RW
    kESR_EL3              = encode(0b11, 0b110, 0b0101, 0b0010, 0b000), // RW
    kFAR_EL1              = encode(0b11, 0b000, 0b0110, 0b0000, 0b000), // RW
    kFAR_EL12             = encode(0b11, 0b101, 0b0110, 0b0000, 0b000), // RW
    kFAR_EL2              = encode(0b11, 0b100, 0b0110, 0b0000, 0b000), // RW
    kFAR_EL3              = encode(0b11, 0b110, 0b0110, 0b0000, 0b000), // RW
    kFPCR                 = encode(0b11, 0b011, 0b0100, 0b0100, 0b000), // RW
    kFPEXC32_EL2          = encode(0b11, 0b100, 0b0101, 0b0011, 0b000), // RW
    kFPSR                 = encode(0b11, 0b011, 0b0100, 0b0100, 0b001), // RW
    kGCR_EL1              = encode(0b11, 0b000, 0b0001, 0b0000, 0b110), // RW
    kGMID_EL1             = encode(0b11, 0b001, 0b0000, 0b0000, 0b100), // RO
    kHACR_EL2             = encode(0b11, 0b100, 0b0001, 0b0001, 0b111), // RW
    kHCR_EL2              = encode(0b11, 0b100, 0b0001, 0b0001, 0b000), // RW
    kHDFGRTR_EL2          = encode(0b11, 0b100, 0b0011, 0b0001, 0b100), // RW
    kHDFGWTR_EL2          = encode(0b11, 0b100, 0b0011, 0b0001, 0b101), // RW
    kHFGITR_EL2           = encode(0b11, 0b100, 0b0001, 0b0001, 0b110), // RW
    kHFGRTR_EL2           = encode(0b11, 0b100, 0b0001, 0b0001, 0b100), // RW
    kHFGWTR_EL2           = encode(0b11, 0b100, 0b0001, 0b0001, 0b101), // RW
    kHPFAR_EL2            = encode(0b11, 0b100, 0b0110, 0b0000, 0b100), // RW
    kHSTR_EL2             = encode(0b11, 0b100, 0b0001, 0b0001, 0b011), // RW
    kICC_AP0R0_EL1        = encode(0b11, 0b000, 0b1100, 0b1000, 0b100), // RW
    kICC_AP0R1_EL1        = encode(0b11, 0b000, 0b1100, 0b1000, 0b101), // RW
    kICC_AP0R2_EL1        = encode(0b11, 0b000, 0b1100, 0b1000, 0b110), // RW
    kICC_AP0R3_EL1        = encode(0b11, 0b000, 0b1100, 0b1000, 0b111), // RW
    kICC_AP1R0_EL1        = encode(0b11, 0b000, 0b1100, 0b1001, 0b000), // RW
    kICC_AP1R1_EL1        = encode(0b11, 0b000, 0b1100, 0b1001, 0b001), // RW
    kICC_AP1R2_EL1        = encode(0b11, 0b000, 0b1100, 0b1001, 0b010), // RW
    kICC_AP1R3_EL1        = encode(0b11, 0b000, 0b1100, 0b1001, 0b011), // RW
    kICC_ASGI1R_EL1       = encode(0b11, 0b000, 0b1100, 0b1011, 0b110), // WO
    kICC_BPR0_EL1         = encode(0b11, 0b000, 0b1100, 0b1000, 0b011), // RW
    kICC_BPR1_EL1         = encode(0b11, 0b000, 0b1100, 0b1100, 0b011), // RW
    kICC_CTLR_EL1         = encode(0b11, 0b000, 0b1100, 0b1100, 0b100), // RW
    kICC_CTLR_EL3         = encode(0b11, 0b110, 0b1100, 0b1100, 0b100), // RW
    kICC_DIR_EL1          = encode(0b11, 0b000, 0b1100, 0b1011, 0b001), // WO
    kICC_EOIR0_EL1        = encode(0b11, 0b000, 0b1100, 0b1000, 0b001), // WO
    kICC_EOIR1_EL1        = encode(0b11, 0b000, 0b1100, 0b1100, 0b001), // WO
    kICC_HPPIR0_EL1       = encode(0b11, 0b000, 0b1100, 0b1000, 0b010), // RO
    kICC_HPPIR1_EL1       = encode(0b11, 0b000, 0b1100, 0b1100, 0b010), // RO
    kICC_IAR0_EL1         = encode(0b11, 0b000, 0b1100, 0b1000, 0b000), // RO
    kICC_IAR1_EL1         = encode(0b11, 0b000, 0b1100, 0b1100, 0b000), // RO
    kICC_IGRPEN0_EL1      = encode(0b11, 0b000, 0b1100, 0b1100, 0b110), // RW
    kICC_IGRPEN1_EL1      = encode(0b11, 0b000, 0b1100, 0b1100, 0b111), // RW
    kICC_IGRPEN1_EL3      = encode(0b11, 0b110, 0b1100, 0b1100, 0b111), // RW
    kICC_PMR_EL1          = encode(0b11, 0b000, 0b0100, 0b0110, 0b000), // RW
    kICC_RPR_EL1          = encode(0b11, 0b000, 0b1100, 0b1011, 0b011), // RO
    kICC_SGI0R_EL1        = encode(0b11, 0b000, 0b1100, 0b1011, 0b111), // WO
    kICC_SGI1R_EL1        = encode(0b11, 0b000, 0b1100, 0b1011, 0b101), // WO
    kICC_SRE_EL1          = encode(0b11, 0b000, 0b1100, 0b1100, 0b101), // RW
    kICC_SRE_EL2          = encode(0b11, 0b100, 0b1100, 0b1001, 0b101), // RW
    kICC_SRE_EL3          = encode(0b11, 0b110, 0b1100, 0b1100, 0b101), // RW
    kICH_AP0R0_EL2        = encode(0b11, 0b100, 0b1100, 0b1000, 0b000), // RW
    kICH_AP0R1_EL2        = encode(0b11, 0b100, 0b1100, 0b1000, 0b001), // RW
    kICH_AP0R2_EL2        = encode(0b11, 0b100, 0b1100, 0b1000, 0b010), // RW
    kICH_AP0R3_EL2        = encode(0b11, 0b100, 0b1100, 0b1000, 0b011), // RW
    kICH_AP1R0_EL2        = encode(0b11, 0b100, 0b1100, 0b1001, 0b000), // RW
    kICH_AP1R1_EL2        = encode(0b11, 0b100, 0b1100, 0b1001, 0b001), // RW
    kICH_AP1R2_EL2        = encode(0b11, 0b100, 0b1100, 0b1001, 0b010), // RW
    kICH_AP1R3_EL2        = encode(0b11, 0b100, 0b1100, 0b1001, 0b011), // RW
    kICH_EISR_EL2         = encode(0b11, 0b100, 0b1100, 0b1011, 0b011), // RO
    kICH_ELRSR_EL2        = encode(0b11, 0b100, 0b1100, 0b1011, 0b101), // RO
    kICH_HCR_EL2          = encode(0b11, 0b100, 0b1100, 0b1011, 0b000), // RW
    kICH_LR0_EL2          = encode(0b11, 0b100, 0b1100, 0b1100, 0b000), // RW
    kICH_LR10_EL2         = encode(0b11, 0b100, 0b1100, 0b1101, 0b010), // RW
    kICH_LR11_EL2         = encode(0b11, 0b100, 0b1100, 0b1101, 0b011), // RW
    kICH_LR12_EL2         = encode(0b11, 0b100, 0b1100, 0b1101, 0b100), // RW
    kICH_LR13_EL2         = encode(0b11, 0b100, 0b1100, 0b1101, 0b101), // RW
    kICH_LR14_EL2         = encode(0b11, 0b100, 0b1100, 0b1101, 0b110), // RW
    kICH_LR15_EL2         = encode(0b11, 0b100, 0b1100, 0b1101, 0b111), // RW
    kICH_LR1_EL2          = encode(0b11, 0b100, 0b1100, 0b1100, 0b001), // RW
    kICH_LR2_EL2          = encode(0b11, 0b100, 0b1100, 0b1100, 0b010), // RW
    kICH_LR3_EL2          = encode(0b11, 0b100, 0b1100, 0b1100, 0b011), // RW
    kICH_LR4_EL2          = encode(0b11, 0b100, 0b1100, 0b1100, 0b100), // RW
    kICH_LR5_EL2          = encode(0b11, 0b100, 0b1100, 0b1100, 0b101), // RW
    kICH_LR6_EL2          = encode(0b11, 0b100, 0b1100, 0b1100, 0b110), // RW
    kICH_LR7_EL2          = encode(0b11, 0b100, 0b1100, 0b1100, 0b111), // RW
    kICH_LR8_EL2          = encode(0b11, 0b100, 0b1100, 0b1101, 0b000), // RW
    kICH_LR9_EL2          = encode(0b11, 0b100, 0b1100, 0b1101, 0b001), // RW
    kICH_MISR_EL2         = encode(0b11, 0b100, 0b1100, 0b1011, 0b010), // RO
    kICH_VMCR_EL2         = encode(0b11, 0b100, 0b1100, 0b1011, 0b111), // RW
    kICH_VTR_EL2          = encode(0b11, 0b100, 0b1100, 0b1011, 0b001), // RO
    kID_AA64AFR0_EL1      = encode(0b11, 0b000, 0b0000, 0b0101, 0b100), // RO
    kID_AA64AFR1_EL1      = encode(0b11, 0b000, 0b0000, 0b0101, 0b101), // RO
    kID_AA64DFR0_EL1      = encode(0b11, 0b000, 0b0000, 0b0101, 0b000), // RO
    kID_AA64DFR1_EL1      = encode(0b11, 0b000, 0b0000, 0b0101, 0b001), // RO
    kID_AA64ISAR0_EL1     = encode(0b11, 0b000, 0b0000, 0b0110, 0b000), // RO
    kID_AA64ISAR1_EL1     = encode(0b11, 0b000, 0b0000, 0b0110, 0b001), // RO
    kID_AA64MMFR0_EL1     = encode(0b11, 0b000, 0b0000, 0b0111, 0b000), // RO
    kID_AA64MMFR1_EL1     = encode(0b11, 0b000, 0b0000, 0b0111, 0b001), // RO
    kID_AA64MMFR2_EL1     = encode(0b11, 0b000, 0b0000, 0b0111, 0b010), // RO
    kID_AA64PFR0_EL1      = encode(0b11, 0b000, 0b0000, 0b0100, 0b000), // RO
    kID_AA64PFR1_EL1      = encode(0b11, 0b000, 0b0000, 0b0100, 0b001), // RO
    kID_AA64ZFR0_EL1      = encode(0b11, 0b000, 0b0000, 0b0100, 0b100), // RO
    kID_AFR0_EL1          = encode(0b11, 0b000, 0b0000, 0b0001, 0b011), // RO
    kID_DFR0_EL1          = encode(0b11, 0b000, 0b0000, 0b0001, 0b010), // RO
    kID_ISAR0_EL1         = encode(0b11, 0b000, 0b0000, 0b0010, 0b000), // RO
    kID_ISAR1_EL1         = encode(0b11, 0b000, 0b0000, 0b0010, 0b001), // RO
    kID_ISAR2_EL1         = encode(0b11, 0b000, 0b0000, 0b0010, 0b010), // RO
    kID_ISAR3_EL1         = encode(0b11, 0b000, 0b0000, 0b0010, 0b011), // RO
    kID_ISAR4_EL1         = encode(0b11, 0b000, 0b0000, 0b0010, 0b100), // RO
    kID_ISAR5_EL1         = encode(0b11, 0b000, 0b0000, 0b0010, 0b101), // RO
    kID_ISAR6_EL1         = encode(0b11, 0b000, 0b0000, 0b0010, 0b111), // RO
    kID_MMFR0_EL1         = encode(0b11, 0b000, 0b0000, 0b0001, 0b100), // RO
    kID_MMFR1_EL1         = encode(0b11, 0b000, 0b0000, 0b0001, 0b101), // RO
    kID_MMFR2_EL1         = encode(0b11, 0b000, 0b0000, 0b0001, 0b110), // RO
    kID_MMFR3_EL1         = encode(0b11, 0b000, 0b0000, 0b0001, 0b111), // RO
    kID_MMFR4_EL1         = encode(0b11, 0b000, 0b0000, 0b0010, 0b110), // RO
    kID_MMFR5_EL1         = encode(0b11, 0b000, 0b0000, 0b0011, 0b110), // RO
    kID_PFR0_EL1          = encode(0b11, 0b000, 0b0000, 0b0001, 0b000), // RO
    kID_PFR1_EL1          = encode(0b11, 0b000, 0b0000, 0b0001, 0b001), // RO
    kID_PFR2_EL1          = encode(0b11, 0b000, 0b0000, 0b0011, 0b100), // RO
    kIFSR32_EL2           = encode(0b11, 0b100, 0b0101, 0b0000, 0b001), // RW
    kISR_EL1              = encode(0b11, 0b000, 0b1100, 0b0001, 0b000), // RO
    kLORC_EL1             = encode(0b11, 0b000, 0b1010, 0b0100, 0b011), // RW
    kLOREA_EL1            = encode(0b11, 0b000, 0b1010, 0b0100, 0b001), // RW
    kLORID_EL1            = encode(0b11, 0b000, 0b1010, 0b0100, 0b111), // RO
    kLORN_EL1             = encode(0b11, 0b000, 0b1010, 0b0100, 0b010), // RW
    kLORSA_EL1            = encode(0b11, 0b000, 0b1010, 0b0100, 0b000), // RW
    kMAIR_EL1             = encode(0b11, 0b000, 0b1010, 0b0010, 0b000), // RW
    kMAIR_EL12            = encode(0b11, 0b101, 0b1010, 0b0010, 0b000), // RW
    kMAIR_EL2             = encode(0b11, 0b100, 0b1010, 0b0010, 0b000), // RW
    kMAIR_EL3             = encode(0b11, 0b110, 0b1010, 0b0010, 0b000), // RW
    kMDCCINT_EL1          = encode(0b10, 0b000, 0b0000, 0b0010, 0b000), // RW
    kMDCCSR_EL0           = encode(0b10, 0b011, 0b0000, 0b0001, 0b000), // RO
    kMDCR_EL2             = encode(0b11, 0b100, 0b0001, 0b0001, 0b001), // RW
    kMDCR_EL3             = encode(0b11, 0b110, 0b0001, 0b0011, 0b001), // RW
    kMDRAR_EL1            = encode(0b10, 0b000, 0b0001, 0b0000, 0b000), // RO
    kMDSCR_EL1            = encode(0b10, 0b000, 0b0000, 0b0010, 0b010), // RW
    kMIDR_EL1             = encode(0b11, 0b000, 0b0000, 0b0000, 0b000), // RO
    kMPAM0_EL1            = encode(0b11, 0b000, 0b1010, 0b0101, 0b001), // RW
    kMPAM1_EL1            = encode(0b11, 0b000, 0b1010, 0b0101, 0b000), // RW
    kMPAM1_EL12           = encode(0b11, 0b101, 0b1010, 0b0101, 0b000), // RW
    kMPAM2_EL2            = encode(0b11, 0b100, 0b1010, 0b0101, 0b000), // RW
    kMPAM3_EL3            = encode(0b11, 0b110, 0b1010, 0b0101, 0b000), // RW
    kMPAMHCR_EL2          = encode(0b11, 0b100, 0b1010, 0b0100, 0b000), // RW
    kMPAMIDR_EL1          = encode(0b11, 0b000, 0b1010, 0b0100, 0b100), // RO
    kMPAMVPM0_EL2         = encode(0b11, 0b100, 0b1010, 0b0110, 0b000), // RW
    kMPAMVPM1_EL2         = encode(0b11, 0b100, 0b1010, 0b0110, 0b001), // RW
    kMPAMVPM2_EL2         = encode(0b11, 0b100, 0b1010, 0b0110, 0b010), // RW
    kMPAMVPM3_EL2         = encode(0b11, 0b100, 0b1010, 0b0110, 0b011), // RW
    kMPAMVPM4_EL2         = encode(0b11, 0b100, 0b1010, 0b0110, 0b100), // RW
    kMPAMVPM5_EL2         = encode(0b11, 0b100, 0b1010, 0b0110, 0b101), // RW
    kMPAMVPM6_EL2         = encode(0b11, 0b100, 0b1010, 0b0110, 0b110), // RW
    kMPAMVPM7_EL2         = encode(0b11, 0b100, 0b1010, 0b0110, 0b111), // RW
    kMPAMVPMV_EL2         = encode(0b11, 0b100, 0b1010, 0b0100, 0b001), // RW
    kMPIDR_EL1            = encode(0b11, 0b000, 0b0000, 0b0000, 0b101), // RO
    kMVFR0_EL1            = encode(0b11, 0b000, 0b0000, 0b0011, 0b000), // RO
    kMVFR1_EL1            = encode(0b11, 0b000, 0b0000, 0b0011, 0b001), // RO
    kMVFR2_EL1            = encode(0b11, 0b000, 0b0000, 0b0011, 0b010), // RO
    kNZCV                 = encode(0b11, 0b011, 0b0100, 0b0010, 0b000), // RW
    kOSDLR_EL1            = encode(0b10, 0b000, 0b0001, 0b0011, 0b100), // RW
    kOSDTRRX_EL1          = encode(0b10, 0b000, 0b0000, 0b0000, 0b010), // RW
    kOSDTRTX_EL1          = encode(0b10, 0b000, 0b0000, 0b0011, 0b010), // RW
    kOSECCR_EL1           = encode(0b10, 0b000, 0b0000, 0b0110, 0b010), // RW
    kOSLAR_EL1            = encode(0b10, 0b000, 0b0001, 0b0000, 0b100), // WO
    kOSLSR_EL1            = encode(0b10, 0b000, 0b0001, 0b0001, 0b100), // RO
    kPAN                  = encode(0b11, 0b000, 0b0100, 0b0010, 0b011), // RW
    kPAR_EL1              = encode(0b11, 0b000, 0b0111, 0b0100, 0b000), // RW
    kPMBIDR_EL1           = encode(0b11, 0b000, 0b1001, 0b1010, 0b111), // RO
    kPMBLIMITR_EL1        = encode(0b11, 0b000, 0b1001, 0b1010, 0b000), // RW
    kPMBPTR_EL1           = encode(0b11, 0b000, 0b1001, 0b1010, 0b001), // RW
    kPMBSR_EL1            = encode(0b11, 0b000, 0b1001, 0b1010, 0b011), // RW
    kPMCCFILTR_EL0        = encode(0b11, 0b011, 0b1110, 0b1111, 0b111), // RW
    kPMCCNTR_EL0          = encode(0b11, 0b011, 0b1001, 0b1101, 0b000), // RW
    kPMCEID0_EL0          = encode(0b11, 0b011, 0b1001, 0b1100, 0b110), // RO
    kPMCEID1_EL0          = encode(0b11, 0b011, 0b1001, 0b1100, 0b111), // RO
    kPMCNTENCLR_EL0       = encode(0b11, 0b011, 0b1001, 0b1100, 0b010), // RW
    kPMCNTENSET_EL0       = encode(0b11, 0b011, 0b1001, 0b1100, 0b001), // RW
    kPMCR_EL0             = encode(0b11, 0b011, 0b1001, 0b1100, 0b000), // RW
    kPMEVCNTR0_EL0        = encode(0b11, 0b011, 0b1110, 0b1000, 0b000), // RW
    kPMEVCNTR10_EL0       = encode(0b11, 0b011, 0b1110, 0b1001, 0b010), // RW
    kPMEVCNTR11_EL0       = encode(0b11, 0b011, 0b1110, 0b1001, 0b011), // RW
    kPMEVCNTR12_EL0       = encode(0b11, 0b011, 0b1110, 0b1001, 0b100), // RW
    kPMEVCNTR13_EL0       = encode(0b11, 0b011, 0b1110, 0b1001, 0b101), // RW
    kPMEVCNTR14_EL0       = encode(0b11, 0b011, 0b1110, 0b1001, 0b110), // RW
    kPMEVCNTR15_EL0       = encode(0b11, 0b011, 0b1110, 0b1001, 0b111), // RW
    kPMEVCNTR16_EL0       = encode(0b11, 0b011, 0b1110, 0b1010, 0b000), // RW
    kPMEVCNTR17_EL0       = encode(0b11, 0b011, 0b1110, 0b1010, 0b001), // RW
    kPMEVCNTR18_EL0       = encode(0b11, 0b011, 0b1110, 0b1010, 0b010), // RW
    kPMEVCNTR19_EL0       = encode(0b11, 0b011, 0b1110, 0b1010, 0b011), // RW
    kPMEVCNTR1_EL0        = encode(0b11, 0b011, 0b1110, 0b1000, 0b001), // RW
    kPMEVCNTR20_EL0       = encode(0b11, 0b011, 0b1110, 0b1010, 0b100), // RW
    kPMEVCNTR21_EL0       = encode(0b11, 0b011, 0b1110, 0b1010, 0b101), // RW
    kPMEVCNTR22_EL0       = encode(0b11, 0b011, 0b1110, 0b1010, 0b110), // RW
    kPMEVCNTR23_EL0       = encode(0b11, 0b011, 0b1110, 0b1010, 0b111), // RW
    kPMEVCNTR24_EL0       = encode(0b11, 0b011, 0b1110, 0b1011, 0b000), // RW
    kPMEVCNTR25_EL0       = encode(0b11, 0b011, 0b1110, 0b1011, 0b001), // RW
    kPMEVCNTR26_EL0       = encode(0b11, 0b011, 0b1110, 0b1011, 0b010), // RW
    kPMEVCNTR27_EL0       = encode(0b11, 0b011, 0b1110, 0b1011, 0b011), // RW
    kPMEVCNTR28_EL0       = encode(0b11, 0b011, 0b1110, 0b1011, 0b100), // RW
    kPMEVCNTR29_EL0       = encode(0b11, 0b011, 0b1110, 0b1011, 0b101), // RW
    kPMEVCNTR2_EL0        = encode(0b11, 0b011, 0b1110, 0b1000, 0b010), // RW
    kPMEVCNTR30_EL0       = encode(0b11, 0b011, 0b1110, 0b1011, 0b110), // RW
    kPMEVCNTR3_EL0        = encode(0b11, 0b011, 0b1110, 0b1000, 0b011), // RW
    kPMEVCNTR4_EL0        = encode(0b11, 0b011, 0b1110, 0b1000, 0b100), // RW
    kPMEVCNTR5_EL0        = encode(0b11, 0b011, 0b1110, 0b1000, 0b101), // RW
    kPMEVCNTR6_EL0        = encode(0b11, 0b011, 0b1110, 0b1000, 0b110), // RW
    kPMEVCNTR7_EL0        = encode(0b11, 0b011, 0b1110, 0b1000, 0b111), // RW
    kPMEVCNTR8_EL0        = encode(0b11, 0b011, 0b1110, 0b1001, 0b000), // RW
    kPMEVCNTR9_EL0        = encode(0b11, 0b011, 0b1110, 0b1001, 0b001), // RW
    kPMEVTYPER0_EL0       = encode(0b11, 0b011, 0b1110, 0b1100, 0b000), // RW
    kPMEVTYPER10_EL0      = encode(0b11, 0b011, 0b1110, 0b1101, 0b010), // RW
    kPMEVTYPER11_EL0      = encode(0b11, 0b011, 0b1110, 0b1101, 0b011), // RW
    kPMEVTYPER12_EL0      = encode(0b11, 0b011, 0b1110, 0b1101, 0b100), // RW
    kPMEVTYPER13_EL0      = encode(0b11, 0b011, 0b1110, 0b1101, 0b101), // RW
    kPMEVTYPER14_EL0      = encode(0b11, 0b011, 0b1110, 0b1101, 0b110), // RW
    kPMEVTYPER15_EL0      = encode(0b11, 0b011, 0b1110, 0b1101, 0b111), // RW
    kPMEVTYPER16_EL0      = encode(0b11, 0b011, 0b1110, 0b1110, 0b000), // RW
    kPMEVTYPER17_EL0      = encode(0b11, 0b011, 0b1110, 0b1110, 0b001), // RW
    kPMEVTYPER18_EL0      = encode(0b11, 0b011, 0b1110, 0b1110, 0b010), // RW
    kPMEVTYPER19_EL0      = encode(0b11, 0b011, 0b1110, 0b1110, 0b011), // RW
    kPMEVTYPER1_EL0       = encode(0b11, 0b011, 0b1110, 0b1100, 0b001), // RW
    kPMEVTYPER20_EL0      = encode(0b11, 0b011, 0b1110, 0b1110, 0b100), // RW
    kPMEVTYPER21_EL0      = encode(0b11, 0b011, 0b1110, 0b1110, 0b101), // RW
    kPMEVTYPER22_EL0      = encode(0b11, 0b011, 0b1110, 0b1110, 0b110), // RW
    kPMEVTYPER23_EL0      = encode(0b11, 0b011, 0b1110, 0b1110, 0b111), // RW
    kPMEVTYPER24_EL0      = encode(0b11, 0b011, 0b1110, 0b1111, 0b000), // RW
    kPMEVTYPER25_EL0      = encode(0b11, 0b011, 0b1110, 0b1111, 0b001), // RW
    kPMEVTYPER26_EL0      = encode(0b11, 0b011, 0b1110, 0b1111, 0b010), // RW
    kPMEVTYPER27_EL0      = encode(0b11, 0b011, 0b1110, 0b1111, 0b011), // RW
    kPMEVTYPER28_EL0      = encode(0b11, 0b011, 0b1110, 0b1111, 0b100), // RW
    kPMEVTYPER29_EL0      = encode(0b11, 0b011, 0b1110, 0b1111, 0b101), // RW
    kPMEVTYPER2_EL0       = encode(0b11, 0b011, 0b1110, 0b1100, 0b010), // RW
    kPMEVTYPER30_EL0      = encode(0b11, 0b011, 0b1110, 0b1111, 0b110), // RW
    kPMEVTYPER3_EL0       = encode(0b11, 0b011, 0b1110, 0b1100, 0b011), // RW
    kPMEVTYPER4_EL0       = encode(0b11, 0b011, 0b1110, 0b1100, 0b100), // RW
    kPMEVTYPER5_EL0       = encode(0b11, 0b011, 0b1110, 0b1100, 0b101), // RW
    kPMEVTYPER6_EL0       = encode(0b11, 0b011, 0b1110, 0b1100, 0b110), // RW
    kPMEVTYPER7_EL0       = encode(0b11, 0b011, 0b1110, 0b1100, 0b111), // RW
    kPMEVTYPER8_EL0       = encode(0b11, 0b011, 0b1110, 0b1101, 0b000), // RW
    kPMEVTYPER9_EL0       = encode(0b11, 0b011, 0b1110, 0b1101, 0b001), // RW
    kPMINTENCLR_EL1       = encode(0b11, 0b000, 0b1001, 0b1110, 0b010), // RW
    kPMINTENSET_EL1       = encode(0b11, 0b000, 0b1001, 0b1110, 0b001), // RW
    kPMMIR_EL1            = encode(0b11, 0b000, 0b1001, 0b1110, 0b110), // RW
    kPMOVSCLR_EL0         = encode(0b11, 0b011, 0b1001, 0b1100, 0b011), // RW
    kPMOVSSET_EL0         = encode(0b11, 0b011, 0b1001, 0b1110, 0b011), // RW
    kPMSCR_EL1            = encode(0b11, 0b000, 0b1001, 0b1001, 0b000), // RW
    kPMSCR_EL12           = encode(0b11, 0b101, 0b1001, 0b1001, 0b000), // RW
    kPMSCR_EL2            = encode(0b11, 0b100, 0b1001, 0b1001, 0b000), // RW
    kPMSELR_EL0           = encode(0b11, 0b011, 0b1001, 0b1100, 0b101), // RW
    kPMSEVFR_EL1          = encode(0b11, 0b000, 0b1001, 0b1001, 0b101), // RW
    kPMSFCR_EL1           = encode(0b11, 0b000, 0b1001, 0b1001, 0b100), // RW
    kPMSICR_EL1           = encode(0b11, 0b000, 0b1001, 0b1001, 0b010), // RW
    kPMSIDR_EL1           = encode(0b11, 0b000, 0b1001, 0b1001, 0b111), // RO
    kPMSIRR_EL1           = encode(0b11, 0b000, 0b1001, 0b1001, 0b011), // RW
    kPMSLATFR_EL1         = encode(0b11, 0b000, 0b1001, 0b1001, 0b110), // RW
    kPMSWINC_EL0          = encode(0b11, 0b011, 0b1001, 0b1100, 0b100), // WO
    kPMUSERENR_EL0        = encode(0b11, 0b011, 0b1001, 0b1110, 0b000), // RW
    kPMXEVCNTR_EL0        = encode(0b11, 0b011, 0b1001, 0b1101, 0b010), // RW
    kPMXEVTYPER_EL0       = encode(0b11, 0b011, 0b1001, 0b1101, 0b001), // RW
    kREVIDR_EL1           = encode(0b11, 0b000, 0b0000, 0b0000, 0b110), // RO
    kRGSR_EL1             = encode(0b11, 0b000, 0b0001, 0b0000, 0b101), // RW
    kRMR_EL1              = encode(0b11, 0b000, 0b1100, 0b0000, 0b010), // RW
    kRMR_EL2              = encode(0b11, 0b100, 0b1100, 0b0000, 0b010), // RW
    kRMR_EL3              = encode(0b11, 0b110, 0b1100, 0b0000, 0b010), // RW
    kRNDR                 = encode(0b11, 0b011, 0b0010, 0b0100, 0b000), // RO
    kRNDRRS               = encode(0b11, 0b011, 0b0010, 0b0100, 0b001), // RO
    kRVBAR_EL1            = encode(0b11, 0b000, 0b1100, 0b0000, 0b001), // RO
    kRVBAR_EL2            = encode(0b11, 0b100, 0b1100, 0b0000, 0b001), // RO
    kRVBAR_EL3            = encode(0b11, 0b110, 0b1100, 0b0000, 0b001), // RO
    kSCR_EL3              = encode(0b11, 0b110, 0b0001, 0b0001, 0b000), // RW
    kSCTLR_EL1            = encode(0b11, 0b000, 0b0001, 0b0000, 0b000), // RW
    kSCTLR_EL12           = encode(0b11, 0b101, 0b0001, 0b0000, 0b000), // RW
    kSCTLR_EL2            = encode(0b11, 0b100, 0b0001, 0b0000, 0b000), // RW
    kSCTLR_EL3            = encode(0b11, 0b110, 0b0001, 0b0000, 0b000), // RW
    kSCXTNUM_EL0          = encode(0b11, 0b011, 0b1101, 0b0000, 0b111), // RW
    kSCXTNUM_EL1          = encode(0b11, 0b000, 0b1101, 0b0000, 0b111), // RW
    kSCXTNUM_EL12         = encode(0b11, 0b101, 0b1101, 0b0000, 0b111), // RW
    kSCXTNUM_EL2          = encode(0b11, 0b100, 0b1101, 0b0000, 0b111), // RW
    kSCXTNUM_EL3          = encode(0b11, 0b110, 0b1101, 0b0000, 0b111), // RW
    kSDER32_EL2           = encode(0b11, 0b100, 0b0001, 0b0011, 0b001), // RW
    kSDER32_EL3           = encode(0b11, 0b110, 0b0001, 0b0001, 0b001), // RW
    kSPSR_EL1             = encode(0b11, 0b000, 0b0100, 0b0000, 0b000), // RW
    kSPSR_EL12            = encode(0b11, 0b101, 0b0100, 0b0000, 0b000), // RW
    kSPSR_EL2             = encode(0b11, 0b100, 0b0100, 0b0000, 0b000), // RW
    kSPSR_EL3             = encode(0b11, 0b110, 0b0100, 0b0000, 0b000), // RW
    kSPSR_abt             = encode(0b11, 0b100, 0b0100, 0b0011, 0b001), // RW
    kSPSR_fiq             = encode(0b11, 0b100, 0b0100, 0b0011, 0b011), // RW
    kSPSR_irq             = encode(0b11, 0b100, 0b0100, 0b0011, 0b000), // RW
    kSPSR_und             = encode(0b11, 0b100, 0b0100, 0b0011, 0b010), // RW
    kSPSel                = encode(0b11, 0b000, 0b0100, 0b0010, 0b000), // RW
    kSP_EL0               = encode(0b11, 0b000, 0b0100, 0b0001, 0b000), // RW
    kSP_EL1               = encode(0b11, 0b100, 0b0100, 0b0001, 0b000), // RW
    kSP_EL2               = encode(0b11, 0b110, 0b0100, 0b0001, 0b000), // RW
    kSSBS                 = encode(0b11, 0b011, 0b0100, 0b0010, 0b110), // RW
    kTCO                  = encode(0b11, 0b011, 0b0100, 0b0010, 0b111), // RW
    kTCR_EL1              = encode(0b11, 0b000, 0b0010, 0b0000, 0b010), // RW
    kTCR_EL12             = encode(0b11, 0b101, 0b0010, 0b0000, 0b010), // RW
    kTCR_EL2              = encode(0b11, 0b100, 0b0010, 0b0000, 0b010), // RW
    kTCR_EL3              = encode(0b11, 0b110, 0b0010, 0b0000, 0b010), // RW
    kTEECR32_EL1          = encode(0b10, 0b010, 0b0000, 0b0000, 0b000), // RW
    kTEEHBR32_EL1         = encode(0b10, 0b010, 0b0001, 0b0000, 0b000), // RW
    kTFSRE0_EL1           = encode(0b11, 0b000, 0b0101, 0b0110, 0b001), // RW
    kTFSR_EL1             = encode(0b11, 0b000, 0b0101, 0b0110, 0b000), // RW
    kTFSR_EL12            = encode(0b11, 0b101, 0b0101, 0b0110, 0b000), // RW
    kTFSR_EL2             = encode(0b11, 0b100, 0b0101, 0b0110, 0b000), // RW
    kTFSR_EL3             = encode(0b11, 0b110, 0b0101, 0b0110, 0b000), // RW
    kTPIDRRO_EL0          = encode(0b11, 0b011, 0b1101, 0b0000, 0b011), // RW
    kTPIDR_EL0            = encode(0b11, 0b011, 0b1101, 0b0000, 0b010), // RW
    kTPIDR_EL1            = encode(0b11, 0b000, 0b1101, 0b0000, 0b100), // RW
    kTPIDR_EL2            = encode(0b11, 0b100, 0b1101, 0b0000, 0b010), // RW
    kTPIDR_EL3            = encode(0b11, 0b110, 0b1101, 0b0000, 0b010), // RW
    kTRBBASER_EL1         = encode(0b11, 0b000, 0b1001, 0b1011, 0b010), // RW
    kTRBIDR_EL1           = encode(0b11, 0b000, 0b1001, 0b1011, 0b111), // RO
    kTRBLIMITR_EL1        = encode(0b11, 0b000, 0b1001, 0b1011, 0b000), // RW
    kTRBMAR_EL1           = encode(0b11, 0b000, 0b1001, 0b1011, 0b100), // RW
    kTRBPTR_EL1           = encode(0b11, 0b000, 0b1001, 0b1011, 0b001), // RW
    kTRBSR_EL1            = encode(0b11, 0b000, 0b1001, 0b1011, 0b011), // RW
    kTRBTRG_EL1           = encode(0b11, 0b000, 0b1001, 0b1011, 0b110), // RW
    kTRCACATR0            = encode(0b10, 0b001, 0b0010, 0b0000, 0b010), // RW
    kTRCACATR1            = encode(0b10, 0b001, 0b0010, 0b0010, 0b010), // RW
    kTRCACATR10           = encode(0b10, 0b001, 0b0010, 0b0100, 0b011), // RW
    kTRCACATR11           = encode(0b10, 0b001, 0b0010, 0b0110, 0b011), // RW
    kTRCACATR12           = encode(0b10, 0b001, 0b0010, 0b1000, 0b011), // RW
    kTRCACATR13           = encode(0b10, 0b001, 0b0010, 0b1010, 0b011), // RW
    kTRCACATR14           = encode(0b10, 0b001, 0b0010, 0b1100, 0b011), // RW
    kTRCACATR15           = encode(0b10, 0b001, 0b0010, 0b1110, 0b011), // RW
    kTRCACATR2            = encode(0b10, 0b001, 0b0010, 0b0100, 0b010), // RW
    kTRCACATR3            = encode(0b10, 0b001, 0b0010, 0b0110, 0b010), // RW
    kTRCACATR4            = encode(0b10, 0b001, 0b0010, 0b1000, 0b010), // RW
    kTRCACATR5            = encode(0b10, 0b001, 0b0010, 0b1010, 0b010), // RW
    kTRCACATR6            = encode(0b10, 0b001, 0b0010, 0b1100, 0b010), // RW
    kTRCACATR7            = encode(0b10, 0b001, 0b0010, 0b1110, 0b010), // RW
    kTRCACATR8            = encode(0b10, 0b001, 0b0010, 0b0000, 0b011), // RW
    kTRCACATR9            = encode(0b10, 0b001, 0b0010, 0b0010, 0b011), // RW
    kTRCACVR0             = encode(0b10, 0b001, 0b0010, 0b0000, 0b000), // RW
    kTRCACVR1             = encode(0b10, 0b001, 0b0010, 0b0010, 0b000), // RW
    kTRCACVR10            = encode(0b10, 0b001, 0b0010, 0b0100, 0b001), // RW
    kTRCACVR11            = encode(0b10, 0b001, 0b0010, 0b0110, 0b001), // RW
    kTRCACVR12            = encode(0b10, 0b001, 0b0010, 0b1000, 0b001), // RW
    kTRCACVR13            = encode(0b10, 0b001, 0b0010, 0b1010, 0b001), // RW
    kTRCACVR14            = encode(0b10, 0b001, 0b0010, 0b1100, 0b001), // RW
    kTRCACVR15            = encode(0b10, 0b001, 0b0010, 0b1110, 0b001), // RW
    kTRCACVR2             = encode(0b10, 0b001, 0b0010, 0b0100, 0b000), // RW
    kTRCACVR3             = encode(0b10, 0b001, 0b0010, 0b0110, 0b000), // RW
    kTRCACVR4             = encode(0b10, 0b001, 0b0010, 0b1000, 0b000), // RW
    kTRCACVR5             = encode(0b10, 0b001, 0b0010, 0b1010, 0b000), // RW
    kTRCACVR6             = encode(0b10, 0b001, 0b0010, 0b1100, 0b000), // RW
    kTRCACVR7             = encode(0b10, 0b001, 0b0010, 0b1110, 0b000), // RW
    kTRCACVR8             = encode(0b10, 0b001, 0b0010, 0b0000, 0b001), // RW
    kTRCACVR9             = encode(0b10, 0b001, 0b0010, 0b0010, 0b001), // RW
    kTRCAUTHSTATUS        = encode(0b10, 0b001, 0b0111, 0b1110, 0b110), // RO
    kTRCAUXCTLR           = encode(0b10, 0b001, 0b0000, 0b0110, 0b000), // RW
    kTRCBBCTLR            = encode(0b10, 0b001, 0b0000, 0b1111, 0b000), // RW
    kTRCCCCTLR            = encode(0b10, 0b001, 0b0000, 0b1110, 0b000), // RW
    kTRCCIDCCTLR0         = encode(0b10, 0b001, 0b0011, 0b0000, 0b010), // RW
    kTRCCIDCCTLR1         = encode(0b10, 0b001, 0b0011, 0b0001, 0b010), // RW
    kTRCCIDCVR0           = encode(0b10, 0b001, 0b0011, 0b0000, 0b000), // RW
    kTRCCIDCVR1           = encode(0b10, 0b001, 0b0011, 0b0010, 0b000), // RW
    kTRCCIDCVR2           = encode(0b10, 0b001, 0b0011, 0b0100, 0b000), // RW
    kTRCCIDCVR3           = encode(0b10, 0b001, 0b0011, 0b0110, 0b000), // RW
    kTRCCIDCVR4           = encode(0b10, 0b001, 0b0011, 0b1000, 0b000), // RW
    kTRCCIDCVR5           = encode(0b10, 0b001, 0b0011, 0b1010, 0b000), // RW
    kTRCCIDCVR6           = encode(0b10, 0b001, 0b0011, 0b1100, 0b000), // RW
    kTRCCIDCVR7           = encode(0b10, 0b001, 0b0011, 0b1110, 0b000), // RW
    kTRCCIDR0             = encode(0b10, 0b001, 0b0111, 0b1100, 0b111), // RO
    kTRCCIDR1             = encode(0b10, 0b001, 0b0111, 0b1101, 0b111), // RO
    kTRCCIDR2             = encode(0b10, 0b001, 0b0111, 0b1110, 0b111), // RO
    kTRCCIDR3             = encode(0b10, 0b001, 0b0111, 0b1111, 0b111), // RO
    kTRCCLAIMCLR          = encode(0b10, 0b001, 0b0111, 0b1001, 0b110), // RW
    kTRCCLAIMSET          = encode(0b10, 0b001, 0b0111, 0b1000, 0b110), // RW
    kTRCCNTCTLR0          = encode(0b10, 0b001, 0b0000, 0b0100, 0b101), // RW
    kTRCCNTCTLR1          = encode(0b10, 0b001, 0b0000, 0b0101, 0b101), // RW
    kTRCCNTCTLR2          = encode(0b10, 0b001, 0b0000, 0b0110, 0b101), // RW
    kTRCCNTCTLR3          = encode(0b10, 0b001, 0b0000, 0b0111, 0b101), // RW
    kTRCCNTRLDVR0         = encode(0b10, 0b001, 0b0000, 0b0000, 0b101), // RW
    kTRCCNTRLDVR1         = encode(0b10, 0b001, 0b0000, 0b0001, 0b101), // RW
    kTRCCNTRLDVR2         = encode(0b10, 0b001, 0b0000, 0b0010, 0b101), // RW
    kTRCCNTRLDVR3         = encode(0b10, 0b001, 0b0000, 0b0011, 0b101), // RW
    kTRCCNTVR0            = encode(0b10, 0b001, 0b0000, 0b1000, 0b101), // RW
    kTRCCNTVR1            = encode(0b10, 0b001, 0b0000, 0b1001, 0b101), // RW
    kTRCCNTVR2            = encode(0b10, 0b001, 0b0000, 0b1010, 0b101), // RW
    kTRCCNTVR3            = encode(0b10, 0b001, 0b0000, 0b1011, 0b101), // RW
    kTRCCONFIGR           = encode(0b10, 0b001, 0b0000, 0b0100, 0b000), // RW
    kTRCDEVAFF0           = encode(0b10, 0b001, 0b0111, 0b1010, 0b110), // RO
    kTRCDEVAFF1           = encode(0b10, 0b001, 0b0111, 0b1011, 0b110), // RO
    kTRCDEVARCH           = encode(0b10, 0b001, 0b0111, 0b1111, 0b110), // RO
    kTRCDEVID             = encode(0b10, 0b001, 0b0111, 0b0010, 0b111), // RO
    kTRCDEVTYPE           = encode(0b10, 0b001, 0b0111, 0b0011, 0b111), // RO
    kTRCDVCMR0            = encode(0b10, 0b001, 0b0010, 0b0000, 0b110), // RW
    kTRCDVCMR1            = encode(0b10, 0b001, 0b0010, 0b0100, 0b110), // RW
    kTRCDVCMR2            = encode(0b10, 0b001, 0b0010, 0b1000, 0b110), // RW
    kTRCDVCMR3            = encode(0b10, 0b001, 0b0010, 0b1100, 0b110), // RW
    kTRCDVCMR4            = encode(0b10, 0b001, 0b0010, 0b0000, 0b111), // RW
    kTRCDVCMR5            = encode(0b10, 0b001, 0b0010, 0b0100, 0b111), // RW
    kTRCDVCMR6            = encode(0b10, 0b001, 0b0010, 0b1000, 0b111), // RW
    kTRCDVCMR7            = encode(0b10, 0b001, 0b0010, 0b1100, 0b111), // RW
    kTRCDVCVR0            = encode(0b10, 0b001, 0b0010, 0b0000, 0b100), // RW
    kTRCDVCVR1            = encode(0b10, 0b001, 0b0010, 0b0100, 0b100), // RW
    kTRCDVCVR2            = encode(0b10, 0b001, 0b0010, 0b1000, 0b100), // RW
    kTRCDVCVR3            = encode(0b10, 0b001, 0b0010, 0b1100, 0b100), // RW
    kTRCDVCVR4            = encode(0b10, 0b001, 0b0010, 0b0000, 0b101), // RW
    kTRCDVCVR5            = encode(0b10, 0b001, 0b0010, 0b0100, 0b101), // RW
    kTRCDVCVR6            = encode(0b10, 0b001, 0b0010, 0b1000, 0b101), // RW
    kTRCDVCVR7            = encode(0b10, 0b001, 0b0010, 0b1100, 0b101), // RW
    kTRCEVENTCTL0R        = encode(0b10, 0b001, 0b0000, 0b1000, 0b000), // RW
    kTRCEVENTCTL1R        = encode(0b10, 0b001, 0b0000, 0b1001, 0b000), // RW
    kTRCEXTINSELR         = encode(0b10, 0b001, 0b0000, 0b1000, 0b100), // RW
    kTRCEXTINSELR0        = encode(0b10, 0b001, 0b0000, 0b1000, 0b100), // RW
    kTRCEXTINSELR1        = encode(0b10, 0b001, 0b0000, 0b1001, 0b100), // RW
    kTRCEXTINSELR2        = encode(0b10, 0b001, 0b0000, 0b1010, 0b100), // RW
    kTRCEXTINSELR3        = encode(0b10, 0b001, 0b0000, 0b1011, 0b100), // RW
    kTRCIDR0              = encode(0b10, 0b001, 0b0000, 0b1000, 0b111), // RO
    kTRCIDR1              = encode(0b10, 0b001, 0b0000, 0b1001, 0b111), // RO
    kTRCIDR10             = encode(0b10, 0b001, 0b0000, 0b0010, 0b110), // RO
    kTRCIDR11             = encode(0b10, 0b001, 0b0000, 0b0011, 0b110), // RO
    kTRCIDR12             = encode(0b10, 0b001, 0b0000, 0b0100, 0b110), // RO
    kTRCIDR13             = encode(0b10, 0b001, 0b0000, 0b0101, 0b110), // RO
    kTRCIDR2              = encode(0b10, 0b001, 0b0000, 0b1010, 0b111), // RO
    kTRCIDR3              = encode(0b10, 0b001, 0b0000, 0b1011, 0b111), // RO
    kTRCIDR4              = encode(0b10, 0b001, 0b0000, 0b1100, 0b111), // RO
    kTRCIDR5              = encode(0b10, 0b001, 0b0000, 0b1101, 0b111), // RO
    kTRCIDR6              = encode(0b10, 0b001, 0b0000, 0b1110, 0b111), // RO
    kTRCIDR7              = encode(0b10, 0b001, 0b0000, 0b1111, 0b111), // RO
    kTRCIDR8              = encode(0b10, 0b001, 0b0000, 0b0000, 0b110), // RO
    kTRCIDR9              = encode(0b10, 0b001, 0b0000, 0b0001, 0b110), // RO
    kTRCIMSPEC0           = encode(0b10, 0b001, 0b0000, 0b0000, 0b111), // RW
    kTRCIMSPEC1           = encode(0b10, 0b001, 0b0000, 0b0001, 0b111), // RW
    kTRCIMSPEC2           = encode(0b10, 0b001, 0b0000, 0b0010, 0b111), // RW
    kTRCIMSPEC3           = encode(0b10, 0b001, 0b0000, 0b0011, 0b111), // RW
    kTRCIMSPEC4           = encode(0b10, 0b001, 0b0000, 0b0100, 0b111), // RW
    kTRCIMSPEC5           = encode(0b10, 0b001, 0b0000, 0b0101, 0b111), // RW
    kTRCIMSPEC6           = encode(0b10, 0b001, 0b0000, 0b0110, 0b111), // RW
    kTRCIMSPEC7           = encode(0b10, 0b001, 0b0000, 0b0111, 0b111), // RW
    kTRCITCTRL            = encode(0b10, 0b001, 0b0111, 0b0000, 0b100), // RW
    kTRCLAR               = encode(0b10, 0b001, 0b0111, 0b1100, 0b110), // WO
    kTRCLSR               = encode(0b10, 0b001, 0b0111, 0b1101, 0b110), // RO
    kTRCOSLAR             = encode(0b10, 0b001, 0b0001, 0b0000, 0b100), // WO
    kTRCOSLSR             = encode(0b10, 0b001, 0b0001, 0b0001, 0b100), // RO
    kTRCPDCR              = encode(0b10, 0b001, 0b0001, 0b0100, 0b100), // RW
    kTRCPDSR              = encode(0b10, 0b001, 0b0001, 0b0101, 0b100), // RO
    kTRCPIDR0             = encode(0b10, 0b001, 0b0111, 0b1000, 0b111), // RO
    kTRCPIDR1             = encode(0b10, 0b001, 0b0111, 0b1001, 0b111), // RO
    kTRCPIDR2             = encode(0b10, 0b001, 0b0111, 0b1010, 0b111), // RO
    kTRCPIDR3             = encode(0b10, 0b001, 0b0111, 0b1011, 0b111), // RO
    kTRCPIDR4             = encode(0b10, 0b001, 0b0111, 0b0100, 0b111), // RO
    kTRCPIDR5             = encode(0b10, 0b001, 0b0111, 0b0101, 0b111), // RO
    kTRCPIDR6             = encode(0b10, 0b001, 0b0111, 0b0110, 0b111), // RO
    kTRCPIDR7             = encode(0b10, 0b001, 0b0111, 0b0111, 0b111), // RO
    kTRCPRGCTLR           = encode(0b10, 0b001, 0b0000, 0b0001, 0b000), // RW
    kTRCPROCSELR          = encode(0b10, 0b001, 0b0000, 0b0010, 0b000), // RW
    kTRCQCTLR             = encode(0b10, 0b001, 0b0000, 0b0001, 0b001), // RW
    kTRCRSCTLR10          = encode(0b10, 0b001, 0b0001, 0b1010, 0b000), // RW
    kTRCRSCTLR11          = encode(0b10, 0b001, 0b0001, 0b1011, 0b000), // RW
    kTRCRSCTLR12          = encode(0b10, 0b001, 0b0001, 0b1100, 0b000), // RW
    kTRCRSCTLR13          = encode(0b10, 0b001, 0b0001, 0b1101, 0b000), // RW
    kTRCRSCTLR14          = encode(0b10, 0b001, 0b0001, 0b1110, 0b000), // RW
    kTRCRSCTLR15          = encode(0b10, 0b001, 0b0001, 0b1111, 0b000), // RW
    kTRCRSCTLR16          = encode(0b10, 0b001, 0b0001, 0b0000, 0b001), // RW
    kTRCRSCTLR17          = encode(0b10, 0b001, 0b0001, 0b0001, 0b001), // RW
    kTRCRSCTLR18          = encode(0b10, 0b001, 0b0001, 0b0010, 0b001), // RW
    kTRCRSCTLR19          = encode(0b10, 0b001, 0b0001, 0b0011, 0b001), // RW
    kTRCRSCTLR2           = encode(0b10, 0b001, 0b0001, 0b0010, 0b000), // RW
    kTRCRSCTLR20          = encode(0b10, 0b001, 0b0001, 0b0100, 0b001), // RW
    kTRCRSCTLR21          = encode(0b10, 0b001, 0b0001, 0b0101, 0b001), // RW
    kTRCRSCTLR22          = encode(0b10, 0b001, 0b0001, 0b0110, 0b001), // RW
    kTRCRSCTLR23          = encode(0b10, 0b001, 0b0001, 0b0111, 0b001), // RW
    kTRCRSCTLR24          = encode(0b10, 0b001, 0b0001, 0b1000, 0b001), // RW
    kTRCRSCTLR25          = encode(0b10, 0b001, 0b0001, 0b1001, 0b001), // RW
    kTRCRSCTLR26          = encode(0b10, 0b001, 0b0001, 0b1010, 0b001), // RW
    kTRCRSCTLR27          = encode(0b10, 0b001, 0b0001, 0b1011, 0b001), // RW
    kTRCRSCTLR28          = encode(0b10, 0b001, 0b0001, 0b1100, 0b001), // RW
    kTRCRSCTLR29          = encode(0b10, 0b001, 0b0001, 0b1101, 0b001), // RW
    kTRCRSCTLR3           = encode(0b10, 0b001, 0b0001, 0b0011, 0b000), // RW
    kTRCRSCTLR30          = encode(0b10, 0b001, 0b0001, 0b1110, 0b001), // RW
    kTRCRSCTLR31          = encode(0b10, 0b001, 0b0001, 0b1111, 0b001), // RW
    kTRCRSCTLR4           = encode(0b10, 0b001, 0b0001, 0b0100, 0b000), // RW
    kTRCRSCTLR5           = encode(0b10, 0b001, 0b0001, 0b0101, 0b000), // RW
    kTRCRSCTLR6           = encode(0b10, 0b001, 0b0001, 0b0110, 0b000), // RW
    kTRCRSCTLR7           = encode(0b10, 0b001, 0b0001, 0b0111, 0b000), // RW
    kTRCRSCTLR8           = encode(0b10, 0b001, 0b0001, 0b1000, 0b000), // RW
    kTRCRSCTLR9           = encode(0b10, 0b001, 0b0001, 0b1001, 0b000), // RW
    kTRCRSR               = encode(0b10, 0b001, 0b0000, 0b1010, 0b000), // RW
    kTRCSEQEVR0           = encode(0b10, 0b001, 0b0000, 0b0000, 0b100), // RW
    kTRCSEQEVR1           = encode(0b10, 0b001, 0b0000, 0b0001, 0b100), // RW
    kTRCSEQEVR2           = encode(0b10, 0b001, 0b0000, 0b0010, 0b100), // RW
    kTRCSEQRSTEVR         = encode(0b10, 0b001, 0b0000, 0b0110, 0b100), // RW
    kTRCSEQSTR            = encode(0b10, 0b001, 0b0000, 0b0111, 0b100), // RW
    kTRCSSCCR0            = encode(0b10, 0b001, 0b0001, 0b0000, 0b010), // RW
    kTRCSSCCR1            = encode(0b10, 0b001, 0b0001, 0b0001, 0b010), // RW
    kTRCSSCCR2            = encode(0b10, 0b001, 0b0001, 0b0010, 0b010), // RW
    kTRCSSCCR3            = encode(0b10, 0b001, 0b0001, 0b0011, 0b010), // RW
    kTRCSSCCR4            = encode(0b10, 0b001, 0b0001, 0b0100, 0b010), // RW
    kTRCSSCCR5            = encode(0b10, 0b001, 0b0001, 0b0101, 0b010), // RW
    kTRCSSCCR6            = encode(0b10, 0b001, 0b0001, 0b0110, 0b010), // RW
    kTRCSSCCR7            = encode(0b10, 0b001, 0b0001, 0b0111, 0b010), // RW
    kTRCSSCSR0            = encode(0b10, 0b001, 0b0001, 0b1000, 0b010), // RW
    kTRCSSCSR1            = encode(0b10, 0b001, 0b0001, 0b1001, 0b010), // RW
    kTRCSSCSR2            = encode(0b10, 0b001, 0b0001, 0b1010, 0b010), // RW
    kTRCSSCSR3            = encode(0b10, 0b001, 0b0001, 0b1011, 0b010), // RW
    kTRCSSCSR4            = encode(0b10, 0b001, 0b0001, 0b1100, 0b010), // RW
    kTRCSSCSR5            = encode(0b10, 0b001, 0b0001, 0b1101, 0b010), // RW
    kTRCSSCSR6            = encode(0b10, 0b001, 0b0001, 0b1110, 0b010), // RW
    kTRCSSCSR7            = encode(0b10, 0b001, 0b0001, 0b1111, 0b010), // RW
    kTRCSSPCICR0          = encode(0b10, 0b001, 0b0001, 0b0000, 0b011), // RW
    kTRCSSPCICR1          = encode(0b10, 0b001, 0b0001, 0b0001, 0b011), // RW
    kTRCSSPCICR2          = encode(0b10, 0b001, 0b0001, 0b0010, 0b011), // RW
    kTRCSSPCICR3          = encode(0b10, 0b001, 0b0001, 0b0011, 0b011), // RW
    kTRCSSPCICR4          = encode(0b10, 0b001, 0b0001, 0b0100, 0b011), // RW
    kTRCSSPCICR5          = encode(0b10, 0b001, 0b0001, 0b0101, 0b011), // RW
    kTRCSSPCICR6          = encode(0b10, 0b001, 0b0001, 0b0110, 0b011), // RW
    kTRCSSPCICR7          = encode(0b10, 0b001, 0b0001, 0b0111, 0b011), // RW
    kTRCSTALLCTLR         = encode(0b10, 0b001, 0b0000, 0b1011, 0b000), // RW
    kTRCSTATR             = encode(0b10, 0b001, 0b0000, 0b0011, 0b000), // RO
    kTRCSYNCPR            = encode(0b10, 0b001, 0b0000, 0b1101, 0b000), // RW
    kTRCTRACEIDR          = encode(0b10, 0b001, 0b0000, 0b0000, 0b001), // RW
    kTRCTSCTLR            = encode(0b10, 0b001, 0b0000, 0b1100, 0b000), // RW
    kTRCVDARCCTLR         = encode(0b10, 0b001, 0b0000, 0b1010, 0b010), // RW
    kTRCVDCTLR            = encode(0b10, 0b001, 0b0000, 0b1000, 0b010), // RW
    kTRCVDSACCTLR         = encode(0b10, 0b001, 0b0000, 0b1001, 0b010), // RW
    kTRCVICTLR            = encode(0b10, 0b001, 0b0000, 0b0000, 0b010), // RW
    kTRCVIIECTLR          = encode(0b10, 0b001, 0b0000, 0b0001, 0b010), // RW
    kTRCVIPCSSCTLR        = encode(0b10, 0b001, 0b0000, 0b0011, 0b010), // RW
    kTRCVISSCTLR          = encode(0b10, 0b001, 0b0000, 0b0010, 0b010), // RW
    kTRCVMIDCCTLR0        = encode(0b10, 0b001, 0b0011, 0b0010, 0b010), // RW
    kTRCVMIDCCTLR1        = encode(0b10, 0b001, 0b0011, 0b0011, 0b010), // RW
    kTRCVMIDCVR0          = encode(0b10, 0b001, 0b0011, 0b0000, 0b001), // RW
    kTRCVMIDCVR1          = encode(0b10, 0b001, 0b0011, 0b0010, 0b001), // RW
    kTRCVMIDCVR2          = encode(0b10, 0b001, 0b0011, 0b0100, 0b001), // RW
    kTRCVMIDCVR3          = encode(0b10, 0b001, 0b0011, 0b0110, 0b001), // RW
    kTRCVMIDCVR4          = encode(0b10, 0b001, 0b0011, 0b1000, 0b001), // RW
    kTRCVMIDCVR5          = encode(0b10, 0b001, 0b0011, 0b1010, 0b001), // RW
    kTRCVMIDCVR6          = encode(0b10, 0b001, 0b0011, 0b1100, 0b001), // RW
    kTRCVMIDCVR7          = encode(0b10, 0b001, 0b0011, 0b1110, 0b001), // RW
    kTRFCR_EL1            = encode(0b11, 0b000, 0b0001, 0b0010, 0b001), // RW
    kTRFCR_EL12           = encode(0b11, 0b101, 0b0001, 0b0010, 0b001), // RW
    kTRFCR_EL2            = encode(0b11, 0b100, 0b0001, 0b0010, 0b001), // RW
    kTTBR0_EL1            = encode(0b11, 0b000, 0b0010, 0b0000, 0b000), // RW
    kTTBR0_EL12           = encode(0b11, 0b101, 0b0010, 0b0000, 0b000), // RW
    kTTBR0_EL2            = encode(0b11, 0b100, 0b0010, 0b0000, 0b000), // RW
    kTTBR0_EL3            = encode(0b11, 0b110, 0b0010, 0b0000, 0b000), // RW
    kTTBR1_EL1            = encode(0b11, 0b000, 0b0010, 0b0000, 0b001), // RW
    kTTBR1_EL12           = encode(0b11, 0b101, 0b0010, 0b0000, 0b001), // RW
    kTTBR1_EL2            = encode(0b11, 0b100, 0b0010, 0b0000, 0b001), // RW
    kUAO                  = encode(0b11, 0b000, 0b0100, 0b0010, 0b100), // RW
    kVBAR_EL1             = encode(0b11, 0b000, 0b1100, 0b0000, 0b000), // RW
    kVBAR_EL12            = encode(0b11, 0b101, 0b1100, 0b0000, 0b000), // RW
    kVBAR_EL2             = encode(0b11, 0b100, 0b1100, 0b0000, 0b000), // RW
    kVBAR_EL3             = encode(0b11, 0b110, 0b1100, 0b0000, 0b000), // RW
    kVDISR_EL2            = encode(0b11, 0b100, 0b1100, 0b0001, 0b001), // RW
    kVMPIDR_EL2           = encode(0b11, 0b100, 0b0000, 0b0000, 0b101), // RW
    kVNCR_EL2             = encode(0b11, 0b100, 0b0010, 0b0010, 0b000), // RW
    kVPIDR_EL2            = encode(0b11, 0b100, 0b0000, 0b0000, 0b000), // RW
    kVSESR_EL2            = encode(0b11, 0b100, 0b0101, 0b0010, 0b011), // RW
    kVSTCR_EL2            = encode(0b11, 0b100, 0b0010, 0b0110, 0b010), // RW
    kVSTTBR_EL2           = encode(0b11, 0b100, 0b0010, 0b0110, 0b000), // RW
    kVTCR_EL2             = encode(0b11, 0b100, 0b0010, 0b0001, 0b010), // RW
    kVTTBR_EL2            = encode(0b11, 0b100, 0b0010, 0b0001, 0b000), // RW
    kZCR_EL1              = encode(0b11, 0b000, 0b0001, 0b0010, 0b000), // RW
    kZCR_EL12             = encode(0b11, 0b101, 0b0001, 0b0010, 0b000), // RW
    kZCR_EL2              = encode(0b11, 0b100, 0b0001, 0b0010, 0b000), // RW
    kZCR_EL3              = encode(0b11, 0b110, 0b0001, 0b0010, 0b000)  // RW
  };
};

} // {Predicate}

//! \}

ASMJIT_END_SUB_NAMESPACE

#endif // ASMJIT_ARM_A64GLOBALS_H_INCLUDED
