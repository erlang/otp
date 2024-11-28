// This file is part of AsmJit project <https://asmjit.com>
//
// See asmjit.h or LICENSE.md for license and copyright information
// SPDX-License-Identifier: Zlib

#include "../core/api-build_p.h"
#if !defined(ASMJIT_NO_AARCH32)

#include "../arm/a32instdb_p.h"

ASMJIT_BEGIN_SUB_NAMESPACE(a32)

// a32::InstDB - Id <-> Name
// =========================

#ifndef ASMJIT_NO_TEXT
// ${a32::NameData:Begin}
// ------------------- Automatically generated, do not edit -------------------
const InstNameIndex InstDB::instNameIndex = {{
  { Inst::kIdAdc          , Inst::kIdAsrs          + 1 },
  { Inst::kIdB            , Inst::kIdBxj           + 1 },
  { Inst::kIdCbnz         , Inst::kIdCsdb          + 1 },
  { Inst::kIdDbg          , Inst::kIdDsb           + 1 },
  { Inst::kIdEor          , Inst::kIdEsb           + 1 },
  { Inst::kIdNone         , Inst::kIdNone          + 1 },
  { Inst::kIdNone         , Inst::kIdNone          + 1 },
  { Inst::kIdHlt          , Inst::kIdHvc           + 1 },
  { Inst::kIdIsb          , Inst::kIdItttt         + 1 },
  { Inst::kIdNone         , Inst::kIdNone          + 1 },
  { Inst::kIdNone         , Inst::kIdNone          + 1 },
  { Inst::kIdLda          , Inst::kIdLsrs          + 1 },
  { Inst::kIdMcr          , Inst::kIdMvns          + 1 },
  { Inst::kIdNop          , Inst::kIdNop           + 1 },
  { Inst::kIdOrn          , Inst::kIdOrrs          + 1 },
  { Inst::kIdPkhbt        , Inst::kIdPush          + 1 },
  { Inst::kIdQadd         , Inst::kIdQsub8         + 1 },
  { Inst::kIdRbit         , Inst::kIdRscs          + 1 },
  { Inst::kIdSadd16       , Inst::kIdSxth          + 1 },
  { Inst::kIdTbb          , Inst::kIdTst           + 1 },
  { Inst::kIdUadd16       , Inst::kIdUxth          + 1 },
  { Inst::kIdVaba         , Inst::kIdVzip          + 1 },
  { Inst::kIdWfe          , Inst::kIdWfi           + 1 },
  { Inst::kIdNone         , Inst::kIdNone          + 1 },
  { Inst::kIdYield        , Inst::kIdYield         + 1 },
  { Inst::kIdNone         , Inst::kIdNone          + 1 }
}, uint16_t(9)};

const char InstDB::_instNameStringTable[] =
  "sha256su01h2vqrdmlaulhvqrshruncrc32cbwsha1sha1sushadd1shsub1smlalbtdxtbttsmlslsx"
  "tab1uhadd1uhsub1uqadd1uqsub1uxtab1vqdvqdmvqdmulvqmovvraddhvrsqrtevrsubhvusmsadd1"
  "8ssat1ssub1sxtb1uadd1usadausat1usub1uxtb1vrev164";


const uint32_t InstDB::_instNameIndexTable[] = {
  0x80000000, // Small ''.
  0x80000C81, // Small 'adc'.
  0x80098C81, // Small 'adcs'.
  0x80001081, // Small 'add'.
  0x80099081, // Small 'adds'.
  0x80004881, // Small 'adr'.
  0x80024CA1, // Small 'aesd'.
  0x8002CCA1, // Small 'aese'.
  0x86D4CCA1, // Small 'aesimc'.
  0x8036CCA1, // Small 'aesmc'.
  0x800011C1, // Small 'and'.
  0x800991C1, // Small 'ands'.
  0x80004A61, // Small 'asr'.
  0x8009CA61, // Small 'asrs'.
  0x80000002, // Small 'b'.
  0x80000CC2, // Small 'bfc'.
  0x800024C2, // Small 'bfi'.
  0x80000D22, // Small 'bic'.
  0x80098D22, // Small 'bics'.
  0x800A4162, // Small 'bkpt'.
  0x80000182, // Small 'bl'.
  0x80006182, // Small 'blx'.
  0x80000302, // Small 'bx'.
  0x80002B02, // Small 'bxj'.
  0x800D3843, // Small 'cbnz'.
  0x80006843, // Small 'cbz'.
  0x84814983, // Small 'clrbhb'.
  0x8182C983, // Small 'clrex'.
  0x80006983, // Small 'clz'.
  0x800039A3, // Small 'cmn'.
  0x800041A3, // Small 'cmp'.
  0x80004E03, // Small 'cps'.
  0x8044CE03, // Small 'cpsid'.
  0x8054CE03, // Small 'cpsie'.
  0x85DF0E43, // Small 'crc32b'.
  0x0000701E, // Large 'crc32cb'.
  0x1001601E, // Large 'crc32c|h'.
  0x1025601E, // Large 'crc32c|w'.
  0x91DF0E43, // Small 'crc32h'.
  0xAFDF0E43, // Small 'crc32w'.
  0x80011263, // Small 'csdb'.
  0x80001C44, // Small 'dbg'.
  0x81C9C064, // Small 'dcps1'.
  0x81D9C064, // Small 'dcps2'.
  0x81E9C064, // Small 'dcps3'.
  0x800009A4, // Small 'dmb'.
  0x80000A64, // Small 'dsb'.
  0x800049E5, // Small 'eor'.
  0x8009C9E5, // Small 'eors'.
  0x800A1645, // Small 'eret'.
  0x80000A65, // Small 'esb'.
  0x80005188, // Small 'hlt'.
  0x80000EC8, // Small 'hvc'.
  0x80000A69, // Small 'isb'.
  0x80000289, // Small 'it'.
  0x80001689, // Small 'ite'.
  0x80029689, // Small 'itee'.
  0x80529689, // Small 'iteee'.
  0x81429689, // Small 'iteet'.
  0x800A1689, // Small 'itet'.
  0x805A1689, // Small 'itete'.
  0x814A1689, // Small 'itett'.
  0x80005289, // Small 'itt'.
  0x8002D289, // Small 'itte'.
  0x8052D289, // Small 'ittee'.
  0x8142D289, // Small 'ittet'.
  0x800A5289, // Small 'ittt'.
  0x805A5289, // Small 'ittte'.
  0x814A5289, // Small 'itttt'.
  0x8000048C, // Small 'lda'.
  0x8001048C, // Small 'ldab'.
  0x8182848C, // Small 'ldaex'.
  0x8582848C, // Small 'ldaexb'.
  0x8982848C, // Small 'ldaexd'.
  0x9182848C, // Small 'ldaexh'.
  0x8004048C, // Small 'ldah'.
  0x8012348C, // Small 'ldmda'.
  0x8022348C, // Small 'ldmdb'.
  0x8014B48C, // Small 'ldmia'.
  0x8024B48C, // Small 'ldmib'.
  0x8000488C, // Small 'ldr'.
  0x8001488C, // Small 'ldrb'.
  0x8141488C, // Small 'ldrbt'.
  0x8002488C, // Small 'ldrd'.
  0x8182C88C, // Small 'ldrex'.
  0x8582C88C, // Small 'ldrexb'.
  0x8982C88C, // Small 'ldrexd'.
  0x9182C88C, // Small 'ldrexh'.
  0x8004488C, // Small 'ldrh'.
  0x8144488C, // Small 'ldrht'.
  0x8029C88C, // Small 'ldrsb'.
  0xA829C88C, // Small 'ldrsbt'.
  0x8089C88C, // Small 'ldrsh'.
  0xA889C88C, // Small 'ldrsht'.
  0x800A488C, // Small 'ldrt'.
  0x8000326C, // Small 'lsl'.
  0x8009B26C, // Small 'lsls'.
  0x80004A6C, // Small 'lsr'.
  0x8009CA6C, // Small 'lsrs'.
  0x8000486D, // Small 'mcr'.
  0x800EC86D, // Small 'mcr2'.
  0x8009486D, // Small 'mcrr'.
  0x81D9486D, // Small 'mcrr2'.
  0x8000058D, // Small 'mla'.
  0x8009858D, // Small 'mlas'.
  0x80004D8D, // Small 'mls'.
  0x800059ED, // Small 'mov'.
  0x8009D9ED, // Small 'movs'.
  0x800A59ED, // Small 'movt'.
  0x800BD9ED, // Small 'movw'.
  0x80000E4D, // Small 'mrc'.
  0x800E8E4D, // Small 'mrc2'.
  0x8001CA4D, // Small 'mrrc'.
  0x81D1CA4D, // Small 'mrrc2'.
  0x80004E4D, // Small 'mrs'.
  0x80004A6D, // Small 'msr'.
  0x800032AD, // Small 'mul'.
  0x8009B2AD, // Small 'muls'.
  0x80003ACD, // Small 'mvn'.
  0x8009BACD, // Small 'mvns'.
  0x800041EE, // Small 'nop'.
  0x80003A4F, // Small 'orn'.
  0x8009BA4F, // Small 'orns'.
  0x80004A4F, // Small 'orr'.
  0x8009CA4F, // Small 'orrs'.
  0x81412170, // Small 'pkhbt'.
  0x802A2170, // Small 'pkhtb'.
  0x80001190, // Small 'pld'.
  0x800B9190, // Small 'pldw'.
  0x80002590, // Small 'pli'.
  0x800041F0, // Small 'pop'.
  0x80214E70, // Small 'pssbb'.
  0x80044EB0, // Small 'push'.
  0x80021031, // Small 'qadd'.
  0x10055061, // Large 'qadd1|6'.
  0x10A04061, // Large 'qadd|8'.
  0x800C4C31, // Small 'qasx'.
  0x80420491, // Small 'qdadd'.
  0x802ACC91, // Small 'qdsub'.
  0x800C0671, // Small 'qsax'.
  0x80015671, // Small 'qsub'.
  0x10055067, // Large 'qsub1|6'.
  0x10A04067, // Large 'qsub|8'.
  0x800A2452, // Small 'rbit'.
  0x800058B2, // Small 'rev'.
  0x000050CA, // Large 'rev16'.
  0x8089D8B2, // Small 'revsh'.
  0x801214D2, // Small 'rfeda'.
  0x802214D2, // Small 'rfedb'.
  0x801494D2, // Small 'rfeia'.
  0x802494D2, // Small 'rfeib'.
  0x800049F2, // Small 'ror'.
  0x8009C9F2, // Small 'rors'.
  0x80006252, // Small 'rrx'.
  0x8009E252, // Small 'rrxs'.
  0x80000A72, // Small 'rsb'.
  0x80098A72, // Small 'rsbs'.
  0x80000E72, // Small 'rsc'.
  0x80098E72, // Small 'rscs'.
  0x1005509B, // Large 'sadd1|6'.
  0x10A0409B, // Large 'sadd|8'.
  0x800C4C33, // Small 'sasx'.
  0x80000053, // Small 'sb'.
  0x80000C53, // Small 'sbc'.
  0x80098C53, // Small 'sbcs'.
  0x800C1853, // Small 'sbfx'.
  0x800B2493, // Small 'sdiv'.
  0x800030B3, // Small 'sel'.
  0x88E2D0B3, // Small 'setend'.
  0x9C1850B3, // Small 'setpan'.
  0x800058B3, // Small 'sev'.
  0x800658B3, // Small 'sevl'.
  0x803E0513, // Small 'sha1c'.
  0x808E0513, // Small 'sha1h'.
  0x80DE0513, // Small 'sha1m'.
  0x810E0513, // Small 'sha1p'.
  0x30064026, // Large 'sha1|su0'.
  0x1009602A, // Large 'sha1su|1'.
  0x10016000, // Large 'sha256|h'.
  0x200A6000, // Large 'sha256|h2'.
  0x00009000, // Large 'sha256su0'.
  0x10098000, // Large 'sha256su|1'.
  0x10056030, // Large 'shadd1|6'.
  0x10A05030, // Large 'shadd|8'.
  0x81898513, // Small 'shasx'.
  0x8180CD13, // Small 'shsax'.
  0x10056036, // Large 'shsub1|6'.
  0x10A05036, // Large 'shsub|8'.
  0x80000DB3, // Small 'smc'.
  0x8420B1B3, // Small 'smlabb'.
  0xA820B1B3, // Small 'smlabt'.
  0x8040B1B3, // Small 'smlad'.
  0xB040B1B3, // Small 'smladx'.
  0x80C0B1B3, // Small 'smlal'.
  0x1024603C, // Large 'smlalb|b'.
  0x1042603C, // Large 'smlalb|t'.
  0x88C0B1B3, // Small 'smlald'.
  0x2043503C, // Large 'smlal|dx'.
  0xA6C0B1B3, // Small 'smlals'.
  0x2045503C, // Large 'smlal|tb'.
  0x2047503C, // Large 'smlal|tt'.
  0x8540B1B3, // Small 'smlatb'.
  0xA940B1B3, // Small 'smlatt'.
  0x8570B1B3, // Small 'smlawb'.
  0xA970B1B3, // Small 'smlawt'.
  0x8049B1B3, // Small 'smlsd'.
  0xB049B1B3, // Small 'smlsdx'.
  0x88C9B1B3, // Small 'smlsld'.
  0x20435049, // Large 'smlsl|dx'.
  0x801635B3, // Small 'smmla'.
  0xA41635B3, // Small 'smmlar'.
  0x813635B3, // Small 'smmls'.
  0xA53635B3, // Small 'smmlsr'.
  0x80CAB5B3, // Small 'smmul'.
  0xA4CAB5B3, // Small 'smmulr'.
  0x8040D5B3, // Small 'smuad'.
  0xB040D5B3, // Small 'smuadx'.
  0x842655B3, // Small 'smulbb'.
  0xA82655B3, // Small 'smulbt'.
  0x80C655B3, // Small 'smull'.
  0xA6C655B3, // Small 'smulls'.
  0x854655B3, // Small 'smultb'.
  0xA94655B3, // Small 'smultt'.
  0x857655B3, // Small 'smulwb'.
  0xA97655B3, // Small 'smulwt'.
  0x8049D5B3, // Small 'smusd'.
  0xB049D5B3, // Small 'smusdx'.
  0x80124E53, // Small 'srsda'.
  0x80224E53, // Small 'srsdb'.
  0x8014CE53, // Small 'srsia'.
  0x8024CE53, // Small 'srsib'.
  0x800A0673, // Small 'ssat'.
  0x100550A1, // Large 'ssat1|6'.
  0x800C0673, // Small 'ssax'.
  0x80010A73, // Small 'ssbb'.
  0x100550A6, // Large 'ssub1|6'.
  0x10A040A6, // Large 'ssub|8'.
  0x80003293, // Small 'stl'.
  0x80013293, // Small 'stlb'.
  0x8182B293, // Small 'stlex'.
  0x8582B293, // Small 'stlexb'.
  0x8982B293, // Small 'stlexd'.
  0x9182B293, // Small 'stlexh'.
  0x80043293, // Small 'stlh'.
  0x80123693, // Small 'stmda'.
  0x80223693, // Small 'stmdb'.
  0x8014B693, // Small 'stmia'.
  0x8024B693, // Small 'stmib'.
  0x80004A93, // Small 'str'.
  0x80014A93, // Small 'strb'.
  0x81414A93, // Small 'strbt'.
  0x80024A93, // Small 'strd'.
  0x8182CA93, // Small 'strex'.
  0x8582CA93, // Small 'strexb'.
  0x8982CA93, // Small 'strexd'.
  0x9182CA93, // Small 'strexh'.
  0x80044A93, // Small 'strh'.
  0x81444A93, // Small 'strht'.
  0x800A4A93, // Small 'strt'.
  0x80000AB3, // Small 'sub'.
  0x80098AB3, // Small 'subs'.
  0x80000ED3, // Small 'svc'.
  0x8020D313, // Small 'sxtab'.
  0x1005604E, // Large 'sxtab1|6'.
  0x8080D313, // Small 'sxtah'.
  0x80015313, // Small 'sxtb'.
  0x100550AB, // Large 'sxtb1|6'.
  0x80045313, // Small 'sxth'.
  0x80000854, // Small 'tbb'.
  0x80002054, // Small 'tbh'.
  0x800044B4, // Small 'teq'.
  0x80005274, // Small 'tst'.
  0x100550B0, // Large 'uadd1|6'.
  0x10A040B0, // Large 'uadd|8'.
  0x800C4C35, // Small 'uasx'.
  0x800C1855, // Small 'ubfx'.
  0x80001895, // Small 'udf'.
  0x800B2495, // Small 'udiv'.
  0x10056054, // Large 'uhadd1|6'.
  0x10A05054, // Large 'uhadd|8'.
  0x81898515, // Small 'uhasx'.
  0x8180CD15, // Small 'uhsax'.
  0x1005605A, // Large 'uhsub1|6'.
  0x10A0505A, // Large 'uhsub|8'.
  0x80C085B5, // Small 'umaal'.
  0x80C0B1B5, // Small 'umlal'.
  0xA6C0B1B5, // Small 'umlals'.
  0x80C655B5, // Small 'umull'.
  0xA6C655B5, // Small 'umulls'.
  0x10056060, // Large 'uqadd1|6'.
  0x10A05060, // Large 'uqadd|8'.
  0x81898635, // Small 'uqasx'.
  0x8180CE35, // Small 'uqsax'.
  0x10056066, // Large 'uqsub1|6'.
  0x10A05066, // Large 'uqsub|8'.
  0x10A040B5, // Large 'usad|8'.
  0x10A050B5, // Large 'usada|8'.
  0x800A0675, // Small 'usat'.
  0x100550BA, // Large 'usat1|6'.
  0x800C0675, // Small 'usax'.
  0x100550BF, // Large 'usub1|6'.
  0x10A040BF, // Large 'usub|8'.
  0x8020D315, // Small 'uxtab'.
  0x1005606C, // Large 'uxtab1|6'.
  0x8080D315, // Small 'uxtah'.
  0x80015315, // Small 'uxtb'.
  0x100550C4, // Large 'uxtb1|6'.
  0x80045315, // Small 'uxth'.
  0x80008836, // Small 'vaba'.
  0x80C08836, // Small 'vabal'.
  0x80020836, // Small 'vabd'.
  0x80C20836, // Small 'vabdl'.
  0x80098836, // Small 'vabs'.
  0x80538C36, // Small 'vacge'.
  0x81438C36, // Small 'vacgt'.
  0x80560C36, // Small 'vacle'.
  0x81460C36, // Small 'vaclt'.
  0x80021036, // Small 'vadd'.
  0x9C821036, // Small 'vaddhn'.
  0x80C21036, // Small 'vaddl'.
  0x81721036, // Small 'vaddw'.
  0x80023836, // Small 'vand'.
  0x8001A456, // Small 'vbic'.
  0x80032456, // Small 'vbif'.
  0x800A2456, // Small 'vbit'.
  0x80064C56, // Small 'vbsl'.
  0x80420476, // Small 'vcadd'.
  0x80089476, // Small 'vceq'.
  0x80029C76, // Small 'vcge'.
  0x800A1C76, // Small 'vcgt'.
  0x8002B076, // Small 'vcle'.
  0x8009B076, // Small 'vcls'.
  0x800A3076, // Small 'vclt'.
  0x800D3076, // Small 'vclz'.
  0x80163476, // Small 'vcmla'.
  0x80083476, // Small 'vcmp'.
  0x80583476, // Small 'vcmpe'.
  0x800A3876, // Small 'vcnt'.
  0x800A5876, // Small 'vcvt'.
  0x801A5876, // Small 'vcvta'.
  0x802A5876, // Small 'vcvtb'.
  0x80DA5876, // Small 'vcvtm'.
  0x80EA5876, // Small 'vcvtn'.
  0x810A5876, // Small 'vcvtp'.
  0x812A5876, // Small 'vcvtr'.
  0x814A5876, // Small 'vcvtt'.
  0x800B2496, // Small 'vdiv'.
  0x800A3C96, // Small 'vdot'.
  0x80085496, // Small 'vdup'.
  0x80093CB6, // Small 'veor'.
  0x800A60B6, // Small 'vext'.
  0x8000B4D6, // Small 'vfma'.
  0x8020B4D6, // Small 'vfmab'.
  0x80C0B4D6, // Small 'vfmal'.
  0x8140B4D6, // Small 'vfmat'.
  0x8009B4D6, // Small 'vfms'.
  0x80C9B4D6, // Small 'vfmsl'.
  0x8016B8D6, // Small 'vfnma'.
  0x8136B8D6, // Small 'vfnms'.
  0x80420516, // Small 'vhadd'.
  0x802ACD16, // Small 'vhsub'.
  0x8009B936, // Small 'vins'.
  0x814B0D56, // Small 'vjcvt'.
  0x800E1196, // Small 'vld1'.
  0x812E1196, // Small 'vld1r'.
  0x800E9196, // Small 'vld2'.
  0x812E9196, // Small 'vld2r'.
  0x800F1196, // Small 'vld3'.
  0x812F1196, // Small 'vld3r'.
  0x800F9196, // Small 'vld4'.
  0x812F9196, // Small 'vld4r'.
  0x84469196, // Small 'vldmdb'.
  0x82969196, // Small 'vldmia'.
  0x80091196, // Small 'vldr'.
  0x800C05B6, // Small 'vmax'.
  0x9AEC05B6, // Small 'vmaxnm'.
  0x800725B6, // Small 'vmin'.
  0x9AE725B6, // Small 'vminnm'.
  0x8000B1B6, // Small 'vmla'.
  0x80C0B1B6, // Small 'vmlal'.
  0x8009B1B6, // Small 'vmls'.
  0x80C9B1B6, // Small 'vmlsl'.
  0x801635B6, // Small 'vmmla'.
  0x800B3DB6, // Small 'vmov'.
  0x80CB3DB6, // Small 'vmovl'.
  0x80EB3DB6, // Small 'vmovn'.
  0x818B3DB6, // Small 'vmovx'.
  0x800655B6, // Small 'vmul'.
  0x80C655B6, // Small 'vmull'.
  0x800759B6, // Small 'vmvn'.
  0x800395D6, // Small 'vneg'.
  0x801635D6, // Small 'vnmla'.
  0x813635D6, // Small 'vnmls'.
  0x80CAB5D6, // Small 'vnmul'.
  0x800749F6, // Small 'vorn'.
  0x800949F6, // Small 'vorr'.
  0x98120616, // Small 'vpadal'.
  0x80420616, // Small 'vpadd'.
  0x98420616, // Small 'vpaddl'.
  0x8180B616, // Small 'vpmax'.
  0x80E4B616, // Small 'vpmin'.
  0x80083E16, // Small 'vpop'.
  0x8089D616, // Small 'vpush'.
  0x81310636, // Small 'vqabs'.
  0x80420636, // Small 'vqadd'.
  0x403D3072, // Large 'vqd|mlal'.
  0x404A3072, // Large 'vqd|mlsl'.
  0x30134075, // Large 'vqdm|ulh'.
  0x10116079, // Large 'vqdmul|l'.
  0x9D67B636, // Small 'vqmovn'.
  0x201C507F, // Large 'vqmov|un'.
  0x8072BA36, // Small 'vqneg'.
  0x1001700C, // Large 'vqrdmla|h'.
  0x2000600C, // Large 'vqrdml|sh'.
  0x3013500C, // Large 'vqrdm|ulh'.
  0x9889CA36, // Small 'vqrshl'.
  0x101D6016, // Large 'vqrshr|n'.
  0x00008016, // Large 'vqrshrun'.
  0x80C44E36, // Small 'vqshl'.
  0xAAC44E36, // Small 'vqshlu'.
  0x9D244E36, // Small 'vqshrn'.
  0x5019200C, // Large 'vq|shrun'.
  0x802ACE36, // Small 'vqsub'.
  0x101D6084, // Large 'vraddh|n'.
  0x8B019656, // Small 'vrecpe'.
  0xA7019656, // Small 'vrecps'.
  0x100550C9, // Large 'vrev1|6'.
  0xBBEB1656, // Small 'vrev32'.
  0x20CE40C9, // Large 'vrev|64'.
  0x8840A256, // Small 'vrhadd'.
  0x83472656, // Small 'vrinta'.
  0x9B472656, // Small 'vrintm'.
  0x9D472656, // Small 'vrintn'.
  0xA1472656, // Small 'vrintp'.
  0xA5472656, // Small 'vrintr'.
  0xB1472656, // Small 'vrintx'.
  0xB5472656, // Small 'vrintz'.
  0x80C44E56, // Small 'vrshl'.
  0x81244E56, // Small 'vrshr'.
  0x9D244E56, // Small 'vrshrn'.
  0x0000708A, // Large 'vrsqrte'.
  0x1000608A, // Large 'vrsqrt|s'.
  0x80194E56, // Small 'vrsra'.
  0x101D6091, // Large 'vrsubh|n'.
  0x81479276, // Small 'vsdot'.
  0xA2561676, // Small 'vseleq'.
  0x8A761676, // Small 'vselge'.
  0xA8761676, // Small 'vselgt'.
  0xA7661676, // Small 'vselvs'.
  0x80062276, // Small 'vshl'.
  0x80C62276, // Small 'vshll'.
  0x80092276, // Small 'vshr'.
  0x80E92276, // Small 'vshrn'.
  0x8004B276, // Small 'vsli'.
  0x82C6B676, // Small 'vsmmla'.
  0x81494676, // Small 'vsqrt'.
  0x8000CA76, // Small 'vsra'.
  0x8004CA76, // Small 'vsri'.
  0x800E5276, // Small 'vst1'.
  0x800ED276, // Small 'vst2'.
  0x800F5276, // Small 'vst3'.
  0x800FD276, // Small 'vst4'.
  0x8446D276, // Small 'vstmdb'.
  0x8296D276, // Small 'vstmia'.
  0x80095276, // Small 'vstr'.
  0x80015676, // Small 'vsub'.
  0x9C815676, // Small 'vsubhn'.
  0x80C15676, // Small 'vsubl'.
  0x81715676, // Small 'vsubw'.
  0xA8F25676, // Small 'vsudot'.
  0x80085E76, // Small 'vswp'.
  0x80060A96, // Small 'vtbl'.
  0x800C0A96, // Small 'vtbx'.
  0x80074A96, // Small 'vtrn'.
  0x800A4E96, // Small 'vtst'.
  0x814792B6, // Small 'vudot'.
  0x82C6B6B6, // Small 'vummla'.
  0xA8F24EB6, // Small 'vusdot'.
  0x30104097, // Large 'vusm|mla'.
  0x80086AB6, // Small 'vuzp'.
  0x80082756, // Small 'vzip'.
  0x800014D7, // Small 'wfe'.
  0x800024D7, // Small 'wfi'.
  0x80461539  // Small 'yield'.
};
// ----------------------------------------------------------------------------
// ${a32::NameData:End}

#endif // !ASMJIT_NO_TEXT

ASMJIT_END_SUB_NAMESPACE

#endif // !ASMJIT_NO_AARCH32