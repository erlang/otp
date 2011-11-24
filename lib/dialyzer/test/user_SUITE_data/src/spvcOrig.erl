%%%=======================================================================
%%%
%%% Test from Mats Cronqvist <mats.cronqvist@ericsson.com>. The
%%% analysis crasched due to the handling of tuples-as-funs in
%%% hipe_icode_type.erl, and it also exposed a bug when a control flow
%%% path is first analyzed and then shown to be infeasible.
%%%

-file("./spvcOrig.erl", 1).

-module(spvcOrig).

-author(qamarma).

-id('3/190 55-CNA 121 64').

-vsn('/main/Inc4/R2A/R4A/R6A/R7A/R7D/R8B/R10A/R11A/2').

-date('2004-10-26').

-export([gen_set/3,gen_set/4,connect/3,release_comp_nu/3,release_nu/3,timeout/2,restart_spvc/1,restart_multi_spvcs/1,forced_release/1,error_handler/3,get_backoff_table/2,timeout_event/1]).

-export([release_incumbent/2,switch_over/2]).

-export([call_failure/1,get_backoff_table/2]).

-file("/export/localhome/locmacr/wrk/axd_r11/ATS_CRA12002/SPVC_CNA12164/src/../../../inc/pchTables.hrl", 1).

-hrl_id('2/190 55-CNA 121 08').

-hrl_vsn('/main/Inc3/Inc4/R2A/R3A/R3B/R5A/R6A/R7A/R7D/R8B/13').

-hrl_date('2003-01-24').

-file("/export/localhome/locmacr/wrk/axd_r11/ATS_CRA12002/SPVC_CNA12164/src/../../../derived_hrl/mib/AXD301-PCH-MIB.hrl", 1).

-file("/export/localhome/locmacr/wrk/axd_r11/ATS_CRA12002/SPVC_CNA12164/src/../../../inc/pchTables.hrl", 58).

-record(pchVp, {vplEntry,
                vplLastChange,
                vplReceiveTrafficDescrIndex = 0,
                vplTransmitTrafficDescrIndex = 0,
                vplCcIdentifier,
                vplConnId,
                vplMpId,
                vplLeafId,
                vplChargingIndicator = 1,
                vplRemoteChargingInd = 1,
                vplChargablePartyIdentifier,
                vplSegmentEndPoint = 2,
                vplRowStatus,
                vplCastType = 1,
                vplConnKind = 1,
                vplServiceType = 2,
                vplEndPointData,
                vplContinuityCheck = 1,
                vplUpcNpcMode = 2,
                vplPreventInbandCc = 1,
                vplMonAisRdi = 2,
                vpcAdminStatus = 2,
                vplSpvcAutoTarget = 2,
                vplSchedulingFlag = 2,
                vplApplication,
                vplRemoteData,
                vpccAdminStatus = 2,
                vplContCheckSearch = 1,
                vplPmSearch = 1,
                vplLastBuffFlagRead,
                vplShapingMode = 1,
                vplGroupShapingId}).

-record(pchVpDb, {vplEntry,
                  vplLastChange,
                  vplReceiveTrafficDescrIndex = 0,
                  vplTransmitTrafficDescrIndex = 0,
                  vplCcIdentifier,
                  vplConnId,
                  vplMpId,
                  vplLeafId,
                  vplAttributes,
                  vplChargablePartyIdentifier,
                  vplRowStatus,
                  vplEndPointData,
                  vplApplication,
                  vplRemoteData,
                  vplLastBuffFlagRead,
                  vplShapingMode,
                  vplGroupShapingId}).

-record(pchVpExt, {vplExtEntry,
                   vplExtReceiveTdIndex,
                   vplExtTransmitTdIndex,
                   vplExtUserName = [],
                   vplExtProviderName = [],
                   vplExtUserOperator}).

-record(pchVc, {vclEntry,
                vclLastChange,
                vclReceiveTrafficDescrIndex = 0,
                vclTransmitTrafficDescrIndex = 0,
                vclCcIdentifier,
                vclConnId,
                vclMpId,
                vclLeafId,
                vclChargingIndicator = 1,
                vclRemoteChargingInd = 1,
                vclChargablePartyIdentifier,
                vclPacketDiscard = 2,
                vclSegmentEndPoint = 2,
                vclRowStatus,
                vclCastType = 1,
                vclConnKind = 1,
                vclContinuityCheck = 1,
                vclUpcNpcMode = 2,
                vclEndPointData,
                vclPreventInbandCc = 1,
                vclMonAisRdi = 2,
                vclSpvcAutoTarget = 2,
                vclSchedulingFlag = 2,
                vclApplication,
                vclRemoteData,
                vcccAdminStatus = 2,
                vclContCheckSearch = 1,
                vclPmSearch = 1,
                vclLastBuffFlagRead,
                vclChargingIfChanid,
                vclShapingMode = 1}).

-record(pchVcDb, {vclEntry,
                  vclLastChange,
                  vclReceiveTrafficDescrIndex = 0,
                  vclTransmitTrafficDescrIndex = 0,
                  vclCcIdentifier,
                  vclConnId,
                  vclMpId,
                  vclLeafId,
                  vclAttributes,
                  vclChargablePartyIdentifier,
                  vclRowStatus,
                  vclEndPointData,
                  vclApplication,
                  vclRemoteData,
                  vclLastBuffFlagRead,
                  vclChargingIfChanid,
                  vclShapingMode}).

-record(pchAtd, {tdIndex,
                 tdType,
                 tdParam1 = 0,
                 tdParam2 = 0,
                 tdParam3 = 0,
                 tdParam4 = 0,
                 tdParam5 = 0,
                 tdTrafficQoSClass = 0,
                 tdRowStatus = 1,
                 tdServiceCategory = 6,
                 tdVcCapability = 1,
                 tdName = [],
                 tdUserCounter = 0,
                 tdUser = []}).

-record(pchAbr, {abrIndex,
                 abrIcr,
                 abrTbe = 16277215,
                 abrFrtt = 0,
                 abrRdf = 11,
                 abrRif = 11,
                 abrNrm = 4,
                 abrTrm = 7,
                 abrCdf = 3,
                 abrAdtf = 50,
                 abrRowStatus = 1}).

-record(pchIndexNext, {key,
                       tdIndexNext,
                       vpccIndexNext,
                       vcccIndexNext,
                       scheduledVpCcIndexNext,
                       scheduledVcCcIndexNext}).

-record(pchSchedVpCc, {schedVpCcIndex,
                       schedVpCcTarget,
                       schedVpCcReceiveTdIndex,
                       schedVpCcTransmitTdIndex,
                       schedVpCcOpTime,
                       schedVpCcOpInd,
                       schedVpCcOpStatus,
                       schedVpCcTimerRef,
                       schedVpCcRowStatus,
                       schedVpCcErrorCode,
                       schedVpCcUserName = [],
                       schedVpCcProviderName = []}).

-record(pchVpCc, {vpccId,
                  vpccUserName = [],
                  vpccAdminStatus,
                  vpccApplication,
                  vpccProviderName = []}).

-record(pchSchedVcCc, {schedVcCcIndex,
                       schedVcCcTarget,
                       schedVcCcReceiveTdIndex,
                       schedVcCcTransmitTdIndex,
                       schedVcCcOpTime,
                       schedVcCcOpInd,
                       schedVcCcOpStatus,
                       schedVcCcTimerRef,
                       schedVcCcRowStatus,
                       schedVcCcErrorCode,
                       schedVcCcUserName = [],
                       schedVcCcProviderName = []}).

-record(pchVcCc, {vcccId,
                  vcccUserName = [],
                  vcccAdminStatus,
                  vcccApplication,
                  vcccProviderName = []}).

-record(pchSigChannels, {et_entry,
                         cp_entry,
                         sb_cp_entry,
                         membership,
                         status,
                         sb_status,
                         application = {0,[]}}).

-record(pchSigChannelExt, {et_entry,
                           user_name,
                           provider_name}).

-record(pchApplication, {key,
                         application,
                         rights}).

-record(pchCurrAlarm, {key,
                       type_of_fault,
                       fault_id}).

-record(pchIfAddress, {ifAddressEntry,
                       ifAddressRowStatus}).

-record(pchAddressToIf, {address,
                         if_index}).

-record(pchPreferences, {key,
                         if_format}).

-record(pchSigChannelCallback, {key,
                                callback,
                                function,
                                args,
                                data}).

-record(pchTermHcId, {hcId,
                      vclEntry}).

-record(pchChg, {chgEntry,
                 chgStatus}).

-record(pchCommState, {key,
                       ccid,
                       request,
                       low_cp_state,
                       high_cp_state,
                       et_side,
                       application,
                       data,
                       timestamp,
                       timer_id,
                       callback}).

-record(pchBufferedCmd, {key,
                         resource,
                         module,
                         function,
                         arguments,
                         data}).

-record(pchAnswerCh, {conn_id,
                      chg_data,
                      call_back_cp,
                      old_rtd,
                      old_ttd,
                      old_EpData,
                      action,
                      resource,
                      data,
                      fail_cause}).

-record(pchAnswerOm, {conn_id}).

-record(ccPch, {rowInd,
                admState = 2}).

-record(pchIf, {ilmiVpi = 0,
                ilmiVci = 0,
                ilmiS = 1,
                ilmiT = 5,
                ilmiK = 4,
                neighborIfName = [],
                neighborIpAddr = [0,0,0,0],
                maxVciSvc,
                overbookingFactor = {0,0},
                shapingMode = 0,
                maxVpiSvc,
                cdvtMultFactor = 100,
                scBandwidth1 = 0,
                scBandwidth2 = 0,
                scBandwidth3 = 0,
                scBandwidth4 = 0}).

-record(pchMpTemp, {key,
                    data}).

-record(pchLatestErrorCode, {key,
                             errorCode}).

-record(pchRangeTable, {node,
                        tdIndexRange,
                        vpccIndexRange,
                        vcccIndexRange}).

-record(pchIndexBitmaps, {key,
                          available,
                          bitmap}).

-record(pchLinkState, {key,
                       op_state,
                       last_change}).

-record(pchFailedVpl, {vplEntry,
                       vplLastChange}).

-record(pchFailedVcl, {vclEntry,
                       vclLastChange}).

-record(pchStatCounters, {key,
                          ingress,
                          egress}).

-record(pchEtStatTable, {index,
                         value = 0}).

-record(pchAuditResult, {key,
                         passed,
                         not_passed,
                         sizes,
                         obj_keys}).

-record(pch_fault_reqc, {fault_type,
                         fault_location}).

-record(pch_cid, {conn_id,
                  mp_id,
                  leaf_id}).

-file("./spvcOrig.erl", 207).

-file("/export/localhome/locmacr/wrk/axd_r11/ATS_CRA12002/SPVC_CNA12164/src/../../../inc/pchI.hrl", 1).

-hrl_id('52/190 55-CNA 121 08 Ux').

-hrl_vsn('/main/R6A/R7A/R7D/R8B/3').

-hrl_date('2002-10-14').

-hrl_author(uabdomo).

-record(pch_vc_rec, {ifIndex,
                     vpi,
                     vci,
                     application}).

-record(pch_vp_rec, {ifIndex,
                     vpi}).

-record(pch_td_index, {rtd_index,
                       ttd_index}).

-record(pch_td, {service_cat,
                 pcr,
                 scr,
                 mbs,
                 mcr,
                 cdvt,
                 tagging,
                 clp_significance}).

-record(pch_call_back_req, {module,
                            function,
                            user_data}).

-record(pch_chg_rec, {chg_type,
                      chg_interface,
                      chg_chan_id,
                      chg_party_name}).

-record(pch_polic_rec, {policing,
                        packet_discard}).

-record(pch_user_name_rec, {user_name}).

-record(pch_shaping_rec, {shaping}).

-record(pch_audit_callback, {mod,
                             arg}).

-file("./spvcOrig.erl", 208).

-file("/export/localhome/locmacr/wrk/axd_r11/ATS_CRA12002/SPVC_CNA12164/src/../../../inc/plc.hrl", 1).

-hrl_id('12/190 55-CNA 121 45 Ux').

-hrl_vsn('/main/R6A/R6B/R7A/R7D/R8B/R9A/R11A/4').

-hrl_date('2004-12-07').

-hrl_author(ethrba).

-record(plcQueues, {name,
                    type,
                    weight,
                    maxlength,
                    owner}).

-record(plcSettings, {flag,
                      value}).

-record(plcAlarm, {flag,
                   value}).

-file("./spvcOrig.erl", 209).

-file("/export/localhome/locmacr/wrk/axd_r11/ATS_CRA12002/SPVC_CNA12164/src/../../../inc/spvcTables.hrl", 1).

-hrl_id('10/190 55-CNA 121 64').

-hrl_vsn('/main/Inc4/R2A/R3A/R3B/R5A/R6A/R7A/R7D/R8B/4').

-hrl_date('2003-02-12').

-hrl_author(etxovp).

-record(spvcVpc, {spvcVpcEntry,
                  spvcVpcTargetAddress,
                  spvcVpcTargetSelectType,
                  spvcVpcTargetVpi,
                  spvcVpcLastReleaseCause,
                  spvcVpcLastReleaseDiagnostic,
                  spvcVpcRetryInterval = 1000,
                  spvcVpcRetryTimer = 0,
                  spvcVpcRetryThreshold = 1,
                  spvcVpcRetryFailures = 0,
                  spvcVpcRetryLimit = 15,
                  spvcVpcRowStatus,
                  spvcVpcUserName = [],
                  spvcVpcProviderName = [],
                  currentState,
                  crankBackCounter = 0,
                  spvcVpcApplication,
                  spvcRerCap = false,
                  spvcRerStatus = false}).

-record(spvcVpcOpState, {state,
                         timeOfChange}).

-record(spvcVpcPerm, {spvcVpcEntry,
                      spvcVpcTargetAddress,
                      spvcVpcTargetSelectType,
                      spvcVpcTargetVpi,
                      spvcVpcRetryInterval = 1000,
                      spvcVpcRetryThreshold = 1,
                      spvcVpcRetryLimit = 15,
                      spvcVpcRowStatus,
                      spvcVpcUserName,
                      spvcVpcProviderName,
                      spvcVpcApplication}).

-record(spvcVpcDyn, {spvcVpcEntry,
                     spvcVpcLastReleaseCause,
                     spvcVpcLastReleaseDiagnostic,
                     spvcVpcRetryTimer = 0,
                     spvcVpcRetryFailures = 0,
                     currentState,
                     crankBackCounter = 0}).

-record(spvcVcc, {spvcVccEntry,
                  spvcVccTargetAddress,
                  spvcVccTargetSelectType,
                  spvcVccTargetVpi,
                  spvcVccTargetVci,
                  spvcVccLastReleaseCause,
                  spvcVccLastReleaseDiagnostic,
                  spvcVccRetryInterval = 1000,
                  spvcVccRetryTimer = 0,
                  spvcVccRetryThreshold = 1,
                  spvcVccRetryFailures = 0,
                  spvcVccRetryLimit = 15,
                  spvcVccRowStatus,
                  spvcVccUserName = [],
                  spvcVccProviderName = [],
                  currentState,
                  crankBackCounter = 0,
                  spvcVccTargetDlci,
                  spvcVccTargetType,
                  spvcVccApplication,
                  spvcVccFrKey,
                  spvcVccTranslationMode,
                  spvcRerCap = false,
                  spvcRerStatus = false}).

-record(spvcVccOpState, {state,
                         timeOfChange}).

-record(spvcVccPerm, {spvcVccEntry,
                      spvcVccTargetAddress,
                      spvcVccTargetSelectType,
                      spvcVccTargetVpi,
                      spvcVccTargetVci,
                      spvcVccRetryInterval = 1000,
                      spvcVccRetryThreshold = 1,
                      spvcVccRetryLimit = 15,
                      spvcVccRowStatus,
                      spvcVccUserName,
                      spvcVccProviderName,
                      spvcVccTargetDlci,
                      spvcVccTargetType,
                      spvcVccApplication,
                      spvcVccFrKey,
                      spvcVccTranslationMode = 2}).

-record(spvcVccDyn, {spvcVccEntry,
                     spvcVccLastReleaseCause,
                     spvcVccLastReleaseDiagnostic,
                     spvcVccRetryTimer = 0,
                     spvcVccRetryFailures = 0,
                     currentState,
                     crankBackCounter = 0}).

-record(spvcFailures, {dummy_key,
                       spvcCallFailuresTrapEnable = 2,
                       spvcNotificationInterval = 30,
                       backoff_interval = 0.100000,
                       delay_factor = 2,
                       max_delay = 200000}).

-record(spvcCounters, {key,
                       value}).

-record(spvcEventIndicator, {dummy_key,
                             spvcTimerInd = 2,
                             spvcSendEventInd = 2}).

-record(spvcIndexNext, {dummy_key,
                        schedVccIndexNext = 1,
                        schedVpcIndexNext = 1}).

-record(spvcHcIdToTp, {hcId,
                       tpEntry}).

-record(spvcTpToHcId, {tpEntry,
                       hcId,
                       orig_number,
                       orig_vpi,
                       orig_vci,
                       orig_dlci,
                       frKey}).

-record(spvcSchedVpc, {schedVpcIndex,
                       schedVpcSource,
                       schedVpcTargetAddr,
                       schedVpcTargetSelType,
                       schedVpcTargetVpi,
                       schedVpcRetryInt,
                       schedVpcRetryThres,
                       schedVpcRetryLimit,
                       schedVpcOpTime,
                       schedVpcOpInd,
                       schedVpcOpStatus,
                       schedVpcTimerRef,
                       schedVpcRowStatus,
                       schedVpcUserName,
                       schedVpcProviderName,
                       schedVpcFaultCause,
                       schedVpcRerCap = false}).

-record(spvcSchedVcc, {schedVccIndex,
                       schedVccSource,
                       schedVccTargetAddr,
                       schedVccTargetSelType,
                       schedVccTargetVpi,
                       schedVccTargetVci,
                       schedVccRetryInt,
                       schedVccRetryThres,
                       schedVccRetryLimit,
                       schedVccOpTime,
                       schedVccOpInd,
                       schedVccOpStatus,
                       schedVccTimerRef,
                       schedVccRowStatus,
                       schedVccUserName,
                       schedVccProviderName,
                       schedVccFaultCause,
                       schedVccRerCap = false}).

-record(spvcCurrAlarm, {key,
                        fault_id,
                        data}).

-record(spvcChg, {key,
                  data}).

-record(spvcBackoff, {key,
                      delay_time,
                      flag}).

-record(spvcAutoVp, {entry,
                     lastChange,
                     receiveTrafficDescrIndex,
                     transmitTrafficDescrIndex,
                     ccIdentifier,
                     connId,
                     mpId,
                     leafId,
                     chargingIndicator = 1,
                     remoteChargingInd = 1,
                     chargablePartyIdentifier,
                     segmentEndPoint = 2,
                     rowStatus,
                     castType = 1,
                     connKind,
                     serviceType = 2,
                     endPointData,
                     continuityCheck = 1,
                     upcNpcMode = 2,
                     preventInbandCc = 1,
                     monAisRdi = 2,
                     adminStatus,
                     autoTarget = 1,
                     schedulingFlag = 2,
                     application = [],
                     remoteData,
                     vpccAdminStatus = 2,
                     contCheckSearch = 1,
                     pmSearch = 1,
                     lastBuffFlagRead,
                     shapingMode = 1,
                     groupShapingId}).

-record(spvcAutoVc, {entry,
                     lastChange,
                     receiveTrafficDescrIndex,
                     transmitTrafficDescrIndex,
                     ccIdentifier,
                     connId,
                     mpId,
                     leafId,
                     chargingIndicator = 1,
                     remoteChargingInd = 1,
                     chargablePartyIdentifier,
                     packetDiscard = 2,
                     segmentEndPoint = 2,
                     rowStatus,
                     castType = 1,
                     connKind,
                     continuityCheck = 1,
                     upcNpcMode = 2,
                     endPointData,
                     preventInbandCc = 1,
                     monAisRdi = 2,
                     autoTarget = 1,
                     schedulingFlag = 2,
                     application = [],
                     remoteData,
                     vcccAdminStatus = 2,
                     contCheckSearch = 1,
                     pmSearch = 1,
                     lastBuffFlagRead,
                     chargingIfChanid,
                     shapingMode = 1}).

-record(spvcAutoAtd, {index,
                      type,
                      param1 = 0,
                      param2 = 0,
                      param3 = 0,
                      param4 = 0,
                      param5 = 0,
                      trafficQoSClass = 0,
                      rowStatus = 1,
                      serviceCategory = 6,
                      vcCapability = 1,
                      name = [],
                      userCounter = 0}).

-record(spvcAutoAbr, {index,
                      icr,
                      tbe = 16277215,
                      frtt = 0,
                      rdf = 11,
                      rif = 11,
                      nrm = 4,
                      trm = 7,
                      cdf = 3,
                      adtf = 50,
                      rowStatus = 1}).

-record(spvcLatestErrorCode, {key,
                              errorCode}).

-record(spvcVcDyn, {vclEntry,
                    vclCcIdentifier,
                    vclConnId,
                    vclMpId,
                    vclLeafId}).

-record(spvcVpDyn, {vplEntry,
                    vplCcIdentifier,
                    vplConnId,
                    vplMpId,
                    vplLeafId}).

-record(spvcObj, {spvcEntry,
                  spvcTargetAddress,
                  spvcTargetSelectType,
                  spvcTargetVpi,
                  spvcTargetVci,
                  spvcLastReleaseCause,
                  spvcLastReleaseDiagnostic,
                  spvcRetryInterval = 1000,
                  spvcRetryTimer = 0,
                  spvcRetryThreshold = 1,
                  spvcRetryFailures = 0,
                  spvcRetryLimit = 15,
                  spvcRowStatus,
                  spvcUserName,
                  spvcProviderName,
                  currentState,
                  spvcTargetDlci,
                  spvcTargetType,
                  spvcApplication,
                  spvcFrKey,
                  spvcVccTranslationMode = 2,
                  spvcRerCap = false,
                  spvcRerStatus = false}).

-record(spvcTargetVc, {entry,
                       userName = [],
                       providerName = [],
                       opState,
                       rowStatus}).

-record(spvcTargetVp, {entry,
                       userName = [],
                       providerName = [],
                       opState,
                       rowStatus}).

-record(spvcReestablishTimer, {time,
                               timer_id,
                               module,
                               function,
                               args}).

-record(spvcRerVp, {entry,
                    rerCap,
                    rerData}).

-record(spvcRerVc, {entry,
                    rerCap,
                    rerData}).

-record(spvcHcEtStat, {key,
                       counter = 0}).

-record(spvcSaEtStat, {key,
                       counter = 0}).

-file("./spvcOrig.erl", 210).

-file("/export/localhome/locmacr/wrk/axd_r11/ATS_CRA12002/SPVC_CNA12164/src/../../../inc/spvcDefines.hrl", 1).

-hrl_id('41/190 55-CNA 121 64 Ux').

-hrl_vsn('/main/R6A/R7A/R7D/R8B/3').

-hrl_date('2003-02-21').

-hrl_author(etxhebl).

-file("./spvcOrig.erl", 211).

-file("/export/localhome/locmacr/wrk/axd_r11/ATS_CRA12002/SPVC_CNA12164/src/../../../inc/spvcFr.hrl", 1).

-hrl_id('48/190 55-CNA 121 64 Ux').

-hrl_vsn('/main/R7A/R7D/2').

-hrl_date('2001-12-06').

-hrl_author(etxhtb).

-record(spvcFr, {spvcFrEntry,
                 spvcFrAtmEntry,
                 spvcFrTargetAddress,
                 spvcFrTargetSelectType,
                 spvcFrTargetIdentifier,
                 spvcFrTargetVpi,
                 spvcFrTargetVci,
                 spvcFrAtmTranslation,
                 spvcFrLastReleaseCause,
                 spvcFrLastReleaseDiagnostic,
                 spvcFrAdminStatus,
                 spvcFrRetryInterval = 1000,
                 spvcFrRetryTimer = 0,
                 spvcFrRetryThreshold = 1,
                 spvcFrRetryFailures = 0,
                 spvcFrRetryLimit = 15,
                 spvcFrRowStatus,
                 spvcFrUserName,
                 spvcFrProviderName,
                 currentState}).

-record(spvcFrPerm, {spvcFrEntry,
                     spvcFrAtmEntry,
                     spvcFrAtmTranslation,
                     spvcFrAdminStatus,
                     spvcFrConnect}).

-record(spvcFrAddress, {addressEntry,
                        addressRowStatus}).

-record(spvcFrAddressToIf, {address,
                            if_index}).

-record(fr_end_point, {ifIndex,
                       dlci}).

-record(fr_atm_translation, {routedIp = off,
                             routedOsi = off,
                             otherRouted = off,
                             arpTranslation = off}).

-record(link_layer_core_parameters, {outgoing_max_ifs,
                                     incoming_max_ifs}).

-record(priority_and_service_class, {outgoing_transfer_priority,
                                     incoming_transfer_priority,
                                     outgoing_discard_priority,
                                     incoming_discard_priority}).

-file("./spvcOrig.erl", 212).

-file("/export/localhome/locmacr/wrk/axd_r11/ATS_CRA12002/SPVC_CNA12164/src/../../../derived_hrl/mib/AXD301-PCH-MIB.hrl", 1).

-file("./spvcOrig.erl", 213).

-file("/export/localhome/locmacr/wrk/axd_r11/ATS_CRA12002/SPVC_CNA12164/src/../../../derived_hrl/mib/AXD301-SPVC-MIB.hrl", 1).

-file("./spvcOrig.erl", 214).

-file("/export/localhome/locmacr/wrk/axd_r11/ATS_CRA12002/SPVC_CNA12164/src/../../../derived_hrl/mib/AXD301-FRSPVC-MIB.hrl", 1).

-file("./spvcOrig.erl", 215).

-file("/export/localhome/locmacr/wrk/axd_r11/ATS_CRA12002/SPVC_CNA12164/src/../../../inc/sysDefines.hrl", 1).

-hrl_id('3/190 55-CNA 121 70').

-hrl_vsn('/main/Inc3/Inc4/Inc5/R3B/R4A/R5B/R6A/R7A/R8B/2').

-hrl_date('2002-06-07').

-hrl_author(etxjotj).

-file("./spvcOrig.erl", 216).

-file("/export/localhome/locmacr/wrk/axd_r11/ATS_CRA12002/SPVC_CNA12164/src/../../../inc/hciMsg.hrl", 1).

-hrl_id('4/190 55-CNA 121 159 Ux').

-hrl_vsn('/main/R7A/R8B/10').

-hrl_date('2003-02-21').

-hrl_author(etxmexa).

-file("/export/localhome/locmacr/wrk/axd_r11/ATS_CRA12002/SPVC_CNA12164/src/../../../inc/hciComp.hrl", 1).

-hrl_id('3/190 55-CNA 121 159 Ux').

-hrl_vsn('/main/R7A/1').

-hrl_date('00-03-22').

-hrl_author(etxmexa).

-record(hci_comp_info, {required_FC = 0,
                        desired_FC = 0}).

-record(hci_comp_res, {not_supported_required_FCs,
                       not_supported_desired_FCs,
                       all_supported_FCs}).

-file("/export/localhome/locmacr/wrk/axd_r11/ATS_CRA12002/SPVC_CNA12164/src/../../../inc/hciMsg.hrl", 14).

-record(hci_add_party, {hci_cpn,
                        hci_aal,
                        hci_bhli,
                        hci_blli,
                        hci_blli_bici,
                        hci_bsco,
                        hci_epr,
                        hci_e2etd,
                        hci_noti,
                        hci_cpsa,
                        hci_clpn,
                        hci_clpsa,
                        hci_cpn_soft,
                        hci_clpn_soft,
                        hci_geidt_list = [],
                        hci_dtl_bin_list = [],
                        hci_pa_list = [],
                        hci_gat_list = [],
                        hci_data,
                        hci_prot_comp}).

-record(hci_add_party_ack, {hci_epr,
                            hci_aal,
                            hci_blli,
                            hci_blli_bici,
                            hci_e2etd,
                            hci_noti,
                            hci_cpn_soft,
                            hci_cnosa,
                            hci_cno,
                            hci_geidt_list = [],
                            hci_pa_list = [],
                            hci_gat_list = [],
                            hci_data,
                            hci_prot_comp}).

-record(hci_add_party_rej, {hci_cause,
                            hci_epr,
                            hci_geidt_list = [],
                            hci_cb,
                            hci_pa_list = [],
                            hci_internal_rel_info,
                            hci_gat_list = [],
                            hci_data,
                            hci_prot_comp}).

-record(hci_alerting, {hci_mci,
                       hci_unrps,
                       hci_cdpi,
                       hci_epr,
                       hci_prog_list = [],
                       hci_nbc,
                       hci_nbhlc,
                       hci_noti,
                       hci_geidt_list = [],
                       hci_pa_list = [],
                       hci_gat_list = [],
                       hci_ssie,
                       hci_data,
                       hci_prot_comp}).

-record(hci_b_resources, {hci_rem_dataB,
                          hci_vpiB,
                          hci_vciB,
                          hci_data,
                          hci_prot_comp}).

-record(hci_connect, {hci_mci,
                      hci_unrps,
                      hci_aal,
                      hci_blli,
                      hci_blli_bici,
                      hci_epr,
                      hci_atd,
                      hci_e2etd,
                      hci_noti,
                      hci_abrs,
                      hci_abra,
                      hci_nbc,
                      hci_nbhlc,
                      hci_nbllc,
                      hci_prog_list = [],
                      hci_geidt_list = [],
                      hci_eqos,
                      hci_cpn_soft,
                      hci_cnosa,
                      hci_cno,
                      hci_pa_list = [],
                      hci_gat_list = [],
                      hci_rem_dataB,
                      hci_con_dir = both,
                      hci_ssie,
                      hci_rer_services,
                      hci_rer,
                      hci_opt_traf,
                      hci_data,
                      hci_prot_comp}).

-record(hci_drop_party, {hci_cause,
                         hci_epr,
                         hci_noti,
                         hci_geidt_list = [],
                         hci_pa_list = [],
                         hci_internal_rel_info,
                         hci_gat_list = [],
                         hci_data,
                         hci_prot_comp}).

-record(hci_local_connect, {hci_rem_data,
                            hci_con_dir,
                            hci_data,
                            hci_prot_comp}).

-record(hci_local_connected, {hci_rem_data,
                              hci_con_dir,
                              hci_data,
                              hci_prot_comp}).

-record(hci_local_disconnect, {hci_discon_dir,
                               hci_data,
                               hci_prot_comp}).

-record(hci_local_disconnected, {hci_data,
                                 hci_prot_comp}).

-record(hci_notify, {hci_epr,
                     hci_noti,
                     hci_pa_list = [],
                     hci_gat_list = [],
                     hci_data,
                     hci_prot_comp}).

-record(hci_party_alerting, {hci_epr,
                             hci_noti,
                             hci_geidt_list = [],
                             hci_pa_list = [],
                             hci_gat_list = [],
                             hci_data,
                             hci_prot_comp}).

-record(hci_progress, {hci_mci,
                       hci_unrps,
                       hci_cdpi,
                       hci_prog_list = [],
                       hci_nbc,
                       hci_nbhlc,
                       hci_noti,
                       hci_pa_list = [],
                       hci_gat_list = [],
                       hci_data,
                       hci_prot_comp}).

-record(hci_release, {hci_mci,
                      hci_unrps,
                      hci_cause_list = [],
                      hci_noti,
                      hci_prog_list = [],
                      hci_geidt_list = [],
                      hci_cb,
                      hci_pa_list = [],
                      hci_internal_rel_info,
                      hci_gat_list = [],
                      hci_ssie,
                      hci_rer_cause,
                      hci_data,
                      hci_prot_comp,
                      hci_internal_dbg_cc,
                      hci_internal_dbg_l3}).

-record(hci_setup, {hci_mci,
                    hci_unrps,
                    hci_atd,
                    hci_bbc,
                    hci_qos,
                    hci_cpn,
                    hci_aal,
                    hci_bhli,
                    hci_blli_brep,
                    hci_blli_bici,
                    hci_bsco,
                    hci_epr,
                    hci_lpt,
                    hci_e2etd,
                    hci_noti,
                    hci_abrs,
                    hci_abra,
                    hci_prog_list = [],
                    hci_eqos,
                    hci_cpsa_list = [],
                    hci_clpn,
                    hci_bici_clpn,
                    hci_clpsa_list = [],
                    hci_cgpc,
                    hci_nbc_brep,
                    hci_nbhlc_list = [],
                    hci_nbllc_brep,
                    hci_conss,
                    hci_geidt_list = [],
                    hci_cpn_soft,
                    hci_clpn_soft,
                    hci_dtl_bin_list = [],
                    hci_pa_list = [],
                    hci_ncci,
                    hci_routing_address,
                    hci_protocol_internal_info,
                    hci_gat_list = [],
                    hci_con_dir = both,
                    hci_ssie,
                    hci_rer_services,
                    hci_rer,
                    hci_opt_traf,
                    hci_data_setup,
                    hci_prot_comp}).

-record(hci_setup_ack, {hci_assign,
                        hci_rem_dataB,
                        hci_con_dir = both,
                        hci_vpiB,
                        hci_vciB,
                        hci_data,
                        hci_prot_comp}).

-record(hci_status, {hci_state,
                     hci_data,
                     hci_prot_comp}).

-record(hci_status_enq, {hci_state,
                         hci_data,
                         hci_prot_comp}).

-record(hci_remote_data, {hci_prot_type,
                          hci_data,
                          hci_dummy1,
                          hci_dummy2}).

-record(hci_unrec, {hci_mci,
                    hci_head,
                    hci_binary,
                    hci_data,
                    hci_prot_comp}).

-record(hci_atd, {hci_pci,
                  hci_apci,
                  hci_fwd_pcr_clp_0,
                  hci_bwd_pcr_clp_0,
                  hci_fwd_pcr_clp_0_1,
                  hci_bwd_pcr_clp_0_1,
                  hci_fwd_scr_clp_0,
                  hci_bwd_scr_clp_0,
                  hci_fwd_scr_clp_0_1,
                  hci_bwd_scr_clp_0_1,
                  hci_fwd_mbs_clp_0,
                  hci_bwd_mbs_clp_0,
                  hci_fwd_mbs_clp_0_1,
                  hci_bwd_mbs_clp_0_1,
                  hci_best_effort_ind = 0,
                  hci_fwd_frame_discard = 0,
                  hci_bwd_frame_discard = 0,
                  hci_tagging_bwd = 0,
                  hci_tagging_fwd = 0,
                  hci_fwd_abr_mcr,
                  hci_bwd_abr_mcr,
                  hci_binary}).

-record(hci_bbc, {hci_pci,
                  hci_bearer_class,
                  hci_atm_transfer_capability,
                  hci_user_plane_connection_configuration,
                  hci_susceptibility_to_clipping,
                  hci_binary}).

-record(hci_cause, {hci_pci,
                    hci_location,
                    hci_cause_value,
                    hci_diagnostics_list = [],
                    hci_binary}).

-record(hci_cpn, {hci_pci,
                  hci_type_of_number,
                  hci_intern_netw_numb_indic,
                  hci_numbering_plan_indicator,
                  hci_number_digits,
                  hci_orig_native = false}).

-record(hci_clpn, {hci_pci,
                   hci_type_of_number,
                   hci_numbering_plan_indicator,
                   hci_presentation_indicator,
                   hci_screening_indicator,
                   hci_number_digits,
                   hci_incomplete_indicator = 0,
                   hci_binary}).

-record(hci_cno, {hci_type_of_number,
                  hci_numbering_plan_indicator,
                  hci_presentation_indicator,
                  hci_screening_indicator,
                  hci_number_digits,
                  hci_binary}).

-record(hci_cnosa, {hci_binary}).

-record(hci_cpn_soft, {hci_select_type,
                       hci_soft_vpi,
                       hci_soft_vci,
                       hci_soft_dlci,
                       hci_binary}).

-record(hci_clpn_soft, {hci_soft_vpi,
                        hci_soft_vci,
                        hci_soft_dlci,
                        hci_binary}).

-record(hci_rer_services, {hci_inter_req_hard,
                           hci_inter_cap_hard,
                           hci_intra_req_soft,
                           hci_intra_req_hard,
                           hci_intra_cap_asym,
                           hci_intra_cap_sym,
                           hci_intra_cap_hard,
                           hci_binary}).

-record(hci_rer, {hci_func_addr,
                  hci_endpoint_key,
                  hci_switchover,
                  hci_incarnation,
                  hci_pnni_cumul_fw_max_cell_td,
                  hci_cumul_fw_p2p_cdv,
                  hci_cumul_bw_p2p_cdv,
                  hci_binary}).

-record(hci_rer_cause, {hci_rer_rel_cause,
                        hci_binary}).

-record(hci_opt_traf, {hci_origin,
                       hci_cumul_fw_aw,
                       hci_cumul_bw_aw,
                       hci_binary}).

-record(hci_qos, {hci_pci,
                  hci_qos_class_fwd,
                  hci_qos_class_bwd,
                  hci_binary}).

-record(hci_aal, {hci_pci,
                  hci_binary}).

-record(hci_bhli, {hci_pci,
                   hci_binary}).

-record(hci_blli_brep, {hci_brep,
                        hci_blli_list = []}).

-record(hci_blli, {hci_binary}).

-record(hci_blli_bici, {hci_repeated,
                        hci_priority,
                        hci_pci,
                        hci_binary}).

-record(hci_cpsa, {hci_pci,
                   hci_binary}).

-record(hci_clpsa, {hci_pci,
                    hci_binary}).

-record(hci_gat, {hci_binary}).

-record(hci_epr, {hci_epr_type,
                  hci_epr_value,
                  hci_epr_flag,
                  hci_binary}).

-record(hci_eqos, {hci_origin,
                   hci_acc_fwd_p2p_cdv,
                   hci_acc_bwd_p2p_cdv,
                   hci_cum_fwd_p2p_cdv,
                   hci_cum_bwd_p2p_cdv,
                   hci_acc_fwd_clr,
                   hci_acc_bwd_clr,
                   hci_binary}).

-record(hci_brep, {hci_binary}).

-record(hci_bsco, {hci_binary}).

-record(hci_noti, {hci_binary}).

-record(hci_abrs, {hci_fwd_abr_icr,
                   hci_bwd_abr_icr,
                   hci_fwd_abr_tbe,
                   hci_bwd_abr_tbe,
                   hci_cum_rm_fix_round_trip,
                   hci_fwd_rif,
                   hci_bwd_rif,
                   hci_fwd_rdf,
                   hci_bwd_rdf,
                   hci_binary}).

-record(hci_abra, {hci_fwd_nrm,
                   hci_fwd_trm,
                   hci_fwd_cdf,
                   hci_fwd_atdf,
                   hci_bwd_nrm,
                   hci_bwd_trm,
                   hci_bwd_cdf,
                   hci_bwd_atdf,
                   hci_binary}).

-record(hci_prog, {hci_coding_std,
                   hci_location,
                   hci_prog_desc,
                   hci_binary}).

-record(hci_nbc_brep, {hci_brep,
                       hci_nbc_list = []}).

-record(hci_nbc, {hci_binary}).

-record(hci_nbhlc, {hci_binary}).

-record(hci_nbllc_brep, {hci_brep,
                         hci_nbllc_list = []}).

-record(hci_nbllc, {hci_binary}).

-record(hci_geidt, {hci_binary}).

-record(hci_conss, {hci_type_of_conn_scope,
                    hci_conn_scope,
                    hci_binary}).

-record(hci_e2etd, {hci_pci,
                    hci_cumul_td,
                    hci_max_td,
                    hci_pnni_cumul_td,
                    hci_pnni_accept_fwd_max_td,
                    hci_netw_gen}).

-record(hci_cdpi, {hci_pci,
                   hci_cdpci,
                   hci_cdpsi,
                   hci_binary}).

-record(hci_cgpc, {hci_pci,
                   hci_binary}).

-record(hci_lpt, {hci_pci,
                  hci_ptype}).

-record(hci_cb, {hci_cb_level,
                 hci_bl_transit_type,
                 hci_bl_node_id,
                 hci_bl_link_proc_node_id,
                 hci_bl_link_port_id,
                 hci_bl_link_succ_node_id,
                 cause_value,
                 hci_cb_diagnostics,
                 hci_binary}).

-record(hci_pa, {hci_ie_id,
                 hci_coding,
                 hci_action,
                 hci_length,
                 hci_binary,
                 hci_error_type}).

-record(hci_ncci, {hci_pci,
                   hci_ni,
                   hci_point_code,
                   hci_call_id}).

-record(hci_ssie, {hci_ssie_sas = [],
                   hci_binary}).

-record(hci_sas, {hci_sas_vsn,
                  hci_sas_transp_ind,
                  hci_sas_flow_ind,
                  hci_sas_discard,
                  hci_sas_scope,
                  hci_sas_relative_id,
                  hci_binary}).

-record(hci_data, {hci_hcid,
                   hci_sender_ifindex,
                   hci_sender_hcid}).

-record(hci_data_setup, {hci_hcidA,
                         hci_pidA,
                         hci_protA,
                         hci_protB,
                         hci_portB,
                         hci_hcidB,
                         hci_rem_dataA,
                         hci_assign,
                         hci_ifindexB,
                         hci_node_id,
                         hci_succ_node_id,
                         hci_ifindexA,
                         hci_vpiA,
                         hci_vciA,
                         hci_cpA,
                         hci_cpB}).

-record(hci_prot_comp, {hci_requiredFC = 0,
                        hci_desiredFC = 0}).

-file("./spvcOrig.erl", 217).

-file("/export/localhome/locmacr/wrk/axd_r11/ATS_CRA12002/SPVC_CNA12164/src/../../../inc/ccCd.hrl", 1).

-hrl_id('13/190 55-CNA 121 101 Ux').

-hrl_vsn('/main/R6A/R7A/R8A/R8B/8').

-hrl_date('2003-02-21').

-hrl_author(etxmexa).

-record(ccCdRR, {hcid,
                 vpi,
                 vci,
                 ifindexA,
                 call_type,
                 spvc = false,
                 reserve = yes,
                 etA,
                 destdata,
                 leafdata,
                 loopdata,
                 l3,
                 l3_loop,
                 cc}).

-record(ccCdRD, {destid,
                 loopdata,
                 cc}).

-record(ccCdRL, {leafid,
                 protTypeB,
                 loopdata,
                 l3,
                 l3_loop,
                 cc}).

-record(ccCdDD, {hcid,
                 hcidA,
                 vpi,
                 vci,
                 ifindexB,
                 portB,
                 call_type,
                 spvc = false,
                 reserve = yes,
                 protTypeA,
                 etB,
                 leafdata,
                 loopdata,
                 l3,
                 l3_loop,
                 cc}).

-record(ccCdDL, {leafid,
                 loopdata,
                 l3,
                 l3_loop,
                 cc}).

-record(ccRR, {protTypeA,
               remote_dataA,
               remote_dataB,
               chg_counters,
               sc,
               chg_decision = on,
               cc_loop}).

-record(ccRL, {hcidB,
               charging,
               cc_loop}).

-record(ccRD, {portB,
               ifindexB,
               cpB,
               vpiB,
               vciB,
               cc_loop}).

-record(ccDD, {protTypeB,
               remote_dataA,
               remote_dataB,
               ifindexA,
               cpA,
               vpiA,
               vciA,
               chg_counters,
               sc,
               chg_decision = on,
               cc_loop}).

-record(ccDL, {cc_loop}).

-record(loopRR, {vpList,
                 nodeid,
                 succ_nodeid,
                 connection_type,
                 policing,
                 delay_contrib,
                 charging = on,
                 prev_routing_data}).

-record(loopRD, {}).

-record(loopRL, {msg_rec,
                 providerName,
                 userName,
                 partyId,
                 serviceIfA,
                 serviceIdA,
                 serviceIfB,
                 serviceIdB,
                 estAw,
                 dtlLevels}).

-record(loopDD, {nodeid,
                 succ_nodeid,
                 vpList,
                 connection_type,
                 policing,
                 assign,
                 delay_contrib,
                 charging = on}).

-record(loopDL, {msg_rec,
                 providerName,
                 userName,
                 partyId,
                 serviceIfA,
                 serviceIdA,
                 serviceIfB,
                 serviceIdB}).

-record(ccLoopRR, {pidB,
                   qos,
                   atd,
                   bbc,
                   cscope,
                   e2etd,
                   eqos,
                   con_state = none,
                   con_order = both,
                   mr_flag,
                   catch_up_id,
                   cpA}).

-record(ccLoopRD, {}).

-record(ccLoopRL, {route,
                   linklist,
                   routelist,
                   failurelist = [],
                   nodeidlist,
                   cb,
                   cpn,
                   dtl,
                   routing_state,
                   assign,
                   timer_counter = 0,
                   timer_ref,
                   status_enq_ind,
                   link_CB,
                   node_CB,
                   pnnir_rlp,
                   pnni_only}).

-record(ccLoopDD, {pidA,
                   con_state = none,
                   con_order = both,
                   mr_flag,
                   catch_up_id,
                   cpB}).

-record(ccLoopDL, {timer_counter = 0,
                   timer_ref,
                   status_enq_ind}).

-file("./spvcOrig.erl", 218).

-file("/export/localhome/locmacr/built/lib/erlang/lib/snmp-4.1.2/include/STANDARD-MIB.hrl", 1).

-file("./spvcOrig.erl", 219).

error_handler({From,Tag},{M,F,Args},EXITReason) ->
    spvcLib:do_report(sccm,M,F,Args,"",EXITReason).

connect(HcId,Connect,Key) ->
    debug_disabled,
    Obj = spvcDataBase:db_read({spvcObj,Key}),
    orig_state_machine(Obj#spvcObj.currentState,connect_nu,Obj,[HcId,Connect]).

release_nu(HcId,Release,Key) ->
    debug_disabled,
    Obj = spvcDataBase:db_read({spvcObj,Key}),
    spvcDataBase:db_delete({spvcHcIdToTp,HcId}),
    orig_state_machine(Obj#spvcObj.currentState,release_nu,Obj,[HcId,Release]).

release_comp_nu(HcId,Release_comp,Key) ->
    debug_disabled,
    Obj = spvcDataBase:db_read({spvcObj,Key}),
    spvcDataBase:db_delete({spvcHcIdToTp,HcId}),
    orig_state_machine(Obj#spvcObj.currentState,release_comp_nu,Obj,[HcId,Release_comp]).

release_incumbent(HcId,Release) ->
    debug_disabled,
    release_incumbent2(spvcDataBase:db_read({spvcHcIdToTp,HcId}),Release).

release_incumbent2(SpvcHcIdToTp,Release) ->
    release_incumbent3(SpvcHcIdToTp#spvcHcIdToTp.tpEntry,Release).

release_incumbent3({orig,If,Vpi,Vci,Leaf},Release) ->
    release_incumbent4({If,Vpi,Vci,Leaf},Release);
release_incumbent3({orig,If,Vpi,Leaf},Release) ->
    release_incumbent4({If,Vpi,Leaf},Release).

release_incumbent4(TpKey,Release) ->
    Spvc = spvcDataBase:db_read({spvcObj,TpKey}),
    active = Spvc#spvcObj.currentState,
    orig_state_machine(active,release_incumbent,Spvc,[Release]).

switch_over(HcId,{If,Vpi,Vci}) ->
    Key = case {If,Vpi,Vci} of
              {If_Value,Vpi_Value,Vci_Value} when integer(Vci_Value) ->
                  {If_Value,Vpi_Value,Vci_Value,1};
              {If_Value,Vpi_Value,_} ->
                  {If_Value,Vpi_Value,1};
              {If_Value,Vpi_Value} ->
                  {If_Value,Vpi_Value,1}
          end,
    Spvc = spvcDataBase:db_read({spvcObj,Key}),
    do_switch_over(HcId,Spvc);
switch_over(HcId,{If,Vpi}) ->
    Key = case {If,Vpi,no_vc} of
              {If_Value,Vpi_Value,Vci_Value} when integer(Vci_Value) ->
                  {If_Value,Vpi_Value,Vci_Value,1};
              {If_Value,Vpi_Value,_} ->
                  {If_Value,Vpi_Value,1};
              {If_Value,Vpi_Value} ->
                  {If_Value,Vpi_Value,1}
          end,
    Spvc = spvcDataBase:db_read({spvcObj,Key}),
    do_switch_over(HcId,Spvc).

do_switch_over(HcId,Spvc) ->
    State = Spvc#spvcObj.currentState,
    orig_state_machine(State,switch_over,Spvc,[HcId]).

gen_set(Type,Row,Cols) ->
    debug_disabled,
    gen_set(Type,Row,Cols,undefined).

gen_set(Type,Row,Cols,FrKey) ->
    debug_disabled,
    case lists:keysearch(case {case Row of
                                   {_,_,_,_} ->
                                       spvcVcc;
                                   {_,_,_} ->
                                       spvcVpc;
                                   {_,_} ->
                                       spvcFr;
                                   [_,_,_,_] ->
                                       spvcVcc;
                                   [_,_,_] ->
                                       spvcVpc;
                                   [_,_] ->
                                       spvcFr
                               end,rowStatus} of
                             {spvcVcc,targetAddress} ->
                                 2;
                             {spvcVcc,selectType} ->
                                 3;
                             {spvcVcc,targetVpi} ->
                                 18;
                             {spvcVcc,targetVci} ->
                                 5;
                             {spvcVcc,releaseCause} ->
                                 6;
                             {spvcVcc,releaseDiagnostic} ->
                                 7;
                             {spvcVcc,retryInterval} ->
                                 10;
                             {spvcVcc,retryTimer} ->
                                 11;
                             {spvcVcc,retryThreshold} ->
                                 12;
                             {spvcVcc,retryFailures} ->
                                 13;
                             {spvcVcc,retryLimit} ->
                                 14;
                             {spvcVcc,rowStatus} ->
                                 15;
                             {spvcVcc,restart} ->
                                 9;
                             {spvcVcc,targetSelectType_any} ->
                                 2;
                             {spvcVcc,targetSelectType_required} ->
                                 1;
                             {spvcVpc,targetAddress} ->
                                 2;
                             {spvcVpc,selectType} ->
                                 3;
                             {spvcVpc,targetVpi} ->
                                 15;
                             {spvcVpc,releaseCause} ->
                                 5;
                             {spvcVpc,releaseDiagnostic} ->
                                 6;
                             {spvcVpc,retryInterval} ->
                                 9;
                             {spvcVpc,retryTimer} ->
                                 10;
                             {spvcVpc,retryThreshold} ->
                                 11;
                             {spvcVpc,retryFailures} ->
                                 12;
                             {spvcVpc,retryLimit} ->
                                 13;
                             {spvcVpc,rowStatus} ->
                                 14;
                             {spvcVpc,restart} ->
                                 8;
                             {spvcVpc,targetSelectType_any} ->
                                 2;
                             {spvcVpc,targetSelectType_required} ->
                                 1;
                             {spvcFr,targetAddress} ->
                                 3;
                             {spvcFr,selectType} ->
                                 5;
                             {spvcFr,identifier} ->
                                 6;
                             {spvcFr,targetVpi} ->
                                 7;
                             {spvcFr,targetVci} ->
                                 8;
                             {spvcFr,translation} ->
                                 9;
                             {spvcFr,releaseCause} ->
                                 10;
                             {spvcFr,releaseDiagnostic} ->
                                 11;
                             {spvcFr,operStatus} ->
                                 12;
                             {spvcFr,adminStatus} ->
                                 13;
                             {spvcFr,restart} ->
                                 14;
                             {spvcFr,retryInterval} ->
                                 15;
                             {spvcFr,retryTimer} ->
                                 16;
                             {spvcFr,retryThreshold} ->
                                 17;
                             {spvcFr,retryFailures} ->
                                 18;
                             {spvcFr,retryLimit} ->
                                 19;
                             {spvcFr,lastChange} ->
                                 20;
                             {spvcFr,rowStatus} ->
                                 21
                         end,1,Cols) of
        {value,{_,4}} ->
            debug_disabled,
            mnesia:dirty_update_counter(spvcHcEtStat,spvcLib:get_board(hd(Row)),1),
            case get_link_state(case Row of
                                    Row when record(Row,spvcObj) ->
                                        case Row#spvcObj.spvcEntry of
                                            {If_Value,_,_,_} ->
                                                If_Value;
                                            {If_Value,_,_} ->
                                                If_Value
                                        end;
                                    Row when record(Row,spvcVcc) ->
                                        {If_Value,_,_,_} = Row#spvcVcc.spvcVccEntry,
                                        If_Value;
                                    Row when record(Row,spvcVpc) ->
                                        {If_Value,_,_} = Row#spvcVpc.spvcVpcEntry,
                                        If_Value;
                                    Row when record(Row,spvcVpcPerm) ->
                                        {If_Value,_,_} = Row#spvcVpcPerm.spvcVpcEntry,
                                        If_Value;
                                    Row when record(Row,spvcVccPerm) ->
                                        {If_Value,_,_,_} = Row#spvcVccPerm.spvcVccEntry,
                                        If_Value;
                                    Row when record(Row,spvcTargetVc) ->
                                        {If_Value,_,_} = Row#spvcTargetVc.entry,
                                        If_Value;
                                    Row when record(Row,spvcTargetVp) ->
                                        {If_Value,_} = Row#spvcTargetVp.entry,
                                        If_Value;
                                    Row when record(Row,pchVc) ->
                                        {If_Value,_,_} = Row#pchVc.vclEntry,
                                        If_Value;
                                    Row when record(Row,pchVp) ->
                                        {If_Value,_} = Row#pchVp.vplEntry,
                                        If_Value;
                                    Row when record(Row,spvcFr) ->
                                        {If_Value,_} = Row#spvcFr.spvcFrEntry,
                                        If_Value;
                                    Row when record(Row,spvcFrPerm) ->
                                        {If_Value,_} = Row#spvcFrPerm.spvcFrEntry,
                                        If_Value;
                                    {If_Value,_,_,_} ->
                                        If_Value;
                                    {If_Value,_,_} ->
                                        If_Value;
                                    {If_Value,_} ->
                                        If_Value;
                                    [If_Value|_] ->
                                        If_Value;
                                    _ ->
                                        error
                                end) of
                disabled ->
                    orig_state_machine(null,createAndGo_disabled,[],[Row,Cols,Type,FrKey]);
                enabled ->
                    orig_state_machine(null,createAndGo_enabled,[],[Row,Cols,Type,FrKey])
            end;
        {value,{_,5}} ->
            debug_disabled,
            mnesia:dirty_update_counter(spvcHcEtStat,spvcLib:get_board(hd(Row)),1),
            orig_state_machine(null,createAndWait,[],[Row,Cols,Type,FrKey]);
        {value,{_,1}} ->
            debug_disabled,
            case spvcDataBase:db_read({spvcObj,list_to_tuple(Row)}) of
                [] ->
                    ok;
                Spvc ->
                    case get_link_state(case Row of
                                            Row when record(Row,spvcObj) ->
                                                case Row#spvcObj.spvcEntry of
                                                    {If_Value,_,_,_} ->
                                                        If_Value;
                                                    {If_Value,_,_} ->
                                                        If_Value
                                                end;
                                            Row when record(Row,spvcVcc) ->
                                                {If_Value,_,_,_} = Row#spvcVcc.spvcVccEntry,
                                                If_Value;
                                            Row when record(Row,spvcVpc) ->
                                                {If_Value,_,_} = Row#spvcVpc.spvcVpcEntry,
                                                If_Value;
                                            Row when record(Row,spvcVpcPerm) ->
                                                {If_Value,_,_} = Row#spvcVpcPerm.spvcVpcEntry,
                                                If_Value;
                                            Row when record(Row,spvcVccPerm) ->
                                                {If_Value,_,_,_} = Row#spvcVccPerm.spvcVccEntry,
                                                If_Value;
                                            Row when record(Row,spvcTargetVc) ->
                                                {If_Value,_,_} = Row#spvcTargetVc.entry,
                                                If_Value;
                                            Row when record(Row,spvcTargetVp) ->
                                                {If_Value,_} = Row#spvcTargetVp.entry,
                                                If_Value;
                                            Row when record(Row,pchVc) ->
                                                {If_Value,_,_} = Row#pchVc.vclEntry,
                                                If_Value;
                                            Row when record(Row,pchVp) ->
                                                {If_Value,_} = Row#pchVp.vplEntry,
                                                If_Value;
                                            Row when record(Row,spvcFr) ->
                                                {If_Value,_} = Row#spvcFr.spvcFrEntry,
                                                If_Value;
                                            Row when record(Row,spvcFrPerm) ->
                                                {If_Value,_} = Row#spvcFrPerm.spvcFrEntry,
                                                If_Value;
                                            {If_Value,_,_,_} ->
                                                If_Value;
                                            {If_Value,_,_} ->
                                                If_Value;
                                            {If_Value,_} ->
                                                If_Value;
                                            [If_Value|_] ->
                                                If_Value;
                                            _ ->
                                                error
                                        end) of
                        disabled ->
                            orig_state_machine(Spvc#spvcObj.currentState,activate_disabled,Spvc,Cols);
                        enabled ->
                            orig_state_machine(Spvc#spvcObj.currentState,activate_enabled,Spvc,Cols)
                    end
            end;
        {value,{_,6}} ->
            debug_disabled,
            case spvcDataBase:db_read({spvcObj,list_to_tuple(Row)}) of
                [] ->
                    ok;
                Spvc ->
                    mnesia:dirty_update_counter(spvcHcEtStat,spvcLib:get_board(hd(Row)),- 1),
                    orig_state_machine(Spvc#spvcObj.currentState,destroy,Spvc,Cols)
            end;
        {value,{_,2}} ->
            debug_disabled,
            case spvcDataBase:db_read({spvcObj,list_to_tuple(Row)}) of
                [] ->
                    mnesia:dirty_update_counter(spvcHcEtStat,spvcLib:get_board(hd(Row)),1),
                    ok;
                Spvc ->
                    orig_state_machine(Spvc#spvcObj.currentState,not_in_service,Spvc,Cols)
            end;
        false ->
            debug_disabled,
            Spvc = spvcDataBase:db_read({spvcObj,list_to_tuple(Row)}),
            CurrentState = Spvc#spvcObj.currentState,
            NewSpvc = set_attrs(Spvc,Cols),
            Restart = case {case Row of
                                {_,_,_,_} ->
                                    spvcVcc;
                                {_,_,_} ->
                                    spvcVpc;
                                {_,_} ->
                                    spvcFr;
                                [_,_,_,_] ->
                                    spvcVcc;
                                [_,_,_] ->
                                    spvcVpc;
                                [_,_] ->
                                    spvcFr
                            end,restart} of
                          {spvcVcc,targetAddress} ->
                              2;
                          {spvcVcc,selectType} ->
                              3;
                          {spvcVcc,targetVpi} ->
                              18;
                          {spvcVcc,targetVci} ->
                              5;
                          {spvcVcc,releaseCause} ->
                              6;
                          {spvcVcc,releaseDiagnostic} ->
                              7;
                          {spvcVcc,retryInterval} ->
                              10;
                          {spvcVcc,retryTimer} ->
                              11;
                          {spvcVcc,retryThreshold} ->
                              12;
                          {spvcVcc,retryFailures} ->
                              13;
                          {spvcVcc,retryLimit} ->
                              14;
                          {spvcVcc,rowStatus} ->
                              15;
                          {spvcVcc,restart} ->
                              9;
                          {spvcVcc,targetSelectType_any} ->
                              2;
                          {spvcVcc,targetSelectType_required} ->
                              1;
                          {spvcVpc,targetAddress} ->
                              2;
                          {spvcVpc,selectType} ->
                              3;
                          {spvcVpc,targetVpi} ->
                              15;
                          {spvcVpc,releaseCause} ->
                              5;
                          {spvcVpc,releaseDiagnostic} ->
                              6;
                          {spvcVpc,retryInterval} ->
                              9;
                          {spvcVpc,retryTimer} ->
                              10;
                          {spvcVpc,retryThreshold} ->
                              11;
                          {spvcVpc,retryFailures} ->
                              12;
                          {spvcVpc,retryLimit} ->
                              13;
                          {spvcVpc,rowStatus} ->
                              14;
                          {spvcVpc,restart} ->
                              8;
                          {spvcVpc,targetSelectType_any} ->
                              2;
                          {spvcVpc,targetSelectType_required} ->
                              1;
                          {spvcFr,targetAddress} ->
                              3;
                          {spvcFr,selectType} ->
                              5;
                          {spvcFr,identifier} ->
                              6;
                          {spvcFr,targetVpi} ->
                              7;
                          {spvcFr,targetVci} ->
                              8;
                          {spvcFr,translation} ->
                              9;
                          {spvcFr,releaseCause} ->
                              10;
                          {spvcFr,releaseDiagnostic} ->
                              11;
                          {spvcFr,operStatus} ->
                              12;
                          {spvcFr,adminStatus} ->
                              13;
                          {spvcFr,restart} ->
                              14;
                          {spvcFr,retryInterval} ->
                              15;
                          {spvcFr,retryTimer} ->
                              16;
                          {spvcFr,retryThreshold} ->
                              17;
                          {spvcFr,retryFailures} ->
                              18;
                          {spvcFr,retryLimit} ->
                              19;
                          {spvcFr,lastChange} ->
                              20;
                          {spvcFr,rowStatus} ->
                              21
                      end,
            case lists:keysearch(Restart,1,Cols) of
                {value,{Restart,1}} ->
                    orig_state_machine(CurrentState,restart,NewSpvc,Cols);
                _ ->
                    spvcDataBase:db_write(NewSpvc),
                    ok
            end
    end,
    {noError,0}.

restart_spvc(Key) ->
    debug_disabled,
    Spvc = spvcDataBase:db_read({spvcObj,Key}),
    handle_restart_spvc(Spvc#spvcObj.currentState,Spvc),
    ok.

handle_restart_spvc(rest_in_peace,Spvc) ->
    debug_disabled,
    rest_in_peace(restart,Spvc,undefined);
handle_restart_spvc(_,_) ->
    ok.

restart_multi_spvcs(Key) ->
    debug_disabled,
    Spvc = spvcDataBase:db_read({spvcObj,Key}),
    handle_restart_multi_spvcs(Spvc#spvcObj.currentState,Spvc),
    ok.

handle_restart_multi_spvcs(rest_in_peace,Spvc) ->
    debug_disabled,
    handle_restart_spvc(rest_in_peace,Spvc);
handle_restart_multi_spvcs(active,Spvc) ->
    debug_disabled,
    active(restart,Spvc,undefined);
handle_restart_multi_spvcs(outgoing_callproceeding,Spvc) ->
    debug_disabled,
    outgoing_callproceeding(restart,Spvc,undefined);
handle_restart_multi_spvcs(release_at_restart,Spvc) ->
    debug_disabled,
    release_at_restart(restart,Spvc,undefined);
handle_restart_multi_spvcs(wait,Spvc) ->
    debug_disabled,
    wait(restart,Spvc,undefined);
handle_restart_multi_spvcs(rest_in_peace,Spvc) ->
    debug_disabled,
    rest_in_peace(restart,Spvc,undefined);
handle_restart_multi_spvcs(_,_) ->
    ok.

orig_state_machine(null,createAndGo_enabled,Spvc,Attrs) ->
    null(createAndGo_enabled,Spvc,Attrs);
orig_state_machine(null,createAndGo_disabled,Spvc,Attrs) ->
    null(createAndGo_disabled,Spvc,Attrs);
orig_state_machine(null,createAndWait,Spvc,Attrs) ->
    null(createAndWait,Spvc,Attrs);
orig_state_machine(created,activate_disabled,Spvc,Attrs) ->
    created(activate_disabled,Spvc,Attrs);
orig_state_machine(created,activate_enabled,Spvc,Attrs) ->
    created(activate_enabled,Spvc,Attrs);
orig_state_machine(created,destroy,Spvc,Attrs) ->
    created(destroy,Spvc,Attrs);
orig_state_machine(outgoing_callproceeding,connect_nu,Spvc,Attrs) ->
    outgoing_callproceeding(connect_nu,Spvc,Attrs);
orig_state_machine(outgoing_callproceeding,destroy,Spvc,Attrs) ->
    outgoing_callproceeding(destroy,Spvc,Attrs);
orig_state_machine(outgoing_callproceeding,restart,Spvc,Attrs) ->
    outgoing_callproceeding(restart,Spvc,Attrs);
orig_state_machine(outgoing_callproceeding,release_nu,Spvc,Attrs) ->
    case get_link_state_intf(case Spvc of
                                 Spvc when record(Spvc,spvcObj) ->
                                     case Spvc#spvcObj.spvcEntry of
                                         {If_Value,_,_,_} ->
                                             If_Value;
                                         {If_Value,_,_} ->
                                             If_Value
                                     end;
                                 Spvc when record(Spvc,spvcVcc) ->
                                     {If_Value,_,_,_} = Spvc#spvcVcc.spvcVccEntry,
                                     If_Value;
                                 Spvc when record(Spvc,spvcVpc) ->
                                     {If_Value,_,_} = Spvc#spvcVpc.spvcVpcEntry,
                                     If_Value;
                                 Spvc when record(Spvc,spvcVpcPerm) ->
                                     {If_Value,_,_} = Spvc#spvcVpcPerm.spvcVpcEntry,
                                     If_Value;
                                 Spvc when record(Spvc,spvcVccPerm) ->
                                     {If_Value,_,_,_} = Spvc#spvcVccPerm.spvcVccEntry,
                                     If_Value;
                                 Spvc when record(Spvc,spvcTargetVc) ->
                                     {If_Value,_,_} = Spvc#spvcTargetVc.entry,
                                     If_Value;
                                 Spvc when record(Spvc,spvcTargetVp) ->
                                     {If_Value,_} = Spvc#spvcTargetVp.entry,
                                     If_Value;
                                 Spvc when record(Spvc,pchVc) ->
                                     {If_Value,_,_} = Spvc#pchVc.vclEntry,
                                     If_Value;
                                 Spvc when record(Spvc,pchVp) ->
                                     {If_Value,_} = Spvc#pchVp.vplEntry,
                                     If_Value;
                                 Spvc when record(Spvc,spvcFr) ->
                                     {If_Value,_} = Spvc#spvcFr.spvcFrEntry,
                                     If_Value;
                                 Spvc when record(Spvc,spvcFrPerm) ->
                                     {If_Value,_} = Spvc#spvcFrPerm.spvcFrEntry,
                                     If_Value;
                                 {If_Value,_,_,_} ->
                                     If_Value;
                                 {If_Value,_,_} ->
                                     If_Value;
                                 {If_Value,_} ->
                                     If_Value;
                                 [If_Value|_] ->
                                     If_Value;
                                 _ ->
                                     error
                             end,release_nu) of
        disabled ->
            outgoing_callproceeding(release_nu_disabled,Spvc,Attrs);
        enabled ->
            outgoing_callproceeding(release_nu_enabled,Spvc,Attrs)
    end;
orig_state_machine(outgoing_callproceeding,release_comp_nu,Spvc,Attrs) ->
    case get_link_state_intf(tuple_to_list(Spvc#spvcObj.spvcEntry),release_comp_nu) of
        disabled ->
            outgoing_callproceeding(release_comp_nu_disabled,Spvc,Attrs);
        enabled ->
            outgoing_callproceeding(release_comp_nu_enabled,Spvc,Attrs)
    end;
orig_state_machine(outgoing_callproceeding,not_in_service,Spvc,Attrs) ->
    outgoing_callproceeding(not_in_service,Spvc,Attrs);
orig_state_machine(outgoing_callproceeding,activate_enabled,Spvc,Attrs) ->
    ok;
orig_state_machine(outgoing_callproceeding,activate_disabled,Spvc,Attrs) ->
    ok;
orig_state_machine(active,destroy,Spvc,Attrs) ->
    active(destroy,Spvc,Attrs);
orig_state_machine(active,restart,Spvc,Attrs) ->
    active(restart,Spvc,Attrs);
orig_state_machine(active,release_nu,Spvc,Attrs) ->
    case cnhChi:get_link_opstate(case Spvc of
                                     Spvc when record(Spvc,spvcObj) ->
                                         case Spvc#spvcObj.spvcEntry of
                                             {If_Value,_,_,_} ->
                                                 If_Value;
                                             {If_Value,_,_} ->
                                                 If_Value
                                         end;
                                     Spvc when record(Spvc,spvcVcc) ->
                                         {If_Value,_,_,_} = Spvc#spvcVcc.spvcVccEntry,
                                         If_Value;
                                     Spvc when record(Spvc,spvcVpc) ->
                                         {If_Value,_,_} = Spvc#spvcVpc.spvcVpcEntry,
                                         If_Value;
                                     Spvc when record(Spvc,spvcVpcPerm) ->
                                         {If_Value,_,_} = Spvc#spvcVpcPerm.spvcVpcEntry,
                                         If_Value;
                                     Spvc when record(Spvc,spvcVccPerm) ->
                                         {If_Value,_,_,_} = Spvc#spvcVccPerm.spvcVccEntry,
                                         If_Value;
                                     Spvc when record(Spvc,spvcTargetVc) ->
                                         {If_Value,_,_} = Spvc#spvcTargetVc.entry,
                                         If_Value;
                                     Spvc when record(Spvc,spvcTargetVp) ->
                                         {If_Value,_} = Spvc#spvcTargetVp.entry,
                                         If_Value;
                                     Spvc when record(Spvc,pchVc) ->
                                         {If_Value,_,_} = Spvc#pchVc.vclEntry,
                                         If_Value;
                                     Spvc when record(Spvc,pchVp) ->
                                         {If_Value,_} = Spvc#pchVp.vplEntry,
                                         If_Value;
                                     Spvc when record(Spvc,spvcFr) ->
                                         {If_Value,_} = Spvc#spvcFr.spvcFrEntry,
                                         If_Value;
                                     Spvc when record(Spvc,spvcFrPerm) ->
                                         {If_Value,_} = Spvc#spvcFrPerm.spvcFrEntry,
                                         If_Value;
                                     {If_Value,_,_,_} ->
                                         If_Value;
                                     {If_Value,_,_} ->
                                         If_Value;
                                     {If_Value,_} ->
                                         If_Value;
                                     [If_Value|_] ->
                                         If_Value;
                                     _ ->
                                         error
                                 end) of
        disabled ->
            active(release_nu_disabled,Spvc,Attrs);
        enabled ->
            active(release_nu_enabled,Spvc,Attrs)
    end;
orig_state_machine(active,release_comp_nu,Spvc,Attrs) ->
    release_at_restart(release_comp_nu,Spvc,Attrs);
orig_state_machine(active,not_in_service,Spvc,Attrs) ->
    active(not_in_service,Spvc,Attrs);
orig_state_machine(active,activate_enabled,Spvc,Attrs) ->
    ok;
orig_state_machine(active,activate_disabled,Spvc,Attrs) ->
    ok;
orig_state_machine(active,release_incumbent,Spvc,Attrs) ->
    active(release_incumbent,Spvc,Attrs);
orig_state_machine(wait,destroy,Spvc,Attrs) ->
    wait(destroy,Spvc,Attrs);
orig_state_machine(wait,timeout,Spvc,Attrs) ->
    wait(timeout,Spvc,Attrs);
orig_state_machine(wait,restart,Spvc,Attrs) ->
    wait(restart,Spvc,Attrs);
orig_state_machine(wait,release_nu,Spvc,Attrs) ->
    ok;
orig_state_machine(wait,not_in_service,Spvc,Attrs) ->
    wait(not_in_service,Spvc,Attrs);
orig_state_machine(wait,activate_enabled,Spvc,Attrs) ->
    wait(timeout,Spvc,Attrs);
orig_state_machine(wait,activate_disabled,Spvc,Attrs) ->
    ok;
orig_state_machine(release_at_restart,release_comp_nu,Spvc,Attrs) ->
    release_at_restart(release_comp_nu,Spvc,Attrs);
orig_state_machine(release_at_restart,release_nu,Spvc,Attrs) ->
    release_at_restart(release_nu,Spvc,Attrs);
orig_state_machine(release_at_restart,connect_nu,Spvc,Attrs) ->
    release_at_restart(connect_nu,Spvc,Attrs);
orig_state_machine(release_at_restart,destroy,Spvc,Attrs) ->
    release_at_restart(destroy,Spvc,Attrs);
orig_state_machine(release_at_restart,not_in_service,Spvc,Attrs) ->
    release_at_restart(not_in_service,Spvc,Attrs);
orig_state_machine(release_at_restart,activate_enabled,Spvc,Attrs) ->
    ok;
orig_state_machine(release_at_restart,activate_disabled,Spvc,Attrs) ->
    ok;
orig_state_machine(release_request,release_comp_nu,Spvc,Attrs) ->
    release_request(release_comp_nu,Spvc,Attrs);
orig_state_machine(release_request,release_nu,Spvc,Attrs) ->
    release_request(release_nu,Spvc,Attrs);
orig_state_machine(release_request,destroy,Spvc,Attrs) ->
    release_request(destroy,Spvc,Attrs);
orig_state_machine(release_request,not_in_service,Spvc,Attrs) ->
    release_request(not_in_service,Spvc,Attrs);
orig_state_machine(release_request,activate_enabled,Spvc,Attrs) ->
    ok;
orig_state_machine(release_request,activate_disabled,Spvc,Attrs) ->
    ok;
orig_state_machine(rest_in_peace,restart,Spvc,Attrs) ->
    rest_in_peace(restart,Spvc,Attrs);
orig_state_machine(rest_in_peace,destroy,Spvc,Attrs) ->
    rest_in_peace(destroy,Spvc,Attrs);
orig_state_machine(rest_in_peace,not_in_service,Spvc,Attrs) ->
    rest_in_peace(not_in_service,Spvc,Attrs);
orig_state_machine(rest_in_peace,connect_nu,Spvc,Attrs) ->
    rest_in_peace(connect_nu,Spvc,Attrs);
orig_state_machine(rest_in_peace,activate_enabled,Spvc,Attrs) ->
    rest_in_peace(restart,Spvc,Attrs);
orig_state_machine(rest_in_peace,activate_disabled,Spvc,Attrs) ->
    ok;
orig_state_machine(rest_in_peace,release_nu,Spvc,Attrs) ->
    ok;
orig_state_machine(rest_in_peace,release_comp_nu,Spvc,Attrs) ->
    ok;
orig_state_machine(not_in_service,activate_enabled,Spvc,Attrs) ->
    not_in_service(activate_enabled,Spvc,Attrs);
orig_state_machine(not_in_service,activate_disabled,Spvc,Attrs) ->
    not_in_service(activate_disabled,Spvc,Attrs);
orig_state_machine(not_in_service,destroy,Spvc,Attrs) ->
    not_in_service(destroy,Spvc,Attrs);
orig_state_machine(not_in_service,connect_nu,Spvc,Attrs) ->
    not_in_service(connect_nu,Spvc,Attrs);
orig_state_machine(not_in_service,_,Spvc,Attrs) ->
    ok;
orig_state_machine(awaiting_switch_over,switch_over,Spvc,[HcId]) ->
    awaiting_switch_over(switch_over,Spvc,[HcId]);
orig_state_machine(awaiting_switch_over,activate_disabled,Spvc,Attrs) ->
    awaiting_switch_over(activate_disabled,Spvc,Attrs);
orig_state_machine(awaiting_switch_over,destroy,Spvc,Attrs) ->
    awaiting_switch_over(destroy,Spvc,Attrs);
orig_state_machine(awaiting_switch_over,restart,Spvc,Attrs) ->
    awaiting_switch_over(restart,Spvc,Attrs);
orig_state_machine(awaiting_switch_over,_,Spvc,Attrs) ->
    ok;
orig_state_machine(undefined,destroy,Spvc,Attrs) ->
    rest_in_peace(destroy,Spvc,Attrs).

null(createAndGo_enabled,[],[Row,Cols,Type,FrKey]) ->
    debug_disabled,
    Key = list_to_tuple(Row),
    Spvc = #spvcObj{spvcEntry = Key,
                    spvcApplication = Type,
                    spvcRowStatus = 1,
                    spvcFrKey = FrKey},
    Spvc1 = set_attrs(Spvc,Cols),
    {Spvc2,HcId,Setup} = new_state_outgoing_call_proceeding(Spvc1),
    pchTpUpdate(case Key of
                    {IfIndex_Value,Vpi_Value,Vci_Value,_} ->
                        {IfIndex_Value,Vpi_Value,Vci_Value};
                    {IfIndex_Value,Vpi_Value,_} ->
                        {IfIndex_Value,Vpi_Value};
                    [IfIndex_Value,Vpi_Value,Vci_Value,_] ->
                        [IfIndex_Value,Vpi_Value,Vci_Value];
                    [IfIndex_Value,Vpi_Value,_] ->
                        [IfIndex_Value,Vpi_Value]
                end),
    spvcDataBase:db_write(Spvc2),
    setup(HcId,Setup,Spvc2);
null(createAndGo_disabled,[],[Row,Cols,Type,FrKey]) ->
    debug_disabled,
    case get_link_state_intf(Row,null_createAndGo_disabled) of
        disabled ->
            Key = list_to_tuple(Row),
            Spvc = #spvcObj{spvcEntry = Key,
                            spvcRowStatus = 1,
                            currentState = rest_in_peace,
                            spvcApplication = Type,
                            spvcFrKey = FrKey},
            Spvc1 = set_attrs(Spvc,Cols),
            pchTpUpdate(case Key of
                            {IfIndex_Value,Vpi_Value,Vci_Value,_} ->
                                {IfIndex_Value,Vpi_Value,Vci_Value};
                            {IfIndex_Value,Vpi_Value,_} ->
                                {IfIndex_Value,Vpi_Value};
                            [IfIndex_Value,Vpi_Value,Vci_Value,_] ->
                                [IfIndex_Value,Vpi_Value,Vci_Value];
                            [IfIndex_Value,Vpi_Value,_] ->
                                [IfIndex_Value,Vpi_Value]
                        end),
            set_call_failure_data_and_send_spvcFailingAlarm(Key),
            spvcDataBase:db_write(Spvc1);
        enabled ->
            null(createAndGo_enabled,[],[Row,Cols,Type,FrKey])
    end;
null(createAndWait,[],[Row,Cols,Type,FrKey]) ->
    debug_disabled,
    Key = list_to_tuple(Row),
    Spvc = #spvcObj{spvcEntry = Key,
                    spvcApplication = Type,
                    spvcFrKey = FrKey},
    Spvc1 = new_state_created(Spvc,Cols),
    pchTpUpdate(case Key of
                    {IfIndex_Value,Vpi_Value,Vci_Value,_} ->
                        {IfIndex_Value,Vpi_Value,Vci_Value};
                    {IfIndex_Value,Vpi_Value,_} ->
                        {IfIndex_Value,Vpi_Value};
                    [IfIndex_Value,Vpi_Value,Vci_Value,_] ->
                        [IfIndex_Value,Vpi_Value,Vci_Value];
                    [IfIndex_Value,Vpi_Value,_] ->
                        [IfIndex_Value,Vpi_Value]
                end),
    spvcDataBase:db_write(Spvc1).

pchTpUpdate({If,Vpi,Vci}) ->
    spvcDataBase:db_write(#spvcVcDyn{vclEntry = {If,Vpi,Vci},
                                     vclCcIdentifier = 0});
pchTpUpdate({If,Vpi}) ->
    spvcDataBase:db_write(#spvcVpDyn{vplEntry = {If,Vpi},
                                     vplCcIdentifier = 0}).

created(activate_enabled,Spvc,Attrs) ->
    debug_disabled,
    Spvc1 = set_attrs(Spvc,Attrs),
    Spvc2 = Spvc1#spvcObj{spvcRowStatus = 1},
    {Spvc3,HcId,HciMsg} = new_state_outgoing_call_proceeding(Spvc1),
    spvcDataBase:db_write(Spvc3),
    setup(HcId,HciMsg,Spvc3);
created(activate_disabled,Spvc,Attrs) ->
    debug_disabled,
    Spvc1 = set_attrs(Spvc,Attrs),
    Spvc2 = Spvc1#spvcObj{currentState = rest_in_peace,
                          spvcRowStatus = 1},
    update_state(Spvc,4),
    spvcDataBase:db_write(Spvc2);
created(destroy,Spvc,Attrs) ->
    debug_disabled,
    clear(Spvc).

outgoing_callproceeding(connect_nu,Spvc,[HcId,Connect]) ->
    debug_disabled,
    Spvc1 = new_state_active(Spvc),
    case Spvc#spvcObj.spvcTargetSelectType of
        2 ->
            Cpn = Connect#hci_connect.hci_cpn_soft,
            TargetVpi = Cpn#hci_cpn_soft.hci_soft_vpi,
            TargetVci = Cpn#hci_cpn_soft.hci_soft_vci,
            TargetDlci = Cpn#hci_cpn_soft.hci_soft_dlci,
            Spvc2 = Spvc1#spvcObj{spvcTargetSelectType = 1,
                                  spvcTargetVpi = TargetVpi,
                                  spvcTargetVci = TargetVci,
                                  spvcTargetDlci = TargetDlci},
            spvcDataBase:db_write(Spvc2);
        1 ->
            spvcDataBase:db_write(ets,Spvc1);
        2 ->
            Cpn = Connect#hci_connect.hci_cpn_soft,
            TargetVpi = Cpn#hci_cpn_soft.hci_soft_vpi,
            TargetDlci = Cpn#hci_cpn_soft.hci_soft_dlci,
            Spvc2 = Spvc1#spvcObj{spvcTargetSelectType = 1,
                                  spvcTargetVpi = TargetVpi,
                                  spvcTargetDlci = TargetDlci},
            spvcDataBase:db_write(Spvc2);
        1 ->
            spvcDataBase:db_write(ets,Spvc1)
    end,
    Key = Spvc#spvcObj.spvcEntry,
    PchKey = case Key of
                 {IfIndex_Value,Vpi_Value,Vci_Value,_} ->
                     {IfIndex_Value,Vpi_Value,Vci_Value};
                 {IfIndex_Value,Vpi_Value,_} ->
                     {IfIndex_Value,Vpi_Value};
                 [IfIndex_Value,Vpi_Value,Vci_Value,_] ->
                     [IfIndex_Value,Vpi_Value,Vci_Value];
                 [IfIndex_Value,Vpi_Value,_] ->
                     [IfIndex_Value,Vpi_Value]
             end,
    SpvcDyn = case PchKey of
                  {_,_,_} ->
                      case spvcDataBase:db_read({spvcVcDyn,PchKey}) of
                          [] ->
                              #spvcVcDyn{vclEntry = PchKey,
                                         vclCcIdentifier = 0,
                                         vclConnId = HcId};
                          SpvcVcDyn ->
                              SpvcVcDyn#spvcVcDyn{vclEntry = PchKey,
                                                  vclConnId = HcId}
                      end;
                  {_,_} ->
                      case spvcDataBase:db_read({spvcVpDyn,PchKey}) of
                          [] ->
                              #spvcVpDyn{vplEntry = PchKey,
                                         vplCcIdentifier = 0,
                                         vplConnId = HcId};
                          SpvcVpDyn ->
                              SpvcVpDyn#spvcVpDyn{vplEntry = PchKey,
                                                  vplConnId = HcId}
                      end
              end,
    spvcDataBase:db_write(SpvcDyn),
    CbCValue = get(no_of_rerouting),
    CbC = case CbCValue of
              undefined ->
                  debug_disabled,
                  0;
              _ ->
                  CbCValue
          end,
    SpvcDyn2 = case Key of
                   {_,_,_,_} ->
                       case spvcDataBase:db_read({spvcVccDyn,Key}) of
                           [] ->
                               #spvcVccDyn{spvcVccEntry = Key,
                                           crankBackCounter = CbC};
                           SpvcVccDyn ->
                               SpvcVccDyn#spvcVccDyn{spvcVccEntry = Key,
                                                     crankBackCounter = CbC}
                       end;
                   {_,_,_} ->
                       case spvcDataBase:db_read({spvcVpcDyn,Key}) of
                           [] ->
                               #spvcVpcDyn{spvcVpcEntry = Key,
                                           crankBackCounter = CbC};
                           SpvcVpcDyn ->
                               SpvcVpcDyn#spvcVpcDyn{spvcVpcEntry = Key,
                                                     crankBackCounter = CbC}
                       end
               end,
    spvcDataBase:db_write(SpvcDyn2),
    NewPch = spvcDataBase:db_read({pch,PchKey}),
    spvcLib:clear_spvcStillTryingAlarm(Key),
    case Spvc#spvcObj.spvcFrKey of
        undefined ->
            spvcLib:ilmi_change(PchKey,1),
            ok;
        FrEndPoint ->
            SpvcFrObj = spvcDataBase:db_read({spvcFrPerm,FrEndPoint}),
            NewSpvcFrObj = SpvcFrObj#spvcFrPerm{spvcFrConnect = 3},
            spvcDataBase:db_write(NewSpvcFrObj),
            spvcLib:ilmi_change(PchKey,1),
            set_fr_atm_iw_admin_state(FrEndPoint,up,Spvc)
    end;
outgoing_callproceeding(restart,Spvc,_) ->
    Key = Spvc#spvcObj.spvcEntry,
    debug_disabled,
    Spvc1 = new_state_release_at_restart(Spvc),
    spvcDataBase:db_write(ets,Spvc1),
    spvcLib:clear_spvcStillTryingAlarm(Key);
outgoing_callproceeding(release_nu_enabled,Spvc,[HcId,HciMsg]) ->
    debug_disabled,
    Spvc1 = new_state_rest_in_peace_or_wait(Spvc,[HcId,HciMsg]),
    [CcCause|_] = HciMsg#hci_release.hci_cause_list,
    Spvc2 = Spvc1#spvcObj{spvcLastReleaseCause = CcCause#hci_cause.hci_cause_value,
                          spvcLastReleaseDiagnostic = CcCause#hci_cause.hci_diagnostics_list},
    spvcDataBase:db_write(ets,Spvc2);
outgoing_callproceeding(release_nu_disabled,Spvc,[HcId,Release]) ->
    debug_disabled,
    Spvc1 = new_state_rest_in_peace(Spvc),
    [CcCause|_] = Release#hci_release.hci_cause_list,
    Spvc2 = Spvc1#spvcObj{spvcLastReleaseCause = CcCause#hci_cause.hci_cause_value,
                          spvcLastReleaseDiagnostic = CcCause#hci_cause.hci_diagnostics_list},
    spvcDataBase:db_write(ets,Spvc2),
    spvcLib:clear_spvcStillTryingAlarm(Spvc#spvcObj.spvcEntry);
outgoing_callproceeding(release_comp_nu_enabled,Spvc,[HcId,Release_complete]) ->
    debug_disabled,
    Spvc1 = new_state_rest_in_peace_or_wait(Spvc,[HcId,Release_complete]),
    spvcDataBase:db_write(ets,Spvc1);
outgoing_callproceeding(release_comp_nu_disabled,Spvc,[HcId,Release_complete]) ->
    debug_disabled,
    Key = Spvc#spvcObj.spvcEntry,
    Spvc1 = new_state_rest_in_peace(Spvc),
    spvcDataBase:db_write(ets,Spvc1),
    spvcLib:clear_spvcStillTryingAlarm(Key);
outgoing_callproceeding(destroy,Spvc,_) ->
    debug_disabled,
    Key = Spvc#spvcObj.spvcEntry,
    Spvc1 = new_state_release_request(Spvc),
    spvcDataBase:db_write(ets,Spvc1),
    SpvcTpToHcId = read_spvcTpToHcId(Key),
    Release = spvcEncode:encode_cc_release(31),
    spvcManager:release_un(a_side,SpvcTpToHcId#spvcTpToHcId.hcId,Release,Spvc1),
    spvcLib:clear_spvcStillTryingAlarm(Key);
outgoing_callproceeding(not_in_service,Spvc,_) ->
    debug_disabled,
    Key = Spvc#spvcObj.spvcEntry,
    Spvc1 = new_state_not_in_service(Spvc),
    spvcDataBase:db_write(Spvc1),
    SpvcTpToHcId = read_spvcTpToHcId(Key),
    Release = spvcEncode:encode_cc_release(31),
    spvcManager:release_un(a_side,SpvcTpToHcId#spvcTpToHcId.hcId,Release,Spvc1),
    spvcLib:clear_spvcStillTryingAlarm(Key).

active(restart,Spvc,_) ->
    debug_disabled,
    Key = Spvc#spvcObj.spvcEntry,
    Spvc1 = new_state_release_at_restart(Spvc),
    spvcDataBase:db_write(ets,Spvc1),
    PchKey = case Key of
                 {IfIndex_Value,Vpi_Value,Vci_Value,_} ->
                     {IfIndex_Value,Vpi_Value,Vci_Value};
                 {IfIndex_Value,Vpi_Value,_} ->
                     {IfIndex_Value,Vpi_Value};
                 [IfIndex_Value,Vpi_Value,Vci_Value,_] ->
                     [IfIndex_Value,Vpi_Value,Vci_Value];
                 [IfIndex_Value,Vpi_Value,_] ->
                     [IfIndex_Value,Vpi_Value]
             end,
    spvcLib:ilmi_change(PchKey,2),
    case Spvc#spvcObj.spvcFrKey of
        undefined ->
            ok;
        FrEndPoint ->
            set_fr_atm_iw_admin_state(FrEndPoint,down,Spvc)
    end;
active(release_nu_enabled,Spvc,[HcId,Release]) ->
    debug_disabled,
    Key = Spvc#spvcObj.spvcEntry,
    Spvc1 = new_state_rest_in_peace_or_wait(Spvc,[HcId,Release]),
    [CcCause|_] = Release#hci_release.hci_cause_list,
    Spvc2 = Spvc1#spvcObj{spvcLastReleaseCause = CcCause#hci_cause.hci_cause_value,
                          spvcLastReleaseDiagnostic = CcCause#hci_cause.hci_diagnostics_list},
    spvcDataBase:db_write(ets,Spvc2),
    PchKey = case Key of
                 {IfIndex_Value,Vpi_Value,Vci_Value,_} ->
                     {IfIndex_Value,Vpi_Value,Vci_Value};
                 {IfIndex_Value,Vpi_Value,_} ->
                     {IfIndex_Value,Vpi_Value};
                 [IfIndex_Value,Vpi_Value,Vci_Value,_] ->
                     [IfIndex_Value,Vpi_Value,Vci_Value];
                 [IfIndex_Value,Vpi_Value,_] ->
                     [IfIndex_Value,Vpi_Value]
             end,
    spvcLib:ilmi_change(PchKey,2),
    case Spvc#spvcObj.spvcFrKey of
        undefined ->
            ok;
        FrEndPoint ->
            set_fr_atm_iw_admin_state(FrEndPoint,down,Spvc)
    end;
active(release_nu_disabled,Spvc,[HcId,Release]) ->
    debug_disabled,
    case get_link_state_intf(case Spvc of
                                 Spvc when record(Spvc,spvcObj) ->
                                     case Spvc#spvcObj.spvcEntry of
                                         {If_Value,_,_,_} ->
                                             If_Value;
                                         {If_Value,_,_} ->
                                             If_Value
                                     end;
                                 Spvc when record(Spvc,spvcVcc) ->
                                     {If_Value,_,_,_} = Spvc#spvcVcc.spvcVccEntry,
                                     If_Value;
                                 Spvc when record(Spvc,spvcVpc) ->
                                     {If_Value,_,_} = Spvc#spvcVpc.spvcVpcEntry,
                                     If_Value;
                                 Spvc when record(Spvc,spvcVpcPerm) ->
                                     {If_Value,_,_} = Spvc#spvcVpcPerm.spvcVpcEntry,
                                     If_Value;
                                 Spvc when record(Spvc,spvcVccPerm) ->
                                     {If_Value,_,_,_} = Spvc#spvcVccPerm.spvcVccEntry,
                                     If_Value;
                                 Spvc when record(Spvc,spvcTargetVc) ->
                                     {If_Value,_,_} = Spvc#spvcTargetVc.entry,
                                     If_Value;
                                 Spvc when record(Spvc,spvcTargetVp) ->
                                     {If_Value,_} = Spvc#spvcTargetVp.entry,
                                     If_Value;
                                 Spvc when record(Spvc,pchVc) ->
                                     {If_Value,_,_} = Spvc#pchVc.vclEntry,
                                     If_Value;
                                 Spvc when record(Spvc,pchVp) ->
                                     {If_Value,_} = Spvc#pchVp.vplEntry,
                                     If_Value;
                                 Spvc when record(Spvc,spvcFr) ->
                                     {If_Value,_} = Spvc#spvcFr.spvcFrEntry,
                                     If_Value;
                                 Spvc when record(Spvc,spvcFrPerm) ->
                                     {If_Value,_} = Spvc#spvcFrPerm.spvcFrEntry,
                                     If_Value;
                                 {If_Value,_,_,_} ->
                                     If_Value;
                                 {If_Value,_,_} ->
                                     If_Value;
                                 {If_Value,_} ->
                                     If_Value;
                                 [If_Value|_] ->
                                     If_Value;
                                 _ ->
                                     error
                             end,active_release_nu_disabled) of
        disabled ->
            debug_disabled,
            Key = Spvc#spvcObj.spvcEntry,
            Spvc1 = Spvc#spvcObj{currentState = rest_in_peace},
            [CcCause|_] = Release#hci_release.hci_cause_list,
            Spvc2 = Spvc1#spvcObj{spvcLastReleaseCause = CcCause#hci_cause.hci_cause_value,
                                  spvcLastReleaseDiagnostic = CcCause#hci_cause.hci_diagnostics_list},
            PchKey = case Key of
                         {IfIndex_Value,Vpi_Value,Vci_Value,_} ->
                             {IfIndex_Value,Vpi_Value,Vci_Value};
                         {IfIndex_Value,Vpi_Value,_} ->
                             {IfIndex_Value,Vpi_Value};
                         [IfIndex_Value,Vpi_Value,Vci_Value,_] ->
                             [IfIndex_Value,Vpi_Value,Vci_Value];
                         [IfIndex_Value,Vpi_Value,_] ->
                             [IfIndex_Value,Vpi_Value]
                     end,
            spvcLib:ilmi_change(PchKey,2),
            update_state(Spvc,4),
            spvcDataBase:db_write(ets,Spvc2),
            case Spvc#spvcObj.spvcFrKey of
                undefined ->
                    ok;
                FrEndPoint ->
                    set_fr_atm_iw_admin_state(FrEndPoint,down,Spvc)
            end;
        enabled ->
            active(release_nu_enabled,Spvc,[HcId,Release])
    end;
active(destroy,Spvc,_) ->
    debug_disabled,
    Key = Spvc#spvcObj.spvcEntry,
    Spvc1 = new_state_release_request(Spvc),
    spvcDataBase:db_write(ets,Spvc1),
    PchKey = case Key of
                 {IfIndex_Value,Vpi_Value,Vci_Value,_} ->
                     {IfIndex_Value,Vpi_Value,Vci_Value};
                 {IfIndex_Value,Vpi_Value,_} ->
                     {IfIndex_Value,Vpi_Value};
                 [IfIndex_Value,Vpi_Value,Vci_Value,_] ->
                     [IfIndex_Value,Vpi_Value,Vci_Value];
                 [IfIndex_Value,Vpi_Value,_] ->
                     [IfIndex_Value,Vpi_Value]
             end,
    spvcLib:ilmi_change(PchKey,2),
    SpvcTpToHcId = read_spvcTpToHcId(Key),
    Release = spvcEncode:encode_cc_release(31),
    spvcManager:release_un(a_side,SpvcTpToHcId#spvcTpToHcId.hcId,Release,Spvc);
active(not_in_service,Spvc,_) ->
    debug_disabled,
    Key = Spvc#spvcObj.spvcEntry,
    Spvc1 = new_state_not_in_service(Spvc),
    spvcDataBase:db_write(Spvc1),
    PchKey = case Key of
                 {IfIndex_Value,Vpi_Value,Vci_Value,_} ->
                     {IfIndex_Value,Vpi_Value,Vci_Value};
                 {IfIndex_Value,Vpi_Value,_} ->
                     {IfIndex_Value,Vpi_Value};
                 [IfIndex_Value,Vpi_Value,Vci_Value,_] ->
                     [IfIndex_Value,Vpi_Value,Vci_Value];
                 [IfIndex_Value,Vpi_Value,_] ->
                     [IfIndex_Value,Vpi_Value]
             end,
    spvcLib:ilmi_change(PchKey,2),
    SpvcTpToHcId = read_spvcTpToHcId(Key),
    Release = spvcEncode:encode_cc_release(31),
    spvcManager:release_un(a_side,SpvcTpToHcId#spvcTpToHcId.hcId,Release,Spvc1),
    case Spvc#spvcObj.spvcFrKey of
        undefined ->
            ok;
        FrEndPoint ->
            set_fr_atm_iw_admin_state(FrEndPoint,down,Spvc)
    end;
active(release_incumbent,Spvc,[Release]) ->
    debug_disabled,
    Key = Spvc#spvcObj.spvcEntry,
    Spvc1 = new_state_awaiting_switch_over(Spvc),
    spvcDataBase:db_write(Spvc1),
    SpvcTpToHcId = read_spvcTpToHcId(Key),
    spvcManager:release_un(a_side,SpvcTpToHcId#spvcTpToHcId.hcId,Release,Spvc1).

read_spvcTpToHcId({If,Vpi,Vci,Leaf}) ->
    spvcDataBase:db_read({spvcTpToHcId,{orig,If,Vpi,Vci,Leaf}});
read_spvcTpToHcId({If,Vpi,Leaf}) ->
    spvcDataBase:db_read({spvcTpToHcId,{orig,If,Vpi,Leaf}}).

release_request(release_nu,Spvc,[HcId,Release]) ->
    debug_disabled,
    clear(Spvc);
release_request(release_comp_nu,Spvc,[HcId,Release_comp]) ->
    debug_disabled,
    clear(Spvc);
release_request(destroy,Spvc,_) ->
    debug_disabled,
    case Spvc#spvcObj.spvcEntry of
        {If,Vpi,Vci,Leaf} ->
            case spvcDataBase:db_read({spvcTpToHcId,{orig,If,Vpi,Vci,Leaf}}) of
                SpvcTpToHcId ->
                    Release = spvcEncode:encode_cc_release(31),
                    spvcManager:release_un(a_side,SpvcTpToHcId#spvcTpToHcId.hcId,Release,Spvc),
                    clear(Spvc);
                _ ->
                    ok
            end;
        {If,Vpi,Leaf} ->
            case spvcDataBase:db_read({spvcTpToHcId,{orig,If,Vpi,Leaf}}) of
                SpvcTpToHcId ->
                    Release = spvcEncode:encode_cc_release(31),
                    spvcManager:release_un(a_side,SpvcTpToHcId#spvcTpToHcId.hcId,Release,Spvc),
                    clear(Spvc);
                _ ->
                    ok
            end
    end,
    ok;
release_request(not_in_service,Spvc,_) ->
    debug_disabled,
    ok.

release_at_restart(release_nu,Spvc,[HcId,Release]) ->
    debug_disabled,
    {Spvc1,NewHcId,Setup} = new_state_outgoing_call_proceeding(Spvc),
    [CcCause|_] = Release#hci_release.hci_cause_list,
    Spvc2 = Spvc1#spvcObj{spvcLastReleaseCause = CcCause#hci_cause.hci_cause_value,
                          spvcLastReleaseDiagnostic = CcCause#hci_cause.hci_diagnostics_list},
    spvcDataBase:db_write(ets,Spvc2),
    timer:sleep(500),
    setup(NewHcId,Setup,Spvc2);
release_at_restart(release_comp_nu,Spvc,[HcId,Release_complete]) ->
    debug_disabled,
    {Spvc1,NewHcId,Setup} = new_state_outgoing_call_proceeding(Spvc),
    Spvc2 = Spvc1#spvcObj{spvcLastReleaseCause = 31,
                          spvcLastReleaseDiagnostic = []},
    spvcDataBase:db_write(ets,Spvc2),
    timer:sleep(500),
    setup(NewHcId,Setup,Spvc1);
release_at_restart(connect_nu,Spvc,_) ->
    debug_disabled,
    ok;
release_at_restart(destroy,Spvc,_) ->
    debug_disabled,
    Spvc1 = new_state_release_request(Spvc),
    spvcDataBase:db_write(ets,Spvc1);
release_at_restart(restart,Spvc,_) ->
    debug_disabled,
    Spvc1 = new_state_release_at_restart(Spvc);
release_at_restart(not_in_service,Spvc,_) ->
    debug_disabled,
    Spvc1 = new_state_not_in_service(Spvc),
    spvcDataBase:db_write(Spvc1).

wait(timeout,Spvc,_) ->
    debug_disabled,
    {Spvc1,HcId,Setup} = new_state_outgoing_call_proceeding(Spvc),
    spvcDataBase:db_write(ets,Spvc1),
    setup(HcId,Setup,Spvc1);
wait(destroy,Spvc,_) ->
    debug_disabled,
    spvcLib:clear_spvcStillTryingAlarm(Spvc#spvcObj.spvcEntry),
    clear(Spvc);
wait(restart,Spvc,_) ->
    debug_disabled,
    {Spvc1,HcId,Setup} = new_state_outgoing_call_proceeding(Spvc#spvcObj{spvcRetryFailures = 0}),
    spvcDataBase:db_write(ets,Spvc1),
    spvcReestablishTimer:cancel(Spvc#spvcObj.spvcEntry),
    spvcLib:clear_spvcStillTryingAlarm(Spvc#spvcObj.spvcEntry),
    setup(HcId,Setup,Spvc1);
wait(not_in_service,Spvc,_) ->
    debug_disabled,
    Spvc1 = new_state_not_in_service(Spvc),
    spvcDataBase:db_write(Spvc1),
    spvcLib:clear_spvcStillTryingAlarm(Spvc#spvcObj.spvcEntry).

rest_in_peace(restart,Spvc,_) ->
    debug_disabled,
    {Spvc1,HcId,Setup} = new_state_outgoing_call_proceeding(Spvc#spvcObj{spvcRetryFailures = 0}),
    spvcDataBase:db_write(ets,Spvc1),
    setup(HcId,Setup,Spvc1),
    sccmManager:cast_to_sccm(spvcLib:get_cp(om_node),spvcLib,clear_spvcFailingAlarm,[spvcLib:get_membership(node())]);
rest_in_peace(destroy,Spvc,_) ->
    debug_disabled,
    sccmManager:cast_to_sccm(spvcLib:get_cp(om_node),spvcLib,clear_spvcFailingAlarm,[spvcLib:get_membership(node())]),
    clear(Spvc);
rest_in_peace(connect_nu,Spvc,_) ->
    debug_disabled,
    Key = Spvc#spvcObj.spvcEntry,
    SpvcTpToHcId = read_spvcTpToHcId(Key),
    Release = spvcEncode:encode_cc_release(31),
    spvcManager:release_un(b_side,SpvcTpToHcId#spvcTpToHcId.hcId,Release,Spvc);
rest_in_peace(not_in_service,Spvc,_) ->
    debug_disabled,
    Spvc1 = new_state_not_in_service(Spvc),
    spvcDataBase:db_write(Spvc1),
    sccmManager:cast_to_sccm(spvcLib:get_cp(om_node),spvcLib,clear_spvcFailingAlarm,[spvcLib:get_membership(node())]).

not_in_service(activate_enabled,Spvc,_) ->
    debug_disabled,
    {Spvc1,HcId,Setup} = new_state_outgoing_call_proceeding(Spvc#spvcObj{spvcRetryFailures = 0}),
    spvcDataBase:db_write(Spvc1#spvcObj{spvcRowStatus = 1}),
    setup(HcId,Setup,Spvc1);
not_in_service(activate_disabled,Spvc,_) ->
    debug_disabled,
    Spvc1 = new_state_rest_in_peace(Spvc),
    spvcDataBase:db_write(Spvc1#spvcObj{spvcRowStatus = 1});
not_in_service(connect_nu,Spvc,_) ->
    debug_disabled,
    Spvc1 = new_state_rest_in_peace(Spvc),
    spvcDataBase:db_write(Spvc1#spvcObj{spvcRowStatus = 1}),
    Key = Spvc#spvcObj.spvcEntry,
    SpvcTpToHcId = read_spvcTpToHcId(Key),
    Release = spvcEncode:encode_cc_release(31),
    spvcManager:release_un(b_side,SpvcTpToHcId#spvcTpToHcId.hcId,Release,Spvc1);
not_in_service(destroy,Spvc,_) ->
    debug_disabled,
    clear(Spvc).

awaiting_switch_over(switch_over,Spvc,[HcId]) ->
    debug_disabled,
    Spvc1 = Spvc#spvcObj{currentState = active},
    Index = Spvc#spvcObj.spvcEntry,
    TpIndex = create_tp_index(Index),
    spvcDataBase:db_write(Spvc1),
    ets:insert(spvcTpToHcId,#spvcTpToHcId{tpEntry = TpIndex,
                             hcId = HcId}),
    ets:insert(spvcHcIdToTp,#spvcHcIdToTp{tpEntry = TpIndex,
                             hcId = HcId}),
    update_dyn_table_hcid(Index,HcId),
    ok;
awaiting_switch_over(activate_disabled,Spvc,Attrs) ->
    Spvc1 = new_state_rest_in_peace(Spvc),
    spvcDataBase:db_write(Spvc1),
    ok;
awaiting_switch_over(restart,Spvc,Attrs) ->
    debug_disabled,
    Key = Spvc#spvcObj.spvcEntry,
    Spvc1 = new_state_release_at_restart(Spvc),
    spvcDataBase:db_write(ets,Spvc1),
    PchKey = case Key of
                 {IfIndex_Value,Vpi_Value,Vci_Value,_} ->
                     {IfIndex_Value,Vpi_Value,Vci_Value};
                 {IfIndex_Value,Vpi_Value,_} ->
                     {IfIndex_Value,Vpi_Value};
                 [IfIndex_Value,Vpi_Value,Vci_Value,_] ->
                     [IfIndex_Value,Vpi_Value,Vci_Value];
                 [IfIndex_Value,Vpi_Value,_] ->
                     [IfIndex_Value,Vpi_Value]
             end,
    spvcLib:ilmi_change(PchKey,2),
    case Spvc#spvcObj.spvcFrKey of
        undefined ->
            ok;
        FrEndPoint ->
            set_fr_atm_iw_admin_state(FrEndPoint,down,Spvc)
    end;
awaiting_switch_over(destroy,Spvc,Attrs) ->
    clear(Spvc).

create_tp_index({If,Vpi,Vci,Leaf}) ->
    list_to_tuple([orig,If,Vpi,Vci,Leaf]);
create_tp_index({If,Vpi,Leaf}) ->
    list_to_tuple([orig,If,Vpi,Leaf]).

update_dyn_table_hcid({If,Vpi,Vci,Leaf},HcId) ->
    [VcDyn] = ets:lookup(spvcVcDyn,{If,Vpi,Vci}),
    ets:insert(spvcVcDyn,VcDyn#spvcVcDyn{vclConnId = HcId});
update_dyn_table_hcid({If,Vpi,Leaf},HcId) ->
    [VpDyn] = ets:lookup(spvcVpDyn,{If,Vpi}),
    ets:insert(spvcVpDyn,VpDyn#spvcVpDyn{vplConnId = HcId}).

new_state_outgoing_call_proceeding(Spvc) ->
    debug_disabled,
    Spvc1 = Spvc#spvcObj{spvcRowStatus = 1,
                         currentState = outgoing_callproceeding},
    Key = Spvc1#spvcObj.spvcEntry,
    update_state(Spvc,outgoing_callproceeding),
    PchKey = case Key of
                 {IfIndex_Value,Vpi_Value,Vci_Value,_} ->
                     {IfIndex_Value,Vpi_Value,Vci_Value};
                 {IfIndex_Value,Vpi_Value,_} ->
                     {IfIndex_Value,Vpi_Value};
                 [IfIndex_Value,Vpi_Value,Vci_Value,_] ->
                     [IfIndex_Value,Vpi_Value,Vci_Value];
                 [IfIndex_Value,Vpi_Value,_] ->
                     [IfIndex_Value,Vpi_Value]
             end,
    {FwdAtdIndex,BwdAtdIndex} = case PchKey of
                                    {_,_,_} ->
                                        Vc = spvcDataBase:db_read({pchVc,PchKey}),
                                        {Vc#pchVc.vclReceiveTrafficDescrIndex,Vc#pchVc.vclTransmitTrafficDescrIndex};
                                    {_,_} ->
                                        Vp = spvcDataBase:db_read({pchVp,PchKey}),
                                        {Vp#pchVp.vplReceiveTrafficDescrIndex,Vp#pchVp.vplTransmitTrafficDescrIndex}
                                end,
    FwdPchAtd = spvcDataBase:db_read({pchAtd,FwdAtdIndex}),
    BwdPchAtd = spvcDataBase:db_read({pchAtd,BwdAtdIndex}),
    Row = tuple_to_list(Key),
    HcId = spvcLib:create_hcid(Row,case Row of
                                   Row when record(Row,spvcObj) ->
                                       case Row#spvcObj.spvcEntry of
                                           {If_Value,_,_,_} ->
                                               If_Value;
                                           {If_Value,_,_} ->
                                               If_Value
                                       end;
                                   Row when record(Row,spvcVcc) ->
                                       {If_Value,_,_,_} = Row#spvcVcc.spvcVccEntry,
                                       If_Value;
                                   Row when record(Row,spvcVpc) ->
                                       {If_Value,_,_} = Row#spvcVpc.spvcVpcEntry,
                                       If_Value;
                                   Row when record(Row,spvcVpcPerm) ->
                                       {If_Value,_,_} = Row#spvcVpcPerm.spvcVpcEntry,
                                       If_Value;
                                   Row when record(Row,spvcVccPerm) ->
                                       {If_Value,_,_,_} = Row#spvcVccPerm.spvcVccEntry,
                                       If_Value;
                                   Row when record(Row,spvcTargetVc) ->
                                       {If_Value,_,_} = Row#spvcTargetVc.entry,
                                       If_Value;
                                   Row when record(Row,spvcTargetVp) ->
                                       {If_Value,_} = Row#spvcTargetVp.entry,
                                       If_Value;
                                   Row when record(Row,pchVc) ->
                                       {If_Value,_,_} = Row#pchVc.vclEntry,
                                       If_Value;
                                   Row when record(Row,pchVp) ->
                                       {If_Value,_} = Row#pchVp.vplEntry,
                                       If_Value;
                                   Row when record(Row,spvcFr) ->
                                       {If_Value,_} = Row#spvcFr.spvcFrEntry,
                                       If_Value;
                                   Row when record(Row,spvcFrPerm) ->
                                       {If_Value,_} = Row#spvcFrPerm.spvcFrEntry,
                                       If_Value;
                                   {If_Value,_,_,_} ->
                                       If_Value;
                                   {If_Value,_,_} ->
                                       If_Value;
                                   {If_Value,_} ->
                                       If_Value;
                                   [If_Value|_] ->
                                       If_Value;
                                   _ ->
                                       error
                               end),
    Setup = spvcEncode:encode_cc_setup(Row,Spvc1,FwdPchAtd,BwdPchAtd),
    debug_disabled,
    debug_disabled,
    debug_disabled,
    {Spvc1,HcId,Setup}.

new_state_release_request(Spvc) ->
    debug_disabled,
    update_state(Spvc,release_request),
    Spvc#spvcObj{currentState = release_request}.

new_state_release_at_restart(Spvc) ->
    debug_disabled,
    Spvc1 = Spvc#spvcObj{spvcRetryFailures = 0,
                         currentState = release_at_restart},
    update_state(Spvc,release_at_restart),
    HcId = spvcEncode:encode_cc_hcid(Spvc1#spvcObj.spvcEntry),
    Release = spvcEncode:encode_cc_release(31),
    spvcManager:release_un(a_side,HcId,Release,Spvc1),
    Spvc1.

new_state_rest_in_peace_or_wait(Spvc,[HcId,HciMsg]) ->
    debug_disabled,
    Spvc1 = Spvc#spvcObj{spvcRetryFailures = Spvc#spvcObj.spvcRetryFailures + 1},
    case check_limits(Spvc1) of
        {ok,ok,no_retries} ->
            send_spvcFailingAlarm(Spvc#spvcObj.spvcEntry),
            update_state(Spvc,4),
            spvcLib:clear_spvcStillTryingAlarm(Spvc#spvcObj.spvcEntry),
            Spvc1#spvcObj{currentState = rest_in_peace};
        {ok,ok,_} ->
            Spvc2 = Spvc1#spvcObj{spvcRetryTimer = time(),
                                  currentState = wait},
            update_state(Spvc,wait),
            start_timer(wait,Spvc2),
            Spvc2;
        {retry_threshold,ok,no_retries} ->
            Spvc2 = Spvc1#spvcObj{currentState = rest_in_peace},
            update_state(Spvc,4),
            send_call_failure(Spvc),
            send_spvcFailingAlarm(Spvc#spvcObj.spvcEntry),
            spvcLib:clear_spvcStillTryingAlarm(Spvc#spvcObj.spvcEntry),
            Spvc2;
        {retry_threshold,ok,_} ->
            Spvc2 = Spvc1#spvcObj{spvcRetryTimer = time(),
                                  currentState = wait},
            update_state(Spvc,wait),
            send_call_failure(Spvc2),
            start_timer(wait,Spvc2),
            Spvc2;
        {ok,retry_limit,_} ->
            send_spvcFailingAlarm(Spvc#spvcObj.spvcEntry),
            update_state(Spvc,4),
            spvcLib:clear_spvcStillTryingAlarm(Spvc#spvcObj.spvcEntry),
            Spvc1#spvcObj{currentState = rest_in_peace};
        {retry_threshold,retry_limit,_} ->
            Spvc2 = Spvc1#spvcObj{currentState = rest_in_peace},
            update_state(Spvc,4),
            send_call_failure(Spvc2),
            send_spvcFailingAlarm(Spvc#spvcObj.spvcEntry),
            spvcLib:clear_spvcStillTryingAlarm(Spvc#spvcObj.spvcEntry),
            Spvc2
    end.

send_call_failure(Spvc) ->
    case Spvc#spvcObj.spvcRetryThreshold of
        0 ->
            ok;
        _ ->
            sccmManager:cast_to_sccm(spvcLib:get_cp(om_node),spvcOrig,call_failure,[Spvc])
    end.

new_state_rest_in_peace(Spvc) ->
    debug_disabled,
    update_state(Spvc,4),
    Spvc1 = Spvc#spvcObj{spvcRetryFailures = Spvc#spvcObj.spvcRetryFailures + 1},
    send_spvcFailingAlarm(Spvc#spvcObj.spvcEntry),
    case check_limits(Spvc1) of
        {ok,_,_} ->
            Spvc1#spvcObj{currentState = rest_in_peace};
        {retry_threshold,_,_} ->
            Spvc2 = Spvc1#spvcObj{currentState = rest_in_peace},
            case Spvc2#spvcObj.spvcRetryThreshold of
                0 ->
                    ok;
                _ ->
                    sccmManager:cast_to_sccm(spvcLib:get_cp(om_node),spvcOrig,call_failure,[Spvc2])
            end,
            Spvc2
    end.

new_state_active(Spvc) ->
    debug_disabled,
    update_state(Spvc,3),
    Spvc#spvcObj{spvcRetryFailures = 0,
                 currentState = active}.

new_state_created(Spvc,SetCols) ->
    debug_disabled,
    update_state(Spvc,created),
    case spvcSNMP:is_all_values(case Spvc#spvcObj.spvcEntry of
                                    {_,_,_,_} ->
                                        spvcVcc;
                                    {_,_,_} ->
                                        spvcVpc;
                                    {_,_} ->
                                        spvcFr;
                                    [_,_,_,_] ->
                                        spvcVcc;
                                    [_,_,_] ->
                                        spvcVpc;
                                    [_,_] ->
                                        spvcFr
                                end,SetCols) of
        true ->
            Spvc1 = Spvc#spvcObj{spvcRowStatus = 2,
                                 currentState = created},
            set_attrs(Spvc1,SetCols);
        false ->
            Spvc1 = Spvc#spvcObj{spvcRowStatus = 3,
                                 currentState = created},
            set_attrs(Spvc1,SetCols)
    end.

new_state_not_in_service(Spvc) ->
    debug_disabled,
    update_state(Spvc,not_in_service),
    Spvc#spvcObj{currentState = not_in_service,
                 spvcRowStatus = 2}.

new_state_awaiting_switch_over(Spvc) ->
    debug_disabled,
    Spvc#spvcObj{currentState = awaiting_switch_over}.

update_state(Spvc,NewState) ->
    State = Spvc#spvcObj.currentState,
    SpvcEntry = Spvc#spvcObj.spvcEntry,
    debug_disabled,
    spvcLib:update_state({State,SpvcEntry},NewState).

send_spvcFailingAlarm(Key) ->
    debug_disabled,
    rpc:cast(spvcLib:get_cp(om_node),spvcLib,send_spvcFailingAlarm,[Key]).

set_call_failure_data_and_send_spvcFailingAlarm({If,Vpi,Leaf}) ->
    debug_disabled,
    Spvc = spvcDataBase:db_read({spvcObj,{If,Vpi,Leaf}}),
    if
        Spvc == [] ->
            ok;
        true ->
            spvcLib:update_state({Spvc#spvcObj.currentState,{If,Vpi,Leaf}},4)
    end;
set_call_failure_data_and_send_spvcFailingAlarm({If,Vpi,Vci,Leaf}) ->
    debug_disabled,
    Spvc = spvcDataBase:db_read({spvcObj,{If,Vpi,Vci,Leaf}}),
    if
        Spvc == [] ->
            ok;
        true ->
            spvcLib:update_state({Spvc#spvcObj.currentState,{If,Vpi,Vci,Leaf}},4)
    end.

set_attrs(Spvc,SetCols) ->
    case Spvc#spvcObj.spvcEntry of
        {_,_,_,_} ->
            set_attrs_spvcc(Spvc,SetCols);
        {_,_,_} ->
            set_attrs_spvpc(Spvc,SetCols)
    end.

set_attrs_spvcc(Spvc,[{2,Value}|T]) ->
    Spvc1 = Spvc#spvcObj{spvcTargetAddress = Value},
    set_attrs_spvcc(Spvc1,T);
set_attrs_spvcc(Spvc,[{3,Value}|T]) ->
    Spvc1 = Spvc#spvcObj{spvcTargetSelectType = Value},
    set_attrs_spvcc(Spvc1,T);
set_attrs_spvcc(Spvc,[{18,Value}|T]) ->
    Spvc1 = Spvc#spvcObj{spvcTargetVpi = Value},
    set_attrs_spvcc(Spvc1,T);
set_attrs_spvcc(Spvc,[{4,Value}|T]) ->
    Spvc1 = Spvc#spvcObj{spvcTargetVpi = Value},
    set_attrs_spvcc(Spvc1,T);
set_attrs_spvcc(Spvc,[{5,Value}|T]) ->
    Spvc1 = Spvc#spvcObj{spvcTargetVci = Value},
    set_attrs_spvcc(Spvc1,T);
set_attrs_spvcc(Spvc,[{6,Value}|T]) ->
    Spvc1 = Spvc#spvcObj{spvcLastReleaseCause = Value},
    set_attrs_spvcc(Spvc1,T);
set_attrs_spvcc(Spvc,[{7,Value}|T]) ->
    Spvc1 = Spvc#spvcObj{spvcLastReleaseDiagnostic = Value},
    set_attrs_spvcc(Spvc1,T);
set_attrs_spvcc(Spvc,[{10,Value}|T]) ->
    Spvc1 = Spvc#spvcObj{spvcRetryInterval = Value},
    set_attrs_spvcc(Spvc1,T);
set_attrs_spvcc(Spvc,[{11,Value}|T]) ->
    Spvc1 = Spvc#spvcObj{spvcRetryTimer = Value},
    set_attrs_spvcc(Spvc1,T);
set_attrs_spvcc(Spvc,[{12,Value}|T]) ->
    Spvc1 = Spvc#spvcObj{spvcRetryThreshold = Value},
    set_attrs_spvcc(Spvc1,T);
set_attrs_spvcc(Spvc,[{13,Value}|T]) ->
    Spvc1 = Spvc#spvcObj{spvcRetryFailures = Value},
    set_attrs_spvcc(Spvc1,T);
set_attrs_spvcc(Spvc,[{14,Value}|T]) ->
    Spvc1 = Spvc#spvcObj{spvcRetryLimit = Value},
    set_attrs_spvcc(Spvc1,T);
set_attrs_spvcc(Spvc,[{16,Value}|T]) ->
    Spvc1 = Spvc#spvcObj{spvcTargetDlci = Value},
    set_attrs_spvcc(Spvc1,T);
set_attrs_spvcc(Spvc,[{17,Value}|T]) ->
    Spvc1 = Spvc#spvcObj{spvcTargetType = Value},
    set_attrs_spvcc(Spvc1,T);
set_attrs_spvcc(Spvc,[_|T]) ->
    set_attrs_spvcc(Spvc,T);
set_attrs_spvcc(Spvc,[]) ->
    debug_disabled,
    Spvc.

set_attrs_spvpc(Spvc,[{2,Value}|T]) ->
    Spvc1 = Spvc#spvcObj{spvcTargetAddress = Value},
    set_attrs_spvpc(Spvc1,T);
set_attrs_spvpc(Spvc,[{3,Value}|T]) ->
    Spvc1 = Spvc#spvcObj{spvcTargetSelectType = Value},
    set_attrs_spvpc(Spvc1,T);
set_attrs_spvpc(Spvc,[{15,Value}|T]) ->
    Spvc1 = Spvc#spvcObj{spvcTargetVpi = Value},
    set_attrs_spvpc(Spvc1,T);
set_attrs_spvpc(Spvc,[{4,Value}|T]) ->
    Spvc1 = Spvc#spvcObj{spvcTargetVpi = Value},
    set_attrs_spvpc(Spvc1,T);
set_attrs_spvpc(Spvc,[{5,Value}|T]) ->
    Spvc1 = Spvc#spvcObj{spvcLastReleaseCause = Value},
    set_attrs_spvpc(Spvc1,T);
set_attrs_spvpc(Spvc,[{6,Value}|T]) ->
    Spvc1 = Spvc#spvcObj{spvcLastReleaseDiagnostic = Value},
    set_attrs_spvpc(Spvc1,T);
set_attrs_spvpc(Spvc,[{9,Value}|T]) ->
    Spvc1 = Spvc#spvcObj{spvcRetryInterval = Value},
    set_attrs_spvpc(Spvc1,T);
set_attrs_spvpc(Spvc,[{10,Value}|T]) ->
    Spvc1 = Spvc#spvcObj{spvcRetryTimer = Value},
    set_attrs_spvpc(Spvc1,T);
set_attrs_spvpc(Spvc,[{11,Value}|T]) ->
    Spvc1 = Spvc#spvcObj{spvcRetryThreshold = Value},
    set_attrs_spvpc(Spvc1,T);
set_attrs_spvpc(Spvc,[{12,Value}|T]) ->
    Spvc1 = Spvc#spvcObj{spvcRetryFailures = Value},
    set_attrs_spvpc(Spvc1,T);
set_attrs_spvpc(Spvc,[{13,Value}|T]) ->
    Spvc1 = Spvc#spvcObj{spvcRetryLimit = Value},
    set_attrs_spvpc(Spvc1,T);
set_attrs_spvpc(Spvc,[_|T]) ->
    set_attrs_spvpc(Spvc,T);
set_attrs_spvpc(Spvc,[]) ->
    Spvc.

call_failure(Spvc) ->
    debug_disabled,
    Key = case Spvc#spvcObj.spvcFrKey of
              undefined ->
                  spvcLib:update_counter(callFailures,1,spvcLib:get_membership(node())),
                  atm_spvc;
              _ ->
                  spvcLib:update_counter(callFrFailures,1,spvcLib:get_membership(node())),
                  fr_spvc
          end,
    Obj = spvcDataBase:db_read({spvcFailures,Key}),
    case Obj#spvcFailures.spvcCallFailuresTrapEnable of
        1 ->
            EventIndObj = spvcDataBase:db_read({spvcEventIndicator,Key}),
            case EventIndObj#spvcEventIndicator.spvcTimerInd of
                1 ->
                    spvcDataBase:db_write(EventIndObj#spvcEventIndicator{spvcSendEventInd = 1}),
                    NI = Obj#spvcFailures.spvcNotificationInterval,
                    sysTimer:apply_after(1000 * NI,spvcOrig,timeout_event,[EventIndObj]);
                _ ->
                    spvcManager:send_event(Key),
                    NI = Obj#spvcFailures.spvcNotificationInterval,
                    sysTimer:apply_after(1000 * NI,spvcManager,timeout,[Key]),
                    spvcDataBase:db_write(EventIndObj#spvcEventIndicator{spvcTimerInd = 1,
                                                                         spvcSendEventInd = 2})
            end;
        _ ->
            ok
    end.

timeout_event(EventIndObj) ->
    spvcDataBase:db_write(EventIndObj#spvcEventIndicator{spvcTimerInd = 2}).

check_limits(Spvc) ->
    debug_disabled,
    T = Spvc#spvcObj.spvcRetryThreshold,
    L = Spvc#spvcObj.spvcRetryLimit,
    F = Spvc#spvcObj.spvcRetryFailures,
    I = Spvc#spvcObj.spvcRetryInterval,
    {check_threshold(F,T),check_limit(F,L),check_interval(I)}.

check_threshold(Failures,Threshold) when Failures == Threshold ->
    debug_disabled,
    retry_threshold;
check_threshold(Failures,Threshold) ->
    debug_disabled,
    ok.

check_limit(Failures,0) ->
    debug_disabled,
    ok;
check_limit(Failures,Limit) when Failures < Limit ->
    debug_disabled,
    ok;
check_limit(Failures,Limit) ->
    debug_disabled,
    retry_limit.

check_interval(0) ->
    no_retries;
check_interval(I) ->
    I.

start_timer(wait,Spvc) ->
    debug_disabled,
    Key = Spvc#spvcObj.spvcEntry,
    Id = spvcReestablishTimer:apply_after(backoff_delay(Key),spvcServer,cast_to_spvc,[node(),spvcOrig,timeout,[wait,Key]]).

timeout(wait,Key) ->
    debug_disabled,
    case spvcDataBase:db_read({spvcObj,Key}) of
        [] ->
            debug_disabled,
            ok;
        Spvc ->
            case Spvc#spvcObj.currentState of
                wait ->
                    IfIndex = element(1,Key),
                    case spvcOam:is_reassign_et_in_progress(IfIndex) of
                        true ->
                            ok;
                        _ ->
                            orig_state_machine(wait,timeout,Spvc,[])
                    end;
                _ ->
                    ok
            end
    end;
timeout(X,Y) ->
    debug_disabled,
    ok.

clear(Spvc) ->
    debug_disabled,
    Key = Spvc#spvcObj.spvcEntry,
    PchKey = case Key of
                 {IfIndex_Value,Vpi_Value,Vci_Value,_} ->
                     {IfIndex_Value,Vpi_Value,Vci_Value};
                 {IfIndex_Value,Vpi_Value,_} ->
                     {IfIndex_Value,Vpi_Value};
                 [IfIndex_Value,Vpi_Value,Vci_Value,_] ->
                     [IfIndex_Value,Vpi_Value,Vci_Value];
                 [IfIndex_Value,Vpi_Value,_] ->
                     [IfIndex_Value,Vpi_Value]
             end,
    spvcEndPoint:free_tp_spvc(PchKey),
    spvcDataBase:db_delete({spvcObj,Key}),
    update_state(Spvc,clear),
    OrigKey = list_to_tuple([orig] ++ tuple_to_list(Key)),
    case Spvc#spvcObj.currentState of
        created ->
            ok;
        _ ->
            case spvcDataBase:db_read({spvcTpToHcId,OrigKey}) of
                [] ->
                    ok;
                #spvcTpToHcId{hcId = HcId} ->
                    spvcDataBase:db_delete({spvcHcIdToTp,HcId})
            end,
            ets:delete(spvcTpToHcId,OrigKey),
            spvcReestablishTimer:cancel(Key),
            ets:delete(spvcBackoff,Spvc#spvcObj.spvcEntry)
    end,
    case Spvc#spvcObj.spvcFrKey of
        undefined ->
            sccmManager:cast_to_sccm(spvcLib:get_cp(om_node),spvcEndPoint,remove_tp,[tuple_to_list(PchKey)]);
        FrKey ->
            spvcFr:clean_up(FrKey)
    end,
    case {Spvc#spvcObj.spvcRerCap,Spvc#spvcObj.spvcEntry} of
        {false,_} ->
            ok;
        {true,Entry} when size(Entry) == 3 ->
            spvcDataBase:db_delete({spvcRerVp,Entry});
        {true,Entry} when size(Entry) == 4 ->
            spvcDataBase:db_delete({spvcRerVc,Entry})
    end.

get_link_state(If) when integer(If) ->
    debug_disabled,
    cnhChi:get_link_opstate(If);
get_link_state(Other) ->
    debug_disabled,
    disabled.

get_link_state_intf(If,Msg) when integer(If) ->
    debug_disabled,
    case cnhChi:get_link_opstate(If) of
        enabled ->
            enabled;
        _ ->
            Om_Node = spvcLib:get_cp(om_node),
            case rpc:call(Om_Node,intfI,get_link_op_state,[If]) of
                {ok,enabled} ->
                    enabled;
                Result ->
                    disabled
            end
    end;
get_link_state_intf(Other,Msg) ->
    debug_disabled,
    disabled.

setup(HcId,Setup,Spvc) ->
    case spvcDataBase:db_read({spvcObj,Spvc#spvcObj.spvcEntry}) of
        [] ->
            ok;
        Spvc1 ->
            case Spvc#spvcObj.currentState == Spvc1#spvcObj.currentState of
                true ->
                    spvcLib:increase_counter(spvcSaEtStat,Spvc),
                    case Spvc#spvcObj.spvcFrKey of
                        undefined ->
                            do_setup(HcId,Setup,Spvc#spvcObj.spvcRerCap);
                        FrKey ->
                            do_setup(HcId,Setup,FrKey)
                    end;
                _ ->
                    ok
            end
    end.

do_setup(HcId,Setup,Type) when Type == undefined; Type == false ->
    debug_disabled,
    ReturnData = {0,HcId},
    L3Data = {0,[HcId,Setup]},
    mdisp:msg(node(),{plcOperator,1,infinity},{HcId,{spvcI,ReturnData}},{ccI,l3_msg,[HcId,spvcI,L3Data]});
do_setup(HcId,Setup,true) ->
    debug_disabled,
    ReturnData = {0,HcId},
    L3Data = {0,[HcId,Setup]},
    mdisp:msg(node(),{plcOperator,1,infinity},{HcId,{spvcRerI,ReturnData}},{ccI,l3_msg,[HcId,spvcRerI,L3Data]});
do_setup(HcId,Setup,FrKey) ->
    debug_disabled,
    ReturnData = {0,HcId},
    L3Data = {0,[HcId,Setup]},
    mdisp:msg(node(),{plcOperator,1,infinity},{HcId,{spvcFrI,ReturnData}},{ccI,l3_msg,[HcId,spvcFrI,L3Data]}).

backoff_delay(Key) ->
    debug_disabled,
    Obj = spvcDataBase:db_read({spvcObj,Key}),
    Var = spvcDataBase:db_read({spvcFailures,atm_spvc}),
    {Delay,Flag} = case Obj#spvcObj.spvcRetryFailures of
                       0 ->
                           {100,no_alarm};
                       1 ->
                           {Obj#spvcObj.spvcRetryInterval,no_alarm};
                       _ ->
                           Table = get_backoff_table(Key,Obj),
                           Max_Delay = Var#spvcFailures.max_delay,
                           case Var#spvcFailures.delay_factor * Table#spvcBackoff.delay_time of
                               DelayValue when DelayValue < Max_Delay ->
                                   {DelayValue,no_alarm};
                               _ ->
                                   Org_Retry_Interval = Obj#spvcObj.spvcRetryInterval,
                                   if
                                       Org_Retry_Interval < Max_Delay ->
                                           spvcLib:send_spvcStillTryingAlarm(Key,Table#spvcBackoff.flag),
                                           {Max_Delay,alarm};
                                       true ->
                                           spvcLib:send_spvcStillTryingAlarm(Key,Table#spvcBackoff.flag),
                                           {Org_Retry_Interval,alarm}
                                   end
                           end
                   end,
    ets:insert(spvcBackoff,#spvcBackoff{key = Key,
                            delay_time = Delay,
                            flag = Flag}),
    round(Delay).

get_backoff_table(Index,Spvc) ->
    case ets:lookup(spvcBackoff,Index) of
        [Obj] ->
            Obj;
        _ ->
            #spvcBackoff{key = Spvc#spvcObj.spvcEntry,
                         delay_time = Spvc#spvcObj.spvcRetryInterval,
                         flag = no_alarm}
    end.

set_fr_atm_iw_admin_state(FrEndPoint,up,Spvc) ->
    ok;
set_fr_atm_iw_admin_state(FrEndPoint,NewStatus,Spvc) ->
    ok.

forced_release(FrEndPoint) ->
    FrPerm = spvcDataBase:db_read({spvcFr,FrEndPoint}),
    case FrPerm of
        [] ->
            {error,no_fr_spvc};
        _ ->
            Key = FrPerm#spvcFr.spvcFrAtmEntry,
            Spvc = spvcDataBase:db_read({spvcObj,Key}),
            SpvcFrObj = spvcDataBase:db_read({spvcFrPerm,FrEndPoint}),
            case SpvcFrObj#spvcFrPerm.spvcFrConnect of
                3 ->
                    SpvcTpToHcId = read_spvcTpToHcId(Key),
                    Release = spvcEncode:encode_cc_release(31),
                    spvcManager:release_un(b_side,SpvcTpToHcId#spvcTpToHcId.hcId,Release,Spvc);
                _ ->
                    {error,target_not_owned_by_this_connection}
            end
    end.
