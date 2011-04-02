
%%
%% WAP Port Number Definitions (WDP Appendix B.)
%%

-define(WAP_PORT_WTA_CL_SEC,   2805).
-define(WAP_PORT_WTA_CO_SEC,   2923).
-define(WAP_PORT_PUSH_CL,      2948).
-define(WAP_PORT_PUSH_CL_SEC,  2949).

-define(WAP_PORT_CL,           9200).
-define(WAP_PORT_CO,           9201).
-define(WAP_PORT_CL_SEC,       9202).
-define(WAP_PORT_CO_SEC,       9203).
-define(WAP_PORT_VCARD,        9204).
-define(WAP_PORT_VCAL,         9205).
-define(WAP_PORT_VCARD_SEC,    9206).
-define(WAP_PORT_VCAL_SEC,     9207).

-define(WAP_PORT_RINGTONE,     5505).
-define(WAP_PORT_OPER_LOGO,    5506).
-define(WAP_PORT_CLI_LOGO,     5507).

%%
%% WDP Bearer Type Assignments (WDP Appendix C.)
%%

%%
%% Names after the tag WAP_BEARER_ is [network]_[bearer_type]_[address_type]
%%
-define(WAP_BEARER_ANY_ANY_IPV4, 16#00).
-define(WAP_BEARER_ANY_ANY_IPV6, 16#01).
-define(WAP_BEARER_GSM_USSD_ANY, 16#02).
-define(WAP_BEARER_GSM_SMS_GSMMSISDN, 16#03).
-define(WAP_BEARER_ANSI136_GUTS_ANSI136MSISDN, 16#04).
-define(WAP_BEARER_IS95CDMA_SMS_IS637MSISDN, 16#05).
-define(WAP_BEARER_IS95CDMA_CSD_IPV4, 16#06).
-define(WAP_BEARER_IS95CDMA_PACKETDATA_IPV4, 16#07).
-define(WAP_BEARER_ANSI136_CSD_IPV4, 16#08).
-define(WAP_BEARER_ANSI136_PACKETDATA_IPV4, 16#09).
-define(WAP_BEARER_GSM_CSD_IPV4, 16#0a).
-define(WAP_BEARER_GSM_GPRS_IPV4, 16#0b).
-define(WAP_BEARER_GSM_USSD_IPV4, 16#0c).
-define(WAP_BEARER_AMPS_CDPD_IPV4, 16#0d).
-define(WAP_BEARER_PDC_CSD_IPV4, 16#0e).
-define(WAP_BEARER_PDC_PACKETDATA_IPV4, 16#0f).
-define(WAP_BEARER_IDEN_SMS_IDENMSISDN, 16#10).
-define(WAP_BEARER_IDEN_CSD_IPV4, 16#11).
-define(WAP_BEARER_IDEN_PACKETDATA_IPV4, 16#12).
-define(WAP_BEARER_PAGINGNETWORK_FLEX_FLEXMSISDN, 16#13).
-define(WAP_BEARER_PHS_SMS_PHSMSISDN, 16#14).
-define(WAP_BEARER_PHS_CSD_IPV4, 16#15).
-define(WAP_BEARER_GSM_USSD_GSMSERVICECODE, 16#16).
-define(WAP_BEARER_TETRA_SDS_TETRAITSI, 16#17).
-define(WAP_BEARER_TETRA_SDS_TETRAMSISDN, 16#18).
-define(WAP_BEARER_TETRA_PACKETDATA_IPV4, 16#19).
-define(WAP_BEARER_PAGINGNETWORK_REFLEX_REFLEXMSISDN, 16#1a).
-define(WAP_BEARER_GSM_USSD_GSMMSISDN, 16#1b).
-define(WAP_BEARER_MOBITEX_MPAK_MAN, 16#1c).
-define(WAP_BEARER_ANSI136_GHOST_GSMMSISDN, 16#1d).

-record(wdp_address,
	{
	  bearer,
	  address,
	  portnum
	 }).

-record(wdp_sap_info,
	{
	  mtu,   %% max transmission unit (bytes)
	  mru    %% max receive unit (bytes)
	 }).

%%
%% Source and destination address are wdp_addresses
%%
-record(wdp_socket_pair,
	{
	  source,
	  destination
	 }).

-record(wdp_local_port,
	{
	  port,   %% wdp "socket"
	  sap,    %% source address
	  user,   %% WDP user process
	  monitor %% monitor on WDP user
	 }).

-record(wdp_local_sap,
	{
	  sap,    %% source address
	  port    %% wdp "socket"
	 }).
