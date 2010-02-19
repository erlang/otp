SwCDR DEFINITIONS
     IMPLICIT TAGS ::= 


BEGIN

EXPORTS
    SwCDR;

SwCDR			::= CHOICE
{
	origSvcCallRecord		[0] OrigSvcCallRecord,
	termSvcCallRecord		[1] TermSvcCallRecord
}

--OrigSvcCallRecord			::= SET
OrigSvcCallRecord			::= SEQUENCE
{
	callCorrelationId	[0] INTEGER ,
	chargingIndicator	[1] ChargingIndicator,
	sequenceNo		[2] INTEGER ,
	callingParty		[3] CallingNumber,
	calledParty		[4] CalledNumber,
	connectedNumber		[5] ConnectedNumber,
	startDate		[6] StartDate,
	startTime		[7] StartTime,
	duration		[8] CallDuration ,
--	bearerClass		[] BearerClass ,
	trafficType		[9] TrafficType ,
	qosFwd			[10] QoSClass ,
	qosBkwd			[11] QoSClass ,
	forwardPcrClp0		[12] CellRate ,
	forwardPcrClp01		[13] CellRate ,
	backwardPcrClp0		[14] CellRate ,
	backwardPcrClp01	[15] CellRate ,
	forwardScrClp0		[16] CellRate ,
	forwardScrClp01		[17] CellRate ,
	backwardScrClp0		[18] CellRate ,
	backwardScrClp01	[19] CellRate ,
	forwardMcrClp0		[20] CellRate ,
	forwardMcrClp01		[21] CellRate ,
	backwardMcrClp0		[22] CellRate ,
	backwardMcrClp01	[23] CellRate ,
	forwardMbsClp0		[24] CellRate ,
	forwardMbsClp01		[25] CellRate ,
	forwardBEI		[26] INTEGER ,
	backwardBEI		[27] INTEGER ,
	forwardTagging		[28] INTEGER ,
	backwardTagging		[29] INTEGER ,
--	egressCellrate0		[] INTEGER,
--	egressCellrate01	[] INTEGER,
	ingressCellrate0	[30] INTEGER ,
--	ingressCellrate01	[] INTEGER ,
	ingressCellrate1	[31] INTEGER ,
	connectionConfig	[32] UserPlaneConnection OPTIONAL
--	causeForTerm		[33] CauseForTerm OPTIONAL
}

--TermSvcCallRecord			::=	SET
TermSvcCallRecord			::= SEQUENCE
{
	callCorrelationId	[0] INTEGER ,
	chargingIndicator	[1] ChargingIndicator,
	sequenceNo		[2] INTEGER ,
	callingParty		[3] CallingNumber,
	calledParty		[4] CalledNumber,
	connectedNumber		[5] ConnectedNumber,
	startDate		[6] StartDate,
	startTime		[7] StartTime,
	duration		[8] CallDuration ,
--	bearerClass		[] BearerClass ,
	trafficType		[9] TrafficType ,
	qosFwd			[10] QoSClass ,
	qosBkwd			[11] QoSClass ,
	forwardPcrClp0		[12] CellRate ,
	forwardPcrClp01		[13] CellRate ,
	backwardPcrClp0		[14] CellRate ,
	backwardPcrClp01	[15] CellRate ,
	forwardScrClp0		[16] CellRate ,
	forwardScrClp01		[17] CellRate ,
	backwardScrClp0		[18] CellRate ,
	backwardScrClp01	[19] CellRate ,
	forwardMcrClp0		[20] CellRate ,
	forwardMcrClp01		[21] CellRate ,
	backwardMcrClp0		[22] CellRate ,
	backwardMcrClp01	[23] CellRate ,
	forwardMbsClp0		[24] CellRate ,
	forwardMbsClp01		[25] CellRate ,
	forwardBEI		[26] INTEGER ,
	backwardBEI		[27] INTEGER ,
	forwardTagging		[28] INTEGER ,
	backwardTagging		[29] INTEGER ,
--	egressCellrate0		[] INTEGER ,
--	egressCellrate01	[] INTEGER ,
	ingressCellrate0	[30] INTEGER ,
--	ingressCellrate01	[] INTEGER ,
	ingressCellrate1	[31] INTEGER ,
	connectionConfig	[32] UserPlaneConnection OPTIONAL
--	causeForTerm		[33] CauseForTerm OPTIONAL
}

ChargingIndicator		::=	INTEGER
{
	origCallRecord		(0),
	termCallRecord		(1)
}

CallingNumber			::=	OCTET STRING (SIZE (12))
	-- BCD encoded representation of the number.
	-- Contains: TypeOfNumber, NumberingPlanInformation
	-- and either an E.164 number or a NSAP style of number,
	-- including a possible subaddress.
CalledNumber			::=	OCTET STRING (SIZE (20))
	-- BCD encoded representation of the number.
	-- Contains: TypeOfNumber, NumberingPlanInformation,
	-- PresentationIndicator, ScreeningIndicator
	-- and either an E.164 number or a NSAP style of number,
	-- including a possible subaddress.

ConnectedNumber			::=	OCTET STRING (SIZE (12))
	-- BCD encoded representation of the number.
	-- Contains: TypeOfNumber, NumberingPlanInformation,
	-- PresentationIndicator, ScreeningIndicator
	-- and either an E.164 number or a NSAP style of number,
	-- including a possible subaddress.


QoSClass			::= INTEGER
	-- Explicit values ToBeDefined,
	-- until then: value received in SETUP-msg

--BearerClass			::=	INTEGER
--{
--	bcobA			(0),
--	bcobC			(1),
--	bcobX			(2)
--}
TrafficType			::=	INTEGER
{
	noIndication		(0),
	abr			(1),
	cbr			(2),
	vbr			(3),
	vbrrt			(4),
	vbrnrt			(5),
	ubr			(6)
}

--TimingRequirements		::=	INTEGER
--{
--	noIndication		(0),
--	endToEndRequired	(1),
--	endToEndNotRequired	(2)
--}

--ClippingSusceptibility		::=	INTEGER
--{
--	notSusceptible		(0),
--	susceptible		(1)
--}
UserPlaneConnection		::=	 INTEGER
{
	pointToPoint		(0),
	pointToMultipoint	(1)
}

--AALParameters			::=	INTEGER		AAL Type only
--{
--	userDefined		(0),
--	aal1			(1),
--	aal2			(2),
--	aal34			(3),
--	aal5			(5)
--}

CellRate			::=	INTEGER
	-- Value range not less than 2^24.

-- BurstSize			::= ToBeDefined

-- TaggingRequest		::= ToBeDefined
--Timestamp			::=	OCTET STRING (SIZE (11))
	-- The contents of this field is a compact form of 
	-- the UTCTime format, containing local time plus
	-- an offset to universal time.
	-- The compact format is YYMMDDhhmmssdddShhmm, where:
	-- YY		= year,			00-99, BCD encoded
	-- MM		= month,		01-12, BCD encoded
	-- DD		= day,			01-31, BCD encoded
	-- hh		= hour,			00-23, BCD encoded
	-- mm		= minute,		00-59, BCD encoded
	-- ss		= second, 		00-59, BCD encoded
	-- ddd		= millisecond, 000-999, BCD encoded
	--    		  and rightjustified as "0ddd"
	-- S		= sign, 		"+"/"-", ASCII encoded

StartDate			::=	OCTET STRING (SIZE (8))

StartTime			::=	OCTET STRING (SIZE (6))

CallDuration			::=	INTEGER
--	Expressed as number of millseconds

Cellrate			::=	INTEGER
--	Value range 0-2^64
CauseForTerm			::=	INTEGER
{
	unsuccessfulCallAttempt	(0),
	abnormalTermination	(1)
}

END

