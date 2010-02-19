DS-EquipmentUser-CommonFunctionOrig-TransmissionPath DEFINITIONS AUTOMATIC TAGS ::=
BEGIN
	
IMPORTS	AllocateTransmissionPathRejectReason, 
	Bandwidth, 
	BandwidthReducedInformation,
	CallType, 
	ConnectionIdentifier, 
	ConnectionInformation, 
	ConnectTransmissionPathRejectReason,
	DeallocateTransmissionPathRejectReason,
	DisconnectTransmissionPathRejectReason,
	RelationToUser, 
	UserIdentifier FROM CommonDataTypes ;

EquipmentUserCommonFunctionOrigTransmissionPathMessages ::= CHOICE
{
	allocateTransmissionPathEU		AllocateTransmissionPathEU,
	allocateTransmissionPathConfirmedUE	AllocateTransmissionPathConfirmedUE,	-- Reply
	allocateTransmissionPathRejectedUE	AllocateTransmissionPathRejectedUE,	-- Reply
	connectTransmissionPathEU		ConnectTransmissionPathEU,
	connectTransmissionPathConfirmedUE	ConnectTransmissionPathConfirmedUE,	-- Reply
	connectTransmissionPathRejectedUE	ConnectTransmissionPathRejectedUE,	-- Reply
	deallocateTransmissionPathEU		DeallocateTransmissionPathEU,
	deallocateTransmissionPathConfirmedUE	DeallocateTransmissionPathConfirmedUE,	-- Reply
	deallocateTransmissionPathRejectedUE	DeallocateTransmissionPathRejectedUE,	-- Reply
	disconnectTransmissionPathEU		DisconnectTransmissionPathEU,
	disconnectTransmissionPathConfirmedUE	DisconnectTransmissionPathConfirmedUE,	-- Reply
	disconnectTransmissionPathRejectedUE	DisconnectTransmissionPathRejectedUE,	-- Reply
	...
}



-- ----------------------------------
--
-- Allocate transmission path
--
--
-- ----------------------------------

AllocateTransmissionPathEU ::= SEQUENCE
{
	callType					CallType,
	bandwidth					Bandwidth,
	destinationUserIdentifiers			SEQUENCE OF UserIdentifier,
	sourceConnectionInformation			ConnectionInformation,
	relationToSourceUser				RelationToUser,
	...
}


--
-- @param	bandwidthReducedInformation	Mandatory if bandwidth has been reduced.
--
--

AllocateTransmissionPathConfirmedUE ::= SEQUENCE
{
	connectionIdentifier					ConnectionIdentifier,
	bandwidthReducedInformation				BandwidthReducedInformation OPTIONAL,
	...
}


AllocateTransmissionPathRejectedUE ::= SEQUENCE
{
	allocateTransmissionPathRejectReason			AllocateTransmissionPathRejectReason,
	...
}


-- ----------------------------------
--
-- Connect transmission path
--
-- ----------------------------------

ConnectTransmissionPathEU  ::= SEQUENCE
{
	connectionIdentifier				ConnectionIdentifier,
	...
}

ConnectTransmissionPathConfirmedUE  ::= SEQUENCE
{
	...
}


ConnectTransmissionPathRejectedUE  ::= SEQUENCE
{
	connectTransmissionPathRejectReason			ConnectTransmissionPathRejectReason,
	...
}


-- ----------------------------------
--
-- Deallocate transmission path
--
-- ----------------------------------

DeallocateTransmissionPathEU ::= SEQUENCE
{
	connectionIdentifier				ConnectionIdentifier,
	...
}


DeallocateTransmissionPathConfirmedUE ::= SEQUENCE
{
	...
}


DeallocateTransmissionPathRejectedUE ::= SEQUENCE
{
	deallocateTransmissionPathRejectReason			DeallocateTransmissionPathRejectReason,
	...
}


-- ----------------------------------
--
-- Disconnect transmission path
--
-- ----------------------------------

DisconnectTransmissionPathEU ::= SEQUENCE
{
	connectionIdentifier				ConnectionIdentifier,
	...
}


DisconnectTransmissionPathConfirmedUE ::= SEQUENCE
{
	...
}


DisconnectTransmissionPathRejectedUE ::= SEQUENCE
{
	disconnectTransmissionPathRejectReason			DisconnectTransmissionPathRejectReason,
	...
}

END

