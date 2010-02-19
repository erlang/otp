CommonDataTypes DEFINITIONS AUTOMATIC TAGS ::=
BEGIN
-- @prop 	dataType
-- @descr	This types only purpose is to avoid OSS compiler warning : Duplicate PDU tag
-- @
CommonDataTypeWrapper ::= CHOICE 
{
	wrapAddAnalysisRejectReason	AddAnalysisRejectReason,
	wrapAddServiceToServiceProfileRejectReason	AddServiceToServiceProfileRejectReason,
	wrapAddUserIdentifiersRejectReason	AddUserIdentifiersRejectReason,
	wrapAdmissionRejectReason	AdmissionRejectReason,
	wrapAlertingUUIE	AlertingUUIE,
	wrapAllocateTransmissionPathRejectReason	AllocateTransmissionPathRejectReason,
	wrapAnalyseRejectReason	AnalyseRejectReason,
	wrapAvailabilityOfEquipment	AvailabilityOfEquipment,
	wrapBandwidth	Bandwidth,
	wrapBandwidthReducedInformation	BandwidthReducedInformation,
	wrapBandwidthReducedReason	BandwidthReducedReason,
	wrapBandwidthRejectReason	BandwidthRejectReason,
	wrapBasicCallCategories	BasicCallCategories,
	wrapBearerCapability	BearerCapability,
	wrapCallInformation	CallInformation,
	wrapCallModel	CallModel,
	wrapCallProceedingUUIE	CallProceedingUUIE,
	wrapCallReference	CallReference,
	wrapCallServices	CallServices,
	wrapCallState	CallState,
	wrapCallType	CallType,
	wrapCause	Cause,
	wrapCauseValue	CauseValue,
	wrapChangeServiceAndStatusRejectReason	ChangeServiceAndStatusRejectReason,
	wrapCheckServiceRejectReason	CheckServiceRejectReason,
	wrapCoding	Coding,
	wrapConferenceGoal	ConferenceGoal,
	wrapConferenceIdentifier	ConferenceIdentifier,
	wrapConnectTransmissionPathRejectReason	ConnectTransmissionPathRejectReason,
	wrapConnectUUIE	ConnectUUIE,
	wrapConnectionData	ConnectionData,
	wrapConnectionIdentifier	ConnectionIdentifier,
	wrapConnectionInformation	ConnectionInformation,
	wrapConnectionInformationOriginatingSide	ConnectionInformationOriginatingSide,
	wrapConnectionInformationTerminatingSide	ConnectionInformationTerminatingSide,
	wrapConnectionType	ConnectionType,
	wrapCreateEquipmentRepresentationRejectReason	CreateEquipmentRepresentationRejectReason,
	wrapCreateServiceAndStatusRejectReason	CreateServiceAndStatusRejectReason,
	wrapCreateServiceIdentifierRejectReason	CreateServiceIdentifierRejectReason,
	wrapDeallocateTransmissionPathRejectReason	DeallocateTransmissionPathRejectReason,
	wrapDetailedReasonAtom	DetailedReasonAtom,
	wrapDiagnostics	Diagnostics,
	wrapDisconnectTransmissionPathRejectReason	DisconnectTransmissionPathRejectReason,
	wrapDisengageReason	DisengageReason,
	wrapDisengageRejectReason	DisengageRejectReason,
	wrapDisplay	Display,
	wrapE164Identifier	E164Identifier,
	wrapEndToEndEndpointInformationServiceCallAcknowledge	EndToEndEndpointInformationServiceCallAcknowledge,
	wrapEndToEndEndpointInformationServiceCallActive	EndToEndEndpointInformationServiceCallActive,
	wrapEndToEndEndpointInformationServiceCallProgress	EndToEndEndpointInformationServiceCallProgress,
	wrapEndToEndEndpointInformationServiceCallSetup	EndToEndEndpointInformationServiceCallSetup,
	wrapEndToEndEndpointInformationServiceCallTermination	EndToEndEndpointInformationServiceCallTermination,
	wrapEndpointIdentifier	EndpointIdentifier,
	wrapEndpointRegistrationCategories	EndpointRegistrationCategories,
	wrapEndpointRegistrationRejectReason	EndpointRegistrationRejectReason,
	wrapEndpointType	EndpointType,
	wrapEndpointUnregistrationCategories	EndpointUnregistrationCategories,
	wrapEndpointUnregistrationRejectReason	EndpointUnregistrationRejectReason,
	wrapEquipmentAddressAN	EquipmentAddressAN,
	wrapEquipmentAddressLAN	EquipmentAddressLAN,
	wrapEquipmentRelatedInformation	EquipmentRelatedInformation,
	wrapEquipmentRelatedInformationIdentifier	EquipmentRelatedInformationIdentifier,
	wrapFacilityReason	FacilityReason,
	wrapFacilityUUIE	FacilityUUIE,
	wrapGatekeeperIdentifier	GatekeeperIdentifier,
	wrapGatekeeperInformation	GatekeeperInformation,
	wrapGatekeeperRejectReason	GatekeeperRejectReason,
	wrapGatewayInformation	GatewayInformation,
	wrapGetAnalysisRejectReason	GetAnalysisRejectReason,
	wrapGetEquipmentInformationRejectReason	GetEquipmentInformationRejectReason,
	wrapGetLANDataRejectReason	GetLANDataRejectReason,
	wrapGetPartyInformationRejectReason	GetPartyInformationRejectReason,
	wrapGetRejectReasonUser	GetRejectReasonUser,
	wrapGetServiceFromServiceProfileRejectReason	GetServiceFromServiceProfileRejectReason,
	wrapGetServiceProfileRejectReason	GetServiceProfileRejectReason,
	wrapGetServicesAndStatusRejectReason	GetServicesAndStatusRejectReason,
	wrapGetUserServiceInformationAndStatusRejectReason	GetUserServiceInformationAndStatusRejectReason,
	wrapH221NonStandard	H221NonStandard,
	wrapH310Information	H310Information,
	wrapH320Information	H320Information,
	wrapH321Information	H321Information,
	wrapH322Information	H322Information,
	wrapH323Information	H323Information,
	wrapH323InterfaceAddCallReferenceRejectReason	H323InterfaceAddCallReferenceRejectReason,
	wrapH323InterfaceAddCallRelatedDataRejectReason	H323InterfaceAddCallRelatedDataRejectReason,
	wrapH323InterfaceAddFixedTransportAddressDataRejectReason	H323InterfaceAddFixedTransportAddressDataRejectReason,
	wrapH323InterfaceAddKeysAndSetAttributesRejectReason	H323InterfaceAddKeysAndSetAttributesRejectReason,
	wrapH323InterfaceAdditionalKeys	H323InterfaceAdditionalKeys,
	wrapH323InterfaceAllocateResourceRejectReason	H323InterfaceAllocateResourceRejectReason,
	wrapH323InterfaceChangeKeysAndRelationsToUsersReject	H323InterfaceChangeKeysAndRelationsToUsersReject,
	wrapH323InterfaceCommonAttribute	H323InterfaceCommonAttribute,
	wrapH323InterfaceCommonAttributeIdentifier	H323InterfaceCommonAttributeIdentifier,
	wrapH323InterfaceCreateCallReferenceRejectReason	H323InterfaceCreateCallReferenceRejectReason,
	wrapH323InterfaceCreateRejectReason	H323InterfaceCreateRejectReason,
	wrapH323InterfaceDeallocateResourceRejectReason	H323InterfaceDeallocateResourceRejectReason,
	wrapH323InterfaceGetFixedTransportAddressDataRejectReason	H323InterfaceGetFixedTransportAddressDataRejectReason,
	wrapH323InterfaceGetOrRemoveCallRelatedDataRejectReason	H323InterfaceGetOrRemoveCallRelatedDataRejectReason,
	wrapH323InterfaceGetOrSetCommonRejectReason	H323InterfaceGetOrSetCommonRejectReason,
	wrapH323InterfaceGetOrSetInstanceRejectReason	H323InterfaceGetOrSetInstanceRejectReason,
	wrapH323InterfaceInstanceAttribute	H323InterfaceInstanceAttribute,
	wrapH323InterfaceInstanceAttributeIdentifier	H323InterfaceInstanceAttributeIdentifier,
	wrapH323InterfaceKey	H323InterfaceKey,
	wrapH323InterfaceKeyEndpointIdentifier	H323InterfaceKeyEndpointIdentifier,
	wrapH323InterfaceReduceBandwidthRejectReason	H323InterfaceReduceBandwidthRejectReason,
	wrapH323InterfaceRemoveCallReferenceRejectReason	H323InterfaceRemoveCallReferenceRejectReason,
	wrapH323InterfaceRemoveFixedTransportAddressDataRejectReason	H323InterfaceRemoveFixedTransportAddressDataRejectReason,
	wrapH323InterfaceRemoveKeysAndSetAttributesRejectReason	H323InterfaceRemoveKeysAndSetAttributesRejectReason,
	wrapH323InterfaceRemoveRejectReason	H323InterfaceRemoveRejectReason,
	wrapH324Information	H324Information,
	wrapHighLayerCompatibility	HighLayerCompatibility,
	wrapInterfaceRegistrationInformation	InterfaceRegistrationInformation,
	wrapLANAttribute	LANAttribute,
	wrapLANAttributeIdentifier	LANAttributeIdentifier,
	wrapLayer1ProtUserInfo	Layer1ProtUserInfo,
	wrapLocation	Location,
	wrapLocationRejectReason	LocationRejectReason,
	wrapLogicalConnectionPointIdentifier	LogicalConnectionPointIdentifier,
	wrapLowLayerCompatibility	LowLayerCompatibility,
	wrapMaximumNumberOfAllowedConnections	MaximumNumberOfAllowedConnections,
	wrapMaximumTotalBandwidth	MaximumTotalBandwidth,
	wrapMcuInformation	McuInformation,
	wrapNonStandardIdentifier	NonStandardIdentifier,
	wrapNonStandardMessage	NonStandardMessage,
	wrapNonStandardParameter	NonStandardParameter,
	wrapNumber	Number,
	wrapNumberOfTimesLANWasCrowded	NumberOfTimesLANWasCrowded,
	wrapNumberType	NumberType,
	wrapNumberingPlan	NumberingPlan,
	wrapObjectIdentifier	ObjectIdentifier,
	wrapPhysicalConnectionPointIdentifier	PhysicalConnectionPointIdentifier,
	wrapPid	Pid,
	wrapPreStringToRemoveInDestinationAddress	PreStringToRemoveInDestinationAddress,
	wrapProgressIndicator	ProgressIndicator,
	wrapProtocolIdentifier	ProtocolIdentifier,
	wrapQ931Timer301Value	Q931Timer301Value,
	wrapQ931Timer303Value	Q931Timer303Value,
	wrapQ954Details	Q954Details,
	wrapQseriesOptions	QseriesOptions,
	wrapRASMessageTimerValue	RASMessageTimerValue,
	wrapRTPSession	RTPSession,
	wrapRegistrationRejectReason	RegistrationRejectReason,
	wrapRegistrationStatus	RegistrationStatus,
	wrapRelationToEquipment	RelationToEquipment,
	wrapRelationToUser	RelationToUser,
	wrapReleaseCompleteReason	ReleaseCompleteReason,
	wrapReleaseCompleteUUIE	ReleaseCompleteUUIE,
	wrapReleaseInformation	ReleaseInformation,
	wrapRemoveAnalysisRejectReason	RemoveAnalysisRejectReason,
	wrapRemoveEquipmentRepresentationRejectReason	RemoveEquipmentRepresentationRejectReason,
	wrapRemoveServiceAndStatusRejectReason	RemoveServiceAndStatusRejectReason,
	wrapRemoveServiceFromServiceProfileRejectReason	RemoveServiceFromServiceProfileRejectReason,
	wrapRemoveServiceIdentifierRejectReason	RemoveServiceIdentifierRejectReason,
	wrapRepeatIndicator	RepeatIndicator,
	wrapRequestSeqNum	RequestSeqNum,
	wrapRequestedUserAndLinkedUserAreIdentical	RequestedUserAndLinkedUserAreIdentical,
	wrapServiceAndStatus	ServiceAndStatus,
	wrapServiceCallSetupRejectionInformation	ServiceCallSetupRejectionInformation,
	wrapServiceCallSetupRejectionReason	ServiceCallSetupRejectionReason,
	wrapServiceCallTerminationInformation	ServiceCallTerminationInformation,
	wrapServiceCallTerminationReason	ServiceCallTerminationReason,
	wrapServiceData	ServiceData,
	wrapServiceIdentifier	ServiceIdentifier,
	wrapServiceProfile	ServiceProfile,
	wrapSetEquipmentStatusRejectReason	SetEquipmentStatusRejectReason,
	wrapSetLANDataRejectReason	SetLANDataRejectReason,
	wrapSetUserAttributeData	SetUserAttributeData,
	wrapSetupUUIE	SetupUUIE,
	wrapStateOfEquipment	StateOfEquipment,
	wrapStateOfUser	StateOfUser,
	wrapStatusOfService	StatusOfService,
	wrapSubaddress	Subaddress,
	wrapSubaddressInformation	SubaddressInformation,
	wrapSubaddressType	SubaddressType,
	wrapSupportedProtocols	SupportedProtocols,
	wrapT120Information	T120Information,
	wrapTerminalInformation	TerminalInformation,
	wrapTerminationInitiatior	TerminationInitiatior,
	wrapTimeSlot	TimeSlot,
	wrapTransferCapability	TransferCapability,
	wrapTransferRate	TransferRate,
	wrapTransportAddress	TransportAddress,
	wrapTransportAddressInformation	TransportAddressInformation,
	wrapTransportChannelInformation	TransportChannelInformation,
	wrapTypeOfEquipment	TypeOfEquipment,
	wrapTypeOfFlowControl	TypeOfFlowControl,
	wrapTypeOfLAN	TypeOfLAN,
	wrapTypeOfRegistration	TypeOfRegistration,
	wrapTypeOfService	TypeOfService,
	wrapTypeOfUser	TypeOfUser,
	wrapUnknownMessageResponse	UnknownMessageResponse,
	wrapUnregistrationRejectReason	UnregistrationRejectReason,
	wrapUserAllocateResourceRejectReason	UserAllocateResourceRejectReason,
	wrapUserAttributeData	UserAttributeData,
	wrapUserAttributeIdentifier	UserAttributeIdentifier,
	wrapUserCreateRejectReason	UserCreateRejectReason,
	wrapUserDeallocateResourceRejectReason	UserDeallocateResourceRejectReason,
	wrapUserIdentifier	UserIdentifier,
	wrapUserIdentifierInformation	UserIdentifierInformation,
	wrapUserInformation	UserInformation,
	wrapUserInformationUUIE	UserInformationUUIE,
	wrapUserKey	UserKey,
	wrapUserOrEquipmentRelatedInformation	UserOrEquipmentRelatedInformation,
	wrapUserOrEquipmentRelatedInformationIdentifier	UserOrEquipmentRelatedInformationIdentifier,
	wrapUserRelatedInformation	UserRelatedInformation,
	wrapUserRelatedInformationIdentifier	UserRelatedInformationIdentifier,
	wrapUserRemoveRejectReason	UserRemoveRejectReason,
	wrapUserSetRejectReason	UserSetRejectReason,
	wrapUserSpecificInformation	UserSpecificInformation,
	wrapVendorIdentifier	VendorIdentifier,
	wrapVoiceInformation	VoiceInformation,
	...
}


-- ---------------------------------
--
--   AddAnalysisRejectReason
--
--   @prop	dataType
--
--   @descr	This parameter states the reason for the rejection.
--
--   @
--
-- ---------------------------------

AddAnalysisRejectReason ::= CHOICE
{
	analysisTableEntryAlreadyExist		NULL,
	undefined				NULL,
	...
}



-- ---------------------------------
--
--   AddServiceToServiceProfileRejectReason
--
--   @prop	dataType
--
--   @descr	This parameter states the reason for the rejection.
--
--   @
--
-- ---------------------------------

AddServiceToServiceProfileRejectReason ::= CHOICE
{
	keyNotValid				NULL,
	serviceAlreadyExist			NULL,
	undefined				NULL,
	...
}



-- ---------------------------------
--
--   AddUserIdentifiersRejectReason
--
--   @prop	dataType
--
--   @descr	This parameter states the reason for the rejection.
--
--   @
--
-- ---------------------------------

AddUserIdentifiersRejectReason ::= CHOICE
{
	userIdentifierExist			NULL,
	undefined				NULL,
	...
}



-- ---------------------------------
--
--   AdmissionRejectReason
--
--   @prop	dataType
--
--   @descr
--
--   @
--
-- ---------------------------------

AdmissionRejectReason ::= CHOICE
{
	calledPartyNotRegistered	NULL,		
	invalidPermission		NULL,	
	requestDenied			NULL,	
	undefinedReason			NULL,
	callerNotRegistered		NULL,
	routeCallToGatekeeper		NULL,
	invalidEndpointIdentifier	NULL,
	resourceUnavailable		NULL,
	...
}


-- ---------------------------------
--
--   AlertingUUIE
--
--   @prop	dataType
--
--   @descr	
--
--   @
--
-- ---------------------------------

AlertingUUIE ::= SEQUENCE
{
	protocolIdentifier		ProtocolIdentifier,		
	destinationEndpointType 	EndpointType,	-- destinationInfo		
	destinationH245Address		TransportAddress OPTIONAL,	-- h245Address
	...
}


-- ---------------------------------
--
--   AllocateTransmissionPathRejectReason
--
--   @prop	dataType
--
--   @descr	Reason for the rejection.
--
--   @
--
-- ---------------------------------

AllocateTransmissionPathRejectReason ::= CHOICE
{
	calledUserNotAvailable		NULL,
	calledUserUnknown		NULL,
	permissionDenied		NULL,
	resourcesNotAvailable		NULL,
	undefined			NULL,
	...
}



-- ---------------------------------
--
--   AnalyseRejectReason
--
--   @prop	dataType
--
--   @descr	This parameter states the reason for the rejection.
--
--   @
--
-- ---------------------------------

AnalyseRejectReason ::= CHOICE
{
	noMatchingEntryFound			NULL,
	undefined				NULL,
	...
}



-- ---------------------------------
--
--   AvailabilityOfEquipment
--
--   @prop	dataType
--
--   @descr	
--
--   @
--
-- ---------------------------------

AvailabilityOfEquipment ::= CHOICE
{
	available		NULL,
	notAvailable		NULL,
	...
}



-- ---------------------------------
--
--   Bandwidth
--
--   @prop	dataType
--
--   @descr	States the bandwidth to be used in 100 bps.
--
--   @
--
-- ---------------------------------

Bandwidth ::= INTEGER ( 1.. 4294967295 )



-- ---------------------------------
--
--   BandwidthReducedInformation
--
--   @prop	dataType
--
--   @descr	States information related to the recuction of the bandwidth.
--
--   @
--
-- ---------------------------------

BandwidthReducedInformation ::= SEQUENCE
{
	allocatedBandwidth 	Bandwidth,
	bandwidthReducedReason 	BandwidthReducedReason,
	...
}



-- ---------------------------------
--
--   BandwidthReducedReason
--	@prop 	dataType
--   	@descr	Reason for the rejection.
--	@						
--
-- ---------------------------------

BandwidthReducedReason ::= CHOICE
{
	bandwidthLimited				NULL,
	bandwidthAdaptedToOriginatingEndpoint		NULL,
	originBandwidthBarredDueToCategories		NULL,
	undefined					NULL,
	...
}


-- ---------------------------------
--
--   BandwidthRejectReason
--
--   @prop	dataType
--
--   @descr
--
--   @
--
-- ---------------------------------

BandwidthRejectReason ::= CHOICE
{
	notBound		NULL,	
	invalidConferenceID	NULL,	
	invalidPermission	NULL,	
	insufficientResources	NULL,	
	invalidRevision		NULL,
	undefinedReason		NULL,
	...
} 


-- ---------------------------------
--
--   BasicCallCategories
--
--   @prop	dataType
--
--   @descr	Categories for the service basic call.
--
--   @
-- ---------------------------------

BasicCallCategories ::= SEQUENCE
{
	...				-- So far, no specific categories identified
}

-- ---------------------------------
--
--   BearerCapability
--
--   @prop	dataType
--
--   @descr	Origin: Q931
--
--   @
--
-- ---------------------------------

BearerCapability ::= SEQUENCE
{
	transferCapability 	TransferCapability,
	transferRate		TransferRate,
	layer1ProtUserInfo	Layer1ProtUserInfo,
	rateMultiplier		INTEGER (0..127),
	...
}



-- ---------------------------------
--
--   CallInformation
--
--   @prop	dataType
--
--   @descr	
--
--   @
--
-- ---------------------------------

CallInformation ::= SEQUENCE
{				
	nonStandardData		NonStandardParameter OPTIONAL,
	callReference		CallReference,	-- callReferenceValue		
	conferenceID		ConferenceIdentifier,			
	originator		BOOLEAN OPTIONAL,				
	audio			SEQUENCE OF RTPSession OPTIONAL,
	video			SEQUENCE OF RTPSession OPTIONAL,
	data			SEQUENCE OF TransportChannelInformation OPTIONAL,
	h245			TransportChannelInformation,
	callSignaling		TransportChannelInformation,
	callType		CallType,				
	bandwidth		Bandwidth,	-- bandWidth		
	callModel		CallModel,
	...
}

-- ---------------------------------
--
--   CallModel
--
--   @prop	dataType
--
--   @descr Type of callmodel used i.e routed via gatekeeper or not
--
--   @
--
-- ---------------------------------

CallModel ::= CHOICE
{
	gatekeeperRouted	NULL,
	direct			NULL,
	...
}


-- ---------------------------------
--
--   CallProceedingUUIE
--
--   @prop	dataType
--
--   @descr
--
--   @
--
-- ---------------------------------


CallProceedingUUIE ::= SEQUENCE
{
	protocolIdentifier		ProtocolIdentifier,				
	destinationEndpointType		EndpointType,	-- destinationInfo
	destinationH245Address		TransportAddress OPTIONAL,	-- h245Address
	...
}

-- ---------------------------------
--
--   PreStringToRemoveInDestinationAddress
--
--   @prop	dataType
--
--   @descr	states the call reference that identifies a specific call.
--		Origin: H.225.0 CallReferenceValue.
--
--   @
--
-- ---------------------------------

CallReference	::=	INTEGER (0..65535)


-- ---------------------------------
--
--   CallServices
--
--   @prop	dataType
--
--   @descr
--
--   @
--
-- ---------------------------------

CallServices ::= SEQUENCE
{
	q932Full	BOOLEAN,
	q951Full	BOOLEAN, 
	q952Full	BOOLEAN, 
	q953Full	BOOLEAN, 
	q955Full	BOOLEAN, 
	q956Full	BOOLEAN, 
	q957Full	BOOLEAN, 	
	q954Info	Q954Details,	
	...
}


-- ---------------------------------
--
--   CallType
--
--   @prop	dataType
--
--   @descr
--
--   @
--
-- ---------------------------------

CallState ::= CHOICE
{
	null				NULL,
	callInit			NULL,
	overlapSending			NULL,
	outgoingCallProceeding		NULL,
	callDelivered			NULL,
	callPresent			NULL,
	callReceived			NULL,
	connectRequest			NULL,
	incomingCallProceeding		NULL,
	active				NULL,
	disconnectRequest		NULL,
	disconnectIndication		NULL,
	releaseRequest			NULL,
	facilityRequest			NULL,
	overlapReceiving		NULL,
	restartRequest			NULL,
	restart				NULL,
	 ...     
}


-- ---------------------------------
--
--   CallType
--
--   @prop	dataType
--
--   @descr
--
--   @
--
-- ---------------------------------

CallType ::= CHOICE
{	
	pointToPoint		NULL,		
	oneToN			NULL,		
	nToOne			NULL,		
	nToN			NULL,		
	...
}

-- ---------------------------------
--
--   Cause
--
--   @prop	dataType
--
--   @descr	Origin: Q931
--
--   @
--
-- ---------------------------------

Cause ::= SEQUENCE
{
	coding			Coding,
	location		Location,
	value			CauseValue,
	diagnostics		Diagnostics,
	...
}


-- ---------------------------------
--
--   CauseValue
--
--   @prop	dataType
--
--   @descr	Origin: Q931
--
--   @
--
-- ---------------------------------

CauseValue ::= CHOICE
{
	unassignedNumber			 	NULL, -- 1
	noRouteToSpecifiedTransitNetwork		NULL, -- 2
	noRouteToDestination				NULL, -- 3
	channelUnacceptable				NULL, -- 6
	normalClearing					NULL, -- 16
	userBusy					NULL, -- 17
	noUserResponding				NULL, -- 18
	noAnswereFromUser				NULL, -- 19
	portableNotAvailable				NULL, -- 20
	callRejected					NULL, -- 21
	numberChanged					NULL, -- 22
	destinationOutOfOrder				NULL, -- 27
	invalidNumberFormat				NULL, -- 28
	facilityRequestRejected				NULL, -- 29
	responseToStatusEnquiry				NULL, -- 30
	normalUnspecified				NULL, -- 31
	noCircuitChannelAvailable			NULL, -- 34
	networkOutOfOrder				NULL, -- 38
	temporaryFailure				NULL, -- 41
	switchingEquipmentCongestion			NULL, -- 42
	accessInformationDiscarded			NULL, -- 43
	requestedCircuitChannelNotAvailable		NULL, -- 44
	resourceUnavailableUnspecified			NULL, -- 47
	qualityOfServiceUnavailable			NULL, -- 49
	notSubscribedToRequestedFacility		NULL, -- 50
	bearerCapabilityNotAuthorized			NULL, -- 57
	bearerCapabilityNotPresentlyAvailable		NULL, -- 58
	serviceOrOptionNotAvailableUnspecified		NULL, -- 63, 79
	bearerCapabilityNotImplemented			NULL, -- 65
	channelTypeNotImplemented			NULL, -- 66
	requestedFacilityNotImplemented			NULL, -- 69
	onlyRestrictedDigitalInformationBcIsAvailable	NULL, -- 70
	invalidCallReferenceValue			NULL, -- 81
	incompatibleDestination				NULL, -- 88
	invalidTransitNetworkSelection			NULL, -- 91
	invalidMessageUnspecified			NULL, -- 95
	mandatoryInformationElementIsMissing		NULL, -- 96
	messageTypeNonexistingOrNotimplemented		NULL, -- 97
	messageNotCompatibleOrImplemented		NULL, -- 98
	informationElementNonExisting			NULL, -- 99
	invalidInformationElementContents		NULL, -- 100
	messageNotCompatibleWithCallState		NULL, -- 101
	recoveryOnTimerExpiry				NULL, -- 102
	protocolErrorUnspecified			NULL, -- 111
	interworkingUnspecified				NULL, -- 127
	...
}


-- ---------------------------------
--
--   ChangeServiceAndStatusRejectReason
--
--   @prop	dataType
--
--   @descr	Reason for the rejection.
--
--   @
--
-- ---------------------------------

ChangeServiceAndStatusRejectReason ::= CHOICE
{
	identifierOfServiceNotKnown		NULL,
	userNotKnown				NULL,
	undefined				NULL,
	...
}



-- ---------------------------------
--
--   CheckServiceRejectReason
--
--   @prop	dataType
--
--   @descr	Reason for the rejection.
--
--   @
--
-- ---------------------------------

CheckServiceRejectReason ::= CHOICE
{
	deniedDueToInteraction			NULL,
	deniedDueToCategories			NULL,
	undefined				NULL,
	userNotKnown				NULL,
	...
}


-- ---------------------------------
--
--   Coding
--
--   @prop	dataType
--
--   @descr	Origin: Q931
--
--   @
--
-- ---------------------------------

Coding ::= CHOICE
{	
	ccitt					NULL,
	ecma					NULL,
	national				NULL,
	network					NULL,
 	...     
}


-- ---------------------------------
--
--   ConferenceGoal
--
--   @prop	dataType
--
--   @descr	Type of call setup desire	
--
--   @
--
-- ---------------------------------

ConferenceGoal ::= CHOICE
{
	create		NULL,
	join		NULL,
	invite		NULL,
	...
}



-- ---------------------------------
--
--   ConferenceIdentifier
--
--   @prop	dataType
--
--   
--
--   @
--
-- ---------------------------------

ConferenceIdentifier	::= 	OCTET STRING (SIZE (16))


-- ---------------------------------
--
--   ConnectTransmissionPathRejectReason
--
--   @prop	dataType
--
--   @descr	Reason for the rejection.
--
--   @
--
-- ---------------------------------

ConnectTransmissionPathRejectReason ::= CHOICE
{
	resourcesNotAllocated		NULL,
	switchFailure			NULL,
	undefined			NULL,
	...
}



-- ---------------------------------
--
--   ConnectUUIE
--
--   @prop	dataType
--
--   @descr
--
--   @
--
-- ---------------------------------

ConnectUUIE ::= SEQUENCE
{
	protocolIdentifier		ProtocolIdentifier,				
	destinationH245Address		TransportAddress OPTIONAL,	-- h245Address
	destinationEndpointType		EndpointType,	-- destinationInfo
	conferenceIdentifier		ConferenceIdentifier,	-- conferenceID
	...
}



-- ---------------------------------
--
--   ConnectionData
--
--   @prop	dataType
--
--   @descr	This parameter holds connection data that are specific for   
--   		certain types of Equipments.
--   @
--
-- ---------------------------------

ConnectionData ::= CHOICE
{
	timeSlotInformation			SEQUENCE OF TimeSlot,
	...
}



-- ---------------------------------
--
--   ConnectionIdentifier
--
--   @prop	dataType
--
--   @descr	Identifier to the connection handler instance.
--
--   @
--
-- ---------------------------------

ConnectionIdentifier ::= ObjectIdentifier


-- ---------------------------------
--
--   ConnectionInformation
--
--   @prop	dataType
--
--   @descr	This parameter specifies information that are of interest for 
--   		the functionallity handled by component Connection Handler. 
--   @
--
-- ---------------------------------

ConnectionInformation ::= SEQUENCE
{
	logicalConnectionPointIdentifier	LogicalConnectionPointIdentifier,
	connectionData				ConnectionData OPTIONAL,
	...
}



-- ---------------------------------
--
-- ConnectionInformationOriginatingSide
--
-- @prop	dataType
--
-- @descr	Contains connection information that shall be used for the originating side of the connection.
--
-- @
--
-- ---------------------------------

ConnectionInformationOriginatingSide ::= SEQUENCE
{
	bandwidth				Bandwidth,
	callType				CallType,
	originatorConnectionInformation		ConnectionInformation,
	terminatorConnectionInformation		ConnectionInformation,
	...
}



-- ---------------------------------
--
-- ConnectionInformationTerminatingSide
--
-- @prop	dataType
--
-- @descr	Contains connection information that shall be used for the terminating side of the connection.
--
-- @
--
-- ---------------------------------

ConnectionInformationTerminatingSide ::= SEQUENCE
{
	connectionIdentifier			ConnectionIdentifier,
	originatorConnectionInformation		ConnectionInformation,
	...
}



-- ---------------------------------
--
--   ConnectionType
--
--   @prop	dataType
--
--   @descr	States the type of connection.
--
--   @
--
-- ---------------------------------

ConnectionType ::= CHOICE
{
	pointToPoint			NULL,
	oneToN				NULL,
	nToOne				NULL,
	nToN				NULL,
	...
}



-- ---------------------------------
--
--   CreateEquipmentRepresentationRejectReason
--
--   @prop	dataType
--
--   @descr	This reason for rejection.
--
--   @
--
-- ---------------------------------

CreateEquipmentRepresentationRejectReason ::= CHOICE
{
	equipmentRepresentationAlreadyExist	NULL,
	undefined				NULL,
	...
}



-- ---------------------------------
--
--   CreateServiceAndStatusRejectReason
--
--   @prop	dataType
--
--   @descr	Reason for the rejection.
--
--   @
--
-- ---------------------------------

CreateServiceAndStatusRejectReason ::= CHOICE
{
	undefined				NULL,
	...
}



-- ---------------------------------
--
--   CreateServiceIdentifierRejectReason
--
--   @prop	dataType
--
--   @descr	Reason for the rejection.
--
--   @
--
-- ---------------------------------

CreateServiceIdentifierRejectReason ::= CHOICE
{
	keyNotKnown				NULL,
	undefined				NULL,
	...
}



-- ---------------------------------
--
--   DeallocateTransmissionPathRejectReason
--
--   @prop	dataType
--
--   @descr	Reason for the rejection.
--
--   @
--
-- ---------------------------------

DeallocateTransmissionPathRejectReason ::= CHOICE
{
	resourcesNotAllocated		NULL,
	undefined			NULL,
	...
}



-- ---------------------------------
--
--   DetailedReasonAtom
--
--	@prop 	dataType
--   	@descr	This data type indicates the release information of a forced drop 
--   		during a call.
--	@						
--
-- ---------------------------------

DetailedReasonAtom ::= CHOICE
{
	internalDataMissmatch				NULL,
	destinationUserIdentifierNotKnown		NULL,
	rejectedDueToCategories				NULL,
	rejectedDueToResources				NULL,
	failedToOpenDestinationCallSignallingPort	NULL,
	theRequestedServiceIsNotSupported		NULL,
	undefined					NULL,
	...
}

-- ---------------------------------
--
--   Diagnostics
--
--   @prop	dataType
--
--   @descr	Origin: Q931
--
--   @
--
-- ---------------------------------

Diagnostics ::= INTEGER(1..127)


-- ---------------------------------
--
--   DisconnectTransmissionPathRejectReason
--
--   @prop	dataType
--
--   @descr	Reason for the rejection.
--
--   @
--
-- ---------------------------------

DisconnectTransmissionPathRejectReason ::= CHOICE
{
	resourcesNotAllocated		NULL,
	switchFailure			NULL,
	switchNotConnected		NULL,
	undefined			NULL,
	...
}


-- ---------------------------------
--
--   DisengageReason
--
--	@prop 	dataType
--   	@descr	the reason why a change was requested by the gatekeeper or the terminal. 
--	@
-- ---------------------------------

DisengageReason	::= CHOICE
{
	forcedDrop		NULL,	
	normalDrop		NULL,	
	undefinedReason		NULL,
	...
}


-- ---------------------------------
--
--   DisengageRejectReason
--
--   @prop	dataType
--
--   @descr
--
--   @
--
-- ---------------------------------

DisengageRejectReason ::= CHOICE
{
	notRegistered		NULL,	
	requestToDropOther	NULL, 	
	...
}
-- ---------------------------------
--
--   Display
--
--   @prop	dataType
--
--   @descr	Origin: Q931
--
--   @
--
-- ---------------------------------

Display ::= OCTET STRING (SIZE(1..82))
	


-- ---------------------------------
--
--   E164Identifier
--
--   @prop	dataType
--
--   @descr	Identifier for the user identifier of the type E.164.
--
--   @
--
-- ---------------------------------

E164Identifier ::= IA5String (SIZE (1..128)) (FROM ("0123456789#*,"))


-- ---------------------------------
--
--   EndToEndEndpointInformationServiceCallAcknowledge
--
--   @prop	dataType
--
--   @descr	Information that shall be sent end to end.
--
--   @
--
-- ---------------------------------

EndToEndEndpointInformationServiceCallAcknowledge ::= SEQUENCE
{
	bearerCapability			BearerCapability OPTIONAL,
	highLayerCompatibility			HighLayerCompatibility OPTIONAL,
	progressIndicator			ProgressIndicator OPTIONAL,
	userToUserQ931Information		UserInformation OPTIONAL,
	userToUserH323AcknowledgeInformation	AlertingUUIE OPTIONAL,
	...
}



-- ---------------------------------
--
--   EndToEndEndpointInformationServiceCallActive
--
--   @prop	dataType
--
--   @descr	Information that shall be sent end to end.
--
--   @
--
-- ---------------------------------

EndToEndEndpointInformationServiceCallActive ::= SEQUENCE
{
	bearerCapability			BearerCapability OPTIONAL,
	highLayerCompatibility			HighLayerCompatibility OPTIONAL,
	lowLayerCompatibility			LowLayerCompatibility OPTIONAL,
	progressIndicator			ProgressIndicator OPTIONAL,
	userToUserQ931Information		UserInformation OPTIONAL,
	userToUserH323ActiveInformation		ConnectUUIE OPTIONAL,
	...
}



-- ---------------------------------
--
--   EndToEndEndpointInformationServiceCallProgress
--
--   @prop	dataType
--
--   @descr	Information that shall be sent end to end.
--
--   @
--
-- ---------------------------------

EndToEndEndpointInformationServiceCallProgress ::=SEQUENCE
{
	cause					Cause OPTIONAL,
	highLayerCompatibility			HighLayerCompatibility OPTIONAL,
	progressIndicator			ProgressIndicator OPTIONAL,
	userToUserQ931Information		UserInformation OPTIONAL,
	...
}




-- ---------------------------------
--
--   EndToEndEndpointInformationServiceCallSetup
--
--   @prop	dataType
--
--   @descr	Information that shall be sent end to end.
--
--   @
--
-- ---------------------------------

EndToEndEndpointInformationServiceCallSetup ::=SEQUENCE
{
	bearerCapability			BearerCapability OPTIONAL,
	calledNumber				Number OPTIONAL,
	calledSubaddress			Subaddress OPTIONAL,
	callingNumber				Number OPTIONAL,
	callingSubaddress			Subaddress OPTIONAL,
	highLayerCompatibility			HighLayerCompatibility OPTIONAL,
	lowLayerCompatibility			LowLayerCompatibility OPTIONAL,
	progressIndicator			ProgressIndicator OPTIONAL,
	repeatIndicator				RepeatIndicator OPTIONAL,
	userToUserQ931Information		UserInformation OPTIONAL,
	userToUserH323SetupInformation		SetupUUIE OPTIONAL,
	...
}



-- ---------------------------------
--
--   EndToEndEndpointInformationServiceCallTermination
--
--   @prop	dataType
--
--   @descr	Information that shall be sent end to end.
--
--   @
--
-- ---------------------------------

EndToEndEndpointInformationServiceCallTermination ::=SEQUENCE
{
	cause					Cause OPTIONAL,
	progressIndicator			ProgressIndicator OPTIONAL,
	userToUserQ931Information		UserInformation OPTIONAL,
	userToUserH323TerminationInformation	ReleaseCompleteUUIE OPTIONAL,
	...
}



-- ---------------------------------
--
--   EndpointIdentifier
--
--   @prop	dataType
--
--
--   @
--
-- ---------------------------------

EndpointIdentifier	::=	BMPString (SIZE(1..128)) -- change from SIZE(128)


-- ---------------------------------
--
--   EndpointRegistrationCategories
--
--   @prop	dataType
--
--   @descr	Categories for the service endpoint registration.
--
--   @
-- ---------------------------------

EndpointRegistrationCategories ::= SEQUENCE
{
	...				-- So far, no specific categories identified
}



-- ---------------------------------
--
--   EndpointRegistrationRejectReason
--
--   @prop	dataType
--
--
--   @
--
-- ---------------------------------

EndpointRegistrationRejectReason ::= CHOICE
{
	attemptToChangeEndpoint		NULL,
	requestedUserNotKnown		NULL,
	endpointTypeNotKnown		NULL,
	undefined			NULL,
	...
}



-- ---------------------------------
--
--   EndpointType
--
--   @prop	dataType
--
--   @descr
--
--   @
--
-- ---------------------------------

EndpointType ::= SEQUENCE
{	
	nonStandardData		NonStandardParameter OPTIONAL,
	vendor			VendorIdentifier OPTIONAL,
	gatekeeper		GatekeeperInformation OPTIONAL,
	gateway			GatewayInformation OPTIONAL,
	mcu			McuInformation OPTIONAL,  
	terminal		TerminalInformation OPTIONAL,
	mc			BOOLEAN,		
	undefinedNode		BOOLEAN,	
	...
}


-- ---------------------------------
--
--   EndpointUnregistrationCategories
--
--   @prop	dataType
--
--   @descr	Categories for the service endpoint unregistration.
--
--   @
-- ---------------------------------

EndpointUnregistrationCategories ::= SEQUENCE
{
	...				-- So far, no specific categories identified
}



-- ---------------------------------
--
--   EndpointUnregistrationRejectReason
--
--   @prop	dataType
--
--   @descr	
--
--   @
--
-- ---------------------------------

EndpointUnregistrationRejectReason ::= CHOICE
{
	permissionDenied		NULL,
	userNotKnown			NULL,
	undefined			NULL,
	...
}



-- ---------------------------------
--
-- EquipmentAddressAN
--
-- @prop	dataType
--
-- @descr	States the address for a certain equipment connected
--		to the Access Node.
-- @
--
-- ---------------------------------

EquipmentAddressAN ::= SEQUENCE
{
	--TBD by SEA,
	...
}



-- ---------------------------------
--
-- EquipmentAddressLAN
--
-- @prop	dataType
--
-- @descr	States the transport address for a certain equipment 
--
-- @
--
-- ---------------------------------

EquipmentAddressLAN ::= SEQUENCE
{
	transportAddresses			SEQUENCE OF TransportAddress,
	...
}



-- ---------------------------------
--
--   EquipmentRelatedInformation
--
--   @prop	dataType
--
--   @descr	Contains the retreived data.
--
--   @
--
-- ---------------------------------

EquipmentRelatedInformation ::= CHOICE
{
	logicalConnectionPointIdentifier	LogicalConnectionPointIdentifier,
	registrationStatus			RegistrationStatus,
	stateOfEquipment			StateOfEquipment,
	typeOfEquipment				TypeOfEquipment,
	...
}



-- ---------------------------------
--
--   EquipmentRelatedInformationIdentifier
--
-- 
-- @prop 	dataType
--
-- @descr	This parameter specifies different types of data 
--   		that are specific to a certain equipment. 
--
-- @
-- ---------------------------------

EquipmentRelatedInformationIdentifier ::= CHOICE
{
	logicalConnectionPointIdentifier	NULL,
	registrationStatus			NULL,
	stateOfEquipment			NULL,
	typeOfEquipment				NULL,
	...
}



-- ---------------------------------
--
--   FacilityReason
--
--   @prop	dataType
--
--
--   @
--
-- ---------------------------------

FacilityReason ::= CHOICE
{
		routeCallToGatekeeper	NULL,		
		callForwarded		NULL,
		routeCallToMC		NULL,
		undefinedReason		NULL,
		...
}


-- ---------------------------------
--
--   FacilityUUIE
--
--   @prop	dataType
--
--
--   @
--
-- ---------------------------------

FacilityUUIE ::= SEQUENCE
{
	protocolIdentifier			ProtocolIdentifier,
	alternativeH245Address			TransportAddress OPTIONAL,	-- alternativeAddress
	alternativeUserIdentifierInformation	UserIdentifierInformation OPTIONAL,	-- alternativeAliasAddress
	conferenceIdentifier			ConferenceIdentifier OPTIONAL,	-- conferenceID
	facilityReason				FacilityReason,	-- reason
	...
}


-- ---------------------------------
--
--   GatekeeperIdentifier
--
--   @prop	dataType
--
--   @descr	
--
--   @
--
-- ---------------------------------

GatekeeperIdentifier	::=	BMPString (SIZE(1..128))


-- ---------------------------------
--
--   GatekeeperInformation
--
--   @prop	dataType
--
--   @descr
--
--   @
--
-- ---------------------------------

GatekeeperInformation ::= SEQUENCE 
{
	nonStandardData	NonStandardParameter OPTIONAL,
	...
}


-- ---------------------------------
--
--   GatekeeperRejectReason
--
--   @prop	dataType
--
--   @descr	
--
--   @
--
-- ---------------------------------

GatekeeperRejectReason ::= CHOICE
{
	resourceUnavailable	NULL,
	terminalExcluded	NULL,	
	invalidRevision		NULL,
	undefinedReason		NULL,
	...
}


-- ---------------------------------
--
--   GatewayInformation
--
--   @prop	dataType
--
--   @descr
--
--   @
--
-- ---------------------------------

GatewayInformation ::= SEQUENCE
{
	protocol		SEQUENCE OF SupportedProtocols OPTIONAL,
	nonStandardData		NonStandardParameter OPTIONAL,
	...
}


-- ---------------------------------
--
--   GetAnalysisRejectReason
--
--   @prop	dataType
--
--   @descr	This parameter states the reason for the rejection.
--
--   @
--
-- ---------------------------------

GetAnalysisRejectReason ::= CHOICE
{
	noDataStored				NULL,
	undefined				NULL,
	...
}



-- ---------------------------------
--
--   GetEquipmentInformationRejectReason
--
--   @prop	dataType
--
--   @descr	Reason for the rejection.
--
--   @
--
-- ---------------------------------

GetEquipmentInformationRejectReason ::= CHOICE
{
	equipmentUnknown			NULL,
	undefined				NULL,
	...
}



-- ---------------------------------
--
--   GetLANDataRejectReason
--
--   @prop	dataType
--
--   @descr	This reason for rejection.
--
--   @
--
-- ---------------------------------

GetLANDataRejectReason ::= CHOICE
{
	noDataStored		NULL,
	undefined		NULL,
	...
}



-- ---------------------------------
--
--   GetPartyInformationRejectReason
--
--   @prop	dataType
--
--   @descr	Reason for the rejection.
--
--   @
--
-- ---------------------------------

GetPartyInformationRejectReason ::= CHOICE
{
	noEquipmentAvailable			NULL,
	userNotKnown				NULL,
	undefined				NULL,
	...
}



-- ---------------------------------
--
--   GetRejectReasonUser
--
--   @prop	dataType
--
--   @descr	This parameter states the reason for the rejection.
--
--   @
--
-- ---------------------------------

GetRejectReasonUser ::= CHOICE
{
	keyNotKnown				NULL,
	undefined				NULL,
	...
}



-- ---------------------------------
--
--   GetServiceFromServiceProfileRejectReason
--
--   @prop	dataType
--
--   @descr	This parameter states the reason for the rejection.
--
--   @
--
-- ---------------------------------

GetServiceFromServiceProfileRejectReason ::= CHOICE
{
	keyNotValid				NULL,
	serviceDoNotExist			NULL,
	undefined				NULL,
	...
}



-- ---------------------------------
--
--   GetServiceProfileRejectReason
--
--   @prop	dataType
--
--   @descr	
--
--   @
--
-- ---------------------------------

GetServiceProfileRejectReason ::= CHOICE
{
	userNotKnown				NULL,
	undefined				NULL,
	...
}



-- ---------------------------------
--
--   GetServicesAndStatusRejectReason
--
--   @prop	dataType
--
--   @descr	Reason for the rejection.
--
--   @
--
-- ---------------------------------

GetServicesAndStatusRejectReason ::= CHOICE
{
	userNotKnown				NULL,
	undefined				NULL,
	...
}



-- ---------------------------------
--
--   GetUserServiceInformationAndStatusRejectReason
--
--   @prop	dataType
--
--   @descr	Reason for the rejection.
--
--   @
--
-- ---------------------------------

GetUserServiceInformationAndStatusRejectReason ::= CHOICE
{
	undefined				NULL,
	userNotKnown				NULL,
	...
}



-- ---------------------------------
--
--   H221NonStandard
--	@prop 	dataType
--
--   	@descr	Gives non standard information about the standard protocol H.221.
--	@						
--
-- ---------------------------------

H221NonStandard ::= SEQUENCE
{	t35CountryCode		INTEGER(0..255),
	t35Extension		INTEGER(0..255),
	manufacturerCode	INTEGER(0..65535),
	...
}


-- ---------------------------------
--
--   H310Information
--	@prop 	dataType
--   	@descr	Gives detailed information about the standard protocol H.310.
--	@						
--
-- ---------------------------------

H310Information ::= SEQUENCE
{
	nonStandardData	NonStandardParameter OPTIONAL,
	...
}


-- ---------------------------------
--
--   	H320Information
--	@prop 	dataType
--
--   	@descr	Gives detailed information about the standard protocol H.320.
--	@						
--
-- ---------------------------------

H320Information ::= SEQUENCE
{
	nonStandardData	NonStandardParameter OPTIONAL,
	...
}


-- ---------------------------------
--
--   H321Information
--
--	@prop 	dataType
--   	@descr	Gives detailed information about the standard protocol H.321.
--	@						
--
-- ---------------------------------

H321Information ::= SEQUENCE
{
	nonStandardData	NonStandardParameter OPTIONAL,
	...
}


-- ---------------------------------
--
--   H322Information
--
--	@prop 	dataType
--   	@descr	Gives detailed information about the standard protocol H.322.
--	@						
--
-- ---------------------------------

H322Information ::= SEQUENCE
{
	nonStandardData	NonStandardParameter OPTIONAL,
	...
}


-- ---------------------------------
--
--   H323Information
--
--	@prop 	dataType
--   	@descr	Gives detailed information about the standard protocol H.323.
--	@						
--
-- ---------------------------------

H323Information ::= SEQUENCE
{
	nonStandardData	NonStandardParameter OPTIONAL,
	...
}


-- ---------------------------------
--
--   H323InterfaceAddCallReferenceRejectReason
--
--	@prop 	dataType
--   	@descr	Reason for the rejection.
--	@						
--
-- ---------------------------------

H323InterfaceAddCallReferenceRejectReason ::= CHOICE
{
	keyNotValid				NULL,
	requestedCallReferenceAlreadyInUse	NULL,
	undefined				NULL,
	...
}



-- ---------------------------------
--
--   H323InterfaceAddCallRelatedDataRejectReason
--
--	@prop 	dataType
--   	@descr	Reason for the rejection.
--	@						
--
-- ---------------------------------

H323InterfaceAddCallRelatedDataRejectReason ::= CHOICE
{
	callReferenceNotValid			NULL,
	keyNotValid				NULL,
	callRelatedDataAlredyStored		NULL,
	undefined				NULL,
	...
}



-- ---------------------------------
--
--   H323InterfaceAddFixedTransportAddressDataRejectReason
--
--	@prop 	dataType
--   	@descr	Reason for the rejection.
--	@						
--
-- ---------------------------------

H323InterfaceAddFixedTransportAddressDataRejectReason ::= CHOICE
{
	fixedTransportAddressDataAlredyStored		NULL,
	undefined					NULL,
	...
}



-- ---------------------------------
--
--   H323InterfaceAddKeysAndSetAttributesRejectReason
--
--   @prop	dataType
--
--   @descr	This parameter states the reason for the rejection.
--
--   @
--
-- ---------------------------------

H323InterfaceAddKeysAndSetAttributesRejectReason ::= CHOICE
{
	existingKeyNotValid		NULL,
	newKeyAlreadyExists		NULL,
	newKeyNotValid			NULL,
	undefined			NULL,
	...
}



-- ---------------------------------
--
--   H323InterfaceAdditionalKeys
--
--	@prop 	dataType
--   	@descr	Additional keys for an instance of the type H.323Interface.
--	@
-- ---------------------------------

H323InterfaceAdditionalKeys ::= SEQUENCE
{
	endpointCallSignallingAddresses		SEQUENCE OF TransportAddress,
	endpointRASAddresses			SEQUENCE OF TransportAddress,
	...
}


-- ---------------------------------
--
--   H323InterfaceAllocateResourceRejectReason
--
--	@prop 	dataType
--   	@descr	Reason for the rejection.
--	@						
--
-- ---------------------------------

H323InterfaceAllocateResourceRejectReason ::= CHOICE
{
	callReferenceNotValid		NULL,
	keyNotValid			NULL,
	resourceNotAvailable		NULL,
	undefined			NULL,
	...
}



-- ---------------------------------
--
--   H323InterfaceChangeKeysAndRelationsToUsersReject
--
--   @prop	dataType
--
--   @descr	This parameter states the reason for the rejection.
--
--   @
--
-- ---------------------------------

H323InterfaceChangeKeysAndRelationsToUsersReject ::= CHOICE
{
	firstKeyNotValid		NULL,
	secondKeyNotValid		NULL,
	undefined			NULL,
	...
}



-- ---------------------------------
--
--   H323InterfaceCommonAttribute
--
--   @prop	dataType
--
--   @descr	This parameter contains the attributes which holds data
--		that are common for all objects of the type H.323Interface.
--
--   @
--
-- ---------------------------------

H323InterfaceCommonAttribute ::= CHOICE
{
	gatekeeperCallSignallingAddressData		CHOICE
	{
		gatekeeperCallSignallingAddresses	SEQUENCE OF TransportAddress,
		undefined				NULL,
		...
	},
	gatekeeperRASAddressInformation			CHOICE
	{
		gatekeeperRASAddressData		SEQUENCE
		{
			multicastRASAddress		TransportAddress,
			gatekeeperRASAddress		TransportAddress,
			...
		},
		undefined				NULL,
		...
	},
	q931Timer301Value				Q931Timer301Value,
	q931Timer303Value				Q931Timer303Value,
	rasMessageTimerValue				RASMessageTimerValue,
	...
}



-- ---------------------------------
--
--   H323InterfaceCommonAttributeIdentifier
--
--   @prop	dataType
--
--   @descr	This parameter contains the attribute identifiers of the
--		attributes which holds data that are common for all objects
--		of the type H.323Interface.
--
--   @
--
-- ---------------------------------

H323InterfaceCommonAttributeIdentifier ::= CHOICE
{
	gatekeeperCallSignallingAddresses	NULL,
	gatekeeperRASAddress			NULL,
	q931Timer301Value			NULL,
	q931Timer303Value			NULL,
	rasMessageTimerValue			NULL,
	...
}



-- ---------------------------------
--
--   H323InterfaceCreateCallReferenceRejectReason
--
--	@prop 	dataType
--   	@descr	Reason for the rejection.
--	@
-- ---------------------------------

H323InterfaceCreateCallReferenceRejectReason ::= CHOICE
{
	keyNotValid				NULL,
	noCallReferenceAvailable		NULL,
	undefined				NULL,
	...
}



-- ---------------------------------
--
--   H323InterfaceCreateRejectReason
--
--   @prop	dataType
--
--   @descr	This parameter states the reason for the rejection.
--
--   @
--
-- ---------------------------------

H323InterfaceCreateRejectReason ::= CHOICE
{
	keyAlreadyInUse			NULL,
	undefined			NULL,
	...
}



-- ---------------------------------
--
--   H323InterfaceDeallocateResourceRejectReason
--
--	@prop 	dataType
--   	@descr	Reason for the rejection.
--	@
-- ---------------------------------

H323InterfaceDeallocateResourceRejectReason ::= CHOICE
{
	resourceNotAllocated		NULL,
	callReferenceNotValid		NULL,
	keyNotValid			NULL,
	undefined			NULL,
	...
}



-- ---------------------------------
--
--   H323InterfaceGetFixedTransportAddressDataRejectReason
--
--	@prop 	dataType
--   	@descr	Reason for the rejection.
--	@
-- ---------------------------------

H323InterfaceGetFixedTransportAddressDataRejectReason ::= CHOICE
{
	noDataStoredForThisTransportAddress	NULL,
	noFixedTransportAddressDataStored	NULL,
	undefined				NULL,
	...
}



-- ---------------------------------
--
--   H323InterfaceGetOrRemoveCallRelatedDataRejectReason
--
--	@prop 	dataType
--   	@descr	Reason for the rejection.
--	@						
--
-- ---------------------------------

H323InterfaceGetOrRemoveCallRelatedDataRejectReason ::= CHOICE
{
	callReferenceNotValid		NULL,
	keyNotValid			NULL,
	noCallRelatedDataStored		NULL,
	undefined			NULL,
	...
}



-- ---------------------------------
--
--   H323InterfaceGetOrSetCommonRejectReason
--
--   @prop	dataType
--
--   @descr	This parameter states the reason for the rejection.
--
--   @
--
-- ---------------------------------

H323InterfaceGetOrSetCommonRejectReason ::= CHOICE
{
	undefined				NULL,
	...
}



-- ---------------------------------
--
--   H323InterfaceGetOrSetInstanceRejectReason
--
--   @prop	dataType
--
--   @descr	This parameter states the reason for the rejection.
--
--   @
--
-- ---------------------------------

H323InterfaceGetOrSetInstanceRejectReason ::= CHOICE
{
	keyNotValid				NULL,
	undefined				NULL,
	...
}



-- ---------------------------------
--
--   H323InterfaceInstanceAttribute
--
--   @prop	dataType
--
--   @descr	This parameter contains the attributes which holds data
--		that are specific for a h323Interface object.
--
--   @
--
-- ---------------------------------

H323InterfaceInstanceAttribute ::= CHOICE
{
	endpointCallSignallingAddresses			SEQUENCE OF TransportAddress,
	endpointRasAddresses				SEQUENCE OF TransportAddress,
	registrationStatus				RegistrationStatus,
	gatekeeperCallSignallingAddress			TransportAddress,
	maximumTotalBandwidthForInterface		Bandwidth,
	preStringsToRemoveInDestinationAddress		SEQUENCE OF PreStringToRemoveInDestinationAddress,
	relationToH2250CallSignalling			Pid,
	relationToUser					RelationToUser,
	typeOfEquipment					TypeOfEquipment,
	...
}



-- ---------------------------------
--
--   H323InterfaceInstanceAttributeIdentifier
--
--   @prop	dataType
--
--   @descr	This parameter contains the attribute identifiers of the
--		attributes which holds data that are specific for a
--		h323Interface object.
--
--   @
--
-- ---------------------------------

H323InterfaceInstanceAttributeIdentifier ::= CHOICE
{
	endpointCallSignallingAddresses			NULL,
	endpointRASAddresses				NULL,
	registrationStatus				NULL,
	gatekeeperCallSignallingAddress			NULL,
	maximumTotalBandwidthForInterface		NULL,
	preStringsToRemoveInDestinationAddress		NULL,
	relationToH2250CallSignalling			NULL,
	relationToUser					NULL,
	typeOfEquipment					NULL,
	...
}



-- ---------------------------------
--
--   H323InterfaceKey
--
--	@prop 	dataType
--   	@descr	Allowed keys for an instance of the type H.323Interface.
--	@
-- ---------------------------------

H323InterfaceKey ::= CHOICE
{
	endpointIdentifier			EndpointIdentifier,
	endpointCallSignallingAddresses		SEQUENCE OF TransportAddress,
	endpointRASAddresses			SEQUENCE OF TransportAddress,
	...
}


-- ---------------------------------
--
--   H323InterfaceKeyEndpointIdentifier
--
--   @descr	Allowed keys for an instance of the type H.323Interface.
--
-- ---------------------------------

H323InterfaceKeyEndpointIdentifier ::= SEQUENCE
{
	endpointIdentifier			EndpointIdentifier,
	...
}


-- ---------------------------------
--
--   H323InterfaceReduceBandwidthRejectReason
--
--	@prop 	dataType
--   	@descr	Reason for the rejection.
--	@						
--
-- ---------------------------------

H323InterfaceReduceBandwidthRejectReason ::= CHOICE
{
	bandwidthNotAllocated				NULL,
	callReferenceNotValid				NULL,
	keyNotValid					NULL,
	newBandwidthHigherThanAllocatedBandwidth	NULL,
	undefined					NULL,
	...
}



-- ---------------------------------
--
--   H323InterfaceRemoveCallReferenceRejectReason
--
--	@prop 	dataType
--   	@descr	Reason for the rejection.
--	@						
--
-- ---------------------------------

H323InterfaceRemoveCallReferenceRejectReason ::= CHOICE
{
	callReferenceNotStored		NULL,
	keyNotValid			NULL,
	undefined			NULL,
	...
}



-- ---------------------------------
--
--  	 H323InterfaceRemoveFixedTransportAddressDataRejectReason
--   	@prop dataType
--   	@descr	Reason for the rejection.
--	@
-- ---------------------------------

H323InterfaceRemoveFixedTransportAddressDataRejectReason ::= CHOICE
{
	noDataStoredForThisTransportAddress	NULL,
	undefined				NULL,
	...
}



-- ---------------------------------
--
--   H323InterfaceRemoveKeysAndSetAttributesRejectReason
--
--   @prop	dataType
--
--   @descr	This parameter states the reason for the rejection.
--
--   @
--
-- ---------------------------------

H323InterfaceRemoveKeysAndSetAttributesRejectReason ::= CHOICE
{
	keysNotValid			NULL,
	undefined			NULL,
	...
}



-- ---------------------------------
--
--   H323InterfaceRemoveRejectReason
--
--   @prop	dataType
--
--   @descr	This parameter states the reason for the rejection.
--
--   @
--
-- ---------------------------------

H323InterfaceRemoveRejectReason ::= CHOICE
{
	keyNotValid				NULL,
	serviceInProgress			NULL,
	undefined				NULL,
	...
}



-- ---------------------------------
--
--   H324Information
--	@prop 	dataType
--
--   	@descr	Gives detailed information about the standard protocol H.324.
--	@						
--
-- ---------------------------------

H324Information ::= SEQUENCE
{
	nonStandardData	NonStandardParameter OPTIONAL,
	...
}
-- 	@prop 	dataType
-- 	@descr	Origin: Q931
--	@						

HighLayerCompatibility ::= SEQUENCE
{
	...
}
-- ---------------------------------
--
--   	InterfaceRegistrationInformation
--	@prop 	dataType
--
--  	@descr	This parameter specifies the current registration status of an
--   		endpoints registration request.
--	@						
--
-- ---------------------------------

InterfaceRegistrationInformation ::= SEQUENCE
{
	isInterfaceRegistered	BOOLEAN,
	relationToH323User	EndpointIdentifier OPTIONAL,
	...
}	


-- ---------------------------------
--
--   LANAttribute
--
--   @prop	dataType
--   @descr	This parameter contains a LAN attribute value.
--   @
--
-- ---------------------------------

LANAttribute ::= CHOICE
{
	maximumTotalBandwidth					MaximumTotalBandwidth,
	maximumNumberOfAllowedConnections	MaximumNumberOfAllowedConnections,
	numberOfTimesLANWasCrowded			NumberOfTimesLANWasCrowded,
	typeOfFlowControl					TypeOfFlowControl,
	typeOfLAN							TypeOfLAN,
	...
}



-- ---------------------------------
--
--   LANAttributeIdentifier
--
--   @prop	dataType
--   @descr	This parameter contains a LAN attribute identifier.
--   @
--
-- ---------------------------------

LANAttributeIdentifier ::= CHOICE
{
	maximumTotalBandwidth			NULL,
	maximumNumberOfAllowedConnections	NULL,
	numberOfTimesLANWasCrowded		NULL,
	typeOfFlowControl			NULL,
	typeOfLAN				NULL,
	...
}

--	@prop 	dataType
-- 	@descr	Origin: Q931
--	@						

	Layer1ProtUserInfo ::= CHOICE			
	{
		g711u-law				NULL,
		g711a-law				NULL,
		h323VidephoneCall			NULL, -- the meaning of "5" in H323
		h221Andh242				NULL, -- the meaning of "5" in Q931
		...
	}-- 	@prop 	dataType
-- 	@descr	Origin: Q931
--	@						

	Location ::= CHOICE
	{
		user					NULL,
		localPrivateNetwork			NULL,
		localPublicNetwork			NULL,
		transitNetwork				NULL,
		remotePublicNetwork			NULL,
		remotePrivateNetwork			NULL,
		internationalNetwork			NULL,
		beyondInterworkingPoint			NULL,
	 	...     
	}

-- ---------------------------------
--
--   LocationRejectReason
--	@prop 	dataType
--
--  	@descr	
--	@
-- ---------------------------------

LocationRejectReason ::= CHOICE
{
	notRegistered		NULL,
	invalidPermission	NULL,	
	requestDenied		NULL,	
	undefinedReason		NULL,
	...
}


-- ---------------------------------
--
--   LogicalConnectionPointIdentifier
--
--	@prop 	dataType
--   	@descr	Identifier of the logical connection point.
--	@						
--
-- ---------------------------------

LogicalConnectionPointIdentifier	::=	INTEGER (0..65535)
--   
--  Created by  	:
--  Creation date 	:
--  Modified by 	:
--  Modification date 	:
--  Version 		:
-- 
--	@prop 	dataType
--	@descr	origin Q931
--	@

	LowLayerCompatibility ::= SEQUENCE
	{
	}

-- ---------------------------------
--
--   MaximumNumberOfAllowedConnections
--
--	@prop 	dataType
--   	@descr	States the maximum number of allowed connections.
--	@						
--
-- ---------------------------------

MaximumNumberOfAllowedConnections ::= CHOICE
{
	maximumNumberOfAllowedConnectionsValue		INTEGER ( 0.. 999999999),
	undefined					NULL,
	...
}



-- ---------------------------------
--
--   MaximumTotalBandwidth
--	@prop 	dataType
--   	@descr	States the maximum total bandwidth.
--	@
-- ---------------------------------

MaximumTotalBandwidth ::= CHOICE
{
	maximumTotalBandwidthValue		Bandwidth,
	undefined				NULL,
	...
}



-- ---------------------------------
--
--   McuInformation
--	@prop 	dataType
--
--   	@descr	Gives detailed information about the endpoint type, MCU.
--	@
-- ---------------------------------

McuInformation ::= SEQUENCE
{
	nonStandardData	NonStandardParameter OPTIONAL,
	...
}


-- ---------------------------------
--
--  	 NonStandardIdentifier
--	@prop 	dataType
--
--  	@descr
--	@						
--
-- ---------------------------------

NonStandardIdentifier ::= CHOICE
{
	object			OBJECT IDENTIFIER,
	h221NonStandard		H221NonStandard,
	...
}


-- ---------------------------------
--
--   NonStandardMessage
--	@prop 	dataType
--
--   	@descr	
--	@						
--
-- ---------------------------------

NonStandardMessage ::= SEQUENCE
{
	requestSeqNum		RequestSeqNum,
	nonStandardData		NonStandardParameter,
	...
}

-- ---------------------------------
--
--   	NonStandardParameter
--
--	@prop 	dataType
--	@
-- ---------------------------------

NonStandardParameter ::= SEQUENCE
{
	nonStandardIdentifier	NonStandardIdentifier,
	data			OCTET STRING,
	...
}



-- 	@prop 	dataType
-- 	@descr	Origin: Q931
-- 	@						

	Number ::= SEQUENCE
	{
		type			NumberType,
		indicator		NumberingPlan,
		number			IA5String (FROM ("0123456789#*")),
	 	...     
	}

-- ---------------------------------
--
--   NumberOfTimesLANWasCrowded
--	@prop 	dataType
--
--   	@descr	States the number of times the Lan has been crowded,
-- 		i.e. the endpoints has released the initiated call due to
--		heavy load in the LAN.
--	@						
--
-- ---------------------------------

NumberOfTimesLANWasCrowded ::= CHOICE
{
	numberOfTimesLANWasCrowdedValue		INTEGER ( 0.. 999999999),
	undefined				NULL,
	...
}

-- 	@prop 	dataType
-- 	@descr	Origin: Q931
--	@						

	NumberType 	::= CHOICE
	{
		unknown		NULL,
		international	NULL,
		national	NULL,
		network		NULL,
		local		NULL,
		abbreviated	NULL,
	 	...     
	}

-- 	@prop 	dataType
-- 	@descr	Origin: Q931
--	@						

	NumberingPlan	::= CHOICE
	{
		unknown		NULL,
		e164		NULL,
		data		NULL,
		telex		NULL,
		national	NULL,
		private		NULL,
	 	...     
	}



-- ---------------------------------
--
--   ObjectIdentifier
--
--	@prop 	dataType
--   	@descr	An identifier to a certain instance of an object.
--	@						
--
-- ---------------------------------

ObjectIdentifier ::= OCTET STRING



-- ---------------------------------
--
-- PhysicalConnectionPointIdentifier
--
-- @prop	dataType
-- @descr	Contains data that identifies a specific equipment instance. 
-- @
--
-- ---------------------------------

PhysicalConnectionPointIdentifier ::= CHOICE
{
	equipmentAN			EquipmentAddressAN,  -- Equipment connected to the Access Node.
	equipmentLAN			EquipmentAddressLAN, -- Equipment connected to the LAN.
	...
}


-- ---------------------------------
--
--   Pid
--	@prop 	dataType
--
--   	@descr	A process identifier.
--	@						
--
-- ---------------------------------

Pid ::= ObjectIdentifier



-- ---------------------------------
--
--   PreStringToRemoveInDestinationAddress
--
--   @prop	dataType
--
--   @descr	A pre-string that shall be removed when sending the destination address.
--
--   @
--
-- ---------------------------------

PreStringToRemoveInDestinationAddress ::= CHOICE
{
	e164		IA5String (SIZE (1..128)) (FROM ("0123456789,")),
	h323		BMPString (SIZE (1..256)),	
			-- h323 is Basic ISO/IEC 10646-1 (Unicode)
	...
}

-- 	@prop 	dataType
-- 	@descr	Origin: Q931
--	@	
					
	ProgressIndicator ::= SEQUENCE
	{
	}

-- ---------------------------------
--
--   ProtocolIdentifier
--
--	@prop 	dataType
--  	@descr
--	@						
--
-- ---------------------------------

ProtocolIdentifier	::=	OBJECT IDENTIFIER	


-- ---------------------------------
--
--   Q931Timer301Value
--	@prop 	dataType
--
--   	@descr	States the Q931 timer 301 value to be used in milli seconds.
--	@						
--
-- ---------------------------------

Q931Timer301Value ::= INTEGER ( 180000.. 360000 )



-- ---------------------------------
--
--   Q931Timer303Value
--
--	@prop 	dataType
--   	@descr	States the Q931 timer 303 value to be used in milli seconds.
--	@						
--
-- ---------------------------------

Q931Timer303Value ::= INTEGER ( 1000.. 10000 )



-- ---------------------------------
--
--   Q954Details
--
--	@prop 	dataType
--   	@descr
--	@						
--
-- ---------------------------------

Q954Details ::= SEQUENCE
{
	conferenceCalling	BOOLEAN,
	threePartyService	BOOLEAN,
	...
}


-- ---------------------------------
--
--   QseriesOptions
--
--	@prop 	dataType
--	@						
--
-- ---------------------------------

QseriesOptions			::=SEQUENCE
{
	q932Full		BOOLEAN,
	q951Full		BOOLEAN, 
	q952Full		BOOLEAN, 
	q953Full		BOOLEAN, 
	q955Full		BOOLEAN, 
	q956Full		BOOLEAN, 
	q957Full		BOOLEAN, 	
	q954Info		Q954Details,	
	...
}


-- ---------------------------------
--
--   RASMessageTimerValue
--
--	@prop 	dataType
--   	@descr	States the RAS message timer value to be used in milli seconds.
--	@						
--
-- ---------------------------------

RASMessageTimerValue ::= INTEGER ( 1000.. 10000 )



-- ---------------------------------
--
--   RTPSession
--
--	@prop 	dataType
--	@						
--
-- ---------------------------------

RTPSession ::= SEQUENCE
{
	rtpAddress		TransportChannelInformation,
	rtcpAddress		TransportChannelInformation,
	cname			PrintableString,
	ssrc			INTEGER (1.. 134217727), -- change from 4294967295 for erl 4.2
	sessionId		INTEGER (1..255),
	associatedSessionIds	SEQUENCE OF INTEGER (1..255),
	...
}


-- ---------------------------------
--
--   RegistrationRejectReason
--
--	@prop 	dataType
--   	@descr	Specifies the registration reject reason that are valid
--   		in the H.225.0 message RegistartionReject
--	@						--
-- ---------------------------------

RegistrationRejectReason ::= CHOICE
{
	discoveryRequired		NULL,	
	invalidRevision			NULL,
	invalidCallSignalAddress	NULL,
	invalidRasAddress		NULL,	
	duplicateAlias			UserIdentifierInformation,	
	invalidTerminalType		NULL,
	undefinedReason			NULL,
	transportNotSupported		NULL,	
	...
}


-- ---------------------------------
--
--   RegistrationStatus
--
--	@prop 	dataType
--  	@
--
-- ---------------------------------

RegistrationStatus ::= CHOICE
{
	notRegistered		NULL,
	registered		NULL,
	...
}



-- ---------------------------------
--
--   RelationToEquipment
--
--	@prop 	dataType
--   	@descr	Relation to the architecture component Equipment.
--	@						
--
-- ---------------------------------

RelationToEquipment ::= SEQUENCE
{
	relationToUser		RelationToUser,
	typeOfEquipment		TypeOfEquipment,
	...
}


-- ---------------------------------
--
--   RelationToUser
--
--	@prop 	dataType
--   	@descr	Relation to the architecture component User.
--	@						
--
-- ---------------------------------

RelationToUser ::= BMPString (SIZE(1..128))



-- ---------------------------------
--
--   	ReleaseCompleteReason
--
--	@prop 	dataType
--  	@descr	
--	@						
--
-- ---------------------------------

ReleaseCompleteReason ::= CHOICE
{
	noBandwidth		NULL,		
	gatekeeperResources	NULL,		
	unreachableDestination	NULL,     	
	destinationRejection	NULL,	
	invalidRevision		NULL,
	noPermission		NULL,		
	unreachableGatekeeper	NULL,				
	gatewayResources	NULL,
	badFormatAddress	NULL,
	adaptiveBusy		NULL,		
	inConf			NULL,		
	undefinedReason		NULL,
	...
}	






-- ---------------------------------
--
--   ReleaseCompleteUUIE
--	@prop 	dataType
--
--	@
-- ---------------------------------

ReleaseCompleteUUIE ::= SEQUENCE
{
	protocolIdentifier	ProtocolIdentifier,
	releaseCompleteReason 	ReleaseCompleteReason OPTIONAL,	-- reason
	...
}


-- ---------------------------------
--
--   ReleaseInformation
--
--	@prop 	dataType
--   	@descr	This data type is used to transfer the reason for the 
--   		rejection or release.
--	@						
--
-- ---------------------------------

ReleaseInformation ::= CHOICE
{
	forcedDrop	DetailedReasonAtom,
	normalDrop	NULL,
	...
} 


-- ---------------------------------
--
--   RemoveAnalysisRejectReason
--
--   @prop	dataType
--
--   @descr	This parameter states the reason for the rejection.
--
--   @
--
-- ---------------------------------

RemoveAnalysisRejectReason ::= CHOICE
{
	analysisTableEntryNotFound		NULL,
	undefined				NULL,
	...
}



-- ---------------------------------
--
--   RemoveEquipmentRepresentationRejectReason
--
--   @prop	dataType
--   @descr	This reason for rejection.
--   @
--
-- ---------------------------------

RemoveEquipmentRepresentationRejectReason ::= CHOICE
{
	invalidInputData			NULL,
	equipmentRepresentationDoesNotExist	NULL,
	other					NULL,
	...
}



-- ---------------------------------
--
--   	RemoveServiceAndStatusRejectReason
--
--	@prop 	dataType
--   	@descr	Reason for the rejection.
--	@						
--
-- ---------------------------------

RemoveServiceAndStatusRejectReason ::= CHOICE
{
	identifierOfServiceNotKnown		NULL,
	undefined				NULL,
	...
}



-- ---------------------------------
--
--   RemoveServiceFromServiceProfileRejectReason
--
--   @prop	dataType
--
--   @descr	This parameter states the service and its categories that
--		shall be added to a service profile.
--
--   @
--
-- ---------------------------------

RemoveServiceFromServiceProfileRejectReason ::= CHOICE
{
	keyNotValid				NULL,
	serviceDoNotExist			NULL,
	undefined				NULL,
	...
}



-- ---------------------------------
--
--   RemoveServiceIdentifierRejectReason
--
--	@prop 	dataType
--   	@descr	Reason for the rejection.
--	@
-- ---------------------------------

RemoveServiceIdentifierRejectReason ::= CHOICE
{
	keyNotKnown				NULL,
	serviceIdentifierDoNotExist		NULL,
	undefined				NULL,
	...
}

--   
--  Created by  	:
--  Creation date 	:
--  Modified by 	:
--  Modification date 	:
--  Version 		:
-- 
--	@prop 	dataType
--	@	

	RepeatIndicator ::= SEQUENCE
	{
	}

-- ---------------------------------
--
--   RequestSeqNum
--
--	@prop 	dataType
--   	@descr	
--	@
-- ---------------------------------

RequestSeqNum	::=	INTEGER (1..65535)



-- ---------------------------------
--
--   RequestedUserAndLinkedUserAreIdentical
--
--   @prop	dataType
--   @descr	This parameter indicates if the requested user and the user
--		linked to the requested endpoint are identical, not identical
--		or if this is undefined.
--   @
--
-- ---------------------------------

RequestedUserAndLinkedUserAreIdentical ::= CHOICE
{
	yes		NULL,
	no		NULL,
	undefined	NULL,
	...
}



-- ---------------------------------
--
--   	ServiceAndStatus
--	@prop 	dataType
--
--   	@descr	Information of a service and its state.
--	@						
--
-- ---------------------------------

ServiceAndStatus ::= SEQUENCE
{
	typeOfService		TypeOfService,
	status			StatusOfService,
	...
}



-- ---------------------------------
--
--   ServiceCallSetupRejectionInformation
--
--	@prop 	dataType
--   	@descr	Information related to the call setup rejection.
--	@						
--
-- ---------------------------------

ServiceCallSetupRejectionInformation ::= SEQUENCE
{
	terminationInitiatior	TerminationInitiatior,
	terminationReason	ServiceCallSetupRejectionReason,
	...
}



-- ---------------------------------
--
--   ServiceCallSetupRejectionReason
--
--	@prop 	dataType
--   	@descr	Reason for rejection.
--	@
-- ---------------------------------

ServiceCallSetupRejectionReason ::= CHOICE
{
	calledUserBusy			NULL,
	calledUserNotAvailable		NULL,
	destinationOutOfOrder		NULL,
	requestedServiceBarred		NULL,
	requestedServiceNotAvailable	NULL,
	requestedServiceNotSubscribed	NULL,
	resourceUnavailable		NULL,
	temporaryFailure		NULL,
	unassignedUserIdentifier	NULL,
	undefined			NULL,
	...
}



-- ---------------------------------
--
--   ServiceCallTerminationInformation
--	@prop 	dataType
--
--   	@descr	States information related to the termination.
--	@						
--
-- ---------------------------------

ServiceCallTerminationInformation ::= SEQUENCE
{
	terminationInitiation	TerminationInitiatior,
	terminationReason	ServiceCallTerminationReason,
	...
}


-- ---------------------------------
--
--   ServiceCallTerminationReason
--
--	@prop 	dataType
--   	@descr	Reason for termination.
--	@						
--
-- ---------------------------------

ServiceCallTerminationReason ::= CHOICE
{
	noAnswerFromCalledUser		NULL,
	normalTermination		NULL,
	resourceUnavailable		NULL,
	temporaryFailure		NULL,
	undefined			NULL,
	...
}



-- ---------------------------------
--
--   ServiceData
--
--	@prop 	dataType
--   	@descr	Contains the identified services and theirs categories
--   	@
-- ---------------------------------

ServiceData ::= CHOICE
{
	basicCall			BasicCallCategories,
	endpointRegistration		EndpointRegistrationCategories,
	endpointUnregistration		EndpointUnregistrationCategories,
	...
}

-- 	@prop 	dataType
--   	@descr	
--      @
--

	ServiceIdentifier	::=	INTEGER


-- ---------------------------------
--
--   	ServiceProfile
--
--	@prop 	dataType
--   	@descr	Contains services and data related to the services. 
--   	@
-- ---------------------------------

ServiceProfile ::= SEQUENCE
{
	serviceDataInformation		SEQUENCE OF ServiceData OPTIONAL,
	...
}



-- ---------------------------------
--
--   SetEquipmentStatusRejectReason
--
--   @prop	dataType
--
--
--   @
--
-- ---------------------------------

SetEquipmentStatusRejectReason ::= CHOICE
{
	userNotKnown		NULL,	
	undefined		NULL, 	
	...
}


-- ---------------------------------
--
--   SetLANDataRejectReason
--
--   @prop	dataType
--   @descr	This reason for rejection.
--   @
--
-- ---------------------------------

SetLANDataRejectReason ::= CHOICE
{
	invalidInputData	NULL,
	other			NULL,
	...
}



-- ---------------------------------
--
--   SetUserAttributeData
--
--   @prop	dataType
--
--   @descr	This parameter contains an User attribute value.
--
--   @
--
-- ---------------------------------

SetUserAttributeData ::= CHOICE
{
	maximumTotalBandwidth			Bandwidth,
	maximumBandwidthPerService		Bandwidth,
	stateOfUser				StateOfUser,
	typeOfUser				TypeOfUser,
	...
}



-- ---------------------------------
--
--   	SetupUUIE
--	@prop 	dataType
--	@						
--
-- ---------------------------------

SetupUUIE ::= SEQUENCE
{
	protocolIdentifier			ProtocolIdentifier,				
	sourceH245Address			TransportAddress OPTIONAL,	-- h245Address
	sourceUserIdentifierInformation		UserIdentifierInformation OPTIONAL,	-- sourceAddress
	sourceEndpointType			EndpointType,	-- sourceInfo					
	destinationUserIdentifierInformation		UserIdentifierInformation OPTIONAL,	-- destinationAddress
	destinationCallSignallingAddress		TransportAddress OPTIONAL,	-- destCallSignalAddress
	destinationExtraUserIdentifierInformation	UserIdentifierInformation OPTIONAL, -- destExtraCallInfo
	destinationExtraCallReference			SEQUENCE OF CallReference OPTIONAL,	-- destExtraCRV
	activeMC				BOOLEAN,					
	conferenceIdentifier			ConferenceIdentifier,	-- conferenceID
	conferenceGoal				ConferenceGoal,			
	callServices				CallServices  OPTIONAL,
	callType				CallType,
	...
}


-- ---------------------------------
--
--   StateOfEquipment
--
--	@prop 	dataType
--   	@descr	States the state of the equipment.
--	@						
--
-- ---------------------------------

StateOfEquipment ::= CHOICE
{
	blocked			NULL,		-- Equipment is blocked
	busy			NULL,		-- Equipment is busy, no more calls possible for moment
	available		NULL,		-- Equipment has reported itself as present and is ready for actions
	unregistered		NULL,		-- Equipment is not present
	...
}



-- ---------------------------------
--
--   StateOfUser
--
--	@prop 	dataType
--   	@descr	This parameter specifies the state of the user. 
--	@
-- ---------------------------------

StateOfUser ::= CHOICE
{
	absent					NULL,
	present					NULL,
	...
}



-- ---------------------------------
--
--   StatusOfService
--
--	@prop 	dataType
--   	@descr	States the state of the service.
--	@						
--
-- ---------------------------------

StatusOfService ::= CHOICE
{
	acknowledge		NULL,
	active			NULL,
	initiatied		NULL,
	...
}

-- @prop 	dataType
-- @descr	Origin: Q931
--	@						

	Subaddress ::= SEQUENCE
	{
		type			SubaddressType,
		indicator		BOOLEAN,
		address			SubaddressInformation,
	 	...     
	}
-- 	@prop 	dataType
-- 	@descr	Origin: Q931
-- 	@						

	SubaddressInformation 	::=	OCTET STRING (SIZE(1..23))
-- 	@prop 	dataType
-- 	@descr	Origin: Q931
--	@						

	SubaddressType	::= CHOICE
	{
		nsap		NULL,
		user		NULL,
	 	...     
	}

-- ---------------------------------
--
--   SupportedProtocols
--
--	@prop 	dataType
--   	@descr	Gives detailed information about protocols that are 
--		supported by the stated endpoint.
--	@
-- ---------------------------------

SupportedProtocols ::= CHOICE
{
	nonStandardData	NonStandardParameter,
	h310		H310Information,
	h320		H320Information,
	h321		H321Information,
	h322		H322Information,
	h323		H323Information,
	h324		H324Information,
	voice		VoiceInformation,
	t120Only	T120Information,
	...
}


-- ---------------------------------
--
--   T120Information
--
--	@prop 	dataType
--   	@descr	Gives detailed information about the standard protocol T.120
--	@
-- ---------------------------------

T120Information ::= SEQUENCE
{
	nonStandardData	NonStandardParameter OPTIONAL,
	...
}


-- ---------------------------------
--
--   TerminalInformation
--	@prop 	dataType
--
--	@						
--
-- ---------------------------------

TerminalInformation ::= SEQUENCE 
{
	nonStandardData	NonStandardParameter OPTIONAL,
	...
}


-- ---------------------------------
--
--   TerminationInitiatior
--
--	@prop 	dataType
--  	@descr	States who initiated the termination.
--	@						
--
-- ---------------------------------

TerminationInitiatior ::= CHOICE
{
	endpoint		NULL,
	serviceNode		NULL,
	...
}



-- ---------------------------------
--
--   TimeSlot
--
--   @prop	dataType
--   @descr	This parameter contains the identity of the time slot used 
--     		for the connection.
--   @
--
-- ---------------------------------

TimeSlot ::= INTEGER

-- 	@prop 	dataType
-- 	@descr	Origin: Q931
--	@						

	TransferCapability ::= CHOICE
	{
		speech					NULL,
		unrestrictedDigital			NULL,
		restrictedDigital			NULL,
		audio3point1kHz				NULL,
		unrestrictedWithTonesAndAnnouncements	NULL,
		video					NULL,
	 	...     
	}

-- 	@prop 	dataType
-- 	@descr	Origin: Q931
--	@						

	TransferRate ::= CHOICE
	{
		packedMode				NULL,
		r64kbps					NULL,
		r2x64kbps				NULL,
		r384kbps				NULL,
		r1536kbps				NULL,
		r1920kbps				NULL,
		multirate				NULL,
	 	...     
	}

-- ---------------------------------
--
--   TransportAddress
--
--	@prop 	dataType
--   	@descr	The transport address.
--	@						
--
-- ---------------------------------

TransportAddress ::= CHOICE
{
	ipV4Address		SEQUENCE
	{
		ip		OCTET STRING ( SIZE (4) ),
		port		INTEGER ( 0..65535 )
	},

	ipV6Address		SEQUENCE
	{
		ip		OCTET STRING ( SIZE (16) ),
		port		INTEGER ( 0..65535 ),
		...
	},
	...	
}


-- ---------------------------------
--
--   TransportAddressInformation
--
--	@prop 	dataType
--   	@descr	sequence of TransportAdress
--	@
-- ---------------------------------

TransportAddressInformation ::= SEQUENCE OF TransportAddress


-- ---------------------------------
--
--   TransportChannelInformation
--
--	@prop 	dataType
--	@						
--
-- ---------------------------------

TransportChannelInformation ::= SEQUENCE
{
	sendAddress		TransportAddress OPTIONAL,
	recvAddress		TransportAddress OPTIONAL,
	...
}


-- ---------------------------------
--
--   TypeOfEquipment
--
--	@prop 	dataType
--   	@descr	Type of equipment.
--	@						
--
-- ---------------------------------

TypeOfEquipment ::= CHOICE
{
	cordlessTerminal	NULL,
	h323Terminal		NULL,
	h323Gateway		NULL,
	isdnTerminal		NULL,
	...
}



-- ---------------------------------
--
--   TypeOfFlowControl
--
--   @prop	dataType
--   @descr	This parameter specifies the type of flow control used in the LAN.
--   @
--
-- ---------------------------------

TypeOfFlowControl ::= CHOICE
{
	isa				NULL,
	priorityOutputRouting		NULL,
	other				NULL,
	undefined			NULL,
	...
}



-- ---------------------------------
--
--   TypeOfLAN
--
--   @prop	dataType
--   @descr	This parameter specifies the type of LAN.
--   @
--
-- ---------------------------------

TypeOfLAN ::= CHOICE
{
	ethernet		NULL,
	tokenRing		NULL,
	other			NULL,
	undefined		NULL,
	...
}



-- ---------------------------------
--
--   TypeOfRegistration
--
--	@prop 	dataType
--   	@descr	Type of service.
--	@						
--
-- ---------------------------------

TypeOfRegistration ::= CHOICE
{
	changeOfUser			NULL,
	noChangeOfUser			NULL,
	...
}



-- ---------------------------------
--
--   TypeOfService
--
--	@prop 	dataType
--   	@descr	Type of service.
--	@						
--
-- ---------------------------------

TypeOfService ::= CHOICE
{
	basicCall		NULL,
	endpointRegistration	NULL,
	endpointUnregistration	NULL,
	...
}



-- ---------------------------------
--
--   TypeOfUser
--
--	@prop 	dataType
--   	@descr	Type of user.
--	@						
--
-- ---------------------------------

TypeOfUser ::= CHOICE
{
	human			NULL,
	network			NULL,
	...
}



-- ---------------------------------
--
--   UnknownMessageResponse
--
--	@prop 	dataType
--   	@descr	
--	@						
--
-- ---------------------------------

UnknownMessageResponse ::= SEQUENCE	
{
	requestSeqNum		RequestSeqNum,
	...
}

-- ---------------------------------
--
--   UnregistrationRejectReason
--
--	@prop 	dataType
--   	@descr	
--	@						
--
-- ---------------------------------

UnregistrationRejectReason ::= CHOICE
{
	notCurrentlyRegistered		NULL,	
	callInProgress			NULL,	
	undefinedReason			NULL,
	...
}

-- ---------------------------------
--
--   UserAllocateResourceRejectReason
--
--	@prop 	dataType
--   	@descr	Reason for the rejection.
--	@						
--
-- ---------------------------------

UserAllocateResourceRejectReason ::= CHOICE
{
	keyNotValid			NULL,
	resourceNotAvailable		NULL,
	serviceIdentifierExist		NULL,
	undefined			NULL,
	...
}



-- ---------------------------------
--
--   UserAttributeData
--
--   @prop	dataType
--
--   @descr	This parameter contains an User attribute value.
--
--   @
--
-- ---------------------------------

UserAttributeData ::= CHOICE
{
	maximumTotalBandwidth			Bandwidth,
	maximumBandwidthPerService		Bandwidth,
	relationToEquipment			SEQUENCE OF RelationToEquipment,
	stateOfUser				StateOfUser,
	typeOfUser				TypeOfUser,
	userIdentifierInformation		SEQUENCE OF UserIdentifier,
	...
}



-- ---------------------------------
--
--   UserAttributeIdentifier
--
--   @prop	dataType
--
--   @descr	This parameter contains User attribute identifiers.
--
--   @
--
-- ---------------------------------

UserAttributeIdentifier ::= CHOICE
{
	maximumTotalBandwidth			NULL,
	maximumBandwidthPerService		NULL,
	relationToEquipment			NULL,
	stateOfUser				NULL,
	typeOfUser				NULL,
	userIdentifierInformation		NULL,
	...
}



-- ---------------------------------
--
--   UserCreateRejectReason
--
--   @prop	dataType
--
--   @descr	This parameter states the reason for the rejection.
--
--   @
--
-- ---------------------------------

UserCreateRejectReason ::= CHOICE
{
	userIdentifierAlreadyExist	NULL,
	undefined			NULL,
	...
}



-- ---------------------------------
--
--   	UserDeallocateResourceRejectReason
--	@prop 	dataType
--
--   	@descr	Reason for the rejection.
--	@						
--
-- ---------------------------------

UserDeallocateResourceRejectReason ::= CHOICE
{
	resourceNotAllocated		NULL,
	serviceIdentifierNotValid	NULL,
	userNotExist			NULL,
	undefined			NULL,
	...
}



-- ---------------------------------
--
--   UserIdentifier
--
--	@prop 	dataType
--   	@descr	The identifier of the User.
--	@						
--
-- ---------------------------------

UserIdentifier ::= CHOICE
{
	e164		E164Identifier,
	h323		BMPString (SIZE (1..256)),	
			-- h323 is Basic ISO/IEC 10646-1 (Unicode)
	...
}


-- ---------------------------------
--
--   UserIdentifierInformation
--
--	@prop 	dataType
--   	@descr	sequence of UserIdentifier
--	@						
--
-- ---------------------------------

UserIdentifierInformation ::= SEQUENCE OF UserIdentifier--   
--  Created by  	:
--  Creation date 	:
--  Modified by 	:
--  Modification date 	:
--  Version 		:
-- 
--	@prop 	dataType
--	@

UserInformation ::= OCTET STRING (SIZE(1..131)) 




-- ---------------------------------
--
--   UserInformationUUIE
--
--	@prop 	dataType
--	@	--
-- ---------------------------------

UserInformationUUIE	::= SEQUENCE
{
	protocolIdentifier	ProtocolIdentifier,			
	...
}


-- ---------------------------------
--
--   UserKey
--
--	@prop 	dataType
--   	@descr	Unique key for a certain user.
--	@
-- ---------------------------------

UserKey ::= CHOICE
{
	relationToUser			RelationToUser,
	userIdentifierInformation	SEQUENCE OF UserIdentifier,
	...
}


-- ---------------------------------
--
--   UserOrEquipmentRelatedInformation
--
--	@prop 	dataType
--  	@descr	This parameter specifies the type of information.
--	@
-- ---------------------------------

UserOrEquipmentRelatedInformation ::= CHOICE
{
	userRelatedInformation		SEQUENCE OF UserRelatedInformation,
	equipmentRelatedInformation	SEQUENCE OF EquipmentRelatedInformation,
	...
}



-- ---------------------------------
--
--   UserOrEquipmentRelatedInformationIdentifier
--
--	@prop 	dataType
--   	@descr	This parameter specifies the type of information identifiers.
--	@
-- ---------------------------------

UserOrEquipmentRelatedInformationIdentifier ::= CHOICE
{
	userRelatedInformationIdentifiers		SEQUENCE OF UserRelatedInformationIdentifier,
	equipmentRelatedInformationIdentifiers		SEQUENCE OF EquipmentRelatedInformationIdentifier,
	...
}



-- ---------------------------------
--
--   UserRelatedInformation
--
--	@prop 	dataType
--   	@descr	This parameter specifies different types of data 
--   		that are related to the user. 
--	@						
--
-- ---------------------------------

UserRelatedInformation ::= CHOICE
{
	numberOfEquipments			INTEGER,
	stateOfUser				StateOfUser,
	typeOfUser				TypeOfUser,
	...
}



-- ---------------------------------
--
--   UserRelatedInformationIdentifier
--
-- 
-- @prop 	dataType
--
-- @descr	This parameter specifies different types of data 
--   		that are specific to a certain user. 
--
-- @
-- ---------------------------------

UserRelatedInformationIdentifier ::= CHOICE
{
	numberOfEquipments		NULL,
	stateOfUser			NULL,
	typeOfUser			NULL,
	...
}



-- ---------------------------------
--
--   UserRemoveRejectReason
--
--   @prop	dataType
--
--   @descr	This parameter states the reason for the rejection.
--
--   @
--
-- ---------------------------------

UserRemoveRejectReason ::= CHOICE
{
	keyNotValid				NULL,
	serviceInProgress			NULL,
	undefined				NULL,
	...
}



-- ---------------------------------
--
--   UserSetRejectReason
--
--   @prop	dataType
--
--   @descr	This parameter states the reason for the rejection.
--
--   @
--
-- ---------------------------------

UserSetRejectReason ::= CHOICE
{
	keyNotValid				NULL,
	undefined				NULL,
	...
}



-- ---------------------------------
--
--   UserSpecificInformation
--
--   	@descr	This parameter specifies different types of data 
--   		that are specific to the user. 
--	@						
--
-- ---------------------------------

UserSpecificInformation ::= CHOICE
{
	userRelatedInformation		SEQUENCE OF UserRelatedInformation,
	equipmentRelatedInformation	SEQUENCE OF EquipmentRelatedInformation,
	...
}



-- ---------------------------------
--
--   VendorIdentifier
--
--	@prop 	dataType
--	@						
--
-- ---------------------------------

VendorIdentifier ::= SEQUENCE
{
	vendor		H221NonStandard,
	productId	OCTET STRING (SIZE(1..256)) OPTIONAL,
	versionId	OCTET STRING (SIZE(1..256)) OPTIONAL,
	...
}


-- ---------------------------------
--
--   VoiceInformation
--
--	@prop 	dataType
--   	@descr
--	@						
--
-- ---------------------------------

VoiceInformation ::= SEQUENCE
{
	nonStandardData	NonStandardParameter OPTIONAL,
	...
}
END
