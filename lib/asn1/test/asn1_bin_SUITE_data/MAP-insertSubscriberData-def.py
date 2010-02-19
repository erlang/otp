MAP-insertSubscriberData-def
	{ ccitt (0) identified-organization( 4) etsi( 0) mobileDomain(0)
		gsm-Network( 1) modules( 3) map-Protocol( 4) version2(2) }
DEFINITIONS ::=

BEGIN

EXPORTS
InsertSubsDataArg, InsertSubsDatRes;
IMPORTS
IMSI, ISDN-AddressString, LMSI FROM MAP-commonDataTypes;

InsertSubsDataArg ::=	SEQUENCE{
      imsi            [0] IMPLICIT IMSI	OPTIONAL, 
      msisdn          [1] IMPLICIT ISDN-AddressString OPTIONAL,
      category        [2] IMPLICIT OCTET STRING (SIZE(1)) OPTIONAL,
      subscriberStatus [3] IMPLICIT SubscriberStatus OPTIONAL,
      bearerServiceList [4] IMPLICIT SEQUENCE OF
      		                       OCTET STRING(SIZE(1)) OPTIONAL,
      teleServiceList [6] IMPLICIT SEQUENCE OF
      		                       OCTET STRING(SIZE(1)) OPTIONAL,
      provisionedSS [7] IMPLICIT SEQUENCE  OF SS-Information OPTIONAL
         	                }

SS-Information ::= CHOICE{
          forwardingInfo [0]  IMPLICIT ForwardingInfo, 
          callBarringInfoInfo [1] IMPLICIT CallBarringInfoInfo,
          ss-Data [3] IMPLICIT SS-Data                                                                 } 
          
SS-Data ::= SEQUENCE {
          ss-Code   OCTET STRING (SIZE(1)),
          ss-Status  [4] IMPLICIT OCTET STRING (SIZE(1))
                       }
                       

ForwardingInfo ::= SEQUENCE {
     	    ss-Code OCTET STRING(SIZE(1)) OPTIONAL,
           forwardingFeatureList ForwardingFeatureList  
 	                   }
 	                   
CallBarringInfoInfo ::= SEQUENCE {
            ss-Code  OCTET STRING(SIZE(1)) OPTIONAL,
            callBarringFeatureList CallBarringFeatureList}
            
CallBarringFeatureList ::= SEQUENCE OF   CallBarringFeature

CallBarringFeature ::=                SEQUENCE{
                basicService  BasicServiceCode OPTIONAL,
                ss-Status [2]  IMPLICIT OCTET STRING(SIZE(1)) OPTIONAL
                       }

InsertSubsDatRes ::= 
  SEQUENCE {
   teleServiceList   [1]  IMPLICIT SEQUENCE OF
       OCTET STRING (SIZE(1))  OPTIONAL,
    bearerServiceList [2]  IMPLICIT SEQUENCE OF
                               OCTET STRING (SIZE(1)) OPTIONAL,
    ss-List           [3]  IMPLICIT SEQUENCE OF
                               OCTET STRING (SIZE(1)) OPTIONAL,
   odb-GeneralData [4] IMPLICIT BIT STRING {
      allOG-CallsBarred (0),
      internationalOGCallsBarred (1),
      internationalOGCallsNotToHPLMN-CountryBarred (2),
      premiumRateInformationOGCallsBarred (3),
      premiumRateEntertainementOGCallsBarred (4),
      ss-AccessBarred (5)                  } (SIZE(6)) OPTIONAL,
   regionalSubscriptionResponse [5] IMPLICIT ENUMERATED{
     msc-AreaRestricted (0),
     tooManyZoneCodes (1),
     zoneCodeConflict (2),
     regionalSubscNotSupported (3)                      } OPTIONAL
            }
            
            
ForwardingFeatureList ::= SEQUENCE OF ForwardingFeature

ForwardingFeature ::=             SEQUENCE{
                basicService BasicServiceCode OPTIONAL,
 	        ss-Status [4]  IMPLICIT OCTET STRING(SIZE(1)) OPTIONAL,     
 	        forwardedToNumber [5]  ISDN-AddressString OPTIONAL,
                forwardingOptions [6]  IMPLICIT OCTET STRING(SIZE(1)) OPTIONAL,
               noReplyConditionTime [7]  IMPLICIT INTEGER(5..30) OPTIONAL
                  }


BasicServiceCode ::=  CHOICE {
                  bearerService [2]  IMPLICIT OCTET STRING(SIZE(1)),
 	          teleService [3]  IMPLICIT OCTET STRING(SIZE(1))      
	                 }
 	                 

BasicServiceGroupList ::= SEQUENCE OF
		 BasicServiceCode

            
SubscriberStatus ::=           ENUMERATED {
      		           serviceGranted (0),
      		           operatorDeterminedBarring (1)
      		                                }             
     
END  -- of MAP-insertSubscriberData-def

