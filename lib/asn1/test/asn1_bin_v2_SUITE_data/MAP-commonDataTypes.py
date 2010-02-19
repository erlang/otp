MAP-commonDataTypes
	{ iso (1) identified-organization (4) etsi (0) mobileDomain (0)
		gsm-Network (1) modules (3) map-CommonDataTypes (18) version2 (2) }
DEFINITIONS ::=
BEGIN

EXPORTS
    TBCD-STRING,
    AddressString,
    ISDN-AddressString,
    LMSI,
    IMSI,
    TMSI,
--    IMSIBCD,
    SubscriberID;


   TBCD-STRING ::= OCTET STRING (SIZE(3..8))
   AddressString ::= OCTET STRING (SIZE(1..20))
   ISDN-AddressString ::=  AddressString 
   LMSI ::= OCTET STRING (SIZE(4))
 --  IMSIBCD ::= BCD-STRING
   IMSI ::= TBCD-STRING
   TMSI ::= OCTET STRING (SIZE(1..4))
   SubscriberID ::= CHOICE{
       imsi		[0] IMSI,
       tmsi		[1] TMSI}
   
   
END  -- of MAP-commonDataTypes

