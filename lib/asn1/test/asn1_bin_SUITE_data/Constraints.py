Constraints DEFINITIONS ::=
BEGIN

-- Single Value
SingleValue ::= INTEGER (1)
SingleValue2 ::= INTEGER (1..20)
Range2to19 ::= INTEGER (1<..<20)
Range10to20 ::= INTEGER (10..20)
ContainedSubtype ::= INTEGER (INCLUDES Range10to20)
FixedSize ::= OCTET STRING (SIZE(10)) 
FixedSize2 ::= OCTET STRING (SIZE(10|20)) 
VariableSize ::= OCTET STRING (SIZE(1..10)) 
PemittedAlphabet ::= PrintableString (FROM ("a"|"yx"))
AliasAddress		::=CHOICE
{
	e164		IA5String (SIZE (1..128) ^ FROM ("0123456789#*,")),
	h323-ID		BMPString (SIZE (1..256)),
	...
}	
Obj ::= OBJECT IDENTIFIER


-- OTP-4559: a referenced type that has a permitted alphabet constraint
-- Example from H323-MESSAGES ver (11/2000)
TBCD-STRING ::= IA5String (FROM ("0123456789#*abc"))

ANSI-41-UIM ::= SEQUENCE {
  imsi  [0] TBCD-STRING(SIZE (3..16)) OPTIONAL,
  esn   [1] TBCD-STRING(SIZE (16)) OPTIONAL
}

-- OTP-4869: a BIT STRING constrained by SIZE(C) was encoded wrong
-- when C was larger than 16. There was also an error when encodeing
-- in compact_bit_string mode.

IP ::= SEQUENCE {
  perm SEQUENCE OF INTEGER (0..15),
  key BIT STRING (SIZE (128)),
  bool BOOLEAN OPTIONAL
}

-- add for OTP-3558 and OTP-4917
Day ::= ENUMERATED{monday(0),tuesday(1),wednesday(2),thursday(3),friday(4),saturday(5),sunday(6)}

Wednesday ::= Day(wednesday)


Thing ::= INTEGER {fred (0),fred2 (1),fred3 (2)}


AnotherThing ::= Thing (fred | fred2)

I ::= INTEGER (0|15..269) -- OTP-5457

-- OTP-5511

maxNrOfCellPortionsPerCell-1 INTEGER ::= 35
CellPortionID	::= INTEGER (0..maxNrOfCellPortionsPerCell-1,...)

-- OTP-6763
T ::=  IA5String (SIZE (1|2, ..., SIZE (1|2|3))) -- Dubuisson 268
T2 ::= IA5String (SIZE (1|2, ..., 3)) -- equal with T

-- OTP-8046
DateAndTime ::= VisibleString (PATTERN "\d#2/\d#2/\d#4-\d#2:\d#2") 
-- DD/MM/YYYY-HH:MM


-- OTP-6828
HandoverCommand-r8-IEs ::=			SEQUENCE {
  handoverCommandMessage	OCTET STRING (CONTAINING MyType),
  ...
}

MoreCompact ::= OCTET STRING (CONTAINING MyType ENCODED BY {joint-iso-itu-t asn1 packed-encoding(3) basic(0) unaligned(1)})

MyType ::= SEQUENCE {a INTEGER, b INTEGER}

Document ::= OCTET STRING (ENCODED BY pdf)

pdf OBJECT IDENTIFIER ::= {1,2,3,4,5}


END
