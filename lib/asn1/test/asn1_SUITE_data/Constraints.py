Constraints DEFINITIONS ::=
BEGIN

-- Single Value
SingleValue ::= INTEGER (1)
SingleValue2 ::= INTEGER (1..20)
predefined INTEGER ::= 1
SingleValue3 ::= INTEGER (predefined | 5 | 10)
Range2to19 ::= INTEGER (1<..<20)
Range10to20 ::= INTEGER (10..20)
ContainedSubtype ::= INTEGER (INCLUDES Range10to20)
-- Some ranges for additional constrained number testing.
LongLong ::= INTEGER (0..18446744073709551615)
Range256to65536 ::= INTEGER (256..65536)
SemiConstrained ::= INTEGER (100..MAX)
NegSemiConstrained ::= INTEGER (-128..MAX)
SemiConstrainedExt ::= INTEGER (42..MAX, ...)
NegSemiConstrainedExt ::= INTEGER (-128..MAX, ...)
SemiNamed ::= INTEGER {a(100), b(200)} (100..MAX)
-- Extensions --
LongLongExt ::= INTEGER (0..18446744073709551615, ..., -5000..-1)
Range256to65536Ext ::= INTEGER (256..65536, ..., 1000000..9000000)

-- Union of single values
Sv1 ::= INTEGER (2|3|17)
Sv2 ::= INTEGER (2|3|17, ...)
Sv3 ::= INTEGER {a(2),b(3),z(17)} (2|3|17, ...)

-- Other constraints
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

OneMoreThing ::= INTEGER {wilma(0), fred(1), betty(3), barney(2)}
OneMoreThing-1 ::= OneMoreThing (wilma | fred)
OneMoreThing-2 ::= OneMoreThing (fred | barney)

I ::= INTEGER (0|15..269) -- OTP-5457
X1 ::= INTEGER (1..4 | 8 | 10 | 20) -- OTP-9946

-- OTP-5511

maxNrOfCellPortionsPerCell-1 INTEGER ::= 35
CellPortionID	::= INTEGER (0..maxNrOfCellPortionsPerCell-1,...)

-- OTP-6763
T ::=  IA5String (SIZE (1|2), ..., SIZE (1|2|3)) -- Dubuisson 268
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

ShorterExt ::= IA5String (SIZE (5, ...))

SeqOverlapping ::= SEQUENCE {
    v Overlapping
}

SeqNonOverlapping ::= SEQUENCE {
    v NonOverlapping
}

Overlapping ::= INTEGER (7280..7560 |
7580..7680 |
7910..8210 |
8600..8940 |
9250..9600 |
14759..15109 |
15250..15590 |
18050..18800 |
19300..19950 |
21100..21700 |
26200..26900 |
18500..19900 |
20100..20250 |
21100..21700 |
23000..24000 |
24960..26900)

-- The same intervals, but merged and sorted --
NonOverlapping ::= INTEGER (7280..7560 |
7580..7680 |
7910..8210 |
8600..8940 |
9250..9600 |
14759..15109 |
15250..15590 |
18050..19950 |
20100..20250 |
21100..21700 |
23000..24000 |
24960..26900)

--
-- Test INTEGER constraints from fields in objects.
--

INT-HOLDER ::= CLASS {
  &id INTEGER UNIQUE,
  &obj INT-HOLDER OPTIONAL
} WITH SYNTAX {
  ID &id
  [OBJ &obj]
}

int-holder-1 INT-HOLDER ::= { ID 2 }
int-holder-2 INT-HOLDER ::= { ID 4 OBJ int-holder-1 }

IntObjectConstr ::= INTEGER (int-holder-2.&obj.&id..int-holder-2.&id)

--
-- INTEGER constraints defined using named INTEGERs.
--

ConstrainedNamedInt ::= INTEGER {v1(42)} (v1)
constrainedNamedInt-1 INTEGER {v1(42)} (v1) ::= 42
constrainedNamedInt-2 ConstrainedNamedInt ::= 100

SeqWithNamedInt ::= SEQUENCE {
   int INTEGER {v2(7)} (v2)
}

--
-- Cover simpletable constraint checking code.
--

ContentInfo ::= SEQUENCE {
  contentType ContentType
}

Contents TYPE-IDENTIFIER ::= {
  {OCTET STRING IDENTIFIED BY {2 1 1 1 1 1 1}}
}

ContentType ::= TYPE-IDENTIFIER.&id({Contents})

END
