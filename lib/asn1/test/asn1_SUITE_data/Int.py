Int DEFINITIONS IMPLICIT TAGS ::=
BEGIN
EXPORTS;

Int ::= INTEGER
Int2 ::= INTEGER (1..10)

-- OTP-5457
int11 INTEGER ::= 11
int11-2 INTEGER ::= int11  --correct should not cause crash or warning

END
