From DEFINITIONS IMPLICIT TAGS ::= 

BEGIN

AreaCode ::= SEQUENCE 
{

    firstDigit IA5String 
         (FROM ( "2" | "3" ) ),
    secondDigit IA5String 
         (FROM ( "3" | "4" ) )
}

END

