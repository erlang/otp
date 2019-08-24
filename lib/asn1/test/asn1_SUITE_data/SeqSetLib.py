SeqSetLib DEFINITIONS IMPLICIT TAGS ::= 

BEGIN
EXPORTS Seq1,Set1;

Seq1 ::= SEQUENCE 
{
  bool1  BOOLEAN,
  int1  INTEGER,
  seq1  SeqIn
}

Set1 ::= SET 
{
  bool1  BOOLEAN,
  int1  INTEGER,
  set1  SetIn
}

SetIn ::= SET 
{
  boolIn  BOOLEAN,
  intIn  INTEGER
}

SeqIn ::= SEQUENCE 
{
  boolIn  BOOLEAN,
  intIn  INTEGER
}

END
