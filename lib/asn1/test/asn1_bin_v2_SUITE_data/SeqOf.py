SeqOf DEFINITIONS IMPLICIT TAGS ::= 

BEGIN


Seq1 ::= SEQUENCE 
{
  bool1  BOOLEAN,
  int1  INTEGER,
  seq1  SEQUENCE OF SeqIn DEFAULT {}
}

Seq2 ::= SEQUENCE 
{
  seq2  SEQUENCE OF SeqIn DEFAULT {},
  bool2  BOOLEAN,
  int2  INTEGER
}

Seq3 ::= SEQUENCE 
{
  bool3  BOOLEAN,
  seq3  SEQUENCE OF SeqIn DEFAULT {},
  int3  INTEGER
}

Seq4 ::= SEQUENCE 
{
  seq41 [41] SEQUENCE OF SeqIn DEFAULT {},
  seq42 [42] SEQUENCE OF SeqIn DEFAULT {},
  seq43 [43] SEQUENCE OF SeqIn DEFAULT {}
}



SeqIn ::= SEQUENCE 
{
  boolIn  BOOLEAN,
  intIn  INTEGER
}




END
