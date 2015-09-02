Seq DEFINITIONS IMPLICIT TAGS ::= 

BEGIN

IMPORTS Set1 FROM SeqSetLib;

Seq ::=  SEQUENCE 
{
  bool  BOOLEAN,
  boolCon [20] BOOLEAN,
  boolPri [PRIVATE 21] BOOLEAN,
  boolApp  [APPLICATION 22] BOOLEAN,
  boolExpCon [30] EXPLICIT BOOLEAN,
  boolExpPri [PRIVATE 31] EXPLICIT BOOLEAN,
  boolExpApp  [APPLICATION 32] EXPLICIT BOOLEAN
}

Seq1 ::= SEQUENCE 
{
  bool1  BOOLEAN,
  int1  INTEGER,
  seq1  SeqIn
}

Seq2 ::= SEQUENCE 
{
  seq2  SeqIn,
  bool2  BOOLEAN,
  int2  INTEGER
}

Seq3 ::= SEQUENCE 
{
  bool3  BOOLEAN,
  seq3  SeqIn,
  int3  INTEGER
}

Seq4 ::= SEQUENCE 
{
  seq41  SeqIn,
  seq42  SeqIn,
  seq43  SeqIn
}
SeqDef1 ::= SET 
{
  bool1  BOOLEAN DEFAULT TRUE,
  int1  INTEGER,
  seq1  SeqIn DEFAULT {}
}

SeqDef2 ::= SET 
{
  seq2  SeqIn DEFAULT {},
  bool2  BOOLEAN,
  int2  INTEGER
}

SeqDef3 ::= SET 
{
  bool3  BOOLEAN DEFAULT TRUE,
  seq3  SeqIn DEFAULT {},
  int3  INTEGER DEFAULT 17
}

SeqOpt1 ::= SET 
{
  bool1  BOOLEAN OPTIONAL,
  int1  INTEGER,
  seq1  SeqIn OPTIONAL
}

SeqOpt2 ::= SET 
{
  seq2  SeqIn OPTIONAL,
  bool2  BOOLEAN,
  int2  INTEGER
}

SeqOpt3 ::= SET 
{
  bool3  BOOLEAN OPTIONAL,
  seq3  SeqIn OPTIONAL,
  int3  INTEGER OPTIONAL
}


SeqIn ::= SEQUENCE 
{
  boolIn  BOOLEAN OPTIONAL,
  intIn  INTEGER DEFAULT 12
}


SeqS1 ::= SEQUENCE 
{
  boolS1  BOOLEAN,
  intS1  INTEGER,
  seqS1  SEQUENCE { boolIn BOOLEAN,
                    intIn  INTEGER }
}

SeqS2 ::= SEQUENCE 
{
  seqS2  SEQUENCE { boolIn BOOLEAN,
                    intIn  INTEGER },
  boolS2  BOOLEAN,
  intS2  INTEGER

}

SeqS3 ::= SEQUENCE 
{
  boolS3  BOOLEAN,
  seqS3  SEQUENCE { boolIn BOOLEAN,
                    intIn  INTEGER },
  intS3  INTEGER

}


SeqImp1 ::= SET 
{
  set  Set1,
  bool  BOOLEAN,
  int  INTEGER
}


SeqImp2 ::= SET 
{
  bool  BOOLEAN,
  set  Set1,
  int  INTEGER
}


SeqImp3 ::= SET 
{
  bool  BOOLEAN,
  int  INTEGER,
  set  Set1
}

SeqCompOf ::= SEQUENCE {
  ...,
  COMPONENTS OF SeqS3
}

END





