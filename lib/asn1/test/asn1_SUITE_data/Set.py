Set DEFINITIONS IMPLICIT TAGS ::= 

BEGIN

IMPORTS Seq1 FROM SeqSetLib;

Set ::=  SET 
{
  bool  BOOLEAN,
  boolCon [20] BOOLEAN,
  boolPri [PRIVATE 21] BOOLEAN,
  boolApp  [APPLICATION 22] BOOLEAN,
  boolExpCon [30] EXPLICIT BOOLEAN,
  boolExpPri [PRIVATE 31] EXPLICIT BOOLEAN,
  boolExpApp  [APPLICATION 32] EXPLICIT BOOLEAN
}

Set1 ::= SET 
{
  bool1  BOOLEAN,
  int1  INTEGER,
  set1  SetIn
}

Set2 ::= SET 
{
  set2  SetIn,
  bool2  BOOLEAN,
  int2  INTEGER
}

Set3 ::= SET 
{
  bool3  BOOLEAN,
  set3  SetIn,
  int3  INTEGER
}

SetDef1 ::= SET 
{
  bool1  BOOLEAN DEFAULT TRUE,
  int1  INTEGER,
  set1  SetIn DEFAULT {}
}

SetDef2 ::= SET 
{
  set2  SetIn DEFAULT {},
  bool2  BOOLEAN,
  int2  INTEGER
}

SetDef3 ::= SET 
{
  bool3  BOOLEAN DEFAULT TRUE,
  set3  SetIn DEFAULT {},
  int3  INTEGER DEFAULT 17
}

SetOpt1 ::= SET 
{
  bool1  BOOLEAN OPTIONAL,
  int1  INTEGER,
  set1  SetIn OPTIONAL
}

SetOpt2 ::= SET 
{
  set2  SetIn OPTIONAL,
  bool2  BOOLEAN,
  int2  INTEGER
}

SetOpt3 ::= SET 
{
  bool3  BOOLEAN OPTIONAL,
  set3  SetIn OPTIONAL,
  int3  INTEGER OPTIONAL
}

SetIn ::= SET 
{
  boolIn  BOOLEAN OPTIONAL,
  intIn  INTEGER OPTIONAL
}


SetS1 ::= SET 
{
  boolS1  BOOLEAN,
  intS1  INTEGER,
  setS1  SET { boolIn BOOLEAN,
               intIn  INTEGER }
}

SetS2 ::= SET 
{
  setS2  SET { boolIn BOOLEAN,
               intIn  INTEGER },
  boolS2  BOOLEAN,
  intS2  INTEGER

}

SetS3 ::= SET 
{
  boolS3  BOOLEAN,
  setS3  SET { boolIn BOOLEAN,
               intIn  INTEGER },
  intS3  INTEGER

}


SetImp1 ::= SET 
{
  seq  Seq1,
  bool  BOOLEAN,
  int  INTEGER
}


SetImp2 ::= SET 
{
  bool  BOOLEAN,
  seq  Seq1,
  int  INTEGER
}


SetImp3 ::= SET 
{
  bool  BOOLEAN,
  int  INTEGER,
  seq  Seq1
}




END
