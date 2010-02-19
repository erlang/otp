SetOf DEFINITIONS IMPLICIT TAGS ::= 

BEGIN


Set1 ::= SET 
{
  bool1  BOOLEAN,
  int1  INTEGER,
  set1  SET OF SetIn DEFAULT {}
}

Set2 ::= SET 
{
  set2  SET OF SetIn DEFAULT {},
  bool2  BOOLEAN,
  int2  INTEGER
}

Set3 ::= SET 
{
  bool3  BOOLEAN,
  set3  SET OF SetIn DEFAULT {},
  int3  INTEGER
}

Set4 ::= SET 
{
  set41 [41] SET OF SetIn DEFAULT {},
  set42 [42] SET OF SetIn DEFAULT {},
  set43 [43] SET OF SetIn DEFAULT {}
}



SetIn ::= SET 
{
  boolIn  BOOLEAN,
  intIn  INTEGER
}

END
