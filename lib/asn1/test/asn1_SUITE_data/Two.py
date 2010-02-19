Two { 1 2 3} DEFINITIONS EXPLICIT TAGS ::=

BEGIN
EXPORTS  A, D,Boo,Szz;



D ::= [PRIVATE 1] SEQUENCE {
	a INTEGER,
	b Boo,
	c  ANY DEFINED BY a ,
	d ANY }


Boo ::=  SEQUENCE OF INTEGER (198..200)

A ::= [PRIVATE 2] SEQUENCE {
	a INTEGER (1..1),
	b INTEGER (3..3) }


Szz ::= CHOICE {
	one INTEGER,
	two BOOLEAN }

C ::= SET {
	a [0] INTEGER (0..8),
	xx [4] CHOICE {
		[7] INTEGER (9..10),
		a INTEGER (11 ..13) },
	 f Boo,
 	 r [2] INTEGER (20..22)}
END

