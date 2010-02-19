Tst { 2 6 6 24 7 1 } DEFINITIONS IMPLICIT TAGS ::=

BEGIN

--EXPORTS SomeSet , Id0 , Aset,Id1 ,A,B,C,
--	Uhh ,Foo ,Cho,Person,Hobbe,Robbe,X,Y;

IMPORTS Fooo FROM Bobby;


Robbe ::=   SET {
	ttt TT }
	
Koo ::= SET {
	c CHOICE {
		a INTEGER,
		b BOOLEAN },
	s SET OF Id0 }


Hobbe ::= [APPLICATION 1] SET {
	     aaa [0] SET OF INTEGER,
	     bbb [1] UU
	}

UU ::= PP
PP ::=   CHOICE {
	 cc [1] CHOICE {
		a [0] INTEGER,
		b [1] BOOLEAN,
		c [2] BIT STRING },
	 ii [0] Id0
	}


TT ::=  SS
SS ::=  SET {
	b BOOLEAN DEFAULT TRUE
	}

Aset ::= [PRIVATE 2]  SET OF Uhh



SomeSet ::= [PRIVATE 3]  IMPLICIT SET {
	aaaa [2] SET{  
		ggg [0] INTEGER},
	kkkk [1] SET OF Id2,
	booby [4]  OCTET STRING,
	puck [3] INTEGER {red(0),blue(1),yellow(-2)},
	baby [5] IMPLICIT Id1,
	bool [6] BOOLEAN }


Id0 ::=  INTEGER (4 .. 99)

Id1 ::=   Id0

Id2 ::= [PRIVATE 4] EXPLICIT Id1


Uhh ::=  SET {
	a [1] IMPLICIT Id1}



Soon ::= [PRIVATE 5] Moon

Moon ::= [PRIVATE 6] IMPLICIT Person


Person ::= [PRIVATE 7] IMPLICIT SEQUENCE {
	szzzs SET OF SET {
		aaa [0] INTEGER,
		bbb [1] Id0},
	cho Cho,
	name OCTET STRING ,
	location INTEGER,
	asss Aset,
	oops [2] IMPLICIT SET {
		q [0] INTEGER,
		p [1] Uhh},
	on INTEGER,
	mybits [3] IMPLICIT BIT STRING,
	foo Foo,
	age INTEGER,
	hobbe [5] SEQUENCE {
		a [4] CHOICE {
			a INTEGER,
			b BOOLEAN },
		b [5] Id0}}





Foo ::= [PRIVATE 8] IMPLICIT SEQUENCE {
	goofy [3]  INTEGER OPTIONAL,
	somestring [10] IMPLICIT OCTET STRING DEFAULT '77BB'H,
	hoohoo  [11] IMPLICIT SEQUENCE {
		bar [1] Id1 OPTIONAL,
		foo INTEGER, 
		zombie [9] CHOICE {
			a [1] IMPLICIT INTEGER,
			b [2]  IMPLICIT BOOLEAN }
		},
	moon [4]  IMPLICIT INTEGER }



Cho ::= [PRIVATE 9]  EXPLICIT CHOICE {
	somestring [2] IMPLICIT OCTET STRING,
	goofy [9]  INTEGER,
	moon [4] IMPLICIT INTEGER } 


A ::= [APPLICATION 2] SET {
	ppp IA5String ,
	a [0] INTEGER {aaa(6),bbb(77)} DEFAULT 998,
	b [1] Id1 OPTIONAL,
	c [2] OCTET STRING (SIZE(8)),
	dd [3] BIT STRING DEFAULT '11001'B }

B ::= [APPLICATION 3] SET {
	ww [1] SET {
		a A OPTIONAL,
		goofy [3]  INTEGER OPTIONAL,
		somestring [10] IMPLICIT OCTET STRING DEFAULT '77BB'H } 
	}


C::= [APPLICATION 4]  SEQUENCE OF X

Y ::= OBJECT IDENTIFIER

X ::= SET {
	a NULL,
	b GeneralString,
	c UTCTime,
	d VideotexString,
	g GeneralizedTime,
	h GraphicString,
	i VisibleString,
	j IA5String,
	k PrintableString,
	l OCTET STRING,
	e TeletexString,
	m  ANY,
	n ObjectDescriptor,
	o OBJECT IDENTIFIER,
	f NumericString }

END
