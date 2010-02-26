P-Record DEFINITIONS  ::=
BEGIN


PersonnelRecord ::= [APPLICATION 0] SET
{	name			  Name,
	title			  VisibleString,
	number			  EmployeeNumber,
	dateOfHire		  Date,
	nameOfSpouse  	[1]	  Name,
	children		  SEQUENCE OF ChildInformation DEFAULT {}
}

ChildInformation ::= SET
{	name			Name,
	dateOfBirth		Date
}

Name ::= [APPLICATION 1] SEQUENCE
{	givenName		VisibleString,
	initial			VisibleString,
	familyName		VisibleString
}

EmployeeNumber ::= [APPLICATION 2] INTEGER
Date ::= [APPLICATION 3] VisibleString  --  YYYY MMDD

v PersonnelRecord ::=
{
	name {
		givenName "John", 
		initial "P", 
		familyName "Smith"
	},
	title	"Director",
	number	51,
	dateOfHire "19710917",
	nameOfSpouse {
		givenName "Mary", 
		initial "T", 
		familyName "Smith"
	},
	children { 
		{name {
			givenName "Ralph", 
			initial "T", 
			familyName "Smith"
		} ,
		dateOfBirth "19571111"},
		{name {
			givenName "Susan", 
			initial "B", 
			familyName "Jones"
		} ,
		dateOfBirth "19590717" }
	}
}

END
