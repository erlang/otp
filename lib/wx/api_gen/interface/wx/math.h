/////////////////////////////////////////////////////////////////////////////
// Name:        math.h
// Purpose:     interface of global functions
// Author:      wxWidgets team
// Licence:     wxWindows licence
/////////////////////////////////////////////////////////////////////////////

/** @addtogroup group_funcmacro_math */
//@{

/**
    Returns a non-zero value if @a x is neither infinite nor NaN (not a
    number), returns 0 otherwise.

    @header{wx/math.h}
*/
int wxFinite(double x);

/**
    Returns the greatest common divisor of the two given numbers.

    @since 3.1.0

    @header{wx/math.h}
*/
unsigned int wxGCD(unsigned int u, unsigned int v);

/**
    Returns a non-zero value if x is NaN (not a number), returns 0 otherwise.

    @header{wx/math.h}
*/
bool wxIsNaN(double x);

/**
    Converts the given array of 10 bytes (corresponding to 80 bits) to
    a float number according to the IEEE floating point standard format
    (aka IEEE standard 754).

    @see wxConvertToIeeeExtended() to perform the opposite operation
*/
wxFloat64 wxConvertFromIeeeExtended(const wxInt8 *bytes);

/**
    Converts the given floating number @a num in a sequence of 10 bytes
    which are stored in the given array @a bytes (which must be large enough)
    according to the IEEE floating point standard format
    (aka IEEE standard 754).

    @see wxConvertFromIeeeExtended() to perform the opposite operation
*/
void wxConvertToIeeeExtended(wxFloat64 num, wxInt8 *bytes);

/**
    Convert degrees to radians.

    This function simply returns its argument multiplied by @c M_PI/180 but is
    more readable than writing this expression directly.

    @see wxRadToDeg()

    @since 3.1.0
 */
double wxDegToRad(double deg);

/**
    Convert radians to degrees.

    This function simply returns its argument multiplied by @c 180/M_PI but is
    more readable than writing this expression directly.

    @see wxDegToRad()

    @since 3.1.0
 */
double wxRadToDeg(double rad);

/**
    Count the number of trailing zeros.

    This function returns the number of trailing zeros in the binary notation
    of its argument @a x. E.g. for @a x equal to 4, or 0b100, the return value
    is 2.

    @param x Strictly positive, i.e. non-zero, 32 bit number.

    @since 3.1.2
 */
unsigned int wxCTZ(wxUint32 x);

/**
    Small wrapper around round().
*/
int wxRound(double x);

/**
   Returns true if both double values are identical. This is
   only reliable if both values have been assigned the same
   value.
*/
bool wxIsSameDouble(double x, double y);

/**
   Return true of @a x is exactly zero. This is only reliable
   if it has been assigned 0.
*/
bool wxIsNullDouble(double x);

//@}

