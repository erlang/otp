/////////////////////////////////////////////////////////////////////////////
// Name:        wx/numformatter.h
// Purpose:     interface to wxNumberFormatter
// Author:      Fulvio Senore, Vadim Zeitlin
// Licence:     wxWindows licence
/////////////////////////////////////////////////////////////////////////////

/**
    @class wxNumberFormatter

    Helper class for formatting and parsing numbers with thousands separators.

    This class contains only static functions, so users must not create instances
    but directly call the member functions.

    @since 2.9.2

    @library{wxbase}
*/
class wxNumberFormatter
{
public:
    /**
        Bit masks used with ToString().
     */
    enum Style
    {
        /**
            This flag can be used to indicate absence of any other flags below.
         */
        Style_None              = 0x00,

        /**
            If this flag is given, thousands separators will be inserted in the
            number string representation as defined by the current locale.
         */
        Style_WithThousandsSep  = 0x01,

        /**
            If this flag is given, trailing zeroes in a floating point number
            string representation will be omitted.

            If the number is actually integer, the decimal separator will be
            omitted as well. To give an example, formatting the number @c 1.23
            with precision 5 will normally yield "1.23000" but with this flag
            it would return "1.23". And formatting @c 123 with this flag will
            return just "123" for any precision.

            This flag can't be used with ToString() overload taking the integer
            value.
         */
        Style_NoTrailingZeroes  = 0x02
    };

    /**
        Returns string representation of an integer number.

        By default, the string will use thousands separators if appropriate for
        the current locale. This can be avoided by passing Style_None as @a
        flags in which case the call to the function has exactly the same
        effect as <code>wxString::Format("%ld", val)</code>.

        Notice that calling ToString() with a value of type @c int and
        non-default flags results in ambiguity between this overload and the
        one below. To resolve it, you need to cast the value to @c long.

        @param val
            The variable to convert to a string.
        @param flags
            Combination of values from the Style enumeration (except for
            Style_NoTrailingZeroes which can't be used with this overload).
    */
    static wxString ToString(long val, int flags = Style_WithThousandsSep);

    /**
        Returns string representation of a floating point number.

        @param val
            The variable to convert to a string.
        @param precision
            Number of decimals to write in formatted string.
        @param flags
            Combination of values from the Style enumeration.
    */
    static wxString
    ToString(double val, int precision, int flags = Style_WithThousandsSep);


    /**
        Parse a string representation of a number possibly including thousands
        separators.

        These functions parse number representation in the current locale. On
        success they return @true and store the result at the location pointed
        to by @a val (which can't be @NULL), otherwise @false is returned.

        @see wxString::ToLong(), wxString::ToDouble()
     */
    //@{
    static bool FromString(wxString s, long *val);
    static bool FromString(wxString s, double *val);
    //@}

    /**
        Get the decimal separator for the current locale.

        Decimal separators is always defined and we fall back to returning '.'
        in case of an error.
     */
    static wxChar GetDecimalSeparator();

    /**
        Get the thousands separator if grouping of the digits is used by the
        current locale.

        The value returned in @a sep should be only used if the function
        returns @true, otherwise no thousands separator should be used at all.

        @param sep
            Points to the variable receiving the thousands separator character
            if it is used by the current locale. May be @NULL if only the
            function return value is needed.
     */
    static bool GetThousandsSeparatorIfUsed(wxChar *sep);

};
