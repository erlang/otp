/////////////////////////////////////////////////////////////////////////////
// Name:        wx/ustring.h
// Purpose:     interface of wxUString
// Author:      Robert Roebling
// Copyright:   (c) Robert Roebling
// Licence:     wxWindows licence
/////////////////////////////////////////////////////////////////////////////

/**
    @class wxUString

    wxUString is a class representing a Unicode character string where
    each character is stored using a 32-bit value. This is different from
    wxString which may store a character either as a UTF-8 or as a UTF-16
    sequence and different from @c std::string which stores a string
    as a sequence of simple 8-bit characters and also different from
    @c std::wstring which stores the string differently depending on
    the definition of wchar_t.

    The main purpose of wxUString is a to give users a Unicode string
    class that has O(1) access to its content, to be identical on all
    platforms and to be easily convertible to wxString as well as other
    ways to store strings (C string literals, wide character
    string literals, character buffer, etc) by providing several overloads
    and built-in conversions to and from the various string formats.

    wxUString derives from @c std::basic_string<wxChar32> and therefore
    offers the complete API of @c std::string.

    @library{wxbase}
    @category{data}

    @see wxString, @ref overview_string "wxString overview", @ref overview_unicode
    "Unicode overview"
*/


class WXDLLIMPEXP_BASE wxUString: public std::basic_string<wxChar32>
{
public:
    /**
        Default constructor.
    */
    wxUString();
    /**
        Copy constructor.
    */
    wxUString( const wxUString &str );
    /**
        Constructs a string from a 32-bit string literal.
    */
    wxUString( const wxChar32 *str );
    /**
        Constructs a string from 32-bit string buffer.
    */
    wxUString( const wxU32CharBuffer &buf );
    /**
        Constructs a string from C string literal using wxConvLibc to convert it to Unicode.
    */
    wxUString( const char *str );
    /**
        Constructs a string from C string buffer using wxConvLibc to convert it to Unicode.
    */
    wxUString( const wxCharBuffer &buf );
    /**
        Constructs a string from C string literal using @a conv to convert it to Unicode.
    */
    wxUString( const char *str, const wxMBConv &conv );
    /**
        Constructs a string from C string literal using @a conv to convert it to Unicode.
    */
    wxUString( const wxCharBuffer &buf, const wxMBConv &conv );
    /**
        Constructs a string from UTF-16 string literal
    */
    wxUString( const wxChar16 *str );
    /**
        Constructs a string from UTF-16 string buffer
    */
    wxUString( const wxU16CharBuffer &buf );
    /**
        Constructs a string from wxString.
    */
    wxUString( const wxString &str );
    /**
        Constructs a string from using wxConvLibc to convert it to Unicode.
    */
    wxUString( char ch );
    /**
        Constructs a string from a UTF-16 character.
    */
    wxUString( wxChar16 ch );
    /**
        Constructs a string from 32-bit Unicode character.
    */
    wxUString( wxChar32 ch );
    /**
        Constructs a string from wxUniChar (returned by wxString's access operator)
    */
    wxUString( wxUniChar ch );
    /**
        Constructs a string from wxUniCharRef (returned by wxString's access operator)
    */
    wxUString( wxUniCharRef ch );
    /**
        Constructs a string from @a n characters @a ch.
    */
    wxUString( size_t n, char ch );
    /**
        Constructs a string from @a n characters @a ch.
    */
    wxUString( size_t n, wxChar16 ch );
    /**
        Constructs a string from @a n characters @a ch.
    */
    wxUString( size_t n, wxChar32 ch );
    /**
        Constructs a string from @a n characters @a ch.
    */
    wxUString( size_t n, wxUniChar ch );
    /**
        Constructs a string from @a n characters @a ch.
    */
    wxUString( size_t n, wxUniCharRef ch );

    /**
        Static construction of a wxUString from a 7-bit ASCII string
    */
    static wxUString FromAscii( const char *str, size_t n );
    /**
        Static construction of a wxUString from a 7-bit ASCII string
    */
    static wxUString FromAscii( const char *str );
    /**
        Static construction of a wxUString from a UTF-8 encoded string
    */
    static wxUString FromUTF8( const char *str, size_t n );
    /**
        Static construction of a wxUString from a UTF-8 encoded string
    */
    static wxUString FromUTF8( const char *str );
    /**
        Static construction of a wxUString from a UTF-16 encoded string
    */
    static wxUString FromUTF16( const wxChar16 *str, size_t n );
    /**
        Static construction of a wxUString from a UTF-16 encoded string
    */
    static wxUString FromUTF16( const wxChar16 *str );


    /**
       Assignment from a 7-bit ASCII string literal
    */
    wxUString &assignFromAscii( const char *str );
    /**
       Assignment from a 7-bit ASCII string literal
    */
    wxUString &assignFromAscii( const char *str, size_t n );
    /**
       Assignment from a UTF-8 string literal
    */
    wxUString &assignFromUTF8( const char *str );
    /**
       Assignment from a UTF-8 string literal
    */
    wxUString &assignFromUTF8( const char *str, size_t n );
    /**
       Assignment from a UTF-16 string literal
    */
    wxUString &assignFromUTF16( const wxChar16* str );
    /**
       Assignment from a UTF-16 string literal
    */
    wxUString &assignFromUTF16( const wxChar16* str, size_t n );
    /**
       Assignment from a C string literal using wxConvLibc
    */
    wxUString &assignFromCString( const char* str );
    /**
       Assignment from a C string literal using @a conv
    */
    wxUString &assignFromCString( const char* str, const wxMBConv &conv );

    /**
        Conversion to a UTF-8 string
    */
    wxCharBuffer utf8_str() const;
    /**
        Conversion to a UTF-16 string
    */
    wxU16CharBuffer utf16_str() const;

    /**
        Conversion to a wide character string (either UTF-16
        or UCS-4, depending on the size of wchar_t).
    */
    wxWCharBuffer wc_str() const;

    /**
       Implicit conversion to wxString.
    */
    operator wxString() const;

    /**
       wxUString assignment. wxUString additionally provides overloads for
       wxString, C string, UTF-16 strings, 32-bit strings, char buffers,
       single and repeated characters etc.
    */
    wxUString &assign( const wxUString &str );

    /**
       Appending. wxUString additionally provides overloads for
       wxString, C string, UTF-16 strings, 32-bit strings, char buffers,
       single and repeated characters etc.
    */
    wxUString &append( const wxUString &s );

    /**
       Insertion. wxUString additionally provides overloads for
       wxString, C string, UTF-16 strings, 32-bit strings, char buffers,
       single characters etc.
    */
    wxUString &insert( size_t pos, const wxUString &s );

    /**
       Assignment operator. wxUString additionally provides overloads for
       wxString, C string, UTF-16 strings, 32-bit strings, char buffers,
       single characters etc.
    */
    inline wxUString& operator=(const wxUString& s);

    /**
       Concatenation operator. wxUString additionally provides overloads for
       wxString, C string, UTF-16 strings, 32-bit strings, char buffers,
       single characters etc.
    */
    inline wxUString& operator+=(const wxUString& s);

};

    /**
       Concatenation operator. wxUString additionally provides overloads for
       wxString, C string, UTF-16 strings, 32-bit strings, char buffers,
       single characters etc.
    */
inline wxUString operator+(const wxUString &s1, const wxUString &s2);

    /**
       Equality operator. wxUString additionally provides overloads for
       wxString, C string, UTF-16 strings, 32-bit strings, char buffers,
       single characters etc.
    */
inline bool operator==(const wxUString& s1, const wxUString& s2);
    /**
       Inequality operator. wxUString additionally provides overloads for
       wxString, C string, UTF-16 strings, 32-bit strings, char buffers,
       single characters etc.
    */
inline bool operator!=(const wxUString& s1, const wxUString& s2);
    /**
       Comparison operator. wxUString additionally provides overloads for
       wxString, C string, UTF-16 strings, 32-bit strings, char buffers,
       single characters etc.
    */
inline bool operator< (const wxUString& s1, const wxUString& s2);
    /**
       Comparison operator. wxUString additionally provides overloads for
       wxString, C string, UTF-16 strings, 32-bit strings, char buffers,
       single characters etc.
    */
inline bool operator> (const wxUString& s1, const wxUString& s2);
    /**
       Comparison operator. wxUString additionally provides overloads for
       wxString, C string, UTF-16 strings, 32-bit strings, char buffers,
       single characters etc.
    */
inline bool operator<=(const wxUString& s1, const wxUString& s2);
    /**
       Comparison operator. wxUString additionally provides overloads for
       wxString, C string, UTF-16 strings, 32-bit strings, char buffers,
       single characters etc.
    */
inline bool operator>=(const wxUString& s1, const wxUString& s2);
