/////////////////////////////////////////////////////////////////////////////
// Name:        tokenzr.h
// Purpose:     interface of wxStringTokenizer
// Author:      wxWidgets team
// Licence:     wxWindows licence
/////////////////////////////////////////////////////////////////////////////

/**
    The behaviour of wxStringTokenizer is governed by the
    wxStringTokenizer::wxStringTokenizer() or wxStringTokenizer::SetString()
    with the parameter @e mode, which may be one of the following:
*/
enum wxStringTokenizerMode
{
    wxTOKEN_INVALID = -1,   ///< Invalid tokenizer mode.

    /**
        Default behaviour: wxStringTokenizer will behave in the same way as
        @c strtok() (::wxTOKEN_STRTOK) if the delimiters string only contains
        white space characters but, unlike the standard function, it will
        behave like ::wxTOKEN_RET_EMPTY, returning empty tokens if this is not
        the case. This is helpful for parsing strictly formatted data where
        the number of fields is fixed but some of them may be empty (i.e.
        @c TAB or comma delimited text files).
    */
    wxTOKEN_DEFAULT,

    /**
        In this mode, the empty tokens in the middle of the string will be returned,
        i.e. @c "a::b:" will be tokenized in three tokens @c 'a', @c '' and @c 'b'.
        Notice that all trailing delimiters are ignored in this mode, not just the last one,
        i.e. a string @c "a::b::" would still result in the same set of tokens.
    */
    wxTOKEN_RET_EMPTY,

    /**
        In this mode, empty trailing tokens (including the one after the last delimiter
        character) will be returned as well. The string @c "a::b:" will be tokenized in
        four tokens: the already mentioned ones and another empty one as the last one
        and a string @c "a::b::" will have five tokens.
    */
    wxTOKEN_RET_EMPTY_ALL,

    /**
        In this mode, the delimiter character after the end of the current token (there
        may be none if this is the last token) is returned appended to the token.
        Otherwise, it is the same mode as ::wxTOKEN_RET_EMPTY. Notice that there is no
        mode like this one but behaving like ::wxTOKEN_RET_EMPTY_ALL instead of
        ::wxTOKEN_RET_EMPTY, use ::wxTOKEN_RET_EMPTY_ALL and
        wxStringTokenizer::GetLastDelimiter() to emulate it.
    */
    wxTOKEN_RET_DELIMS,

    /**
        In this mode the class behaves exactly like the standard @c strtok() function:
        the empty tokens are never returned.
    */
    wxTOKEN_STRTOK
};

/// Default wxStringTokenizer delimiters are the usual white space characters.
#define wxDEFAULT_DELIMITERS " \t\r\n"

/**
    @class wxStringTokenizer

    wxStringTokenizer helps you to break a string up into a number of tokens.
    It replaces the standard C function @c strtok() and also extends it in a
    number of ways.

    To use this class, you should create a wxStringTokenizer object, give it the
    string to tokenize and also the delimiters which separate tokens in the string
    (by default, white space characters will be used).

    Then wxStringTokenizer::GetNextToken() may be called repeatedly until
    wxStringTokenizer::HasMoreTokens() returns @false.

    For example:

    @code
    wxStringTokenizer tokenizer("first:second:third:fourth", ":");
    while ( tokenizer.HasMoreTokens() )
    {
        wxString token = tokenizer.GetNextToken();

        // process token here
    }
    @endcode

    @library{wxbase}
    @category{data}

    @see ::wxStringTokenize()
*/
class wxStringTokenizer : public wxObject
{
public:
    /**
        Default constructor. You must call SetString() before calling any other
        methods.
    */
    wxStringTokenizer();
    /**
        Constructor. Pass the string to tokenize, a string containing
        delimiters, and the @a mode specifying how the string should be
        tokenized.

        @see SetString()
   */
    wxStringTokenizer(const wxString& str,
                      const wxString& delims = wxDEFAULT_DELIMITERS,
                      wxStringTokenizerMode mode = wxTOKEN_DEFAULT);

    /**
        Returns the number of tokens remaining in the input string. The number
        of tokens returned by this function is decremented each time
        GetNextToken() is called and when it reaches 0, HasMoreTokens()
        returns @false.
    */
    size_t CountTokens() const;

    /**
        Returns the delimiter which ended scan for the last token returned by
        GetNextToken() or @c NUL if there had been no calls to this function
        yet or if it returned the trailing empty token in
        ::wxTOKEN_RET_EMPTY_ALL mode.

        @since 2.7.0
    */
    wxChar GetLastDelimiter() const;

    /**
        Returns the next token or empty string if the end of string was reached.
    */
    wxString GetNextToken();

    /**
        Returns the current position (i.e.\ one index after the last returned
        token or 0 if GetNextToken() has never been called) in the original
        string.
    */
    size_t GetPosition() const;

    /**
        Returns the part of the initial string which is yet to be tokenized.

        That is, the substring from the current position up to the end,
        possibly empty if there are no more tokens left.
    */
    wxString GetString() const;

    /**
        Returns @true if the tokenizer has further tokens, @false if none are left.
    */
    bool HasMoreTokens() const;

    /**
        Initializes the tokenizer. Pass the string to tokenize, a string
        containing delimiters, and the @a mode specifying how the string
        should be tokenized.
    */
    void SetString(const wxString& str,
                   const wxString& delims = wxDEFAULT_DELIMITERS,
                   wxStringTokenizerMode mode = wxTOKEN_DEFAULT);
};


/** @addtogroup group_funcmacro_string */
//@{

/**
    This is a convenience function wrapping wxStringTokenizer which simply
    returns all tokens found in the given @a str as an array.

    Please see wxStringTokenizer::wxStringTokenizer for the description
    of the other parameters.

    @return The array with the parsed tokens.

    @header{wx/tokenzr.h}
*/
wxArrayString
wxStringTokenize(const wxString& str,
                 const wxString& delims = wxDEFAULT_DELIMITERS,
                 wxStringTokenizerMode mode = wxTOKEN_DEFAULT);

//@}
