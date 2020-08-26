/////////////////////////////////////////////////////////////////////////////
// Name:        txtstrm.h
// Purpose:     interface of wxTextInputStream
// Author:      wxWidgets team
// Licence:     wxWindows licence
/////////////////////////////////////////////////////////////////////////////



/**
    @class wxTextInputStream

    This class provides functions that reads text data using an input stream,
    allowing you to read text, floats, and integers.

    The wxTextInputStream correctly reads text files (or streams) in DOS,
    Macintosh and Unix formats and reports a single newline char as a line
    ending.

    wxTextInputStream::operator>>() is overloaded and you can use this class
    like a standard C++ iostream. Note, however, that the arguments are the
    fixed size types wxUint32, wxInt32 etc and on a typical 32-bit computer,
    none of these match to the "long" type (wxInt32 is defined as int on 32-bit
    architectures) so that you cannot use long. To avoid problems (here and
    elsewhere), make use of wxInt32, wxUint32 and similar types.

    If you're scanning through a file using wxTextInputStream, you should check
    for @c EOF @b before reading the next item (word / number), because
    otherwise the last item may get lost. You should however be prepared to
    receive an empty item (empty string / zero number) at the end of file,
    especially on Windows systems. This is unavoidable because most (but not
    all) files end with whitespace (i.e. usually a newline).

    For example:

    @code
    wxFileInputStream input( "mytext.txt" );
    wxTextInputStream text( input );
    wxUint8 i1;
    float f2;
    wxString word;

    text >> i1;       // read a 8 bit integer.
    text >> i1 >> f2; // read a 8 bit integer followed by float.
    text >> word;     // read a word
    @endcode

    @library{wxbase}
    @category{streams}

    @see wxTextOutputStream
*/
class wxTextInputStream
{
public:
    /**
        Constructs a text stream associated to the given input stream.

        @param stream
            The underlying input stream.
        @param sep
            The initial string separator characters.
        @param conv
            <b>In Unicode build only:</b> The encoding converter used to
            convert the bytes in the underlying input stream to characters.
    */
    wxTextInputStream(wxInputStream& stream, const wxString& sep = " \\ t",
                      const wxMBConv& conv = wxConvAuto());

    /**
        Destructor.
    */
    ~wxTextInputStream();

    /**
        Returns a pointer to the underlying input stream object.

        @since 2.9.2
    */
    const wxInputStream& GetInputStream() const;

    /**
        Reads a character, returns 0 if there are no more characters in the
        stream.
    */
    wxChar GetChar();

    /**
        Reads a unsigned 16 bit integer from the stream.

        See Read8() for the description of the @a base parameter.
    */
    wxUint16 Read16(int base = 10);

    /**
        Reads a signed 16 bit integer from the stream.

        See Read8() for the description of the @a base parameter.
    */
    wxInt16 Read16S(int base = 10);

    /**
        Reads a 32 bit unsigned integer from the stream.

        See Read8() for the description of the @a base parameter.
    */
    wxUint32 Read32(int base = 10);

    /**
        Reads a 32 bit signed integer from the stream.

        See Read8() for the description of the @a base parameter.
    */
    wxInt32 Read32S(int base = 10);

    /**
        Reads a 64 bit unsigned integer from the stream.

        See Read8() for the description of the @a base parameter.

        @since 3.1.0
    */
    wxUint64 Read64(int base = 10);

    /**
        Reads a 64 bit signed integer from the stream.

        See Read8() for the description of the @a base parameter.

        @since 3.1.0
    */
    wxInt64 Read64S(int base = 10);

    /**
        Reads a single unsigned byte from the stream, given in base @a base.

        The value of @a base must be comprised between 2 and 36, inclusive, or
        be a special value 0 which means that the usual rules of C numbers are
        applied: if the number starts with @c 0x it is considered to be in base
        16, if it starts with 0 - in base 8 and in base 10 otherwise. Note that
        you may not want to specify the base 0 if you are parsing the numbers
        which may have leading zeroes as they can yield unexpected (to the user
        not familiar with C) results.
    */
    wxUint8 Read8(int base = 10);

    /**
        Reads a single signed byte from the stream.

        See Read8() for the description of the @a base parameter.
    */
    wxInt8 Read8S(int base = 10);

    /**
        Reads a double (IEEE encoded) from the stream.
    */
    double ReadDouble();

    /**
        Reads a line from the input stream and returns it (without the end of
        line character).
    */
    wxString ReadLine();

    /**
        @deprecated Use ReadLine() or ReadWord() instead.

        Same as ReadLine().
    */
    wxString ReadString();

    /**
        Reads a word (a sequence of characters until the next separator) from
        the input stream.

        @see SetStringSeparators()
    */
    wxString ReadWord();

    /**
        Sets the characters which are used to define the word boundaries in
        ReadWord().

        The default separators are the @c space and @c TAB characters.
    */
    void SetStringSeparators(const wxString& sep);

    /**
        Uses ReadWord() to extract @a word.
    */
    wxTextInputStream& operator>>(wxString& word);

    /**
        Extracts the next char in the stream.
    */
    wxTextInputStream& operator>>(char& c);

    /**
        Uses GetChar() to extract @a wc.
        @note Only available in Unicode builds.
    */
    wxTextInputStream& operator>>(wchar_t& wc);

    /**
        Uses Read16S() to extract @a i.
    */
    wxTextInputStream& operator>>(wxInt16& i);

    /**
        Uses Read32S() to extract @a i.
    */
    wxTextInputStream& operator>>(wxInt32& i);

    /**
        Uses Read64S() to extract @a i.
    */
    wxTextInputStream& operator>>(wxInt64& i);

    /**
        Uses Read16() to extract @a i.
    */
    wxTextInputStream& operator>>(wxUint16& i);

    /**
        Uses Read32() to extract @a i.
    */
    wxTextInputStream& operator>>(wxUint32& i);

    /**
        Uses Read64() to extract @a i.
    */
    wxTextInputStream& operator>>(wxUint64& i);

    /**
        Uses ReadDouble() to extract @a i.
    */
    wxTextInputStream& operator>>(double& i);

    /**
        Uses truncated result of ReadDouble() to extract @a f.
    */
    wxTextInputStream& operator>>(float& f);

    /**
        Calls @c func(*this).
    */
    wxTextInputStream& operator>>(__wxTextInputManip func);
};


/**
    Specifies the end-of-line characters to use with wxTextOutputStream.
*/
enum wxEOL
{
    /**
        Specifies wxTextOutputStream to use the native end-of-line characters.
    */
    wxEOL_NATIVE,

    /**
        Specifies wxTextOutputStream to use Unix end-of-line characters.
    */
    wxEOL_UNIX,

    /**
        Specifies wxTextOutputStream to use Mac end-of-line characters.
    */
    wxEOL_MAC,

    /**
        Specifies wxTextOutputStream to use DOS end-of-line characters.
    */
    wxEOL_DOS
};


/**
    @class wxTextOutputStream

    This class provides functions that write text data using an output stream,
    allowing you to write text, floats, and integers.

    You can also simulate the C++ @c std::cout class:

    @code
    wxFFileOutputStream output( stderr );
    wxTextOutputStream cout( output );

    cout << "This is a text line" << endl;
    cout << 1234;
    cout << 1.23456;
    @endcode

    The wxTextOutputStream writes text files (or streams) on Windows or Unix in
    their native formats (concerning the line ending).

    @library{wxbase}
    @category{streams}

    @see wxTextInputStream
*/
class wxTextOutputStream
{
public:
    /**
        Constructs a text stream object associated to the given output stream.

        @param stream
            The output stream.
        @param mode
            The end-of-line mode. One of ::wxEOL_NATIVE, ::wxEOL_DOS,
            ::wxEOL_MAC and ::wxEOL_UNIX.
        @param conv
            <b>In Unicode build only:</b> The object used to convert
            Unicode text into ASCII characters written to the output stream.
    */
    wxTextOutputStream(wxOutputStream& stream,
                       wxEOL mode = wxEOL_NATIVE,
                       const wxMBConv& conv = wxConvAuto());

    /**
        Destroys the wxTextOutputStream object.

        Also calls Flush().
    */
    virtual ~wxTextOutputStream();

    /**
        Flushes the stream.

        This method should be called when using stateful encodings (currently
        the only example of such encoding in wxWidgets is wxMBConvUTF7) to
        write the end of the encoded data to the stream.

        @since 2.9.0
     */
    void Flush();

    /**
        Returns a pointer to the underlying output stream object.

        @since 2.9.2
    */
    const wxOutputStream& GetOutputStream() const;

    /**
        Returns the end-of-line mode. One of ::wxEOL_DOS, ::wxEOL_MAC and
        ::wxEOL_UNIX.
    */
    wxEOL GetMode();

    /**
        Writes a character to the stream.
    */
    wxTextOutputStream& PutChar(wxChar c);

    /**
        Set the end-of-line mode. One of ::wxEOL_NATIVE, ::wxEOL_DOS,
        ::wxEOL_MAC and ::wxEOL_UNIX.
    */
    void SetMode(wxEOL mode = wxEOL_NATIVE);

    /**
        Writes @a i to the stream using wxString::operator<<().

        @since 3.1.0
    */
    template<typename T>
    void Write(const T& i);

     /**
        Writes the 64 bit unsigned integer @a i64 to the stream.

        @since 3.1.0
    */
    void Write64(wxUint64 i64);

    /**
        Writes the 32 bit unsigned integer @a i32 to the stream.
    */
    void Write32(wxUint32 i32);

    /**
        Writes the 16 bit unsigned integer @a i16 to the stream.
    */
    void Write16(wxUint16 i16);

    /**
        Writes the single unsigned byte @a i8 to the stream.
    */
    void Write8(wxUint8 i8);

    /**
        Writes the double @a f to the stream using the IEEE format.
    */
    virtual void WriteDouble(double f);

    /**
        Writes @a string as a line. Depending on the end-of-line mode the end of
        line ('\\n') characters in the string are converted to the correct line
        ending terminator.
    */
    virtual void WriteString(const wxString& string);

    /**
        Uses WriteString() to insert @a string.
    */
    wxTextOutputStream& operator<<(const wxString& string);

    /**
        Uses WriteString() to insert @a c as ASCII.
    */
    wxTextOutputStream& operator<<(char c);

    /**
        Uses PutChar() to insert @a wc.
        @note Only available in Unicode builds.
    */
    wxTextOutputStream& operator<<(wchar_t wc);

    /**
        Uses Write() to insert @a c.
    */
    wxTextOutputStream& operator<<(wxInt16 c);

    /**
        Uses Write() to insert @a c.
    */
    wxTextOutputStream& operator<<(wxInt32 c);

    /**
        Uses Write() to insert @a c.
    */
    wxTextOutputStream& operator<<(wxInt64 c);

    /**
        Uses Write() to insert @a c.
    */
    wxTextOutputStream& operator<<(wxUint16 c);

    /**
        Uses Write() to insert @a c.
    */
    wxTextOutputStream& operator<<(wxUint32 c);

    /**
        Uses Write() to insert @a c.
    */
    wxTextOutputStream& operator<<(wxUint64 c);

    /**
        Uses Write() to insert @a f.
    */
    wxTextOutputStream& operator<<(double f);

    /**
        Uses Write() to insert @a f.
    */
    wxTextOutputStream& operator<<(float f);

    /**
        Calls @c func(*this).
    */
    wxTextOutputStream& operator<<(__wxTextOutputManip func);
};


/** @addtogroup group_funcmacro_misc */
//@{

/**
    Writes @c '\\n' to @a stream.

    @header{wx/textstrm.h}
*/
wxTextOutputStream& endl(wxTextOutputStream& stream);

//@}
