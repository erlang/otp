/////////////////////////////////////////////////////////////////////////////
// Name:        sstream.h
// Purpose:     interface of wxStringInputStream
// Author:      wxWidgets team
// Licence:     wxWindows licence
/////////////////////////////////////////////////////////////////////////////

/**
    @class wxStringInputStream

    This class implements an input stream which reads data from a string.
    It supports seeking.

    @library{wxbase}
    @category{streams}
*/
class wxStringInputStream : public wxInputStream
{
public:
    /**
        Creates a new read-only stream using the specified string.

        Note that the string is copied by the stream so if the original string is
        modified after using this constructor, changes to it are not reflected
        when reading from stream.
    */
    wxStringInputStream(const wxString& s);
};



/**
    @class wxStringOutputStream

    This class implements an output stream which writes data either to a
    user-provided or internally allocated string.

    Note that currently this stream does not support seeking but can tell
    its current position.

    @library{wxbase}
    @category{streams}
*/
class wxStringOutputStream : public wxOutputStream
{
public:
    /**
        Construct a new stream object writing the data to a string.

        If the provided pointer is non-@NULL, data will be written to it.
        Otherwise, an internal string is used for the data written to this
        stream, use GetString() to get access to it.

        If @a str is used, data written to the stream is appended to the current
        contents of it, i.e. the string is not cleared here. However if it is not
        empty, the positions returned by wxOutputStream::TellO will be offset by
        the initial string length, i.e. initial stream position will be the
        initial length of the string and not 0.

        Notice that the life time of @a conv must be greater than the life time
        of this object itself as it stores a reference to it. Also notice that
        with default value of this argument the data written to the stream must
        be valid UTF-8, pass @c wxConvISO8859_1 to deal with arbitrary 8 bit data.
    */
    explicit wxStringOutputStream(wxString* pString = NULL, wxMBConv& conv = wxConvUTF8);

    /**
        Returns the string containing all the data written to the stream so far.
    */
    const wxString& GetString() const;
};

