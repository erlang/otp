/////////////////////////////////////////////////////////////////////////////
// Name:        sckstrm.h
// Purpose:     interface of wxSocketOutputStream
// Author:      wxWidgets team
// Licence:     wxWindows licence
/////////////////////////////////////////////////////////////////////////////

/**
    @class wxSocketOutputStream

    This class implements an output stream which writes data from
    a connected socket. Note that this stream is purely sequential
    and it does not support seeking.

    @library{wxnet}
    @category{net,streams}

    @see wxSocketBase
*/
class wxSocketOutputStream : public wxOutputStream
{
public:
    /**
        Creates a new write-only socket stream using the specified initialized
        socket connection.
    */
    wxSocketOutputStream(wxSocketBase& s);
};



/**
    @class wxSocketInputStream

    This class implements an input stream which reads data from
    a connected socket. Note that this stream is purely sequential
    and it does not support seeking.

    @library{wxnet}
    @category{net,streams}

    @see wxSocketBase
*/
class wxSocketInputStream : public wxInputStream
{
public:
    /**
        Creates a new read-only socket stream using the specified initialized
        socket connection.
    */
    wxSocketInputStream(wxSocketBase& s);
};

