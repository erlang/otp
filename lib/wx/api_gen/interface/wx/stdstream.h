/////////////////////////////////////////////////////////////////////////////
// Name:        stdstream.h
// Purpose:     interface of wxStdInputStream, wxStdInputStreamBuffer,
//              wxStdOutputStream, wxStdOutputStreamBuffer
// Author:      Jonathan Liu <net147@gmail.com>
// Copyright:   (c) 2009 Jonathan Liu
// Licence:     wxWindows licence
/////////////////////////////////////////////////////////////////////////////

/**
    @class wxStdInputStreamBuffer

    wxStdInputStreamBuffer is a std::streambuf derived stream buffer which
    reads from a wxInputStream.

    Example:
    @code
    wxFFileInputStream file("input.txt.gz");
    wxZlibInputStream gzipInput(file, wxZLIB_GZIP);
    wxStdInputStreamBuffer gzipStreamBuffer(gzipInput);

    // redirect std::cin to read from compressed file
    std::streambuf* streamBufferOld = std::cin.rdbuf(&gzipStreamBuffer);

    // prompt for integer
    int number;
    std::cout << "Enter an integer: " << std::flush;
    std::cin >> number;
    std::cout << std::endl;
    std::cout << "You entered the integer " << number << "." << std::endl;

    // restore std::cin
    std::cin.rdbuf(streamBufferOld);
    @endcode

    @library{wxbase}
    @category{streams}

    @see wxInputStream, wxStdInputStream
*/
class wxStdInputStreamBuffer : public std::streambuf
{
public:
    /**
        Creates a std::steambuf derived stream buffer which reads from a
        wxInputStream.

        @param stream
            Stream to read from.
    */
    wxStdInputStreamBuffer(wxInputStream& stream);

    /**
        Destructor.
    */
    virtual ~wxStdInputStreamBuffer();
};

/**
    @class wxStdInputStream

    wxStdInputStream is a std::istream derived stream which reads from
    a wxInputStream.

    Example:
    @code
    wxFFileInputStream file("words.txt");
    wxStdInputStream in(file);
    std::vector<std::string> words;

    // read words from words.txt
    std::copy(std::istream_iterator<std::string>(in),
              std::istream_iterator<std::string>(),
              std::back_inserter(words));

    // sort and remove duplicates
    std::sort(words.begin(), words.end());
    words.resize(std::unique(words.begin(), words.end()) - words.begin());

    // print words
    std::copy(words.begin(), words.end(),
              std::ostream_iterator<std::string>(std::cout, "\n"));
    @endcode

    @library{wxbase}
    @category{streams}

    @see wxInputStream, wxStdInputStreamBuffer
*/
class wxStdInputStream : public std::istream
{
public:
    /**
        Creates a std::istream derived stream which reads from a
        wxInputStream.

        @param stream
            Stream to read from.
    */
    wxStdInputStream(wxInputStream& stream);

    /**
        Destructor.
    */
    virtual ~wxStdInputStream();
};

/**
    @class wxStdOutputStreamBuffer

    wxStdOutputStreamBuffer is a std::streambuf derived stream buffer which
    writes to a wxOutputStream.

    Example:
    @code
    wxFFileOutputStream file("cout.txt.gz");
    wxZlibOutputStream gzipOutput(file, -1, wxZLIB_GZIP);
    wxStdOutputStreamBuffer gzipStreamBuffer(gzipOutput);

    // redirect std::cout to cout.txt.gz using GZIP compression
    std::streambuf* streamBufferOld = std::cout.rdbuf(&gzipStreamBuffer);

    // write to std::cout
    std::cout << "Hello world!" << std::endl;

    // restore std::cout
    std::cout.rdbuf(streamBufferOld);
    @endcode

    @library{wxbase}
    @category{streams}

    @see wxOutputStream, wxStdOutputStream
*/
class wxStdOutputStreamBuffer : public std::streambuf
{
public:
    /**
        Creates a std::steambuf derived stream buffer which writes to a
        wxOutputStream.

        @param stream
            Stream to write to.
    */
    wxStdOutputStreamBuffer(wxOutputStream& stream);

    /**
        Destructor.
    */
    virtual ~wxStdOutputStreamBuffer();
};

/**
    @class wxStdOutputStream

    wxStdOutputStream is a std::ostream derived stream which writes to a
    wxOutputStream.

    Example:
    @code
    wxFFileOutputStream file("out.txt.gz");
    wxZlibOutputStream gzipOutput(file, -1, wxZLIB_GZIP);
    wxStdOutputStream out(gzipOutput);

    out << "Hello world!" << std::endl;
    @endcode

    @library{wxbase}
    @category{streams}

    @see wxOutputStream, wxStdOutputStreamBuffer
*/
class wxStdOutputStream : public std::ostream
{
public:
    /**
        Creates a std::ostream derived stream which writes to a
        wxOutputStream.

        @param stream
            Stream to write to.
    */
    wxStdOutputStream(wxOutputStream& stream);

    /**
        Destructor.
    */
    virtual ~wxStdOutputStream();
};
