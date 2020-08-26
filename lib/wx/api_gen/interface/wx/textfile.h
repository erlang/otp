/////////////////////////////////////////////////////////////////////////////
// Name:        textfile.h
// Purpose:     interface of wxTextFile
// Author:      wxWidgets team
// Licence:     wxWindows licence
/////////////////////////////////////////////////////////////////////////////

// TODO: document wxTextBuffer

/**
    The line termination type.
*/
enum wxTextFileType
{
    wxTextFileType_None,  //!< incomplete (the last line of the file only)
    wxTextFileType_Unix,  //!< line is terminated with 'LF' = 0xA = 10 = '\\n'
    wxTextFileType_Dos,   //!< line is terminated with 'CR' 'LF'
    wxTextFileType_Mac,   //!< line is terminated with 'CR' = 0xD = 13 = '\\r'
    wxTextFileType_Os2    //!< line is terminated with 'CR' 'LF'
};

/**
    @class wxTextFile

    The wxTextFile is a simple class which allows working with text files on line by
    line basis. It also understands the differences in line termination characters
    under different platforms and will not do anything bad to files with "non
    native" line termination sequences - in fact, it can be also used to modify the
    text files and change the line termination characters from one type (say DOS) to
    another (say Unix).

    One word of warning: the class is not at all optimized for big files and thus
    it will load the file entirely into memory when opened. Of course, you should
    not work in this way with large files (as an estimation, anything over 1 Megabyte
    is surely too big for this class). On the other hand, it is not a serious
    limitation for small files like configuration files or program sources
    which are well handled by wxTextFile.

    The typical things you may do with wxTextFile in order are:

    - Create and open it: this is done with either wxTextFile::Create or wxTextFile::Open
      function which opens the file (name may be specified either as the argument to
      these functions or in the constructor), reads its contents in memory (in the
      case of wxTextFile::Open()) and closes it.
    - Work with the lines in the file: this may be done either with "direct
      access" functions like wxTextFile::GetLineCount and wxTextFile::GetLine
      (@e operator[] does exactly the same but looks more like array addressing)
      or with "sequential access" functions which include wxTextFile::GetFirstLine,
      wxTextFile::GetNextLine and also wxTextFile::GetLastLine, wxTextFile::GetPrevLine.
      For the sequential access functions the current line number is maintained: it is
      returned by wxTextFile::GetCurrentLine and may be changed with wxTextFile::GoToLine.
    - Add/remove lines to the file: wxTextFile::AddLine and wxTextFile::InsertLine
      add new lines while wxTextFile::RemoveLine deletes the existing ones.
      wxTextFile::Clear resets the file to empty.
    - Save your changes: notice that the changes you make to the file will @b not be
      saved automatically; calling wxTextFile::Close or doing nothing discards them!
      To save the changes you must explicitly call wxTextFile::Write - here, you may
      also change the line termination type if you wish.

    @library{wxbase}
    @category{file}

    @see wxFile
*/
class wxTextFile
{
public:
    /**
        Default type for current platform determined at compile time.
     */
    static const wxTextFileType typeDefault;

    /**
        Default constructor, use Create() or Open() with a file name parameter to
        initialize the object.
    */
    wxTextFile();

    /**
        Constructor does not load the file into memory, use Open() to do it.
    */
    wxTextFile(const wxString& strFile);

    /**
        Destructor does nothing.
    */
    virtual ~wxTextFile();

    /**
        Adds a line to the end of file.
    */
    void AddLine(const wxString& str, wxTextFileType type = typeDefault);

    /**
        Delete all lines from the file, set current line number to 0.
    */
    void Clear();

    /**
        Closes the file and frees memory, @b "losing all changes".
        Use Write() if you want to save them.
    */
    bool Close();

    /**
        Creates the file with the name which was given in the
        wxTextFile(const wxString&) constructor.
        The array of file lines is initially empty.

        It will fail if the file already exists, Open() should be used in this case.
    */
    bool Create();

    /**
        Creates the file with the given name.
        The array of file lines is initially empty.

        It will fail if the file already exists, Open() should be used in this case.
    */
    bool Create(const wxString& strFile);

    /**
        Returns @true if the current line is the last one.
    */
    bool Eof() const;

    /**
        Return @true if file exists - the name of the file should have been specified
        in the constructor before calling Exists().
    */
    bool Exists() const;

    /**
        Returns the current line: it has meaning only when you're using
        GetFirstLine()/GetNextLine() functions, it doesn't get updated when
        you're using "direct access" functions like GetLine().
        GetFirstLine() and GetLastLine() also change the value of the current line,
        as well as GoToLine().
    */
    size_t GetCurrentLine() const;

    /**
        Get the line termination string corresponding to given constant.

        @e typeDefault is the value defined during the compilation and corresponds
        to the native format of the platform, i.e. it will be @c wxTextFileType_Dos
        under Windows and @c wxTextFileType_Unix under Unix (including OS
        X, the value @c wxTextFileType_Mac was only used for classic Mac OS
        versions).
    */
    static const wxChar* GetEOL(wxTextFileType type = typeDefault);

    /**
        This method together with GetNextLine() allows more "iterator-like"
        traversal of the list of lines, i.e. you may write something like:

        @code
        wxTextFile file;
        ...
        for ( str = file.GetFirstLine(); !file.Eof(); str = file.GetNextLine() )
        {
            // do something with the current line in str
        }
        // do something with the last line in str
        @endcode
    */
    wxString& GetFirstLine();

    /**
        Gets the last line of the file.

        Together with GetPrevLine() it allows enumerating the lines
        in the file from the end to the beginning like this:

        @code
        wxTextFile file;
        ...
        for ( str = file.GetLastLine();
            file.GetCurrentLine() > 0;
            str = file.GetPrevLine() )
        {
            // do something with the current line in str
        }
        // do something with the first line in str
        @endcode
    */
    wxString& GetLastLine();

    /**
        Retrieves the line number @a n from the file.

        The returned line may be modified when non-const method is used but you
        shouldn't add line terminator at the end -- this will be done by
        wxTextFile itself.
    */
    //@{
    wxString& GetLine(size_t n);
    const wxString& GetLine(size_t n) const;
    //@}

    /**
        Get the number of lines in the file.
    */
    size_t GetLineCount() const;

    /**
        Get the type of the line (see also wxTextFile::GetEOL).
    */
    wxTextFileType GetLineType(size_t n) const;

    /**
        Get the name of the file.
    */
    const wxString& GetName() const;

    /**
        Gets the next line (see GetFirstLine() for the example).
    */
    wxString& GetNextLine();

    /**
        Gets the previous line in the file.
    */
    wxString& GetPrevLine();

    /**
        Changes the value returned by GetCurrentLine() and used by GetFirstLine()
        and GetNextLine().
    */
    void GoToLine(size_t n);

    /**
        Guess the type of file (which is supposed to be opened).

        If sufficiently many lines of the file are in DOS/Unix/Mac format,
        the corresponding value will be returned.
        If the detection mechanism fails @c wxTextFileType_None is returned.
    */
    wxTextFileType GuessType() const;

    /**
        Insert a line before the line number @a n.
    */
    void InsertLine(const wxString& str, size_t n,
                    wxTextFileType type = typeDefault);

    /**
        Returns @true if the file is currently opened.
    */
    bool IsOpened() const;

    /**
        Opens the file with the name which was given in the wxTextFile(const wxString&)
        constructor and also loads file in memory on success.

        It will fail if the file does not exist, Create() should be used in this case.

        The @a conv argument is only meaningful in Unicode build of wxWidgets when
        it is used to convert the file to wide character representation.
    */
    bool Open(const wxMBConv& conv = wxConvAuto());

    /**
        Opens the file with the given name and also loads file in memory on success.

        It will fail if the file does not exist, Create() should be used in this case.

        The @a conv argument is only meaningful in Unicode build of wxWidgets when
        it is used to convert the file to wide character representation.
    */
    bool Open(const wxString& strFile, const wxMBConv& conv = wxConvAuto());

    /**
        Delete line number @a n from the file.
    */
    void RemoveLine(size_t n);

    /**
        Change the file on disk.

        The @a typeNew parameter allows you to change the file format
        (default argument means "don't change type") and may be used to convert,
        for example, DOS files to Unix.

        The @a conv argument is only meaningful in Unicode build of wxWidgets when
        it is used to convert all lines to multibyte representation before writing
        them to physical file.

        @return
            @true if operation succeeded, @false if it failed.
    */
    bool Write(wxTextFileType typeNew = wxTextFileType_None,
               const wxMBConv& conv = wxConvAuto());

    /**
        The same as GetLine().
    */
    wxString& operator[](size_t n) const;
};

