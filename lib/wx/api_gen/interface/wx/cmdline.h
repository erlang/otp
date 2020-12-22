/////////////////////////////////////////////////////////////////////////////
// Name:        cmdline.h
// Purpose:     interface of wxCmdLineParser
// Author:      wxWidgets team
// Licence:     wxWindows licence
/////////////////////////////////////////////////////////////////////////////

/**
    wxCmdLineEntryDesc::flags field is a combination of these bit masks.

    Notice that by default (i.e. if flags are just 0), options are optional
    (sic) and each call to wxCmdLineParser::AddParam() allows one more
    parameter - this may be changed by giving non-default flags to it, i.e. use
    @c wxCMD_LINE_OPTION_MANDATORY to require that the option is given and
    @c wxCMD_LINE_PARAM_OPTIONAL to make a parameter optional.

    Also, @c wxCMD_LINE_PARAM_MULTIPLE may be specified if the programs accepts a
    variable number of parameters - but it only can be given for the last
    parameter in the command line description. If you use this flag, you will
    probably need to use wxCmdLineEntryDesc::GetParamCount() to retrieve the
    number of parameters effectively specified after calling
    wxCmdLineEntryDesc::Parse().

    @c wxCMD_LINE_NEEDS_SEPARATOR can be specified to require a separator (either
    a colon, an equal sign or white space) between the option name and its
    value. By default, no separator is required.

    @c wxCMD_LINE_SWITCH_NEGATABLE can be specified if you want to allow the
    user to specify the switch in both normal form and in negated one (e.g.
    /R-). You will need to use wxCmdLineParser::FoundSwitch() to distinguish
    between the normal and negated forms of the switch. This flag is new since
    wxWidgets 2.9.2.

    @c wxCMD_LINE_HIDDEN can be specified for arguments that should exist but
    are not to be included in the output of Usage(). These could be, for
    example, diagnostics switches that are not useful to the end user.
    This flags is new since wxWidgets 3.1.1.
*/
enum wxCmdLineEntryFlags
{
    wxCMD_LINE_OPTION_MANDATORY = 0x01, ///< This option must be given.
    wxCMD_LINE_PARAM_OPTIONAL   = 0x02, ///< The parameter may be omitted.
    wxCMD_LINE_PARAM_MULTIPLE   = 0x04, ///< The parameter may be repeated.
    wxCMD_LINE_OPTION_HELP      = 0x08, ///< This option is a help request.
    wxCMD_LINE_NEEDS_SEPARATOR  = 0x10, ///< Must have a separator before the value.
    wxCMD_LINE_SWITCH_NEGATABLE = 0x20, ///< This switch can be negated (e.g. /S-)
    wxCMD_LINE_HIDDEN           = 0x40  ///< This switch is not listed by Usage()
};

/**
    The possible values of wxCmdLineEntryDesc::type which specify the type of
    the value accepted by an option.
*/
enum wxCmdLineParamType
{
    wxCMD_LINE_VAL_STRING,
    wxCMD_LINE_VAL_NUMBER,
    wxCMD_LINE_VAL_DATE,
    wxCMD_LINE_VAL_DOUBLE,
    wxCMD_LINE_VAL_NONE
};

/**
    The type of a command line entity used for wxCmdLineEntryDesc::kind.
*/
enum wxCmdLineEntryType
{
    /// A boolean argument of the program; e.g. @c -v to enable verbose mode.
    wxCMD_LINE_SWITCH,

    /// An argument with an associated value; e.g. @c "-o filename" to specify
    /// an optional output filename.
    wxCMD_LINE_OPTION,

    /// A parameter: a required program argument.
    wxCMD_LINE_PARAM,

    /// Additional usage text. See wxCmdLineParser::AddUsageText.
    wxCMD_LINE_USAGE_TEXT,

    wxCMD_LINE_NONE     ///< Use this to terminate the list.
};

/**
    The state of a switch as returned by wxCmdLineParser::FoundSwitch().

    @since 2.9.2
*/
enum wxCmdLineSwitchState
{
    /// The switch was found in negated form, i.e. followed by a '-'.
    wxCMD_SWITCH_OFF,

    /// The switch was not found at all on the command line.
    wxCMD_SWITCH_NOT_FOUND

    /// The switch was found (and was not negated)
    wxCMD_SWITCH_ON
};


/**
    Flags determining wxCmdLineParser::ConvertStringToArgs() behaviour.
 */
enum wxCmdLineSplitType
{
    wxCMD_LINE_SPLIT_DOS,
    wxCMD_LINE_SPLIT_UNIX
};

/**
    The structure wxCmdLineEntryDesc is used to describe a command line
    switch, option or parameter. An array of such structures should be passed
    to wxCmdLineParser::SetDesc().

    Note that the meanings of parameters of the wxCmdLineParser::AddXXX() functions
    are the same as of the corresponding fields in this structure.
*/
struct wxCmdLineEntryDesc
{
    /**
        The kind of this program argument.
        See ::wxCmdLineEntryType for more info.
    */
    wxCmdLineEntryType kind;

    /**
        The usual, short, name of the switch or the option.

        It may contain only letters, digits and the underscores.
        This field is unused if <tt>kind == wxCMD_LINE_PARAM</tt>.
    */
    const char *shortName;

    /**
        The long name for this program argument (may be empty if the option
        has no long name).

        It may contain only letters, digits and the underscores.
        This field is unused if <tt>kind == wxCMD_LINE_PARAM</tt>.
    */
    const char *longName;

    /**
        This description is used by the wxCmdLineParser::Usage() method to
        construct a help message explaining the syntax of the program.
    */
    const char *description;

    /**
        The type associated with this option (ignored if <tt>kind != wxCMD_LINE_OPTION</tt>).
        See ::wxCmdLineParamType for more info.
    */
    wxCmdLineParamType type;

    /**
        A combination of one or more ::wxCmdLineEntryFlags enum values.
    */
    int flags;
};

/**
    The interface wxCmdLineArg provides information for an instance of argument
    passed on command line.

    Example of use:

    @code
    wxCmdLineParser parser;

    for (wxCmdLineArgs::const_iterator itarg=parser.GetArguments().begin();
                                       itarg!=parser.GetArguments().end();
                                       ++itarg)
    {
        wxString optionName;
        switch (itarg->GetKind())
        {
        case wxCMD_LINE_SWITCH:
            if (itarg->IsNegated()) {
            }
            else {
            }
            break;

        case wxCMD_LINE_OPTION:
            // assuming that all the options have a short name
            optionName = itarg->GetShortName();

            switch (itarg->GetType()) {
                case wxCMD_LINE_VAL_NUMBER:
                    // do something with itarg->GetLongVal();
                    break;

                case wxCMD_LINE_VAL_DOUBLE:
                    // do something with itarg->GetDoubleVal();
                    break;

                case wxCMD_LINE_VAL_DATE:
                    // do something with itarg->GetDateVal();
                    break;

                case wxCMD_LINE_VAL_STRING:
                    // do something with itarg->GetStrVal();
                    break;
            }
            break;

        case wxCMD_LINE_PARAM:
            // do something with itarg->GetStrVal();
            break;
        }
    }
    @endcode

    With C++11, the for loop could be written:
    @code
    for (const auto &arg : parser.GetArguments()) {
        // working on arg as with *itarg above
    }
    @endcode

    @since 3.1.0
*/
class wxCmdLineArg
{
public:
    virtual ~wxCmdLineArg();

    /**
        Returns the command line argument value as a wxDateTime.

        @note This call works only for @c wxCMD_LINE_VAL_DATE options
    */
    virtual const wxDateTime& GetDateVal() const = 0;

    /**
        Returns the command line argument value as a double.

        @note This call works only for @c wxCMD_LINE_VAL_DOUBLE options
    */
    virtual double GetDoubleVal() const = 0;

    /**
        Returns the command line argument entry kind.

        @note Parameters can only be retrieved as strings, with GetStrVal()

        @see wxCmdLineEntryType, GetType()
    */
    virtual wxCmdLineEntryType GetKind() const = 0;

    /**
        Returns the command line argument value as a long.

        @note This call works only for @c wxCMD_LINE_VAL_NUMBER options
    */
    virtual long GetLongVal() const = 0;

    /**
        Returns the command line argument long name if any.

        @note This call makes sense only for options and switches
    */
    virtual wxString GetLongName() const = 0;

    /**
        Returns the command line argument short name if any.

        @note This call makes sense only for options and switches
    */
    virtual wxString GetShortName() const = 0;

    /**
        Returns the command line argument value as a string.

        @note This call works only for @c wxCMD_LINE_VAL_STRING options
        and parameters
    */
    virtual const wxString& GetStrVal() const = 0;

    /**
        Returns the command line argument parameter type

        @note This call makes sense only for options
        (i.e. GetKind() == @c wxCMD_LINE_OPTION).

        @see wxCmdLineParamType, GetKind()
    */
    virtual wxCmdLineParamType GetType() const = 0;

    /**
        Returns true if the switch was negated

        @note This call works only for switches.
    */
    virtual bool IsNegated() const = 0;
};

/**
    An ordered collection of wxCmdLineArg providing an iterator to enumerate
    the arguments passed on command line.

    @see wxCmdLineParser::GetArguments()

    @since 3.1.0
*/
class wxCmdLineArgs
{
public:
    /**
        A bidirectional constant iterator exposing the command line arguments as
        wxCmdLineArg references.
    */
    class const_iterator;

    const_iterator begin() const;
    const_iterator end() const;

    /**
        Returns the number of command line arguments in this collection.
    */
    size_t size() const;
};

/**
    @class wxCmdLineParser

    wxCmdLineParser is a class for parsing the command line.

    It has the following features:

    - distinguishes options, switches and parameters
    - allows option grouping
    - allows both short and long options
    - automatically generates the usage message from the command line description
    - checks types of the options values (number, date, ...).

    To use it you should follow these steps:

    -# @ref cmdlineparser_construction "Construct" an object of this class
       giving it the command line to parse and optionally its description or
       use the @c AddXXX() functions later.
    -# Call Parse().
    -# Use Found() to retrieve the results.

    You can also use wxApp's default command line processing just overriding
    wxAppConsole::OnInitCmdLine() and wxAppConsole::OnCmdLineParsed().

    In the documentation below the following terminology is used:

    - @b switch: a boolean option which can be given or not, but which doesn't have
                 any value. We use the word @e switch to distinguish
                 such boolean options from more generic options like those
                 described below. For example, @c "-v" might be a switch
                 meaning "enable verbose mode".
    - @b option: a switch with a value associated to it.
                 For example, @c "-o filename" might be an
                 option for specifying the name of the output file.
    - @b parameter: a required program argument.


    @section cmdlineparser_construction Construction

    Before Parse() can be called, the command line parser object must have the
    command line to parse and also the rules saying which switches, options and
    parameters are valid - this is called command line description in what
    follows.

    You have complete freedom of choice as to when specify the required
    information, the only restriction is that it must be done before calling
    Parse().

    To specify the command line to parse you may use either one of constructors
    accepting it (wxCmdLineParser(int, char**) or
    wxCmdLineParser(const wxString&) usually) or, if you use the default
    constructor, you can do it later by calling SetCmdLine().

    The same holds for command line description: it can be specified either in
    the constructor (with or without the command line itself) or constructed
    later using either SetDesc() or combination of AddSwitch(), AddOption(),
    AddParam() and AddUsageText() methods.

    Using constructors or SetDesc() uses a (usually const static) table
    containing the command line description. If you want to decide which
    options to accept during the run-time, using one of the AddXXX() functions
    above might be preferable.


    @section cmdlineparser_customization Customization

    wxCmdLineParser has several global options which may be changed by the
    application. All of the functions described in this section should be
    called before Parse().

    First global option is the support for long (also known as GNU-style)
    options. The long options are the ones which start with two dashes and look
    like "\--verbose", i.e. they generally are complete words and not some
    abbreviations of them. As long options are used by more and more
    applications, they are enabled by default, but may be disabled with
    DisableLongOptions().

    Another global option is the set of characters which may be used to start
    an option (otherwise, the word on the command line is assumed to be a
    parameter). Under Unix, @c "-" is always used, but Windows has at least two
    common choices for this: @c "-" and @c "/". Some programs also use "+". The
    default is to use what suits most the current platform, but may be changed
    with SetSwitchChars() method.

    Finally, SetLogo() can be used to show some application-specific text
    before the explanation given by Usage() function.


    @section cmdlineparser_parsing Parsing the Command Line

    After the command line description was constructed and the desired options
    were set, you can finally call Parse() method. It returns 0 if the command
    line was correct and was parsed, -1 if the help option was specified (this
    is a separate case as, normally, the program will terminate after this) or
    a positive number if there was an error during the command line parsing.

    In the latter case, the appropriate error message and usage information are
    logged by wxCmdLineParser itself using the standard wxWidgets logging
    functions.


    @section cmdlineparser_results Getting Results

    After calling Parse() (and if it returned 0), you may access the results of
    parsing using one of overloaded Found() methods.

    For a simple switch, you will simply call Found to determine if the switch
    was given or not, for an option or a parameter, you will call a version of
    Found() which also returns the associated value in the provided variable.
    All Found() functions return true if the switch or option were found in the
    command line or false if they were not specified.


    @library{wxbase}
    @category{appmanagement}

    @see wxApp::argc, wxApp::argv, @ref page_samples_console
*/
class wxCmdLineParser
{
public:
    /**
        Default constructor, you must use SetCmdLine() later.
    */
    wxCmdLineParser();

    /**
        Constructor which specifies the command line to parse. This is the
        traditional (Unix) command line format. The parameters @a argc and
        @a argv have the same meaning as the typical @c main() function.

        This constructor is available in both ANSI and Unicode modes because under
        some platforms the command line arguments are passed as ASCII strings
        even to Unicode programs.
    */
    wxCmdLineParser(int argc, char** argv);

    /**
        Constructor which specifies the command line to parse.
        This is the traditional (Unix) command line format.

        The parameters @a argc and @a argv have the same meaning as the typical
        @c main() function.

        This constructor is only available in Unicode build.
    */
    wxCmdLineParser(int argc, wchar_t** argv);

    /**
        Constructor which specify the command line to parse in Windows format.
        The parameter cmdline has the same meaning as the corresponding
        parameter of @c WinMain().
    */
    wxCmdLineParser(const wxString& cmdline);

    /**
        Specifies the @ref SetDesc() "command line description" but not the
        command line. You must use SetCmdLine() later.
    */
    wxCmdLineParser(const wxCmdLineEntryDesc* desc);

    /**
        Specifies both the command line (in Unix format) and the
        @ref SetDesc() "command line description".
    */
    wxCmdLineParser(const wxCmdLineEntryDesc* desc, int argc, char** argv);

    /**
        Specifies both the command line (in Windows format) and the
        @ref SetDesc() "command line description".
    */
    wxCmdLineParser(const wxCmdLineEntryDesc* desc,
                    const wxString& cmdline);

    /**
        Frees resources allocated by the object.

        @note This destructor is not virtual, don't use this class
              polymorphically.
    */
    ~wxCmdLineParser();

    /**
        Adds an option with only long form.

        This is just a convenient wrapper for AddOption() passing an empty
        string as short option name.

        @since 2.9.3
     */
    void AddLongOption(const wxString& lng,
                       const wxString& desc = wxEmptyString,
                       wxCmdLineParamType type = wxCMD_LINE_VAL_STRING,
                       int flags = 0);

    /**
        Adds a switch with only long form.

        This is just a convenient wrapper for AddSwitch() passing an empty
        string as short switch name.

        @since 2.9.3
     */

    void AddLongSwitch(const wxString& lng,
                       const wxString& desc = wxEmptyString,
                       int flags = 0);

    /**
        Add an option @a name with an optional long name @a lng (no long name
        if it is empty, which is default) taking a value of the given type
        (string by default) to the command line description.
    */
    void AddOption(const wxString& name,
                   const wxString& lng = wxEmptyString,
                   const wxString& desc = wxEmptyString,
                   wxCmdLineParamType type = wxCMD_LINE_VAL_STRING,
                   int flags = 0);

    /**
        Add a parameter of the given @a type to the command line description.
    */
    void AddParam(const wxString& desc = wxEmptyString,
                  wxCmdLineParamType type = wxCMD_LINE_VAL_STRING,
                  int flags = 0);

    /**
        Add a switch @a name with an optional long name @a lng (no long name if
        it is empty, which is default), description @a desc and flags @a flags
        to the command line description.
    */
    void AddSwitch(const wxString& name,
                   const wxString& lng = wxEmptyString,
                   const wxString& desc = wxEmptyString,
                   int flags = 0);

    /**
        Add a string @a text to the command line description shown by Usage().

        @since 2.9.0
    */
    void AddUsageText(const wxString& text);

    /**
        Returns @true if long options are enabled, otherwise @false.

        @see EnableLongOptions()
    */
    bool AreLongOptionsEnabled() const;

    /**
        Breaks down the string containing the full command line in words.

        Words are separated by whitespace and double quotes can be used to
        preserve the spaces inside the words.

        By default, this function uses Windows-like word splitting algorithm,
        i.e. single quotes have no special meaning and backslash can't be used
        to escape spaces neither. With @c wxCMD_LINE_SPLIT_UNIX flag Unix
        semantics is used, i.e. both single and double quotes can be used and
        backslash can be used to escape all the other special characters.
    */
    static wxArrayString
    ConvertStringToArgs(const wxString& cmdline,
                        wxCmdLineSplitType flags = wxCMD_LINE_SPLIT_DOS);

    /**
        Identical to EnableLongOptions(@false).
    */
    void DisableLongOptions();

    /**
        Enable or disable support for the long options.

        As long options are not (yet) POSIX-compliant, this option allows
        disabling them.

        @see @ref cmdlineparser_customization and AreLongOptionsEnabled()
    */
    void EnableLongOptions(bool enable = true);

    /**
        Returns @true if the given switch was found, @false otherwise.
    */
    bool Found(const wxString& name) const;

    /**
        Returns whether the switch was found on the command line and whether it
        was negated.

        This method can be used for any kind of switch but is especially useful
        for switches that can be negated, i.e. were added with
        wxCMD_LINE_SWITCH_NEGATABLE flag, as otherwise Found() is simpler to
        use.

        However Found() doesn't allow to distinguish between switch specified
        normally, i.e. without dash following it, and negated switch, i.e. with
        the following dash. This method will return @c wxCMD_SWITCH_ON or @c
        wxCMD_SWITCH_OFF depending on whether the switch was negated or not.
        And if the switch was not found at all, @c wxCMD_SWITCH_NOT_FOUND is
        returned.

        @since 2.9.2
    */
    wxCmdLineSwitchState FoundSwitch(const wxString& name) const;

    /**
        Returns true if an option taking a string value was found and stores
        the value in the provided pointer (which should not be @NULL).
    */
    bool Found(const wxString& name, wxString* value) const;

    /**
        Returns @true if an option taking an integer value was found and stores
        the value in the provided pointer (which should not be @NULL).
    */
    bool Found(const wxString& name, long* value) const;

    /**
        Returns @true if an option taking a float value was found and stores
        the value in the provided pointer (which should not be @NULL).
    */
    bool Found(const wxString& name, double* value) const;

    /**
        Returns @true if an option taking a date value was found and stores the
        value in the provided pointer (which should not be @NULL).
    */
    bool Found(const wxString& name, wxDateTime* value) const;

    /**
        Returns the value of Nth parameter (as string only).
    */
    wxString GetParam(size_t n = 0) const;

    /**
        Returns the number of parameters found. This function makes sense
        mostly if you had used @c wxCMD_LINE_PARAM_MULTIPLE flag.
    */
    size_t GetParamCount() const;

    /**
        Returns the collection of arguments

        @note The returned object just refers to the command line parser. The
        command line parser must live longer than it.

        @see wxCmdLineArgs
        @since 3.1.0
    */
    wxCmdLineArgs GetArguments() const;

    /**
        Parse the command line, return 0 if ok, -1 if @c "-h" or @c "\--help"
        option was encountered and the help message was given or a positive
        value if a syntax error occurred.

        @param giveUsage
            If @true (default), the usage message is given if a syntax error
            was encountered while parsing the command line or if help was
            requested. If @false, only error messages about possible syntax
            errors are given, use Usage to show the usage message from the
            caller if needed.
    */
    int Parse(bool giveUsage = true);

    //@{
    /**
        Set the command line to parse after using one of the constructors which
        don't do it.
    */
    void SetCmdLine(int argc, char** argv);
    void SetCmdLine(int argc, wchar_t** argv);
    void SetCmdLine(const wxString& cmdline);
    //@}

    /**
        Constructs the command line description.

        Take the command line description from the wxCMD_LINE_NONE terminated
        table.

        Example of usage:

        @code
        static const wxCmdLineEntryDesc cmdLineDesc[] =
        {
            { wxCMD_LINE_SWITCH, "v", "verbose", "be verbose" },
            { wxCMD_LINE_SWITCH, "q", "quiet",   "be quiet" },

            { wxCMD_LINE_OPTION, "o", "output",  "output file" },
            { wxCMD_LINE_OPTION, "i", "input",   "input dir" },
            { wxCMD_LINE_OPTION, "s", "size",    "output block size", wxCMD_LINE_VAL_NUMBER },
            { wxCMD_LINE_OPTION, "d", "date",    "output file date", wxCMD_LINE_VAL_DATE },

            { wxCMD_LINE_PARAM,  NULL, NULL, "input file", wxCMD_LINE_VAL_STRING, wxCMD_LINE_PARAM_MULTIPLE },

            { wxCMD_LINE_NONE }
        };

        wxCmdLineParser parser;

        parser.SetDesc(cmdLineDesc);
        @endcode
    */
    void SetDesc(const wxCmdLineEntryDesc* desc);

    /**
        The @a logo is some extra text which will be shown by Usage() method.
    */
    void SetLogo(const wxString& logo);

    /**
        @a switchChars contains all characters with which an option or switch
        may start. Default is @c "-" for Unix, @c "-/" for Windows.
    */
    void SetSwitchChars(const wxString& switchChars);

    /**
        Give the standard usage message describing all program options. It will
        use the options and parameters descriptions specified earlier, so the
        resulting message will not be helpful to the user unless the
        descriptions were indeed specified.

        @see SetLogo()
    */
    void Usage() const;

    /**
        Return the string containing the program usage description.

        Call Usage() to directly show this string to the user.
     */
    wxString GetUsageString() const;
};

