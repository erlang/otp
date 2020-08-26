/////////////////////////////////////////////////////////////////////////////
// Name:        cmndata.h
// Purpose:     interface of print wx*Data classes
// Author:      wxWidgets team
// Licence:     wxWindows licence
/////////////////////////////////////////////////////////////////////////////

/**
    @class wxPageSetupDialogData

    This class holds a variety of information related to wxPageSetupDialog.

    It contains a wxPrintData member which is used to hold basic printer
    configuration data (as opposed to the user-interface configuration settings
    stored by wxPageSetupDialogData).

    @library{wxcore}
    @category{printing,data}

    @see @ref overview_printing, wxPageSetupDialog
*/
class wxPageSetupDialogData : public wxObject
{
public:
    /**
        Default constructor.
    */
    wxPageSetupDialogData();

    /**
        Copy constructor.
    */
    wxPageSetupDialogData(const wxPageSetupDialogData& data);

    /**
        Construct an object from a print data object.
    */
    wxPageSetupDialogData(const wxPrintData& printData);

    /**
        Destructor.
    */
    virtual ~wxPageSetupDialogData();

    /**
        Enables or disables the "Help" button (Windows only).
    */
    void EnableHelp(bool flag);

    /**
        Enables or disables the margin controls (Windows only).
    */
    void EnableMargins(bool flag);

    /**
        Enables or disables the orientation control (Windows only).
    */
    void EnableOrientation(bool flag);

    /**
        Enables or disables the paper size control (Windows only).
    */
    void EnablePaper(bool flag);

    /**
        Enables or disables the "Printer" button, which invokes a printer setup
        dialog.
    */
    void EnablePrinter(bool flag);

    /**
        Returns @true if the dialog will simply return default printer
        information (such as orientation) instead of showing a dialog (Windows
        only).
    */
    bool GetDefaultInfo() const;

    /**
        Returns @true if the page setup dialog will take its minimum margin
        values from the currently selected printer properties (Windows only).
    */
    bool GetDefaultMinMargins() const;

    /**
        Returns @true if the printer setup button is enabled.
    */
    bool GetEnableHelp() const;

    /**
        Returns @true if the margin controls are enabled (Windows only).
    */
    bool GetEnableMargins() const;

    /**
        Returns @true if the orientation control is enabled (Windows only).
    */
    bool GetEnableOrientation() const;

    /**
        Returns @true if the paper size control is enabled (Windows only).
    */
    bool GetEnablePaper() const;

    /**
        Returns @true if the printer setup button is enabled.
    */
    bool GetEnablePrinter() const;

    /**
        Returns the right (x) and bottom (y) margins in millimetres.
    */
    wxPoint GetMarginBottomRight() const;

    /**
        Returns the left (x) and top (y) margins in millimetres.
    */
    wxPoint GetMarginTopLeft() const;

    /**
        Returns the right (x) and bottom (y) minimum margins the user can enter
        (Windows only). Units are in millimetres.
    */
    wxPoint GetMinMarginBottomRight() const;

    /**
        Returns the left (x) and top (y) minimum margins the user can enter
        (Windows only). Units are in millimetres.
    */
    wxPoint GetMinMarginTopLeft() const;

    /**
        Returns the paper id (stored in the internal wxPrintData object).

        @see wxPrintData::SetPaperId()
    */
    wxPaperSize GetPaperId() const;

    /**
        Returns the paper size in millimetres.
    */
    wxSize GetPaperSize() const;

    /**
        Returns a reference to the print data associated with this object.
    */
    wxPrintData& GetPrintData();
    const wxPrintData& GetPrintData() const;

    /**
        Returns @true if the print data associated with the dialog data is
        valid. This can return @false on Windows if the current printer is not
        set, for example. On all other platforms, it returns @true.
    */
    bool IsOk() const;

    /**
        Pass @true if the dialog will simply return default printer information
        (such as orientation) instead of showing a dialog (Windows only).
    */
    void SetDefaultInfo(bool flag);

    /**
        Pass @true if the page setup dialog will take its minimum margin values
        from the currently selected printer properties (Windows only). Units
        are in millimetres.
    */
    void SetDefaultMinMargins(bool flag);

    /**
        Sets the right (x) and bottom (y) margins in millimetres.
    */
    void SetMarginBottomRight(const wxPoint& pt);

    /**
        Sets the left (x) and top (y) margins in millimetres.
    */
    void SetMarginTopLeft(const wxPoint& pt);

    /**
        Sets the right (x) and bottom (y) minimum margins the user can enter
        (Windows only). Units are in millimetres.
    */
    void SetMinMarginBottomRight(const wxPoint& pt);

    /**
        Sets the left (x) and top (y) minimum margins the user can enter
        (Windows only). Units are in millimetres.
    */
    void SetMinMarginTopLeft(const wxPoint& pt);

    /**
        Sets the paper size id. Calling this function overrides the explicit
        paper dimensions passed in SetPaperSize().

        @see wxPrintData::SetPaperId()
    */
    void SetPaperId(wxPaperSize id);

    /**
        Sets the paper size in millimetres. If a corresponding paper id is
        found, it will be set in the internal wxPrintData object, otherwise the
        paper size overrides the paper id.
    */
    void SetPaperSize(const wxSize& size);

    /**
        Sets the print data associated with this object.
    */
    void SetPrintData(const wxPrintData& printData);

    /**
        Assigns print data to this object.
    */
    wxPageSetupDialogData& operator =(const wxPrintData& data);

    /**
        Assigns page setup data to this object.
    */
    wxPageSetupDialogData& operator =(const wxPageSetupDialogData& data);
};

/**
    Enumeration of various printer bin sources.

    @see wxPrintData::SetBin()
*/
enum wxPrintBin
{
    wxPRINTBIN_DEFAULT,

    wxPRINTBIN_ONLYONE,
    wxPRINTBIN_LOWER,
    wxPRINTBIN_MIDDLE,
    wxPRINTBIN_MANUAL,
    wxPRINTBIN_ENVELOPE,
    wxPRINTBIN_ENVMANUAL,
    wxPRINTBIN_AUTO,
    wxPRINTBIN_TRACTOR,
    wxPRINTBIN_SMALLFMT,
    wxPRINTBIN_LARGEFMT,
    wxPRINTBIN_LARGECAPACITY,
    wxPRINTBIN_CASSETTE,
    wxPRINTBIN_FORMSOURCE,

    wxPRINTBIN_USER,
};

/**
    @class wxPrintData

    This class holds a variety of information related to printers and printer
    device contexts. This class is used to create a wxPrinterDC and a
    wxPostScriptDC. It is also used as a data member of wxPrintDialogData and
    wxPageSetupDialogData, as part of the mechanism for transferring data
    between the print dialogs and the application.

    @library{wxcore}
    @category{printing,data}

    @see @ref overview_printing, wxPrintDialog, wxPageSetupDialog,
         wxPrintDialogData, wxPageSetupDialogData, @ref overview_cmndlg_print,
         wxPrinterDC, wxPostScriptDC
*/
class wxPrintData : public wxObject
{
public:
    /**
        Default constructor.
    */
    wxPrintData();

    /**
        Copy constructor.
    */
    wxPrintData(const wxPrintData& data);

    /**
        Destructor.
    */
    virtual ~wxPrintData();

    /**
        Returns the current bin (papersource). By default, the system is left
        to select the bin (@c wxPRINTBIN_DEFAULT is returned).

        See SetBin() for the full list of bin values.
    */
    wxPrintBin GetBin() const;

    /**
        Returns @true if collation is on.
    */
    bool GetCollate() const;

    /**
        Returns @true if colour printing is on.
    */
    bool GetColour() const;

    /**
        Returns the duplex mode. One of wxDUPLEX_SIMPLEX, wxDUPLEX_HORIZONTAL,
        wxDUPLEX_VERTICAL.
    */
    wxDuplexMode GetDuplex() const;

    /**
        Returns the number of copies requested by the user.
    */
    int GetNoCopies() const;

    /**
        Gets the orientation. This can be wxLANDSCAPE or wxPORTRAIT.
    */
    wxPrintOrientation GetOrientation() const;

    /**
        Returns the paper size id.

        @see SetPaperId()
    */
    wxPaperSize GetPaperId() const;

    /**
        Returns the printer name. If the printer name is the empty string, it
        indicates that the default printer should be used.
    */
    const wxString& GetPrinterName() const;

    /**
        Returns the current print quality. This can be a positive integer,
        denoting the number of dots per inch, or  one of the following
        identifiers:

        - wxPRINT_QUALITY_HIGH
        - wxPRINT_QUALITY_MEDIUM
        - wxPRINT_QUALITY_LOW
        - wxPRINT_QUALITY_DRAFT

        On input you should pass one of these identifiers, but on return you
        may get back a positive integer indicating the current resolution
        setting.
    */
    wxPrintQuality GetQuality() const;

    /**
        Returns @true if the print data is valid for using in print dialogs.
        This can return @false on Windows if the current printer is not set,
        for example. On all other platforms, it returns @true.
    */
    bool IsOk() const;

    /**
        Sets the current bin.
    */
    void SetBin(wxPrintBin flag);

    /**
        Sets collation to on or off.
    */
    void SetCollate(bool flag);

    /**
        Sets colour printing on or off.
    */
    void SetColour(bool flag);

    /**
        Returns the duplex mode. One of wxDUPLEX_SIMPLEX, wxDUPLEX_HORIZONTAL,
        wxDUPLEX_VERTICAL.
    */
    void SetDuplex(wxDuplexMode mode);

    /**
        Sets the default number of copies to be printed out.
    */
    void SetNoCopies(int n);

    /**
        Sets the orientation. This can be wxLANDSCAPE or wxPORTRAIT.
    */
    void SetOrientation(wxPrintOrientation orientation);

    /**
        Sets the paper id. This indicates the type of paper to be used. For a
        mapping between paper id, paper size and string name, see
        wxPrintPaperDatabase in @c "paper.h" (not yet documented).

        @see SetPaperSize()
    */
    void SetPaperId(wxPaperSize paperId);

    /**
        Sets custom paper size.

        This method can be used to set up custom paper size. When using a
        standard size, prefer to use SetPaperId() instead.
     */
    void SetPaperSize(const wxSize& size);

    /**
        Sets the printer name. This can be the empty string to indicate that
        the default printer should be used.
    */
    void SetPrinterName(const wxString& printerName);

    /**
        Sets the desired print quality. This can be a positive integer,
        denoting the number of dots per inch, or one of the following
        identifiers:

        - wxPRINT_QUALITY_HIGH
        - wxPRINT_QUALITY_MEDIUM
        - wxPRINT_QUALITY_LOW
        - wxPRINT_QUALITY_DRAFT

        On input you should pass one of these identifiers, but on return you
        may get back a positive integer indicating the current resolution
        setting.
    */
    void SetQuality(wxPrintQuality quality);

    /**
        Assigns print data to this object.
    */
    wxPrintData& operator =(const wxPrintData& data);


    wxString GetFilename() const;
    void SetFilename( const wxString &filename );

    wxPrintMode GetPrintMode() const ;
    void SetPrintMode(wxPrintMode printMode) ;
};



/**
    @class wxPrintDialogData

    This class holds information related to the visual characteristics of
    wxPrintDialog. It contains a wxPrintData object with underlying printing
    settings.

    @library{wxcore}
    @category{printing,cmndlg,data}

    @see @ref overview_printing, wxPrintDialog, @ref overview_cmndlg_print
*/
class wxPrintDialogData : public wxObject
{
public:
    /**
        Default constructor.
    */
    wxPrintDialogData();

    /**
        Copy constructor.
    */
    wxPrintDialogData(const wxPrintDialogData& dialogData);

    /**
        Construct an object from a print dialog data object.
    */
    wxPrintDialogData(const wxPrintData& printData);

    /**
        Destructor.
    */
    virtual ~wxPrintDialogData();

    /**
        Enables or disables the "Help" button.
    */
    void EnableHelp(bool flag);

    /**
        Enables or disables the "Page numbers" controls.
    */
    void EnablePageNumbers(bool flag);

    /**
        Enables or disables the "Print to file" checkbox.
    */
    void EnablePrintToFile(bool flag);

    /**
        Enables or disables the "Selection" radio button.
    */
    void EnableSelection(bool flag);

    /**
        Returns @true if the user requested that all pages be printed.
    */
    bool GetAllPages() const;

    /**
        Returns @true if the user requested that the document(s) be collated.
    */
    bool GetCollate() const;

    /**
        Returns the @e from page number, as entered by the user.
    */
    int GetFromPage() const;

    /**
        Returns the @e maximum page number.
    */
    int GetMaxPage() const;

    /**
        Returns the @e minimum page number.
    */
    int GetMinPage() const;

    /**
        Returns the number of copies requested by the user.
    */
    int GetNoCopies() const;

    /**
        Returns a reference to the internal wxPrintData object.
    */
    wxPrintData& GetPrintData();

    /**
        Returns @true if the user has selected printing to a file.
    */
    bool GetPrintToFile() const;

    /**
        Returns @true if the user requested that the selection be printed
        (where "selection" is a concept specific to the application).
    */
    bool GetSelection() const;

    /**
        Returns the @e "print to" page number, as entered by the user.
    */
    int GetToPage() const;

    /**
        Returns @true if the print data is valid for using in print dialogs.
        This can return @false on Windows if the current printer is not set,
        for example. On all other platforms, it returns @true.
    */
    bool IsOk() const;

    /**
        Sets the "Collate" checkbox to @true or @false.
    */
    void SetCollate(bool flag);

    /**
        Sets the @e from page number.
    */
    void SetFromPage(int page);

    /**
        Sets the @e maximum page number.
    */
    void SetMaxPage(int page);

    /**
        Sets the @e minimum page number.
    */
    void SetMinPage(int page);

    /**
        Sets the default number of copies the user has requested to be printed
        out.
    */
    void SetNoCopies(int n);

    /**
        Sets the internal wxPrintData.
    */
    void SetPrintData(const wxPrintData& printData);

    /**
        Sets the "Print to file" checkbox to @true or @false.
    */
    void SetPrintToFile(bool flag);

    /**
        Selects the "Selection" radio button. The effect of printing the
        selection depends on how the application implements this command, if at
        all.
    */
    void SetSelection(bool flag);

    /**
        @deprecated This function has been deprecated since version 2.5.4.

        Determines whether the dialog to be shown will be the Print dialog
        (pass @false) or Print Setup dialog (pass @true).

    */
    void SetSetupDialog(bool flag);

    /**
        Sets the @e "print to" page number.
    */
    void SetToPage(int page);

    /**
        Assigns print data to this object.
    */
    void operator =(const wxPrintData& data);

    /**
        Assigns another print dialog data object to this object.
    */
    void operator =(const wxPrintDialogData& data);
};

