/////////////////////////////////////////////////////////////////////////////
// Name:        dcps.h
// Purpose:     interface of wxPostScriptDC
// Author:      wxWidgets team
// Licence:     wxWindows licence
/////////////////////////////////////////////////////////////////////////////

/**
    @class wxPostScriptDC

    This defines the wxWidgets Encapsulated PostScript device context, which
    can write PostScript files on any platform. See wxDC for descriptions of
    the member functions.

    @section start_doc Starting a document

    Document should be started with call to StartDoc() prior to calling any
    function to execute a drawing operation.
    However, some functions, like SetFont(), may be legitimately called even
    before StartDoc().

    @library{wxbase}
    @category{dc}
*/
class wxPostScriptDC : public wxDC
{
public:
    wxPostScriptDC();

    /**
        Constructs a PostScript printer device context from a wxPrintData object.
    */
    wxPostScriptDC(const wxPrintData& printData);

};

