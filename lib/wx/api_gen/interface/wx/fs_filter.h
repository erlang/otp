/////////////////////////////////////////////////////////////////////////////
// Name:        wx/fs_filter.h
// Purpose:     Filter file system handler
// Author:      Mike Wetherell
// Copyright:   (c) 2006 Mike Wetherell
// Licence:     wxWindows licence
/////////////////////////////////////////////////////////////////////////////

/**
    @class wxFilterFSHandler

    Filter file system handler.
*/

class wxFilterFSHandler : public wxFileSystemHandler
{
public:
    wxFilterFSHandler();
    virtual ~wxFilterFSHandler();
};

