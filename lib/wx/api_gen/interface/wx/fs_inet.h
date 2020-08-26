/////////////////////////////////////////////////////////////////////////////
// Name:        wx/fs_inet.h
// Purpose:     HTTP and FTP file system
// Author:      Vaclav Slavik
// Copyright:   (c) 1999 Vaclav Slavik
// Licence:     wxWindows licence
/////////////////////////////////////////////////////////////////////////////

/**
    @class wxInternetFSHandler

    A file system handler for accessing files from internet servers.
*/
class wxInternetFSHandler : public wxFileSystemHandler
{
public:
    wxInternetFSHandler();
};

