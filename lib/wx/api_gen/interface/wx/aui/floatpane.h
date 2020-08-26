///////////////////////////////////////////////////////////////////////////////
// Name:        wx/aui/floatpane.h
// Purpose:     wxaui: wx advanced user interface - docking window manager
// Author:      Benjamin I. Williams
// Created:     2005-05-17
// Copyright:   (C) Copyright 2005, Kirix Corporation, All Rights Reserved.
// Licence:     wxWindows Library Licence, Version 3.1
///////////////////////////////////////////////////////////////////////////////


class wxAuiFloatingFrame : public wxFrame
{
public:
    wxAuiFloatingFrame(wxWindow* parent,
                   wxAuiManager* ownerMgr,
                   const wxAuiPaneInfo& pane,
                   wxWindowID id = wxID_ANY,
                   long style = wxRESIZE_BORDER | wxSYSTEM_MENU | wxCAPTION |
                                wxFRAME_NO_TASKBAR | wxFRAME_FLOAT_ON_PARENT |
                                wxCLIP_CHILDREN
                   );
    virtual ~wxAuiFloatingFrame();
    void SetPaneWindow(const wxAuiPaneInfo& pane);
    wxAuiManager* GetOwnerManager() const;

protected:
    virtual void OnMoveStart();
    virtual void OnMoving(const wxRect& windowRect, wxDirection dir);
    virtual void OnMoveFinished();
};
