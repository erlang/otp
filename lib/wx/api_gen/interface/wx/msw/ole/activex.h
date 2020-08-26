/////////////////////////////////////////////////////////////////////////////
// Name:        msw/ole/activex.h
// Purpose:     interface of wxActiveXEvent
// Author:      wxWidgets team
// Licence:     wxWindows licence
/////////////////////////////////////////////////////////////////////////////

/**
    @class wxActiveXEvent

    An event class for handling ActiveX events passed from wxActiveXContainer.

    ActiveX events are basically a function call with the parameters passed
    through an array of wxVariants along with a return value that is a wxVariant
    itself. What type the parameters or return value are depends on the context
    (i.e. what the .idl specifies).

    @beginEventTable{wxActiveXEvent}
    @event{EVT_ACTIVEX(func)}
        Sent when the ActiveX control hosted by wxActiveXContainer receives an
        ActiveX event.
    @endEventTable

    ActiveX event parameters can get extremely complex and may be beyond the
    abilities of wxVariant. If 'operator[]' fails, prints an error messages or
    crashes the application, event handlers should use GetNativeParameters()
    instead to obtain the original event information.
    Calls to operator[] and GetNativeParmeters() can be mixed. It is valid
    to handle some parameters of an event with operator[] and others directly
    through GetNativeParameters(). It is \b not valid however to manipulate
    the same parameter using both approaches at the same time.

    @onlyfor{wxmsw}

    @library{wxcore}
    @category{events}
*/
class wxActiveXEvent : public wxCommandEvent
{
public:
    /**
        Returns the dispatch id of this ActiveX event.
        This is the numeric value from the .idl file specified by the id().
    */
    DISPID GetDispatchId(int idx) const;

    /**
        Obtains the number of parameters passed through the ActiveX event.
    */
    size_t ParamCount() const;

    /**
        Obtains the param name of the param number idx specifies as a string.
    */
    wxString ParamName(size_t idx) const;

    /**
        Obtains the param type of the param number idx specifies as a string.
    */
    wxString ParamType(size_t idx) const;

    /**
        Obtains the actual parameter value specified by idx.
    */
    wxVariant operator[](size_t idx);

    /**
        Obtain the original MSW parameters for the event.
        Event handlers can use this information to handle complex event parameters
        that are beyond the scope of wxVariant.
        The information returned here is the information passed to the original
        'Invoke' method call.
        \return a pointer to a struct containing the original MSW event parameters
    */
    wxActiveXEventNativeMSW *GetNativeParameters() const;
};



/**
    @class wxActiveXContainer

    wxActiveXContainer is a host for an ActiveX control on Windows (and as such
    is a platform-specific class).

    Note that the HWND that the class contains is the actual HWND of the ActiveX
    control so using dynamic events and connecting to @c wxEVT_SIZE, for example,
    will receive the actual size message sent to the control.

    It is somewhat similar to the ATL class CAxWindow in operation.

    The size of the ActiveX control's content is generally guaranteed to be that
    of the client size of the parent of this wxActiveXContainer.

    You can also process ActiveX events through wxActiveXEvent.


    @section activexcontainer_example Example

    This is an example of how to use the Adobe Acrobat Reader ActiveX control to
    read PDF files (requires Acrobat Reader 4 and up).
    Controls like this are typically found and dumped from OLEVIEW.exe that is
    distributed with Microsoft Visual C++.
    This example also demonstrates how to create a backend for wxMediaCtrl.

    @code
    //+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
    //
    // wxPDFMediaBackend
    //
    // http://partners.adobe.com/public/developer/en/acrobat/sdk/pdf/iac/IACOverview.pdf
    //++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

    #include "wx/mediactrl.h"       // wxMediaBackendCommonBase
    #include "wx/msw/ole/activex.h" // wxActiveXContainer
    #include "wx/msw/ole/automtn.h" // wxAutomationObject

    const IID DIID__DPdf = {0xCA8A9781,0x280D,0x11CF,{0xA2,0x4D,0x44,0x45,0x53,0x54,0x00,0x00}};
    const IID DIID__DPdfEvents = {0xCA8A9782,0x280D,0x11CF,{0xA2,0x4D,0x44,0x45,0x53,0x54,0x00,0x00}};
    const CLSID CLSID_Pdf = {0xCA8A9780,0x280D,0x11CF,{0xA2,0x4D,0x44,0x45,0x53,0x54,0x00,0x00}};

    class WXDLLIMPEXP_MEDIA wxPDFMediaBackend : public wxMediaBackendCommonBase
    {
    public:
        wxPDFMediaBackend() : m_pAX(NULL) {}
        virtual ~wxPDFMediaBackend()
        {
            if(m_pAX)
            {
                m_pAX->DissociateHandle();
                delete m_pAX;
            }
        }
        virtual bool CreateControl(wxControl* ctrl, wxWindow* parent,
                                        wxWindowID id,
                                        const wxPoint& pos,
                                        const wxSize& size,
                                        long style,
                                        const wxValidator& validator,
                                        const wxString& name)
        {
            IDispatch* pDispatch;
            if( ::CoCreateInstance(CLSID_Pdf, NULL,
                                    CLSCTX_INPROC_SERVER,
                                    DIID__DPdf, (void**)&pDispatch) != 0 )
                return false;

            m_PDF.SetDispatchPtr(pDispatch); // wxAutomationObject will release itself

            if ( !ctrl->wxControl::Create(parent, id, pos, size,
                                    (style & ~wxBORDER_MASK) | wxBORDER_NONE,
                                    validator, name) )
                return false;

            m_ctrl = wxStaticCast(ctrl, wxMediaCtrl);
            m_pAX = new wxActiveXContainer(ctrl,
                        DIID__DPdf,
                        pDispatch);

            wxPDFMediaBackend::ShowPlayerControls(wxMEDIACTRLPLAYERCONTROLS_NONE);
            return true;
        }

        virtual bool Play()
        {
            return true;
        }
        virtual bool Pause()
        {
            return true;
        }
        virtual bool Stop()
        {
            return true;
        }

        virtual bool Load(const wxString& fileName)
        {
            if(m_PDF.CallMethod("LoadFile", fileName).GetBool())
            {
                m_PDF.CallMethod("setCurrentPage", wxVariant((long)0));
                NotifyMovieLoaded(); // initial refresh
                wxSizeEvent event;
                m_pAX->OnSize(event);
                return true;
            }

            return false;
        }
        virtual bool Load(const wxURI& location)
        {
            return m_PDF.CallMethod("LoadFile", location.BuildUnescapedURI()).GetBool();
        }
        virtual bool Load(const wxURI& WXUNUSED(location),
                        const wxURI& WXUNUSED(proxy))
        {
            return false;
        }

        virtual wxMediaState GetState()
        {
            return wxMEDIASTATE_STOPPED;
        }

        virtual bool SetPosition(wxLongLong where)
        {
            m_PDF.CallMethod("setCurrentPage", wxVariant((long)where.GetValue()));
            return true;
        }
        virtual wxLongLong GetPosition()
        {
            return 0;
        }
        virtual wxLongLong GetDuration()
        {
            return 0;
        }

        virtual void Move(int WXUNUSED(x), int WXUNUSED(y),
                        int WXUNUSED(w), int WXUNUSED(h))
        {
        }
        wxSize GetVideoSize() const
        {
            return wxDefaultSize;
        }

        virtual double GetPlaybackRate()
        {
            return 0;
        }
        virtual bool SetPlaybackRate(double)
        {
            return false;
        }

        virtual double GetVolume()
        {
            return 0;
        }
        virtual bool SetVolume(double)
        {
            return false;
        }

        virtual bool ShowPlayerControls(wxMediaCtrlPlayerControls flags)
        {
            if(flags)
            {
                m_PDF.CallMethod("setShowToolbar", true);
                m_PDF.CallMethod("setShowScrollbars", true);
            }
            else
            {
                m_PDF.CallMethod("setShowToolbar", false);
                m_PDF.CallMethod("setShowScrollbars", false);
            }

            return true;
        }

        wxActiveXContainer* m_pAX;
        wxAutomationObject m_PDF;

        wxDECLARE_DYNAMIC_CLASS(wxPDFMediaBackend);
    };

    wxIMPLEMENT_DYNAMIC_CLASS(wxPDFMediaBackend, wxMediaBackend);

    // Put this in one of your existing source files and then create a wxMediaCtrl with
    wxMediaCtrl* mymediactrl = new wxMediaCtrl(this, "myfile.pdf", wxID_ANY,
                                            wxDefaultPosition, wxSize(300,300),
                                            0, "wxPDFMediaBackend");
            // [this] is the parent window, "myfile.pdf" is the PDF file to open
    @endcode


    @onlyfor{wxmsw}

    @library{wxcore}
    @category{ctrl,ipc}

    @see wxActiveXEvent
*/
class wxActiveXContainer : public wxControl
{
public:
    /**
        Creates this ActiveX container.

        @param parent
            parent of this control. Must not be @NULL.
        @param iid
            COM IID of pUnk to query. Must be a valid interface to an ActiveX control.
        @param pUnk
            Interface of ActiveX control.
    */
    wxActiveXContainer(wxWindow* parent, REFIID iid, IUnknown* pUnk);
    /**
        Queries host's site for interface.

        @param iid
            The iid of the required interface.
        @param _interface
            Double pointer to outgoing interface. Supply your own interface if desired.
        @param desc
            The description of the outgoing interface.
        @return bool
            Return true if interface supplied else return false.
    */
    virtual bool QueryClientSiteInterface(REFIID iid, void **_interface, const char *&desc);
};

