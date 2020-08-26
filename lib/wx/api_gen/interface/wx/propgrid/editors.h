/////////////////////////////////////////////////////////////////////////////
// Name:        editors.h
// Purpose:     interface of wxPropertyGrid editors
// Author:      wxWidgets team
// Licence:     wxWindows licence
/////////////////////////////////////////////////////////////////////////////

/**
   @class wxPGWindowList

   Contains a list of editor windows returned by CreateControls.
*/

class wxPGWindowList
{
public:
    wxPGWindowList(wxWindow* primary, wxWindow* secondary = NULL);

    void SetSecondary(wxWindow* secondary);

    /** Gets window of primary editor.
        @since 3.1.4
    */
    wxWindow* GetPrimary() const;
    /** Gets window of secondary editor.
        @since 3.1.4
    */
    wxWindow* GetSecondary() const;
};



/**
    @class wxPGEditor

    Base class for custom wxPropertyGrid editors.

    @remarks
    - Names of built-in property editors are: TextCtrl, Choice,
      ComboBox, CheckBox, TextCtrlAndButton, and ChoiceAndButton. Additional
      editors include SpinCtrl and DatePickerCtrl, but using them requires
      calling wxPropertyGrid::RegisterAdditionalEditors() prior use.

    - Pointer to built-in editor is available as wxPGEditor_EditorName
      (e.g. wxPGEditor_TextCtrl).

    - Before you start using new editor you just created, you need to register
      it using static function
      wxPropertyGrid::RegisterEditorClass(), with code like this:
        @code
            wxPGEditor* editorPointer = wxPropertyGrid::RegisterEditorClass(new MyEditorClass(), "MyEditor");
        @endcode
      After that, wxPropertyGrid will take ownership of the given object, but
      you should still store editorPointer somewhere, so you can pass it to
      wxPGProperty::SetEditor(), or return it from wxPGEditor::DoGetEditorClass().

    @library{wxpropgrid}
    @category{propgrid}
*/
class wxPGEditor : public wxObject
{
public:

    /** Constructor. */
    wxPGEditor();

    /** Destructor. */
    virtual ~wxPGEditor();

    /**
        Returns pointer to the name of the editor. For example,
        wxPGEditor_TextCtrl has name "TextCtrl". If you don't need to access
        your custom editor by string name, then you do not need to implement
        this function.
    */
    virtual wxString GetName() const;

    /**
        Instantiates editor controls.

        @param propgrid
            wxPropertyGrid to which the property belongs (use as parent for control).

        @param property
            Property for which this method is called.

        @param pos
            Position, inside wxPropertyGrid, to create control(s) to.

        @param size
            Initial size for control(s).

        @remarks
        - Unlike in previous version of wxPropertyGrid, it is no longer
          necessary to call wxEvtHandler::Connect() for interesting editor
          events. Instead, all events from control are now automatically
          forwarded to wxPGEditor::OnEvent() and wxPGProperty::OnEvent().
    */
    virtual wxPGWindowList CreateControls( wxPropertyGrid* propgrid,
                                           wxPGProperty* property,
                                           const wxPoint& pos,
                                           const wxSize& size ) const = 0;

    /** Loads value from property to the control. */
    virtual void UpdateControl( wxPGProperty* property, wxWindow* ctrl ) const = 0;

    /**
        Draws value for given property.
    */
    virtual void DrawValue( wxDC& dc, const wxRect& rect,
                            wxPGProperty* property, const wxString& text ) const;

    /**
        Handles events. Returns @true if value in control was modified
        (see wxPGProperty::OnEvent() for more information).

        @remarks wxPropertyGrid will automatically unfocus the editor when
                 @c wxEVT_TEXT_ENTER is received and when it results in
                 property value being modified. This happens regardless of
                 editor type (i.e. behaviour is same for any wxTextCtrl and
                 wxComboBox based editor).
    */
    virtual bool OnEvent( wxPropertyGrid* propgrid, wxPGProperty* property,
        wxWindow* wnd_primary, wxEvent& event ) const = 0;

    /**
        Returns value from control, via parameter @a variant.
        Usually ends up calling property's StringToValue() or IntToValue().
        Returns @true if value was different.
    */
    virtual bool GetValueFromControl( wxVariant& variant, wxPGProperty* property,
                                      wxWindow* ctrl ) const;

    /** Sets value in control to unspecified. */
    virtual void SetValueToUnspecified( wxPGProperty* property,
                                        wxWindow* ctrl ) const;

    /**
        Called by property grid to set new appearance for the control.
        Default implementation  sets foreground colour, background colour,
        font, plus text for wxTextCtrl and wxComboCtrl.

        @param pg
            Property grid to which the edited property belongs.

        @param property
            Edited property to which the editor control belongs.

        @param ctrl
            Editor control.

        @param appearance
            New appearance to be applied.

        @param oldAppearance
            Previously applied appearance. Used to detect which control
            attributes need to be changed (e.g. so we only change background
            colour if really needed).

        @param unspecified
            If @true tells this function that the new appearance represents
            an unspecified property value.
    */
    virtual void SetControlAppearance( wxPropertyGrid* pg,
                                       wxPGProperty* property,
                                       wxWindow* ctrl,
                                       const wxPGCell& appearance,
                                       const wxPGCell& oldAppearance,
                                       bool unspecified ) const;

    /** Sets control's value specifically from string. */
    virtual void SetControlStringValue( wxPGProperty* property,
                                        wxWindow* ctrl, const wxString& txt ) const;

    /** Sets control's value specifically from int (applies to choice etc.). */
    virtual void SetControlIntValue( wxPGProperty* property,
                                     wxWindow* ctrl, int value ) const;

    /**
        Inserts item to existing control. Index -1 means end of list.
        Default implementation does nothing. Returns index of item added.
    */
    virtual int InsertItem( wxWindow* ctrl, const wxString& label, int index ) const;

    /**
        Deletes item from existing control.
        Default implementation does nothing.
    */
    virtual void DeleteItem( wxWindow* ctrl, int index ) const;

    /**
        Extra processing when control gains focus.
        For example, wxTextCtrl based controls should select all text.
    */
    virtual void OnFocus( wxPGProperty* property, wxWindow* wnd ) const;

    /**
        Returns @true if control itself can contain the custom image.
        Default implementation returns @false.
    */
    virtual bool CanContainCustomImage() const;

    //
    // This member is public so scripting language bindings
    // wrapper code can access it freely.
    void*       m_clientData;

};




class wxPGTextCtrlEditor : public wxPGEditor
{
public:
    wxPGTextCtrlEditor();
    virtual ~wxPGTextCtrlEditor();

    virtual wxPGWindowList CreateControls(wxPropertyGrid* propgrid,
                                          wxPGProperty* property,
                                          const wxPoint& pos,
                                          const wxSize& size) const;
    virtual void UpdateControl( wxPGProperty* property,
                                wxWindow* ctrl ) const;
    virtual bool OnEvent( wxPropertyGrid* propgrid,
                          wxPGProperty* property,
                          wxWindow* primaryCtrl,
                          wxEvent& event ) const;
    virtual bool GetValueFromControl( wxVariant& variant,
                                      wxPGProperty* property,
                                      wxWindow* ctrl ) const;

    virtual wxString GetName() const;

    virtual void SetControlStringValue( wxPGProperty* property,
                                        wxWindow* ctrl,
                                        const wxString& txt ) const;
    virtual void OnFocus( wxPGProperty* property, wxWindow* wnd ) const;

    static bool OnTextCtrlEvent( wxPropertyGrid* propgrid,
                                 wxPGProperty* property,
                                 wxWindow* ctrl,
                                 wxEvent& event );

    static bool GetTextCtrlValueFromControl( wxVariant& variant,
                                             wxPGProperty* property,
                                             wxWindow* ctrl );
};


class wxPGChoiceEditor : public wxPGEditor
{
public:
    wxPGChoiceEditor()
    virtual ~wxPGChoiceEditor();

    virtual wxPGWindowList CreateControls(wxPropertyGrid* propgrid,
                                          wxPGProperty* property,
                                          const wxPoint& pos,
                                          const wxSize& size) const;
    virtual void UpdateControl( wxPGProperty* property,
                                wxWindow* ctrl ) const;
    virtual bool OnEvent( wxPropertyGrid* propgrid,
                          wxPGProperty* property,
                          wxWindow* primaryCtrl,
                          wxEvent& event ) const;
    virtual bool GetValueFromControl( wxVariant& variant,
                                      wxPGProperty* property,
                                      wxWindow* ctrl ) const;
    virtual void SetValueToUnspecified( wxPGProperty* property,
                                        wxWindow* ctrl ) const;
    virtual wxString GetName() const;

    virtual void SetControlIntValue( wxPGProperty* property,
                                     wxWindow* ctrl,
                                     int value ) const;
    virtual void SetControlStringValue( wxPGProperty* property,
                                        wxWindow* ctrl,
                                        const wxString& txt ) const;

    virtual int InsertItem( wxWindow* ctrl,
                            const wxString& label,
                            int index ) const;
    virtual void DeleteItem( wxWindow* ctrl, int index ) const;
    virtual bool CanContainCustomImage() const;

    wxWindow* CreateControlsBase( wxPropertyGrid* propgrid,
                                  wxPGProperty* property,
                                  const wxPoint& pos,
                                  const wxSize& sz,
                                  long extraStyle ) const;

};


class wxPGComboBoxEditor : public wxPGChoiceEditor
{
public:
    wxPGComboBoxEditor();
    virtual ~wxPGComboBoxEditor();

    virtual wxPGWindowList CreateControls(wxPropertyGrid* propgrid,
                                          wxPGProperty* property,
                                          const wxPoint& pos,
                                          const wxSize& size) const;

    virtual wxString GetName() const;

    virtual void UpdateControl( wxPGProperty* property, wxWindow* ctrl ) const;

    virtual bool OnEvent( wxPropertyGrid* propgrid, wxPGProperty* property,
        wxWindow* ctrl, wxEvent& event ) const;

    virtual bool GetValueFromControl( wxVariant& variant,
                                      wxPGProperty* property,
                                      wxWindow* ctrl ) const;

    virtual void OnFocus( wxPGProperty* property, wxWindow* wnd ) const;

};


class wxPGChoiceAndButtonEditor : public wxPGChoiceEditor
{
public:
    wxPGChoiceAndButtonEditor();
    virtual ~wxPGChoiceAndButtonEditor();
    virtual wxString GetName() const;

    virtual wxPGWindowList CreateControls(wxPropertyGrid* propgrid,
                                          wxPGProperty* property,
                                          const wxPoint& pos,
                                          const wxSize& size) const;
};

class wxPGTextCtrlAndButtonEditor : public wxPGTextCtrlEditor
{
public:
    wxPGTextCtrlAndButtonEditor();
    virtual ~wxPGTextCtrlAndButtonEditor();
    virtual wxString GetName() const;

    virtual wxPGWindowList CreateControls(wxPropertyGrid* propgrid,
                                          wxPGProperty* property,
                                          const wxPoint& pos,
                                          const wxSize& size) const;
};





class wxPGCheckBoxEditor : public wxPGEditor
{
public:
    wxPGCheckBoxEditor();
    virtual ~wxPGCheckBoxEditor();

    virtual wxString GetName() const;
    virtual wxPGWindowList CreateControls(wxPropertyGrid* propgrid,
                                          wxPGProperty* property,
                                          const wxPoint& pos,
                                          const wxSize& size) const;
    virtual void UpdateControl( wxPGProperty* property,
                                wxWindow* ctrl ) const;
    virtual bool OnEvent( wxPropertyGrid* propgrid,
                          wxPGProperty* property,
                          wxWindow* primaryCtrl,
                          wxEvent& event ) const;
    virtual bool GetValueFromControl( wxVariant& variant,
                                      wxPGProperty* property,
                                      wxWindow* ctrl ) const;
    virtual void SetValueToUnspecified( wxPGProperty* property,
                                        wxWindow* ctrl ) const;

    virtual void DrawValue( wxDC& dc,
                            const wxRect& rect,
                            wxPGProperty* property,
                            const wxString& text ) const;

    virtual void SetControlIntValue( wxPGProperty* property,
                                     wxWindow* ctrl,
                                     int value ) const;
};



/**
    @class wxPGEditorDialogAdapter

    Derive a class from this to adapt an existing editor dialog or function to
    be used when editor button of a property is pushed.

    You only need to derive class and implement DoShowDialog() to create and
    show the dialog, and finally submit the value returned by the dialog
    via SetValue().

    @library{wxpropgrid}
    @category{propgrid}
*/
class wxPGEditorDialogAdapter : public wxObject
{
public:
    wxPGEditorDialogAdapter();
    virtual ~wxPGEditorDialogAdapter();

    bool ShowDialog( wxPropertyGrid* propGrid, wxPGProperty* property );

    virtual bool DoShowDialog( wxPropertyGrid* propGrid,
                               wxPGProperty* property ) = 0;

    void SetValue( wxVariant value );

    /**
        This method is typically only used if deriving class from existing
        adapter with value conversion purposes.
    */
    wxVariant& GetValue() { return m_value; }

    //
    // This member is public so scripting language bindings
    // wrapper code can access it freely.
    void*               m_clientData;
};



/**
    @class wxPGMultiButton

    This class can be used to have multiple buttons in a property editor.
    You will need to create a new property editor class, override CreateControls,
    and have it return wxPGMultiButton instance in wxPGWindowList::SetSecondary().

    For instance, here we add three buttons to a TextCtrl editor:

    @code
    #include <wx/propgrid/editors.h>

    class wxSampleMultiButtonEditor : public wxPGTextCtrlEditor
    {
        wxDECLARE_DYNAMIC_CLASS(wxSampleMultiButtonEditor);

    public:
        wxSampleMultiButtonEditor() {}
        virtual ~wxSampleMultiButtonEditor() {}

        virtual wxString GetName() const { return "SampleMultiButtonEditor"; }

        virtual wxPGWindowList CreateControls( wxPropertyGrid* propGrid,
                                               wxPGProperty* property,
                                               const wxPoint& pos,
                                               const wxSize& sz ) const;
        virtual bool OnEvent( wxPropertyGrid* propGrid,
                              wxPGProperty* property,
                              wxWindow* ctrl,
                              wxEvent& event ) const;
    };

    wxIMPLEMENT_DYNAMIC_CLASS(wxSampleMultiButtonEditor, wxPGTextCtrlEditor);

    wxPGWindowList wxSampleMultiButtonEditor::CreateControls( wxPropertyGrid* propGrid,
                                                              wxPGProperty* property,
                                                              const wxPoint& pos,
                                                              const wxSize& sz ) const
    {
        // Create and populate buttons-subwindow
        wxPGMultiButton* buttons = new wxPGMultiButton( propGrid, sz );

        // Add two regular buttons
        buttons->Add( "..." );
        buttons->Add( "A" );
        // Add a bitmap button
        buttons->Add( wxArtProvider::GetBitmap(wxART_FOLDER) );

        // Create the 'primary' editor control (textctrl in this case)
        wxPGWindowList wndList = wxPGTextCtrlEditor::CreateControls
                                 ( propGrid, property, pos,
                                   buttons->GetPrimarySize() );

        // Finally, move buttons-subwindow to correct position and make sure
        // returned wxPGWindowList contains our custom button list.
        buttons->Finalize(propGrid, pos);

        wndList.SetSecondary( buttons );
        return wndList;
    }

    bool wxSampleMultiButtonEditor::OnEvent( wxPropertyGrid* propGrid,
                                             wxPGProperty* property,
                                             wxWindow* ctrl,
                                             wxEvent& event ) const
    {
        if ( event.GetEventType() == wxEVT_BUTTON )
        {
            wxPGMultiButton* buttons = (wxPGMultiButton*) propGrid->GetEditorControlSecondary();

            if ( event.GetId() == buttons->GetButtonId(0) )
            {
                // Do something when the first button is pressed
                // Return true if the action modified the value in editor.
                ...
            }
            if ( event.GetId() == buttons->GetButtonId(1) )
            {
                // Do something when the second button is pressed
                ...
            }
            if ( event.GetId() == buttons->GetButtonId(2) )
            {
                // Do something when the third button is pressed
                ...
            }
        }
        return wxPGTextCtrlEditor::OnEvent(propGrid, property, ctrl, event);
    }
    @endcode

    Further to use this editor, code like this can be used:

    @code
        // Register editor class - needs only to be called once
        wxPGEditor* multiButtonEditor = new wxSampleMultiButtonEditor();
        wxPropertyGrid::RegisterEditorClass( multiButtonEditor );

        // Insert the property that will have multiple buttons
        propGrid->Append( new wxLongStringProperty("MultipleButtons", wxPG_LABEL) );

        // Change property to use editor created in the previous code segment
        propGrid->SetPropertyEditor( "MultipleButtons", multiButtonEditor );
    @endcode

    @library{wxpropgrid}
    @category{propgrid}
*/
class WXDLLIMPEXP_PROPGRID wxPGMultiButton : public wxWindow
{
public:
    /**
        Constructor.
    */
    wxPGMultiButton( wxPropertyGrid* pg, const wxSize& sz );

    /**
        Destructor.
    */
    virtual ~wxPGMultiButton() { }

    /**
        Adds new button, with given label.
    */
    void Add( const wxString& label, int id = -2 );

    /**
        Adds new bitmap button.
    */
    void Add( const wxBitmap& bitmap, int id = -2 );

    /**
        Call this in CreateControls() of your custom editor class
        after all buttons have been added.

        @param propGrid
            wxPropertyGrid given in CreateControls().

        @param pos
            wxPoint given in CreateControls().
    */
    void Finalize( wxPropertyGrid* propGrid, const wxPoint& pos );

    /**
        Returns pointer to one of the buttons.
    */
    wxWindow* GetButton( unsigned int i );

    /**
        Returns Id of one of the buttons.
        This is utility function to be used in event handlers.
    */
    int GetButtonId( unsigned int i ) const;

    /**
        Returns number of buttons.
    */
    unsigned int GetCount();

    /**
        Returns size of primary editor control, as appropriately
        reduced by number of buttons present.
    */
    wxSize GetPrimarySize() const;
};

extern wxPGEditor* wxPGEditor_TextCtrl;
extern wxPGEditor* wxPGEditor_Choice;
extern wxPGEditor* wxPGEditor_ComboBox;
extern wxPGEditor* wxPGEditor_TextCtrlAndButton;
extern wxPGEditor* wxPGEditor_CheckBox;
extern wxPGEditor* wxPGEditor_ChoiceAndButton;
