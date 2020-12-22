/////////////////////////////////////////////////////////////////////////////
// Name:        propgridiface.h
// Purpose:     interface of wxPGProperty
// Author:      wxWidgets team
// Licence:     wxWindows licence
/////////////////////////////////////////////////////////////////////////////

// -----------------------------------------------------------------------
/**
    @section propgrid_property_values_attributes wxPropertyGrid Property Values Attribute Identifiers

    Many wxPropertyGridInterface and wxPropertyGrid methods to set property
    value or to modify its state use these flags to specify additional details
    of the operation.

    @{
*/
enum wxPG_PROPERTYVALUES_FLAGS
{
/**
    Flag for wxPropertyGridInterface::SetProperty* functions,
    wxPropertyGridInterface::HideProperty(), etc.
    Apply changes only for the property in question.
    @hideinitializer
*/
wxPG_DONT_RECURSE                 = 0x00000000,

/**
    Flag for wxPropertyGridInterface::GetPropertyValues().
    Use this flag to retain category structure; each sub-category
    will be its own wxVariantList of wxVariant.
    @hideinitializer
*/
wxPG_KEEP_STRUCTURE               = 0x00000010,

/**
    Flag for wxPropertyGridInterface::SetProperty* functions,
    wxPropertyGridInterface::HideProperty(), etc.
    Apply changes recursively for the property and all its children.
    @hideinitializer
*/
wxPG_RECURSE                      = 0x00000020,

/**
    Flag for wxPropertyGridInterface::GetPropertyValues().
    Use this flag to include property attributes as well.
    @hideinitializer
*/
wxPG_INC_ATTRIBUTES               = 0x00000040,

/**
    Used when first starting recursion.
    @hideinitializer
*/
wxPG_RECURSE_STARTS               = 0x00000080,

/**
    Force value change.
    @hideinitializer
*/
wxPG_FORCE                        = 0x00000100,

/**
    Only sort categories and their immediate children.
    Sorting done by wxPG_AUTO_SORT option uses this.
    @hideinitializer
*/
wxPG_SORT_TOP_LEVEL_ONLY          = 0x00000200
};

/** @}
*/

/** @section propgrid_misc_data wxPropertyGrid Miscellaneous

    This section describes some miscellaneous values, types and macros.
    @{
*/

/**
    Used to tell wxPGProperty to use label as name as well.
*/
#define wxPG_LABEL              (*wxPGProperty::sm_wxPG_LABEL)

/**
    This is the value placed in wxPGProperty::sm_wxPG_LABEL
*/
#define wxPG_LABEL_STRING       wxS("@!")

#define wxPG_COLOUR_BLACK       (*wxBLACK)

/**
    Convert Red, Green and Blue to a single 32-bit value.
*/
#define wxPG_COLOUR(R,G,B) ((wxUint32)(R+(G<<8)+(B<<16)))

/**
    If property is supposed to have custom-painted image, then returning
    this in OnMeasureImage() will usually be enough.
*/
#define wxPG_DEFAULT_IMAGE_SIZE  wxDefaultSize

/** @}
*/

/**
    @class wxPropertyGridInterface

    Most of the shared property manipulation interface shared by wxPropertyGrid,
    wxPropertyGridPage, and wxPropertyGridManager is defined in this class.

    @remarks
    - In separate wxPropertyGrid component this class was known as
      wxPropertyContainerMethods.

    - wxPropertyGridInterface's property operation member functions all accept
      a special wxPGPropArg id argument, using which you can refer to properties
      either by their pointer (for performance) or by their name (for conveniency).

    @library{wxpropgrid}
    @category{propgrid}
*/
class wxPropertyGridInterface
{
public:

    /** Destructor. */
    virtual ~wxPropertyGridInterface();

    /**
        Appends property to the list. wxPropertyGrid assumes ownership of the
        object. Becomes child of most recently added category.

        @remarks
        - wxPropertyGrid takes the ownership of the property pointer.
        - If appending a category with name identical to a category already in
          the wxPropertyGrid, then newly created category is deleted, and most
          recently added category (under which properties are appended) is set
          to the one with same name. This allows easier adding of items to same
          categories in multiple passes.
        - Does not automatically redraw the control, so you may need to call
          Refresh() when calling this function after control has been shown for
          the first time.
        - This functions deselects selected property, if any. Validation
          failure option wxPG_VFB_STAY_IN_PROPERTY is not respected, i.e.
          selection is cleared even if editor had invalid value.
    */
    wxPGProperty* Append( wxPGProperty* property );

    /**
        Same as Append(), but appends under given parent property.

        @param id
            Name or pointer to parent property.

        @param newProperty
            Property to be added.
    */
    wxPGProperty* AppendIn( wxPGPropArg id, wxPGProperty* newProperty );

    /**
        In order to add new items into a property with private children (for
        instance, wxFlagsProperty), you need to call this method.
        After populating has been finished, you need to call EndAddChildren().

        @see EndAddChildren()
    */
    void BeginAddChildren( wxPGPropArg id );

    /**
        Deletes all properties.

        @remarks This functions deselects selected property, if any. Validation
                failure option wxPG_VFB_STAY_IN_PROPERTY is not respected, i.e.
                selection is cleared even if editor had invalid value.
    */
    virtual void Clear() = 0;

    /**
        Clears current selection, if any.

        @param validation
            If set to @false, deselecting the property will always work,
            even if its editor had invalid value in it.

        @return Returns @true if successful or if there was no selection. May
               fail if validation was enabled and active editor had invalid
               value.

        @remarks In wxWidgets 2.9 and later, this function no longer
        sends @c wxPG_EVT_SELECTED.

        @see wxPropertyGrid::SelectProperty()
    */
    bool ClearSelection( bool validation = false);

    /**
        Resets modified status of all properties.
    */
    void ClearModifiedStatus();

    /**
        Collapses given category or property with children.

        @return Returns @true if actually collapsed.

        @remarks This function may deselect selected property, if any. Validation
                failure option wxPG_VFB_STAY_IN_PROPERTY is not respected, i.e.
                selection is cleared even if editor had invalid value.
    */
    bool Collapse( wxPGPropArg id );

    /**
        Collapses all items that can be collapsed.

        @return
        Return @false if failed (may fail if editor value cannot be validated).

        @remarks This functions clears selection. Validation failure option
                wxPG_VFB_STAY_IN_PROPERTY is not respected, i.e. selection
                is cleared even if editor had invalid value.
    */
    bool CollapseAll();

    /**
        Changes value of a property, as if by user. Use this instead of
        SetPropertyValue() if you need the value to run through validation
        process, and also send @c wxEVT_PG_CHANGED.

        @remarks Since this function sends @c wxEVT_PG_CHANGED, it should not
        be called from @c EVT_PG_CHANGED handler.

        @return Returns @true if value was successfully changed.
    */
    bool ChangePropertyValue( wxPGPropArg id, wxVariant newValue );

    /**
        Removes and deletes a property and any children.

        @param id
            Pointer or name of a property.

        @remarks If you delete a property in a wxPropertyGrid event
                 handler, the actual deletion is postponed until the next
                 idle event.

                 This functions deselects selected property, if any.
                 Validation failure option wxPG_VFB_STAY_IN_PROPERTY is not
                 respected, i.e. selection is cleared even if editor had
                 invalid value.
    */
    void DeleteProperty( wxPGPropArg id );

    /**
        Disables a property.

        @remarks
        Property is refreshed with new settings.

        @see EnableProperty(), wxPGProperty::Enable()
    */
    bool DisableProperty( wxPGPropArg id );

    /**
        Returns @true if all property grid data changes have been committed. Usually
        only returns @false if value in active editor has been invalidated by a
        wxValidator.
    */
    bool EditorValidate();

    /**
        Enables or disables property. Disabled property usually appears as
        having grey text.

        @param id
            Name or pointer to a property.
        @param enable
            If @false, property is disabled instead.

        @remarks
        Property is refreshed with new settings.

        @see wxPGProperty::Enable()
    */
    bool EnableProperty( wxPGPropArg id, bool enable = true );

    /**
        Called after population of property with fixed children has finished.

        @see BeginAddChildren()
    */
    void EndAddChildren( wxPGPropArg id );

    /**
        Expands given category or property with children.

        @return Returns @true if actually expanded.

        @remarks This function may deselect selected property, if any. Validation
                failure option wxPG_VFB_STAY_IN_PROPERTY is not respected, i.e.
                selection is cleared even if editor had invalid value.
    */
    bool Expand( wxPGPropArg id );

    /**
        Expands all items that can be expanded.

        @remarks This functions clears selection. Validation failure option
                wxPG_VFB_STAY_IN_PROPERTY is not respected, i.e. selection
                is cleared even if editor had invalid value.
    */
    bool ExpandAll( bool expand = true );

    /**
        Returns auto-resize proportion of the given column.

        @see SetColumnProportion()
    */
    int GetColumnProportion( unsigned int column ) const;

    /**
        Returns id of first child of given property.

        @remarks Does not return private children!
    */
    wxPGProperty* GetFirstChild( wxPGPropArg id );

    //@{
    /**
        Returns iterator class instance.

        @param flags
            See @ref propgrid_iterator_flags. Value wxPG_ITERATE_DEFAULT causes
            iteration over everything except private child properties.
        @param firstProp
            Property to start iteration from. If @NULL, then first child of root
            is used.

    */
    wxPropertyGridIterator GetIterator( int flags = wxPG_ITERATE_DEFAULT,
                                        wxPGProperty* firstProp = NULL );
    wxPropertyGridConstIterator GetIterator( int flags = wxPG_ITERATE_DEFAULT,
                                             wxPGProperty* firstProp = NULL ) const;
    //@}

    //@{
    /**
        Returns iterator class instance.

        @param flags
            See @ref propgrid_iterator_flags. Value wxPG_ITERATE_DEFAULT causes
            iteration over everything except private child properties.
        @param startPos
            Either wxTOP or wxBOTTOM. wxTOP will indicate that iterations start
            from the first property from the top, and wxBOTTOM means that the
            iteration will instead begin from bottommost valid item.

    */
    wxPropertyGridIterator GetIterator( int flags, int startPos );
    wxPropertyGridConstIterator GetIterator( int flags, int startPos ) const;
    //@}

    //@{
    /**
        Returns id of first item that matches given criteria.

        @param flags
            See @ref propgrid_iterator_flags.
    */
    wxPGProperty* GetFirst( int flags = wxPG_ITERATE_ALL );

    const wxPGProperty* GetFirst( int flags = wxPG_ITERATE_ALL ) const;
    //@}

    /**
        Returns pointer to a property with given name (case-sensitive).
        If there is no property with such name, @NULL pointer is returned.

        @remarks Properties which have non-category, non-root parent
                 cannot be accessed globally by their name. Instead, use
                 @"@<property@>.@<subproperty@>@" instead of @"@<subproperty@>@".
    */
    wxPGProperty* GetProperty( const wxString& name ) const;

    /**
        Adds to @a targetArr pointers to properties that have given @a flags set.
        However, if @a inverse is set to @true, then only properties without
        given flags are stored.

        @param targetArr
            Array of pointers to properties in which found properties are
            stored.
        @param flags
            Property flags to use.
        @param inverse
            If @false, properties that have given flags are stored, otherwise
            there are stored only properties without given flags.
        @param iterFlags
            Iterator flags to use. Default is everything expect private children.
            See @ref propgrid_iterator_flags.
    */
    void GetPropertiesWithFlag( wxArrayPGProperty* targetArr,
                                wxPGProperty::FlagType flags,
                                bool inverse = false,
                                int iterFlags = (wxPG_ITERATE_PROPERTIES|wxPG_ITERATE_HIDDEN|wxPG_ITERATE_CATEGORIES) ) const;

    /**
        Returns value of given attribute. If none found, returns wxNullVariant.
    */
    wxVariant GetPropertyAttribute( wxPGPropArg id, const wxString& attrName ) const;

    /**
        Returns map-like storage of property's attributes.

        @remarks
        Note that if extra style ::wxPG_EX_WRITEONLY_BUILTIN_ATTRIBUTES is set,
        then builtin-attributes are not included in the storage.
    */
    const wxPGAttributeStorage& GetPropertyAttributes( wxPGPropArg id ) const;

    /**
        Returns background colour of first cell of a property.
    */
    wxColour GetPropertyBackgroundColour( wxPGPropArg id ) const;

    /**
        Returns pointer of property's nearest parent category. If no category
        found, returns @NULL.
    */
    wxPropertyCategory* GetPropertyCategory( wxPGPropArg id ) const;

    /** Returns client data (void*) of a property. */
    void* GetPropertyClientData( wxPGPropArg id ) const;

    /**
        Returns first property which label matches given string. @NULL if none
        found. Note that this operation is very slow when compared to
        GetPropertyByName().
    */
    wxPGProperty* GetPropertyByLabel( const wxString& label ) const;

    /**
        Returns pointer to a property with given name (case-sensitive).
        If there is no property with such name, @NULL pointer is returned.

        @remarks Properties which have non-category, non-root parent
                 cannot be accessed globally by their name. Instead, use
                 "<property>.<subproperty>" instead of "<subproperty>".
    */
    wxPGProperty* GetPropertyByName( const wxString& name ) const;

    /**
        Returns child property @a subname of property @a name. Same as
        calling GetPropertyByName(@"name.subname@"), albeit slightly faster.
    */
    wxPGProperty* GetPropertyByName( const wxString& name,
                                     const wxString& subname ) const;

    /**
        Returns property's editor.
    */
    const wxPGEditor* GetPropertyEditor( wxPGPropArg id ) const;

    /**
        Returns help string associated with a property.
    */
    wxString GetPropertyHelpString( wxPGPropArg id ) const;

    /**
        Returns property's custom value image (@NULL of none).
    */
    wxBitmap* GetPropertyImage( wxPGPropArg id ) const;

    /**
        Returns label of a property.
    */
    const wxString& GetPropertyLabel( wxPGPropArg id );

    /**
        Returns property's name, by which it is globally accessible.
    */
    wxString GetPropertyName( wxPGProperty* property );

    /**
        Returns parent item of a property.
    */
    wxPGProperty* GetPropertyParent( wxPGPropArg id );

    /**
        Returns text colour of first cell of a property.
    */
    wxColour GetPropertyTextColour( wxPGPropArg id ) const;

    /**
        Returns validator of a property as a reference, which you
        can pass to any number of SetPropertyValidator.
    */
    wxValidator* GetPropertyValidator( wxPGPropArg id );

    /**
        Returns property's value as wxVariant.

        If property value is unspecified, wxNullVariant is returned.
    */
    wxVariant GetPropertyValue( wxPGPropArg id );

    /** Return's property's value as wxArrayInt. */
    wxArrayInt GetPropertyValueAsArrayInt( wxPGPropArg id ) const;

    /** Returns property's value as wxArrayString. */
    wxArrayString GetPropertyValueAsArrayString( wxPGPropArg id ) const;

    /** Returns property's value as bool */
    bool GetPropertyValueAsBool( wxPGPropArg id ) const;

    /** Return's property's value as wxDateTime. */
    wxDateTime GetPropertyValueAsDateTime( wxPGPropArg id ) const;

    /** Returns property's value as double-precision floating point number. */
    double GetPropertyValueAsDouble( wxPGPropArg id ) const;

    /** Returns property's value as integer */
    int GetPropertyValueAsInt( wxPGPropArg id ) const;

    /** Returns property's value as integer */
    long GetPropertyValueAsLong( wxPGPropArg id ) const;

    /** Returns property's value as native signed 64-bit integer. */
    wxLongLong_t GetPropertyValueAsLongLong( wxPGPropArg id ) const;

    /**
        Returns property's value as wxString. If property does not
        use string value type, then its value is converted using
        wxPGProperty::GetValueAsString().
    */
    wxString GetPropertyValueAsString( wxPGPropArg id ) const;

    /** Returns property's value as unsigned integer */
    unsigned long GetPropertyValueAsULong( wxPGPropArg id ) const;

    /** Returns property's value as native unsigned 64-bit integer. */
    wxULongLong_t GetPropertyValueAsULongLong( wxPGPropArg id ) const;

    /**
        Returns a wxVariant list containing wxVariant versions of all
        property values. Order is not guaranteed.

        @param listname
            Name of the returned wxVariant list.
        @param baseparent
            The base property which children properties will be queried for
            values.
        @param flags
            Use @c wxPG_KEEP_STRUCTURE to retain category structure; each sub
            category will be its own wxVariantList of wxVariant.
            Use @c wxPG_INC_ATTRIBUTES to include property attributes as well.
            Each attribute will be stored as list variant named
            @"@@@<propname@>@@attr.@"
    */
    wxVariant GetPropertyValues( const wxString& listname = wxEmptyString,
                                 wxPGProperty* baseparent = NULL, long flags = 0 ) const;

    /**
        Returns list of currently selected properties.

        @remarks wxArrayPGProperty should be compatible with std::vector API.
    */
    const wxArrayPGProperty& GetSelectedProperties() const;

    /**
        Returns currently selected property. @NULL if none.

        @remarks When wxPG_EX_MULTIPLE_SELECTION extra style is used, this
                 member function returns the focused property, that is the
                 one which can have active editor.
    */
    wxPGProperty* GetSelection() const;

    /**
        Similar to GetIterator(), but instead returns wxPGVIterator instance,
        which can be useful for forward-iterating through arbitrary property
        containers.

        @param flags
            See @ref propgrid_iterator_flags.
    */
    virtual wxPGVIterator GetVIterator( int flags ) const;

    /**
        Hides or reveals a property.

        @param id
            Name or pointer to a property.
        @param hide
            If @true, hides property, otherwise reveals it.
        @param flags
            By default changes are applied recursively. Set this parameter
            wxPG_DONT_RECURSE to prevent this.
    */
    bool HideProperty( wxPGPropArg id, bool hide = true, int flags = wxPG_RECURSE );

    /**
        Initializes *all* property types. Causes references to most object
        files in the library, so calling this may cause significant increase
        in executable size when linking with static library.
    */
    static void InitAllTypeHandlers();

    /**
        Inserts property to the property container.

        @param priorThis
            New property is inserted just prior to this. Available only
            in the first variant. There are two versions of this function
            to allow this parameter to be either an id or name to
            a property.
        @param newProperty
            Pointer to the inserted property. wxPropertyGrid will take
            ownership of this object.

        @return Returns newProperty.

        @remarks

        - wxPropertyGrid takes the ownership of the property pointer.

        - While Append may be faster way to add items, make note that when
          both types of data storage (categoric and
          non-categoric) are active, Insert becomes even more slow. This is
          especially true if current mode is non-categoric.

        - This functions deselects selected property, if any. Validation
          failure option wxPG_VFB_STAY_IN_PROPERTY is not respected, i.e.
          selection is cleared even if editor had invalid value.

        Example of use:

        @code

            // append category
            wxPGProperty* my_cat_id = propertygrid->Append( new wxPropertyCategory("My Category") );

            ...

            // insert into category - using second variant
            wxPGProperty* my_item_id_1 = propertygrid->Insert( my_cat_id, 0, new wxStringProperty("My String 1") );

            // insert before to first item - using first variant
            wxPGProperty* my_item_id_2 = propertygrid->Insert( my_item_id, new wxStringProperty("My String 2") );

        @endcode
    */
    wxPGProperty* Insert( wxPGPropArg priorThis, wxPGProperty* newProperty );

    /**
        Inserts property to the property container.
        See the other overload for more details.

        @param parent
            New property is inserted under this category. Available only
            in the second variant. There are two versions of this function
            to allow this parameter to be either an id or name to
            a property.
        @param index
            Index under category. Available only in the second variant.
            If index is < 0, property is appended in category.
        @param newProperty
            Pointer to the inserted property. wxPropertyGrid will take
            ownership of this object.

        @return Returns newProperty.
    */
    wxPGProperty* Insert( wxPGPropArg parent, int index, wxPGProperty* newProperty );

    /** Returns @true if property is a category. */
    bool IsPropertyCategory( wxPGPropArg id ) const;

    /** Returns @true if property is enabled. */
    bool IsPropertyEnabled( wxPGPropArg id ) const;

    /**
        Returns @true if given property is expanded. Naturally, always returns
        @false for properties that cannot be expanded.
    */
    bool IsPropertyExpanded( wxPGPropArg id ) const;

    /**
        Returns @true if property has been modified after value set or modify
        flag clear by software.
    */
    bool IsPropertyModified( wxPGPropArg id ) const;

    /**
        Returns @true if property is selected.
    */
    bool IsPropertySelected( wxPGPropArg id ) const;

    /**
        Returns @true if property is shown (i.e. HideProperty() with @true not
        called for it).
    */
    bool IsPropertyShown( wxPGPropArg id ) const;

    /**
        Returns @true if property value is set to unspecified.
    */
    bool IsPropertyValueUnspecified( wxPGPropArg id ) const;

    /**
        Disables (@a limit = @true) or enables (@a limit = @false) wxTextCtrl
        editor of a property, if it is not the sole mean to edit the value.

        @remarks
        Property is refreshed with new settings.
    */
    void LimitPropertyEditing( wxPGPropArg id, bool limit = true );

    /**
        If state is shown in its grid, refresh it now.
    */
    virtual void RefreshGrid( wxPropertyGridPageState* state = NULL );

    /**
        Initializes additional property editors (SpinCtrl etc.). Causes
        references to most object files in the library, so calling this may
        cause significant increase in executable size when linking with static
        library.
    */
    static void RegisterAdditionalEditors();

    /**
        Removes a property. Does not delete the property object, but
        instead returns it.

        @param id
            Pointer or name of a property.

        @remarks Removed property cannot have any children.

                 Also, if you remove property in a wxPropertyGrid event
                 handler, the actual removal is postponed until the next
                 idle event.
    */
    wxPGProperty* RemoveProperty( wxPGPropArg id );

    /**
        Replaces property with id with newly created one. For example,
        this code replaces existing property named "Flags" with one that
        will have different set of items:

        @code
            pg->ReplaceProperty("Flags",
                wxFlagsProperty("Flags", wxPG_LABEL, newItems))
        @endcode

        @see Insert()
    */
    wxPGProperty* ReplaceProperty( wxPGPropArg id, wxPGProperty* property );

    /**
        @anchor propgridinterface_editablestate_flags

        Flags for wxPropertyGridInterface::SaveEditableState() and
        wxPropertyGridInterface::RestoreEditableState().
    */
    enum EditableStateFlags
    {
        /**
            Include selected property.
            @hideinitializer
        */
        SelectionState   = 0x01,
        /**
            Include expanded/collapsed property information.
            @hideinitializer
        */
        ExpandedState    = 0x02,
        /**
            Include scrolled position.
            @hideinitializer
        */
        ScrollPosState   = 0x04,
        /**
            Include selected page information. Only applies to
            wxPropertyGridManager.
            @hideinitializer
        */
        PageState        = 0x08,
        /**
            Include splitter position. Stored for each page.
            @hideinitializer
        */
        SplitterPosState = 0x10,
        /**
            Include description box size.
            Only applies to wxPropertyGridManager.
            @hideinitializer
        */
        DescBoxState     = 0x20,

        /**
            Include all supported user editable state information.
            This is usually the default value.
            @hideinitializer
        */
        AllStates        = SelectionState |
                           ExpandedState |
                           ScrollPosState |
                           PageState |
                           SplitterPosState |
                           DescBoxState
    };

    /**
        Restores user-editable state. See also wxPropertyGridInterface::SaveEditableState().

        @param src
        String generated by SaveEditableState.

        @param restoreStates
        Which parts to restore from source string. See @ref propgridinterface_editablestate_flags
        "list of editable state flags".

        @return Returns @false if there was problem reading the string.

        @remarks If some parts of state (such as scrolled or splitter position)
                fail to restore correctly, please make sure that you call this
                function after wxPropertyGrid size has been set (this may
                sometimes be tricky when sizers are used).
    */
    bool RestoreEditableState( const wxString& src,
                               int restoreStates = AllStates );

    /**
        Used to acquire user-editable state (selected property, expanded
        properties, scrolled position, splitter positions).

        @param includedStates
        Which parts of state to include. See @ref propgridinterface_editablestate_flags
        "list of editable state flags".
    */
    wxString SaveEditableState( int includedStates = AllStates ) const;

    /**
        Sets strings listed in the choice dropdown of a wxBoolProperty.

        Defaults are "True" and "False", so changing them to, say, "Yes" and
        "No" may be useful in some less technical applications.
    */
    static void SetBoolChoices( const wxString& trueChoice,
                                const wxString& falseChoice );

    /**
        Set proportion of a auto-stretchable column. wxPG_SPLITTER_AUTO_CENTER
        window style needs to be used to indicate that columns are auto-
        resizable.

        @returns Returns @false on failure.

        @remarks You should call this for individual pages of
                 wxPropertyGridManager (if used).

        @see GetColumnProportion()
    */
    bool SetColumnProportion( unsigned int column, int proportion );

    /**
        Sets an attribute for this property.

        @param id
            Name or pointer to a property.
        @param attrName
            Text identifier of attribute. See @ref propgrid_property_attributes.
        @param value
            Value of attribute.
        @param argFlags
            Optional.
            Use wxPG_RECURSE to set the attribute to child properties recursively.

        @remarks
        - Setting attribute's value to wxNullVariant will simply remove it
        from property's set of attributes.
        - Property is refreshed with new settings.
    */
    void SetPropertyAttribute( wxPGPropArg id, const wxString& attrName,
                               wxVariant value, long argFlags = 0 );

    /**
        Sets property attribute for all applicable properties.
        Be sure to use this method only after all properties have been
        added to the grid.

        @remarks
        Properties are refreshed with new settings.
    */
    void SetPropertyAttributeAll( const wxString& attrName, wxVariant value );

    /**
        Sets background colour of given property.

        @param id
            Property name or pointer.

        @param colour
            New background colour.

        @param flags
            Default is wxPG_RECURSE which causes colour to be set recursively.
            Omit this flag to only set colour for the property in question
            and not any of its children.

        @remarks
        - If category is tried to set recursively, only its children are affected.
        - Property is redrawn with new colour.
    */
    void SetPropertyBackgroundColour( wxPGPropArg id,
                                      const wxColour& colour,
                                      int flags = wxPG_RECURSE );

    /**
        Sets text, bitmap, and colours for given column's cell.

        @remarks
        - You can set label cell by using column 0.
        - You can use wxPG_LABEL as text to use default text for column.
    */
    void SetPropertyCell( wxPGPropArg id,
                          int column,
                          const wxString& text = wxEmptyString,
                          const wxBitmap& bitmap = wxNullBitmap,
                          const wxColour& fgCol = wxNullColour,
                          const wxColour& bgCol = wxNullColour );

    /**
        Sets client data (void*) of a property.

        @remarks
        This untyped client data has to be deleted manually.
    */
    void SetPropertyClientData( wxPGPropArg id, void* clientData );

    /**
        Resets text and background colours of given property.

        @param id
            Property name or pointer.

        @param flags
            Default is wxPG_DONT_RECURSE which causes colour to be reset
            only for the property in question (for backward compatibility).

        @remarks
        - If category is tried to set recursively, only its children are affected.
        - Property is redrawn with new colours.
    */
    void SetPropertyColoursToDefault(wxPGPropArg id, int flags = wxPG_DONT_RECURSE);

    /**
        Sets editor for a property.

        @param id
            Property name or pointer to a property.
        @param editor
            For builtin editors, use wxPGEditor_X, where X is builtin editor's
            name (TextCtrl, Choice, etc. see wxPGEditor documentation for full
            list).

        For custom editors, use pointer you received from wxPropertyGrid::RegisterEditorClass().
    */
    void SetPropertyEditor( wxPGPropArg id, const wxPGEditor* editor );

    /**
        Sets editor control of a property. As editor argument, use
        editor name string, such as "TextCtrl" or "Choice".
    */
    void SetPropertyEditor( wxPGPropArg id, const wxString& editorName );

    /**
        Sets label of a property.

        @remarks
        - Properties under same parent may have same labels. However,
        property names must still remain unique.
    */
    void SetPropertyLabel( wxPGPropArg id, const wxString& newproplabel );

    /**
        Sets name of a property.

        @param id
            Name or pointer of property which name to change.

        @param newName
            New name for property.
    */
    void SetPropertyName( wxPGPropArg id, const wxString& newName );

    /**
        Sets property (and, recursively, its children) to have read-only value.
        In other words, user cannot change the value in the editor, but they can
        still copy it.

        @param id
            Property name or pointer.

        @param set
            Use @true to enable read-only, @false to disable it.

        @param flags
            By default changes are applied recursively. Set this parameter
            to wxPG_DONT_RECURSE to prevent this.

        @remarks
        - This is mainly for use with textctrl editor. Only some other
        editors fully support it.
        - Property is refreshed with new settings.
    */
    void SetPropertyReadOnly( wxPGPropArg id, bool set = true,
                              int flags = wxPG_RECURSE );

    /**
        Sets property's value to unspecified. If it has children (it may be
        category), then the same thing is done to them.
    */
    void SetPropertyValueUnspecified( wxPGPropArg id );

    /**
        Sets property values from a list of wxVariants.
    */
    void SetPropertyValues( const wxVariantList& list,
                            wxPGPropArg defaultCategory = wxNullProperty );

    /**
        Sets property values from a list of wxVariants.
    */
    void SetPropertyValues( const wxVariant& list,
                            wxPGPropArg defaultCategory = wxNullProperty );

    /**
        Associates the help string with property.

        @remarks By default, text is shown either in the manager's @"description@"
                text box or in the status bar. If extra window style
                wxPG_EX_HELP_AS_TOOLTIPS is used, then the text will appear as
                a tooltip.
    */
    void SetPropertyHelpString( wxPGPropArg id, const wxString& helpString );

    /**
        Set wxBitmap in front of the value.

        @remarks Bitmap will be scaled to a size returned by
                wxPropertyGrid::GetImageSize();
    */
    void SetPropertyImage( wxPGPropArg id, wxBitmap& bmp );

    /**
        Sets maximum length of text in property text editor.

        @param id
            Property name or pointer.
        @param maxLen
            Maximum number of characters of the text the user can enter in
            the text editor. If it is 0, the length is not limited and the text
            can be as long as it is supported by the the underlying native text
            control widget.
        @return
            Returns @true if maximum length was set.
        @see
            wxPGProperty::SetMaxLength.
    */
    bool SetPropertyMaxLength( wxPGPropArg id, int maxLen );


    /**
        Sets text colour of given property.

        @param id
            Property name or pointer.

        @param colour
            New text colour.

        @param flags
            Default is wxPG_RECURSE which causes colour to be set recursively.
            Omit this flag to only set colour for the property in question
            and not any of its children.

        @remarks
        - If category is tried to set recursively, only its children are affected.
        - Property is redrawn with new colour.
    */
    void SetPropertyTextColour( wxPGPropArg id,
                                const wxColour& colour,
                                int flags = wxPG_RECURSE );

    /**
        Sets validator of a property.
    */
    void SetPropertyValidator( wxPGPropArg id, const wxValidator& validator );

    /** Sets value (long integer) of a property. */
    void SetPropertyValue( wxPGPropArg id, long value );

    /** Sets value (integer) of a property. */
    void SetPropertyValue( wxPGPropArg id, int value );

    /** Sets value (floating point) of a property. */
    void SetPropertyValue( wxPGPropArg id, double value );

    /** Sets value (bool) of a property. */
    void SetPropertyValue( wxPGPropArg id, bool value );

    /** Sets value (wchar_t*) of a property. */
    void SetPropertyValue( wxPGPropArg id, const wchar_t* value );

    /** Sets value (char*) of a property. */
    void SetPropertyValue( wxPGPropArg id, const char* value );

    /** Sets value (string) of a property. */
    void SetPropertyValue( wxPGPropArg id, const wxString& value );

    /** Sets value (wxArrayString) of a property. */
    void SetPropertyValue( wxPGPropArg id, const wxArrayString& value );

    /** Sets value (wxDateTime) of a property. */
    void SetPropertyValue( wxPGPropArg id, const wxDateTime& value );

    /** Sets value (wxObject*) of a property. */
    void SetPropertyValue( wxPGPropArg id, wxObject* value );

    /** Sets value (wxObject&) of a property. */
    void SetPropertyValue( wxPGPropArg id, wxObject& value );

    /** Sets value (native 64-bit int) of a property. */
    void SetPropertyValue( wxPGPropArg id, wxLongLong_t value );

    /** Sets value (wxLongLong) of a property. */
    void SetPropertyValue( wxPGPropArg id, wxLongLong value );

    /** Sets value (native 64-bit unsigned int) of a property. */
    void SetPropertyValue( wxPGPropArg id, wxULongLong_t value );

    /** Sets value (wxULongLong) of a property. */
    void SetPropertyValue( wxPGPropArg id, wxULongLong value );

    /** Sets value (wxArrayInt&) of a property. */
    void SetPropertyValue( wxPGPropArg id, const wxArrayInt& value );

    /**
        Sets value (wxString) of a property.

        @remarks This method uses wxPGProperty::SetValueFromString(), which all
                properties should implement. This means that there should not be
                a type error, and instead the string is converted to property's
                actual value type.
    */
    void SetPropertyValueString( wxPGPropArg id, const wxString& value );

    /**
        Sets value (wxVariant) of a property.

        @remarks Use ChangePropertyValue() instead if you need to
        run through validation process and send property change event.
    */
    void SetPropertyValue( wxPGPropArg id, wxVariant value );

    /**
        Sets value (wxVariant&) of a property. Same as SetPropertyValue,
        but accepts reference.
    */
    void SetPropVal( wxPGPropArg id, wxVariant& value );

    /**
        Adjusts how wxPropertyGrid behaves when invalid value is entered
        in a property.

        @param vfbFlags
            See @ref propgrid_vfbflags for possible values.
    */
    void SetValidationFailureBehavior( int vfbFlags );

    /**
        Sorts all properties recursively.

        @param flags
            This can contain any of the following options:
              wxPG_SORT_TOP_LEVEL_ONLY: Only sort categories and their
                immediate children. Sorting done by wxPG_AUTO_SORT option
                uses this.

        @see SortChildren, wxPropertyGrid::SetSortFunction
    */
    void Sort( int flags = 0 );

    /**
        Sorts children of a property.

        @param id
            Name or pointer to a property.

        @param flags
            This can contain any of the following options:
              wxPG_RECURSE: Sorts recursively.

        @see Sort, wxPropertyGrid::SetSortFunction
    */
    void SortChildren( wxPGPropArg id, int flags = 0 );

    /**
        Returns editor pointer of editor with given name.
    */
    static wxPGEditor* GetEditorByName( const wxString& editorName );

    /**
        GetPropertyByName() with assertion error message.
    */
    wxPGProperty* GetPropertyByNameA( const wxString& name ) const;

    /**
        @remarks This function reselects the property and may cause
        excess flicker, so to just call Refresh() on a rect
        of single property, call DrawItem() instead.
    */
    virtual void RefreshProperty( wxPGProperty* p ) = 0;
};

// -----------------------------------------------------------------------

/** @section wxPGPropArgCls

    Most property grid functions have this type as their argument,
    as it can convey a property by either a pointer or name.
*/
class wxPGPropArgCls
{
public:
    wxPGPropArgCls( const wxPGProperty* property );
    wxPGPropArgCls( const wxString& str );
    wxPGPropArgCls( const wxPGPropArgCls& id );
    /** This is only needed for wxPython bindings. */
    wxPGPropArgCls( wxString* str, bool WXUNUSED(deallocPtr) );
    ~wxPGPropArgCls();

    wxPGProperty* GetPtr() const;
    wxPGPropArgCls( const char* str );
    wxPGPropArgCls( const wchar_t* str );
    /** This constructor is required for @NULL. */
    wxPGPropArgCls( int );
    wxPGProperty* GetPtr( wxPropertyGridInterface* iface ) const;
    wxPGProperty* GetPtr( const wxPropertyGridInterface* iface ) const;
    wxPGProperty* GetPtr0() const;
    bool HasName() const;
    const wxString& GetName() const;
};

typedef const wxPGPropArgCls& wxPGPropArg;
