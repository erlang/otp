/////////////////////////////////////////////////////////////////////////////
// Name:        wx/persist.h
// Purpose:     interface of wxPersistenceManager and related classes
// Author:      Vadim Zeitlin
// Copyright:   (c) 2009 Vadim Zeitlin <vadim@wxwidgets.org>
// Licence:     wxWindows licence
/////////////////////////////////////////////////////////////////////////////

/**
    Provides support for automatically saving and restoring object properties
    to persistent storage.

    This class is the central element of wxWidgets persistence framework, see
    @ref overview_persistence for its overview.

    This is a singleton class and its unique instance can be retrieved using
    Get() method.

    @since 2.9.0

    @library{wxcore}
 */
class wxPersistenceManager
{
public:
    /**
        Set the global persistence manager to use.

        Call this method to specify a non-default persistence manager to use.
        It should usually be called very early (e.g. in wxApp-derived class
        constructor or in the beginning of overridden wxApp::OnInit()) to
        affect creation of all persistent controls and the object passed to it
        must have a lifetime long enough to be still alive when the persistent
        controls are destroyed and need it to save their state so typically
        this would be a global or a wxApp member.

        @since 2.9.3
     */
    static void Set(wxPersistenceManager& manager);

    /**
        Returns the unique persistence manager object.

        If Set() hadn't been called before, a default persistence manager
        implementation is returned.
     */
    static wxPersistenceManager& Get();

    /**
        Globally disable saving the persistence object properties.

        By default, saving properties in Save() is enabled but the program may
        wish to disable if, for example, it detects that it is running on a
        system which shouldn't be modified in any way and so configuration
        file (or Windows registry) shouldn't be written to.

        @see DisableRestoring()
     */
    void DisableSaving();

    /**
        Globally disable restoring the persistence object properties.

        By default, restoring properties in Restore() is enabled but this
        function allows disabling it. This is mostly useful for testing.

        @see DisableSaving()
     */
    void DisableRestoring();


    /**
        Register an object with the manager automatically creating a
        persistence adapter for it.

        This is equivalent to calling Register(void *, wxPersistentObject *)
        with wxCreatePersistentObject(obj) as the second argument.

        @param obj
            The object to register. wxCreatePersistentObject() overload must be
            defined for the objects of this class.
     */
    template <class T>
    wxPersistentObject *Register(T *obj);

    /**
        Register an object with the manager.

        Note that registering the object doesn't do anything except allowing to
        call Restore() for it later. If you want to register the object and
        restore its properties, use RegisterAndRestore().

        The manager takes ownership of @a po and will delete it when it is
        unregistered.

        @param obj
            The object to register.
        @param po
            The wxPersistentObject to use for saving and restoring this object
            properties.
     */
    wxPersistentObject *Register(void *obj, wxPersistentObject *po);

    /**
        Check if the object is registered and return the associated
        wxPersistentObject if it is or @NULL otherwise.
     */
    wxPersistentObject *Find(void *obj) const;

    /**
        Unregister the object and delete the associated wxPersistentObject.

        For the persistent windows this is done automatically (via
        SaveAndUnregister()) when the window is destroyed so you only need to
        call this function explicitly if you are using custom persistent
        objects or if you want to prevent the object properties from being
        saved.

        @param obj
            An object previously registered with Register().
     */
    void Unregister(void *obj);


    /**
        Save the object properties to persistent storage.

        This method does nothing if DisableSaving() had been called.

        @param obj
            An object previously registered with Register().

        @see SaveAndUnregister()
     */
    void Save(void *obj);

    /**
        Restore the object properties previously saved by Save().

        This method does nothing if DisableRestoring() had been called.

        @param obj
            An object previously registered with Register().
        @return
            @true if the object properties were restored or @false if nothing
            was found to restore or the saved settings were invalid.

        @see RegisterAndRestore()
     */
    bool Restore(void *obj);

    /// Combines both Save() and Unregister() calls.
    void SaveAndUnregister(void *obj);

    /// Combines both Register() and Restore() calls.
    //@{
    template <class T>
    bool RegisterAndRestore(T *obj);

    bool RegisterAndRestore(void *obj, wxPersistentObject *po);
    //@}

protected:
    /**
        Protected default constructor.

        This constructor is only provided for the derived classes, to use an
        object of this class static Get() method should be called.
     */
    wxPersistenceManager();

    /**
        Return the config object to use.

        By default the global wxConfig, returned by wxConfigBase::Get(), is
        used but a derived class could override this function to return a
        different one if necessary.

        @since 2.9.3
     */
    virtual wxConfigBase *GetConfig() const;

    /**
        Return the path to use for saving the setting with the given name for
        the specified object.

        Notice that the @a name argument is the name of the setting, not the
        name of the object itself which can be retrieved with its GetName()
        method.

        This method can be overridden by a derived class to change where in
        wxConfig the different options are stored. By default, all settings of
        the persistent controls are stored under "Persistent_Options" group and
        grouped by control type (e.g. "Window" for top level windows or
        "Splitter") and name, so that the position of a splitter called "sep"
        could be stored under "Persistent_Options/Splitter/sep/Position" key.

        @since 2.9.3
     */
    virtual wxString GetKey(const wxPersistentObject& who,
                            const wxString& name) const;
};

/**
    Base class for persistent object adapters.

    See @ref overview_persistence for an overview of persistent objects within
    wxWidgets.

    wxWidgets persistence framework is non-intrusive, i.e. can work with the
    classes which have no relationship to nor knowledge of it. To allow this,
    an intermediate persistence adapter is used: this is just a simple object
    which provides the methods used by wxPersistenceManager to save and restore
    the object properties and implements them using the concrete class methods.

    You may derive your own classes from wxPersistentObject to implement
    persistence support for your common classes, see @ref persistence_defining.

    @see wxPersistentWindow<>
 */
class wxPersistentObject
{
public:
    /**
        Constructor takes the object which we're associated with.

        This object must have life-time greater than ours as we keep a pointer
        to it.
     */
    wxPersistentObject(void *obj);

    /// Trivial but virtual destructor.
    virtual ~wxPersistentObject();


    /**
        @name Methods to be implemented in the derived classes.

        Notice that these methods are only used by wxPersistenceManager
        normally and shouldn't be called directly.
     */
    //@{

    /**
        Save the object properties.

        The implementation of this method should use SaveValue().
     */
    virtual void Save() const = 0;

    /**
        Restore the object properties.

        The implementation of this method should use RestoreValue().
     */
    virtual bool Restore() = 0;


    /**
        Returns the string uniquely identifying the objects supported by this
        adapter.

        This method is called from SaveValue() and RestoreValue() and normally
        returns some short (but not too cryptic) strings, e.g. @c "Checkbox".
     */
    virtual wxString GetKind() const = 0;

    /**
        Returns the string uniquely identifying the object we're associated
        with among all the other objects of the same type.

        This method is used together with GetKind() to construct the unique
        full name of the object in e.g. a configuration file.
     */
    virtual wxString GetName() const = 0;

    //@}


    /// Return the associated object.
    void *GetObject() const;

protected:
    /**
        Save the specified value using the given name.

        @param name
            The name of the value in the configuration file.
        @param value
            The value to save, currently must be a type supported by wxConfig.
        @return
            @true if the value was saved or @false if an error occurred.
     */
    template <typename T>
    bool SaveValue(const wxString& name, T value) const;

    /**
        Restore a value saved by SaveValue().

        @param name
            The same name as was used by SaveValue().
        @param value
            Non-@NULL pointer to the same type that was passed to SaveValue().
            The pointed to object will be filled with the saved value if it
            was read successfully or not modified otherwise.
        @return
            @true if the value was successfully read or @false if it was not
            found or an error occurred.
     */
    template <typename T>
    bool RestoreValue(const wxString& name, T *value);
};

/**
    Function used to create the correct persistent adapter for the given type
    of objects.

    To be precise, there is no such template function definition but there are
    overloads of wxCreatePersistentObject() taking different object types for
    all wxWidgets classes supporting persistence. And you may also define your
    own overloads to integrate your custom classes with wxWidgets persistence
    framework.

    @see @ref persistence_defining

    @header{wx/persist.h}
 */
template <class T>
wxPersistentObject *wxCreatePersistentObject(T *obj);

/**
    A shorter synonym for wxPersistenceManager::RegisterAndRestore().

    This function simply calls wxPersistenceManager::RegisterAndRestore() but
    using it results in slightly shorter code as it calls
    wxPersistenceManager::Get() internally. As an additional convenience, this
    function can also set the window name.

    @param obj wxWindow-derived object to register with persistence manager and
        to try to restore the settings for.
    @param name If not empty, @a obj name is changed to the provided value
        before registering it.
    @return true if the settings were restored or false otherwise (this will
        always be the case when the program runs for the first time, for
        example).

    @since 2.9.0, @a name is new in 2.9.1.

    @header{wx/persist.h}
 */
template <class T>
bool wxPersistentRegisterAndRestore(T *obj, const wxString& name = wxString());
