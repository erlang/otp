/////////////////////////////////////////////////////////////////////////////
// Name:        module.h
// Purpose:     interface of wxModule
// Author:      wxWidgets team
// Licence:     wxWindows licence
/////////////////////////////////////////////////////////////////////////////

/**
    @class wxModule

    The module system is a very simple mechanism to allow applications (and parts
    of wxWidgets itself) to define initialization and cleanup functions that are
    automatically called on wxWidgets startup and exit.

    To define a new kind of module, derive a class from wxModule, override the
    wxModule::OnInit and wxModule::OnExit functions, and add the
    wxDECLARE_DYNAMIC_CLASS and wxIMPLEMENT_DYNAMIC_CLASS to header and implementation
    files (which can be the same file).
    On initialization, wxWidgets will find all classes derived from wxModule, create
    an instance of each, and call each wxModule::OnInit function. On exit, wxWidgets
    will call the wxModule::OnExit function for each module instance.

    Note that your module class does not have to be in a header file.

    For example:

    @code
        // A module to allow DDE initialization/cleanup
      // without calling these functions from app.cpp or from
      // the user's application.
      class wxDDEModule: public wxModule
      {
      public:
          wxDDEModule() { }
          virtual bool OnInit() { wxDDEInitialize(); return true; };
          virtual void OnExit() { wxDDECleanUp(); };

      private:
          wxDECLARE_DYNAMIC_CLASS(wxDDEModule);
      };

      wxIMPLEMENT_DYNAMIC_CLASS(wxDDEModule, wxModule);

      // Another module which uses DDE in its OnInit()
      class MyModule: public wxModule
      {
      public:
          MyModule() { AddDependency(wxCLASSINFO(wxDDEModule)); }
          virtual bool OnInit() { ... code using DDE ... }
          virtual void OnExit() { ... }

      private:
          wxDECLARE_DYNAMIC_CLASS(MyModule);
      };

      wxIMPLEMENT_DYNAMIC_CLASS(MyModule, wxModule);

      // Another module which uses DDE in its OnInit()
      // but uses a named dependency
      class MyModule2: public wxModule
      {
      public:
          MyModule2() { AddDependency("wxDDEModule"); }
          virtual bool OnInit() { ... code using DDE ... }
          virtual void OnExit() { ... }

      private:
          wxDECLARE_DYNAMIC_CLASS(MyModule2);
      };

      wxIMPLEMENT_DYNAMIC_CLASS(MyModule2, wxModule);
    @endcode

    @library{wxbase}
    @category{appmanagement}
*/
class wxModule : public wxObject
{
public:
    /**
        Constructs a wxModule object.
    */
    wxModule();

    /**
        Destructor.
    */
    virtual ~wxModule();

    /**
        Provide this function with appropriate cleanup for your module.
    */
    virtual void OnExit() = 0;

    /**
        Provide this function with appropriate initialization for your module.
        If the function returns @false, wxWidgets will exit immediately.
    */
    virtual bool OnInit() = 0;

protected:

    /**
        Call this function from the constructor of the derived class.

        @a dep must be the wxCLASSINFO() of a wxModule-derived class and the
        corresponding module will be loaded before and unloaded after this module.

        @param dep
            The class information object for the dependent module.
    */
    void AddDependency(wxClassInfo* dep);

    /**
        Call this function from the constructor of the derived class.

        This overload allows a dependency to be added by name without access to
        the class info.

        This is useful when a module is  declared entirely in a source file and
        there is no header for the declaration of the module needed by wxCLASSINFO(),
        however errors are not detected until run-time, instead of compile-time, then.
        Note that circular dependencies are detected and result in a fatal error.

        @param classname
            The class name of the dependent module.
    */
    void AddDependency(const char* classname);
};

