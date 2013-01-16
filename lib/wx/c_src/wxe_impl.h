/*
 * %CopyrightBegin%
 *
 * Copyright Ericsson AB 2008-2013. All Rights Reserved.
 *
 * The contents of this file are subject to the Erlang Public License,
 * Version 1.1, (the "License"); you may not use this file except in
 * compliance with the License. You should have received a copy of the
 * Erlang Public License along with this software. If not, it can be
 * retrieved online at http://www.erlang.org/.
 *
 * Software distributed under the License is distributed on an "AS IS"
 * basis, WITHOUT WARRANTY OF ANY KIND, either express or implied. See
 * the License for the specific language governing rights and limitations
 * under the License.
 *
 * %CopyrightEnd% 
 */

#ifndef _WXE_IMPL_H
#define	_WXE_IMPL_H

#include <wx/glcanvas.h>
#include <wx/treectrl.h>
#include <wx/print.h>
extern "C" {
#include "wxe_driver.h"
}

DECLARE_EVENT_TYPE(wxeEVT_META_COMMAND, -1)

class wxeMetaCommand : public wxEvent
{
 public: 
 wxeMetaCommand(wxe_data *sd, int EvId) 
    : wxEvent(EvId, wxeEVT_META_COMMAND)
   {  caller = driver_caller(sd->port_handle);  port = sd->port; pdl = sd->pdl; } ;
 wxeMetaCommand(const wxeMetaCommand& event)  
    : wxEvent(event) 
   {  caller = event.caller; port = event.port; pdl = event.pdl; };
   virtual ~wxeMetaCommand() {};
   virtual wxEvent *Clone() const { return new wxeMetaCommand(*this); }
   
   ErlDrvTermData   caller;
   ErlDrvTermData   port;
   ErlDrvPDL        pdl;
};

class wxeCommand : public wxObject
{
 public: 
   wxeCommand(int fc,char * cbuf,int buflen, wxe_data *);
   virtual ~wxeCommand();

   ErlDrvTermData   caller;
   ErlDrvTermData   port;
   WXEBinRef *      bin[3];
   char *           buffer;
   int              len;
   int              op;
};

#define WXE_EVENT_PTR  0
#define WXE_OBJECT_PTR 1

class intListElement {
public: 
  intListElement(int Element) {car = Element; cdr = NULL;};
  intListElement(int Element, intListElement *list) 
  {car = Element; cdr = list;};
  int car;
  intListElement *cdr;
};

class intList { 
public: 
  intList() {list = NULL;};
  bool IsEmpty() {return list == NULL;};
  void Append(int Element) { list = new intListElement(Element, list); };
  int Pop() { 
    intListElement *temp = list; 
    int res = list->car;
    list = temp->cdr; 
    delete temp;
    return res;
  }
  intListElement *list;
};

class wxe_badarg 
{
public: 
  wxe_badarg(int Ref) : ref(Ref) { } ;
  int ref;
};

class wxeErlTerm : public wxClientData
{
 public:
   wxeErlTerm(WXEBinRef * data)
   { 
      size = data->size;
      bin = (char *) driver_alloc(size);
      memcpy(bin, data->base, size);
   } ;
   ~wxeErlTerm() { driver_free(bin); };
   char * bin;
   int size;
};

class wxeMemEnv 
{   
public:
  wxeMemEnv();
  int  next;
  int  max;
  void ** ref2ptr;
  intList  free;   
  ~wxeMemEnv();
  ErlDrvTermData owner;
};

class wxeRefData {
 public: 
   wxeRefData(unsigned int dref, int ttype, int is_new, wxeMemEnv *menv) : 
   ref(dref), type(ttype), alloc_in_erl(is_new), memenv(menv), pid(-1) { } ;
   int ref;
   int type;  
   // 0 = wxWindow subclasses, 1 = wxObject subclasses 
   // 2 = wxDialog subclasses, 3 = allocated wxObjects but not returned from new
   // > 3 classes which lack virtual destr, or are supposed to be allocated on
   //     the stack
   bool alloc_in_erl;
   wxeMemEnv *memenv;
   ErlDrvTermData pid;
};

WX_DECLARE_HASH_MAP(ErlDrvTermData, wxGLCanvas*, wxIntegerHash, wxIntegerEqual, wxeGLC);
WX_DECLARE_HASH_MAP(ErlDrvTermData, wxeMemEnv*, wxIntegerHash, wxIntegerEqual, wxeMemMap);


WX_DECLARE_VOIDPTR_HASH_MAP(wxeRefData *, ptrMap);

class WxeApp : public wxApp
{
public:
  virtual bool OnInit();
  void shutdown(wxeMetaCommand& event);

  int dispatch(wxList *, int, int);
  void dispatch_cb(wxList * batch, wxList * temp, ErlDrvTermData process);

  void wxe_dispatch(wxeCommand& event);

  void idle(wxIdleEvent& event);
  void dispatch_cmds();

  void dummy_close(wxEvent& Ev);
  bool sendevent(wxEvent *event);

  // MemEnv handling 
  void newMemEnv(wxeMetaCommand& event);
  void destroyMemEnv(wxeMetaCommand& event);
  wxeMemEnv * getMemEnv(ErlDrvTermData port);
  
  int  newPtr(void * ptr, int type, wxeMemEnv *memenv);
  int  getRef(void * ptr, wxeMemEnv *memenv);
  void * getPtr(char * bp, wxeMemEnv *memenv); 
  void clearPtr(void *ptr);
  void registerPid(char *ptr, ErlDrvTermData pid, wxeMemEnv *memenv);
  void init_nonconsts(wxeMemEnv *memenv, ErlDrvTermData caller); 
  
  // Code found in gen/wxe_derived_dest.h
  void delete_object(void *ptr, wxeRefData *refd);
  
  wxeMemMap refmap;
  ptrMap   ptr2ref;
  wxeMemEnv * global_me;
  
  // Temp container for callbacks
  char *cb_buff;
  int  cb_len;
};

class wxETreeItemData : public wxTreeItemData 
{
 public:
   wxETreeItemData(int sz, char * data);
   
   ~wxETreeItemData();
   
   int size;
   char * bin;
};

bool sendevent(wxEvent * event, ErlDrvTermData port);
void pre_callback();
void handle_event_callback(ErlDrvPort port, ErlDrvTermData process);

void activateGL(ErlDrvTermData caller);
void setActiveGL(ErlDrvTermData caller, wxGLCanvas *canvas);
void deleteActiveGL(wxGLCanvas *canvas);

void send_msg(const char *, wxString *);   // For debugging and error msgs

extern wxeGLC glc;

class wxEPrintout : public wxPrintout
{
 public: 
   wxEPrintout(wxString Title, int onPrintP, int onPrepareP,
	       int onBeginP, int onEndP,
	       int onBeginD, int onEndD,
	       int hasP, int getPageI, ErlDrvTermData Port) :
      wxPrintout(Title),
      onPrintPage(onPrintP), onPreparePrinting(onPrepareP), 
      onBeginPrinting(onBeginP), onEndPrinting(onEndP),
      onBeginDocument(onBeginD), onEndDocument(onEndD), hasPage(hasP), getPageInfo(getPageI),
      port(Port)
       { } ;

   ~wxEPrintout();
   
   bool OnBeginDocument(int startPage, int endPage);
   void OnEndDocument();
   void OnBeginPrinting();
   void OnEndPrinting();
   
   void OnPreparePrinting();
   
   bool HasPage(int page);
   bool OnPrintPage(int page);
   void GetPageInfo(int *minPage, int *maxPage, int *pageFrom, int *pageTo);

   int onPrintPage;
   int onPreparePrinting;
   int onBeginPrinting; 
   int onEndPrinting;
   int onBeginDocument;
   int onEndDocument;
   int hasPage; 
   int getPageInfo;

   ErlDrvTermData port;
};

void clear_cb(ErlDrvTermData port, int callback);


// Implementation of wxListCtrlCompare
struct callbackInfo {
   ErlDrvTermData port;
   int callbackID;
};

int wxCALLBACK wxEListCtrlCompare(long item1, long item2, long callbackInfoPtr);

#endif  //_WXE_IMPL_H
