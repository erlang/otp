/*
 * %CopyrightBegin%
 *
 * Copyright Ericsson AB 2008-2023. All Rights Reserved.
 *
 * Licensed under the Apache License, Version 2.0 (the "License");
 * you may not use this file except in compliance with the License.
 * You may obtain a copy of the License at
 *
 *     http://www.apache.org/licenses/LICENSE-2.0
 *
 * Unless required by applicable law or agreed to in writing, software
 * distributed under the License is distributed on an "AS IS" BASIS,
 * WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
 * See the License for the specific language governing permissions and
 * limitations under the License.
 *
 * %CopyrightEnd%
*/

/***** This file is generated do not edit ****/

#include <wx/wx.h>
#include "../wxe_impl.h"
#include "../wxe_events.h"
#include "../wxe_return.h"
#include "../wxe_gl.h"
#include "wxe_macros.h"
#include "wxe_derived_dest.h"

// gdicmn::wxDisplaySize
void gdicmn_wxDisplaySize(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd)
{
  int width;
  int height;
  ::wxDisplaySize(&width,&height);
  wxeReturn rt = wxeReturn(memenv, Ecmd.caller, true);
  ERL_NIF_TERM msg = enif_make_tuple2(rt.env,
  rt.make_int(width),
  rt.make_int(height));
  rt.send(msg);

}

// gdicmn::wxSetCursor
void gdicmn_wxSetCursor(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd)
{
  ErlNifEnv *env = Ecmd.env;
  ERL_NIF_TERM * argv = Ecmd.args;
  wxCursor *cursor;
  cursor = (wxCursor *) memenv->getPtr(env, argv[0], "cursor");
  ::wxSetCursor(*cursor);

}

// wxEraseEvent::GetDC
void wxEraseEvent_GetDC(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd)
{
  ErlNifEnv *env = Ecmd.env;
  ERL_NIF_TERM * argv = Ecmd.args;
  wxEraseEvent *This;
  This = (wxEraseEvent *) memenv->getPtr(env, argv[0], "This");
  if(!This) throw wxe_badarg("This");
  wxDC * Result = (wxDC*)This->GetDC();
  wxeReturn rt = wxeReturn(memenv, Ecmd.caller, true);
  rt.send(  rt.make_ref(app->getRef((void *)Result,memenv), "wxDC"));

}

// wxEvent::GetId
void wxEvent_GetId(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd)
{
  ErlNifEnv *env = Ecmd.env;
  ERL_NIF_TERM * argv = Ecmd.args;
  wxEvent *This;
  This = (wxEvent *) memenv->getPtr(env, argv[0], "This");
  if(!This) throw wxe_badarg("This");
  int Result = This->GetId();
  wxeReturn rt = wxeReturn(memenv, Ecmd.caller, true);
  rt.send(  rt.make_int(Result));

}

// wxEvent::GetSkipped
void wxEvent_GetSkipped(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd)
{
  ErlNifEnv *env = Ecmd.env;
  ERL_NIF_TERM * argv = Ecmd.args;
  wxEvent *This;
  This = (wxEvent *) memenv->getPtr(env, argv[0], "This");
  if(!This) throw wxe_badarg("This");
  bool Result = This->GetSkipped();
  wxeReturn rt = wxeReturn(memenv, Ecmd.caller, true);
  rt.send(  rt.make_bool(Result));

}

// wxEvent::GetTimestamp
void wxEvent_GetTimestamp(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd)
{
  ErlNifEnv *env = Ecmd.env;
  ERL_NIF_TERM * argv = Ecmd.args;
  wxEvent *This;
  This = (wxEvent *) memenv->getPtr(env, argv[0], "This");
  if(!This) throw wxe_badarg("This");
  long Result = This->GetTimestamp();
  wxeReturn rt = wxeReturn(memenv, Ecmd.caller, true);
  rt.send(  rt.make_uint(Result));

}

// wxEvent::IsCommandEvent
void wxEvent_IsCommandEvent(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd)
{
  ErlNifEnv *env = Ecmd.env;
  ERL_NIF_TERM * argv = Ecmd.args;
  wxEvent *This;
  This = (wxEvent *) memenv->getPtr(env, argv[0], "This");
  if(!This) throw wxe_badarg("This");
  bool Result = This->IsCommandEvent();
  wxeReturn rt = wxeReturn(memenv, Ecmd.caller, true);
  rt.send(  rt.make_bool(Result));

}

// wxEvent::ResumePropagation
void wxEvent_ResumePropagation(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd)
{
  ErlNifEnv *env = Ecmd.env;
  ERL_NIF_TERM * argv = Ecmd.args;
  wxEvent *This;
  This = (wxEvent *) memenv->getPtr(env, argv[0], "This");
  int propagationLevel;
  if(!enif_get_int(env, argv[1], &propagationLevel)) Badarg("propagationLevel"); // int
  if(!This) throw wxe_badarg("This");
  This->ResumePropagation(propagationLevel);

}

// wxEvent::ShouldPropagate
void wxEvent_ShouldPropagate(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd)
{
  ErlNifEnv *env = Ecmd.env;
  ERL_NIF_TERM * argv = Ecmd.args;
  wxEvent *This;
  This = (wxEvent *) memenv->getPtr(env, argv[0], "This");
  if(!This) throw wxe_badarg("This");
  bool Result = This->ShouldPropagate();
  wxeReturn rt = wxeReturn(memenv, Ecmd.caller, true);
  rt.send(  rt.make_bool(Result));

}

// wxEvent::Skip
void wxEvent_Skip(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd)
{
  bool skip=true;
  ErlNifEnv *env = Ecmd.env;
  ERL_NIF_TERM * argv = Ecmd.args;
  wxEvent *This;
  This = (wxEvent *) memenv->getPtr(env, argv[0], "This");
  ERL_NIF_TERM lstHead, lstTail;
  lstTail = argv[1];
  if(!enif_is_list(env, lstTail)) Badarg("Options");
  const ERL_NIF_TERM *tpl;
  int tpl_sz;
  while(!enif_is_empty_list(env, lstTail)) {
    if(!enif_get_list_cell(env, lstTail, &lstHead, &lstTail)) Badarg("Options");
    if(!enif_get_tuple(env, lstHead, &tpl_sz, &tpl) || tpl_sz != 2) Badarg("Options");
    if(enif_is_identical(tpl[0], enif_make_atom(env, "skip"))) {
  skip = enif_is_identical(tpl[1], WXE_ATOM_true);
    } else        Badarg("Options");
  };
  if(!This) throw wxe_badarg("This");
  This->Skip(skip);

}

// wxEvent::StopPropagation
void wxEvent_StopPropagation(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd)
{
  ErlNifEnv *env = Ecmd.env;
  ERL_NIF_TERM * argv = Ecmd.args;
  wxEvent *This;
  This = (wxEvent *) memenv->getPtr(env, argv[0], "This");
  if(!This) throw wxe_badarg("This");
  int Result = This->StopPropagation();
  wxeReturn rt = wxeReturn(memenv, Ecmd.caller, true);
  rt.send(  rt.make_int(Result));

}


void wxEvtHandler_Connect(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd)
{
  ErlNifEnv *env = Ecmd.env;
  ERL_NIF_TERM * argv = Ecmd.args;
  int winid;
  int lastid;
  int skip;
  wxeErlTerm * userData;
  int fun_cb;
  wxEvtHandler *This = (wxEvtHandler *) memenv->getPtr(env, argv[0], "This");
  if(!enif_get_int(env, argv[1], &winid)) Badarg("Winid");
  if(!enif_get_int(env, argv[2], &lastid)) Badarg("LastId");
  skip = enif_is_identical(argv[3], WXE_ATOM_true);
  userData = new wxeErlTerm(argv[4]);
  if(!enif_get_int(env, argv[5], &fun_cb)) Badarg("FunId");
  if(!enif_is_atom(env, argv[6])) Badarg("EvType");
  int eventType = wxeEventTypeFromAtom(argv[6]);
  if(!enif_is_atom(env, argv[7])) Badarg("ClassName");

  wxeReturn rt = wxeReturn(memenv, Ecmd.caller, true);
  if(eventType > 0 ) {
    wxeEvtListener * Evt_cb = new wxeEvtListener(Ecmd.caller,app->getRef(This, memenv),
                                                 argv[7], fun_cb, skip, userData, memenv->me_ref);
    This->Connect(winid, lastid, eventType,
	          (wxObjectEventFunction)(wxEventFunction) &wxeEvtListener::forward,
	          Evt_cb, Evt_cb);
    rt.send(enif_make_tuple2(rt.env, WXE_ATOM_ok,
			     rt.make_ref(app->getRef((void *)Evt_cb,memenv),
					 "wxeEvtListener")));
  } else {
    rt.send(enif_make_tuple2(rt.env, WXE_ATOM_badarg, rt.make_atom("event_type")));
  }
}

void wxEvtHandler_Disconnect_2(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd)
{
  ErlNifEnv *env = Ecmd.env;
  ERL_NIF_TERM * argv = Ecmd.args;
  int winid;
  int lastid;

  wxeEvtListener *Listener = (wxeEvtListener *) memenv->getPtr(env, argv[0], "Listener");
  wxEvtHandler *This = (wxEvtHandler *) memenv->getPtr(env, argv[1],"This");
  if(!enif_get_int(env, argv[2], &winid)) Badarg("Winid");
  if(!enif_get_int(env, argv[3], &lastid)) Badarg("LastId");
  if(!enif_is_atom(env, argv[4])) Badarg("EvType");
  int eventType = wxeEventTypeFromAtom(argv[4]);
  wxeReturn rt = wxeReturn(memenv, Ecmd.caller, true);

  if(eventType > 0) {
    if(app->recurse_level > 1) {
      Ecmd.op = 101;
      app->delayed_delete->Append(&Ecmd);
    } else {
      bool Result = This->Disconnect(winid,lastid,eventType,
				     (wxObjectEventFunction)(wxEventFunction)
				     &wxeEvtListener::forward,
				     NULL, Listener);
      rt.send(rt.make_bool(Result));
    }
  } else {
    rt.send(enif_make_tuple2(rt.env, WXE_ATOM_badarg, rt.make_atom("event_type")));
  }

}

// wxFileDataObject::wxFileDataObject
void wxFileDataObject_new(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd)
{
  wxFileDataObject * Result = new wxFileDataObject();
  app->newPtr((void *) Result, 216, memenv);
  wxeReturn rt = wxeReturn(memenv, Ecmd.caller, true);
  rt.send(  rt.make_ref(app->getRef((void *)Result,memenv), "wxFileDataObject"));

}

// wxFileDataObject::AddFile
void wxFileDataObject_AddFile(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd)
{
  ErlNifEnv *env = Ecmd.env;
  ERL_NIF_TERM * argv = Ecmd.args;
  wxFileDataObject *This;
  This = (wxFileDataObject *) memenv->getPtr(env, argv[0], "This");
  ErlNifBinary file_bin;
  wxString file;
  if(!enif_inspect_binary(env, argv[1], &file_bin)) Badarg("file");
  file = wxString(file_bin.data, wxConvUTF8, file_bin.size);
  if(!This) throw wxe_badarg("This");
  This->AddFile(file);

}

// wxFileDataObject::GetFilenames
void wxFileDataObject_GetFilenames(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd)
{
  ErlNifEnv *env = Ecmd.env;
  ERL_NIF_TERM * argv = Ecmd.args;
  wxFileDataObject *This;
  This = (wxFileDataObject *) memenv->getPtr(env, argv[0], "This");
  if(!This) throw wxe_badarg("This");
  const wxArrayString Result = This->GetFilenames();
  wxeReturn rt = wxeReturn(memenv, Ecmd.caller, true);
  rt.send(  rt.make(Result));

}

// wxFileDataObject::destroy
void wxFileDataObject_destroy(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd)
{
 ErlNifEnv *env = Ecmd.env;
 ERL_NIF_TERM * argv = Ecmd.args;
  wxFileDataObject *This;
  This = (wxFileDataObject *) memenv->getPtr(env, argv[0], "This");
 if(This) {   ((WxeApp *) wxTheApp)->clearPtr((void *) This);
   delete This;}
}

// wxFileDialog::wxFileDialog
void wxFileDialog_new(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd)
{
  wxString message= wxFileSelectorPromptStr;
  wxString defaultDir= wxEmptyString;
  wxString defaultFile= wxEmptyString;
  wxString wildCard= wxFileSelectorDefaultWildcardStr;
  long style=wxFD_DEFAULT_STYLE;
  wxPoint pos= wxDefaultPosition;
  wxSize sz= wxDefaultSize;
  ErlNifEnv *env = Ecmd.env;
  ERL_NIF_TERM * argv = Ecmd.args;
  wxWindow *parent;
  parent = (wxWindow *) memenv->getPtr(env, argv[0], "parent");
  ERL_NIF_TERM lstHead, lstTail;
  lstTail = argv[1];
  if(!enif_is_list(env, lstTail)) Badarg("Options");
  const ERL_NIF_TERM *tpl;
  int tpl_sz;
  while(!enif_is_empty_list(env, lstTail)) {
    if(!enif_get_list_cell(env, lstTail, &lstHead, &lstTail)) Badarg("Options");
    if(!enif_get_tuple(env, lstHead, &tpl_sz, &tpl) || tpl_sz != 2) Badarg("Options");
    if(enif_is_identical(tpl[0], enif_make_atom(env, "message"))) {
  ErlNifBinary message_bin;
  if(!enif_inspect_binary(env, tpl[1], &message_bin)) Badarg("message");
  message = wxString(message_bin.data, wxConvUTF8, message_bin.size);
    } else     if(enif_is_identical(tpl[0], enif_make_atom(env, "defaultDir"))) {
  ErlNifBinary defaultDir_bin;
  if(!enif_inspect_binary(env, tpl[1], &defaultDir_bin)) Badarg("defaultDir");
  defaultDir = wxString(defaultDir_bin.data, wxConvUTF8, defaultDir_bin.size);
    } else     if(enif_is_identical(tpl[0], enif_make_atom(env, "defaultFile"))) {
  ErlNifBinary defaultFile_bin;
  if(!enif_inspect_binary(env, tpl[1], &defaultFile_bin)) Badarg("defaultFile");
  defaultFile = wxString(defaultFile_bin.data, wxConvUTF8, defaultFile_bin.size);
    } else     if(enif_is_identical(tpl[0], enif_make_atom(env, "wildCard"))) {
  ErlNifBinary wildCard_bin;
  if(!enif_inspect_binary(env, tpl[1], &wildCard_bin)) Badarg("wildCard");
  wildCard = wxString(wildCard_bin.data, wxConvUTF8, wildCard_bin.size);
    } else     if(enif_is_identical(tpl[0], enif_make_atom(env, "style"))) {
  if(!enif_get_long(env, tpl[1], &style)) Badarg("style");
    } else     if(enif_is_identical(tpl[0], enif_make_atom(env, "pos"))) {
  const ERL_NIF_TERM *pos_t;
  int pos_sz;
  if(!enif_get_tuple(env, tpl[1], &pos_sz, &pos_t)) Badarg("pos");
  int posX;
  if(!enif_get_int(env, pos_t[0], &posX)) Badarg("pos");
  int posY;
  if(!enif_get_int(env, pos_t[1], &posY)) Badarg("pos");
  pos = wxPoint(posX,posY);
    } else     if(enif_is_identical(tpl[0], enif_make_atom(env, "sz"))) {
  const ERL_NIF_TERM *sz_t;
  int sz_sz;
  if(!enif_get_tuple(env, tpl[1], &sz_sz, &sz_t)) Badarg("sz");
  int szW;
  if(!enif_get_int(env, sz_t[0], &szW)) Badarg("sz");
  int szH;
  if(!enif_get_int(env, sz_t[1], &szH)) Badarg("sz");
  sz = wxSize(szW,szH);
    } else        Badarg("Options");
  };
  wxFileDialog * Result = new EwxFileDialog(parent,message,defaultDir,defaultFile,wildCard,style,pos,sz);
  app->newPtr((void *) Result, 2, memenv);
  wxeReturn rt = wxeReturn(memenv, Ecmd.caller, true);
  rt.send(  rt.make_ref(app->getRef((void *)Result,memenv), "wxFileDialog"));

}

// wxFileDialog::GetDirectory
void wxFileDialog_GetDirectory(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd)
{
  ErlNifEnv *env = Ecmd.env;
  ERL_NIF_TERM * argv = Ecmd.args;
  wxFileDialog *This;
  This = (wxFileDialog *) memenv->getPtr(env, argv[0], "This");
  if(!This) throw wxe_badarg("This");
  wxString Result = This->GetDirectory();
  wxeReturn rt = wxeReturn(memenv, Ecmd.caller, true);
  rt.send(  rt.make(Result));

}

// wxFileDialog::GetFilename
void wxFileDialog_GetFilename(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd)
{
  ErlNifEnv *env = Ecmd.env;
  ERL_NIF_TERM * argv = Ecmd.args;
  wxFileDialog *This;
  This = (wxFileDialog *) memenv->getPtr(env, argv[0], "This");
  if(!This) throw wxe_badarg("This");
  wxString Result = This->GetFilename();
  wxeReturn rt = wxeReturn(memenv, Ecmd.caller, true);
  rt.send(  rt.make(Result));

}

// wxFileDialog::GetFilenames
void wxFileDialog_GetFilenames(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd)
{
  wxArrayString filenames;
  ErlNifEnv *env = Ecmd.env;
  ERL_NIF_TERM * argv = Ecmd.args;
  wxFileDialog *This;
  This = (wxFileDialog *) memenv->getPtr(env, argv[0], "This");
  if(!This) throw wxe_badarg("This");
  This->GetFilenames(filenames);
  wxeReturn rt = wxeReturn(memenv, Ecmd.caller, true);
  rt.send(  rt.make(filenames));

}

// wxFileDialog::GetFilterIndex
void wxFileDialog_GetFilterIndex(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd)
{
  ErlNifEnv *env = Ecmd.env;
  ERL_NIF_TERM * argv = Ecmd.args;
  wxFileDialog *This;
  This = (wxFileDialog *) memenv->getPtr(env, argv[0], "This");
  if(!This) throw wxe_badarg("This");
  int Result = This->GetFilterIndex();
  wxeReturn rt = wxeReturn(memenv, Ecmd.caller, true);
  rt.send(  rt.make_int(Result));

}

// wxFileDialog::GetMessage
void wxFileDialog_GetMessage(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd)
{
  ErlNifEnv *env = Ecmd.env;
  ERL_NIF_TERM * argv = Ecmd.args;
  wxFileDialog *This;
  This = (wxFileDialog *) memenv->getPtr(env, argv[0], "This");
  if(!This) throw wxe_badarg("This");
  wxString Result = This->GetMessage();
  wxeReturn rt = wxeReturn(memenv, Ecmd.caller, true);
  rt.send(  rt.make(Result));

}

// wxFileDialog::GetPath
void wxFileDialog_GetPath(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd)
{
  ErlNifEnv *env = Ecmd.env;
  ERL_NIF_TERM * argv = Ecmd.args;
  wxFileDialog *This;
  This = (wxFileDialog *) memenv->getPtr(env, argv[0], "This");
  if(!This) throw wxe_badarg("This");
  wxString Result = This->GetPath();
  wxeReturn rt = wxeReturn(memenv, Ecmd.caller, true);
  rt.send(  rt.make(Result));

}

// wxFileDialog::GetPaths
void wxFileDialog_GetPaths(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd)
{
  wxArrayString paths;
  ErlNifEnv *env = Ecmd.env;
  ERL_NIF_TERM * argv = Ecmd.args;
  wxFileDialog *This;
  This = (wxFileDialog *) memenv->getPtr(env, argv[0], "This");
  if(!This) throw wxe_badarg("This");
  This->GetPaths(paths);
  wxeReturn rt = wxeReturn(memenv, Ecmd.caller, true);
  rt.send(  rt.make(paths));

}

// wxFileDialog::GetWildcard
void wxFileDialog_GetWildcard(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd)
{
  ErlNifEnv *env = Ecmd.env;
  ERL_NIF_TERM * argv = Ecmd.args;
  wxFileDialog *This;
  This = (wxFileDialog *) memenv->getPtr(env, argv[0], "This");
  if(!This) throw wxe_badarg("This");
  wxString Result = This->GetWildcard();
  wxeReturn rt = wxeReturn(memenv, Ecmd.caller, true);
  rt.send(  rt.make(Result));

}

// wxFileDialog::SetDirectory
void wxFileDialog_SetDirectory(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd)
{
  ErlNifEnv *env = Ecmd.env;
  ERL_NIF_TERM * argv = Ecmd.args;
  wxFileDialog *This;
  This = (wxFileDialog *) memenv->getPtr(env, argv[0], "This");
  ErlNifBinary directory_bin;
  wxString directory;
  if(!enif_inspect_binary(env, argv[1], &directory_bin)) Badarg("directory");
  directory = wxString(directory_bin.data, wxConvUTF8, directory_bin.size);
  if(!This) throw wxe_badarg("This");
  This->SetDirectory(directory);

}

// wxFileDialog::SetFilename
void wxFileDialog_SetFilename(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd)
{
  ErlNifEnv *env = Ecmd.env;
  ERL_NIF_TERM * argv = Ecmd.args;
  wxFileDialog *This;
  This = (wxFileDialog *) memenv->getPtr(env, argv[0], "This");
  ErlNifBinary setfilename_bin;
  wxString setfilename;
  if(!enif_inspect_binary(env, argv[1], &setfilename_bin)) Badarg("setfilename");
  setfilename = wxString(setfilename_bin.data, wxConvUTF8, setfilename_bin.size);
  if(!This) throw wxe_badarg("This");
  This->SetFilename(setfilename);

}

// wxFileDialog::SetFilterIndex
void wxFileDialog_SetFilterIndex(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd)
{
  ErlNifEnv *env = Ecmd.env;
  ERL_NIF_TERM * argv = Ecmd.args;
  wxFileDialog *This;
  This = (wxFileDialog *) memenv->getPtr(env, argv[0], "This");
  int filterIndex;
  if(!enif_get_int(env, argv[1], &filterIndex)) Badarg("filterIndex"); // int
  if(!This) throw wxe_badarg("This");
  This->SetFilterIndex(filterIndex);

}

// wxFileDialog::SetMessage
void wxFileDialog_SetMessage(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd)
{
  ErlNifEnv *env = Ecmd.env;
  ERL_NIF_TERM * argv = Ecmd.args;
  wxFileDialog *This;
  This = (wxFileDialog *) memenv->getPtr(env, argv[0], "This");
  ErlNifBinary message_bin;
  wxString message;
  if(!enif_inspect_binary(env, argv[1], &message_bin)) Badarg("message");
  message = wxString(message_bin.data, wxConvUTF8, message_bin.size);
  if(!This) throw wxe_badarg("This");
  This->SetMessage(message);

}

// wxFileDialog::SetPath
void wxFileDialog_SetPath(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd)
{
  ErlNifEnv *env = Ecmd.env;
  ERL_NIF_TERM * argv = Ecmd.args;
  wxFileDialog *This;
  This = (wxFileDialog *) memenv->getPtr(env, argv[0], "This");
  ErlNifBinary path_bin;
  wxString path;
  if(!enif_inspect_binary(env, argv[1], &path_bin)) Badarg("path");
  path = wxString(path_bin.data, wxConvUTF8, path_bin.size);
  if(!This) throw wxe_badarg("This");
  This->SetPath(path);

}

// wxFileDialog::SetWildcard
void wxFileDialog_SetWildcard(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd)
{
  ErlNifEnv *env = Ecmd.env;
  ERL_NIF_TERM * argv = Ecmd.args;
  wxFileDialog *This;
  This = (wxFileDialog *) memenv->getPtr(env, argv[0], "This");
  ErlNifBinary wildCard_bin;
  wxString wildCard;
  if(!enif_inspect_binary(env, argv[1], &wildCard_bin)) Badarg("wildCard");
  wildCard = wxString(wildCard_bin.data, wxConvUTF8, wildCard_bin.size);
  if(!This) throw wxe_badarg("This");
  This->SetWildcard(wildCard);

}

// wxFileDirPickerEvent::GetPath
void wxFileDirPickerEvent_GetPath(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd)
{
  ErlNifEnv *env = Ecmd.env;
  ERL_NIF_TERM * argv = Ecmd.args;
  wxFileDirPickerEvent *This;
  This = (wxFileDirPickerEvent *) memenv->getPtr(env, argv[0], "This");
  if(!This) throw wxe_badarg("This");
  wxString Result = This->GetPath();
  wxeReturn rt = wxeReturn(memenv, Ecmd.caller, true);
  rt.send(  rt.make(Result));

}

// wxFilePickerCtrl::wxFilePickerCtrl
void wxFilePickerCtrl_new_0(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd)
{
  wxFilePickerCtrl * Result = new EwxFilePickerCtrl();
  app->newPtr((void *) Result, 0, memenv);
  wxeReturn rt = wxeReturn(memenv, Ecmd.caller, true);
  rt.send(  rt.make_ref(app->getRef((void *)Result,memenv), "wxFilePickerCtrl"));

}

// wxFilePickerCtrl::wxFilePickerCtrl
void wxFilePickerCtrl_new_3(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd)
{
  wxString path= wxEmptyString;
  wxString message= wxFileSelectorPromptStr;
  wxString wildcard= wxFileSelectorDefaultWildcardStr;
  wxPoint pos= wxDefaultPosition;
  wxSize size= wxDefaultSize;
  long style=wxFLP_DEFAULT_STYLE;
  const wxValidator * validator= &wxDefaultValidator;
  ErlNifEnv *env = Ecmd.env;
  ERL_NIF_TERM * argv = Ecmd.args;
  wxWindow *parent;
  parent = (wxWindow *) memenv->getPtr(env, argv[0], "parent");
  int id;
  if(!enif_get_int(env, argv[1], &id)) Badarg("id"); // wxWindowID
  ERL_NIF_TERM lstHead, lstTail;
  lstTail = argv[2];
  if(!enif_is_list(env, lstTail)) Badarg("Options");
  const ERL_NIF_TERM *tpl;
  int tpl_sz;
  while(!enif_is_empty_list(env, lstTail)) {
    if(!enif_get_list_cell(env, lstTail, &lstHead, &lstTail)) Badarg("Options");
    if(!enif_get_tuple(env, lstHead, &tpl_sz, &tpl) || tpl_sz != 2) Badarg("Options");
    if(enif_is_identical(tpl[0], enif_make_atom(env, "path"))) {
  ErlNifBinary path_bin;
  if(!enif_inspect_binary(env, tpl[1], &path_bin)) Badarg("path");
  path = wxString(path_bin.data, wxConvUTF8, path_bin.size);
    } else     if(enif_is_identical(tpl[0], enif_make_atom(env, "message"))) {
  ErlNifBinary message_bin;
  if(!enif_inspect_binary(env, tpl[1], &message_bin)) Badarg("message");
  message = wxString(message_bin.data, wxConvUTF8, message_bin.size);
    } else     if(enif_is_identical(tpl[0], enif_make_atom(env, "wildcard"))) {
  ErlNifBinary wildcard_bin;
  if(!enif_inspect_binary(env, tpl[1], &wildcard_bin)) Badarg("wildcard");
  wildcard = wxString(wildcard_bin.data, wxConvUTF8, wildcard_bin.size);
    } else     if(enif_is_identical(tpl[0], enif_make_atom(env, "pos"))) {
  const ERL_NIF_TERM *pos_t;
  int pos_sz;
  if(!enif_get_tuple(env, tpl[1], &pos_sz, &pos_t)) Badarg("pos");
  int posX;
  if(!enif_get_int(env, pos_t[0], &posX)) Badarg("pos");
  int posY;
  if(!enif_get_int(env, pos_t[1], &posY)) Badarg("pos");
  pos = wxPoint(posX,posY);
    } else     if(enif_is_identical(tpl[0], enif_make_atom(env, "size"))) {
  const ERL_NIF_TERM *size_t;
  int size_sz;
  if(!enif_get_tuple(env, tpl[1], &size_sz, &size_t)) Badarg("size");
  int sizeW;
  if(!enif_get_int(env, size_t[0], &sizeW)) Badarg("size");
  int sizeH;
  if(!enif_get_int(env, size_t[1], &sizeH)) Badarg("size");
  size = wxSize(sizeW,sizeH);
    } else     if(enif_is_identical(tpl[0], enif_make_atom(env, "style"))) {
  if(!enif_get_long(env, tpl[1], &style)) Badarg("style");
    } else     if(enif_is_identical(tpl[0], enif_make_atom(env, "validator"))) {
  validator = (wxValidator *) memenv->getPtr(env, tpl[1], "validator");
    } else        Badarg("Options");
  };
  wxFilePickerCtrl * Result = new EwxFilePickerCtrl(parent,id,path,message,wildcard,pos,size,style,*validator);
  app->newPtr((void *) Result, 0, memenv);
  wxeReturn rt = wxeReturn(memenv, Ecmd.caller, true);
  rt.send(  rt.make_ref(app->getRef((void *)Result,memenv), "wxFilePickerCtrl"));

}

// wxFilePickerCtrl::Create
void wxFilePickerCtrl_Create(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd)
{
  wxString path= wxEmptyString;
  wxString message= wxFileSelectorPromptStr;
  wxString wildcard= wxFileSelectorDefaultWildcardStr;
  wxPoint pos= wxDefaultPosition;
  wxSize size= wxDefaultSize;
  long style=wxFLP_DEFAULT_STYLE;
  const wxValidator * validator= &wxDefaultValidator;
  ErlNifEnv *env = Ecmd.env;
  ERL_NIF_TERM * argv = Ecmd.args;
  wxFilePickerCtrl *This;
  This = (wxFilePickerCtrl *) memenv->getPtr(env, argv[0], "This");
  wxWindow *parent;
  parent = (wxWindow *) memenv->getPtr(env, argv[1], "parent");
  int id;
  if(!enif_get_int(env, argv[2], &id)) Badarg("id"); // wxWindowID
  ERL_NIF_TERM lstHead, lstTail;
  lstTail = argv[3];
  if(!enif_is_list(env, lstTail)) Badarg("Options");
  const ERL_NIF_TERM *tpl;
  int tpl_sz;
  while(!enif_is_empty_list(env, lstTail)) {
    if(!enif_get_list_cell(env, lstTail, &lstHead, &lstTail)) Badarg("Options");
    if(!enif_get_tuple(env, lstHead, &tpl_sz, &tpl) || tpl_sz != 2) Badarg("Options");
    if(enif_is_identical(tpl[0], enif_make_atom(env, "path"))) {
  ErlNifBinary path_bin;
  if(!enif_inspect_binary(env, tpl[1], &path_bin)) Badarg("path");
  path = wxString(path_bin.data, wxConvUTF8, path_bin.size);
    } else     if(enif_is_identical(tpl[0], enif_make_atom(env, "message"))) {
  ErlNifBinary message_bin;
  if(!enif_inspect_binary(env, tpl[1], &message_bin)) Badarg("message");
  message = wxString(message_bin.data, wxConvUTF8, message_bin.size);
    } else     if(enif_is_identical(tpl[0], enif_make_atom(env, "wildcard"))) {
  ErlNifBinary wildcard_bin;
  if(!enif_inspect_binary(env, tpl[1], &wildcard_bin)) Badarg("wildcard");
  wildcard = wxString(wildcard_bin.data, wxConvUTF8, wildcard_bin.size);
    } else     if(enif_is_identical(tpl[0], enif_make_atom(env, "pos"))) {
  const ERL_NIF_TERM *pos_t;
  int pos_sz;
  if(!enif_get_tuple(env, tpl[1], &pos_sz, &pos_t)) Badarg("pos");
  int posX;
  if(!enif_get_int(env, pos_t[0], &posX)) Badarg("pos");
  int posY;
  if(!enif_get_int(env, pos_t[1], &posY)) Badarg("pos");
  pos = wxPoint(posX,posY);
    } else     if(enif_is_identical(tpl[0], enif_make_atom(env, "size"))) {
  const ERL_NIF_TERM *size_t;
  int size_sz;
  if(!enif_get_tuple(env, tpl[1], &size_sz, &size_t)) Badarg("size");
  int sizeW;
  if(!enif_get_int(env, size_t[0], &sizeW)) Badarg("size");
  int sizeH;
  if(!enif_get_int(env, size_t[1], &sizeH)) Badarg("size");
  size = wxSize(sizeW,sizeH);
    } else     if(enif_is_identical(tpl[0], enif_make_atom(env, "style"))) {
  if(!enif_get_long(env, tpl[1], &style)) Badarg("style");
    } else     if(enif_is_identical(tpl[0], enif_make_atom(env, "validator"))) {
  validator = (wxValidator *) memenv->getPtr(env, tpl[1], "validator");
    } else        Badarg("Options");
  };
  if(!This) throw wxe_badarg("This");
  bool Result = This->Create(parent,id,path,message,wildcard,pos,size,style,*validator);
  wxeReturn rt = wxeReturn(memenv, Ecmd.caller, true);
  rt.send(  rt.make_bool(Result));

}

// wxFilePickerCtrl::GetPath
void wxFilePickerCtrl_GetPath(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd)
{
  ErlNifEnv *env = Ecmd.env;
  ERL_NIF_TERM * argv = Ecmd.args;
  wxFilePickerCtrl *This;
  This = (wxFilePickerCtrl *) memenv->getPtr(env, argv[0], "This");
  if(!This) throw wxe_badarg("This");
  wxString Result = This->GetPath();
  wxeReturn rt = wxeReturn(memenv, Ecmd.caller, true);
  rt.send(  rt.make(Result));

}

// wxFilePickerCtrl::SetPath
void wxFilePickerCtrl_SetPath(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd)
{
  ErlNifEnv *env = Ecmd.env;
  ERL_NIF_TERM * argv = Ecmd.args;
  wxFilePickerCtrl *This;
  This = (wxFilePickerCtrl *) memenv->getPtr(env, argv[0], "This");
  ErlNifBinary filename_bin;
  wxString filename;
  if(!enif_inspect_binary(env, argv[1], &filename_bin)) Badarg("filename");
  filename = wxString(filename_bin.data, wxConvUTF8, filename_bin.size);
  if(!This) throw wxe_badarg("This");
  This->SetPath(filename);

}

// wxFindReplaceData::wxFindReplaceData
void wxFindReplaceData_new(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd)
{
  unsigned int flags=0;
  ErlNifEnv *env = Ecmd.env;
  ERL_NIF_TERM * argv = Ecmd.args;
  ERL_NIF_TERM lstHead, lstTail;
  lstTail = argv[0];
  if(!enif_is_list(env, lstTail)) Badarg("Options");
  const ERL_NIF_TERM *tpl;
  int tpl_sz;
  while(!enif_is_empty_list(env, lstTail)) {
    if(!enif_get_list_cell(env, lstTail, &lstHead, &lstTail)) Badarg("Options");
    if(!enif_get_tuple(env, lstHead, &tpl_sz, &tpl) || tpl_sz != 2) Badarg("Options");
    if(enif_is_identical(tpl[0], enif_make_atom(env, "flags"))) {
  if(!enif_get_uint(env, tpl[1], &flags)) Badarg("flags");
    } else        Badarg("Options");
  };
  wxFindReplaceData * Result = new EwxFindReplaceData(flags);
  app->newPtr((void *) Result, 1, memenv);
  wxeReturn rt = wxeReturn(memenv, Ecmd.caller, true);
  rt.send(  rt.make_ref(app->getRef((void *)Result,memenv), "wxFindReplaceData"));

}

// wxFindReplaceData::GetFindString
void wxFindReplaceData_GetFindString(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd)
{
  ErlNifEnv *env = Ecmd.env;
  ERL_NIF_TERM * argv = Ecmd.args;
  wxFindReplaceData *This;
  This = (wxFindReplaceData *) memenv->getPtr(env, argv[0], "This");
  if(!This) throw wxe_badarg("This");
  const wxString Result = This->GetFindString();
  wxeReturn rt = wxeReturn(memenv, Ecmd.caller, true);
  rt.send(  rt.make(Result));

}

// wxFindReplaceData::GetReplaceString
void wxFindReplaceData_GetReplaceString(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd)
{
  ErlNifEnv *env = Ecmd.env;
  ERL_NIF_TERM * argv = Ecmd.args;
  wxFindReplaceData *This;
  This = (wxFindReplaceData *) memenv->getPtr(env, argv[0], "This");
  if(!This) throw wxe_badarg("This");
  const wxString Result = This->GetReplaceString();
  wxeReturn rt = wxeReturn(memenv, Ecmd.caller, true);
  rt.send(  rt.make(Result));

}

// wxFindReplaceData::GetFlags
void wxFindReplaceData_GetFlags(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd)
{
  ErlNifEnv *env = Ecmd.env;
  ERL_NIF_TERM * argv = Ecmd.args;
  wxFindReplaceData *This;
  This = (wxFindReplaceData *) memenv->getPtr(env, argv[0], "This");
  if(!This) throw wxe_badarg("This");
  int Result = This->GetFlags();
  wxeReturn rt = wxeReturn(memenv, Ecmd.caller, true);
  rt.send(  rt.make_int(Result));

}

// wxFindReplaceData::SetFlags
void wxFindReplaceData_SetFlags(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd)
{
  ErlNifEnv *env = Ecmd.env;
  ERL_NIF_TERM * argv = Ecmd.args;
  wxFindReplaceData *This;
  This = (wxFindReplaceData *) memenv->getPtr(env, argv[0], "This");
  unsigned int flags;
  if(!enif_get_uint(env, argv[1], &flags)) Badarg("flags");
  if(!This) throw wxe_badarg("This");
  This->SetFlags(flags);

}

// wxFindReplaceData::SetFindString
void wxFindReplaceData_SetFindString(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd)
{
  ErlNifEnv *env = Ecmd.env;
  ERL_NIF_TERM * argv = Ecmd.args;
  wxFindReplaceData *This;
  This = (wxFindReplaceData *) memenv->getPtr(env, argv[0], "This");
  ErlNifBinary str_bin;
  wxString str;
  if(!enif_inspect_binary(env, argv[1], &str_bin)) Badarg("str");
  str = wxString(str_bin.data, wxConvUTF8, str_bin.size);
  if(!This) throw wxe_badarg("This");
  This->SetFindString(str);

}

// wxFindReplaceData::SetReplaceString
void wxFindReplaceData_SetReplaceString(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd)
{
  ErlNifEnv *env = Ecmd.env;
  ERL_NIF_TERM * argv = Ecmd.args;
  wxFindReplaceData *This;
  This = (wxFindReplaceData *) memenv->getPtr(env, argv[0], "This");
  ErlNifBinary str_bin;
  wxString str;
  if(!enif_inspect_binary(env, argv[1], &str_bin)) Badarg("str");
  str = wxString(str_bin.data, wxConvUTF8, str_bin.size);
  if(!This) throw wxe_badarg("This");
  This->SetReplaceString(str);

}

// wxFindReplaceDialog::wxFindReplaceDialog
void wxFindReplaceDialog_new_0(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd)
{
  wxFindReplaceDialog * Result = new EwxFindReplaceDialog();
  app->newPtr((void *) Result, 2, memenv);
  wxeReturn rt = wxeReturn(memenv, Ecmd.caller, true);
  rt.send(  rt.make_ref(app->getRef((void *)Result,memenv), "wxFindReplaceDialog"));

}

// wxFindReplaceDialog::wxFindReplaceDialog
void wxFindReplaceDialog_new_4(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd)
{
  int style=0;
  ErlNifEnv *env = Ecmd.env;
  ERL_NIF_TERM * argv = Ecmd.args;
  wxWindow *parent;
  parent = (wxWindow *) memenv->getPtr(env, argv[0], "parent");
  wxFindReplaceData *data;
  data = (wxFindReplaceData *) memenv->getPtr(env, argv[1], "data");
  ErlNifBinary title_bin;
  wxString title;
  if(!enif_inspect_binary(env, argv[2], &title_bin)) Badarg("title");
  title = wxString(title_bin.data, wxConvUTF8, title_bin.size);
  ERL_NIF_TERM lstHead, lstTail;
  lstTail = argv[3];
  if(!enif_is_list(env, lstTail)) Badarg("Options");
  const ERL_NIF_TERM *tpl;
  int tpl_sz;
  while(!enif_is_empty_list(env, lstTail)) {
    if(!enif_get_list_cell(env, lstTail, &lstHead, &lstTail)) Badarg("Options");
    if(!enif_get_tuple(env, lstHead, &tpl_sz, &tpl) || tpl_sz != 2) Badarg("Options");
    if(enif_is_identical(tpl[0], enif_make_atom(env, "style"))) {
  if(!enif_get_int(env, tpl[1], &style)) Badarg("style"); // int
    } else        Badarg("Options");
  };
  wxFindReplaceDialog * Result = new EwxFindReplaceDialog(parent,data,title,style);
  app->newPtr((void *) Result, 2, memenv);
  wxeReturn rt = wxeReturn(memenv, Ecmd.caller, true);
  rt.send(  rt.make_ref(app->getRef((void *)Result,memenv), "wxFindReplaceDialog"));

}

// wxFindReplaceDialog::Create
void wxFindReplaceDialog_Create(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd)
{
  int style=0;
  ErlNifEnv *env = Ecmd.env;
  ERL_NIF_TERM * argv = Ecmd.args;
  wxFindReplaceDialog *This;
  This = (wxFindReplaceDialog *) memenv->getPtr(env, argv[0], "This");
  wxWindow *parent;
  parent = (wxWindow *) memenv->getPtr(env, argv[1], "parent");
  wxFindReplaceData *data;
  data = (wxFindReplaceData *) memenv->getPtr(env, argv[2], "data");
  ErlNifBinary title_bin;
  wxString title;
  if(!enif_inspect_binary(env, argv[3], &title_bin)) Badarg("title");
  title = wxString(title_bin.data, wxConvUTF8, title_bin.size);
  ERL_NIF_TERM lstHead, lstTail;
  lstTail = argv[4];
  if(!enif_is_list(env, lstTail)) Badarg("Options");
  const ERL_NIF_TERM *tpl;
  int tpl_sz;
  while(!enif_is_empty_list(env, lstTail)) {
    if(!enif_get_list_cell(env, lstTail, &lstHead, &lstTail)) Badarg("Options");
    if(!enif_get_tuple(env, lstHead, &tpl_sz, &tpl) || tpl_sz != 2) Badarg("Options");
    if(enif_is_identical(tpl[0], enif_make_atom(env, "style"))) {
  if(!enif_get_int(env, tpl[1], &style)) Badarg("style"); // int
    } else        Badarg("Options");
  };
  if(!This) throw wxe_badarg("This");
  bool Result = This->Create(parent,data,title,style);
  wxeReturn rt = wxeReturn(memenv, Ecmd.caller, true);
  rt.send(  rt.make_bool(Result));

}

// wxFindReplaceDialog::GetData
void wxFindReplaceDialog_GetData(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd)
{
  ErlNifEnv *env = Ecmd.env;
  ERL_NIF_TERM * argv = Ecmd.args;
  wxFindReplaceDialog *This;
  This = (wxFindReplaceDialog *) memenv->getPtr(env, argv[0], "This");
  if(!This) throw wxe_badarg("This");
  const wxFindReplaceData * Result = (wxFindReplaceData*)This->GetData();
  wxeReturn rt = wxeReturn(memenv, Ecmd.caller, true);
  rt.send(  rt.make_ref(app->getRef((void *)Result,memenv), "wxFindReplaceData"));

}

// wxFlexGridSizer::wxFlexGridSizer
void wxFlexGridSizer_new_3_0(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd)
{
  ErlNifEnv *env = Ecmd.env;
  ERL_NIF_TERM * argv = Ecmd.args;
  int cols;
  if(!enif_get_int(env, argv[0], &cols)) Badarg("cols"); // int
  int vgap;
  if(!enif_get_int(env, argv[1], &vgap)) Badarg("vgap"); // int
  int hgap;
  if(!enif_get_int(env, argv[2], &hgap)) Badarg("hgap"); // int
  wxFlexGridSizer * Result = new EwxFlexGridSizer(cols,vgap,hgap);
  app->newPtr((void *) Result, 1, memenv);
  wxeReturn rt = wxeReturn(memenv, Ecmd.caller, true);
  rt.send(  rt.make_ref(app->getRef((void *)Result,memenv), "wxFlexGridSizer"));

}

// wxFlexGridSizer::wxFlexGridSizer
void wxFlexGridSizer_new_2(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd)
{
  wxSize gap= wxSize(0, 0);
  ErlNifEnv *env = Ecmd.env;
  ERL_NIF_TERM * argv = Ecmd.args;
  int cols;
  if(!enif_get_int(env, argv[0], &cols)) Badarg("cols"); // int
  ERL_NIF_TERM lstHead, lstTail;
  lstTail = argv[1];
  if(!enif_is_list(env, lstTail)) Badarg("Options");
  const ERL_NIF_TERM *tpl;
  int tpl_sz;
  while(!enif_is_empty_list(env, lstTail)) {
    if(!enif_get_list_cell(env, lstTail, &lstHead, &lstTail)) Badarg("Options");
    if(!enif_get_tuple(env, lstHead, &tpl_sz, &tpl) || tpl_sz != 2) Badarg("Options");
    if(enif_is_identical(tpl[0], enif_make_atom(env, "gap"))) {
  const ERL_NIF_TERM *gap_t;
  int gap_sz;
  if(!enif_get_tuple(env, tpl[1], &gap_sz, &gap_t)) Badarg("gap");
  int gapW;
  if(!enif_get_int(env, gap_t[0], &gapW)) Badarg("gap");
  int gapH;
  if(!enif_get_int(env, gap_t[1], &gapH)) Badarg("gap");
  gap = wxSize(gapW,gapH);
    } else        Badarg("Options");
  };
  wxFlexGridSizer * Result = new EwxFlexGridSizer(cols,gap);
  app->newPtr((void *) Result, 1, memenv);
  wxeReturn rt = wxeReturn(memenv, Ecmd.caller, true);
  rt.send(  rt.make_ref(app->getRef((void *)Result,memenv), "wxFlexGridSizer"));

}

// wxFlexGridSizer::wxFlexGridSizer
void wxFlexGridSizer_new_4(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd)
{
  ErlNifEnv *env = Ecmd.env;
  ERL_NIF_TERM * argv = Ecmd.args;
  int rows;
  if(!enif_get_int(env, argv[0], &rows)) Badarg("rows"); // int
  int cols;
  if(!enif_get_int(env, argv[1], &cols)) Badarg("cols"); // int
  int vgap;
  if(!enif_get_int(env, argv[2], &vgap)) Badarg("vgap"); // int
  int hgap;
  if(!enif_get_int(env, argv[3], &hgap)) Badarg("hgap"); // int
  wxFlexGridSizer * Result = new EwxFlexGridSizer(rows,cols,vgap,hgap);
  app->newPtr((void *) Result, 1, memenv);
  wxeReturn rt = wxeReturn(memenv, Ecmd.caller, true);
  rt.send(  rt.make_ref(app->getRef((void *)Result,memenv), "wxFlexGridSizer"));

}

// wxFlexGridSizer::wxFlexGridSizer
void wxFlexGridSizer_new_3_1(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd)
{
  ErlNifEnv *env = Ecmd.env;
  ERL_NIF_TERM * argv = Ecmd.args;
  int rows;
  if(!enif_get_int(env, argv[0], &rows)) Badarg("rows"); // int
  int cols;
  if(!enif_get_int(env, argv[1], &cols)) Badarg("cols"); // int
  const ERL_NIF_TERM *gap_t;
  int gap_sz;
  if(!enif_get_tuple(env, argv[2], &gap_sz, &gap_t)) Badarg("gap");
  int gapW;
  if(!enif_get_int(env, gap_t[0], &gapW)) Badarg("gap");
  int gapH;
  if(!enif_get_int(env, gap_t[1], &gapH)) Badarg("gap");
  wxSize gap = wxSize(gapW,gapH);
  wxFlexGridSizer * Result = new EwxFlexGridSizer(rows,cols,gap);
  app->newPtr((void *) Result, 1, memenv);
  wxeReturn rt = wxeReturn(memenv, Ecmd.caller, true);
  rt.send(  rt.make_ref(app->getRef((void *)Result,memenv), "wxFlexGridSizer"));

}

// wxFlexGridSizer::AddGrowableCol
void wxFlexGridSizer_AddGrowableCol(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd)
{
  int proportion=0;
  ErlNifEnv *env = Ecmd.env;
  ERL_NIF_TERM * argv = Ecmd.args;
  wxFlexGridSizer *This;
  This = (wxFlexGridSizer *) memenv->getPtr(env, argv[0], "This");
  size_t idx;
  if(!wxe_get_size_t(env, argv[1], &idx)) Badarg("idx");
  ERL_NIF_TERM lstHead, lstTail;
  lstTail = argv[2];
  if(!enif_is_list(env, lstTail)) Badarg("Options");
  const ERL_NIF_TERM *tpl;
  int tpl_sz;
  while(!enif_is_empty_list(env, lstTail)) {
    if(!enif_get_list_cell(env, lstTail, &lstHead, &lstTail)) Badarg("Options");
    if(!enif_get_tuple(env, lstHead, &tpl_sz, &tpl) || tpl_sz != 2) Badarg("Options");
    if(enif_is_identical(tpl[0], enif_make_atom(env, "proportion"))) {
  if(!enif_get_int(env, tpl[1], &proportion)) Badarg("proportion"); // int
    } else        Badarg("Options");
  };
  if(!This) throw wxe_badarg("This");
  This->AddGrowableCol(idx,proportion);

}

// wxFlexGridSizer::AddGrowableRow
void wxFlexGridSizer_AddGrowableRow(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd)
{
  int proportion=0;
  ErlNifEnv *env = Ecmd.env;
  ERL_NIF_TERM * argv = Ecmd.args;
  wxFlexGridSizer *This;
  This = (wxFlexGridSizer *) memenv->getPtr(env, argv[0], "This");
  size_t idx;
  if(!wxe_get_size_t(env, argv[1], &idx)) Badarg("idx");
  ERL_NIF_TERM lstHead, lstTail;
  lstTail = argv[2];
  if(!enif_is_list(env, lstTail)) Badarg("Options");
  const ERL_NIF_TERM *tpl;
  int tpl_sz;
  while(!enif_is_empty_list(env, lstTail)) {
    if(!enif_get_list_cell(env, lstTail, &lstHead, &lstTail)) Badarg("Options");
    if(!enif_get_tuple(env, lstHead, &tpl_sz, &tpl) || tpl_sz != 2) Badarg("Options");
    if(enif_is_identical(tpl[0], enif_make_atom(env, "proportion"))) {
  if(!enif_get_int(env, tpl[1], &proportion)) Badarg("proportion"); // int
    } else        Badarg("Options");
  };
  if(!This) throw wxe_badarg("This");
  This->AddGrowableRow(idx,proportion);

}

// wxFlexGridSizer::GetFlexibleDirection
void wxFlexGridSizer_GetFlexibleDirection(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd)
{
  ErlNifEnv *env = Ecmd.env;
  ERL_NIF_TERM * argv = Ecmd.args;
  wxFlexGridSizer *This;
  This = (wxFlexGridSizer *) memenv->getPtr(env, argv[0], "This");
  if(!This) throw wxe_badarg("This");
  int Result = This->GetFlexibleDirection();
  wxeReturn rt = wxeReturn(memenv, Ecmd.caller, true);
  rt.send(  rt.make_int(Result));

}

// wxFlexGridSizer::GetNonFlexibleGrowMode
void wxFlexGridSizer_GetNonFlexibleGrowMode(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd)
{
  ErlNifEnv *env = Ecmd.env;
  ERL_NIF_TERM * argv = Ecmd.args;
  wxFlexGridSizer *This;
  This = (wxFlexGridSizer *) memenv->getPtr(env, argv[0], "This");
  if(!This) throw wxe_badarg("This");
  int Result = This->GetNonFlexibleGrowMode();
  wxeReturn rt = wxeReturn(memenv, Ecmd.caller, true);
  rt.send(  rt.make_int(Result));

}

// wxFlexGridSizer::RemoveGrowableCol
void wxFlexGridSizer_RemoveGrowableCol(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd)
{
  ErlNifEnv *env = Ecmd.env;
  ERL_NIF_TERM * argv = Ecmd.args;
  wxFlexGridSizer *This;
  This = (wxFlexGridSizer *) memenv->getPtr(env, argv[0], "This");
  size_t idx;
  if(!wxe_get_size_t(env, argv[1], &idx)) Badarg("idx");
  if(!This) throw wxe_badarg("This");
  This->RemoveGrowableCol(idx);

}

// wxFlexGridSizer::RemoveGrowableRow
void wxFlexGridSizer_RemoveGrowableRow(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd)
{
  ErlNifEnv *env = Ecmd.env;
  ERL_NIF_TERM * argv = Ecmd.args;
  wxFlexGridSizer *This;
  This = (wxFlexGridSizer *) memenv->getPtr(env, argv[0], "This");
  size_t idx;
  if(!wxe_get_size_t(env, argv[1], &idx)) Badarg("idx");
  if(!This) throw wxe_badarg("This");
  This->RemoveGrowableRow(idx);

}

// wxFlexGridSizer::SetFlexibleDirection
void wxFlexGridSizer_SetFlexibleDirection(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd)
{
  ErlNifEnv *env = Ecmd.env;
  ERL_NIF_TERM * argv = Ecmd.args;
  wxFlexGridSizer *This;
  This = (wxFlexGridSizer *) memenv->getPtr(env, argv[0], "This");
  int direction;
  if(!enif_get_int(env, argv[1], &direction)) Badarg("direction"); // int
  if(!This) throw wxe_badarg("This");
  This->SetFlexibleDirection(direction);

}

// wxFlexGridSizer::SetNonFlexibleGrowMode
void wxFlexGridSizer_SetNonFlexibleGrowMode(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd)
{
  ErlNifEnv *env = Ecmd.env;
  ERL_NIF_TERM * argv = Ecmd.args;
  wxFlexGridSizer *This;
  This = (wxFlexGridSizer *) memenv->getPtr(env, argv[0], "This");
  wxFlexSizerGrowMode mode;
  if(!enif_get_int(env, argv[1], (int *) &mode)) Badarg("mode"); // enum
  if(!This) throw wxe_badarg("This");
  This->SetNonFlexibleGrowMode(mode);

}

// wxFocusEvent::GetWindow
void wxFocusEvent_GetWindow(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd)
{
  ErlNifEnv *env = Ecmd.env;
  ERL_NIF_TERM * argv = Ecmd.args;
  wxFocusEvent *This;
  This = (wxFocusEvent *) memenv->getPtr(env, argv[0], "This");
  if(!This) throw wxe_badarg("This");
  wxWindow * Result = (wxWindow*)This->GetWindow();
  wxeReturn rt = wxeReturn(memenv, Ecmd.caller, true);
  rt.send(  rt.make_ref(app->getRef((void *)Result,memenv), "wxWindow"));

}

// wxFont::wxFont
void wxFont_new_0(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd)
{
  wxFont * Result = new EwxFont();
  app->newPtr((void *) Result, 1, memenv);
  wxeReturn rt = wxeReturn(memenv, Ecmd.caller, true);
  rt.send(  rt.make_ref(app->getRef((void *)Result,memenv), "wxFont"));

}

// wxFont::wxFont
void wxFont_new_1_1(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd)
{
  ErlNifEnv *env = Ecmd.env;
  ERL_NIF_TERM * argv = Ecmd.args;
  wxFont *font;
  font = (wxFont *) memenv->getPtr(env, argv[0], "font");
  wxFont * Result = new EwxFont(*font);
  app->newPtr((void *) Result, 1, memenv);
  wxeReturn rt = wxeReturn(memenv, Ecmd.caller, true);
  rt.send(  rt.make_ref(app->getRef((void *)Result,memenv), "wxFont"));

}

// wxFont::wxFont
void wxFont_new_5_0(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd)
{
  bool underlined=false;
  wxString face= wxEmptyString;
 wxFontEncoding encoding=wxFONTENCODING_DEFAULT;
  ErlNifEnv *env = Ecmd.env;
  ERL_NIF_TERM * argv = Ecmd.args;
  int pointSize;
  if(!enif_get_int(env, argv[0], &pointSize)) Badarg("pointSize"); // int
  wxFontFamily family;
  if(!enif_get_int(env, argv[1], (int *) &family)) Badarg("family"); // enum
  wxFontStyle style;
  if(!enif_get_int(env, argv[2], (int *) &style)) Badarg("style"); // enum
  wxFontWeight weight;
  if(!enif_get_int(env, argv[3], (int *) &weight)) Badarg("weight"); // enum
  ERL_NIF_TERM lstHead, lstTail;
  lstTail = argv[4];
  if(!enif_is_list(env, lstTail)) Badarg("Options");
  const ERL_NIF_TERM *tpl;
  int tpl_sz;
  while(!enif_is_empty_list(env, lstTail)) {
    if(!enif_get_list_cell(env, lstTail, &lstHead, &lstTail)) Badarg("Options");
    if(!enif_get_tuple(env, lstHead, &tpl_sz, &tpl) || tpl_sz != 2) Badarg("Options");
    if(enif_is_identical(tpl[0], enif_make_atom(env, "underlined"))) {
  underlined = enif_is_identical(tpl[1], WXE_ATOM_true);
    } else     if(enif_is_identical(tpl[0], enif_make_atom(env, "face"))) {
  ErlNifBinary face_bin;
  if(!enif_inspect_binary(env, tpl[1], &face_bin)) Badarg("face");
  face = wxString(face_bin.data, wxConvUTF8, face_bin.size);
    } else     if(enif_is_identical(tpl[0], enif_make_atom(env, "encoding"))) {
  if(!enif_get_int(env, tpl[1], (int *) &encoding)) Badarg("encoding"); // enum
    } else        Badarg("Options");
  };
  wxFont * Result = new EwxFont(pointSize,family,style,weight,underlined,face,encoding);
  app->newPtr((void *) Result, 1, memenv);
  wxeReturn rt = wxeReturn(memenv, Ecmd.caller, true);
  rt.send(  rt.make_ref(app->getRef((void *)Result,memenv), "wxFont"));

}

// wxFont::wxFont
void wxFont_new_5_1(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd)
{
  bool underline=false;
  wxString faceName= wxEmptyString;
 wxFontEncoding encoding=wxFONTENCODING_DEFAULT;
  ErlNifEnv *env = Ecmd.env;
  ERL_NIF_TERM * argv = Ecmd.args;
  const ERL_NIF_TERM *pixelSize_t;
  int pixelSize_sz;
  if(!enif_get_tuple(env, argv[0], &pixelSize_sz, &pixelSize_t)) Badarg("pixelSize");
  int pixelSizeW;
  if(!enif_get_int(env, pixelSize_t[0], &pixelSizeW)) Badarg("pixelSize");
  int pixelSizeH;
  if(!enif_get_int(env, pixelSize_t[1], &pixelSizeH)) Badarg("pixelSize");
  wxSize pixelSize = wxSize(pixelSizeW,pixelSizeH);
  wxFontFamily family;
  if(!enif_get_int(env, argv[1], (int *) &family)) Badarg("family"); // enum
  wxFontStyle style;
  if(!enif_get_int(env, argv[2], (int *) &style)) Badarg("style"); // enum
  wxFontWeight weight;
  if(!enif_get_int(env, argv[3], (int *) &weight)) Badarg("weight"); // enum
  ERL_NIF_TERM lstHead, lstTail;
  lstTail = argv[4];
  if(!enif_is_list(env, lstTail)) Badarg("Options");
  const ERL_NIF_TERM *tpl;
  int tpl_sz;
  while(!enif_is_empty_list(env, lstTail)) {
    if(!enif_get_list_cell(env, lstTail, &lstHead, &lstTail)) Badarg("Options");
    if(!enif_get_tuple(env, lstHead, &tpl_sz, &tpl) || tpl_sz != 2) Badarg("Options");
    if(enif_is_identical(tpl[0], enif_make_atom(env, "underline"))) {
  underline = enif_is_identical(tpl[1], WXE_ATOM_true);
    } else     if(enif_is_identical(tpl[0], enif_make_atom(env, "faceName"))) {
  ErlNifBinary faceName_bin;
  if(!enif_inspect_binary(env, tpl[1], &faceName_bin)) Badarg("faceName");
  faceName = wxString(faceName_bin.data, wxConvUTF8, faceName_bin.size);
    } else     if(enif_is_identical(tpl[0], enif_make_atom(env, "encoding"))) {
  if(!enif_get_int(env, tpl[1], (int *) &encoding)) Badarg("encoding"); // enum
    } else        Badarg("Options");
  };
  wxFont * Result = new EwxFont(pixelSize,family,style,weight,underline,faceName,encoding);
  app->newPtr((void *) Result, 1, memenv);
  wxeReturn rt = wxeReturn(memenv, Ecmd.caller, true);
  rt.send(  rt.make_ref(app->getRef((void *)Result,memenv), "wxFont"));

}

// wxFont::wxFont
void wxFont_new_1_0(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd)
{
  ErlNifEnv *env = Ecmd.env;
  ERL_NIF_TERM * argv = Ecmd.args;
  ErlNifBinary nativeInfoString_bin;
  wxString nativeInfoString;
  if(!enif_inspect_binary(env, argv[0], &nativeInfoString_bin)) Badarg("nativeInfoString");
  nativeInfoString = wxString(nativeInfoString_bin.data, wxConvUTF8, nativeInfoString_bin.size);
  wxFont * Result = new EwxFont(nativeInfoString);
  app->newPtr((void *) Result, 1, memenv);
  wxeReturn rt = wxeReturn(memenv, Ecmd.caller, true);
  rt.send(  rt.make_ref(app->getRef((void *)Result,memenv), "wxFont"));

}

// wxFont::IsFixedWidth
void wxFont_IsFixedWidth(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd)
{
  ErlNifEnv *env = Ecmd.env;
  ERL_NIF_TERM * argv = Ecmd.args;
  wxFont *This;
  This = (wxFont *) memenv->getPtr(env, argv[0], "This");
  if(!This) throw wxe_badarg("This");
  bool Result = This->IsFixedWidth();
  wxeReturn rt = wxeReturn(memenv, Ecmd.caller, true);
  rt.send(  rt.make_bool(Result));

}

// wxFont::GetDefaultEncoding
void wxFont_GetDefaultEncoding(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd)
{
  int Result = wxFont::GetDefaultEncoding();
  wxeReturn rt = wxeReturn(memenv, Ecmd.caller, true);
  rt.send(  rt.make_int(Result));

}

// wxFont::GetFaceName
void wxFont_GetFaceName(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd)
{
  ErlNifEnv *env = Ecmd.env;
  ERL_NIF_TERM * argv = Ecmd.args;
  wxFont *This;
  This = (wxFont *) memenv->getPtr(env, argv[0], "This");
  if(!This) throw wxe_badarg("This");
  wxString Result = This->GetFaceName();
  wxeReturn rt = wxeReturn(memenv, Ecmd.caller, true);
  rt.send(  rt.make(Result));

}

// wxFont::GetFamily
void wxFont_GetFamily(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd)
{
  ErlNifEnv *env = Ecmd.env;
  ERL_NIF_TERM * argv = Ecmd.args;
  wxFont *This;
  This = (wxFont *) memenv->getPtr(env, argv[0], "This");
  if(!This) throw wxe_badarg("This");
  int Result = This->GetFamily();
  wxeReturn rt = wxeReturn(memenv, Ecmd.caller, true);
  rt.send(  rt.make_int(Result));

}

// wxFont::GetNativeFontInfoDesc
void wxFont_GetNativeFontInfoDesc(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd)
{
  ErlNifEnv *env = Ecmd.env;
  ERL_NIF_TERM * argv = Ecmd.args;
  wxFont *This;
  This = (wxFont *) memenv->getPtr(env, argv[0], "This");
  if(!This) throw wxe_badarg("This");
  wxString Result = This->GetNativeFontInfoDesc();
  wxeReturn rt = wxeReturn(memenv, Ecmd.caller, true);
  rt.send(  rt.make(Result));

}

// wxFont::GetNativeFontInfoUserDesc
void wxFont_GetNativeFontInfoUserDesc(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd)
{
  ErlNifEnv *env = Ecmd.env;
  ERL_NIF_TERM * argv = Ecmd.args;
  wxFont *This;
  This = (wxFont *) memenv->getPtr(env, argv[0], "This");
  if(!This) throw wxe_badarg("This");
  wxString Result = This->GetNativeFontInfoUserDesc();
  wxeReturn rt = wxeReturn(memenv, Ecmd.caller, true);
  rt.send(  rt.make(Result));

}

// wxFont::GetPointSize
void wxFont_GetPointSize(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd)
{
  ErlNifEnv *env = Ecmd.env;
  ERL_NIF_TERM * argv = Ecmd.args;
  wxFont *This;
  This = (wxFont *) memenv->getPtr(env, argv[0], "This");
  if(!This) throw wxe_badarg("This");
  int Result = This->GetPointSize();
  wxeReturn rt = wxeReturn(memenv, Ecmd.caller, true);
  rt.send(  rt.make_int(Result));

}

// wxFont::GetStyle
void wxFont_GetStyle(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd)
{
  ErlNifEnv *env = Ecmd.env;
  ERL_NIF_TERM * argv = Ecmd.args;
  wxFont *This;
  This = (wxFont *) memenv->getPtr(env, argv[0], "This");
  if(!This) throw wxe_badarg("This");
  int Result = This->GetStyle();
  wxeReturn rt = wxeReturn(memenv, Ecmd.caller, true);
  rt.send(  rt.make_int(Result));

}

// wxFont::GetUnderlined
void wxFont_GetUnderlined(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd)
{
  ErlNifEnv *env = Ecmd.env;
  ERL_NIF_TERM * argv = Ecmd.args;
  wxFont *This;
  This = (wxFont *) memenv->getPtr(env, argv[0], "This");
  if(!This) throw wxe_badarg("This");
  bool Result = This->GetUnderlined();
  wxeReturn rt = wxeReturn(memenv, Ecmd.caller, true);
  rt.send(  rt.make_bool(Result));

}

// wxFont::GetWeight
void wxFont_GetWeight(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd)
{
  ErlNifEnv *env = Ecmd.env;
  ERL_NIF_TERM * argv = Ecmd.args;
  wxFont *This;
  This = (wxFont *) memenv->getPtr(env, argv[0], "This");
  if(!This) throw wxe_badarg("This");
  int Result = This->GetWeight();
  wxeReturn rt = wxeReturn(memenv, Ecmd.caller, true);
  rt.send(  rt.make_int(Result));

}

// wxFont::IsOk
void wxFont_IsOk(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd)
{
  ErlNifEnv *env = Ecmd.env;
  ERL_NIF_TERM * argv = Ecmd.args;
  wxFont *This;
  This = (wxFont *) memenv->getPtr(env, argv[0], "This");
  if(!This) throw wxe_badarg("This");
  bool Result = This->IsOk();
  wxeReturn rt = wxeReturn(memenv, Ecmd.caller, true);
  rt.send(  rt.make_bool(Result));

}

// wxFont::SetDefaultEncoding
void wxFont_SetDefaultEncoding(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd)
{
  ErlNifEnv *env = Ecmd.env;
  ERL_NIF_TERM * argv = Ecmd.args;
  wxFontEncoding encoding;
  if(!enif_get_int(env, argv[0], (int *) &encoding)) Badarg("encoding"); // enum
  wxFont::SetDefaultEncoding(encoding);

}

// wxFont::SetFaceName
void wxFont_SetFaceName(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd)
{
  ErlNifEnv *env = Ecmd.env;
  ERL_NIF_TERM * argv = Ecmd.args;
  wxFont *This;
  This = (wxFont *) memenv->getPtr(env, argv[0], "This");
  ErlNifBinary faceName_bin;
  wxString faceName;
  if(!enif_inspect_binary(env, argv[1], &faceName_bin)) Badarg("faceName");
  faceName = wxString(faceName_bin.data, wxConvUTF8, faceName_bin.size);
  if(!This) throw wxe_badarg("This");
  bool Result = This->SetFaceName(faceName);
  wxeReturn rt = wxeReturn(memenv, Ecmd.caller, true);
  rt.send(  rt.make_bool(Result));

}

// wxFont::SetFamily
void wxFont_SetFamily(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd)
{
  ErlNifEnv *env = Ecmd.env;
  ERL_NIF_TERM * argv = Ecmd.args;
  wxFont *This;
  This = (wxFont *) memenv->getPtr(env, argv[0], "This");
  wxFontFamily family;
  if(!enif_get_int(env, argv[1], (int *) &family)) Badarg("family"); // enum
  if(!This) throw wxe_badarg("This");
  This->SetFamily(family);

}

// wxFont::SetPointSize
void wxFont_SetPointSize(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd)
{
  ErlNifEnv *env = Ecmd.env;
  ERL_NIF_TERM * argv = Ecmd.args;
  wxFont *This;
  This = (wxFont *) memenv->getPtr(env, argv[0], "This");
  int pointSize;
  if(!enif_get_int(env, argv[1], &pointSize)) Badarg("pointSize"); // int
  if(!This) throw wxe_badarg("This");
  This->SetPointSize(pointSize);

}

// wxFont::SetStyle
void wxFont_SetStyle(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd)
{
  ErlNifEnv *env = Ecmd.env;
  ERL_NIF_TERM * argv = Ecmd.args;
  wxFont *This;
  This = (wxFont *) memenv->getPtr(env, argv[0], "This");
  wxFontStyle style;
  if(!enif_get_int(env, argv[1], (int *) &style)) Badarg("style"); // enum
  if(!This) throw wxe_badarg("This");
  This->SetStyle(style);

}

// wxFont::SetUnderlined
void wxFont_SetUnderlined(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd)
{
  ErlNifEnv *env = Ecmd.env;
  ERL_NIF_TERM * argv = Ecmd.args;
  wxFont *This;
  This = (wxFont *) memenv->getPtr(env, argv[0], "This");
  bool underlined;
  underlined = enif_is_identical(argv[1], WXE_ATOM_true);
  if(!This) throw wxe_badarg("This");
  This->SetUnderlined(underlined);

}

// wxFont::SetWeight
void wxFont_SetWeight(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd)
{
  ErlNifEnv *env = Ecmd.env;
  ERL_NIF_TERM * argv = Ecmd.args;
  wxFont *This;
  This = (wxFont *) memenv->getPtr(env, argv[0], "This");
  wxFontWeight weight;
  if(!enif_get_int(env, argv[1], (int *) &weight)) Badarg("weight"); // enum
  if(!This) throw wxe_badarg("This");
  This->SetWeight(weight);

}

// wxFontData::wxFontData
void wxFontData_new_0(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd)
{
  wxFontData * Result = new EwxFontData();
  app->newPtr((void *) Result, 1, memenv);
  wxeReturn rt = wxeReturn(memenv, Ecmd.caller, true);
  rt.send(  rt.make_ref(app->getRef((void *)Result,memenv), "wxFontData"));

}

// wxFontData::wxFontData
void wxFontData_new_1(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd)
{
  ErlNifEnv *env = Ecmd.env;
  ERL_NIF_TERM * argv = Ecmd.args;
  wxFontData *data;
  data = (wxFontData *) memenv->getPtr(env, argv[0], "data");
  wxFontData * Result = new EwxFontData(*data);
  app->newPtr((void *) Result, 1, memenv);
  wxeReturn rt = wxeReturn(memenv, Ecmd.caller, true);
  rt.send(  rt.make_ref(app->getRef((void *)Result,memenv), "wxFontData"));

}

// wxFontData::EnableEffects
void wxFontData_EnableEffects(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd)
{
  ErlNifEnv *env = Ecmd.env;
  ERL_NIF_TERM * argv = Ecmd.args;
  wxFontData *This;
  This = (wxFontData *) memenv->getPtr(env, argv[0], "This");
  bool enable;
  enable = enif_is_identical(argv[1], WXE_ATOM_true);
  if(!This) throw wxe_badarg("This");
  This->EnableEffects(enable);

}

// wxFontData::GetAllowSymbols
void wxFontData_GetAllowSymbols(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd)
{
  ErlNifEnv *env = Ecmd.env;
  ERL_NIF_TERM * argv = Ecmd.args;
  wxFontData *This;
  This = (wxFontData *) memenv->getPtr(env, argv[0], "This");
  if(!This) throw wxe_badarg("This");
  bool Result = This->GetAllowSymbols();
  wxeReturn rt = wxeReturn(memenv, Ecmd.caller, true);
  rt.send(  rt.make_bool(Result));

}

// wxFontData::GetColour
void wxFontData_GetColour(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd)
{
  ErlNifEnv *env = Ecmd.env;
  ERL_NIF_TERM * argv = Ecmd.args;
  wxFontData *This;
  This = (wxFontData *) memenv->getPtr(env, argv[0], "This");
  if(!This) throw wxe_badarg("This");
  const wxColour * Result = &This->GetColour();
  wxeReturn rt = wxeReturn(memenv, Ecmd.caller, true);
  rt.send(  rt.make((*Result)));

}

// wxFontData::GetChosenFont
void wxFontData_GetChosenFont(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd)
{
  ErlNifEnv *env = Ecmd.env;
  ERL_NIF_TERM * argv = Ecmd.args;
  wxFontData *This;
  This = (wxFontData *) memenv->getPtr(env, argv[0], "This");
  if(!This) throw wxe_badarg("This");
  wxFont * Result = new wxFont(This->GetChosenFont()); app->newPtr((void *) Result,3, memenv);;
  wxeReturn rt = wxeReturn(memenv, Ecmd.caller, true);
  rt.send(  rt.make_ref(app->getRef((void *)Result,memenv), "wxFont"));

}

// wxFontData::GetEnableEffects
void wxFontData_GetEnableEffects(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd)
{
  ErlNifEnv *env = Ecmd.env;
  ERL_NIF_TERM * argv = Ecmd.args;
  wxFontData *This;
  This = (wxFontData *) memenv->getPtr(env, argv[0], "This");
  if(!This) throw wxe_badarg("This");
  bool Result = This->GetEnableEffects();
  wxeReturn rt = wxeReturn(memenv, Ecmd.caller, true);
  rt.send(  rt.make_bool(Result));

}

// wxFontData::GetInitialFont
void wxFontData_GetInitialFont(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd)
{
  ErlNifEnv *env = Ecmd.env;
  ERL_NIF_TERM * argv = Ecmd.args;
  wxFontData *This;
  This = (wxFontData *) memenv->getPtr(env, argv[0], "This");
  if(!This) throw wxe_badarg("This");
  wxFont * Result = new wxFont(This->GetInitialFont()); app->newPtr((void *) Result,3, memenv);;
  wxeReturn rt = wxeReturn(memenv, Ecmd.caller, true);
  rt.send(  rt.make_ref(app->getRef((void *)Result,memenv), "wxFont"));

}

// wxFontData::GetShowHelp
void wxFontData_GetShowHelp(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd)
{
  ErlNifEnv *env = Ecmd.env;
  ERL_NIF_TERM * argv = Ecmd.args;
  wxFontData *This;
  This = (wxFontData *) memenv->getPtr(env, argv[0], "This");
  if(!This) throw wxe_badarg("This");
  bool Result = This->GetShowHelp();
  wxeReturn rt = wxeReturn(memenv, Ecmd.caller, true);
  rt.send(  rt.make_bool(Result));

}

// wxFontData::SetAllowSymbols
void wxFontData_SetAllowSymbols(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd)
{
  ErlNifEnv *env = Ecmd.env;
  ERL_NIF_TERM * argv = Ecmd.args;
  wxFontData *This;
  This = (wxFontData *) memenv->getPtr(env, argv[0], "This");
  bool allowSymbols;
  allowSymbols = enif_is_identical(argv[1], WXE_ATOM_true);
  if(!This) throw wxe_badarg("This");
  This->SetAllowSymbols(allowSymbols);

}

// wxFontData::SetChosenFont
void wxFontData_SetChosenFont(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd)
{
  ErlNifEnv *env = Ecmd.env;
  ERL_NIF_TERM * argv = Ecmd.args;
  wxFontData *This;
  This = (wxFontData *) memenv->getPtr(env, argv[0], "This");
  wxFont *font;
  font = (wxFont *) memenv->getPtr(env, argv[1], "font");
  if(!This) throw wxe_badarg("This");
  This->SetChosenFont(*font);

}

// wxFontData::SetColour
void wxFontData_SetColour(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd)
{
  ErlNifEnv *env = Ecmd.env;
  ERL_NIF_TERM * argv = Ecmd.args;
  wxFontData *This;
  This = (wxFontData *) memenv->getPtr(env, argv[0], "This");
  const ERL_NIF_TERM *colour_t;
  int colour_sz;
  if(!enif_get_tuple(env, argv[1], &colour_sz, &colour_t)) Badarg("colour");
  int colourR;
  if(!enif_get_int(env, colour_t[0], &colourR)) Badarg("colour");
  int colourG;
  if(!enif_get_int(env, colour_t[1], &colourG)) Badarg("colour");
  int colourB;
  if(!enif_get_int(env, colour_t[2], &colourB)) Badarg("colour");
  int colourA;
  if(!enif_get_int(env, colour_t[3], &colourA)) Badarg("colour");
  wxColour colour = wxColour(colourR,colourG,colourB,colourA);
  if(!This) throw wxe_badarg("This");
  This->SetColour(colour);

}

// wxFontData::SetInitialFont
void wxFontData_SetInitialFont(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd)
{
  ErlNifEnv *env = Ecmd.env;
  ERL_NIF_TERM * argv = Ecmd.args;
  wxFontData *This;
  This = (wxFontData *) memenv->getPtr(env, argv[0], "This");
  wxFont *font;
  font = (wxFont *) memenv->getPtr(env, argv[1], "font");
  if(!This) throw wxe_badarg("This");
  This->SetInitialFont(*font);

}

// wxFontData::SetRange
void wxFontData_SetRange(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd)
{
  ErlNifEnv *env = Ecmd.env;
  ERL_NIF_TERM * argv = Ecmd.args;
  wxFontData *This;
  This = (wxFontData *) memenv->getPtr(env, argv[0], "This");
  int min;
  if(!enif_get_int(env, argv[1], &min)) Badarg("min"); // int
  int max;
  if(!enif_get_int(env, argv[2], &max)) Badarg("max"); // int
  if(!This) throw wxe_badarg("This");
  This->SetRange(min,max);

}

// wxFontData::SetShowHelp
void wxFontData_SetShowHelp(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd)
{
  ErlNifEnv *env = Ecmd.env;
  ERL_NIF_TERM * argv = Ecmd.args;
  wxFontData *This;
  This = (wxFontData *) memenv->getPtr(env, argv[0], "This");
  bool showHelp;
  showHelp = enif_is_identical(argv[1], WXE_ATOM_true);
  if(!This) throw wxe_badarg("This");
  This->SetShowHelp(showHelp);

}

// wxFontDialog::wxFontDialog
void wxFontDialog_new_0(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd)
{
  wxFontDialog * Result = new EwxFontDialog();
  app->newPtr((void *) Result, 2, memenv);
  wxeReturn rt = wxeReturn(memenv, Ecmd.caller, true);
  rt.send(  rt.make_ref(app->getRef((void *)Result,memenv), "wxFontDialog"));

}

// wxFontDialog::wxFontDialog
void wxFontDialog_new_2(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd)
{
  ErlNifEnv *env = Ecmd.env;
  ERL_NIF_TERM * argv = Ecmd.args;
  wxWindow *parent;
  parent = (wxWindow *) memenv->getPtr(env, argv[0], "parent");
  wxFontData *data;
  data = (wxFontData *) memenv->getPtr(env, argv[1], "data");
  wxFontDialog * Result = new EwxFontDialog(parent,*data);
  app->newPtr((void *) Result, 2, memenv);
  wxeReturn rt = wxeReturn(memenv, Ecmd.caller, true);
  rt.send(  rt.make_ref(app->getRef((void *)Result,memenv), "wxFontDialog"));

}

// wxFontDialog::Create
void wxFontDialog_Create(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd)
{
  ErlNifEnv *env = Ecmd.env;
  ERL_NIF_TERM * argv = Ecmd.args;
  wxFontDialog *This;
  This = (wxFontDialog *) memenv->getPtr(env, argv[0], "This");
  wxWindow *parent;
  parent = (wxWindow *) memenv->getPtr(env, argv[1], "parent");
  wxFontData *data;
  data = (wxFontData *) memenv->getPtr(env, argv[2], "data");
  if(!This) throw wxe_badarg("This");
  bool Result = This->Create(parent,*data);
  wxeReturn rt = wxeReturn(memenv, Ecmd.caller, true);
  rt.send(  rt.make_bool(Result));

}

// wxFontDialog::GetFontData
void wxFontDialog_GetFontData(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd)
{
  ErlNifEnv *env = Ecmd.env;
  ERL_NIF_TERM * argv = Ecmd.args;
  wxFontDialog *This;
  This = (wxFontDialog *) memenv->getPtr(env, argv[0], "This");
  if(!This) throw wxe_badarg("This");
  const wxFontData * Result = &This->GetFontData();
  wxeReturn rt = wxeReturn(memenv, Ecmd.caller, true);
  rt.send(  rt.make_ref(app->getRef((void *)Result,memenv), "wxFontData"));

}

// wxFontPickerCtrl::wxFontPickerCtrl
void wxFontPickerCtrl_new_0(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd)
{
  wxFontPickerCtrl * Result = new EwxFontPickerCtrl();
  app->newPtr((void *) Result, 0, memenv);
  wxeReturn rt = wxeReturn(memenv, Ecmd.caller, true);
  rt.send(  rt.make_ref(app->getRef((void *)Result,memenv), "wxFontPickerCtrl"));

}

// wxFontPickerCtrl::wxFontPickerCtrl
void wxFontPickerCtrl_new_3(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd)
{
  const wxFont * initial= &wxNullFont;
  wxPoint pos= wxDefaultPosition;
  wxSize size= wxDefaultSize;
  long style=wxFNTP_DEFAULT_STYLE;
  const wxValidator * validator= &wxDefaultValidator;
  ErlNifEnv *env = Ecmd.env;
  ERL_NIF_TERM * argv = Ecmd.args;
  wxWindow *parent;
  parent = (wxWindow *) memenv->getPtr(env, argv[0], "parent");
  int id;
  if(!enif_get_int(env, argv[1], &id)) Badarg("id"); // wxWindowID
  ERL_NIF_TERM lstHead, lstTail;
  lstTail = argv[2];
  if(!enif_is_list(env, lstTail)) Badarg("Options");
  const ERL_NIF_TERM *tpl;
  int tpl_sz;
  while(!enif_is_empty_list(env, lstTail)) {
    if(!enif_get_list_cell(env, lstTail, &lstHead, &lstTail)) Badarg("Options");
    if(!enif_get_tuple(env, lstHead, &tpl_sz, &tpl) || tpl_sz != 2) Badarg("Options");
    if(enif_is_identical(tpl[0], enif_make_atom(env, "initial"))) {
  initial = (wxFont *) memenv->getPtr(env, tpl[1], "initial");
    } else     if(enif_is_identical(tpl[0], enif_make_atom(env, "pos"))) {
  const ERL_NIF_TERM *pos_t;
  int pos_sz;
  if(!enif_get_tuple(env, tpl[1], &pos_sz, &pos_t)) Badarg("pos");
  int posX;
  if(!enif_get_int(env, pos_t[0], &posX)) Badarg("pos");
  int posY;
  if(!enif_get_int(env, pos_t[1], &posY)) Badarg("pos");
  pos = wxPoint(posX,posY);
    } else     if(enif_is_identical(tpl[0], enif_make_atom(env, "size"))) {
  const ERL_NIF_TERM *size_t;
  int size_sz;
  if(!enif_get_tuple(env, tpl[1], &size_sz, &size_t)) Badarg("size");
  int sizeW;
  if(!enif_get_int(env, size_t[0], &sizeW)) Badarg("size");
  int sizeH;
  if(!enif_get_int(env, size_t[1], &sizeH)) Badarg("size");
  size = wxSize(sizeW,sizeH);
    } else     if(enif_is_identical(tpl[0], enif_make_atom(env, "style"))) {
  if(!enif_get_long(env, tpl[1], &style)) Badarg("style");
    } else     if(enif_is_identical(tpl[0], enif_make_atom(env, "validator"))) {
  validator = (wxValidator *) memenv->getPtr(env, tpl[1], "validator");
    } else        Badarg("Options");
  };
  wxFontPickerCtrl * Result = new EwxFontPickerCtrl(parent,id,*initial,pos,size,style,*validator);
  app->newPtr((void *) Result, 0, memenv);
  wxeReturn rt = wxeReturn(memenv, Ecmd.caller, true);
  rt.send(  rt.make_ref(app->getRef((void *)Result,memenv), "wxFontPickerCtrl"));

}

// wxFontPickerCtrl::Create
void wxFontPickerCtrl_Create(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd)
{
  const wxFont * initial= &wxNullFont;
  wxPoint pos= wxDefaultPosition;
  wxSize size= wxDefaultSize;
  long style=wxFNTP_DEFAULT_STYLE;
  const wxValidator * validator= &wxDefaultValidator;
  ErlNifEnv *env = Ecmd.env;
  ERL_NIF_TERM * argv = Ecmd.args;
  wxFontPickerCtrl *This;
  This = (wxFontPickerCtrl *) memenv->getPtr(env, argv[0], "This");
  wxWindow *parent;
  parent = (wxWindow *) memenv->getPtr(env, argv[1], "parent");
  int id;
  if(!enif_get_int(env, argv[2], &id)) Badarg("id"); // wxWindowID
  ERL_NIF_TERM lstHead, lstTail;
  lstTail = argv[3];
  if(!enif_is_list(env, lstTail)) Badarg("Options");
  const ERL_NIF_TERM *tpl;
  int tpl_sz;
  while(!enif_is_empty_list(env, lstTail)) {
    if(!enif_get_list_cell(env, lstTail, &lstHead, &lstTail)) Badarg("Options");
    if(!enif_get_tuple(env, lstHead, &tpl_sz, &tpl) || tpl_sz != 2) Badarg("Options");
    if(enif_is_identical(tpl[0], enif_make_atom(env, "initial"))) {
  initial = (wxFont *) memenv->getPtr(env, tpl[1], "initial");
    } else     if(enif_is_identical(tpl[0], enif_make_atom(env, "pos"))) {
  const ERL_NIF_TERM *pos_t;
  int pos_sz;
  if(!enif_get_tuple(env, tpl[1], &pos_sz, &pos_t)) Badarg("pos");
  int posX;
  if(!enif_get_int(env, pos_t[0], &posX)) Badarg("pos");
  int posY;
  if(!enif_get_int(env, pos_t[1], &posY)) Badarg("pos");
  pos = wxPoint(posX,posY);
    } else     if(enif_is_identical(tpl[0], enif_make_atom(env, "size"))) {
  const ERL_NIF_TERM *size_t;
  int size_sz;
  if(!enif_get_tuple(env, tpl[1], &size_sz, &size_t)) Badarg("size");
  int sizeW;
  if(!enif_get_int(env, size_t[0], &sizeW)) Badarg("size");
  int sizeH;
  if(!enif_get_int(env, size_t[1], &sizeH)) Badarg("size");
  size = wxSize(sizeW,sizeH);
    } else     if(enif_is_identical(tpl[0], enif_make_atom(env, "style"))) {
  if(!enif_get_long(env, tpl[1], &style)) Badarg("style");
    } else     if(enif_is_identical(tpl[0], enif_make_atom(env, "validator"))) {
  validator = (wxValidator *) memenv->getPtr(env, tpl[1], "validator");
    } else        Badarg("Options");
  };
  if(!This) throw wxe_badarg("This");
  bool Result = This->Create(parent,id,*initial,pos,size,style,*validator);
  wxeReturn rt = wxeReturn(memenv, Ecmd.caller, true);
  rt.send(  rt.make_bool(Result));

}

// wxFontPickerCtrl::GetSelectedFont
void wxFontPickerCtrl_GetSelectedFont(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd)
{
  ErlNifEnv *env = Ecmd.env;
  ERL_NIF_TERM * argv = Ecmd.args;
  wxFontPickerCtrl *This;
  This = (wxFontPickerCtrl *) memenv->getPtr(env, argv[0], "This");
  if(!This) throw wxe_badarg("This");
  wxFont * Result = new wxFont(This->GetSelectedFont()); app->newPtr((void *) Result,3, memenv);;
  wxeReturn rt = wxeReturn(memenv, Ecmd.caller, true);
  rt.send(  rt.make_ref(app->getRef((void *)Result,memenv), "wxFont"));

}

// wxFontPickerCtrl::SetSelectedFont
void wxFontPickerCtrl_SetSelectedFont(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd)
{
  ErlNifEnv *env = Ecmd.env;
  ERL_NIF_TERM * argv = Ecmd.args;
  wxFontPickerCtrl *This;
  This = (wxFontPickerCtrl *) memenv->getPtr(env, argv[0], "This");
  wxFont *font;
  font = (wxFont *) memenv->getPtr(env, argv[1], "font");
  if(!This) throw wxe_badarg("This");
  This->SetSelectedFont(*font);

}

// wxFontPickerCtrl::GetMaxPointSize
void wxFontPickerCtrl_GetMaxPointSize(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd)
{
  ErlNifEnv *env = Ecmd.env;
  ERL_NIF_TERM * argv = Ecmd.args;
  wxFontPickerCtrl *This;
  This = (wxFontPickerCtrl *) memenv->getPtr(env, argv[0], "This");
  if(!This) throw wxe_badarg("This");
  int Result = This->GetMaxPointSize();
  wxeReturn rt = wxeReturn(memenv, Ecmd.caller, true);
  rt.send(  rt.make_uint(Result));

}

// wxFontPickerCtrl::SetMaxPointSize
void wxFontPickerCtrl_SetMaxPointSize(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd)
{
  ErlNifEnv *env = Ecmd.env;
  ERL_NIF_TERM * argv = Ecmd.args;
  wxFontPickerCtrl *This;
  This = (wxFontPickerCtrl *) memenv->getPtr(env, argv[0], "This");
  unsigned int max;
  if(!enif_get_uint(env, argv[1], &max)) Badarg("max");
  if(!This) throw wxe_badarg("This");
  This->SetMaxPointSize(max);

}

// wxFontPickerEvent::GetFont
void wxFontPickerEvent_GetFont(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd)
{
  ErlNifEnv *env = Ecmd.env;
  ERL_NIF_TERM * argv = Ecmd.args;
  wxFontPickerEvent *This;
  This = (wxFontPickerEvent *) memenv->getPtr(env, argv[0], "This");
  if(!This) throw wxe_badarg("This");
  wxFont * Result = new wxFont(This->GetFont()); app->newPtr((void *) Result,3, memenv);;
  wxeReturn rt = wxeReturn(memenv, Ecmd.caller, true);
  rt.send(  rt.make_ref(app->getRef((void *)Result,memenv), "wxFont"));

}

// wxFrame::wxFrame
void wxFrame_new_0(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd)
{
  wxFrame * Result = new EwxFrame();
  app->newPtr((void *) Result, 0, memenv);
  wxeReturn rt = wxeReturn(memenv, Ecmd.caller, true);
  rt.send(  rt.make_ref(app->getRef((void *)Result,memenv), "wxFrame"));

}

// wxFrame::wxFrame
void wxFrame_new_4(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd)
{
  wxPoint pos= wxDefaultPosition;
  wxSize size= wxDefaultSize;
  long style=wxDEFAULT_FRAME_STYLE;
  ErlNifEnv *env = Ecmd.env;
  ERL_NIF_TERM * argv = Ecmd.args;
  wxWindow *parent;
  parent = (wxWindow *) memenv->getPtr(env, argv[0], "parent");
  int id;
  if(!enif_get_int(env, argv[1], &id)) Badarg("id"); // wxWindowID
  ErlNifBinary title_bin;
  wxString title;
  if(!enif_inspect_binary(env, argv[2], &title_bin)) Badarg("title");
  title = wxString(title_bin.data, wxConvUTF8, title_bin.size);
  ERL_NIF_TERM lstHead, lstTail;
  lstTail = argv[3];
  if(!enif_is_list(env, lstTail)) Badarg("Options");
  const ERL_NIF_TERM *tpl;
  int tpl_sz;
  while(!enif_is_empty_list(env, lstTail)) {
    if(!enif_get_list_cell(env, lstTail, &lstHead, &lstTail)) Badarg("Options");
    if(!enif_get_tuple(env, lstHead, &tpl_sz, &tpl) || tpl_sz != 2) Badarg("Options");
    if(enif_is_identical(tpl[0], enif_make_atom(env, "pos"))) {
  const ERL_NIF_TERM *pos_t;
  int pos_sz;
  if(!enif_get_tuple(env, tpl[1], &pos_sz, &pos_t)) Badarg("pos");
  int posX;
  if(!enif_get_int(env, pos_t[0], &posX)) Badarg("pos");
  int posY;
  if(!enif_get_int(env, pos_t[1], &posY)) Badarg("pos");
  pos = wxPoint(posX,posY);
    } else     if(enif_is_identical(tpl[0], enif_make_atom(env, "size"))) {
  const ERL_NIF_TERM *size_t;
  int size_sz;
  if(!enif_get_tuple(env, tpl[1], &size_sz, &size_t)) Badarg("size");
  int sizeW;
  if(!enif_get_int(env, size_t[0], &sizeW)) Badarg("size");
  int sizeH;
  if(!enif_get_int(env, size_t[1], &sizeH)) Badarg("size");
  size = wxSize(sizeW,sizeH);
    } else     if(enif_is_identical(tpl[0], enif_make_atom(env, "style"))) {
  if(!enif_get_long(env, tpl[1], &style)) Badarg("style");
    } else        Badarg("Options");
  };
  wxFrame * Result = new EwxFrame(parent,id,title,pos,size,style);
  app->newPtr((void *) Result, 0, memenv);
  wxeReturn rt = wxeReturn(memenv, Ecmd.caller, true);
  rt.send(  rt.make_ref(app->getRef((void *)Result,memenv), "wxFrame"));

}

// wxFrame::Create
void wxFrame_Create(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd)
{
  wxPoint pos= wxDefaultPosition;
  wxSize size= wxDefaultSize;
  long style=wxDEFAULT_FRAME_STYLE;
  ErlNifEnv *env = Ecmd.env;
  ERL_NIF_TERM * argv = Ecmd.args;
  wxFrame *This;
  This = (wxFrame *) memenv->getPtr(env, argv[0], "This");
  wxWindow *parent;
  parent = (wxWindow *) memenv->getPtr(env, argv[1], "parent");
  int id;
  if(!enif_get_int(env, argv[2], &id)) Badarg("id"); // wxWindowID
  ErlNifBinary title_bin;
  wxString title;
  if(!enif_inspect_binary(env, argv[3], &title_bin)) Badarg("title");
  title = wxString(title_bin.data, wxConvUTF8, title_bin.size);
  ERL_NIF_TERM lstHead, lstTail;
  lstTail = argv[4];
  if(!enif_is_list(env, lstTail)) Badarg("Options");
  const ERL_NIF_TERM *tpl;
  int tpl_sz;
  while(!enif_is_empty_list(env, lstTail)) {
    if(!enif_get_list_cell(env, lstTail, &lstHead, &lstTail)) Badarg("Options");
    if(!enif_get_tuple(env, lstHead, &tpl_sz, &tpl) || tpl_sz != 2) Badarg("Options");
    if(enif_is_identical(tpl[0], enif_make_atom(env, "pos"))) {
  const ERL_NIF_TERM *pos_t;
  int pos_sz;
  if(!enif_get_tuple(env, tpl[1], &pos_sz, &pos_t)) Badarg("pos");
  int posX;
  if(!enif_get_int(env, pos_t[0], &posX)) Badarg("pos");
  int posY;
  if(!enif_get_int(env, pos_t[1], &posY)) Badarg("pos");
  pos = wxPoint(posX,posY);
    } else     if(enif_is_identical(tpl[0], enif_make_atom(env, "size"))) {
  const ERL_NIF_TERM *size_t;
  int size_sz;
  if(!enif_get_tuple(env, tpl[1], &size_sz, &size_t)) Badarg("size");
  int sizeW;
  if(!enif_get_int(env, size_t[0], &sizeW)) Badarg("size");
  int sizeH;
  if(!enif_get_int(env, size_t[1], &sizeH)) Badarg("size");
  size = wxSize(sizeW,sizeH);
    } else     if(enif_is_identical(tpl[0], enif_make_atom(env, "style"))) {
  if(!enif_get_long(env, tpl[1], &style)) Badarg("style");
    } else        Badarg("Options");
  };
  if(!This) throw wxe_badarg("This");
  bool Result = This->Create(parent,id,title,pos,size,style);
  wxeReturn rt = wxeReturn(memenv, Ecmd.caller, true);
  rt.send(  rt.make_bool(Result));

}

// wxFrame::CreateStatusBar
void wxFrame_CreateStatusBar(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd)
{
  int number=1;
  long style=wxSTB_DEFAULT_STYLE;
  wxWindowID id=0;
  ErlNifEnv *env = Ecmd.env;
  ERL_NIF_TERM * argv = Ecmd.args;
  wxFrame *This;
  This = (wxFrame *) memenv->getPtr(env, argv[0], "This");
  ERL_NIF_TERM lstHead, lstTail;
  lstTail = argv[1];
  if(!enif_is_list(env, lstTail)) Badarg("Options");
  const ERL_NIF_TERM *tpl;
  int tpl_sz;
  while(!enif_is_empty_list(env, lstTail)) {
    if(!enif_get_list_cell(env, lstTail, &lstHead, &lstTail)) Badarg("Options");
    if(!enif_get_tuple(env, lstHead, &tpl_sz, &tpl) || tpl_sz != 2) Badarg("Options");
    if(enif_is_identical(tpl[0], enif_make_atom(env, "number"))) {
  if(!enif_get_int(env, tpl[1], &number)) Badarg("number"); // int
    } else     if(enif_is_identical(tpl[0], enif_make_atom(env, "style"))) {
  if(!enif_get_long(env, tpl[1], &style)) Badarg("style");
    } else     if(enif_is_identical(tpl[0], enif_make_atom(env, "id"))) {
  if(!enif_get_int(env, tpl[1], &id)) Badarg("id"); // wxWindowID
    } else        Badarg("Options");
  };
  if(!This) throw wxe_badarg("This");
  wxStatusBar * Result = (wxStatusBar*)This->CreateStatusBar(number,style,id);
  wxeReturn rt = wxeReturn(memenv, Ecmd.caller, true);
  rt.send(  rt.make_ref(app->getRef((void *)Result,memenv), "wxStatusBar"));

}

// wxFrame::CreateToolBar
void wxFrame_CreateToolBar(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd)
{
  long style=wxTB_DEFAULT_STYLE;
  wxWindowID id=wxID_ANY;
  ErlNifEnv *env = Ecmd.env;
  ERL_NIF_TERM * argv = Ecmd.args;
  wxFrame *This;
  This = (wxFrame *) memenv->getPtr(env, argv[0], "This");
  ERL_NIF_TERM lstHead, lstTail;
  lstTail = argv[1];
  if(!enif_is_list(env, lstTail)) Badarg("Options");
  const ERL_NIF_TERM *tpl;
  int tpl_sz;
  while(!enif_is_empty_list(env, lstTail)) {
    if(!enif_get_list_cell(env, lstTail, &lstHead, &lstTail)) Badarg("Options");
    if(!enif_get_tuple(env, lstHead, &tpl_sz, &tpl) || tpl_sz != 2) Badarg("Options");
    if(enif_is_identical(tpl[0], enif_make_atom(env, "style"))) {
  if(!enif_get_long(env, tpl[1], &style)) Badarg("style");
    } else     if(enif_is_identical(tpl[0], enif_make_atom(env, "id"))) {
  if(!enif_get_int(env, tpl[1], &id)) Badarg("id"); // wxWindowID
    } else        Badarg("Options");
  };
  if(!This) throw wxe_badarg("This");
  wxToolBar * Result = (wxToolBar*)This->CreateToolBar(style,id);
  wxeReturn rt = wxeReturn(memenv, Ecmd.caller, true);
  rt.send(  rt.make_ref(app->getRef((void *)Result,memenv), "wxToolBar"));

}

// wxFrame::GetClientAreaOrigin
void wxFrame_GetClientAreaOrigin(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd)
{
  ErlNifEnv *env = Ecmd.env;
  ERL_NIF_TERM * argv = Ecmd.args;
  wxFrame *This;
  This = (wxFrame *) memenv->getPtr(env, argv[0], "This");
  if(!This) throw wxe_badarg("This");
  wxPoint Result = This->GetClientAreaOrigin();
  wxeReturn rt = wxeReturn(memenv, Ecmd.caller, true);
  rt.send(  rt.make(Result));

}

// wxFrame::GetMenuBar
void wxFrame_GetMenuBar(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd)
{
  ErlNifEnv *env = Ecmd.env;
  ERL_NIF_TERM * argv = Ecmd.args;
  wxFrame *This;
  This = (wxFrame *) memenv->getPtr(env, argv[0], "This");
  if(!This) throw wxe_badarg("This");
  wxMenuBar * Result = (wxMenuBar*)This->GetMenuBar();
  wxeReturn rt = wxeReturn(memenv, Ecmd.caller, true);
  rt.send(  rt.make_ref(app->getRef((void *)Result,memenv), "wxMenuBar"));

}

// wxFrame::GetStatusBar
void wxFrame_GetStatusBar(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd)
{
  ErlNifEnv *env = Ecmd.env;
  ERL_NIF_TERM * argv = Ecmd.args;
  wxFrame *This;
  This = (wxFrame *) memenv->getPtr(env, argv[0], "This");
  if(!This) throw wxe_badarg("This");
  wxStatusBar * Result = (wxStatusBar*)This->GetStatusBar();
  wxeReturn rt = wxeReturn(memenv, Ecmd.caller, true);
  rt.send(  rt.make_ref(app->getRef((void *)Result,memenv), "wxStatusBar"));

}

// wxFrame::GetStatusBarPane
void wxFrame_GetStatusBarPane(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd)
{
  ErlNifEnv *env = Ecmd.env;
  ERL_NIF_TERM * argv = Ecmd.args;
  wxFrame *This;
  This = (wxFrame *) memenv->getPtr(env, argv[0], "This");
  if(!This) throw wxe_badarg("This");
  int Result = This->GetStatusBarPane();
  wxeReturn rt = wxeReturn(memenv, Ecmd.caller, true);
  rt.send(  rt.make_int(Result));

}

// wxFrame::GetToolBar
void wxFrame_GetToolBar(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd)
{
  ErlNifEnv *env = Ecmd.env;
  ERL_NIF_TERM * argv = Ecmd.args;
  wxFrame *This;
  This = (wxFrame *) memenv->getPtr(env, argv[0], "This");
  if(!This) throw wxe_badarg("This");
  wxToolBar * Result = (wxToolBar*)This->GetToolBar();
  wxeReturn rt = wxeReturn(memenv, Ecmd.caller, true);
  rt.send(  rt.make_ref(app->getRef((void *)Result,memenv), "wxToolBar"));

}

// wxFrame::ProcessCommand
void wxFrame_ProcessCommand(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd)
{
  ErlNifEnv *env = Ecmd.env;
  ERL_NIF_TERM * argv = Ecmd.args;
  wxFrame *This;
  This = (wxFrame *) memenv->getPtr(env, argv[0], "This");
  int id;
  if(!enif_get_int(env, argv[1], &id)) Badarg("id"); // int
  if(!This) throw wxe_badarg("This");
  bool Result = This->ProcessCommand(id);
  wxeReturn rt = wxeReturn(memenv, Ecmd.caller, true);
  rt.send(  rt.make_bool(Result));

}

// wxFrame::SendSizeEvent
void wxFrame_SendSizeEvent(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd)
{
  int flags=0;
  ErlNifEnv *env = Ecmd.env;
  ERL_NIF_TERM * argv = Ecmd.args;
  wxFrame *This;
  This = (wxFrame *) memenv->getPtr(env, argv[0], "This");
  ERL_NIF_TERM lstHead, lstTail;
  lstTail = argv[1];
  if(!enif_is_list(env, lstTail)) Badarg("Options");
  const ERL_NIF_TERM *tpl;
  int tpl_sz;
  while(!enif_is_empty_list(env, lstTail)) {
    if(!enif_get_list_cell(env, lstTail, &lstHead, &lstTail)) Badarg("Options");
    if(!enif_get_tuple(env, lstHead, &tpl_sz, &tpl) || tpl_sz != 2) Badarg("Options");
    if(enif_is_identical(tpl[0], enif_make_atom(env, "flags"))) {
  if(!enif_get_int(env, tpl[1], &flags)) Badarg("flags"); // int
    } else        Badarg("Options");
  };
  if(!This) throw wxe_badarg("This");
  This->SendSizeEvent(flags);

}

// wxFrame::SetMenuBar
void wxFrame_SetMenuBar(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd)
{
  ErlNifEnv *env = Ecmd.env;
  ERL_NIF_TERM * argv = Ecmd.args;
  wxFrame *This;
  This = (wxFrame *) memenv->getPtr(env, argv[0], "This");
  wxMenuBar *menuBar;
  menuBar = (wxMenuBar *) memenv->getPtr(env, argv[1], "menuBar");
  if(!This) throw wxe_badarg("This");
  This->SetMenuBar(menuBar);

}

// wxFrame::SetStatusBar
void wxFrame_SetStatusBar(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd)
{
  ErlNifEnv *env = Ecmd.env;
  ERL_NIF_TERM * argv = Ecmd.args;
  wxFrame *This;
  This = (wxFrame *) memenv->getPtr(env, argv[0], "This");
  wxStatusBar *statusBar;
  statusBar = (wxStatusBar *) memenv->getPtr(env, argv[1], "statusBar");
  if(!This) throw wxe_badarg("This");
  This->SetStatusBar(statusBar);

}

// wxFrame::SetStatusBarPane
void wxFrame_SetStatusBarPane(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd)
{
  ErlNifEnv *env = Ecmd.env;
  ERL_NIF_TERM * argv = Ecmd.args;
  wxFrame *This;
  This = (wxFrame *) memenv->getPtr(env, argv[0], "This");
  int n;
  if(!enif_get_int(env, argv[1], &n)) Badarg("n"); // int
  if(!This) throw wxe_badarg("This");
  This->SetStatusBarPane(n);

}

// wxFrame::SetStatusText
void wxFrame_SetStatusText(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd)
{
  int number=0;
  ErlNifEnv *env = Ecmd.env;
  ERL_NIF_TERM * argv = Ecmd.args;
  wxFrame *This;
  This = (wxFrame *) memenv->getPtr(env, argv[0], "This");
  ErlNifBinary text_bin;
  wxString text;
  if(!enif_inspect_binary(env, argv[1], &text_bin)) Badarg("text");
  text = wxString(text_bin.data, wxConvUTF8, text_bin.size);
  ERL_NIF_TERM lstHead, lstTail;
  lstTail = argv[2];
  if(!enif_is_list(env, lstTail)) Badarg("Options");
  const ERL_NIF_TERM *tpl;
  int tpl_sz;
  while(!enif_is_empty_list(env, lstTail)) {
    if(!enif_get_list_cell(env, lstTail, &lstHead, &lstTail)) Badarg("Options");
    if(!enif_get_tuple(env, lstHead, &tpl_sz, &tpl) || tpl_sz != 2) Badarg("Options");
    if(enif_is_identical(tpl[0], enif_make_atom(env, "number"))) {
  if(!enif_get_int(env, tpl[1], &number)) Badarg("number"); // int
    } else        Badarg("Options");
  };
  if(!This) throw wxe_badarg("This");
  This->SetStatusText(text,number);

}

// wxFrame::SetStatusWidths
void wxFrame_SetStatusWidths(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd)
{
  ErlNifEnv *env = Ecmd.env;
  ERL_NIF_TERM * argv = Ecmd.args;
  wxFrame *This;
  This = (wxFrame *) memenv->getPtr(env, argv[0], "This");
  int widths_field_tmp;
  unsigned int widths_fieldLen;
  ERL_NIF_TERM widths_fieldHead, widths_fieldTail;
  if(!enif_get_list_length(env, argv[1], &widths_fieldLen)) Badarg("widths_field");
  std::vector <int> widths_field;
  widths_fieldTail = argv[1];
  while(!enif_is_empty_list(env, widths_fieldTail)) {
    if(!enif_get_list_cell(env, widths_fieldTail, &widths_fieldHead, &widths_fieldTail)) Badarg("widths_field");
    if(!enif_get_int(env, widths_fieldHead, &widths_field_tmp)) Badarg("widths_field");
    widths_field.push_back( (int) widths_field_tmp);
  };
  if(!This) throw wxe_badarg("This");
  This->SetStatusWidths(widths_fieldLen,widths_field.data());

}

// wxFrame::SetToolBar
void wxFrame_SetToolBar(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd)
{
  ErlNifEnv *env = Ecmd.env;
  ERL_NIF_TERM * argv = Ecmd.args;
  wxFrame *This;
  This = (wxFrame *) memenv->getPtr(env, argv[0], "This");
  wxToolBar *toolBar;
  toolBar = (wxToolBar *) memenv->getPtr(env, argv[1], "toolBar");
  if(!This) throw wxe_badarg("This");
  This->SetToolBar(toolBar);

}

#if wxUSE_GRAPHICS_CONTEXT
// wxGCDC::wxGCDC
void wxGCDC_new_1(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd)
{
  ErlNifEnv *env = Ecmd.env;
  ERL_NIF_TERM * argv = Ecmd.args;
  ERL_NIF_TERM windowDC_type;
  void * windowDC = memenv->getPtr(env, argv[0], "windowDC", &windowDC_type);
  wxGCDC * Result;
  if(enif_is_identical(windowDC_type, WXE_ATOM_wxWindowDC))
    Result = new EwxGCDC(* static_cast<wxWindowDC*> (windowDC));
  else if(enif_is_identical(windowDC_type, WXE_ATOM_wxMemoryDC))
    Result = new EwxGCDC(* static_cast<wxMemoryDC*> (windowDC));
  else if(enif_is_identical(windowDC_type, WXE_ATOM_wxGraphicsContext))
    Result = new EwxGCDC(static_cast<wxGraphicsContext*> (windowDC));
  else throw wxe_badarg("windowDC");
  app->newPtr((void *) Result, 8, memenv);
  wxeReturn rt = wxeReturn(memenv, Ecmd.caller, true);
  rt.send(  rt.make_ref(app->getRef((void *)Result,memenv), "wxGCDC"));

}

// wxGCDC::wxGCDC
void wxGCDC_new_0(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd)
{
  wxGCDC * Result = new EwxGCDC();
  app->newPtr((void *) Result, 8, memenv);
  wxeReturn rt = wxeReturn(memenv, Ecmd.caller, true);
  rt.send(  rt.make_ref(app->getRef((void *)Result,memenv), "wxGCDC"));

}

// wxGCDC::GetGraphicsContext
void wxGCDC_GetGraphicsContext(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd)
{
  ErlNifEnv *env = Ecmd.env;
  ERL_NIF_TERM * argv = Ecmd.args;
  wxGCDC *This;
  This = (wxGCDC *) memenv->getPtr(env, argv[0], "This");
  if(!This) throw wxe_badarg("This");
  wxGraphicsContext * Result = (wxGraphicsContext*)This->GetGraphicsContext();
  wxeReturn rt = wxeReturn(memenv, Ecmd.caller, true);
  rt.send(  rt.make_ref(app->getRef((void *)Result,memenv,8), "wxGraphicsContext")
);

}

// wxGCDC::SetGraphicsContext
void wxGCDC_SetGraphicsContext(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd)
{
  ErlNifEnv *env = Ecmd.env;
  ERL_NIF_TERM * argv = Ecmd.args;
  wxGCDC *This;
  This = (wxGCDC *) memenv->getPtr(env, argv[0], "This");
  wxGraphicsContext *context;
  context = (wxGraphicsContext *) memenv->getPtr(env, argv[1], "context");
  if(!This) throw wxe_badarg("This");
  This->SetGraphicsContext(context);

}

#endif // wxUSE_GRAPHICS_CONTEXT
#if wxUSE_GLCANVAS
// wxGLCanvas::wxGLCanvas
void wxGLCanvas_new(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd)
{
  wxWindowID id=wxID_ANY;
  unsigned int attribListLen;
  std::vector <int> attribList;
  wxPoint pos= wxDefaultPosition;
  wxSize size= wxDefaultSize;
  long style=0;
  wxString name= "GLCanvas";
  const wxPalette * palette= &wxNullPalette;
  ErlNifEnv *env = Ecmd.env;
  ERL_NIF_TERM * argv = Ecmd.args;
  wxWindow *parent;
  parent = (wxWindow *) memenv->getPtr(env, argv[0], "parent");
  ERL_NIF_TERM lstHead, lstTail;
  lstTail = argv[1];
  if(!enif_is_list(env, lstTail)) Badarg("Options");
  const ERL_NIF_TERM *tpl;
  int tpl_sz;
  while(!enif_is_empty_list(env, lstTail)) {
    if(!enif_get_list_cell(env, lstTail, &lstHead, &lstTail)) Badarg("Options");
    if(!enif_get_tuple(env, lstHead, &tpl_sz, &tpl) || tpl_sz != 2) Badarg("Options");
    if(enif_is_identical(tpl[0], enif_make_atom(env, "id"))) {
  if(!enif_get_int(env, tpl[1], &id)) Badarg("id"); // wxWindowID
    } else     if(enif_is_identical(tpl[0], enif_make_atom(env, "attribList"))) {
  int attribList_tmp;
  ERL_NIF_TERM attribListHead, attribListTail;
  if(!enif_get_list_length(env, tpl[1], &attribListLen)) Badarg("attribList");
  attribListTail = tpl[1];
  while(!enif_is_empty_list(env, attribListTail)) {
    if(!enif_get_list_cell(env, attribListTail, &attribListHead, &attribListTail)) Badarg("attribList");
    if(!enif_get_int(env, attribListHead, &attribList_tmp)) Badarg("attribList");
    attribList.push_back( (int) attribList_tmp);
  };
    } else     if(enif_is_identical(tpl[0], enif_make_atom(env, "pos"))) {
  const ERL_NIF_TERM *pos_t;
  int pos_sz;
  if(!enif_get_tuple(env, tpl[1], &pos_sz, &pos_t)) Badarg("pos");
  int posX;
  if(!enif_get_int(env, pos_t[0], &posX)) Badarg("pos");
  int posY;
  if(!enif_get_int(env, pos_t[1], &posY)) Badarg("pos");
  pos = wxPoint(posX,posY);
    } else     if(enif_is_identical(tpl[0], enif_make_atom(env, "size"))) {
  const ERL_NIF_TERM *size_t;
  int size_sz;
  if(!enif_get_tuple(env, tpl[1], &size_sz, &size_t)) Badarg("size");
  int sizeW;
  if(!enif_get_int(env, size_t[0], &sizeW)) Badarg("size");
  int sizeH;
  if(!enif_get_int(env, size_t[1], &sizeH)) Badarg("size");
  size = wxSize(sizeW,sizeH);
    } else     if(enif_is_identical(tpl[0], enif_make_atom(env, "style"))) {
  if(!enif_get_long(env, tpl[1], &style)) Badarg("style");
    } else     if(enif_is_identical(tpl[0], enif_make_atom(env, "name"))) {
  ErlNifBinary name_bin;
  if(!enif_inspect_binary(env, tpl[1], &name_bin)) Badarg("name");
  name = wxString(name_bin.data, wxConvUTF8, name_bin.size);
    } else     if(enif_is_identical(tpl[0], enif_make_atom(env, "palette"))) {
  palette = (wxPalette *) memenv->getPtr(env, tpl[1], "palette");
    } else        Badarg("Options");
  };
  wxGLCanvas * Result = new EwxGLCanvas(parent,id, attribList.empty() ? NULL : attribList.data(),pos,size,style,name,*palette);
  app->newPtr((void *) Result, 0, memenv);
  wxeReturn rt = wxeReturn(memenv, Ecmd.caller, true);
  rt.send(  rt.make_ref(app->getRef((void *)Result,memenv), "wxGLCanvas"));

}

// wxGLCanvas::SetCurrent
void wxGLCanvas_SetCurrent(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd)
{
  ErlNifEnv *env = Ecmd.env;
  ERL_NIF_TERM * argv = Ecmd.args;
  wxGLCanvas *This;
  This = (wxGLCanvas *) memenv->getPtr(env, argv[0], "This");
  wxGLContext *context;
  context = (wxGLContext *) memenv->getPtr(env, argv[1], "context");
  if(!This) throw wxe_badarg("This");
  bool Result = This->SetCurrent(*context);
 setActiveGL(memenv, Ecmd.caller, This, context);
  wxeReturn rt = wxeReturn(memenv, Ecmd.caller, true);
  rt.send(  rt.make_bool(Result));

}

#if wxUSE_GLCANVAS_EGL
// wxGLCanvas::CreateSurface
void wxGLCanvas_CreateSurface(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd)
{
  ErlNifEnv *env = Ecmd.env;
  ERL_NIF_TERM * argv = Ecmd.args;
  wxGLCanvas *This;
  This = (wxGLCanvas *) memenv->getPtr(env, argv[0], "This");
 
#if !wxCHECK_VERSION(3,2,3)
  if(!This) throw wxe_badarg(0);
  if(This->GetEGLSurface() != EGL_NO_SURFACE)
     eglDestroySurface(This->GetEGLDisplay(), This->GetEGLSurface());
#endif
;
  if(!This) throw wxe_badarg("This");
  bool Result = This->CreateSurface();
  wxeReturn rt = wxeReturn(memenv, Ecmd.caller, true);
  rt.send(  rt.make_bool(Result));

}

#endif
// wxGLCanvas::IsDisplaySupported
void wxGLCanvas_IsDisplaySupported(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd)
{
  ErlNifEnv *env = Ecmd.env;
  ERL_NIF_TERM * argv = Ecmd.args;
  int attribList_tmp;
  unsigned int attribListLen;
  ERL_NIF_TERM attribListHead, attribListTail;
  if(!enif_get_list_length(env, argv[0], &attribListLen)) Badarg("attribList");
  std::vector <int> attribList;
  attribListTail = argv[0];
  while(!enif_is_empty_list(env, attribListTail)) {
    if(!enif_get_list_cell(env, attribListTail, &attribListHead, &attribListTail)) Badarg("attribList");
    if(!enif_get_int(env, attribListHead, &attribList_tmp)) Badarg("attribList");
    attribList.push_back( (int) attribList_tmp);
  };
  bool Result = wxGLCanvas::IsDisplaySupported(attribList.data());
  wxeReturn rt = wxeReturn(memenv, Ecmd.caller, true);
  rt.send(  rt.make_bool(Result));

}

// wxGLCanvas::SwapBuffers
void wxGLCanvas_SwapBuffers(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd)
{
  ErlNifEnv *env = Ecmd.env;
  ERL_NIF_TERM * argv = Ecmd.args;
  wxGLCanvas *This;
  This = (wxGLCanvas *) memenv->getPtr(env, argv[0], "This");
  if(!This) throw wxe_badarg("This");
  bool Result = This->SwapBuffers();
  wxeReturn rt = wxeReturn(memenv, Ecmd.caller, true);
  rt.send(  rt.make_bool(Result));

}

#endif // wxUSE_GLCANVAS
#if wxUSE_GLCANVAS
// wxGLContext::wxGLContext
void wxGLContext_new(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd)
{
  wxGLContext * other=NULL;
  ErlNifEnv *env = Ecmd.env;
  ERL_NIF_TERM * argv = Ecmd.args;
  wxGLCanvas *win;
  win = (wxGLCanvas *) memenv->getPtr(env, argv[0], "win");
  ERL_NIF_TERM lstHead, lstTail;
  lstTail = argv[1];
  if(!enif_is_list(env, lstTail)) Badarg("Options");
  const ERL_NIF_TERM *tpl;
  int tpl_sz;
  while(!enif_is_empty_list(env, lstTail)) {
    if(!enif_get_list_cell(env, lstTail, &lstHead, &lstTail)) Badarg("Options");
    if(!enif_get_tuple(env, lstHead, &tpl_sz, &tpl) || tpl_sz != 2) Badarg("Options");
    if(enif_is_identical(tpl[0], enif_make_atom(env, "other"))) {
  other = (wxGLContext *) memenv->getPtr(env, tpl[1], "other");
    } else        Badarg("Options");
  };
  wxGLContext * Result = new EwxGLContext(win,other);
  app->newPtr((void *) Result, 1, memenv);
  wxeReturn rt = wxeReturn(memenv, Ecmd.caller, true);
  rt.send(  rt.make_ref(app->getRef((void *)Result,memenv), "wxGLContext"));

}

// wxGLContext::SetCurrent
void wxGLContext_SetCurrent(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd)
{
  ErlNifEnv *env = Ecmd.env;
  ERL_NIF_TERM * argv = Ecmd.args;
  wxGLContext *This;
  This = (wxGLContext *) memenv->getPtr(env, argv[0], "This");
  wxGLCanvas *win;
  win = (wxGLCanvas *) memenv->getPtr(env, argv[1], "win");
  if(!This) throw wxe_badarg("This");
  bool Result = This->SetCurrent(*win);
 setActiveGL(memenv, Ecmd.caller, win, This);
  wxeReturn rt = wxeReturn(memenv, Ecmd.caller, true);
  rt.send(  rt.make_bool(Result));

}

#if wxCHECK_VERSION(3,1,0)
// wxGLContext::IsOK
void wxGLContext_IsOK(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd)
{
  ErlNifEnv *env = Ecmd.env;
  ERL_NIF_TERM * argv = Ecmd.args;
  wxGLContext *This;
  This = (wxGLContext *) memenv->getPtr(env, argv[0], "This");
  if(!This) throw wxe_badarg("This");
  bool Result = This->IsOK();
  wxeReturn rt = wxeReturn(memenv, Ecmd.caller, true);
  rt.send(  rt.make_bool(Result));

}

#endif
#endif // wxUSE_GLCANVAS
// wxGauge::wxGauge
void wxGauge_new_0(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd)
{
  wxGauge * Result = new EwxGauge();
  app->newPtr((void *) Result, 0, memenv);
  wxeReturn rt = wxeReturn(memenv, Ecmd.caller, true);
  rt.send(  rt.make_ref(app->getRef((void *)Result,memenv), "wxGauge"));

}

// wxGauge::wxGauge
void wxGauge_new_4(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd)
{
  wxPoint pos= wxDefaultPosition;
  wxSize size= wxDefaultSize;
  long style=wxGA_HORIZONTAL;
  const wxValidator * validator= &wxDefaultValidator;
  ErlNifEnv *env = Ecmd.env;
  ERL_NIF_TERM * argv = Ecmd.args;
  wxWindow *parent;
  parent = (wxWindow *) memenv->getPtr(env, argv[0], "parent");
  int id;
  if(!enif_get_int(env, argv[1], &id)) Badarg("id"); // wxWindowID
  int range;
  if(!enif_get_int(env, argv[2], &range)) Badarg("range"); // int
  ERL_NIF_TERM lstHead, lstTail;
  lstTail = argv[3];
  if(!enif_is_list(env, lstTail)) Badarg("Options");
  const ERL_NIF_TERM *tpl;
  int tpl_sz;
  while(!enif_is_empty_list(env, lstTail)) {
    if(!enif_get_list_cell(env, lstTail, &lstHead, &lstTail)) Badarg("Options");
    if(!enif_get_tuple(env, lstHead, &tpl_sz, &tpl) || tpl_sz != 2) Badarg("Options");
    if(enif_is_identical(tpl[0], enif_make_atom(env, "pos"))) {
  const ERL_NIF_TERM *pos_t;
  int pos_sz;
  if(!enif_get_tuple(env, tpl[1], &pos_sz, &pos_t)) Badarg("pos");
  int posX;
  if(!enif_get_int(env, pos_t[0], &posX)) Badarg("pos");
  int posY;
  if(!enif_get_int(env, pos_t[1], &posY)) Badarg("pos");
  pos = wxPoint(posX,posY);
    } else     if(enif_is_identical(tpl[0], enif_make_atom(env, "size"))) {
  const ERL_NIF_TERM *size_t;
  int size_sz;
  if(!enif_get_tuple(env, tpl[1], &size_sz, &size_t)) Badarg("size");
  int sizeW;
  if(!enif_get_int(env, size_t[0], &sizeW)) Badarg("size");
  int sizeH;
  if(!enif_get_int(env, size_t[1], &sizeH)) Badarg("size");
  size = wxSize(sizeW,sizeH);
    } else     if(enif_is_identical(tpl[0], enif_make_atom(env, "style"))) {
  if(!enif_get_long(env, tpl[1], &style)) Badarg("style");
    } else     if(enif_is_identical(tpl[0], enif_make_atom(env, "validator"))) {
  validator = (wxValidator *) memenv->getPtr(env, tpl[1], "validator");
    } else        Badarg("Options");
  };
  wxGauge * Result = new EwxGauge(parent,id,range,pos,size,style,*validator);
  app->newPtr((void *) Result, 0, memenv);
  wxeReturn rt = wxeReturn(memenv, Ecmd.caller, true);
  rt.send(  rt.make_ref(app->getRef((void *)Result,memenv), "wxGauge"));

}

// wxGauge::Create
void wxGauge_Create(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd)
{
  wxPoint pos= wxDefaultPosition;
  wxSize size= wxDefaultSize;
  long style=wxGA_HORIZONTAL;
  const wxValidator * validator= &wxDefaultValidator;
  ErlNifEnv *env = Ecmd.env;
  ERL_NIF_TERM * argv = Ecmd.args;
  wxGauge *This;
  This = (wxGauge *) memenv->getPtr(env, argv[0], "This");
  wxWindow *parent;
  parent = (wxWindow *) memenv->getPtr(env, argv[1], "parent");
  int id;
  if(!enif_get_int(env, argv[2], &id)) Badarg("id"); // wxWindowID
  int range;
  if(!enif_get_int(env, argv[3], &range)) Badarg("range"); // int
  ERL_NIF_TERM lstHead, lstTail;
  lstTail = argv[4];
  if(!enif_is_list(env, lstTail)) Badarg("Options");
  const ERL_NIF_TERM *tpl;
  int tpl_sz;
  while(!enif_is_empty_list(env, lstTail)) {
    if(!enif_get_list_cell(env, lstTail, &lstHead, &lstTail)) Badarg("Options");
    if(!enif_get_tuple(env, lstHead, &tpl_sz, &tpl) || tpl_sz != 2) Badarg("Options");
    if(enif_is_identical(tpl[0], enif_make_atom(env, "pos"))) {
  const ERL_NIF_TERM *pos_t;
  int pos_sz;
  if(!enif_get_tuple(env, tpl[1], &pos_sz, &pos_t)) Badarg("pos");
  int posX;
  if(!enif_get_int(env, pos_t[0], &posX)) Badarg("pos");
  int posY;
  if(!enif_get_int(env, pos_t[1], &posY)) Badarg("pos");
  pos = wxPoint(posX,posY);
    } else     if(enif_is_identical(tpl[0], enif_make_atom(env, "size"))) {
  const ERL_NIF_TERM *size_t;
  int size_sz;
  if(!enif_get_tuple(env, tpl[1], &size_sz, &size_t)) Badarg("size");
  int sizeW;
  if(!enif_get_int(env, size_t[0], &sizeW)) Badarg("size");
  int sizeH;
  if(!enif_get_int(env, size_t[1], &sizeH)) Badarg("size");
  size = wxSize(sizeW,sizeH);
    } else     if(enif_is_identical(tpl[0], enif_make_atom(env, "style"))) {
  if(!enif_get_long(env, tpl[1], &style)) Badarg("style");
    } else     if(enif_is_identical(tpl[0], enif_make_atom(env, "validator"))) {
  validator = (wxValidator *) memenv->getPtr(env, tpl[1], "validator");
    } else        Badarg("Options");
  };
  if(!This) throw wxe_badarg("This");
  bool Result = This->Create(parent,id,range,pos,size,style,*validator);
  wxeReturn rt = wxeReturn(memenv, Ecmd.caller, true);
  rt.send(  rt.make_bool(Result));

}

// wxGauge::GetRange
void wxGauge_GetRange(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd)
{
  ErlNifEnv *env = Ecmd.env;
  ERL_NIF_TERM * argv = Ecmd.args;
  wxGauge *This;
  This = (wxGauge *) memenv->getPtr(env, argv[0], "This");
  if(!This) throw wxe_badarg("This");
  int Result = This->GetRange();
  wxeReturn rt = wxeReturn(memenv, Ecmd.caller, true);
  rt.send(  rt.make_int(Result));

}

// wxGauge::GetValue
void wxGauge_GetValue(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd)
{
  ErlNifEnv *env = Ecmd.env;
  ERL_NIF_TERM * argv = Ecmd.args;
  wxGauge *This;
  This = (wxGauge *) memenv->getPtr(env, argv[0], "This");
  if(!This) throw wxe_badarg("This");
  int Result = This->GetValue();
  wxeReturn rt = wxeReturn(memenv, Ecmd.caller, true);
  rt.send(  rt.make_int(Result));

}

// wxGauge::IsVertical
void wxGauge_IsVertical(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd)
{
  ErlNifEnv *env = Ecmd.env;
  ERL_NIF_TERM * argv = Ecmd.args;
  wxGauge *This;
  This = (wxGauge *) memenv->getPtr(env, argv[0], "This");
  if(!This) throw wxe_badarg("This");
  bool Result = This->IsVertical();
  wxeReturn rt = wxeReturn(memenv, Ecmd.caller, true);
  rt.send(  rt.make_bool(Result));

}

// wxGauge::SetRange
void wxGauge_SetRange(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd)
{
  ErlNifEnv *env = Ecmd.env;
  ERL_NIF_TERM * argv = Ecmd.args;
  wxGauge *This;
  This = (wxGauge *) memenv->getPtr(env, argv[0], "This");
  int range;
  if(!enif_get_int(env, argv[1], &range)) Badarg("range"); // int
  if(!This) throw wxe_badarg("This");
  This->SetRange(range);

}

// wxGauge::SetValue
void wxGauge_SetValue(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd)
{
  ErlNifEnv *env = Ecmd.env;
  ERL_NIF_TERM * argv = Ecmd.args;
  wxGauge *This;
  This = (wxGauge *) memenv->getPtr(env, argv[0], "This");
  int pos;
  if(!enif_get_int(env, argv[1], &pos)) Badarg("pos"); // int
  if(!This) throw wxe_badarg("This");
  This->SetValue(pos);

}

// wxGauge::Pulse
void wxGauge_Pulse(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd)
{
  ErlNifEnv *env = Ecmd.env;
  ERL_NIF_TERM * argv = Ecmd.args;
  wxGauge *This;
  This = (wxGauge *) memenv->getPtr(env, argv[0], "This");
  if(!This) throw wxe_badarg("This");
  This->Pulse();

}

// wxGenericDirCtrl::wxGenericDirCtrl
void wxGenericDirCtrl_new_0(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd)
{
  wxGenericDirCtrl * Result = new EwxGenericDirCtrl();
  app->newPtr((void *) Result, 0, memenv);
  wxeReturn rt = wxeReturn(memenv, Ecmd.caller, true);
  rt.send(  rt.make_ref(app->getRef((void *)Result,memenv), "wxGenericDirCtrl"));

}

// wxGenericDirCtrl::wxGenericDirCtrl
void wxGenericDirCtrl_new_2(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd)
{
  wxWindowID id=wxID_ANY;
  wxString dir= wxDirDialogDefaultFolderStr;
  wxPoint pos= wxDefaultPosition;
  wxSize size= wxDefaultSize;
  long style=wxDIRCTRL_3D_INTERNAL;
  wxString filter= wxEmptyString;
  int defaultFilter=0;
  ErlNifEnv *env = Ecmd.env;
  ERL_NIF_TERM * argv = Ecmd.args;
  wxWindow *parent;
  parent = (wxWindow *) memenv->getPtr(env, argv[0], "parent");
  ERL_NIF_TERM lstHead, lstTail;
  lstTail = argv[1];
  if(!enif_is_list(env, lstTail)) Badarg("Options");
  const ERL_NIF_TERM *tpl;
  int tpl_sz;
  while(!enif_is_empty_list(env, lstTail)) {
    if(!enif_get_list_cell(env, lstTail, &lstHead, &lstTail)) Badarg("Options");
    if(!enif_get_tuple(env, lstHead, &tpl_sz, &tpl) || tpl_sz != 2) Badarg("Options");
    if(enif_is_identical(tpl[0], enif_make_atom(env, "id"))) {
  if(!enif_get_int(env, tpl[1], &id)) Badarg("id"); // wxWindowID
    } else     if(enif_is_identical(tpl[0], enif_make_atom(env, "dir"))) {
  ErlNifBinary dir_bin;
  if(!enif_inspect_binary(env, tpl[1], &dir_bin)) Badarg("dir");
  dir = wxString(dir_bin.data, wxConvUTF8, dir_bin.size);
    } else     if(enif_is_identical(tpl[0], enif_make_atom(env, "pos"))) {
  const ERL_NIF_TERM *pos_t;
  int pos_sz;
  if(!enif_get_tuple(env, tpl[1], &pos_sz, &pos_t)) Badarg("pos");
  int posX;
  if(!enif_get_int(env, pos_t[0], &posX)) Badarg("pos");
  int posY;
  if(!enif_get_int(env, pos_t[1], &posY)) Badarg("pos");
  pos = wxPoint(posX,posY);
    } else     if(enif_is_identical(tpl[0], enif_make_atom(env, "size"))) {
  const ERL_NIF_TERM *size_t;
  int size_sz;
  if(!enif_get_tuple(env, tpl[1], &size_sz, &size_t)) Badarg("size");
  int sizeW;
  if(!enif_get_int(env, size_t[0], &sizeW)) Badarg("size");
  int sizeH;
  if(!enif_get_int(env, size_t[1], &sizeH)) Badarg("size");
  size = wxSize(sizeW,sizeH);
    } else     if(enif_is_identical(tpl[0], enif_make_atom(env, "style"))) {
  if(!enif_get_long(env, tpl[1], &style)) Badarg("style");
    } else     if(enif_is_identical(tpl[0], enif_make_atom(env, "filter"))) {
  ErlNifBinary filter_bin;
  if(!enif_inspect_binary(env, tpl[1], &filter_bin)) Badarg("filter");
  filter = wxString(filter_bin.data, wxConvUTF8, filter_bin.size);
    } else     if(enif_is_identical(tpl[0], enif_make_atom(env, "defaultFilter"))) {
  if(!enif_get_int(env, tpl[1], &defaultFilter)) Badarg("defaultFilter"); // int
    } else        Badarg("Options");
  };
  wxGenericDirCtrl * Result = new EwxGenericDirCtrl(parent,id,dir,pos,size,style,filter,defaultFilter);
  app->newPtr((void *) Result, 0, memenv);
  wxeReturn rt = wxeReturn(memenv, Ecmd.caller, true);
  rt.send(  rt.make_ref(app->getRef((void *)Result,memenv), "wxGenericDirCtrl"));

}

// wxGenericDirCtrl::Create
void wxGenericDirCtrl_Create(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd)
{
  wxWindowID id=wxID_ANY;
  wxString dir= wxDirDialogDefaultFolderStr;
  wxPoint pos= wxDefaultPosition;
  wxSize size= wxDefaultSize;
  long style=wxDIRCTRL_3D_INTERNAL;
  wxString filter= wxEmptyString;
  int defaultFilter=0;
  ErlNifEnv *env = Ecmd.env;
  ERL_NIF_TERM * argv = Ecmd.args;
  wxGenericDirCtrl *This;
  This = (wxGenericDirCtrl *) memenv->getPtr(env, argv[0], "This");
  wxWindow *parent;
  parent = (wxWindow *) memenv->getPtr(env, argv[1], "parent");
  ERL_NIF_TERM lstHead, lstTail;
  lstTail = argv[2];
  if(!enif_is_list(env, lstTail)) Badarg("Options");
  const ERL_NIF_TERM *tpl;
  int tpl_sz;
  while(!enif_is_empty_list(env, lstTail)) {
    if(!enif_get_list_cell(env, lstTail, &lstHead, &lstTail)) Badarg("Options");
    if(!enif_get_tuple(env, lstHead, &tpl_sz, &tpl) || tpl_sz != 2) Badarg("Options");
    if(enif_is_identical(tpl[0], enif_make_atom(env, "id"))) {
  if(!enif_get_int(env, tpl[1], &id)) Badarg("id"); // wxWindowID
    } else     if(enif_is_identical(tpl[0], enif_make_atom(env, "dir"))) {
  ErlNifBinary dir_bin;
  if(!enif_inspect_binary(env, tpl[1], &dir_bin)) Badarg("dir");
  dir = wxString(dir_bin.data, wxConvUTF8, dir_bin.size);
    } else     if(enif_is_identical(tpl[0], enif_make_atom(env, "pos"))) {
  const ERL_NIF_TERM *pos_t;
  int pos_sz;
  if(!enif_get_tuple(env, tpl[1], &pos_sz, &pos_t)) Badarg("pos");
  int posX;
  if(!enif_get_int(env, pos_t[0], &posX)) Badarg("pos");
  int posY;
  if(!enif_get_int(env, pos_t[1], &posY)) Badarg("pos");
  pos = wxPoint(posX,posY);
    } else     if(enif_is_identical(tpl[0], enif_make_atom(env, "size"))) {
  const ERL_NIF_TERM *size_t;
  int size_sz;
  if(!enif_get_tuple(env, tpl[1], &size_sz, &size_t)) Badarg("size");
  int sizeW;
  if(!enif_get_int(env, size_t[0], &sizeW)) Badarg("size");
  int sizeH;
  if(!enif_get_int(env, size_t[1], &sizeH)) Badarg("size");
  size = wxSize(sizeW,sizeH);
    } else     if(enif_is_identical(tpl[0], enif_make_atom(env, "style"))) {
  if(!enif_get_long(env, tpl[1], &style)) Badarg("style");
    } else     if(enif_is_identical(tpl[0], enif_make_atom(env, "filter"))) {
  ErlNifBinary filter_bin;
  if(!enif_inspect_binary(env, tpl[1], &filter_bin)) Badarg("filter");
  filter = wxString(filter_bin.data, wxConvUTF8, filter_bin.size);
    } else     if(enif_is_identical(tpl[0], enif_make_atom(env, "defaultFilter"))) {
  if(!enif_get_int(env, tpl[1], &defaultFilter)) Badarg("defaultFilter"); // int
    } else        Badarg("Options");
  };
  if(!This) throw wxe_badarg("This");
  bool Result = This->Create(parent,id,dir,pos,size,style,filter,defaultFilter);
  wxeReturn rt = wxeReturn(memenv, Ecmd.caller, true);
  rt.send(  rt.make_bool(Result));

}

// wxGenericDirCtrl::Init
void wxGenericDirCtrl_Init(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd)
{
  ErlNifEnv *env = Ecmd.env;
  ERL_NIF_TERM * argv = Ecmd.args;
  wxGenericDirCtrl *This;
  This = (wxGenericDirCtrl *) memenv->getPtr(env, argv[0], "This");
  if(!This) throw wxe_badarg("This");
  This->Init();

}

// wxGenericDirCtrl::CollapseTree
void wxGenericDirCtrl_CollapseTree(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd)
{
  ErlNifEnv *env = Ecmd.env;
  ERL_NIF_TERM * argv = Ecmd.args;
  wxGenericDirCtrl *This;
  This = (wxGenericDirCtrl *) memenv->getPtr(env, argv[0], "This");
  if(!This) throw wxe_badarg("This");
  This->CollapseTree();

}

// wxGenericDirCtrl::ExpandPath
void wxGenericDirCtrl_ExpandPath(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd)
{
  ErlNifEnv *env = Ecmd.env;
  ERL_NIF_TERM * argv = Ecmd.args;
  wxGenericDirCtrl *This;
  This = (wxGenericDirCtrl *) memenv->getPtr(env, argv[0], "This");
  ErlNifBinary path_bin;
  wxString path;
  if(!enif_inspect_binary(env, argv[1], &path_bin)) Badarg("path");
  path = wxString(path_bin.data, wxConvUTF8, path_bin.size);
  if(!This) throw wxe_badarg("This");
  bool Result = This->ExpandPath(path);
  wxeReturn rt = wxeReturn(memenv, Ecmd.caller, true);
  rt.send(  rt.make_bool(Result));

}

// wxGenericDirCtrl::GetDefaultPath
void wxGenericDirCtrl_GetDefaultPath(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd)
{
  ErlNifEnv *env = Ecmd.env;
  ERL_NIF_TERM * argv = Ecmd.args;
  wxGenericDirCtrl *This;
  This = (wxGenericDirCtrl *) memenv->getPtr(env, argv[0], "This");
  if(!This) throw wxe_badarg("This");
  wxString Result = This->GetDefaultPath();
  wxeReturn rt = wxeReturn(memenv, Ecmd.caller, true);
  rt.send(  rt.make(Result));

}

// wxGenericDirCtrl::GetPath
void wxGenericDirCtrl_GetPath_0(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd)
{
  ErlNifEnv *env = Ecmd.env;
  ERL_NIF_TERM * argv = Ecmd.args;
  wxGenericDirCtrl *This;
  This = (wxGenericDirCtrl *) memenv->getPtr(env, argv[0], "This");
  if(!This) throw wxe_badarg("This");
  wxString Result = This->GetPath();
  wxeReturn rt = wxeReturn(memenv, Ecmd.caller, true);
  rt.send(  rt.make(Result));

}

// wxGenericDirCtrl::GetPath
void wxGenericDirCtrl_GetPath_1(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd)
{
  ErlNifEnv *env = Ecmd.env;
  ERL_NIF_TERM * argv = Ecmd.args;
  wxGenericDirCtrl *This;
  This = (wxGenericDirCtrl *) memenv->getPtr(env, argv[0], "This");
  ErlNifUInt64 itemId_tmp;
  if(!enif_get_uint64(env, argv[1], &itemId_tmp)) Badarg("itemId");
  wxTreeItemId itemId = wxTreeItemId((void *) (wxUint64) itemId_tmp);
  if(!This) throw wxe_badarg("This");
  wxString Result = This->GetPath(itemId);
  wxeReturn rt = wxeReturn(memenv, Ecmd.caller, true);
  rt.send(  rt.make(Result));

}

// wxGenericDirCtrl::GetFilePath
void wxGenericDirCtrl_GetFilePath(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd)
{
  ErlNifEnv *env = Ecmd.env;
  ERL_NIF_TERM * argv = Ecmd.args;
  wxGenericDirCtrl *This;
  This = (wxGenericDirCtrl *) memenv->getPtr(env, argv[0], "This");
  if(!This) throw wxe_badarg("This");
  wxString Result = This->GetFilePath();
  wxeReturn rt = wxeReturn(memenv, Ecmd.caller, true);
  rt.send(  rt.make(Result));

}

// wxGenericDirCtrl::GetFilter
void wxGenericDirCtrl_GetFilter(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd)
{
  ErlNifEnv *env = Ecmd.env;
  ERL_NIF_TERM * argv = Ecmd.args;
  wxGenericDirCtrl *This;
  This = (wxGenericDirCtrl *) memenv->getPtr(env, argv[0], "This");
  if(!This) throw wxe_badarg("This");
  wxString Result = This->GetFilter();
  wxeReturn rt = wxeReturn(memenv, Ecmd.caller, true);
  rt.send(  rt.make(Result));

}

// wxGenericDirCtrl::GetFilterIndex
void wxGenericDirCtrl_GetFilterIndex(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd)
{
  ErlNifEnv *env = Ecmd.env;
  ERL_NIF_TERM * argv = Ecmd.args;
  wxGenericDirCtrl *This;
  This = (wxGenericDirCtrl *) memenv->getPtr(env, argv[0], "This");
  if(!This) throw wxe_badarg("This");
  int Result = This->GetFilterIndex();
  wxeReturn rt = wxeReturn(memenv, Ecmd.caller, true);
  rt.send(  rt.make_int(Result));

}

// wxGenericDirCtrl::GetRootId
void wxGenericDirCtrl_GetRootId(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd)
{
  ErlNifEnv *env = Ecmd.env;
  ERL_NIF_TERM * argv = Ecmd.args;
  wxGenericDirCtrl *This;
  This = (wxGenericDirCtrl *) memenv->getPtr(env, argv[0], "This");
  if(!This) throw wxe_badarg("This");
  wxTreeItemId Result = This->GetRootId();
  wxeReturn rt = wxeReturn(memenv, Ecmd.caller, true);
  rt.send(  rt.make((wxUIntPtr *) Result.m_pItem));

}

// wxGenericDirCtrl::GetTreeCtrl
void wxGenericDirCtrl_GetTreeCtrl(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd)
{
  ErlNifEnv *env = Ecmd.env;
  ERL_NIF_TERM * argv = Ecmd.args;
  wxGenericDirCtrl *This;
  This = (wxGenericDirCtrl *) memenv->getPtr(env, argv[0], "This");
  if(!This) throw wxe_badarg("This");
  wxTreeCtrl * Result = (wxTreeCtrl*)This->GetTreeCtrl();
  wxeReturn rt = wxeReturn(memenv, Ecmd.caller, true);
  rt.send(  rt.make_ref(app->getRef((void *)Result,memenv), "wxTreeCtrl"));

}

// wxGenericDirCtrl::ReCreateTree
void wxGenericDirCtrl_ReCreateTree(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd)
{
  ErlNifEnv *env = Ecmd.env;
  ERL_NIF_TERM * argv = Ecmd.args;
  wxGenericDirCtrl *This;
  This = (wxGenericDirCtrl *) memenv->getPtr(env, argv[0], "This");
  if(!This) throw wxe_badarg("This");
  This->ReCreateTree();

}

// wxGenericDirCtrl::SetDefaultPath
void wxGenericDirCtrl_SetDefaultPath(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd)
{
  ErlNifEnv *env = Ecmd.env;
  ERL_NIF_TERM * argv = Ecmd.args;
  wxGenericDirCtrl *This;
  This = (wxGenericDirCtrl *) memenv->getPtr(env, argv[0], "This");
  ErlNifBinary path_bin;
  wxString path;
  if(!enif_inspect_binary(env, argv[1], &path_bin)) Badarg("path");
  path = wxString(path_bin.data, wxConvUTF8, path_bin.size);
  if(!This) throw wxe_badarg("This");
  This->SetDefaultPath(path);

}

// wxGenericDirCtrl::SetFilter
void wxGenericDirCtrl_SetFilter(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd)
{
  ErlNifEnv *env = Ecmd.env;
  ERL_NIF_TERM * argv = Ecmd.args;
  wxGenericDirCtrl *This;
  This = (wxGenericDirCtrl *) memenv->getPtr(env, argv[0], "This");
  ErlNifBinary filter_bin;
  wxString filter;
  if(!enif_inspect_binary(env, argv[1], &filter_bin)) Badarg("filter");
  filter = wxString(filter_bin.data, wxConvUTF8, filter_bin.size);
  if(!This) throw wxe_badarg("This");
  This->SetFilter(filter);

}

// wxGenericDirCtrl::SetFilterIndex
void wxGenericDirCtrl_SetFilterIndex(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd)
{
  ErlNifEnv *env = Ecmd.env;
  ERL_NIF_TERM * argv = Ecmd.args;
  wxGenericDirCtrl *This;
  This = (wxGenericDirCtrl *) memenv->getPtr(env, argv[0], "This");
  int n;
  if(!enif_get_int(env, argv[1], &n)) Badarg("n"); // int
  if(!This) throw wxe_badarg("This");
  This->SetFilterIndex(n);

}

// wxGenericDirCtrl::SetPath
void wxGenericDirCtrl_SetPath(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd)
{
  ErlNifEnv *env = Ecmd.env;
  ERL_NIF_TERM * argv = Ecmd.args;
  wxGenericDirCtrl *This;
  This = (wxGenericDirCtrl *) memenv->getPtr(env, argv[0], "This");
  ErlNifBinary path_bin;
  wxString path;
  if(!enif_inspect_binary(env, argv[1], &path_bin)) Badarg("path");
  path = wxString(path_bin.data, wxConvUTF8, path_bin.size);
  if(!This) throw wxe_badarg("This");
  This->SetPath(path);

}

#if wxUSE_GRAPHICS_CONTEXT
#endif // wxUSE_GRAPHICS_CONTEXT
#if wxUSE_GRAPHICS_CONTEXT
// wxGraphicsContext::Create
void wxGraphicsContext_Create_STAT_1(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd)
{
  ErlNifEnv *env = Ecmd.env;
  ERL_NIF_TERM * argv = Ecmd.args;
  ERL_NIF_TERM windowDC_type;
  void * windowDC = memenv->getPtr(env, argv[0], "windowDC", &windowDC_type);
  wxGraphicsContext * Result;
    if(enif_is_identical(windowDC_type, WXE_ATOM_wxWindowDC))
    Result =  (wxGraphicsContext*)wxGraphicsContext::Create(* static_cast<wxWindowDC*> (windowDC));
  else   if(enif_is_identical(windowDC_type, WXE_ATOM_wxWindow))
    Result =  (wxGraphicsContext*)wxGraphicsContext::Create(static_cast<wxWindow*> (windowDC));
  else   if(enif_is_identical(windowDC_type, WXE_ATOM_wxMemoryDC))
    Result =  (wxGraphicsContext*)wxGraphicsContext::Create(* static_cast<wxMemoryDC*> (windowDC));
  else   if(enif_is_identical(windowDC_type, WXE_ATOM_wxImage))
    Result =  (wxGraphicsContext*)wxGraphicsContext::Create(* static_cast<wxImage*> (windowDC));
  else throw wxe_badarg("windowDC");
  wxeReturn rt = wxeReturn(memenv, Ecmd.caller, true);
  rt.send(  rt.make_ref(app->getRef((void *)Result,memenv,8), "wxGraphicsContext")
);

}

// wxGraphicsContext::Create
void wxGraphicsContext_Create_STAT_0(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd)
{
  wxGraphicsContext * Result = (wxGraphicsContext*)wxGraphicsContext::Create();
  wxeReturn rt = wxeReturn(memenv, Ecmd.caller, true);
  rt.send(  rt.make_ref(app->getRef((void *)Result,memenv,8), "wxGraphicsContext")
);

}

// wxGraphicsContext::CreatePen
void wxGraphicsContext_CreatePen(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd)
{
  ErlNifEnv *env = Ecmd.env;
  ERL_NIF_TERM * argv = Ecmd.args;
  wxGraphicsContext *This;
  This = (wxGraphicsContext *) memenv->getPtr(env, argv[0], "This");
  wxPen *pen;
  pen = (wxPen *) memenv->getPtr(env, argv[1], "pen");
  if(!This) throw wxe_badarg("This");
  wxGraphicsPen * Result = new wxGraphicsPen(This->CreatePen(*pen)); app->newPtr((void *) Result,4, memenv);;
  wxeReturn rt = wxeReturn(memenv, Ecmd.caller, true);
  rt.send(  rt.make_ref(app->getRef((void *)Result,memenv), "wxGraphicsPen"));

}

// wxGraphicsContext::CreateBrush
void wxGraphicsContext_CreateBrush(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd)
{
  ErlNifEnv *env = Ecmd.env;
  ERL_NIF_TERM * argv = Ecmd.args;
  wxGraphicsContext *This;
  This = (wxGraphicsContext *) memenv->getPtr(env, argv[0], "This");
  wxBrush *brush;
  brush = (wxBrush *) memenv->getPtr(env, argv[1], "brush");
  if(!This) throw wxe_badarg("This");
  wxGraphicsBrush * Result = new wxGraphicsBrush(This->CreateBrush(*brush)); app->newPtr((void *) Result,4, memenv);;
  wxeReturn rt = wxeReturn(memenv, Ecmd.caller, true);
  rt.send(  rt.make_ref(app->getRef((void *)Result,memenv), "wxGraphicsBrush"));

}

// wxGraphicsContext::CreateRadialGradientBrush
void wxGraphicsContext_CreateRadialGradientBrush_7(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd)
{
  ErlNifEnv *env = Ecmd.env;
  ERL_NIF_TERM * argv = Ecmd.args;
  wxGraphicsContext *This;
  This = (wxGraphicsContext *) memenv->getPtr(env, argv[0], "This");
  double startX;
  if(!wxe_get_double(env, argv[1], &startX)) Badarg("startX");
  double startY;
  if(!wxe_get_double(env, argv[2], &startY)) Badarg("startY");
  double endX;
  if(!wxe_get_double(env, argv[3], &endX)) Badarg("endX");
  double endY;
  if(!wxe_get_double(env, argv[4], &endY)) Badarg("endY");
  double radius;
  if(!wxe_get_double(env, argv[5], &radius)) Badarg("radius");
  const ERL_NIF_TERM *oColor_t;
  int oColor_sz;
  if(!enif_get_tuple(env, argv[6], &oColor_sz, &oColor_t)) Badarg("oColor");
  int oColorR;
  if(!enif_get_int(env, oColor_t[0], &oColorR)) Badarg("oColor");
  int oColorG;
  if(!enif_get_int(env, oColor_t[1], &oColorG)) Badarg("oColor");
  int oColorB;
  if(!enif_get_int(env, oColor_t[2], &oColorB)) Badarg("oColor");
  int oColorA;
  if(!enif_get_int(env, oColor_t[3], &oColorA)) Badarg("oColor");
  wxColour oColor = wxColour(oColorR,oColorG,oColorB,oColorA);
  const ERL_NIF_TERM *cColor_t;
  int cColor_sz;
  if(!enif_get_tuple(env, argv[7], &cColor_sz, &cColor_t)) Badarg("cColor");
  int cColorR;
  if(!enif_get_int(env, cColor_t[0], &cColorR)) Badarg("cColor");
  int cColorG;
  if(!enif_get_int(env, cColor_t[1], &cColorG)) Badarg("cColor");
  int cColorB;
  if(!enif_get_int(env, cColor_t[2], &cColorB)) Badarg("cColor");
  int cColorA;
  if(!enif_get_int(env, cColor_t[3], &cColorA)) Badarg("cColor");
  wxColour cColor = wxColour(cColorR,cColorG,cColorB,cColorA);
  if(!This) throw wxe_badarg("This");
  wxGraphicsBrush * Result = new wxGraphicsBrush(This->CreateRadialGradientBrush(startX,startY,endX,endY,radius,oColor,cColor)); app->newPtr((void *) Result,4, memenv);;
  wxeReturn rt = wxeReturn(memenv, Ecmd.caller, true);
  rt.send(  rt.make_ref(app->getRef((void *)Result,memenv), "wxGraphicsBrush"));

}

// wxGraphicsContext::CreateRadialGradientBrush
void wxGraphicsContext_CreateRadialGradientBrush_6(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd)
{
  ErlNifEnv *env = Ecmd.env;
  ERL_NIF_TERM * argv = Ecmd.args;
  wxGraphicsContext *This;
  This = (wxGraphicsContext *) memenv->getPtr(env, argv[0], "This");
  double startX;
  if(!wxe_get_double(env, argv[1], &startX)) Badarg("startX");
  double startY;
  if(!wxe_get_double(env, argv[2], &startY)) Badarg("startY");
  double endX;
  if(!wxe_get_double(env, argv[3], &endX)) Badarg("endX");
  double endY;
  if(!wxe_get_double(env, argv[4], &endY)) Badarg("endY");
  double radius;
  if(!wxe_get_double(env, argv[5], &radius)) Badarg("radius");
  wxGraphicsGradientStops *stops;
  stops = (wxGraphicsGradientStops *) memenv->getPtr(env, argv[6], "stops");
  if(!This) throw wxe_badarg("This");
  wxGraphicsBrush * Result = new wxGraphicsBrush(This->CreateRadialGradientBrush(startX,startY,endX,endY,radius,*stops)); app->newPtr((void *) Result,4, memenv);;
  wxeReturn rt = wxeReturn(memenv, Ecmd.caller, true);
  rt.send(  rt.make_ref(app->getRef((void *)Result,memenv), "wxGraphicsBrush"));

}

// wxGraphicsContext::CreateLinearGradientBrush
void wxGraphicsContext_CreateLinearGradientBrush_6(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd)
{
  ErlNifEnv *env = Ecmd.env;
  ERL_NIF_TERM * argv = Ecmd.args;
  wxGraphicsContext *This;
  This = (wxGraphicsContext *) memenv->getPtr(env, argv[0], "This");
  double x1;
  if(!wxe_get_double(env, argv[1], &x1)) Badarg("x1");
  double y1;
  if(!wxe_get_double(env, argv[2], &y1)) Badarg("y1");
  double x2;
  if(!wxe_get_double(env, argv[3], &x2)) Badarg("x2");
  double y2;
  if(!wxe_get_double(env, argv[4], &y2)) Badarg("y2");
  const ERL_NIF_TERM *c1_t;
  int c1_sz;
  if(!enif_get_tuple(env, argv[5], &c1_sz, &c1_t)) Badarg("c1");
  int c1R;
  if(!enif_get_int(env, c1_t[0], &c1R)) Badarg("c1");
  int c1G;
  if(!enif_get_int(env, c1_t[1], &c1G)) Badarg("c1");
  int c1B;
  if(!enif_get_int(env, c1_t[2], &c1B)) Badarg("c1");
  int c1A;
  if(!enif_get_int(env, c1_t[3], &c1A)) Badarg("c1");
  wxColour c1 = wxColour(c1R,c1G,c1B,c1A);
  const ERL_NIF_TERM *c2_t;
  int c2_sz;
  if(!enif_get_tuple(env, argv[6], &c2_sz, &c2_t)) Badarg("c2");
  int c2R;
  if(!enif_get_int(env, c2_t[0], &c2R)) Badarg("c2");
  int c2G;
  if(!enif_get_int(env, c2_t[1], &c2G)) Badarg("c2");
  int c2B;
  if(!enif_get_int(env, c2_t[2], &c2B)) Badarg("c2");
  int c2A;
  if(!enif_get_int(env, c2_t[3], &c2A)) Badarg("c2");
  wxColour c2 = wxColour(c2R,c2G,c2B,c2A);
  if(!This) throw wxe_badarg("This");
  wxGraphicsBrush * Result = new wxGraphicsBrush(This->CreateLinearGradientBrush(x1,y1,x2,y2,c1,c2)); app->newPtr((void *) Result,4, memenv);;
  wxeReturn rt = wxeReturn(memenv, Ecmd.caller, true);
  rt.send(  rt.make_ref(app->getRef((void *)Result,memenv), "wxGraphicsBrush"));

}

// wxGraphicsContext::CreateLinearGradientBrush
void wxGraphicsContext_CreateLinearGradientBrush_5(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd)
{
  ErlNifEnv *env = Ecmd.env;
  ERL_NIF_TERM * argv = Ecmd.args;
  wxGraphicsContext *This;
  This = (wxGraphicsContext *) memenv->getPtr(env, argv[0], "This");
  double x1;
  if(!wxe_get_double(env, argv[1], &x1)) Badarg("x1");
  double y1;
  if(!wxe_get_double(env, argv[2], &y1)) Badarg("y1");
  double x2;
  if(!wxe_get_double(env, argv[3], &x2)) Badarg("x2");
  double y2;
  if(!wxe_get_double(env, argv[4], &y2)) Badarg("y2");
  wxGraphicsGradientStops *stops;
  stops = (wxGraphicsGradientStops *) memenv->getPtr(env, argv[5], "stops");
  if(!This) throw wxe_badarg("This");
  wxGraphicsBrush * Result = new wxGraphicsBrush(This->CreateLinearGradientBrush(x1,y1,x2,y2,*stops)); app->newPtr((void *) Result,4, memenv);;
  wxeReturn rt = wxeReturn(memenv, Ecmd.caller, true);
  rt.send(  rt.make_ref(app->getRef((void *)Result,memenv), "wxGraphicsBrush"));

}

// wxGraphicsContext::CreateFont
void wxGraphicsContext_CreateFont_2(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd)
{
  wxColour col= *wxBLACK;
  ErlNifEnv *env = Ecmd.env;
  ERL_NIF_TERM * argv = Ecmd.args;
  wxGraphicsContext *This;
  This = (wxGraphicsContext *) memenv->getPtr(env, argv[0], "This");
  wxFont *font;
  font = (wxFont *) memenv->getPtr(env, argv[1], "font");
  ERL_NIF_TERM lstHead, lstTail;
  lstTail = argv[2];
  if(!enif_is_list(env, lstTail)) Badarg("Options");
  const ERL_NIF_TERM *tpl;
  int tpl_sz;
  while(!enif_is_empty_list(env, lstTail)) {
    if(!enif_get_list_cell(env, lstTail, &lstHead, &lstTail)) Badarg("Options");
    if(!enif_get_tuple(env, lstHead, &tpl_sz, &tpl) || tpl_sz != 2) Badarg("Options");
    if(enif_is_identical(tpl[0], enif_make_atom(env, "col"))) {
  const ERL_NIF_TERM *col_t;
  int col_sz;
  if(!enif_get_tuple(env, tpl[1], &col_sz, &col_t)) Badarg("col");
  int colR;
  if(!enif_get_int(env, col_t[0], &colR)) Badarg("col");
  int colG;
  if(!enif_get_int(env, col_t[1], &colG)) Badarg("col");
  int colB;
  if(!enif_get_int(env, col_t[2], &colB)) Badarg("col");
  int colA;
  if(!enif_get_int(env, col_t[3], &colA)) Badarg("col");
  col = wxColour(colR,colG,colB,colA);
    } else        Badarg("Options");
  };
  if(!This) throw wxe_badarg("This");
  wxGraphicsFont * Result = new wxGraphicsFont(This->CreateFont(*font,col)); app->newPtr((void *) Result,4, memenv);;
  wxeReturn rt = wxeReturn(memenv, Ecmd.caller, true);
  rt.send(  rt.make_ref(app->getRef((void *)Result,memenv), "wxGraphicsFont"));

}

// wxGraphicsContext::CreateFont
void wxGraphicsContext_CreateFont_3(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd)
{
  int flags=wxFONTFLAG_DEFAULT;
  wxColour col= *wxBLACK;
  ErlNifEnv *env = Ecmd.env;
  ERL_NIF_TERM * argv = Ecmd.args;
  wxGraphicsContext *This;
  This = (wxGraphicsContext *) memenv->getPtr(env, argv[0], "This");
  double sizeInPixels;
  if(!wxe_get_double(env, argv[1], &sizeInPixels)) Badarg("sizeInPixels");
  ErlNifBinary facename_bin;
  wxString facename;
  if(!enif_inspect_binary(env, argv[2], &facename_bin)) Badarg("facename");
  facename = wxString(facename_bin.data, wxConvUTF8, facename_bin.size);
  ERL_NIF_TERM lstHead, lstTail;
  lstTail = argv[3];
  if(!enif_is_list(env, lstTail)) Badarg("Options");
  const ERL_NIF_TERM *tpl;
  int tpl_sz;
  while(!enif_is_empty_list(env, lstTail)) {
    if(!enif_get_list_cell(env, lstTail, &lstHead, &lstTail)) Badarg("Options");
    if(!enif_get_tuple(env, lstHead, &tpl_sz, &tpl) || tpl_sz != 2) Badarg("Options");
    if(enif_is_identical(tpl[0], enif_make_atom(env, "flags"))) {
  if(!enif_get_int(env, tpl[1], &flags)) Badarg("flags"); // int
    } else     if(enif_is_identical(tpl[0], enif_make_atom(env, "col"))) {
  const ERL_NIF_TERM *col_t;
  int col_sz;
  if(!enif_get_tuple(env, tpl[1], &col_sz, &col_t)) Badarg("col");
  int colR;
  if(!enif_get_int(env, col_t[0], &colR)) Badarg("col");
  int colG;
  if(!enif_get_int(env, col_t[1], &colG)) Badarg("col");
  int colB;
  if(!enif_get_int(env, col_t[2], &colB)) Badarg("col");
  int colA;
  if(!enif_get_int(env, col_t[3], &colA)) Badarg("col");
  col = wxColour(colR,colG,colB,colA);
    } else        Badarg("Options");
  };
  if(!This) throw wxe_badarg("This");
  wxGraphicsFont * Result = new wxGraphicsFont(This->CreateFont(sizeInPixels,facename,flags,col)); app->newPtr((void *) Result,4, memenv);;
  wxeReturn rt = wxeReturn(memenv, Ecmd.caller, true);
  rt.send(  rt.make_ref(app->getRef((void *)Result,memenv), "wxGraphicsFont"));

}

// wxGraphicsContext::CreateMatrix
void wxGraphicsContext_CreateMatrix(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd)
{
  wxDouble a=1.0;
  wxDouble b=0.0;
  wxDouble c=0.0;
  wxDouble d=1.0;
  wxDouble tx=0.0;
  wxDouble ty=0.0;
  ErlNifEnv *env = Ecmd.env;
  ERL_NIF_TERM * argv = Ecmd.args;
  wxGraphicsContext *This;
  This = (wxGraphicsContext *) memenv->getPtr(env, argv[0], "This");
  ERL_NIF_TERM lstHead, lstTail;
  lstTail = argv[1];
  if(!enif_is_list(env, lstTail)) Badarg("Options");
  const ERL_NIF_TERM *tpl;
  int tpl_sz;
  while(!enif_is_empty_list(env, lstTail)) {
    if(!enif_get_list_cell(env, lstTail, &lstHead, &lstTail)) Badarg("Options");
    if(!enif_get_tuple(env, lstHead, &tpl_sz, &tpl) || tpl_sz != 2) Badarg("Options");
    if(enif_is_identical(tpl[0], enif_make_atom(env, "a"))) {
  if(!wxe_get_double(env, tpl[1], &a)) Badarg("a");
    } else     if(enif_is_identical(tpl[0], enif_make_atom(env, "b"))) {
  if(!wxe_get_double(env, tpl[1], &b)) Badarg("b");
    } else     if(enif_is_identical(tpl[0], enif_make_atom(env, "c"))) {
  if(!wxe_get_double(env, tpl[1], &c)) Badarg("c");
    } else     if(enif_is_identical(tpl[0], enif_make_atom(env, "d"))) {
  if(!wxe_get_double(env, tpl[1], &d)) Badarg("d");
    } else     if(enif_is_identical(tpl[0], enif_make_atom(env, "tx"))) {
  if(!wxe_get_double(env, tpl[1], &tx)) Badarg("tx");
    } else     if(enif_is_identical(tpl[0], enif_make_atom(env, "ty"))) {
  if(!wxe_get_double(env, tpl[1], &ty)) Badarg("ty");
    } else        Badarg("Options");
  };
  if(!This) throw wxe_badarg("This");
  wxGraphicsMatrix * Result = new wxGraphicsMatrix(This->CreateMatrix(a,b,c,d,tx,ty)); app->newPtr((void *) Result,4, memenv);;
  wxeReturn rt = wxeReturn(memenv, Ecmd.caller, true);
  rt.send(  rt.make_ref(app->getRef((void *)Result,memenv), "wxGraphicsMatrix"));

}

// wxGraphicsContext::CreatePath
void wxGraphicsContext_CreatePath(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd)
{
  ErlNifEnv *env = Ecmd.env;
  ERL_NIF_TERM * argv = Ecmd.args;
  wxGraphicsContext *This;
  This = (wxGraphicsContext *) memenv->getPtr(env, argv[0], "This");
  if(!This) throw wxe_badarg("This");
  wxGraphicsPath * Result = new wxGraphicsPath(This->CreatePath()); app->newPtr((void *) Result,4, memenv);;
  wxeReturn rt = wxeReturn(memenv, Ecmd.caller, true);
  rt.send(  rt.make_ref(app->getRef((void *)Result,memenv), "wxGraphicsPath"));

}

// wxGraphicsContext::Clip
void wxGraphicsContext_Clip_1(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd)
{
  ErlNifEnv *env = Ecmd.env;
  ERL_NIF_TERM * argv = Ecmd.args;
  wxGraphicsContext *This;
  This = (wxGraphicsContext *) memenv->getPtr(env, argv[0], "This");
  wxRegion *region;
  region = (wxRegion *) memenv->getPtr(env, argv[1], "region");
  if(!This) throw wxe_badarg("This");
  This->Clip(*region);

}

// wxGraphicsContext::Clip
void wxGraphicsContext_Clip_4(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd)
{
  ErlNifEnv *env = Ecmd.env;
  ERL_NIF_TERM * argv = Ecmd.args;
  wxGraphicsContext *This;
  This = (wxGraphicsContext *) memenv->getPtr(env, argv[0], "This");
  double x;
  if(!wxe_get_double(env, argv[1], &x)) Badarg("x");
  double y;
  if(!wxe_get_double(env, argv[2], &y)) Badarg("y");
  double w;
  if(!wxe_get_double(env, argv[3], &w)) Badarg("w");
  double h;
  if(!wxe_get_double(env, argv[4], &h)) Badarg("h");
  if(!This) throw wxe_badarg("This");
  This->Clip(x,y,w,h);

}

// wxGraphicsContext::ResetClip
void wxGraphicsContext_ResetClip(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd)
{
  ErlNifEnv *env = Ecmd.env;
  ERL_NIF_TERM * argv = Ecmd.args;
  wxGraphicsContext *This;
  This = (wxGraphicsContext *) memenv->getPtr(env, argv[0], "This");
  if(!This) throw wxe_badarg("This");
  This->ResetClip();

}

// wxGraphicsContext::DrawBitmap
void wxGraphicsContext_DrawBitmap(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd)
{
  ErlNifEnv *env = Ecmd.env;
  ERL_NIF_TERM * argv = Ecmd.args;
  wxGraphicsContext *This;
  This = (wxGraphicsContext *) memenv->getPtr(env, argv[0], "This");
  wxBitmap *bmp;
  bmp = (wxBitmap *) memenv->getPtr(env, argv[1], "bmp");
  double x;
  if(!wxe_get_double(env, argv[2], &x)) Badarg("x");
  double y;
  if(!wxe_get_double(env, argv[3], &y)) Badarg("y");
  double w;
  if(!wxe_get_double(env, argv[4], &w)) Badarg("w");
  double h;
  if(!wxe_get_double(env, argv[5], &h)) Badarg("h");
  if(!This) throw wxe_badarg("This");
  This->DrawBitmap(*bmp,x,y,w,h);

}

// wxGraphicsContext::DrawEllipse
void wxGraphicsContext_DrawEllipse(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd)
{
  ErlNifEnv *env = Ecmd.env;
  ERL_NIF_TERM * argv = Ecmd.args;
  wxGraphicsContext *This;
  This = (wxGraphicsContext *) memenv->getPtr(env, argv[0], "This");
  double x;
  if(!wxe_get_double(env, argv[1], &x)) Badarg("x");
  double y;
  if(!wxe_get_double(env, argv[2], &y)) Badarg("y");
  double w;
  if(!wxe_get_double(env, argv[3], &w)) Badarg("w");
  double h;
  if(!wxe_get_double(env, argv[4], &h)) Badarg("h");
  if(!This) throw wxe_badarg("This");
  This->DrawEllipse(x,y,w,h);

}

// wxGraphicsContext::DrawIcon
void wxGraphicsContext_DrawIcon(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd)
{
  ErlNifEnv *env = Ecmd.env;
  ERL_NIF_TERM * argv = Ecmd.args;
  wxGraphicsContext *This;
  This = (wxGraphicsContext *) memenv->getPtr(env, argv[0], "This");
  wxIcon *icon;
  icon = (wxIcon *) memenv->getPtr(env, argv[1], "icon");
  double x;
  if(!wxe_get_double(env, argv[2], &x)) Badarg("x");
  double y;
  if(!wxe_get_double(env, argv[3], &y)) Badarg("y");
  double w;
  if(!wxe_get_double(env, argv[4], &w)) Badarg("w");
  double h;
  if(!wxe_get_double(env, argv[5], &h)) Badarg("h");
  if(!This) throw wxe_badarg("This");
  This->DrawIcon(*icon,x,y,w,h);

}

// wxGraphicsContext::DrawLines
void wxGraphicsContext_DrawLines(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd)
{
 wxPolygonFillMode fillStyle=wxODDEVEN_RULE;
  ErlNifEnv *env = Ecmd.env;
  ERL_NIF_TERM * argv = Ecmd.args;
  wxGraphicsContext *This;
  This = (wxGraphicsContext *) memenv->getPtr(env, argv[0], "This");
  unsigned pointsLen;
  ERL_NIF_TERM pointsHead, pointsTail;
  const ERL_NIF_TERM *points_tpl;
  int points_tsz;
  if(!enif_get_list_length(env, argv[1], &pointsLen)) Badarg("points");
  std::vector <wxPoint2DDouble> points;
  double x, y;
  pointsTail = argv[1];
  while(!enif_is_empty_list(env, pointsTail)) {
    if(!enif_get_list_cell(env, pointsTail, &pointsHead, &pointsTail)) Badarg("points");
    if(!enif_get_tuple(env, pointsHead, &points_tsz, &points_tpl) || points_tsz != 2) Badarg("points");
    if(!wxe_get_double(env, points_tpl[0], &x)) Badarg("points");
    if(!wxe_get_double(env, points_tpl[1], &y)) Badarg("points");
    points.push_back(wxPoint2DDouble(x,y));
  };
  ERL_NIF_TERM lstHead, lstTail;
  lstTail = argv[2];
  if(!enif_is_list(env, lstTail)) Badarg("Options");
  const ERL_NIF_TERM *tpl;
  int tpl_sz;
  while(!enif_is_empty_list(env, lstTail)) {
    if(!enif_get_list_cell(env, lstTail, &lstHead, &lstTail)) Badarg("Options");
    if(!enif_get_tuple(env, lstHead, &tpl_sz, &tpl) || tpl_sz != 2) Badarg("Options");
    if(enif_is_identical(tpl[0], enif_make_atom(env, "fillStyle"))) {
  if(!enif_get_int(env, tpl[1], (int *) &fillStyle)) Badarg("fillStyle"); // enum
    } else        Badarg("Options");
  };
  if(!This) throw wxe_badarg("This");
  This->DrawLines(pointsLen,points.data(),fillStyle);

}

// wxGraphicsContext::DrawPath
void wxGraphicsContext_DrawPath(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd)
{
 wxPolygonFillMode fillStyle=wxODDEVEN_RULE;
  ErlNifEnv *env = Ecmd.env;
  ERL_NIF_TERM * argv = Ecmd.args;
  wxGraphicsContext *This;
  This = (wxGraphicsContext *) memenv->getPtr(env, argv[0], "This");
  wxGraphicsPath *path;
  path = (wxGraphicsPath *) memenv->getPtr(env, argv[1], "path");
  ERL_NIF_TERM lstHead, lstTail;
  lstTail = argv[2];
  if(!enif_is_list(env, lstTail)) Badarg("Options");
  const ERL_NIF_TERM *tpl;
  int tpl_sz;
  while(!enif_is_empty_list(env, lstTail)) {
    if(!enif_get_list_cell(env, lstTail, &lstHead, &lstTail)) Badarg("Options");
    if(!enif_get_tuple(env, lstHead, &tpl_sz, &tpl) || tpl_sz != 2) Badarg("Options");
    if(enif_is_identical(tpl[0], enif_make_atom(env, "fillStyle"))) {
  if(!enif_get_int(env, tpl[1], (int *) &fillStyle)) Badarg("fillStyle"); // enum
    } else        Badarg("Options");
  };
  if(!This) throw wxe_badarg("This");
  This->DrawPath(*path,fillStyle);

}

// wxGraphicsContext::DrawRectangle
void wxGraphicsContext_DrawRectangle(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd)
{
  ErlNifEnv *env = Ecmd.env;
  ERL_NIF_TERM * argv = Ecmd.args;
  wxGraphicsContext *This;
  This = (wxGraphicsContext *) memenv->getPtr(env, argv[0], "This");
  double x;
  if(!wxe_get_double(env, argv[1], &x)) Badarg("x");
  double y;
  if(!wxe_get_double(env, argv[2], &y)) Badarg("y");
  double w;
  if(!wxe_get_double(env, argv[3], &w)) Badarg("w");
  double h;
  if(!wxe_get_double(env, argv[4], &h)) Badarg("h");
  if(!This) throw wxe_badarg("This");
  This->DrawRectangle(x,y,w,h);

}

// wxGraphicsContext::DrawRoundedRectangle
void wxGraphicsContext_DrawRoundedRectangle(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd)
{
  ErlNifEnv *env = Ecmd.env;
  ERL_NIF_TERM * argv = Ecmd.args;
  wxGraphicsContext *This;
  This = (wxGraphicsContext *) memenv->getPtr(env, argv[0], "This");
  double x;
  if(!wxe_get_double(env, argv[1], &x)) Badarg("x");
  double y;
  if(!wxe_get_double(env, argv[2], &y)) Badarg("y");
  double w;
  if(!wxe_get_double(env, argv[3], &w)) Badarg("w");
  double h;
  if(!wxe_get_double(env, argv[4], &h)) Badarg("h");
  double radius;
  if(!wxe_get_double(env, argv[5], &radius)) Badarg("radius");
  if(!This) throw wxe_badarg("This");
  This->DrawRoundedRectangle(x,y,w,h,radius);

}

// wxGraphicsContext::DrawText
void wxGraphicsContext_DrawText_3(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd)
{
  ErlNifEnv *env = Ecmd.env;
  ERL_NIF_TERM * argv = Ecmd.args;
  wxGraphicsContext *This;
  This = (wxGraphicsContext *) memenv->getPtr(env, argv[0], "This");
  ErlNifBinary str_bin;
  wxString str;
  if(!enif_inspect_binary(env, argv[1], &str_bin)) Badarg("str");
  str = wxString(str_bin.data, wxConvUTF8, str_bin.size);
  double x;
  if(!wxe_get_double(env, argv[2], &x)) Badarg("x");
  double y;
  if(!wxe_get_double(env, argv[3], &y)) Badarg("y");
  if(!This) throw wxe_badarg("This");
  This->DrawText(str,x,y);

}

// wxGraphicsContext::DrawText
void wxGraphicsContext_DrawText_4_0(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd)
{
  ErlNifEnv *env = Ecmd.env;
  ERL_NIF_TERM * argv = Ecmd.args;
  wxGraphicsContext *This;
  This = (wxGraphicsContext *) memenv->getPtr(env, argv[0], "This");
  ErlNifBinary str_bin;
  wxString str;
  if(!enif_inspect_binary(env, argv[1], &str_bin)) Badarg("str");
  str = wxString(str_bin.data, wxConvUTF8, str_bin.size);
  double x;
  if(!wxe_get_double(env, argv[2], &x)) Badarg("x");
  double y;
  if(!wxe_get_double(env, argv[3], &y)) Badarg("y");
  double angle;
  if(!wxe_get_double(env, argv[4], &angle)) Badarg("angle");
  if(!This) throw wxe_badarg("This");
  This->DrawText(str,x,y,angle);

}

// wxGraphicsContext::DrawText
void wxGraphicsContext_DrawText_4_1(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd)
{
  ErlNifEnv *env = Ecmd.env;
  ERL_NIF_TERM * argv = Ecmd.args;
  wxGraphicsContext *This;
  This = (wxGraphicsContext *) memenv->getPtr(env, argv[0], "This");
  ErlNifBinary str_bin;
  wxString str;
  if(!enif_inspect_binary(env, argv[1], &str_bin)) Badarg("str");
  str = wxString(str_bin.data, wxConvUTF8, str_bin.size);
  double x;
  if(!wxe_get_double(env, argv[2], &x)) Badarg("x");
  double y;
  if(!wxe_get_double(env, argv[3], &y)) Badarg("y");
  wxGraphicsBrush *backgroundBrush;
  backgroundBrush = (wxGraphicsBrush *) memenv->getPtr(env, argv[4], "backgroundBrush");
  if(!This) throw wxe_badarg("This");
  This->DrawText(str,x,y,*backgroundBrush);

}

// wxGraphicsContext::DrawText
void wxGraphicsContext_DrawText_5(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd)
{
  ErlNifEnv *env = Ecmd.env;
  ERL_NIF_TERM * argv = Ecmd.args;
  wxGraphicsContext *This;
  This = (wxGraphicsContext *) memenv->getPtr(env, argv[0], "This");
  ErlNifBinary str_bin;
  wxString str;
  if(!enif_inspect_binary(env, argv[1], &str_bin)) Badarg("str");
  str = wxString(str_bin.data, wxConvUTF8, str_bin.size);
  double x;
  if(!wxe_get_double(env, argv[2], &x)) Badarg("x");
  double y;
  if(!wxe_get_double(env, argv[3], &y)) Badarg("y");
  double angle;
  if(!wxe_get_double(env, argv[4], &angle)) Badarg("angle");
  wxGraphicsBrush *backgroundBrush;
  backgroundBrush = (wxGraphicsBrush *) memenv->getPtr(env, argv[5], "backgroundBrush");
  if(!This) throw wxe_badarg("This");
  This->DrawText(str,x,y,angle,*backgroundBrush);

}

// wxGraphicsContext::FillPath
void wxGraphicsContext_FillPath(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd)
{
 wxPolygonFillMode fillStyle=wxODDEVEN_RULE;
  ErlNifEnv *env = Ecmd.env;
  ERL_NIF_TERM * argv = Ecmd.args;
  wxGraphicsContext *This;
  This = (wxGraphicsContext *) memenv->getPtr(env, argv[0], "This");
  wxGraphicsPath *path;
  path = (wxGraphicsPath *) memenv->getPtr(env, argv[1], "path");
  ERL_NIF_TERM lstHead, lstTail;
  lstTail = argv[2];
  if(!enif_is_list(env, lstTail)) Badarg("Options");
  const ERL_NIF_TERM *tpl;
  int tpl_sz;
  while(!enif_is_empty_list(env, lstTail)) {
    if(!enif_get_list_cell(env, lstTail, &lstHead, &lstTail)) Badarg("Options");
    if(!enif_get_tuple(env, lstHead, &tpl_sz, &tpl) || tpl_sz != 2) Badarg("Options");
    if(enif_is_identical(tpl[0], enif_make_atom(env, "fillStyle"))) {
  if(!enif_get_int(env, tpl[1], (int *) &fillStyle)) Badarg("fillStyle"); // enum
    } else        Badarg("Options");
  };
  if(!This) throw wxe_badarg("This");
  This->FillPath(*path,fillStyle);

}

// wxGraphicsContext::StrokePath
void wxGraphicsContext_StrokePath(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd)
{
  ErlNifEnv *env = Ecmd.env;
  ERL_NIF_TERM * argv = Ecmd.args;
  wxGraphicsContext *This;
  This = (wxGraphicsContext *) memenv->getPtr(env, argv[0], "This");
  wxGraphicsPath *path;
  path = (wxGraphicsPath *) memenv->getPtr(env, argv[1], "path");
  if(!This) throw wxe_badarg("This");
  This->StrokePath(*path);

}

// wxGraphicsContext::GetPartialTextExtents
void wxGraphicsContext_GetPartialTextExtents(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd)
{
  wxArrayDouble widths;
  ErlNifEnv *env = Ecmd.env;
  ERL_NIF_TERM * argv = Ecmd.args;
  wxGraphicsContext *This;
  This = (wxGraphicsContext *) memenv->getPtr(env, argv[0], "This");
  ErlNifBinary text_bin;
  wxString text;
  if(!enif_inspect_binary(env, argv[1], &text_bin)) Badarg("text");
  text = wxString(text_bin.data, wxConvUTF8, text_bin.size);
  if(!This) throw wxe_badarg("This");
  This->GetPartialTextExtents(text,widths);
  wxeReturn rt = wxeReturn(memenv, Ecmd.caller, true);
  rt.send(  rt.make(widths));

}

// wxGraphicsContext::GetTextExtent
void wxGraphicsContext_GetTextExtent(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd)
{
  wxDouble width;
  wxDouble height;
  wxDouble descent;
  wxDouble externalLeading;
  ErlNifEnv *env = Ecmd.env;
  ERL_NIF_TERM * argv = Ecmd.args;
  wxGraphicsContext *This;
  This = (wxGraphicsContext *) memenv->getPtr(env, argv[0], "This");
  ErlNifBinary text_bin;
  wxString text;
  if(!enif_inspect_binary(env, argv[1], &text_bin)) Badarg("text");
  text = wxString(text_bin.data, wxConvUTF8, text_bin.size);
  if(!This) throw wxe_badarg("This");
  This->GetTextExtent(text,&width,&height,&descent,&externalLeading);
  wxeReturn rt = wxeReturn(memenv, Ecmd.caller, true);
  ERL_NIF_TERM msg = enif_make_tuple4(rt.env,
  rt.make_double(width),
  rt.make_double(height),
  rt.make_double(descent),
  rt.make_double(externalLeading));
  rt.send(msg);

}

// wxGraphicsContext::Rotate
void wxGraphicsContext_Rotate(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd)
{
  ErlNifEnv *env = Ecmd.env;
  ERL_NIF_TERM * argv = Ecmd.args;
  wxGraphicsContext *This;
  This = (wxGraphicsContext *) memenv->getPtr(env, argv[0], "This");
  double angle;
  if(!wxe_get_double(env, argv[1], &angle)) Badarg("angle");
  if(!This) throw wxe_badarg("This");
  This->Rotate(angle);

}

// wxGraphicsContext::Scale
void wxGraphicsContext_Scale(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd)
{
  ErlNifEnv *env = Ecmd.env;
  ERL_NIF_TERM * argv = Ecmd.args;
  wxGraphicsContext *This;
  This = (wxGraphicsContext *) memenv->getPtr(env, argv[0], "This");
  double xScale;
  if(!wxe_get_double(env, argv[1], &xScale)) Badarg("xScale");
  double yScale;
  if(!wxe_get_double(env, argv[2], &yScale)) Badarg("yScale");
  if(!This) throw wxe_badarg("This");
  This->Scale(xScale,yScale);

}

// wxGraphicsContext::Translate
void wxGraphicsContext_Translate(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd)
{
  ErlNifEnv *env = Ecmd.env;
  ERL_NIF_TERM * argv = Ecmd.args;
  wxGraphicsContext *This;
  This = (wxGraphicsContext *) memenv->getPtr(env, argv[0], "This");
  double dx;
  if(!wxe_get_double(env, argv[1], &dx)) Badarg("dx");
  double dy;
  if(!wxe_get_double(env, argv[2], &dy)) Badarg("dy");
  if(!This) throw wxe_badarg("This");
  This->Translate(dx,dy);

}

// wxGraphicsContext::GetTransform
void wxGraphicsContext_GetTransform(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd)
{
  ErlNifEnv *env = Ecmd.env;
  ERL_NIF_TERM * argv = Ecmd.args;
  wxGraphicsContext *This;
  This = (wxGraphicsContext *) memenv->getPtr(env, argv[0], "This");
  if(!This) throw wxe_badarg("This");
  wxGraphicsMatrix * Result = new wxGraphicsMatrix(This->GetTransform()); app->newPtr((void *) Result,4, memenv);;
  wxeReturn rt = wxeReturn(memenv, Ecmd.caller, true);
  rt.send(  rt.make_ref(app->getRef((void *)Result,memenv), "wxGraphicsMatrix"));

}

// wxGraphicsContext::SetTransform
void wxGraphicsContext_SetTransform(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd)
{
  ErlNifEnv *env = Ecmd.env;
  ERL_NIF_TERM * argv = Ecmd.args;
  wxGraphicsContext *This;
  This = (wxGraphicsContext *) memenv->getPtr(env, argv[0], "This");
  wxGraphicsMatrix *matrix;
  matrix = (wxGraphicsMatrix *) memenv->getPtr(env, argv[1], "matrix");
  if(!This) throw wxe_badarg("This");
  This->SetTransform(*matrix);

}

// wxGraphicsContext::ConcatTransform
void wxGraphicsContext_ConcatTransform(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd)
{
  ErlNifEnv *env = Ecmd.env;
  ERL_NIF_TERM * argv = Ecmd.args;
  wxGraphicsContext *This;
  This = (wxGraphicsContext *) memenv->getPtr(env, argv[0], "This");
  wxGraphicsMatrix *matrix;
  matrix = (wxGraphicsMatrix *) memenv->getPtr(env, argv[1], "matrix");
  if(!This) throw wxe_badarg("This");
  This->ConcatTransform(*matrix);

}

// wxGraphicsContext::SetBrush
void wxGraphicsContext_SetBrush(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd)
{
  ErlNifEnv *env = Ecmd.env;
  ERL_NIF_TERM * argv = Ecmd.args;
  wxGraphicsContext *This;
  This = (wxGraphicsContext *) memenv->getPtr(env, argv[0], "This");
  ERL_NIF_TERM brush_type;
  void * brush = memenv->getPtr(env, argv[1], "brush", &brush_type);
  if(!This) throw wxe_badarg("This");
  if(enif_is_identical(brush_type, WXE_ATOM_wxGraphicsBrush))
   This->SetBrush(* static_cast<wxGraphicsBrush*> (brush));
  else if(enif_is_identical(brush_type, WXE_ATOM_wxBrush))
   This->SetBrush(* static_cast<wxBrush*> (brush));
  else throw wxe_badarg("brush");

}

// wxGraphicsContext::SetFont
void wxGraphicsContext_SetFont_2(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd)
{
  ErlNifEnv *env = Ecmd.env;
  ERL_NIF_TERM * argv = Ecmd.args;
  wxGraphicsContext *This;
  This = (wxGraphicsContext *) memenv->getPtr(env, argv[0], "This");
  wxFont *font;
  font = (wxFont *) memenv->getPtr(env, argv[1], "font");
  const ERL_NIF_TERM *colour_t;
  int colour_sz;
  if(!enif_get_tuple(env, argv[2], &colour_sz, &colour_t)) Badarg("colour");
  int colourR;
  if(!enif_get_int(env, colour_t[0], &colourR)) Badarg("colour");
  int colourG;
  if(!enif_get_int(env, colour_t[1], &colourG)) Badarg("colour");
  int colourB;
  if(!enif_get_int(env, colour_t[2], &colourB)) Badarg("colour");
  int colourA;
  if(!enif_get_int(env, colour_t[3], &colourA)) Badarg("colour");
  wxColour colour = wxColour(colourR,colourG,colourB,colourA);
  if(!This) throw wxe_badarg("This");
  This->SetFont(*font,colour);

}

// wxGraphicsContext::SetFont
void wxGraphicsContext_SetFont_1(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd)
{
  ErlNifEnv *env = Ecmd.env;
  ERL_NIF_TERM * argv = Ecmd.args;
  wxGraphicsContext *This;
  This = (wxGraphicsContext *) memenv->getPtr(env, argv[0], "This");
  wxGraphicsFont *font;
  font = (wxGraphicsFont *) memenv->getPtr(env, argv[1], "font");
  if(!This) throw wxe_badarg("This");
  This->SetFont(*font);

}

// wxGraphicsContext::SetPen
void wxGraphicsContext_SetPen(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd)
{
  ErlNifEnv *env = Ecmd.env;
  ERL_NIF_TERM * argv = Ecmd.args;
  wxGraphicsContext *This;
  This = (wxGraphicsContext *) memenv->getPtr(env, argv[0], "This");
  ERL_NIF_TERM pen_type;
  void * pen = memenv->getPtr(env, argv[1], "pen", &pen_type);
  if(!This) throw wxe_badarg("This");
  if(enif_is_identical(pen_type, WXE_ATOM_wxPen))
   This->SetPen(* static_cast<wxPen*> (pen));
  else if(enif_is_identical(pen_type, WXE_ATOM_wxGraphicsPen))
   This->SetPen(* static_cast<wxGraphicsPen*> (pen));
  else throw wxe_badarg("pen");

}

// wxGraphicsContext::StrokeLine
void wxGraphicsContext_StrokeLine(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd)
{
  ErlNifEnv *env = Ecmd.env;
  ERL_NIF_TERM * argv = Ecmd.args;
  wxGraphicsContext *This;
  This = (wxGraphicsContext *) memenv->getPtr(env, argv[0], "This");
  double x1;
  if(!wxe_get_double(env, argv[1], &x1)) Badarg("x1");
  double y1;
  if(!wxe_get_double(env, argv[2], &y1)) Badarg("y1");
  double x2;
  if(!wxe_get_double(env, argv[3], &x2)) Badarg("x2");
  double y2;
  if(!wxe_get_double(env, argv[4], &y2)) Badarg("y2");
  if(!This) throw wxe_badarg("This");
  This->StrokeLine(x1,y1,x2,y2);

}

// wxGraphicsContext::StrokeLines
void wxGraphicsContext_StrokeLines(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd)
{
  ErlNifEnv *env = Ecmd.env;
  ERL_NIF_TERM * argv = Ecmd.args;
  wxGraphicsContext *This;
  This = (wxGraphicsContext *) memenv->getPtr(env, argv[0], "This");
  unsigned pointsLen;
  ERL_NIF_TERM pointsHead, pointsTail;
  const ERL_NIF_TERM *points_tpl;
  int points_tsz;
  if(!enif_get_list_length(env, argv[1], &pointsLen)) Badarg("points");
  std::vector <wxPoint2DDouble> points;
  double x, y;
  pointsTail = argv[1];
  while(!enif_is_empty_list(env, pointsTail)) {
    if(!enif_get_list_cell(env, pointsTail, &pointsHead, &pointsTail)) Badarg("points");
    if(!enif_get_tuple(env, pointsHead, &points_tsz, &points_tpl) || points_tsz != 2) Badarg("points");
    if(!wxe_get_double(env, points_tpl[0], &x)) Badarg("points");
    if(!wxe_get_double(env, points_tpl[1], &y)) Badarg("points");
    points.push_back(wxPoint2DDouble(x,y));
  };
  if(!This) throw wxe_badarg("This");
  This->StrokeLines(pointsLen,points.data());

}

#endif // wxUSE_GRAPHICS_CONTEXT
#if wxUSE_GRAPHICS_CONTEXT
#endif // wxUSE_GRAPHICS_CONTEXT
#if wxUSE_GRAPHICS_CONTEXT
// wxGraphicsGradientStops::wxGraphicsGradientStops
void wxGraphicsGradientStops_new(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd)
{
 wxColour startCol = wxTransparentColour;
 wxColour endCol = wxTransparentColour;
  ErlNifEnv *env = Ecmd.env;
  ERL_NIF_TERM * argv = Ecmd.args;
  ERL_NIF_TERM lstHead, lstTail;
  lstTail = argv[0];
  if(!enif_is_list(env, lstTail)) Badarg("Options");
  const ERL_NIF_TERM *tpl;
  int tpl_sz;
  while(!enif_is_empty_list(env, lstTail)) {
    if(!enif_get_list_cell(env, lstTail, &lstHead, &lstTail)) Badarg("Options");
    if(!enif_get_tuple(env, lstHead, &tpl_sz, &tpl) || tpl_sz != 2) Badarg("Options");
    if(enif_is_identical(tpl[0], enif_make_atom(env, "startCol"))) {
  const ERL_NIF_TERM *startCol_t;
  int startCol_sz;
  if(!enif_get_tuple(env, tpl[1], &startCol_sz, &startCol_t)) Badarg("startCol");
  int startColR;
  if(!enif_get_int(env, startCol_t[0], &startColR)) Badarg("startCol");
  int startColG;
  if(!enif_get_int(env, startCol_t[1], &startColG)) Badarg("startCol");
  int startColB;
  if(!enif_get_int(env, startCol_t[2], &startColB)) Badarg("startCol");
  int startColA;
  if(!enif_get_int(env, startCol_t[3], &startColA)) Badarg("startCol");
  startCol = wxColour(startColR,startColG,startColB,startColA);
    } else     if(enif_is_identical(tpl[0], enif_make_atom(env, "endCol"))) {
  const ERL_NIF_TERM *endCol_t;
  int endCol_sz;
  if(!enif_get_tuple(env, tpl[1], &endCol_sz, &endCol_t)) Badarg("endCol");
  int endColR;
  if(!enif_get_int(env, endCol_t[0], &endColR)) Badarg("endCol");
  int endColG;
  if(!enif_get_int(env, endCol_t[1], &endColG)) Badarg("endCol");
  int endColB;
  if(!enif_get_int(env, endCol_t[2], &endColB)) Badarg("endCol");
  int endColA;
  if(!enif_get_int(env, endCol_t[3], &endColA)) Badarg("endCol");
  endCol = wxColour(endColR,endColG,endColB,endColA);
    } else        Badarg("Options");
  };
  wxGraphicsGradientStops * Result = new EwxGraphicsGradientStops(startCol,endCol);
  app->newPtr((void *) Result, 1, memenv);
  wxeReturn rt = wxeReturn(memenv, Ecmd.caller, true);
  rt.send(  rt.make_ref(app->getRef((void *)Result,memenv), "wxGraphicsGradientStops"));

}

// wxGraphicsGradientStops::Item
void wxGraphicsGradientStops_Item(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd)
{
  ErlNifEnv *env = Ecmd.env;
  ERL_NIF_TERM * argv = Ecmd.args;
  wxGraphicsGradientStops *This;
  This = (wxGraphicsGradientStops *) memenv->getPtr(env, argv[0], "This");
  unsigned int n;
  if(!enif_get_uint(env, argv[1], &n)) Badarg("n");
  if(!This) throw wxe_badarg("This");
  wxGraphicsGradientStop Result = This->Item(n);
  wxeReturn rt = wxeReturn(memenv, Ecmd.caller, true);
  rt.send(  rt.make(Result));

}

// wxGraphicsGradientStops::GetCount
void wxGraphicsGradientStops_GetCount(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd)
{
  ErlNifEnv *env = Ecmd.env;
  ERL_NIF_TERM * argv = Ecmd.args;
  wxGraphicsGradientStops *This;
  This = (wxGraphicsGradientStops *) memenv->getPtr(env, argv[0], "This");
  if(!This) throw wxe_badarg("This");
  size_t Result = This->GetCount();
  wxeReturn rt = wxeReturn(memenv, Ecmd.caller, true);
  rt.send(  rt.make_int(Result));

}

// wxGraphicsGradientStops::SetStartColour
void wxGraphicsGradientStops_SetStartColour(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd)
{
  ErlNifEnv *env = Ecmd.env;
  ERL_NIF_TERM * argv = Ecmd.args;
  wxGraphicsGradientStops *This;
  This = (wxGraphicsGradientStops *) memenv->getPtr(env, argv[0], "This");
  const ERL_NIF_TERM *col_t;
  int col_sz;
  if(!enif_get_tuple(env, argv[1], &col_sz, &col_t)) Badarg("col");
  int colR;
  if(!enif_get_int(env, col_t[0], &colR)) Badarg("col");
  int colG;
  if(!enif_get_int(env, col_t[1], &colG)) Badarg("col");
  int colB;
  if(!enif_get_int(env, col_t[2], &colB)) Badarg("col");
  int colA;
  if(!enif_get_int(env, col_t[3], &colA)) Badarg("col");
  wxColour col = wxColour(colR,colG,colB,colA);
  if(!This) throw wxe_badarg("This");
  This->SetStartColour(col);

}

// wxGraphicsGradientStops::GetStartColour
void wxGraphicsGradientStops_GetStartColour(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd)
{
  ErlNifEnv *env = Ecmd.env;
  ERL_NIF_TERM * argv = Ecmd.args;
  wxGraphicsGradientStops *This;
  This = (wxGraphicsGradientStops *) memenv->getPtr(env, argv[0], "This");
  if(!This) throw wxe_badarg("This");
  wxColour Result = This->GetStartColour();
  wxeReturn rt = wxeReturn(memenv, Ecmd.caller, true);
  rt.send(  rt.make(Result));

}

// wxGraphicsGradientStops::SetEndColour
void wxGraphicsGradientStops_SetEndColour(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd)
{
  ErlNifEnv *env = Ecmd.env;
  ERL_NIF_TERM * argv = Ecmd.args;
  wxGraphicsGradientStops *This;
  This = (wxGraphicsGradientStops *) memenv->getPtr(env, argv[0], "This");
  const ERL_NIF_TERM *col_t;
  int col_sz;
  if(!enif_get_tuple(env, argv[1], &col_sz, &col_t)) Badarg("col");
  int colR;
  if(!enif_get_int(env, col_t[0], &colR)) Badarg("col");
  int colG;
  if(!enif_get_int(env, col_t[1], &colG)) Badarg("col");
  int colB;
  if(!enif_get_int(env, col_t[2], &colB)) Badarg("col");
  int colA;
  if(!enif_get_int(env, col_t[3], &colA)) Badarg("col");
  wxColour col = wxColour(colR,colG,colB,colA);
  if(!This) throw wxe_badarg("This");
  This->SetEndColour(col);

}

// wxGraphicsGradientStops::GetEndColour
void wxGraphicsGradientStops_GetEndColour(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd)
{
  ErlNifEnv *env = Ecmd.env;
  ERL_NIF_TERM * argv = Ecmd.args;
  wxGraphicsGradientStops *This;
  This = (wxGraphicsGradientStops *) memenv->getPtr(env, argv[0], "This");
  if(!This) throw wxe_badarg("This");
  wxColour Result = This->GetEndColour();
  wxeReturn rt = wxeReturn(memenv, Ecmd.caller, true);
  rt.send(  rt.make(Result));

}

// wxGraphicsGradientStops::Add
void wxGraphicsGradientStops_Add(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd)
{
  ErlNifEnv *env = Ecmd.env;
  ERL_NIF_TERM * argv = Ecmd.args;
  wxGraphicsGradientStops *This;
  This = (wxGraphicsGradientStops *) memenv->getPtr(env, argv[0], "This");
  const ERL_NIF_TERM *col_t;
  int col_sz;
  if(!enif_get_tuple(env, argv[1], &col_sz, &col_t)) Badarg("col");
  int colR;
  if(!enif_get_int(env, col_t[0], &colR)) Badarg("col");
  int colG;
  if(!enif_get_int(env, col_t[1], &colG)) Badarg("col");
  int colB;
  if(!enif_get_int(env, col_t[2], &colB)) Badarg("col");
  int colA;
  if(!enif_get_int(env, col_t[3], &colA)) Badarg("col");
  wxColour col = wxColour(colR,colG,colB,colA);
  float pos;
  if(!wxe_get_float(env, argv[2], &pos)) Badarg("pos");
  if(!This) throw wxe_badarg("This");
  This->Add(col,pos);

}

#endif // wxUSE_GRAPHICS_CONTEXT
#if wxUSE_GRAPHICS_CONTEXT
// wxGraphicsMatrix::Concat
void wxGraphicsMatrix_Concat(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd)
{
  ErlNifEnv *env = Ecmd.env;
  ERL_NIF_TERM * argv = Ecmd.args;
  wxGraphicsMatrix *This;
  This = (wxGraphicsMatrix *) memenv->getPtr(env, argv[0], "This");
  wxGraphicsMatrix *t;
  t = (wxGraphicsMatrix *) memenv->getPtr(env, argv[1], "t");
  if(!This) throw wxe_badarg("This");
  This->Concat(t);

}

// wxGraphicsMatrix::Get
void wxGraphicsMatrix_Get(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd)
{
  wxDouble a;
  wxDouble b;
  wxDouble c;
  wxDouble d;
  wxDouble tx;
  wxDouble ty;
  ErlNifEnv *env = Ecmd.env;
  ERL_NIF_TERM * argv = Ecmd.args;
  wxGraphicsMatrix *This;
  This = (wxGraphicsMatrix *) memenv->getPtr(env, argv[0], "This");
  if(!This) throw wxe_badarg("This");
  This->Get(&a,&b,&c,&d,&tx,&ty);
  wxeReturn rt = wxeReturn(memenv, Ecmd.caller, true);
  ERL_NIF_TERM msg = enif_make_tuple6(rt.env,
  rt.make_double(a),
  rt.make_double(b),
  rt.make_double(c),
  rt.make_double(d),
  rt.make_double(tx),
  rt.make_double(ty));
  rt.send(msg);

}

// wxGraphicsMatrix::Invert
void wxGraphicsMatrix_Invert(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd)
{
  ErlNifEnv *env = Ecmd.env;
  ERL_NIF_TERM * argv = Ecmd.args;
  wxGraphicsMatrix *This;
  This = (wxGraphicsMatrix *) memenv->getPtr(env, argv[0], "This");
  if(!This) throw wxe_badarg("This");
  This->Invert();

}

// wxGraphicsMatrix::IsEqual
void wxGraphicsMatrix_IsEqual(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd)
{
  ErlNifEnv *env = Ecmd.env;
  ERL_NIF_TERM * argv = Ecmd.args;
  wxGraphicsMatrix *This;
  This = (wxGraphicsMatrix *) memenv->getPtr(env, argv[0], "This");
  wxGraphicsMatrix *t;
  t = (wxGraphicsMatrix *) memenv->getPtr(env, argv[1], "t");
  if(!This) throw wxe_badarg("This");
  bool Result = This->IsEqual(t);
  wxeReturn rt = wxeReturn(memenv, Ecmd.caller, true);
  rt.send(  rt.make_bool(Result));

}

// wxGraphicsMatrix::IsIdentity
void wxGraphicsMatrix_IsIdentity(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd)
{
  ErlNifEnv *env = Ecmd.env;
  ERL_NIF_TERM * argv = Ecmd.args;
  wxGraphicsMatrix *This;
  This = (wxGraphicsMatrix *) memenv->getPtr(env, argv[0], "This");
  if(!This) throw wxe_badarg("This");
  bool Result = This->IsIdentity();
  wxeReturn rt = wxeReturn(memenv, Ecmd.caller, true);
  rt.send(  rt.make_bool(Result));

}

// wxGraphicsMatrix::Rotate
void wxGraphicsMatrix_Rotate(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd)
{
  ErlNifEnv *env = Ecmd.env;
  ERL_NIF_TERM * argv = Ecmd.args;
  wxGraphicsMatrix *This;
  This = (wxGraphicsMatrix *) memenv->getPtr(env, argv[0], "This");
  double angle;
  if(!wxe_get_double(env, argv[1], &angle)) Badarg("angle");
  if(!This) throw wxe_badarg("This");
  This->Rotate(angle);

}

// wxGraphicsMatrix::Scale
void wxGraphicsMatrix_Scale(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd)
{
  ErlNifEnv *env = Ecmd.env;
  ERL_NIF_TERM * argv = Ecmd.args;
  wxGraphicsMatrix *This;
  This = (wxGraphicsMatrix *) memenv->getPtr(env, argv[0], "This");
  double xScale;
  if(!wxe_get_double(env, argv[1], &xScale)) Badarg("xScale");
  double yScale;
  if(!wxe_get_double(env, argv[2], &yScale)) Badarg("yScale");
  if(!This) throw wxe_badarg("This");
  This->Scale(xScale,yScale);

}

// wxGraphicsMatrix::Translate
void wxGraphicsMatrix_Translate(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd)
{
  ErlNifEnv *env = Ecmd.env;
  ERL_NIF_TERM * argv = Ecmd.args;
  wxGraphicsMatrix *This;
  This = (wxGraphicsMatrix *) memenv->getPtr(env, argv[0], "This");
  double dx;
  if(!wxe_get_double(env, argv[1], &dx)) Badarg("dx");
  double dy;
  if(!wxe_get_double(env, argv[2], &dy)) Badarg("dy");
  if(!This) throw wxe_badarg("This");
  This->Translate(dx,dy);

}

// wxGraphicsMatrix::Set
void wxGraphicsMatrix_Set(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd)
{
  wxDouble a=1.0;
  wxDouble b=0.0;
  wxDouble c=0.0;
  wxDouble d=1.0;
  wxDouble tx=0.0;
  wxDouble ty=0.0;
  ErlNifEnv *env = Ecmd.env;
  ERL_NIF_TERM * argv = Ecmd.args;
  wxGraphicsMatrix *This;
  This = (wxGraphicsMatrix *) memenv->getPtr(env, argv[0], "This");
  ERL_NIF_TERM lstHead, lstTail;
  lstTail = argv[1];
  if(!enif_is_list(env, lstTail)) Badarg("Options");
  const ERL_NIF_TERM *tpl;
  int tpl_sz;
  while(!enif_is_empty_list(env, lstTail)) {
    if(!enif_get_list_cell(env, lstTail, &lstHead, &lstTail)) Badarg("Options");
    if(!enif_get_tuple(env, lstHead, &tpl_sz, &tpl) || tpl_sz != 2) Badarg("Options");
    if(enif_is_identical(tpl[0], enif_make_atom(env, "a"))) {
  if(!wxe_get_double(env, tpl[1], &a)) Badarg("a");
    } else     if(enif_is_identical(tpl[0], enif_make_atom(env, "b"))) {
  if(!wxe_get_double(env, tpl[1], &b)) Badarg("b");
    } else     if(enif_is_identical(tpl[0], enif_make_atom(env, "c"))) {
  if(!wxe_get_double(env, tpl[1], &c)) Badarg("c");
    } else     if(enif_is_identical(tpl[0], enif_make_atom(env, "d"))) {
  if(!wxe_get_double(env, tpl[1], &d)) Badarg("d");
    } else     if(enif_is_identical(tpl[0], enif_make_atom(env, "tx"))) {
  if(!wxe_get_double(env, tpl[1], &tx)) Badarg("tx");
    } else     if(enif_is_identical(tpl[0], enif_make_atom(env, "ty"))) {
  if(!wxe_get_double(env, tpl[1], &ty)) Badarg("ty");
    } else        Badarg("Options");
  };
  if(!This) throw wxe_badarg("This");
  This->Set(a,b,c,d,tx,ty);

}

// wxGraphicsMatrix::TransformPoint
void wxGraphicsMatrix_TransformPoint(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd)
{
  wxDouble x;
  wxDouble y;
  ErlNifEnv *env = Ecmd.env;
  ERL_NIF_TERM * argv = Ecmd.args;
  wxGraphicsMatrix *This;
  This = (wxGraphicsMatrix *) memenv->getPtr(env, argv[0], "This");
  if(!This) throw wxe_badarg("This");
  This->TransformPoint(&x,&y);
  wxeReturn rt = wxeReturn(memenv, Ecmd.caller, true);
  ERL_NIF_TERM msg = enif_make_tuple2(rt.env,
  rt.make_double(x),
  rt.make_double(y));
  rt.send(msg);

}

// wxGraphicsMatrix::TransformDistance
void wxGraphicsMatrix_TransformDistance(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd)
{
  wxDouble dx;
  wxDouble dy;
  ErlNifEnv *env = Ecmd.env;
  ERL_NIF_TERM * argv = Ecmd.args;
  wxGraphicsMatrix *This;
  This = (wxGraphicsMatrix *) memenv->getPtr(env, argv[0], "This");
  if(!This) throw wxe_badarg("This");
  This->TransformDistance(&dx,&dy);
  wxeReturn rt = wxeReturn(memenv, Ecmd.caller, true);
  ERL_NIF_TERM msg = enif_make_tuple2(rt.env,
  rt.make_double(dx),
  rt.make_double(dy));
  rt.send(msg);

}

#endif // wxUSE_GRAPHICS_CONTEXT
#if wxUSE_GRAPHICS_CONTEXT
// wxGraphicsObject::GetRenderer
void wxGraphicsObject_GetRenderer(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd)
{
  ErlNifEnv *env = Ecmd.env;
  ERL_NIF_TERM * argv = Ecmd.args;
  wxGraphicsObject *This;
  This = (wxGraphicsObject *) memenv->getPtr(env, argv[0], "This");
  if(!This) throw wxe_badarg("This");
  wxGraphicsRenderer * Result = (wxGraphicsRenderer*)This->GetRenderer();
  wxeReturn rt = wxeReturn(memenv, Ecmd.caller, true);
  rt.send(  rt.make_ref(app->getRef((void *)Result,memenv), "wxGraphicsRenderer"));

}

// wxGraphicsObject::IsNull
void wxGraphicsObject_IsNull(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd)
{
  ErlNifEnv *env = Ecmd.env;
  ERL_NIF_TERM * argv = Ecmd.args;
  wxGraphicsObject *This;
  This = (wxGraphicsObject *) memenv->getPtr(env, argv[0], "This");
  if(!This) throw wxe_badarg("This");
  bool Result = This->IsNull();
  wxeReturn rt = wxeReturn(memenv, Ecmd.caller, true);
  rt.send(  rt.make_bool(Result));

}

#endif // wxUSE_GRAPHICS_CONTEXT
#if wxUSE_GRAPHICS_CONTEXT
// wxGraphicsPath::MoveToPoint
void wxGraphicsPath_MoveToPoint_2(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd)
{
  ErlNifEnv *env = Ecmd.env;
  ERL_NIF_TERM * argv = Ecmd.args;
  wxGraphicsPath *This;
  This = (wxGraphicsPath *) memenv->getPtr(env, argv[0], "This");
  double x;
  if(!wxe_get_double(env, argv[1], &x)) Badarg("x");
  double y;
  if(!wxe_get_double(env, argv[2], &y)) Badarg("y");
  if(!This) throw wxe_badarg("This");
  This->MoveToPoint(x,y);

}

// wxGraphicsPath::MoveToPoint
void wxGraphicsPath_MoveToPoint_1(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd)
{
  ErlNifEnv *env = Ecmd.env;
  ERL_NIF_TERM * argv = Ecmd.args;
  wxGraphicsPath *This;
  This = (wxGraphicsPath *) memenv->getPtr(env, argv[0], "This");
  const ERL_NIF_TERM *p_t;
  int p_sz;
  if(!enif_get_tuple(env, argv[1], &p_sz, &p_t)) Badarg("p");
  double pX;
  if(!wxe_get_double(env, p_t[0], &pX)) Badarg("p");
  double pY;
  if(!wxe_get_double(env, p_t[1], &pY)) Badarg("p");
  wxPoint2DDouble p = wxPoint2DDouble(pX,pY);
  if(!This) throw wxe_badarg("This");
  This->MoveToPoint(p);

}

// wxGraphicsPath::AddArc
void wxGraphicsPath_AddArc_6(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd)
{
  ErlNifEnv *env = Ecmd.env;
  ERL_NIF_TERM * argv = Ecmd.args;
  wxGraphicsPath *This;
  This = (wxGraphicsPath *) memenv->getPtr(env, argv[0], "This");
  double x;
  if(!wxe_get_double(env, argv[1], &x)) Badarg("x");
  double y;
  if(!wxe_get_double(env, argv[2], &y)) Badarg("y");
  double r;
  if(!wxe_get_double(env, argv[3], &r)) Badarg("r");
  double startAngle;
  if(!wxe_get_double(env, argv[4], &startAngle)) Badarg("startAngle");
  double endAngle;
  if(!wxe_get_double(env, argv[5], &endAngle)) Badarg("endAngle");
  bool clockwise;
  clockwise = enif_is_identical(argv[6], WXE_ATOM_true);
  if(!This) throw wxe_badarg("This");
  This->AddArc(x,y,r,startAngle,endAngle,clockwise);

}

// wxGraphicsPath::AddArc
void wxGraphicsPath_AddArc_5(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd)
{
  ErlNifEnv *env = Ecmd.env;
  ERL_NIF_TERM * argv = Ecmd.args;
  wxGraphicsPath *This;
  This = (wxGraphicsPath *) memenv->getPtr(env, argv[0], "This");
  const ERL_NIF_TERM *c_t;
  int c_sz;
  if(!enif_get_tuple(env, argv[1], &c_sz, &c_t)) Badarg("c");
  double cX;
  if(!wxe_get_double(env, c_t[0], &cX)) Badarg("c");
  double cY;
  if(!wxe_get_double(env, c_t[1], &cY)) Badarg("c");
  wxPoint2DDouble c = wxPoint2DDouble(cX,cY);
  double r;
  if(!wxe_get_double(env, argv[2], &r)) Badarg("r");
  double startAngle;
  if(!wxe_get_double(env, argv[3], &startAngle)) Badarg("startAngle");
  double endAngle;
  if(!wxe_get_double(env, argv[4], &endAngle)) Badarg("endAngle");
  bool clockwise;
  clockwise = enif_is_identical(argv[5], WXE_ATOM_true);
  if(!This) throw wxe_badarg("This");
  This->AddArc(c,r,startAngle,endAngle,clockwise);

}

// wxGraphicsPath::AddArcToPoint
void wxGraphicsPath_AddArcToPoint(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd)
{
  ErlNifEnv *env = Ecmd.env;
  ERL_NIF_TERM * argv = Ecmd.args;
  wxGraphicsPath *This;
  This = (wxGraphicsPath *) memenv->getPtr(env, argv[0], "This");
  double x1;
  if(!wxe_get_double(env, argv[1], &x1)) Badarg("x1");
  double y1;
  if(!wxe_get_double(env, argv[2], &y1)) Badarg("y1");
  double x2;
  if(!wxe_get_double(env, argv[3], &x2)) Badarg("x2");
  double y2;
  if(!wxe_get_double(env, argv[4], &y2)) Badarg("y2");
  double r;
  if(!wxe_get_double(env, argv[5], &r)) Badarg("r");
  if(!This) throw wxe_badarg("This");
  This->AddArcToPoint(x1,y1,x2,y2,r);

}

// wxGraphicsPath::AddCircle
void wxGraphicsPath_AddCircle(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd)
{
  ErlNifEnv *env = Ecmd.env;
  ERL_NIF_TERM * argv = Ecmd.args;
  wxGraphicsPath *This;
  This = (wxGraphicsPath *) memenv->getPtr(env, argv[0], "This");
  double x;
  if(!wxe_get_double(env, argv[1], &x)) Badarg("x");
  double y;
  if(!wxe_get_double(env, argv[2], &y)) Badarg("y");
  double r;
  if(!wxe_get_double(env, argv[3], &r)) Badarg("r");
  if(!This) throw wxe_badarg("This");
  This->AddCircle(x,y,r);

}

// wxGraphicsPath::AddCurveToPoint
void wxGraphicsPath_AddCurveToPoint_6(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd)
{
  ErlNifEnv *env = Ecmd.env;
  ERL_NIF_TERM * argv = Ecmd.args;
  wxGraphicsPath *This;
  This = (wxGraphicsPath *) memenv->getPtr(env, argv[0], "This");
  double cx1;
  if(!wxe_get_double(env, argv[1], &cx1)) Badarg("cx1");
  double cy1;
  if(!wxe_get_double(env, argv[2], &cy1)) Badarg("cy1");
  double cx2;
  if(!wxe_get_double(env, argv[3], &cx2)) Badarg("cx2");
  double cy2;
  if(!wxe_get_double(env, argv[4], &cy2)) Badarg("cy2");
  double x;
  if(!wxe_get_double(env, argv[5], &x)) Badarg("x");
  double y;
  if(!wxe_get_double(env, argv[6], &y)) Badarg("y");
  if(!This) throw wxe_badarg("This");
  This->AddCurveToPoint(cx1,cy1,cx2,cy2,x,y);

}

// wxGraphicsPath::AddCurveToPoint
void wxGraphicsPath_AddCurveToPoint_3(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd)
{
  ErlNifEnv *env = Ecmd.env;
  ERL_NIF_TERM * argv = Ecmd.args;
  wxGraphicsPath *This;
  This = (wxGraphicsPath *) memenv->getPtr(env, argv[0], "This");
  const ERL_NIF_TERM *c1_t;
  int c1_sz;
  if(!enif_get_tuple(env, argv[1], &c1_sz, &c1_t)) Badarg("c1");
  double c1X;
  if(!wxe_get_double(env, c1_t[0], &c1X)) Badarg("c1");
  double c1Y;
  if(!wxe_get_double(env, c1_t[1], &c1Y)) Badarg("c1");
  wxPoint2DDouble c1 = wxPoint2DDouble(c1X,c1Y);
  const ERL_NIF_TERM *c2_t;
  int c2_sz;
  if(!enif_get_tuple(env, argv[2], &c2_sz, &c2_t)) Badarg("c2");
  double c2X;
  if(!wxe_get_double(env, c2_t[0], &c2X)) Badarg("c2");
  double c2Y;
  if(!wxe_get_double(env, c2_t[1], &c2Y)) Badarg("c2");
  wxPoint2DDouble c2 = wxPoint2DDouble(c2X,c2Y);
  const ERL_NIF_TERM *e_t;
  int e_sz;
  if(!enif_get_tuple(env, argv[3], &e_sz, &e_t)) Badarg("e");
  double eX;
  if(!wxe_get_double(env, e_t[0], &eX)) Badarg("e");
  double eY;
  if(!wxe_get_double(env, e_t[1], &eY)) Badarg("e");
  wxPoint2DDouble e = wxPoint2DDouble(eX,eY);
  if(!This) throw wxe_badarg("This");
  This->AddCurveToPoint(c1,c2,e);

}

// wxGraphicsPath::AddEllipse
void wxGraphicsPath_AddEllipse(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd)
{
  ErlNifEnv *env = Ecmd.env;
  ERL_NIF_TERM * argv = Ecmd.args;
  wxGraphicsPath *This;
  This = (wxGraphicsPath *) memenv->getPtr(env, argv[0], "This");
  double x;
  if(!wxe_get_double(env, argv[1], &x)) Badarg("x");
  double y;
  if(!wxe_get_double(env, argv[2], &y)) Badarg("y");
  double w;
  if(!wxe_get_double(env, argv[3], &w)) Badarg("w");
  double h;
  if(!wxe_get_double(env, argv[4], &h)) Badarg("h");
  if(!This) throw wxe_badarg("This");
  This->AddEllipse(x,y,w,h);

}

// wxGraphicsPath::AddLineToPoint
void wxGraphicsPath_AddLineToPoint_2(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd)
{
  ErlNifEnv *env = Ecmd.env;
  ERL_NIF_TERM * argv = Ecmd.args;
  wxGraphicsPath *This;
  This = (wxGraphicsPath *) memenv->getPtr(env, argv[0], "This");
  double x;
  if(!wxe_get_double(env, argv[1], &x)) Badarg("x");
  double y;
  if(!wxe_get_double(env, argv[2], &y)) Badarg("y");
  if(!This) throw wxe_badarg("This");
  This->AddLineToPoint(x,y);

}

// wxGraphicsPath::AddLineToPoint
void wxGraphicsPath_AddLineToPoint_1(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd)
{
  ErlNifEnv *env = Ecmd.env;
  ERL_NIF_TERM * argv = Ecmd.args;
  wxGraphicsPath *This;
  This = (wxGraphicsPath *) memenv->getPtr(env, argv[0], "This");
  const ERL_NIF_TERM *p_t;
  int p_sz;
  if(!enif_get_tuple(env, argv[1], &p_sz, &p_t)) Badarg("p");
  double pX;
  if(!wxe_get_double(env, p_t[0], &pX)) Badarg("p");
  double pY;
  if(!wxe_get_double(env, p_t[1], &pY)) Badarg("p");
  wxPoint2DDouble p = wxPoint2DDouble(pX,pY);
  if(!This) throw wxe_badarg("This");
  This->AddLineToPoint(p);

}

// wxGraphicsPath::AddPath
void wxGraphicsPath_AddPath(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd)
{
  ErlNifEnv *env = Ecmd.env;
  ERL_NIF_TERM * argv = Ecmd.args;
  wxGraphicsPath *This;
  This = (wxGraphicsPath *) memenv->getPtr(env, argv[0], "This");
  wxGraphicsPath *path;
  path = (wxGraphicsPath *) memenv->getPtr(env, argv[1], "path");
  if(!This) throw wxe_badarg("This");
  This->AddPath(*path);

}

// wxGraphicsPath::AddQuadCurveToPoint
void wxGraphicsPath_AddQuadCurveToPoint(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd)
{
  ErlNifEnv *env = Ecmd.env;
  ERL_NIF_TERM * argv = Ecmd.args;
  wxGraphicsPath *This;
  This = (wxGraphicsPath *) memenv->getPtr(env, argv[0], "This");
  double cx;
  if(!wxe_get_double(env, argv[1], &cx)) Badarg("cx");
  double cy;
  if(!wxe_get_double(env, argv[2], &cy)) Badarg("cy");
  double x;
  if(!wxe_get_double(env, argv[3], &x)) Badarg("x");
  double y;
  if(!wxe_get_double(env, argv[4], &y)) Badarg("y");
  if(!This) throw wxe_badarg("This");
  This->AddQuadCurveToPoint(cx,cy,x,y);

}

// wxGraphicsPath::AddRectangle
void wxGraphicsPath_AddRectangle(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd)
{
  ErlNifEnv *env = Ecmd.env;
  ERL_NIF_TERM * argv = Ecmd.args;
  wxGraphicsPath *This;
  This = (wxGraphicsPath *) memenv->getPtr(env, argv[0], "This");
  double x;
  if(!wxe_get_double(env, argv[1], &x)) Badarg("x");
  double y;
  if(!wxe_get_double(env, argv[2], &y)) Badarg("y");
  double w;
  if(!wxe_get_double(env, argv[3], &w)) Badarg("w");
  double h;
  if(!wxe_get_double(env, argv[4], &h)) Badarg("h");
  if(!This) throw wxe_badarg("This");
  This->AddRectangle(x,y,w,h);

}

// wxGraphicsPath::AddRoundedRectangle
void wxGraphicsPath_AddRoundedRectangle(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd)
{
  ErlNifEnv *env = Ecmd.env;
  ERL_NIF_TERM * argv = Ecmd.args;
  wxGraphicsPath *This;
  This = (wxGraphicsPath *) memenv->getPtr(env, argv[0], "This");
  double x;
  if(!wxe_get_double(env, argv[1], &x)) Badarg("x");
  double y;
  if(!wxe_get_double(env, argv[2], &y)) Badarg("y");
  double w;
  if(!wxe_get_double(env, argv[3], &w)) Badarg("w");
  double h;
  if(!wxe_get_double(env, argv[4], &h)) Badarg("h");
  double radius;
  if(!wxe_get_double(env, argv[5], &radius)) Badarg("radius");
  if(!This) throw wxe_badarg("This");
  This->AddRoundedRectangle(x,y,w,h,radius);

}

// wxGraphicsPath::CloseSubpath
void wxGraphicsPath_CloseSubpath(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd)
{
  ErlNifEnv *env = Ecmd.env;
  ERL_NIF_TERM * argv = Ecmd.args;
  wxGraphicsPath *This;
  This = (wxGraphicsPath *) memenv->getPtr(env, argv[0], "This");
  if(!This) throw wxe_badarg("This");
  This->CloseSubpath();

}

// wxGraphicsPath::Contains
void wxGraphicsPath_Contains_2(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd)
{
 wxPolygonFillMode fillStyle=wxODDEVEN_RULE;
  ErlNifEnv *env = Ecmd.env;
  ERL_NIF_TERM * argv = Ecmd.args;
  wxGraphicsPath *This;
  This = (wxGraphicsPath *) memenv->getPtr(env, argv[0], "This");
  const ERL_NIF_TERM *c_t;
  int c_sz;
  if(!enif_get_tuple(env, argv[1], &c_sz, &c_t)) Badarg("c");
  double cX;
  if(!wxe_get_double(env, c_t[0], &cX)) Badarg("c");
  double cY;
  if(!wxe_get_double(env, c_t[1], &cY)) Badarg("c");
  wxPoint2DDouble c = wxPoint2DDouble(cX,cY);
  ERL_NIF_TERM lstHead, lstTail;
  lstTail = argv[2];
  if(!enif_is_list(env, lstTail)) Badarg("Options");
  const ERL_NIF_TERM *tpl;
  int tpl_sz;
  while(!enif_is_empty_list(env, lstTail)) {
    if(!enif_get_list_cell(env, lstTail, &lstHead, &lstTail)) Badarg("Options");
    if(!enif_get_tuple(env, lstHead, &tpl_sz, &tpl) || tpl_sz != 2) Badarg("Options");
    if(enif_is_identical(tpl[0], enif_make_atom(env, "fillStyle"))) {
  if(!enif_get_int(env, tpl[1], (int *) &fillStyle)) Badarg("fillStyle"); // enum
    } else        Badarg("Options");
  };
  if(!This) throw wxe_badarg("This");
  bool Result = This->Contains(c,fillStyle);
  wxeReturn rt = wxeReturn(memenv, Ecmd.caller, true);
  rt.send(  rt.make_bool(Result));

}

// wxGraphicsPath::Contains
void wxGraphicsPath_Contains_3(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd)
{
 wxPolygonFillMode fillStyle=wxODDEVEN_RULE;
  ErlNifEnv *env = Ecmd.env;
  ERL_NIF_TERM * argv = Ecmd.args;
  wxGraphicsPath *This;
  This = (wxGraphicsPath *) memenv->getPtr(env, argv[0], "This");
  double x;
  if(!wxe_get_double(env, argv[1], &x)) Badarg("x");
  double y;
  if(!wxe_get_double(env, argv[2], &y)) Badarg("y");
  ERL_NIF_TERM lstHead, lstTail;
  lstTail = argv[3];
  if(!enif_is_list(env, lstTail)) Badarg("Options");
  const ERL_NIF_TERM *tpl;
  int tpl_sz;
  while(!enif_is_empty_list(env, lstTail)) {
    if(!enif_get_list_cell(env, lstTail, &lstHead, &lstTail)) Badarg("Options");
    if(!enif_get_tuple(env, lstHead, &tpl_sz, &tpl) || tpl_sz != 2) Badarg("Options");
    if(enif_is_identical(tpl[0], enif_make_atom(env, "fillStyle"))) {
  if(!enif_get_int(env, tpl[1], (int *) &fillStyle)) Badarg("fillStyle"); // enum
    } else        Badarg("Options");
  };
  if(!This) throw wxe_badarg("This");
  bool Result = This->Contains(x,y,fillStyle);
  wxeReturn rt = wxeReturn(memenv, Ecmd.caller, true);
  rt.send(  rt.make_bool(Result));

}

// wxGraphicsPath::GetBox
void wxGraphicsPath_GetBox(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd)
{
  ErlNifEnv *env = Ecmd.env;
  ERL_NIF_TERM * argv = Ecmd.args;
  wxGraphicsPath *This;
  This = (wxGraphicsPath *) memenv->getPtr(env, argv[0], "This");
  if(!This) throw wxe_badarg("This");
  wxRect2DDouble Result = This->GetBox();
  wxeReturn rt = wxeReturn(memenv, Ecmd.caller, true);
  rt.send(  rt.make(Result));

}

// wxGraphicsPath::GetCurrentPoint
void wxGraphicsPath_GetCurrentPoint(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd)
{
  ErlNifEnv *env = Ecmd.env;
  ERL_NIF_TERM * argv = Ecmd.args;
  wxGraphicsPath *This;
  This = (wxGraphicsPath *) memenv->getPtr(env, argv[0], "This");
  if(!This) throw wxe_badarg("This");
  wxPoint2DDouble Result = This->GetCurrentPoint();
  wxeReturn rt = wxeReturn(memenv, Ecmd.caller, true);
  rt.send(  rt.make(Result));

}

// wxGraphicsPath::Transform
void wxGraphicsPath_Transform(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd)
{
  ErlNifEnv *env = Ecmd.env;
  ERL_NIF_TERM * argv = Ecmd.args;
  wxGraphicsPath *This;
  This = (wxGraphicsPath *) memenv->getPtr(env, argv[0], "This");
  wxGraphicsMatrix *matrix;
  matrix = (wxGraphicsMatrix *) memenv->getPtr(env, argv[1], "matrix");
  if(!This) throw wxe_badarg("This");
  This->Transform(*matrix);

}

#endif // wxUSE_GRAPHICS_CONTEXT
#if wxUSE_GRAPHICS_CONTEXT
#endif // wxUSE_GRAPHICS_CONTEXT
#if wxUSE_GRAPHICS_CONTEXT
// wxGraphicsRenderer::GetDefaultRenderer
void wxGraphicsRenderer_GetDefaultRenderer(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd)
{
  wxGraphicsRenderer * Result = (wxGraphicsRenderer*)wxGraphicsRenderer::GetDefaultRenderer();
  wxeReturn rt = wxeReturn(memenv, Ecmd.caller, true);
  rt.send(  rt.make_ref(app->getRef((void *)Result,memenv), "wxGraphicsRenderer"));

}

// wxGraphicsRenderer::CreateContext
void wxGraphicsRenderer_CreateContext(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd)
{
  ErlNifEnv *env = Ecmd.env;
  ERL_NIF_TERM * argv = Ecmd.args;
  wxGraphicsRenderer *This;
  This = (wxGraphicsRenderer *) memenv->getPtr(env, argv[0], "This");
  ERL_NIF_TERM windowDC_type;
  void * windowDC = memenv->getPtr(env, argv[1], "windowDC", &windowDC_type);
  if(!This) throw wxe_badarg("This");
  wxGraphicsContext * Result;
  if(enif_is_identical(windowDC_type, WXE_ATOM_wxWindowDC))
   Result =  (wxGraphicsContext*)This->CreateContext(* static_cast<wxWindowDC*> (windowDC));
  else if(enif_is_identical(windowDC_type, WXE_ATOM_wxWindow))
   Result =  (wxGraphicsContext*)This->CreateContext(static_cast<wxWindow*> (windowDC));
  else if(enif_is_identical(windowDC_type, WXE_ATOM_wxMemoryDC))
   Result =  (wxGraphicsContext*)This->CreateContext(* static_cast<wxMemoryDC*> (windowDC));
  else throw wxe_badarg("windowDC");
  wxeReturn rt = wxeReturn(memenv, Ecmd.caller, true);
  rt.send(  rt.make_ref(app->getRef((void *)Result,memenv,8), "wxGraphicsContext")
);

}

// wxGraphicsRenderer::CreateBrush
void wxGraphicsRenderer_CreateBrush(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd)
{
  ErlNifEnv *env = Ecmd.env;
  ERL_NIF_TERM * argv = Ecmd.args;
  wxGraphicsRenderer *This;
  This = (wxGraphicsRenderer *) memenv->getPtr(env, argv[0], "This");
  wxBrush *brush;
  brush = (wxBrush *) memenv->getPtr(env, argv[1], "brush");
  if(!This) throw wxe_badarg("This");
  wxGraphicsBrush * Result = new wxGraphicsBrush(This->CreateBrush(*brush)); app->newPtr((void *) Result,4, memenv);;
  wxeReturn rt = wxeReturn(memenv, Ecmd.caller, true);
  rt.send(  rt.make_ref(app->getRef((void *)Result,memenv), "wxGraphicsBrush"));

}

// wxGraphicsRenderer::CreateLinearGradientBrush
void wxGraphicsRenderer_CreateLinearGradientBrush(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd)
{
  ErlNifEnv *env = Ecmd.env;
  ERL_NIF_TERM * argv = Ecmd.args;
  wxGraphicsRenderer *This;
  This = (wxGraphicsRenderer *) memenv->getPtr(env, argv[0], "This");
  double x1;
  if(!wxe_get_double(env, argv[1], &x1)) Badarg("x1");
  double y1;
  if(!wxe_get_double(env, argv[2], &y1)) Badarg("y1");
  double x2;
  if(!wxe_get_double(env, argv[3], &x2)) Badarg("x2");
  double y2;
  if(!wxe_get_double(env, argv[4], &y2)) Badarg("y2");
  wxGraphicsGradientStops *stops;
  stops = (wxGraphicsGradientStops *) memenv->getPtr(env, argv[5], "stops");
  if(!This) throw wxe_badarg("This");
  wxGraphicsBrush * Result = new wxGraphicsBrush(This->CreateLinearGradientBrush(x1,y1,x2,y2,*stops)); app->newPtr((void *) Result,4, memenv);;
  wxeReturn rt = wxeReturn(memenv, Ecmd.caller, true);
  rt.send(  rt.make_ref(app->getRef((void *)Result,memenv), "wxGraphicsBrush"));

}

// wxGraphicsRenderer::CreateRadialGradientBrush
void wxGraphicsRenderer_CreateRadialGradientBrush(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd)
{
  ErlNifEnv *env = Ecmd.env;
  ERL_NIF_TERM * argv = Ecmd.args;
  wxGraphicsRenderer *This;
  This = (wxGraphicsRenderer *) memenv->getPtr(env, argv[0], "This");
  double startX;
  if(!wxe_get_double(env, argv[1], &startX)) Badarg("startX");
  double startY;
  if(!wxe_get_double(env, argv[2], &startY)) Badarg("startY");
  double endX;
  if(!wxe_get_double(env, argv[3], &endX)) Badarg("endX");
  double endY;
  if(!wxe_get_double(env, argv[4], &endY)) Badarg("endY");
  double radius;
  if(!wxe_get_double(env, argv[5], &radius)) Badarg("radius");
  wxGraphicsGradientStops *stops;
  stops = (wxGraphicsGradientStops *) memenv->getPtr(env, argv[6], "stops");
  if(!This) throw wxe_badarg("This");
  wxGraphicsBrush * Result = new wxGraphicsBrush(This->CreateRadialGradientBrush(startX,startY,endX,endY,radius,*stops)); app->newPtr((void *) Result,4, memenv);;
  wxeReturn rt = wxeReturn(memenv, Ecmd.caller, true);
  rt.send(  rt.make_ref(app->getRef((void *)Result,memenv), "wxGraphicsBrush"));

}

// wxGraphicsRenderer::CreateFont
void wxGraphicsRenderer_CreateFont_2(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd)
{
  wxColour col= *wxBLACK;
  ErlNifEnv *env = Ecmd.env;
  ERL_NIF_TERM * argv = Ecmd.args;
  wxGraphicsRenderer *This;
  This = (wxGraphicsRenderer *) memenv->getPtr(env, argv[0], "This");
  wxFont *font;
  font = (wxFont *) memenv->getPtr(env, argv[1], "font");
  ERL_NIF_TERM lstHead, lstTail;
  lstTail = argv[2];
  if(!enif_is_list(env, lstTail)) Badarg("Options");
  const ERL_NIF_TERM *tpl;
  int tpl_sz;
  while(!enif_is_empty_list(env, lstTail)) {
    if(!enif_get_list_cell(env, lstTail, &lstHead, &lstTail)) Badarg("Options");
    if(!enif_get_tuple(env, lstHead, &tpl_sz, &tpl) || tpl_sz != 2) Badarg("Options");
    if(enif_is_identical(tpl[0], enif_make_atom(env, "col"))) {
  const ERL_NIF_TERM *col_t;
  int col_sz;
  if(!enif_get_tuple(env, tpl[1], &col_sz, &col_t)) Badarg("col");
  int colR;
  if(!enif_get_int(env, col_t[0], &colR)) Badarg("col");
  int colG;
  if(!enif_get_int(env, col_t[1], &colG)) Badarg("col");
  int colB;
  if(!enif_get_int(env, col_t[2], &colB)) Badarg("col");
  int colA;
  if(!enif_get_int(env, col_t[3], &colA)) Badarg("col");
  col = wxColour(colR,colG,colB,colA);
    } else        Badarg("Options");
  };
  if(!This) throw wxe_badarg("This");
  wxGraphicsFont * Result = new wxGraphicsFont(This->CreateFont(*font,col)); app->newPtr((void *) Result,4, memenv);;
  wxeReturn rt = wxeReturn(memenv, Ecmd.caller, true);
  rt.send(  rt.make_ref(app->getRef((void *)Result,memenv), "wxGraphicsFont"));

}

// wxGraphicsRenderer::CreateFont
void wxGraphicsRenderer_CreateFont_3(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd)
{
  int flags=wxFONTFLAG_DEFAULT;
  wxColour col= *wxBLACK;
  ErlNifEnv *env = Ecmd.env;
  ERL_NIF_TERM * argv = Ecmd.args;
  wxGraphicsRenderer *This;
  This = (wxGraphicsRenderer *) memenv->getPtr(env, argv[0], "This");
  double sizeInPixels;
  if(!wxe_get_double(env, argv[1], &sizeInPixels)) Badarg("sizeInPixels");
  ErlNifBinary facename_bin;
  wxString facename;
  if(!enif_inspect_binary(env, argv[2], &facename_bin)) Badarg("facename");
  facename = wxString(facename_bin.data, wxConvUTF8, facename_bin.size);
  ERL_NIF_TERM lstHead, lstTail;
  lstTail = argv[3];
  if(!enif_is_list(env, lstTail)) Badarg("Options");
  const ERL_NIF_TERM *tpl;
  int tpl_sz;
  while(!enif_is_empty_list(env, lstTail)) {
    if(!enif_get_list_cell(env, lstTail, &lstHead, &lstTail)) Badarg("Options");
    if(!enif_get_tuple(env, lstHead, &tpl_sz, &tpl) || tpl_sz != 2) Badarg("Options");
    if(enif_is_identical(tpl[0], enif_make_atom(env, "flags"))) {
  if(!enif_get_int(env, tpl[1], &flags)) Badarg("flags"); // int
    } else     if(enif_is_identical(tpl[0], enif_make_atom(env, "col"))) {
  const ERL_NIF_TERM *col_t;
  int col_sz;
  if(!enif_get_tuple(env, tpl[1], &col_sz, &col_t)) Badarg("col");
  int colR;
  if(!enif_get_int(env, col_t[0], &colR)) Badarg("col");
  int colG;
  if(!enif_get_int(env, col_t[1], &colG)) Badarg("col");
  int colB;
  if(!enif_get_int(env, col_t[2], &colB)) Badarg("col");
  int colA;
  if(!enif_get_int(env, col_t[3], &colA)) Badarg("col");
  col = wxColour(colR,colG,colB,colA);
    } else        Badarg("Options");
  };
  if(!This) throw wxe_badarg("This");
  wxGraphicsFont * Result = new wxGraphicsFont(This->CreateFont(sizeInPixels,facename,flags,col)); app->newPtr((void *) Result,4, memenv);;
  wxeReturn rt = wxeReturn(memenv, Ecmd.caller, true);
  rt.send(  rt.make_ref(app->getRef((void *)Result,memenv), "wxGraphicsFont"));

}

// wxGraphicsRenderer::CreateMatrix
void wxGraphicsRenderer_CreateMatrix(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd)
{
  wxDouble a=1.0;
  wxDouble b=0.0;
  wxDouble c=0.0;
  wxDouble d=1.0;
  wxDouble tx=0.0;
  wxDouble ty=0.0;
  ErlNifEnv *env = Ecmd.env;
  ERL_NIF_TERM * argv = Ecmd.args;
  wxGraphicsRenderer *This;
  This = (wxGraphicsRenderer *) memenv->getPtr(env, argv[0], "This");
  ERL_NIF_TERM lstHead, lstTail;
  lstTail = argv[1];
  if(!enif_is_list(env, lstTail)) Badarg("Options");
  const ERL_NIF_TERM *tpl;
  int tpl_sz;
  while(!enif_is_empty_list(env, lstTail)) {
    if(!enif_get_list_cell(env, lstTail, &lstHead, &lstTail)) Badarg("Options");
    if(!enif_get_tuple(env, lstHead, &tpl_sz, &tpl) || tpl_sz != 2) Badarg("Options");
    if(enif_is_identical(tpl[0], enif_make_atom(env, "a"))) {
  if(!wxe_get_double(env, tpl[1], &a)) Badarg("a");
    } else     if(enif_is_identical(tpl[0], enif_make_atom(env, "b"))) {
  if(!wxe_get_double(env, tpl[1], &b)) Badarg("b");
    } else     if(enif_is_identical(tpl[0], enif_make_atom(env, "c"))) {
  if(!wxe_get_double(env, tpl[1], &c)) Badarg("c");
    } else     if(enif_is_identical(tpl[0], enif_make_atom(env, "d"))) {
  if(!wxe_get_double(env, tpl[1], &d)) Badarg("d");
    } else     if(enif_is_identical(tpl[0], enif_make_atom(env, "tx"))) {
  if(!wxe_get_double(env, tpl[1], &tx)) Badarg("tx");
    } else     if(enif_is_identical(tpl[0], enif_make_atom(env, "ty"))) {
  if(!wxe_get_double(env, tpl[1], &ty)) Badarg("ty");
    } else        Badarg("Options");
  };
  if(!This) throw wxe_badarg("This");
  wxGraphicsMatrix * Result = new wxGraphicsMatrix(This->CreateMatrix(a,b,c,d,tx,ty)); app->newPtr((void *) Result,4, memenv);;
  wxeReturn rt = wxeReturn(memenv, Ecmd.caller, true);
  rt.send(  rt.make_ref(app->getRef((void *)Result,memenv), "wxGraphicsMatrix"));

}

// wxGraphicsRenderer::CreatePath
void wxGraphicsRenderer_CreatePath(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd)
{
  ErlNifEnv *env = Ecmd.env;
  ERL_NIF_TERM * argv = Ecmd.args;
  wxGraphicsRenderer *This;
  This = (wxGraphicsRenderer *) memenv->getPtr(env, argv[0], "This");
  if(!This) throw wxe_badarg("This");
  wxGraphicsPath * Result = new wxGraphicsPath(This->CreatePath()); app->newPtr((void *) Result,4, memenv);;
  wxeReturn rt = wxeReturn(memenv, Ecmd.caller, true);
  rt.send(  rt.make_ref(app->getRef((void *)Result,memenv), "wxGraphicsPath"));

}

#endif // wxUSE_GRAPHICS_CONTEXT
// wxGrid::wxGrid
void wxGrid_new_0(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd)
{
  wxGrid * Result = new EwxGrid();
  app->newPtr((void *) Result, 0, memenv);
  wxeReturn rt = wxeReturn(memenv, Ecmd.caller, true);
  rt.send(  rt.make_ref(app->getRef((void *)Result,memenv), "wxGrid"));

}

// wxGrid::wxGrid
void wxGrid_new_3(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd)
{
  wxPoint pos= wxDefaultPosition;
  wxSize size= wxDefaultSize;
  long style=wxWANTS_CHARS;
  ErlNifEnv *env = Ecmd.env;
  ERL_NIF_TERM * argv = Ecmd.args;
  wxWindow *parent;
  parent = (wxWindow *) memenv->getPtr(env, argv[0], "parent");
  int id;
  if(!enif_get_int(env, argv[1], &id)) Badarg("id"); // wxWindowID
  ERL_NIF_TERM lstHead, lstTail;
  lstTail = argv[2];
  if(!enif_is_list(env, lstTail)) Badarg("Options");
  const ERL_NIF_TERM *tpl;
  int tpl_sz;
  while(!enif_is_empty_list(env, lstTail)) {
    if(!enif_get_list_cell(env, lstTail, &lstHead, &lstTail)) Badarg("Options");
    if(!enif_get_tuple(env, lstHead, &tpl_sz, &tpl) || tpl_sz != 2) Badarg("Options");
    if(enif_is_identical(tpl[0], enif_make_atom(env, "pos"))) {
  const ERL_NIF_TERM *pos_t;
  int pos_sz;
  if(!enif_get_tuple(env, tpl[1], &pos_sz, &pos_t)) Badarg("pos");
  int posX;
  if(!enif_get_int(env, pos_t[0], &posX)) Badarg("pos");
  int posY;
  if(!enif_get_int(env, pos_t[1], &posY)) Badarg("pos");
  pos = wxPoint(posX,posY);
    } else     if(enif_is_identical(tpl[0], enif_make_atom(env, "size"))) {
  const ERL_NIF_TERM *size_t;
  int size_sz;
  if(!enif_get_tuple(env, tpl[1], &size_sz, &size_t)) Badarg("size");
  int sizeW;
  if(!enif_get_int(env, size_t[0], &sizeW)) Badarg("size");
  int sizeH;
  if(!enif_get_int(env, size_t[1], &sizeH)) Badarg("size");
  size = wxSize(sizeW,sizeH);
    } else     if(enif_is_identical(tpl[0], enif_make_atom(env, "style"))) {
  if(!enif_get_long(env, tpl[1], &style)) Badarg("style");
    } else        Badarg("Options");
  };
  wxGrid * Result = new EwxGrid(parent,id,pos,size,style);
  app->newPtr((void *) Result, 0, memenv);
  wxeReturn rt = wxeReturn(memenv, Ecmd.caller, true);
  rt.send(  rt.make_ref(app->getRef((void *)Result,memenv), "wxGrid"));

}

// wxGrid::AppendCols
void wxGrid_AppendCols(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd)
{
  int numCols=1;
  bool updateLabels=true;
  ErlNifEnv *env = Ecmd.env;
  ERL_NIF_TERM * argv = Ecmd.args;
  wxGrid *This;
  This = (wxGrid *) memenv->getPtr(env, argv[0], "This");
  ERL_NIF_TERM lstHead, lstTail;
  lstTail = argv[1];
  if(!enif_is_list(env, lstTail)) Badarg("Options");
  const ERL_NIF_TERM *tpl;
  int tpl_sz;
  while(!enif_is_empty_list(env, lstTail)) {
    if(!enif_get_list_cell(env, lstTail, &lstHead, &lstTail)) Badarg("Options");
    if(!enif_get_tuple(env, lstHead, &tpl_sz, &tpl) || tpl_sz != 2) Badarg("Options");
    if(enif_is_identical(tpl[0], enif_make_atom(env, "numCols"))) {
  if(!enif_get_int(env, tpl[1], &numCols)) Badarg("numCols"); // int
    } else     if(enif_is_identical(tpl[0], enif_make_atom(env, "updateLabels"))) {
  updateLabels = enif_is_identical(tpl[1], WXE_ATOM_true);
    } else        Badarg("Options");
  };
  if(!This) throw wxe_badarg("This");
  bool Result = This->AppendCols(numCols,updateLabels);
  wxeReturn rt = wxeReturn(memenv, Ecmd.caller, true);
  rt.send(  rt.make_bool(Result));

}

// wxGrid::AppendRows
void wxGrid_AppendRows(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd)
{
  int numRows=1;
  bool updateLabels=true;
  ErlNifEnv *env = Ecmd.env;
  ERL_NIF_TERM * argv = Ecmd.args;
  wxGrid *This;
  This = (wxGrid *) memenv->getPtr(env, argv[0], "This");
  ERL_NIF_TERM lstHead, lstTail;
  lstTail = argv[1];
  if(!enif_is_list(env, lstTail)) Badarg("Options");
  const ERL_NIF_TERM *tpl;
  int tpl_sz;
  while(!enif_is_empty_list(env, lstTail)) {
    if(!enif_get_list_cell(env, lstTail, &lstHead, &lstTail)) Badarg("Options");
    if(!enif_get_tuple(env, lstHead, &tpl_sz, &tpl) || tpl_sz != 2) Badarg("Options");
    if(enif_is_identical(tpl[0], enif_make_atom(env, "numRows"))) {
  if(!enif_get_int(env, tpl[1], &numRows)) Badarg("numRows"); // int
    } else     if(enif_is_identical(tpl[0], enif_make_atom(env, "updateLabels"))) {
  updateLabels = enif_is_identical(tpl[1], WXE_ATOM_true);
    } else        Badarg("Options");
  };
  if(!This) throw wxe_badarg("This");
  bool Result = This->AppendRows(numRows,updateLabels);
  wxeReturn rt = wxeReturn(memenv, Ecmd.caller, true);
  rt.send(  rt.make_bool(Result));

}

// wxGrid::AutoSize
void wxGrid_AutoSize(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd)
{
  ErlNifEnv *env = Ecmd.env;
  ERL_NIF_TERM * argv = Ecmd.args;
  wxGrid *This;
  This = (wxGrid *) memenv->getPtr(env, argv[0], "This");
  if(!This) throw wxe_badarg("This");
  This->AutoSize();

}

// wxGrid::AutoSizeColumn
void wxGrid_AutoSizeColumn(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd)
{
  bool setAsMin=true;
  ErlNifEnv *env = Ecmd.env;
  ERL_NIF_TERM * argv = Ecmd.args;
  wxGrid *This;
  This = (wxGrid *) memenv->getPtr(env, argv[0], "This");
  int col;
  if(!enif_get_int(env, argv[1], &col)) Badarg("col"); // int
  ERL_NIF_TERM lstHead, lstTail;
  lstTail = argv[2];
  if(!enif_is_list(env, lstTail)) Badarg("Options");
  const ERL_NIF_TERM *tpl;
  int tpl_sz;
  while(!enif_is_empty_list(env, lstTail)) {
    if(!enif_get_list_cell(env, lstTail, &lstHead, &lstTail)) Badarg("Options");
    if(!enif_get_tuple(env, lstHead, &tpl_sz, &tpl) || tpl_sz != 2) Badarg("Options");
    if(enif_is_identical(tpl[0], enif_make_atom(env, "setAsMin"))) {
  setAsMin = enif_is_identical(tpl[1], WXE_ATOM_true);
    } else        Badarg("Options");
  };
  if(!This) throw wxe_badarg("This");
  This->AutoSizeColumn(col,setAsMin);

}

// wxGrid::AutoSizeColumns
void wxGrid_AutoSizeColumns(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd)
{
  bool setAsMin=true;
  ErlNifEnv *env = Ecmd.env;
  ERL_NIF_TERM * argv = Ecmd.args;
  wxGrid *This;
  This = (wxGrid *) memenv->getPtr(env, argv[0], "This");
  ERL_NIF_TERM lstHead, lstTail;
  lstTail = argv[1];
  if(!enif_is_list(env, lstTail)) Badarg("Options");
  const ERL_NIF_TERM *tpl;
  int tpl_sz;
  while(!enif_is_empty_list(env, lstTail)) {
    if(!enif_get_list_cell(env, lstTail, &lstHead, &lstTail)) Badarg("Options");
    if(!enif_get_tuple(env, lstHead, &tpl_sz, &tpl) || tpl_sz != 2) Badarg("Options");
    if(enif_is_identical(tpl[0], enif_make_atom(env, "setAsMin"))) {
  setAsMin = enif_is_identical(tpl[1], WXE_ATOM_true);
    } else        Badarg("Options");
  };
  if(!This) throw wxe_badarg("This");
  This->AutoSizeColumns(setAsMin);

}

// wxGrid::AutoSizeRow
void wxGrid_AutoSizeRow(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd)
{
  bool setAsMin=true;
  ErlNifEnv *env = Ecmd.env;
  ERL_NIF_TERM * argv = Ecmd.args;
  wxGrid *This;
  This = (wxGrid *) memenv->getPtr(env, argv[0], "This");
  int row;
  if(!enif_get_int(env, argv[1], &row)) Badarg("row"); // int
  ERL_NIF_TERM lstHead, lstTail;
  lstTail = argv[2];
  if(!enif_is_list(env, lstTail)) Badarg("Options");
  const ERL_NIF_TERM *tpl;
  int tpl_sz;
  while(!enif_is_empty_list(env, lstTail)) {
    if(!enif_get_list_cell(env, lstTail, &lstHead, &lstTail)) Badarg("Options");
    if(!enif_get_tuple(env, lstHead, &tpl_sz, &tpl) || tpl_sz != 2) Badarg("Options");
    if(enif_is_identical(tpl[0], enif_make_atom(env, "setAsMin"))) {
  setAsMin = enif_is_identical(tpl[1], WXE_ATOM_true);
    } else        Badarg("Options");
  };
  if(!This) throw wxe_badarg("This");
  This->AutoSizeRow(row,setAsMin);

}

// wxGrid::AutoSizeRows
void wxGrid_AutoSizeRows(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd)
{
  bool setAsMin=true;
  ErlNifEnv *env = Ecmd.env;
  ERL_NIF_TERM * argv = Ecmd.args;
  wxGrid *This;
  This = (wxGrid *) memenv->getPtr(env, argv[0], "This");
  ERL_NIF_TERM lstHead, lstTail;
  lstTail = argv[1];
  if(!enif_is_list(env, lstTail)) Badarg("Options");
  const ERL_NIF_TERM *tpl;
  int tpl_sz;
  while(!enif_is_empty_list(env, lstTail)) {
    if(!enif_get_list_cell(env, lstTail, &lstHead, &lstTail)) Badarg("Options");
    if(!enif_get_tuple(env, lstHead, &tpl_sz, &tpl) || tpl_sz != 2) Badarg("Options");
    if(enif_is_identical(tpl[0], enif_make_atom(env, "setAsMin"))) {
  setAsMin = enif_is_identical(tpl[1], WXE_ATOM_true);
    } else        Badarg("Options");
  };
  if(!This) throw wxe_badarg("This");
  This->AutoSizeRows(setAsMin);

}

// wxGrid::BeginBatch
void wxGrid_BeginBatch(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd)
{
  ErlNifEnv *env = Ecmd.env;
  ERL_NIF_TERM * argv = Ecmd.args;
  wxGrid *This;
  This = (wxGrid *) memenv->getPtr(env, argv[0], "This");
  if(!This) throw wxe_badarg("This");
  This->BeginBatch();

}

// wxGrid::BlockToDeviceRect
void wxGrid_BlockToDeviceRect(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd)
{
  ErlNifEnv *env = Ecmd.env;
  ERL_NIF_TERM * argv = Ecmd.args;
  wxGrid *This;
  This = (wxGrid *) memenv->getPtr(env, argv[0], "This");
  const ERL_NIF_TERM *topLeft_t;
  int topLeft_sz;
  if(!enif_get_tuple(env, argv[1], &topLeft_sz, &topLeft_t)) Badarg("topLeft");
  int topLeftR;
  if(!enif_get_int(env, topLeft_t[0], &topLeftR)) Badarg("topLeft");
  int topLeftC;
  if(!enif_get_int(env, topLeft_t[1], &topLeftC)) Badarg("topLeft");
  wxGridCellCoords topLeft = wxGridCellCoords(topLeftR,topLeftC);
  const ERL_NIF_TERM *bottomRight_t;
  int bottomRight_sz;
  if(!enif_get_tuple(env, argv[2], &bottomRight_sz, &bottomRight_t)) Badarg("bottomRight");
  int bottomRightR;
  if(!enif_get_int(env, bottomRight_t[0], &bottomRightR)) Badarg("bottomRight");
  int bottomRightC;
  if(!enif_get_int(env, bottomRight_t[1], &bottomRightC)) Badarg("bottomRight");
  wxGridCellCoords bottomRight = wxGridCellCoords(bottomRightR,bottomRightC);
  if(!This) throw wxe_badarg("This");
  wxRect Result = This->BlockToDeviceRect(topLeft,bottomRight);
  wxeReturn rt = wxeReturn(memenv, Ecmd.caller, true);
  rt.send(  rt.make(Result));

}

// wxGrid::CanDragCell
void wxGrid_CanDragCell(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd)
{
  ErlNifEnv *env = Ecmd.env;
  ERL_NIF_TERM * argv = Ecmd.args;
  wxGrid *This;
  This = (wxGrid *) memenv->getPtr(env, argv[0], "This");
  if(!This) throw wxe_badarg("This");
  bool Result = This->CanDragCell();
  wxeReturn rt = wxeReturn(memenv, Ecmd.caller, true);
  rt.send(  rt.make_bool(Result));

}

// wxGrid::CanDragColMove
void wxGrid_CanDragColMove(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd)
{
  ErlNifEnv *env = Ecmd.env;
  ERL_NIF_TERM * argv = Ecmd.args;
  wxGrid *This;
  This = (wxGrid *) memenv->getPtr(env, argv[0], "This");
  if(!This) throw wxe_badarg("This");
  bool Result = This->CanDragColMove();
  wxeReturn rt = wxeReturn(memenv, Ecmd.caller, true);
  rt.send(  rt.make_bool(Result));

}

#if wxCHECK_VERSION(3,1,4)
// wxGrid::CanDragGridRowEdges
void wxGrid_CanDragGridRowEdges(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd)
{
  ErlNifEnv *env = Ecmd.env;
  ERL_NIF_TERM * argv = Ecmd.args;
  wxGrid *This;
  This = (wxGrid *) memenv->getPtr(env, argv[0], "This");
  if(!This) throw wxe_badarg("This");
  bool Result = This->CanDragGridRowEdges();
  wxeReturn rt = wxeReturn(memenv, Ecmd.caller, true);
  rt.send(  rt.make_bool(Result));

}

#endif
// wxGrid::CanDragColSize
void wxGrid_CanDragColSize(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd)
{
  ErlNifEnv *env = Ecmd.env;
  ERL_NIF_TERM * argv = Ecmd.args;
  wxGrid *This;
  This = (wxGrid *) memenv->getPtr(env, argv[0], "This");
  int col;
  if(!enif_get_int(env, argv[1], &col)) Badarg("col"); // int
  if(!This) throw wxe_badarg("This");
  bool Result = This->CanDragColSize(col);
  wxeReturn rt = wxeReturn(memenv, Ecmd.caller, true);
  rt.send(  rt.make_bool(Result));

}

// wxGrid::CanDragRowSize
void wxGrid_CanDragRowSize(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd)
{
  ErlNifEnv *env = Ecmd.env;
  ERL_NIF_TERM * argv = Ecmd.args;
  wxGrid *This;
  This = (wxGrid *) memenv->getPtr(env, argv[0], "This");
  int row;
  if(!enif_get_int(env, argv[1], &row)) Badarg("row"); // int
  if(!This) throw wxe_badarg("This");
  bool Result = This->CanDragRowSize(row);
  wxeReturn rt = wxeReturn(memenv, Ecmd.caller, true);
  rt.send(  rt.make_bool(Result));

}

// wxGrid::CanDragGridSize
void wxGrid_CanDragGridSize(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd)
{
  ErlNifEnv *env = Ecmd.env;
  ERL_NIF_TERM * argv = Ecmd.args;
  wxGrid *This;
  This = (wxGrid *) memenv->getPtr(env, argv[0], "This");
  if(!This) throw wxe_badarg("This");
  bool Result = This->CanDragGridSize();
  wxeReturn rt = wxeReturn(memenv, Ecmd.caller, true);
  rt.send(  rt.make_bool(Result));

}

// wxGrid::CanEnableCellControl
void wxGrid_CanEnableCellControl(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd)
{
  ErlNifEnv *env = Ecmd.env;
  ERL_NIF_TERM * argv = Ecmd.args;
  wxGrid *This;
  This = (wxGrid *) memenv->getPtr(env, argv[0], "This");
  if(!This) throw wxe_badarg("This");
  bool Result = This->CanEnableCellControl();
  wxeReturn rt = wxeReturn(memenv, Ecmd.caller, true);
  rt.send(  rt.make_bool(Result));

}

// wxGrid::CellToRect
void wxGrid_CellToRect_2(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd)
{
  ErlNifEnv *env = Ecmd.env;
  ERL_NIF_TERM * argv = Ecmd.args;
  wxGrid *This;
  This = (wxGrid *) memenv->getPtr(env, argv[0], "This");
  int row;
  if(!enif_get_int(env, argv[1], &row)) Badarg("row"); // int
  int col;
  if(!enif_get_int(env, argv[2], &col)) Badarg("col"); // int
  if(!This) throw wxe_badarg("This");
  wxRect Result = This->CellToRect(row,col);
  wxeReturn rt = wxeReturn(memenv, Ecmd.caller, true);
  rt.send(  rt.make(Result));

}

// wxGrid::CellToRect
void wxGrid_CellToRect_1(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd)
{
  ErlNifEnv *env = Ecmd.env;
  ERL_NIF_TERM * argv = Ecmd.args;
  wxGrid *This;
  This = (wxGrid *) memenv->getPtr(env, argv[0], "This");
  const ERL_NIF_TERM *coords_t;
  int coords_sz;
  if(!enif_get_tuple(env, argv[1], &coords_sz, &coords_t)) Badarg("coords");
  int coordsR;
  if(!enif_get_int(env, coords_t[0], &coordsR)) Badarg("coords");
  int coordsC;
  if(!enif_get_int(env, coords_t[1], &coordsC)) Badarg("coords");
  wxGridCellCoords coords = wxGridCellCoords(coordsR,coordsC);
  if(!This) throw wxe_badarg("This");
  wxRect Result = This->CellToRect(coords);
  wxeReturn rt = wxeReturn(memenv, Ecmd.caller, true);
  rt.send(  rt.make(Result));

}

// wxGrid::ClearGrid
void wxGrid_ClearGrid(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd)
{
  ErlNifEnv *env = Ecmd.env;
  ERL_NIF_TERM * argv = Ecmd.args;
  wxGrid *This;
  This = (wxGrid *) memenv->getPtr(env, argv[0], "This");
  if(!This) throw wxe_badarg("This");
  This->ClearGrid();

}

// wxGrid::ClearSelection
void wxGrid_ClearSelection(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd)
{
  ErlNifEnv *env = Ecmd.env;
  ERL_NIF_TERM * argv = Ecmd.args;
  wxGrid *This;
  This = (wxGrid *) memenv->getPtr(env, argv[0], "This");
  if(!This) throw wxe_badarg("This");
  This->ClearSelection();

}

// wxGrid::CreateGrid
void wxGrid_CreateGrid(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd)
{
 wxGrid::wxGridSelectionModes selmode=wxGrid::wxGridSelectCells;
  ErlNifEnv *env = Ecmd.env;
  ERL_NIF_TERM * argv = Ecmd.args;
  wxGrid *This;
  This = (wxGrid *) memenv->getPtr(env, argv[0], "This");
  int numRows;
  if(!enif_get_int(env, argv[1], &numRows)) Badarg("numRows"); // int
  int numCols;
  if(!enif_get_int(env, argv[2], &numCols)) Badarg("numCols"); // int
  ERL_NIF_TERM lstHead, lstTail;
  lstTail = argv[3];
  if(!enif_is_list(env, lstTail)) Badarg("Options");
  const ERL_NIF_TERM *tpl;
  int tpl_sz;
  while(!enif_is_empty_list(env, lstTail)) {
    if(!enif_get_list_cell(env, lstTail, &lstHead, &lstTail)) Badarg("Options");
    if(!enif_get_tuple(env, lstHead, &tpl_sz, &tpl) || tpl_sz != 2) Badarg("Options");
    if(enif_is_identical(tpl[0], enif_make_atom(env, "selmode"))) {
  if(!enif_get_int(env, tpl[1], (int *) &selmode)) Badarg("selmode"); // enum
    } else        Badarg("Options");
  };
  if(!This) throw wxe_badarg("This");
  bool Result = This->CreateGrid(numRows,numCols,selmode);
  wxeReturn rt = wxeReturn(memenv, Ecmd.caller, true);
  rt.send(  rt.make_bool(Result));

}

// wxGrid::DeleteCols
void wxGrid_DeleteCols(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd)
{
  int pos=0;
  int numCols=1;
  bool updateLabels=true;
  ErlNifEnv *env = Ecmd.env;
  ERL_NIF_TERM * argv = Ecmd.args;
  wxGrid *This;
  This = (wxGrid *) memenv->getPtr(env, argv[0], "This");
  ERL_NIF_TERM lstHead, lstTail;
  lstTail = argv[1];
  if(!enif_is_list(env, lstTail)) Badarg("Options");
  const ERL_NIF_TERM *tpl;
  int tpl_sz;
  while(!enif_is_empty_list(env, lstTail)) {
    if(!enif_get_list_cell(env, lstTail, &lstHead, &lstTail)) Badarg("Options");
    if(!enif_get_tuple(env, lstHead, &tpl_sz, &tpl) || tpl_sz != 2) Badarg("Options");
    if(enif_is_identical(tpl[0], enif_make_atom(env, "pos"))) {
  if(!enif_get_int(env, tpl[1], &pos)) Badarg("pos"); // int
    } else     if(enif_is_identical(tpl[0], enif_make_atom(env, "numCols"))) {
  if(!enif_get_int(env, tpl[1], &numCols)) Badarg("numCols"); // int
    } else     if(enif_is_identical(tpl[0], enif_make_atom(env, "updateLabels"))) {
  updateLabels = enif_is_identical(tpl[1], WXE_ATOM_true);
    } else        Badarg("Options");
  };
  if(!This) throw wxe_badarg("This");
  bool Result = This->DeleteCols(pos,numCols,updateLabels);
  wxeReturn rt = wxeReturn(memenv, Ecmd.caller, true);
  rt.send(  rt.make_bool(Result));

}

// wxGrid::DeleteRows
void wxGrid_DeleteRows(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd)
{
  int pos=0;
  int numRows=1;
  bool updateLabels=true;
  ErlNifEnv *env = Ecmd.env;
  ERL_NIF_TERM * argv = Ecmd.args;
  wxGrid *This;
  This = (wxGrid *) memenv->getPtr(env, argv[0], "This");
  ERL_NIF_TERM lstHead, lstTail;
  lstTail = argv[1];
  if(!enif_is_list(env, lstTail)) Badarg("Options");
  const ERL_NIF_TERM *tpl;
  int tpl_sz;
  while(!enif_is_empty_list(env, lstTail)) {
    if(!enif_get_list_cell(env, lstTail, &lstHead, &lstTail)) Badarg("Options");
    if(!enif_get_tuple(env, lstHead, &tpl_sz, &tpl) || tpl_sz != 2) Badarg("Options");
    if(enif_is_identical(tpl[0], enif_make_atom(env, "pos"))) {
  if(!enif_get_int(env, tpl[1], &pos)) Badarg("pos"); // int
    } else     if(enif_is_identical(tpl[0], enif_make_atom(env, "numRows"))) {
  if(!enif_get_int(env, tpl[1], &numRows)) Badarg("numRows"); // int
    } else     if(enif_is_identical(tpl[0], enif_make_atom(env, "updateLabels"))) {
  updateLabels = enif_is_identical(tpl[1], WXE_ATOM_true);
    } else        Badarg("Options");
  };
  if(!This) throw wxe_badarg("This");
  bool Result = This->DeleteRows(pos,numRows,updateLabels);
  wxeReturn rt = wxeReturn(memenv, Ecmd.caller, true);
  rt.send(  rt.make_bool(Result));

}

// wxGrid::DisableCellEditControl
void wxGrid_DisableCellEditControl(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd)
{
  ErlNifEnv *env = Ecmd.env;
  ERL_NIF_TERM * argv = Ecmd.args;
  wxGrid *This;
  This = (wxGrid *) memenv->getPtr(env, argv[0], "This");
  if(!This) throw wxe_badarg("This");
  This->DisableCellEditControl();

}

// wxGrid::DisableDragColSize
void wxGrid_DisableDragColSize(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd)
{
  ErlNifEnv *env = Ecmd.env;
  ERL_NIF_TERM * argv = Ecmd.args;
  wxGrid *This;
  This = (wxGrid *) memenv->getPtr(env, argv[0], "This");
  if(!This) throw wxe_badarg("This");
  This->DisableDragColSize();

}

// wxGrid::DisableDragGridSize
void wxGrid_DisableDragGridSize(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd)
{
  ErlNifEnv *env = Ecmd.env;
  ERL_NIF_TERM * argv = Ecmd.args;
  wxGrid *This;
  This = (wxGrid *) memenv->getPtr(env, argv[0], "This");
  if(!This) throw wxe_badarg("This");
  This->DisableDragGridSize();

}

// wxGrid::DisableDragRowSize
void wxGrid_DisableDragRowSize(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd)
{
  ErlNifEnv *env = Ecmd.env;
  ERL_NIF_TERM * argv = Ecmd.args;
  wxGrid *This;
  This = (wxGrid *) memenv->getPtr(env, argv[0], "This");
  if(!This) throw wxe_badarg("This");
  This->DisableDragRowSize();

}

// wxGrid::EnableCellEditControl
void wxGrid_EnableCellEditControl(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd)
{
  bool enable=true;
  ErlNifEnv *env = Ecmd.env;
  ERL_NIF_TERM * argv = Ecmd.args;
  wxGrid *This;
  This = (wxGrid *) memenv->getPtr(env, argv[0], "This");
  ERL_NIF_TERM lstHead, lstTail;
  lstTail = argv[1];
  if(!enif_is_list(env, lstTail)) Badarg("Options");
  const ERL_NIF_TERM *tpl;
  int tpl_sz;
  while(!enif_is_empty_list(env, lstTail)) {
    if(!enif_get_list_cell(env, lstTail, &lstHead, &lstTail)) Badarg("Options");
    if(!enif_get_tuple(env, lstHead, &tpl_sz, &tpl) || tpl_sz != 2) Badarg("Options");
    if(enif_is_identical(tpl[0], enif_make_atom(env, "enable"))) {
  enable = enif_is_identical(tpl[1], WXE_ATOM_true);
    } else        Badarg("Options");
  };
  if(!This) throw wxe_badarg("This");
  This->EnableCellEditControl(enable);

}

// wxGrid::EnableDragColSize
void wxGrid_EnableDragColSize(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd)
{
  bool enable=true;
  ErlNifEnv *env = Ecmd.env;
  ERL_NIF_TERM * argv = Ecmd.args;
  wxGrid *This;
  This = (wxGrid *) memenv->getPtr(env, argv[0], "This");
  ERL_NIF_TERM lstHead, lstTail;
  lstTail = argv[1];
  if(!enif_is_list(env, lstTail)) Badarg("Options");
  const ERL_NIF_TERM *tpl;
  int tpl_sz;
  while(!enif_is_empty_list(env, lstTail)) {
    if(!enif_get_list_cell(env, lstTail, &lstHead, &lstTail)) Badarg("Options");
    if(!enif_get_tuple(env, lstHead, &tpl_sz, &tpl) || tpl_sz != 2) Badarg("Options");
    if(enif_is_identical(tpl[0], enif_make_atom(env, "enable"))) {
  enable = enif_is_identical(tpl[1], WXE_ATOM_true);
    } else        Badarg("Options");
  };
  if(!This) throw wxe_badarg("This");
  This->EnableDragColSize(enable);

}

// wxGrid::EnableDragGridSize
void wxGrid_EnableDragGridSize(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd)
{
  bool enable=true;
  ErlNifEnv *env = Ecmd.env;
  ERL_NIF_TERM * argv = Ecmd.args;
  wxGrid *This;
  This = (wxGrid *) memenv->getPtr(env, argv[0], "This");
  ERL_NIF_TERM lstHead, lstTail;
  lstTail = argv[1];
  if(!enif_is_list(env, lstTail)) Badarg("Options");
  const ERL_NIF_TERM *tpl;
  int tpl_sz;
  while(!enif_is_empty_list(env, lstTail)) {
    if(!enif_get_list_cell(env, lstTail, &lstHead, &lstTail)) Badarg("Options");
    if(!enif_get_tuple(env, lstHead, &tpl_sz, &tpl) || tpl_sz != 2) Badarg("Options");
    if(enif_is_identical(tpl[0], enif_make_atom(env, "enable"))) {
  enable = enif_is_identical(tpl[1], WXE_ATOM_true);
    } else        Badarg("Options");
  };
  if(!This) throw wxe_badarg("This");
  This->EnableDragGridSize(enable);

}

// wxGrid::EnableDragRowSize
void wxGrid_EnableDragRowSize(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd)
{
  bool enable=true;
  ErlNifEnv *env = Ecmd.env;
  ERL_NIF_TERM * argv = Ecmd.args;
  wxGrid *This;
  This = (wxGrid *) memenv->getPtr(env, argv[0], "This");
  ERL_NIF_TERM lstHead, lstTail;
  lstTail = argv[1];
  if(!enif_is_list(env, lstTail)) Badarg("Options");
  const ERL_NIF_TERM *tpl;
  int tpl_sz;
  while(!enif_is_empty_list(env, lstTail)) {
    if(!enif_get_list_cell(env, lstTail, &lstHead, &lstTail)) Badarg("Options");
    if(!enif_get_tuple(env, lstHead, &tpl_sz, &tpl) || tpl_sz != 2) Badarg("Options");
    if(enif_is_identical(tpl[0], enif_make_atom(env, "enable"))) {
  enable = enif_is_identical(tpl[1], WXE_ATOM_true);
    } else        Badarg("Options");
  };
  if(!This) throw wxe_badarg("This");
  This->EnableDragRowSize(enable);

}

// wxGrid::EnableEditing
void wxGrid_EnableEditing(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd)
{
  ErlNifEnv *env = Ecmd.env;
  ERL_NIF_TERM * argv = Ecmd.args;
  wxGrid *This;
  This = (wxGrid *) memenv->getPtr(env, argv[0], "This");
  bool edit;
  edit = enif_is_identical(argv[1], WXE_ATOM_true);
  if(!This) throw wxe_badarg("This");
  This->EnableEditing(edit);

}

// wxGrid::EnableGridLines
void wxGrid_EnableGridLines(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd)
{
  bool enable=true;
  ErlNifEnv *env = Ecmd.env;
  ERL_NIF_TERM * argv = Ecmd.args;
  wxGrid *This;
  This = (wxGrid *) memenv->getPtr(env, argv[0], "This");
  ERL_NIF_TERM lstHead, lstTail;
  lstTail = argv[1];
  if(!enif_is_list(env, lstTail)) Badarg("Options");
  const ERL_NIF_TERM *tpl;
  int tpl_sz;
  while(!enif_is_empty_list(env, lstTail)) {
    if(!enif_get_list_cell(env, lstTail, &lstHead, &lstTail)) Badarg("Options");
    if(!enif_get_tuple(env, lstHead, &tpl_sz, &tpl) || tpl_sz != 2) Badarg("Options");
    if(enif_is_identical(tpl[0], enif_make_atom(env, "enable"))) {
  enable = enif_is_identical(tpl[1], WXE_ATOM_true);
    } else        Badarg("Options");
  };
  if(!This) throw wxe_badarg("This");
  This->EnableGridLines(enable);

}

// wxGrid::EndBatch
void wxGrid_EndBatch(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd)
{
  ErlNifEnv *env = Ecmd.env;
  ERL_NIF_TERM * argv = Ecmd.args;
  wxGrid *This;
  This = (wxGrid *) memenv->getPtr(env, argv[0], "This");
  if(!This) throw wxe_badarg("This");
  This->EndBatch();

}

// wxGrid::Fit
void wxGrid_Fit(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd)
{
  ErlNifEnv *env = Ecmd.env;
  ERL_NIF_TERM * argv = Ecmd.args;
  wxGrid *This;
  This = (wxGrid *) memenv->getPtr(env, argv[0], "This");
  if(!This) throw wxe_badarg("This");
  This->Fit();

}

// wxGrid::ForceRefresh
void wxGrid_ForceRefresh(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd)
{
  ErlNifEnv *env = Ecmd.env;
  ERL_NIF_TERM * argv = Ecmd.args;
  wxGrid *This;
  This = (wxGrid *) memenv->getPtr(env, argv[0], "This");
  if(!This) throw wxe_badarg("This");
  This->ForceRefresh();

}

// wxGrid::GetBatchCount
void wxGrid_GetBatchCount(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd)
{
  ErlNifEnv *env = Ecmd.env;
  ERL_NIF_TERM * argv = Ecmd.args;
  wxGrid *This;
  This = (wxGrid *) memenv->getPtr(env, argv[0], "This");
  if(!This) throw wxe_badarg("This");
  int Result = This->GetBatchCount();
  wxeReturn rt = wxeReturn(memenv, Ecmd.caller, true);
  rt.send(  rt.make_int(Result));

}

// wxGrid::GetCellAlignment
void wxGrid_GetCellAlignment(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd)
{
  int horiz;
  int vert;
  ErlNifEnv *env = Ecmd.env;
  ERL_NIF_TERM * argv = Ecmd.args;
  wxGrid *This;
  This = (wxGrid *) memenv->getPtr(env, argv[0], "This");
  int row;
  if(!enif_get_int(env, argv[1], &row)) Badarg("row"); // int
  int col;
  if(!enif_get_int(env, argv[2], &col)) Badarg("col"); // int
  if(!This) throw wxe_badarg("This");
  This->GetCellAlignment(row,col,&horiz,&vert);
  wxeReturn rt = wxeReturn(memenv, Ecmd.caller, true);
  ERL_NIF_TERM msg = enif_make_tuple2(rt.env,
  rt.make_int(horiz),
  rt.make_int(vert));
  rt.send(msg);

}

// wxGrid::GetCellBackgroundColour
void wxGrid_GetCellBackgroundColour(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd)
{
  ErlNifEnv *env = Ecmd.env;
  ERL_NIF_TERM * argv = Ecmd.args;
  wxGrid *This;
  This = (wxGrid *) memenv->getPtr(env, argv[0], "This");
  int row;
  if(!enif_get_int(env, argv[1], &row)) Badarg("row"); // int
  int col;
  if(!enif_get_int(env, argv[2], &col)) Badarg("col"); // int
  if(!This) throw wxe_badarg("This");
  wxColour Result = This->GetCellBackgroundColour(row,col);
  wxeReturn rt = wxeReturn(memenv, Ecmd.caller, true);
  rt.send(  rt.make(Result));

}

// wxGrid::GetCellEditor
void wxGrid_GetCellEditor(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd)
{
  ErlNifEnv *env = Ecmd.env;
  ERL_NIF_TERM * argv = Ecmd.args;
  wxGrid *This;
  This = (wxGrid *) memenv->getPtr(env, argv[0], "This");
  int row;
  if(!enif_get_int(env, argv[1], &row)) Badarg("row"); // int
  int col;
  if(!enif_get_int(env, argv[2], &col)) Badarg("col"); // int
  if(!This) throw wxe_badarg("This");
  wxGridCellEditor * Result = (wxGridCellEditor*)This->GetCellEditor(row,col);
  wxeReturn rt = wxeReturn(memenv, Ecmd.caller, true);
  rt.send(  rt.make_ref(app->getRef((void *)Result,memenv), "wxGridCellEditor"));

}

// wxGrid::GetCellFont
void wxGrid_GetCellFont(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd)
{
  ErlNifEnv *env = Ecmd.env;
  ERL_NIF_TERM * argv = Ecmd.args;
  wxGrid *This;
  This = (wxGrid *) memenv->getPtr(env, argv[0], "This");
  int row;
  if(!enif_get_int(env, argv[1], &row)) Badarg("row"); // int
  int col;
  if(!enif_get_int(env, argv[2], &col)) Badarg("col"); // int
  if(!This) throw wxe_badarg("This");
  wxFont * Result = new wxFont(This->GetCellFont(row,col)); app->newPtr((void *) Result,3, memenv);;
  wxeReturn rt = wxeReturn(memenv, Ecmd.caller, true);
  rt.send(  rt.make_ref(app->getRef((void *)Result,memenv), "wxFont"));

}

// wxGrid::GetCellRenderer
void wxGrid_GetCellRenderer(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd)
{
  ErlNifEnv *env = Ecmd.env;
  ERL_NIF_TERM * argv = Ecmd.args;
  wxGrid *This;
  This = (wxGrid *) memenv->getPtr(env, argv[0], "This");
  int row;
  if(!enif_get_int(env, argv[1], &row)) Badarg("row"); // int
  int col;
  if(!enif_get_int(env, argv[2], &col)) Badarg("col"); // int
  if(!This) throw wxe_badarg("This");
  wxGridCellRenderer * Result = (wxGridCellRenderer*)This->GetCellRenderer(row,col);
  wxeReturn rt = wxeReturn(memenv, Ecmd.caller, true);
  rt.send(  rt.make_ref(app->getRef((void *)Result,memenv), "wxGridCellRenderer"));

}

// wxGrid::GetCellTextColour
void wxGrid_GetCellTextColour(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd)
{
  ErlNifEnv *env = Ecmd.env;
  ERL_NIF_TERM * argv = Ecmd.args;
  wxGrid *This;
  This = (wxGrid *) memenv->getPtr(env, argv[0], "This");
  int row;
  if(!enif_get_int(env, argv[1], &row)) Badarg("row"); // int
  int col;
  if(!enif_get_int(env, argv[2], &col)) Badarg("col"); // int
  if(!This) throw wxe_badarg("This");
  wxColour Result = This->GetCellTextColour(row,col);
  wxeReturn rt = wxeReturn(memenv, Ecmd.caller, true);
  rt.send(  rt.make(Result));

}

// wxGrid::GetCellValue
void wxGrid_GetCellValue_2(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd)
{
  ErlNifEnv *env = Ecmd.env;
  ERL_NIF_TERM * argv = Ecmd.args;
  wxGrid *This;
  This = (wxGrid *) memenv->getPtr(env, argv[0], "This");
  int row;
  if(!enif_get_int(env, argv[1], &row)) Badarg("row"); // int
  int col;
  if(!enif_get_int(env, argv[2], &col)) Badarg("col"); // int
  if(!This) throw wxe_badarg("This");
  wxString Result = This->GetCellValue(row,col);
  wxeReturn rt = wxeReturn(memenv, Ecmd.caller, true);
  rt.send(  rt.make(Result));

}

// wxGrid::GetCellValue
void wxGrid_GetCellValue_1(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd)
{
  ErlNifEnv *env = Ecmd.env;
  ERL_NIF_TERM * argv = Ecmd.args;
  wxGrid *This;
  This = (wxGrid *) memenv->getPtr(env, argv[0], "This");
  const ERL_NIF_TERM *coords_t;
  int coords_sz;
  if(!enif_get_tuple(env, argv[1], &coords_sz, &coords_t)) Badarg("coords");
  int coordsR;
  if(!enif_get_int(env, coords_t[0], &coordsR)) Badarg("coords");
  int coordsC;
  if(!enif_get_int(env, coords_t[1], &coordsC)) Badarg("coords");
  wxGridCellCoords coords = wxGridCellCoords(coordsR,coordsC);
  if(!This) throw wxe_badarg("This");
  wxString Result = This->GetCellValue(coords);
  wxeReturn rt = wxeReturn(memenv, Ecmd.caller, true);
  rt.send(  rt.make(Result));

}

// wxGrid::GetColLabelAlignment
void wxGrid_GetColLabelAlignment(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd)
{
  int horiz;
  int vert;
  ErlNifEnv *env = Ecmd.env;
  ERL_NIF_TERM * argv = Ecmd.args;
  wxGrid *This;
  This = (wxGrid *) memenv->getPtr(env, argv[0], "This");
  if(!This) throw wxe_badarg("This");
  This->GetColLabelAlignment(&horiz,&vert);
  wxeReturn rt = wxeReturn(memenv, Ecmd.caller, true);
  ERL_NIF_TERM msg = enif_make_tuple2(rt.env,
  rt.make_int(horiz),
  rt.make_int(vert));
  rt.send(msg);

}

// wxGrid::GetColLabelSize
void wxGrid_GetColLabelSize(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd)
{
  ErlNifEnv *env = Ecmd.env;
  ERL_NIF_TERM * argv = Ecmd.args;
  wxGrid *This;
  This = (wxGrid *) memenv->getPtr(env, argv[0], "This");
  if(!This) throw wxe_badarg("This");
  int Result = This->GetColLabelSize();
  wxeReturn rt = wxeReturn(memenv, Ecmd.caller, true);
  rt.send(  rt.make_int(Result));

}

// wxGrid::GetColLabelValue
void wxGrid_GetColLabelValue(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd)
{
  ErlNifEnv *env = Ecmd.env;
  ERL_NIF_TERM * argv = Ecmd.args;
  wxGrid *This;
  This = (wxGrid *) memenv->getPtr(env, argv[0], "This");
  int col;
  if(!enif_get_int(env, argv[1], &col)) Badarg("col"); // int
  if(!This) throw wxe_badarg("This");
  wxString Result = This->GetColLabelValue(col);
  wxeReturn rt = wxeReturn(memenv, Ecmd.caller, true);
  rt.send(  rt.make(Result));

}

// wxGrid::GetColMinimalAcceptableWidth
void wxGrid_GetColMinimalAcceptableWidth(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd)
{
  ErlNifEnv *env = Ecmd.env;
  ERL_NIF_TERM * argv = Ecmd.args;
  wxGrid *This;
  This = (wxGrid *) memenv->getPtr(env, argv[0], "This");
  if(!This) throw wxe_badarg("This");
  int Result = This->GetColMinimalAcceptableWidth();
  wxeReturn rt = wxeReturn(memenv, Ecmd.caller, true);
  rt.send(  rt.make_int(Result));

}

// wxGrid::GetDefaultCellAlignment
void wxGrid_GetDefaultCellAlignment(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd)
{
  int horiz;
  int vert;
  ErlNifEnv *env = Ecmd.env;
  ERL_NIF_TERM * argv = Ecmd.args;
  wxGrid *This;
  This = (wxGrid *) memenv->getPtr(env, argv[0], "This");
  if(!This) throw wxe_badarg("This");
  This->GetDefaultCellAlignment(&horiz,&vert);
  wxeReturn rt = wxeReturn(memenv, Ecmd.caller, true);
  ERL_NIF_TERM msg = enif_make_tuple2(rt.env,
  rt.make_int(horiz),
  rt.make_int(vert));
  rt.send(msg);

}

// wxGrid::GetDefaultCellBackgroundColour
void wxGrid_GetDefaultCellBackgroundColour(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd)
{
  ErlNifEnv *env = Ecmd.env;
  ERL_NIF_TERM * argv = Ecmd.args;
  wxGrid *This;
  This = (wxGrid *) memenv->getPtr(env, argv[0], "This");
  if(!This) throw wxe_badarg("This");
  wxColour Result = This->GetDefaultCellBackgroundColour();
  wxeReturn rt = wxeReturn(memenv, Ecmd.caller, true);
  rt.send(  rt.make(Result));

}

// wxGrid::GetDefaultCellFont
void wxGrid_GetDefaultCellFont(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd)
{
  ErlNifEnv *env = Ecmd.env;
  ERL_NIF_TERM * argv = Ecmd.args;
  wxGrid *This;
  This = (wxGrid *) memenv->getPtr(env, argv[0], "This");
  if(!This) throw wxe_badarg("This");
  wxFont * Result = new wxFont(This->GetDefaultCellFont()); app->newPtr((void *) Result,3, memenv);;
  wxeReturn rt = wxeReturn(memenv, Ecmd.caller, true);
  rt.send(  rt.make_ref(app->getRef((void *)Result,memenv), "wxFont"));

}

// wxGrid::GetDefaultCellTextColour
void wxGrid_GetDefaultCellTextColour(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd)
{
  ErlNifEnv *env = Ecmd.env;
  ERL_NIF_TERM * argv = Ecmd.args;
  wxGrid *This;
  This = (wxGrid *) memenv->getPtr(env, argv[0], "This");
  if(!This) throw wxe_badarg("This");
  wxColour Result = This->GetDefaultCellTextColour();
  wxeReturn rt = wxeReturn(memenv, Ecmd.caller, true);
  rt.send(  rt.make(Result));

}

// wxGrid::GetDefaultColLabelSize
void wxGrid_GetDefaultColLabelSize(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd)
{
  ErlNifEnv *env = Ecmd.env;
  ERL_NIF_TERM * argv = Ecmd.args;
  wxGrid *This;
  This = (wxGrid *) memenv->getPtr(env, argv[0], "This");
  if(!This) throw wxe_badarg("This");
  int Result = This->GetDefaultColLabelSize();
  wxeReturn rt = wxeReturn(memenv, Ecmd.caller, true);
  rt.send(  rt.make_int(Result));

}

// wxGrid::GetDefaultColSize
void wxGrid_GetDefaultColSize(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd)
{
  ErlNifEnv *env = Ecmd.env;
  ERL_NIF_TERM * argv = Ecmd.args;
  wxGrid *This;
  This = (wxGrid *) memenv->getPtr(env, argv[0], "This");
  if(!This) throw wxe_badarg("This");
  int Result = This->GetDefaultColSize();
  wxeReturn rt = wxeReturn(memenv, Ecmd.caller, true);
  rt.send(  rt.make_int(Result));

}

// wxGrid::GetDefaultEditor
void wxGrid_GetDefaultEditor(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd)
{
  ErlNifEnv *env = Ecmd.env;
  ERL_NIF_TERM * argv = Ecmd.args;
  wxGrid *This;
  This = (wxGrid *) memenv->getPtr(env, argv[0], "This");
  if(!This) throw wxe_badarg("This");
  wxGridCellEditor * Result = (wxGridCellEditor*)This->GetDefaultEditor();
  wxeReturn rt = wxeReturn(memenv, Ecmd.caller, true);
  rt.send(  rt.make_ref(app->getRef((void *)Result,memenv), "wxGridCellEditor"));

}

// wxGrid::GetDefaultEditorForCell
void wxGrid_GetDefaultEditorForCell_2(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd)
{
  ErlNifEnv *env = Ecmd.env;
  ERL_NIF_TERM * argv = Ecmd.args;
  wxGrid *This;
  This = (wxGrid *) memenv->getPtr(env, argv[0], "This");
  int row;
  if(!enif_get_int(env, argv[1], &row)) Badarg("row"); // int
  int col;
  if(!enif_get_int(env, argv[2], &col)) Badarg("col"); // int
  if(!This) throw wxe_badarg("This");
  wxGridCellEditor * Result = (wxGridCellEditor*)This->GetDefaultEditorForCell(row,col);
  wxeReturn rt = wxeReturn(memenv, Ecmd.caller, true);
  rt.send(  rt.make_ref(app->getRef((void *)Result,memenv), "wxGridCellEditor"));

}

// wxGrid::GetDefaultEditorForCell
void wxGrid_GetDefaultEditorForCell_1(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd)
{
  ErlNifEnv *env = Ecmd.env;
  ERL_NIF_TERM * argv = Ecmd.args;
  wxGrid *This;
  This = (wxGrid *) memenv->getPtr(env, argv[0], "This");
  const ERL_NIF_TERM *c_t;
  int c_sz;
  if(!enif_get_tuple(env, argv[1], &c_sz, &c_t)) Badarg("c");
  int cR;
  if(!enif_get_int(env, c_t[0], &cR)) Badarg("c");
  int cC;
  if(!enif_get_int(env, c_t[1], &cC)) Badarg("c");
  wxGridCellCoords c = wxGridCellCoords(cR,cC);
  if(!This) throw wxe_badarg("This");
  wxGridCellEditor * Result = (wxGridCellEditor*)This->GetDefaultEditorForCell(c);
  wxeReturn rt = wxeReturn(memenv, Ecmd.caller, true);
  rt.send(  rt.make_ref(app->getRef((void *)Result,memenv), "wxGridCellEditor"));

}

// wxGrid::GetDefaultEditorForType
void wxGrid_GetDefaultEditorForType(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd)
{
  ErlNifEnv *env = Ecmd.env;
  ERL_NIF_TERM * argv = Ecmd.args;
  wxGrid *This;
  This = (wxGrid *) memenv->getPtr(env, argv[0], "This");
  ErlNifBinary typeName_bin;
  wxString typeName;
  if(!enif_inspect_binary(env, argv[1], &typeName_bin)) Badarg("typeName");
  typeName = wxString(typeName_bin.data, wxConvUTF8, typeName_bin.size);
  if(!This) throw wxe_badarg("This");
  wxGridCellEditor * Result = (wxGridCellEditor*)This->GetDefaultEditorForType(typeName);
  wxeReturn rt = wxeReturn(memenv, Ecmd.caller, true);
  rt.send(  rt.make_ref(app->getRef((void *)Result,memenv), "wxGridCellEditor"));

}

// wxGrid::GetDefaultRenderer
void wxGrid_GetDefaultRenderer(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd)
{
  ErlNifEnv *env = Ecmd.env;
  ERL_NIF_TERM * argv = Ecmd.args;
  wxGrid *This;
  This = (wxGrid *) memenv->getPtr(env, argv[0], "This");
  if(!This) throw wxe_badarg("This");
  wxGridCellRenderer * Result = (wxGridCellRenderer*)This->GetDefaultRenderer();
  wxeReturn rt = wxeReturn(memenv, Ecmd.caller, true);
  rt.send(  rt.make_ref(app->getRef((void *)Result,memenv), "wxGridCellRenderer"));

}

// wxGrid::GetDefaultRendererForCell
void wxGrid_GetDefaultRendererForCell(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd)
{
  ErlNifEnv *env = Ecmd.env;
  ERL_NIF_TERM * argv = Ecmd.args;
  wxGrid *This;
  This = (wxGrid *) memenv->getPtr(env, argv[0], "This");
  int row;
  if(!enif_get_int(env, argv[1], &row)) Badarg("row"); // int
  int col;
  if(!enif_get_int(env, argv[2], &col)) Badarg("col"); // int
  if(!This) throw wxe_badarg("This");
  wxGridCellRenderer * Result = (wxGridCellRenderer*)This->GetDefaultRendererForCell(row,col);
  wxeReturn rt = wxeReturn(memenv, Ecmd.caller, true);
  rt.send(  rt.make_ref(app->getRef((void *)Result,memenv), "wxGridCellRenderer"));

}

// wxGrid::GetDefaultRendererForType
void wxGrid_GetDefaultRendererForType(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd)
{
  ErlNifEnv *env = Ecmd.env;
  ERL_NIF_TERM * argv = Ecmd.args;
  wxGrid *This;
  This = (wxGrid *) memenv->getPtr(env, argv[0], "This");
  ErlNifBinary typeName_bin;
  wxString typeName;
  if(!enif_inspect_binary(env, argv[1], &typeName_bin)) Badarg("typeName");
  typeName = wxString(typeName_bin.data, wxConvUTF8, typeName_bin.size);
  if(!This) throw wxe_badarg("This");
  wxGridCellRenderer * Result = (wxGridCellRenderer*)This->GetDefaultRendererForType(typeName);
  wxeReturn rt = wxeReturn(memenv, Ecmd.caller, true);
  rt.send(  rt.make_ref(app->getRef((void *)Result,memenv), "wxGridCellRenderer"));

}

// wxGrid::GetDefaultRowLabelSize
void wxGrid_GetDefaultRowLabelSize(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd)
{
  ErlNifEnv *env = Ecmd.env;
  ERL_NIF_TERM * argv = Ecmd.args;
  wxGrid *This;
  This = (wxGrid *) memenv->getPtr(env, argv[0], "This");
  if(!This) throw wxe_badarg("This");
  int Result = This->GetDefaultRowLabelSize();
  wxeReturn rt = wxeReturn(memenv, Ecmd.caller, true);
  rt.send(  rt.make_int(Result));

}

// wxGrid::GetDefaultRowSize
void wxGrid_GetDefaultRowSize(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd)
{
  ErlNifEnv *env = Ecmd.env;
  ERL_NIF_TERM * argv = Ecmd.args;
  wxGrid *This;
  This = (wxGrid *) memenv->getPtr(env, argv[0], "This");
  if(!This) throw wxe_badarg("This");
  int Result = This->GetDefaultRowSize();
  wxeReturn rt = wxeReturn(memenv, Ecmd.caller, true);
  rt.send(  rt.make_int(Result));

}

// wxGrid::GetGridCursorCol
void wxGrid_GetGridCursorCol(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd)
{
  ErlNifEnv *env = Ecmd.env;
  ERL_NIF_TERM * argv = Ecmd.args;
  wxGrid *This;
  This = (wxGrid *) memenv->getPtr(env, argv[0], "This");
  if(!This) throw wxe_badarg("This");
  int Result = This->GetGridCursorCol();
  wxeReturn rt = wxeReturn(memenv, Ecmd.caller, true);
  rt.send(  rt.make_int(Result));

}

// wxGrid::GetGridCursorRow
void wxGrid_GetGridCursorRow(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd)
{
  ErlNifEnv *env = Ecmd.env;
  ERL_NIF_TERM * argv = Ecmd.args;
  wxGrid *This;
  This = (wxGrid *) memenv->getPtr(env, argv[0], "This");
  if(!This) throw wxe_badarg("This");
  int Result = This->GetGridCursorRow();
  wxeReturn rt = wxeReturn(memenv, Ecmd.caller, true);
  rt.send(  rt.make_int(Result));

}

// wxGrid::GetGridLineColour
void wxGrid_GetGridLineColour(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd)
{
  ErlNifEnv *env = Ecmd.env;
  ERL_NIF_TERM * argv = Ecmd.args;
  wxGrid *This;
  This = (wxGrid *) memenv->getPtr(env, argv[0], "This");
  if(!This) throw wxe_badarg("This");
  wxColour Result = This->GetGridLineColour();
  wxeReturn rt = wxeReturn(memenv, Ecmd.caller, true);
  rt.send(  rt.make(Result));

}

// wxGrid::GridLinesEnabled
void wxGrid_GridLinesEnabled(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd)
{
  ErlNifEnv *env = Ecmd.env;
  ERL_NIF_TERM * argv = Ecmd.args;
  wxGrid *This;
  This = (wxGrid *) memenv->getPtr(env, argv[0], "This");
  if(!This) throw wxe_badarg("This");
  bool Result = This->GridLinesEnabled();
  wxeReturn rt = wxeReturn(memenv, Ecmd.caller, true);
  rt.send(  rt.make_bool(Result));

}

// wxGrid::GetLabelBackgroundColour
void wxGrid_GetLabelBackgroundColour(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd)
{
  ErlNifEnv *env = Ecmd.env;
  ERL_NIF_TERM * argv = Ecmd.args;
  wxGrid *This;
  This = (wxGrid *) memenv->getPtr(env, argv[0], "This");
  if(!This) throw wxe_badarg("This");
  wxColour Result = This->GetLabelBackgroundColour();
  wxeReturn rt = wxeReturn(memenv, Ecmd.caller, true);
  rt.send(  rt.make(Result));

}

// wxGrid::GetLabelFont
void wxGrid_GetLabelFont(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd)
{
  ErlNifEnv *env = Ecmd.env;
  ERL_NIF_TERM * argv = Ecmd.args;
  wxGrid *This;
  This = (wxGrid *) memenv->getPtr(env, argv[0], "This");
  if(!This) throw wxe_badarg("This");
  wxFont * Result = new wxFont(This->GetLabelFont()); app->newPtr((void *) Result,3, memenv);;
  wxeReturn rt = wxeReturn(memenv, Ecmd.caller, true);
  rt.send(  rt.make_ref(app->getRef((void *)Result,memenv), "wxFont"));

}

// wxGrid::GetLabelTextColour
void wxGrid_GetLabelTextColour(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd)
{
  ErlNifEnv *env = Ecmd.env;
  ERL_NIF_TERM * argv = Ecmd.args;
  wxGrid *This;
  This = (wxGrid *) memenv->getPtr(env, argv[0], "This");
  if(!This) throw wxe_badarg("This");
  wxColour Result = This->GetLabelTextColour();
  wxeReturn rt = wxeReturn(memenv, Ecmd.caller, true);
  rt.send(  rt.make(Result));

}

// wxGrid::GetNumberCols
void wxGrid_GetNumberCols(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd)
{
  ErlNifEnv *env = Ecmd.env;
  ERL_NIF_TERM * argv = Ecmd.args;
  wxGrid *This;
  This = (wxGrid *) memenv->getPtr(env, argv[0], "This");
  if(!This) throw wxe_badarg("This");
  int Result = This->GetNumberCols();
  wxeReturn rt = wxeReturn(memenv, Ecmd.caller, true);
  rt.send(  rt.make_int(Result));

}

// wxGrid::GetNumberRows
void wxGrid_GetNumberRows(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd)
{
  ErlNifEnv *env = Ecmd.env;
  ERL_NIF_TERM * argv = Ecmd.args;
  wxGrid *This;
  This = (wxGrid *) memenv->getPtr(env, argv[0], "This");
  if(!This) throw wxe_badarg("This");
  int Result = This->GetNumberRows();
  wxeReturn rt = wxeReturn(memenv, Ecmd.caller, true);
  rt.send(  rt.make_int(Result));

}

// wxGrid::GetOrCreateCellAttr
void wxGrid_GetOrCreateCellAttr(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd)
{
  ErlNifEnv *env = Ecmd.env;
  ERL_NIF_TERM * argv = Ecmd.args;
  wxGrid *This;
  This = (wxGrid *) memenv->getPtr(env, argv[0], "This");
  int row;
  if(!enif_get_int(env, argv[1], &row)) Badarg("row"); // int
  int col;
  if(!enif_get_int(env, argv[2], &col)) Badarg("col"); // int
  if(!This) throw wxe_badarg("This");
  wxGridCellAttr * Result = (wxGridCellAttr*)This->GetOrCreateCellAttr(row,col);
  wxeReturn rt = wxeReturn(memenv, Ecmd.caller, true);
  rt.send(  rt.make_ref(app->getRef((void *)Result,memenv), "wxGridCellAttr"));

}

// wxGrid::GetRowMinimalAcceptableHeight
void wxGrid_GetRowMinimalAcceptableHeight(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd)
{
  ErlNifEnv *env = Ecmd.env;
  ERL_NIF_TERM * argv = Ecmd.args;
  wxGrid *This;
  This = (wxGrid *) memenv->getPtr(env, argv[0], "This");
  if(!This) throw wxe_badarg("This");
  int Result = This->GetRowMinimalAcceptableHeight();
  wxeReturn rt = wxeReturn(memenv, Ecmd.caller, true);
  rt.send(  rt.make_int(Result));

}

// wxGrid::GetRowLabelAlignment
void wxGrid_GetRowLabelAlignment(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd)
{
  int horiz;
  int vert;
  ErlNifEnv *env = Ecmd.env;
  ERL_NIF_TERM * argv = Ecmd.args;
  wxGrid *This;
  This = (wxGrid *) memenv->getPtr(env, argv[0], "This");
  if(!This) throw wxe_badarg("This");
  This->GetRowLabelAlignment(&horiz,&vert);
  wxeReturn rt = wxeReturn(memenv, Ecmd.caller, true);
  ERL_NIF_TERM msg = enif_make_tuple2(rt.env,
  rt.make_int(horiz),
  rt.make_int(vert));
  rt.send(msg);

}

// wxGrid::GetRowLabelSize
void wxGrid_GetRowLabelSize(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd)
{
  ErlNifEnv *env = Ecmd.env;
  ERL_NIF_TERM * argv = Ecmd.args;
  wxGrid *This;
  This = (wxGrid *) memenv->getPtr(env, argv[0], "This");
  if(!This) throw wxe_badarg("This");
  int Result = This->GetRowLabelSize();
  wxeReturn rt = wxeReturn(memenv, Ecmd.caller, true);
  rt.send(  rt.make_int(Result));

}

// wxGrid::GetRowLabelValue
void wxGrid_GetRowLabelValue(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd)
{
  ErlNifEnv *env = Ecmd.env;
  ERL_NIF_TERM * argv = Ecmd.args;
  wxGrid *This;
  This = (wxGrid *) memenv->getPtr(env, argv[0], "This");
  int row;
  if(!enif_get_int(env, argv[1], &row)) Badarg("row"); // int
  if(!This) throw wxe_badarg("This");
  wxString Result = This->GetRowLabelValue(row);
  wxeReturn rt = wxeReturn(memenv, Ecmd.caller, true);
  rt.send(  rt.make(Result));

}

// wxGrid::GetRowSize
void wxGrid_GetRowSize(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd)
{
  ErlNifEnv *env = Ecmd.env;
  ERL_NIF_TERM * argv = Ecmd.args;
  wxGrid *This;
  This = (wxGrid *) memenv->getPtr(env, argv[0], "This");
  int row;
  if(!enif_get_int(env, argv[1], &row)) Badarg("row"); // int
  if(!This) throw wxe_badarg("This");
  int Result = This->GetRowSize(row);
  wxeReturn rt = wxeReturn(memenv, Ecmd.caller, true);
  rt.send(  rt.make_int(Result));

}

// wxGrid::GetScrollLineX
void wxGrid_GetScrollLineX(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd)
{
  ErlNifEnv *env = Ecmd.env;
  ERL_NIF_TERM * argv = Ecmd.args;
  wxGrid *This;
  This = (wxGrid *) memenv->getPtr(env, argv[0], "This");
  if(!This) throw wxe_badarg("This");
  int Result = This->GetScrollLineX();
  wxeReturn rt = wxeReturn(memenv, Ecmd.caller, true);
  rt.send(  rt.make_int(Result));

}

// wxGrid::GetScrollLineY
void wxGrid_GetScrollLineY(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd)
{
  ErlNifEnv *env = Ecmd.env;
  ERL_NIF_TERM * argv = Ecmd.args;
  wxGrid *This;
  This = (wxGrid *) memenv->getPtr(env, argv[0], "This");
  if(!This) throw wxe_badarg("This");
  int Result = This->GetScrollLineY();
  wxeReturn rt = wxeReturn(memenv, Ecmd.caller, true);
  rt.send(  rt.make_int(Result));

}

// wxGrid::GetSelectedCells
void wxGrid_GetSelectedCells(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd)
{
  ErlNifEnv *env = Ecmd.env;
  ERL_NIF_TERM * argv = Ecmd.args;
  wxGrid *This;
  This = (wxGrid *) memenv->getPtr(env, argv[0], "This");
  if(!This) throw wxe_badarg("This");
  wxGridCellCoordsArray Result = This->GetSelectedCells();
  wxeReturn rt = wxeReturn(memenv, Ecmd.caller, true);
  rt.send(  rt.make_array_objs(Result));

}

// wxGrid::GetSelectedCols
void wxGrid_GetSelectedCols(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd)
{
  ErlNifEnv *env = Ecmd.env;
  ERL_NIF_TERM * argv = Ecmd.args;
  wxGrid *This;
  This = (wxGrid *) memenv->getPtr(env, argv[0], "This");
  if(!This) throw wxe_badarg("This");
  wxArrayInt Result = This->GetSelectedCols();
  wxeReturn rt = wxeReturn(memenv, Ecmd.caller, true);
  rt.send(  rt.make(Result));

}

// wxGrid::GetSelectedRows
void wxGrid_GetSelectedRows(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd)
{
  ErlNifEnv *env = Ecmd.env;
  ERL_NIF_TERM * argv = Ecmd.args;
  wxGrid *This;
  This = (wxGrid *) memenv->getPtr(env, argv[0], "This");
  if(!This) throw wxe_badarg("This");
  wxArrayInt Result = This->GetSelectedRows();
  wxeReturn rt = wxeReturn(memenv, Ecmd.caller, true);
  rt.send(  rt.make(Result));

}

// wxGrid::GetSelectionBackground
void wxGrid_GetSelectionBackground(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd)
{
  ErlNifEnv *env = Ecmd.env;
  ERL_NIF_TERM * argv = Ecmd.args;
  wxGrid *This;
  This = (wxGrid *) memenv->getPtr(env, argv[0], "This");
  if(!This) throw wxe_badarg("This");
  wxColour Result = This->GetSelectionBackground();
  wxeReturn rt = wxeReturn(memenv, Ecmd.caller, true);
  rt.send(  rt.make(Result));

}

// wxGrid::GetSelectionBlockTopLeft
void wxGrid_GetSelectionBlockTopLeft(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd)
{
  ErlNifEnv *env = Ecmd.env;
  ERL_NIF_TERM * argv = Ecmd.args;
  wxGrid *This;
  This = (wxGrid *) memenv->getPtr(env, argv[0], "This");
  if(!This) throw wxe_badarg("This");
  wxGridCellCoordsArray Result = This->GetSelectionBlockTopLeft();
  wxeReturn rt = wxeReturn(memenv, Ecmd.caller, true);
  rt.send(  rt.make_array_objs(Result));

}

// wxGrid::GetSelectionBlockBottomRight
void wxGrid_GetSelectionBlockBottomRight(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd)
{
  ErlNifEnv *env = Ecmd.env;
  ERL_NIF_TERM * argv = Ecmd.args;
  wxGrid *This;
  This = (wxGrid *) memenv->getPtr(env, argv[0], "This");
  if(!This) throw wxe_badarg("This");
  wxGridCellCoordsArray Result = This->GetSelectionBlockBottomRight();
  wxeReturn rt = wxeReturn(memenv, Ecmd.caller, true);
  rt.send(  rt.make_array_objs(Result));

}

// wxGrid::GetSelectionForeground
void wxGrid_GetSelectionForeground(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd)
{
  ErlNifEnv *env = Ecmd.env;
  ERL_NIF_TERM * argv = Ecmd.args;
  wxGrid *This;
  This = (wxGrid *) memenv->getPtr(env, argv[0], "This");
  if(!This) throw wxe_badarg("This");
  wxColour Result = This->GetSelectionForeground();
  wxeReturn rt = wxeReturn(memenv, Ecmd.caller, true);
  rt.send(  rt.make(Result));

}

// wxGrid::GetGridWindow
void wxGrid_GetGridWindow(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd)
{
  ErlNifEnv *env = Ecmd.env;
  ERL_NIF_TERM * argv = Ecmd.args;
  wxGrid *This;
  This = (wxGrid *) memenv->getPtr(env, argv[0], "This");
  if(!This) throw wxe_badarg("This");
  wxWindow * Result = (wxWindow*)This->GetGridWindow();
  wxeReturn rt = wxeReturn(memenv, Ecmd.caller, true);
  rt.send(  rt.make_ref(app->getRef((void *)Result,memenv), "wxWindow"));

}

// wxGrid::GetGridRowLabelWindow
void wxGrid_GetGridRowLabelWindow(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd)
{
  ErlNifEnv *env = Ecmd.env;
  ERL_NIF_TERM * argv = Ecmd.args;
  wxGrid *This;
  This = (wxGrid *) memenv->getPtr(env, argv[0], "This");
  if(!This) throw wxe_badarg("This");
  wxWindow * Result = (wxWindow*)This->GetGridRowLabelWindow();
  wxeReturn rt = wxeReturn(memenv, Ecmd.caller, true);
  rt.send(  rt.make_ref(app->getRef((void *)Result,memenv), "wxWindow"));

}

// wxGrid::GetGridColLabelWindow
void wxGrid_GetGridColLabelWindow(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd)
{
  ErlNifEnv *env = Ecmd.env;
  ERL_NIF_TERM * argv = Ecmd.args;
  wxGrid *This;
  This = (wxGrid *) memenv->getPtr(env, argv[0], "This");
  if(!This) throw wxe_badarg("This");
  wxWindow * Result = (wxWindow*)This->GetGridColLabelWindow();
  wxeReturn rt = wxeReturn(memenv, Ecmd.caller, true);
  rt.send(  rt.make_ref(app->getRef((void *)Result,memenv), "wxWindow"));

}

// wxGrid::GetGridCornerLabelWindow
void wxGrid_GetGridCornerLabelWindow(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd)
{
  ErlNifEnv *env = Ecmd.env;
  ERL_NIF_TERM * argv = Ecmd.args;
  wxGrid *This;
  This = (wxGrid *) memenv->getPtr(env, argv[0], "This");
  if(!This) throw wxe_badarg("This");
  wxWindow * Result = (wxWindow*)This->GetGridCornerLabelWindow();
  wxeReturn rt = wxeReturn(memenv, Ecmd.caller, true);
  rt.send(  rt.make_ref(app->getRef((void *)Result,memenv), "wxWindow"));

}

// wxGrid::HideCellEditControl
void wxGrid_HideCellEditControl(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd)
{
  ErlNifEnv *env = Ecmd.env;
  ERL_NIF_TERM * argv = Ecmd.args;
  wxGrid *This;
  This = (wxGrid *) memenv->getPtr(env, argv[0], "This");
  if(!This) throw wxe_badarg("This");
  This->HideCellEditControl();

}

// wxGrid::InsertCols
void wxGrid_InsertCols(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd)
{
  int pos=0;
  int numCols=1;
  bool updateLabels=true;
  ErlNifEnv *env = Ecmd.env;
  ERL_NIF_TERM * argv = Ecmd.args;
  wxGrid *This;
  This = (wxGrid *) memenv->getPtr(env, argv[0], "This");
  ERL_NIF_TERM lstHead, lstTail;
  lstTail = argv[1];
  if(!enif_is_list(env, lstTail)) Badarg("Options");
  const ERL_NIF_TERM *tpl;
  int tpl_sz;
  while(!enif_is_empty_list(env, lstTail)) {
    if(!enif_get_list_cell(env, lstTail, &lstHead, &lstTail)) Badarg("Options");
    if(!enif_get_tuple(env, lstHead, &tpl_sz, &tpl) || tpl_sz != 2) Badarg("Options");
    if(enif_is_identical(tpl[0], enif_make_atom(env, "pos"))) {
  if(!enif_get_int(env, tpl[1], &pos)) Badarg("pos"); // int
    } else     if(enif_is_identical(tpl[0], enif_make_atom(env, "numCols"))) {
  if(!enif_get_int(env, tpl[1], &numCols)) Badarg("numCols"); // int
    } else     if(enif_is_identical(tpl[0], enif_make_atom(env, "updateLabels"))) {
  updateLabels = enif_is_identical(tpl[1], WXE_ATOM_true);
    } else        Badarg("Options");
  };
  if(!This) throw wxe_badarg("This");
  bool Result = This->InsertCols(pos,numCols,updateLabels);
  wxeReturn rt = wxeReturn(memenv, Ecmd.caller, true);
  rt.send(  rt.make_bool(Result));

}

// wxGrid::InsertRows
void wxGrid_InsertRows(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd)
{
  int pos=0;
  int numRows=1;
  bool updateLabels=true;
  ErlNifEnv *env = Ecmd.env;
  ERL_NIF_TERM * argv = Ecmd.args;
  wxGrid *This;
  This = (wxGrid *) memenv->getPtr(env, argv[0], "This");
  ERL_NIF_TERM lstHead, lstTail;
  lstTail = argv[1];
  if(!enif_is_list(env, lstTail)) Badarg("Options");
  const ERL_NIF_TERM *tpl;
  int tpl_sz;
  while(!enif_is_empty_list(env, lstTail)) {
    if(!enif_get_list_cell(env, lstTail, &lstHead, &lstTail)) Badarg("Options");
    if(!enif_get_tuple(env, lstHead, &tpl_sz, &tpl) || tpl_sz != 2) Badarg("Options");
    if(enif_is_identical(tpl[0], enif_make_atom(env, "pos"))) {
  if(!enif_get_int(env, tpl[1], &pos)) Badarg("pos"); // int
    } else     if(enif_is_identical(tpl[0], enif_make_atom(env, "numRows"))) {
  if(!enif_get_int(env, tpl[1], &numRows)) Badarg("numRows"); // int
    } else     if(enif_is_identical(tpl[0], enif_make_atom(env, "updateLabels"))) {
  updateLabels = enif_is_identical(tpl[1], WXE_ATOM_true);
    } else        Badarg("Options");
  };
  if(!This) throw wxe_badarg("This");
  bool Result = This->InsertRows(pos,numRows,updateLabels);
  wxeReturn rt = wxeReturn(memenv, Ecmd.caller, true);
  rt.send(  rt.make_bool(Result));

}

// wxGrid::IsCellEditControlEnabled
void wxGrid_IsCellEditControlEnabled(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd)
{
  ErlNifEnv *env = Ecmd.env;
  ERL_NIF_TERM * argv = Ecmd.args;
  wxGrid *This;
  This = (wxGrid *) memenv->getPtr(env, argv[0], "This");
  if(!This) throw wxe_badarg("This");
  bool Result = This->IsCellEditControlEnabled();
  wxeReturn rt = wxeReturn(memenv, Ecmd.caller, true);
  rt.send(  rt.make_bool(Result));

}

// wxGrid::IsCurrentCellReadOnly
void wxGrid_IsCurrentCellReadOnly(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd)
{
  ErlNifEnv *env = Ecmd.env;
  ERL_NIF_TERM * argv = Ecmd.args;
  wxGrid *This;
  This = (wxGrid *) memenv->getPtr(env, argv[0], "This");
  if(!This) throw wxe_badarg("This");
  bool Result = This->IsCurrentCellReadOnly();
  wxeReturn rt = wxeReturn(memenv, Ecmd.caller, true);
  rt.send(  rt.make_bool(Result));

}

// wxGrid::IsEditable
void wxGrid_IsEditable(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd)
{
  ErlNifEnv *env = Ecmd.env;
  ERL_NIF_TERM * argv = Ecmd.args;
  wxGrid *This;
  This = (wxGrid *) memenv->getPtr(env, argv[0], "This");
  if(!This) throw wxe_badarg("This");
  bool Result = This->IsEditable();
  wxeReturn rt = wxeReturn(memenv, Ecmd.caller, true);
  rt.send(  rt.make_bool(Result));

}

// wxGrid::IsInSelection
void wxGrid_IsInSelection_2(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd)
{
  ErlNifEnv *env = Ecmd.env;
  ERL_NIF_TERM * argv = Ecmd.args;
  wxGrid *This;
  This = (wxGrid *) memenv->getPtr(env, argv[0], "This");
  int row;
  if(!enif_get_int(env, argv[1], &row)) Badarg("row"); // int
  int col;
  if(!enif_get_int(env, argv[2], &col)) Badarg("col"); // int
  if(!This) throw wxe_badarg("This");
  bool Result = This->IsInSelection(row,col);
  wxeReturn rt = wxeReturn(memenv, Ecmd.caller, true);
  rt.send(  rt.make_bool(Result));

}

// wxGrid::IsInSelection
void wxGrid_IsInSelection_1(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd)
{
  ErlNifEnv *env = Ecmd.env;
  ERL_NIF_TERM * argv = Ecmd.args;
  wxGrid *This;
  This = (wxGrid *) memenv->getPtr(env, argv[0], "This");
  const ERL_NIF_TERM *coords_t;
  int coords_sz;
  if(!enif_get_tuple(env, argv[1], &coords_sz, &coords_t)) Badarg("coords");
  int coordsR;
  if(!enif_get_int(env, coords_t[0], &coordsR)) Badarg("coords");
  int coordsC;
  if(!enif_get_int(env, coords_t[1], &coordsC)) Badarg("coords");
  wxGridCellCoords coords = wxGridCellCoords(coordsR,coordsC);
  if(!This) throw wxe_badarg("This");
  bool Result = This->IsInSelection(coords);
  wxeReturn rt = wxeReturn(memenv, Ecmd.caller, true);
  rt.send(  rt.make_bool(Result));

}

// wxGrid::IsReadOnly
void wxGrid_IsReadOnly(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd)
{
  ErlNifEnv *env = Ecmd.env;
  ERL_NIF_TERM * argv = Ecmd.args;
  wxGrid *This;
  This = (wxGrid *) memenv->getPtr(env, argv[0], "This");
  int row;
  if(!enif_get_int(env, argv[1], &row)) Badarg("row"); // int
  int col;
  if(!enif_get_int(env, argv[2], &col)) Badarg("col"); // int
  if(!This) throw wxe_badarg("This");
  bool Result = This->IsReadOnly(row,col);
  wxeReturn rt = wxeReturn(memenv, Ecmd.caller, true);
  rt.send(  rt.make_bool(Result));

}

// wxGrid::IsSelection
void wxGrid_IsSelection(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd)
{
  ErlNifEnv *env = Ecmd.env;
  ERL_NIF_TERM * argv = Ecmd.args;
  wxGrid *This;
  This = (wxGrid *) memenv->getPtr(env, argv[0], "This");
  if(!This) throw wxe_badarg("This");
  bool Result = This->IsSelection();
  wxeReturn rt = wxeReturn(memenv, Ecmd.caller, true);
  rt.send(  rt.make_bool(Result));

}

// wxGrid::IsVisible
void wxGrid_IsVisible_3(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd)
{
  bool wholeCellVisible=true;
  ErlNifEnv *env = Ecmd.env;
  ERL_NIF_TERM * argv = Ecmd.args;
  wxGrid *This;
  This = (wxGrid *) memenv->getPtr(env, argv[0], "This");
  int row;
  if(!enif_get_int(env, argv[1], &row)) Badarg("row"); // int
  int col;
  if(!enif_get_int(env, argv[2], &col)) Badarg("col"); // int
  ERL_NIF_TERM lstHead, lstTail;
  lstTail = argv[3];
  if(!enif_is_list(env, lstTail)) Badarg("Options");
  const ERL_NIF_TERM *tpl;
  int tpl_sz;
  while(!enif_is_empty_list(env, lstTail)) {
    if(!enif_get_list_cell(env, lstTail, &lstHead, &lstTail)) Badarg("Options");
    if(!enif_get_tuple(env, lstHead, &tpl_sz, &tpl) || tpl_sz != 2) Badarg("Options");
    if(enif_is_identical(tpl[0], enif_make_atom(env, "wholeCellVisible"))) {
  wholeCellVisible = enif_is_identical(tpl[1], WXE_ATOM_true);
    } else        Badarg("Options");
  };
  if(!This) throw wxe_badarg("This");
  bool Result = This->IsVisible(row,col,wholeCellVisible);
  wxeReturn rt = wxeReturn(memenv, Ecmd.caller, true);
  rt.send(  rt.make_bool(Result));

}

// wxGrid::IsVisible
void wxGrid_IsVisible_2(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd)
{
  bool wholeCellVisible=true;
  ErlNifEnv *env = Ecmd.env;
  ERL_NIF_TERM * argv = Ecmd.args;
  wxGrid *This;
  This = (wxGrid *) memenv->getPtr(env, argv[0], "This");
  const ERL_NIF_TERM *coords_t;
  int coords_sz;
  if(!enif_get_tuple(env, argv[1], &coords_sz, &coords_t)) Badarg("coords");
  int coordsR;
  if(!enif_get_int(env, coords_t[0], &coordsR)) Badarg("coords");
  int coordsC;
  if(!enif_get_int(env, coords_t[1], &coordsC)) Badarg("coords");
  wxGridCellCoords coords = wxGridCellCoords(coordsR,coordsC);
  ERL_NIF_TERM lstHead, lstTail;
  lstTail = argv[2];
  if(!enif_is_list(env, lstTail)) Badarg("Options");
  const ERL_NIF_TERM *tpl;
  int tpl_sz;
  while(!enif_is_empty_list(env, lstTail)) {
    if(!enif_get_list_cell(env, lstTail, &lstHead, &lstTail)) Badarg("Options");
    if(!enif_get_tuple(env, lstHead, &tpl_sz, &tpl) || tpl_sz != 2) Badarg("Options");
    if(enif_is_identical(tpl[0], enif_make_atom(env, "wholeCellVisible"))) {
  wholeCellVisible = enif_is_identical(tpl[1], WXE_ATOM_true);
    } else        Badarg("Options");
  };
  if(!This) throw wxe_badarg("This");
  bool Result = This->IsVisible(coords,wholeCellVisible);
  wxeReturn rt = wxeReturn(memenv, Ecmd.caller, true);
  rt.send(  rt.make_bool(Result));

}

// wxGrid::MakeCellVisible
void wxGrid_MakeCellVisible_2(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd)
{
  ErlNifEnv *env = Ecmd.env;
  ERL_NIF_TERM * argv = Ecmd.args;
  wxGrid *This;
  This = (wxGrid *) memenv->getPtr(env, argv[0], "This");
  int row;
  if(!enif_get_int(env, argv[1], &row)) Badarg("row"); // int
  int col;
  if(!enif_get_int(env, argv[2], &col)) Badarg("col"); // int
  if(!This) throw wxe_badarg("This");
  This->MakeCellVisible(row,col);

}

// wxGrid::MakeCellVisible
void wxGrid_MakeCellVisible_1(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd)
{
  ErlNifEnv *env = Ecmd.env;
  ERL_NIF_TERM * argv = Ecmd.args;
  wxGrid *This;
  This = (wxGrid *) memenv->getPtr(env, argv[0], "This");
  const ERL_NIF_TERM *coords_t;
  int coords_sz;
  if(!enif_get_tuple(env, argv[1], &coords_sz, &coords_t)) Badarg("coords");
  int coordsR;
  if(!enif_get_int(env, coords_t[0], &coordsR)) Badarg("coords");
  int coordsC;
  if(!enif_get_int(env, coords_t[1], &coordsC)) Badarg("coords");
  wxGridCellCoords coords = wxGridCellCoords(coordsR,coordsC);
  if(!This) throw wxe_badarg("This");
  This->MakeCellVisible(coords);

}

// wxGrid::MoveCursorDown
void wxGrid_MoveCursorDown(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd)
{
  ErlNifEnv *env = Ecmd.env;
  ERL_NIF_TERM * argv = Ecmd.args;
  wxGrid *This;
  This = (wxGrid *) memenv->getPtr(env, argv[0], "This");
  bool expandSelection;
  expandSelection = enif_is_identical(argv[1], WXE_ATOM_true);
  if(!This) throw wxe_badarg("This");
  bool Result = This->MoveCursorDown(expandSelection);
  wxeReturn rt = wxeReturn(memenv, Ecmd.caller, true);
  rt.send(  rt.make_bool(Result));

}

// wxGrid::MoveCursorLeft
void wxGrid_MoveCursorLeft(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd)
{
  ErlNifEnv *env = Ecmd.env;
  ERL_NIF_TERM * argv = Ecmd.args;
  wxGrid *This;
  This = (wxGrid *) memenv->getPtr(env, argv[0], "This");
  bool expandSelection;
  expandSelection = enif_is_identical(argv[1], WXE_ATOM_true);
  if(!This) throw wxe_badarg("This");
  bool Result = This->MoveCursorLeft(expandSelection);
  wxeReturn rt = wxeReturn(memenv, Ecmd.caller, true);
  rt.send(  rt.make_bool(Result));

}

// wxGrid::MoveCursorRight
void wxGrid_MoveCursorRight(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd)
{
  ErlNifEnv *env = Ecmd.env;
  ERL_NIF_TERM * argv = Ecmd.args;
  wxGrid *This;
  This = (wxGrid *) memenv->getPtr(env, argv[0], "This");
  bool expandSelection;
  expandSelection = enif_is_identical(argv[1], WXE_ATOM_true);
  if(!This) throw wxe_badarg("This");
  bool Result = This->MoveCursorRight(expandSelection);
  wxeReturn rt = wxeReturn(memenv, Ecmd.caller, true);
  rt.send(  rt.make_bool(Result));

}

// wxGrid::MoveCursorUp
void wxGrid_MoveCursorUp(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd)
{
  ErlNifEnv *env = Ecmd.env;
  ERL_NIF_TERM * argv = Ecmd.args;
  wxGrid *This;
  This = (wxGrid *) memenv->getPtr(env, argv[0], "This");
  bool expandSelection;
  expandSelection = enif_is_identical(argv[1], WXE_ATOM_true);
  if(!This) throw wxe_badarg("This");
  bool Result = This->MoveCursorUp(expandSelection);
  wxeReturn rt = wxeReturn(memenv, Ecmd.caller, true);
  rt.send(  rt.make_bool(Result));

}

// wxGrid::MoveCursorDownBlock
void wxGrid_MoveCursorDownBlock(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd)
{
  ErlNifEnv *env = Ecmd.env;
  ERL_NIF_TERM * argv = Ecmd.args;
  wxGrid *This;
  This = (wxGrid *) memenv->getPtr(env, argv[0], "This");
  bool expandSelection;
  expandSelection = enif_is_identical(argv[1], WXE_ATOM_true);
  if(!This) throw wxe_badarg("This");
  bool Result = This->MoveCursorDownBlock(expandSelection);
  wxeReturn rt = wxeReturn(memenv, Ecmd.caller, true);
  rt.send(  rt.make_bool(Result));

}

// wxGrid::MoveCursorLeftBlock
void wxGrid_MoveCursorLeftBlock(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd)
{
  ErlNifEnv *env = Ecmd.env;
  ERL_NIF_TERM * argv = Ecmd.args;
  wxGrid *This;
  This = (wxGrid *) memenv->getPtr(env, argv[0], "This");
  bool expandSelection;
  expandSelection = enif_is_identical(argv[1], WXE_ATOM_true);
  if(!This) throw wxe_badarg("This");
  bool Result = This->MoveCursorLeftBlock(expandSelection);
  wxeReturn rt = wxeReturn(memenv, Ecmd.caller, true);
  rt.send(  rt.make_bool(Result));

}

// wxGrid::MoveCursorRightBlock
void wxGrid_MoveCursorRightBlock(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd)
{
  ErlNifEnv *env = Ecmd.env;
  ERL_NIF_TERM * argv = Ecmd.args;
  wxGrid *This;
  This = (wxGrid *) memenv->getPtr(env, argv[0], "This");
  bool expandSelection;
  expandSelection = enif_is_identical(argv[1], WXE_ATOM_true);
  if(!This) throw wxe_badarg("This");
  bool Result = This->MoveCursorRightBlock(expandSelection);
  wxeReturn rt = wxeReturn(memenv, Ecmd.caller, true);
  rt.send(  rt.make_bool(Result));

}

// wxGrid::MoveCursorUpBlock
void wxGrid_MoveCursorUpBlock(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd)
{
  ErlNifEnv *env = Ecmd.env;
  ERL_NIF_TERM * argv = Ecmd.args;
  wxGrid *This;
  This = (wxGrid *) memenv->getPtr(env, argv[0], "This");
  bool expandSelection;
  expandSelection = enif_is_identical(argv[1], WXE_ATOM_true);
  if(!This) throw wxe_badarg("This");
  bool Result = This->MoveCursorUpBlock(expandSelection);
  wxeReturn rt = wxeReturn(memenv, Ecmd.caller, true);
  rt.send(  rt.make_bool(Result));

}

// wxGrid::MovePageDown
void wxGrid_MovePageDown(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd)
{
  ErlNifEnv *env = Ecmd.env;
  ERL_NIF_TERM * argv = Ecmd.args;
  wxGrid *This;
  This = (wxGrid *) memenv->getPtr(env, argv[0], "This");
  if(!This) throw wxe_badarg("This");
  bool Result = This->MovePageDown();
  wxeReturn rt = wxeReturn(memenv, Ecmd.caller, true);
  rt.send(  rt.make_bool(Result));

}

// wxGrid::MovePageUp
void wxGrid_MovePageUp(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd)
{
  ErlNifEnv *env = Ecmd.env;
  ERL_NIF_TERM * argv = Ecmd.args;
  wxGrid *This;
  This = (wxGrid *) memenv->getPtr(env, argv[0], "This");
  if(!This) throw wxe_badarg("This");
  bool Result = This->MovePageUp();
  wxeReturn rt = wxeReturn(memenv, Ecmd.caller, true);
  rt.send(  rt.make_bool(Result));

}

// wxGrid::RegisterDataType
void wxGrid_RegisterDataType(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd)
{
  ErlNifEnv *env = Ecmd.env;
  ERL_NIF_TERM * argv = Ecmd.args;
  wxGrid *This;
  This = (wxGrid *) memenv->getPtr(env, argv[0], "This");
  ErlNifBinary typeName_bin;
  wxString typeName;
  if(!enif_inspect_binary(env, argv[1], &typeName_bin)) Badarg("typeName");
  typeName = wxString(typeName_bin.data, wxConvUTF8, typeName_bin.size);
  wxGridCellRenderer *renderer;
  renderer = (wxGridCellRenderer *) memenv->getPtr(env, argv[2], "renderer");
  wxGridCellEditor *editor;
  editor = (wxGridCellEditor *) memenv->getPtr(env, argv[3], "editor");
  if(!This) throw wxe_badarg("This");
  This->RegisterDataType(typeName,renderer,editor);

}

// wxGrid::SaveEditControlValue
void wxGrid_SaveEditControlValue(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd)
{
  ErlNifEnv *env = Ecmd.env;
  ERL_NIF_TERM * argv = Ecmd.args;
  wxGrid *This;
  This = (wxGrid *) memenv->getPtr(env, argv[0], "This");
  if(!This) throw wxe_badarg("This");
  This->SaveEditControlValue();

}

// wxGrid::SelectAll
void wxGrid_SelectAll(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd)
{
  ErlNifEnv *env = Ecmd.env;
  ERL_NIF_TERM * argv = Ecmd.args;
  wxGrid *This;
  This = (wxGrid *) memenv->getPtr(env, argv[0], "This");
  if(!This) throw wxe_badarg("This");
  This->SelectAll();

}

// wxGrid::SelectBlock
void wxGrid_SelectBlock_5(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd)
{
  bool addToSelected=false;
  ErlNifEnv *env = Ecmd.env;
  ERL_NIF_TERM * argv = Ecmd.args;
  wxGrid *This;
  This = (wxGrid *) memenv->getPtr(env, argv[0], "This");
  int topRow;
  if(!enif_get_int(env, argv[1], &topRow)) Badarg("topRow"); // int
  int leftCol;
  if(!enif_get_int(env, argv[2], &leftCol)) Badarg("leftCol"); // int
  int bottomRow;
  if(!enif_get_int(env, argv[3], &bottomRow)) Badarg("bottomRow"); // int
  int rightCol;
  if(!enif_get_int(env, argv[4], &rightCol)) Badarg("rightCol"); // int
  ERL_NIF_TERM lstHead, lstTail;
  lstTail = argv[5];
  if(!enif_is_list(env, lstTail)) Badarg("Options");
  const ERL_NIF_TERM *tpl;
  int tpl_sz;
  while(!enif_is_empty_list(env, lstTail)) {
    if(!enif_get_list_cell(env, lstTail, &lstHead, &lstTail)) Badarg("Options");
    if(!enif_get_tuple(env, lstHead, &tpl_sz, &tpl) || tpl_sz != 2) Badarg("Options");
    if(enif_is_identical(tpl[0], enif_make_atom(env, "addToSelected"))) {
  addToSelected = enif_is_identical(tpl[1], WXE_ATOM_true);
    } else        Badarg("Options");
  };
  if(!This) throw wxe_badarg("This");
  This->SelectBlock(topRow,leftCol,bottomRow,rightCol,addToSelected);

}

// wxGrid::SelectBlock
void wxGrid_SelectBlock_3(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd)
{
  bool addToSelected=false;
  ErlNifEnv *env = Ecmd.env;
  ERL_NIF_TERM * argv = Ecmd.args;
  wxGrid *This;
  This = (wxGrid *) memenv->getPtr(env, argv[0], "This");
  const ERL_NIF_TERM *topLeft_t;
  int topLeft_sz;
  if(!enif_get_tuple(env, argv[1], &topLeft_sz, &topLeft_t)) Badarg("topLeft");
  int topLeftR;
  if(!enif_get_int(env, topLeft_t[0], &topLeftR)) Badarg("topLeft");
  int topLeftC;
  if(!enif_get_int(env, topLeft_t[1], &topLeftC)) Badarg("topLeft");
  wxGridCellCoords topLeft = wxGridCellCoords(topLeftR,topLeftC);
  const ERL_NIF_TERM *bottomRight_t;
  int bottomRight_sz;
  if(!enif_get_tuple(env, argv[2], &bottomRight_sz, &bottomRight_t)) Badarg("bottomRight");
  int bottomRightR;
  if(!enif_get_int(env, bottomRight_t[0], &bottomRightR)) Badarg("bottomRight");
  int bottomRightC;
  if(!enif_get_int(env, bottomRight_t[1], &bottomRightC)) Badarg("bottomRight");
  wxGridCellCoords bottomRight = wxGridCellCoords(bottomRightR,bottomRightC);
  ERL_NIF_TERM lstHead, lstTail;
  lstTail = argv[3];
  if(!enif_is_list(env, lstTail)) Badarg("Options");
  const ERL_NIF_TERM *tpl;
  int tpl_sz;
  while(!enif_is_empty_list(env, lstTail)) {
    if(!enif_get_list_cell(env, lstTail, &lstHead, &lstTail)) Badarg("Options");
    if(!enif_get_tuple(env, lstHead, &tpl_sz, &tpl) || tpl_sz != 2) Badarg("Options");
    if(enif_is_identical(tpl[0], enif_make_atom(env, "addToSelected"))) {
  addToSelected = enif_is_identical(tpl[1], WXE_ATOM_true);
    } else        Badarg("Options");
  };
  if(!This) throw wxe_badarg("This");
  This->SelectBlock(topLeft,bottomRight,addToSelected);

}

// wxGrid::SelectCol
void wxGrid_SelectCol(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd)
{
  bool addToSelected=false;
  ErlNifEnv *env = Ecmd.env;
  ERL_NIF_TERM * argv = Ecmd.args;
  wxGrid *This;
  This = (wxGrid *) memenv->getPtr(env, argv[0], "This");
  int col;
  if(!enif_get_int(env, argv[1], &col)) Badarg("col"); // int
  ERL_NIF_TERM lstHead, lstTail;
  lstTail = argv[2];
  if(!enif_is_list(env, lstTail)) Badarg("Options");
  const ERL_NIF_TERM *tpl;
  int tpl_sz;
  while(!enif_is_empty_list(env, lstTail)) {
    if(!enif_get_list_cell(env, lstTail, &lstHead, &lstTail)) Badarg("Options");
    if(!enif_get_tuple(env, lstHead, &tpl_sz, &tpl) || tpl_sz != 2) Badarg("Options");
    if(enif_is_identical(tpl[0], enif_make_atom(env, "addToSelected"))) {
  addToSelected = enif_is_identical(tpl[1], WXE_ATOM_true);
    } else        Badarg("Options");
  };
  if(!This) throw wxe_badarg("This");
  This->SelectCol(col,addToSelected);

}

// wxGrid::SelectRow
void wxGrid_SelectRow(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd)
{
  bool addToSelected=false;
  ErlNifEnv *env = Ecmd.env;
  ERL_NIF_TERM * argv = Ecmd.args;
  wxGrid *This;
  This = (wxGrid *) memenv->getPtr(env, argv[0], "This");
  int row;
  if(!enif_get_int(env, argv[1], &row)) Badarg("row"); // int
  ERL_NIF_TERM lstHead, lstTail;
  lstTail = argv[2];
  if(!enif_is_list(env, lstTail)) Badarg("Options");
  const ERL_NIF_TERM *tpl;
  int tpl_sz;
  while(!enif_is_empty_list(env, lstTail)) {
    if(!enif_get_list_cell(env, lstTail, &lstHead, &lstTail)) Badarg("Options");
    if(!enif_get_tuple(env, lstHead, &tpl_sz, &tpl) || tpl_sz != 2) Badarg("Options");
    if(enif_is_identical(tpl[0], enif_make_atom(env, "addToSelected"))) {
  addToSelected = enif_is_identical(tpl[1], WXE_ATOM_true);
    } else        Badarg("Options");
  };
  if(!This) throw wxe_badarg("This");
  This->SelectRow(row,addToSelected);

}

// wxGrid::SetCellAlignment
void wxGrid_SetCellAlignment(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd)
{
  ErlNifEnv *env = Ecmd.env;
  ERL_NIF_TERM * argv = Ecmd.args;
  wxGrid *This;
  This = (wxGrid *) memenv->getPtr(env, argv[0], "This");
  int row;
  if(!enif_get_int(env, argv[1], &row)) Badarg("row"); // int
  int col;
  if(!enif_get_int(env, argv[2], &col)) Badarg("col"); // int
  int horiz;
  if(!enif_get_int(env, argv[3], &horiz)) Badarg("horiz"); // int
  int vert;
  if(!enif_get_int(env, argv[4], &vert)) Badarg("vert"); // int
  if(!This) throw wxe_badarg("This");
  This->SetCellAlignment(row,col,horiz,vert);

}

// wxGrid::SetCellBackgroundColour
void wxGrid_SetCellBackgroundColour(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd)
{
  ErlNifEnv *env = Ecmd.env;
  ERL_NIF_TERM * argv = Ecmd.args;
  wxGrid *This;
  This = (wxGrid *) memenv->getPtr(env, argv[0], "This");
  int row;
  if(!enif_get_int(env, argv[1], &row)) Badarg("row"); // int
  int col;
  if(!enif_get_int(env, argv[2], &col)) Badarg("col"); // int
  const ERL_NIF_TERM *colour_t;
  int colour_sz;
  if(!enif_get_tuple(env, argv[3], &colour_sz, &colour_t)) Badarg("colour");
  int colourR;
  if(!enif_get_int(env, colour_t[0], &colourR)) Badarg("colour");
  int colourG;
  if(!enif_get_int(env, colour_t[1], &colourG)) Badarg("colour");
  int colourB;
  if(!enif_get_int(env, colour_t[2], &colourB)) Badarg("colour");
  int colourA;
  if(!enif_get_int(env, colour_t[3], &colourA)) Badarg("colour");
  wxColour colour = wxColour(colourR,colourG,colourB,colourA);
  if(!This) throw wxe_badarg("This");
  This->SetCellBackgroundColour(row,col,colour);

}

// wxGrid::SetCellEditor
void wxGrid_SetCellEditor(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd)
{
  ErlNifEnv *env = Ecmd.env;
  ERL_NIF_TERM * argv = Ecmd.args;
  wxGrid *This;
  This = (wxGrid *) memenv->getPtr(env, argv[0], "This");
  int row;
  if(!enif_get_int(env, argv[1], &row)) Badarg("row"); // int
  int col;
  if(!enif_get_int(env, argv[2], &col)) Badarg("col"); // int
  wxGridCellEditor *editor;
  editor = (wxGridCellEditor *) memenv->getPtr(env, argv[3], "editor");
  if(!This) throw wxe_badarg("This");
  This->SetCellEditor(row,col,editor);

}

// wxGrid::SetCellFont
void wxGrid_SetCellFont(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd)
{
  ErlNifEnv *env = Ecmd.env;
  ERL_NIF_TERM * argv = Ecmd.args;
  wxGrid *This;
  This = (wxGrid *) memenv->getPtr(env, argv[0], "This");
  int row;
  if(!enif_get_int(env, argv[1], &row)) Badarg("row"); // int
  int col;
  if(!enif_get_int(env, argv[2], &col)) Badarg("col"); // int
  wxFont *font;
  font = (wxFont *) memenv->getPtr(env, argv[3], "font");
  if(!This) throw wxe_badarg("This");
  This->SetCellFont(row,col,*font);

}

// wxGrid::SetCellRenderer
void wxGrid_SetCellRenderer(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd)
{
  ErlNifEnv *env = Ecmd.env;
  ERL_NIF_TERM * argv = Ecmd.args;
  wxGrid *This;
  This = (wxGrid *) memenv->getPtr(env, argv[0], "This");
  int row;
  if(!enif_get_int(env, argv[1], &row)) Badarg("row"); // int
  int col;
  if(!enif_get_int(env, argv[2], &col)) Badarg("col"); // int
  wxGridCellRenderer *renderer;
  renderer = (wxGridCellRenderer *) memenv->getPtr(env, argv[3], "renderer");
  if(!This) throw wxe_badarg("This");
  This->SetCellRenderer(row,col,renderer);

}

// wxGrid::SetCellTextColour
void wxGrid_SetCellTextColour(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd)
{
  ErlNifEnv *env = Ecmd.env;
  ERL_NIF_TERM * argv = Ecmd.args;
  wxGrid *This;
  This = (wxGrid *) memenv->getPtr(env, argv[0], "This");
  int row;
  if(!enif_get_int(env, argv[1], &row)) Badarg("row"); // int
  int col;
  if(!enif_get_int(env, argv[2], &col)) Badarg("col"); // int
  const ERL_NIF_TERM *colour_t;
  int colour_sz;
  if(!enif_get_tuple(env, argv[3], &colour_sz, &colour_t)) Badarg("colour");
  int colourR;
  if(!enif_get_int(env, colour_t[0], &colourR)) Badarg("colour");
  int colourG;
  if(!enif_get_int(env, colour_t[1], &colourG)) Badarg("colour");
  int colourB;
  if(!enif_get_int(env, colour_t[2], &colourB)) Badarg("colour");
  int colourA;
  if(!enif_get_int(env, colour_t[3], &colourA)) Badarg("colour");
  wxColour colour = wxColour(colourR,colourG,colourB,colourA);
  if(!This) throw wxe_badarg("This");
  This->SetCellTextColour(row,col,colour);

}

// wxGrid::SetCellValue
void wxGrid_SetCellValue_3(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd)
{
  ErlNifEnv *env = Ecmd.env;
  ERL_NIF_TERM * argv = Ecmd.args;
  wxGrid *This;
  This = (wxGrid *) memenv->getPtr(env, argv[0], "This");
  int row;
  if(!enif_get_int(env, argv[1], &row)) Badarg("row"); // int
  int col;
  if(!enif_get_int(env, argv[2], &col)) Badarg("col"); // int
  ErlNifBinary s_bin;
  wxString s;
  if(!enif_inspect_binary(env, argv[3], &s_bin)) Badarg("s");
  s = wxString(s_bin.data, wxConvUTF8, s_bin.size);
  if(!This) throw wxe_badarg("This");
  This->SetCellValue(row,col,s);

}

// wxGrid::SetCellValue
void wxGrid_SetCellValue_2(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd)
{
  ErlNifEnv *env = Ecmd.env;
  ERL_NIF_TERM * argv = Ecmd.args;
  wxGrid *This;
  This = (wxGrid *) memenv->getPtr(env, argv[0], "This");
  const ERL_NIF_TERM *coords_t;
  int coords_sz;
  if(!enif_get_tuple(env, argv[1], &coords_sz, &coords_t)) Badarg("coords");
  int coordsR;
  if(!enif_get_int(env, coords_t[0], &coordsR)) Badarg("coords");
  int coordsC;
  if(!enif_get_int(env, coords_t[1], &coordsC)) Badarg("coords");
  wxGridCellCoords coords = wxGridCellCoords(coordsR,coordsC);
  ErlNifBinary s_bin;
  wxString s;
  if(!enif_inspect_binary(env, argv[2], &s_bin)) Badarg("s");
  s = wxString(s_bin.data, wxConvUTF8, s_bin.size);
  if(!This) throw wxe_badarg("This");
  This->SetCellValue(coords,s);

}

// wxGrid::SetColAttr
void wxGrid_SetColAttr(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd)
{
  ErlNifEnv *env = Ecmd.env;
  ERL_NIF_TERM * argv = Ecmd.args;
  wxGrid *This;
  This = (wxGrid *) memenv->getPtr(env, argv[0], "This");
  int col;
  if(!enif_get_int(env, argv[1], &col)) Badarg("col"); // int
  wxGridCellAttr *attr;
  attr = (wxGridCellAttr *) memenv->getPtr(env, argv[2], "attr");
  if(!This) throw wxe_badarg("This");
  This->SetColAttr(col,attr);

}

// wxGrid::SetColFormatBool
void wxGrid_SetColFormatBool(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd)
{
  ErlNifEnv *env = Ecmd.env;
  ERL_NIF_TERM * argv = Ecmd.args;
  wxGrid *This;
  This = (wxGrid *) memenv->getPtr(env, argv[0], "This");
  int col;
  if(!enif_get_int(env, argv[1], &col)) Badarg("col"); // int
  if(!This) throw wxe_badarg("This");
  This->SetColFormatBool(col);

}

// wxGrid::SetColFormatNumber
void wxGrid_SetColFormatNumber(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd)
{
  ErlNifEnv *env = Ecmd.env;
  ERL_NIF_TERM * argv = Ecmd.args;
  wxGrid *This;
  This = (wxGrid *) memenv->getPtr(env, argv[0], "This");
  int col;
  if(!enif_get_int(env, argv[1], &col)) Badarg("col"); // int
  if(!This) throw wxe_badarg("This");
  This->SetColFormatNumber(col);

}

// wxGrid::SetColFormatFloat
void wxGrid_SetColFormatFloat(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd)
{
  int width=-1;
  int precision=-1;
  ErlNifEnv *env = Ecmd.env;
  ERL_NIF_TERM * argv = Ecmd.args;
  wxGrid *This;
  This = (wxGrid *) memenv->getPtr(env, argv[0], "This");
  int col;
  if(!enif_get_int(env, argv[1], &col)) Badarg("col"); // int
  ERL_NIF_TERM lstHead, lstTail;
  lstTail = argv[2];
  if(!enif_is_list(env, lstTail)) Badarg("Options");
  const ERL_NIF_TERM *tpl;
  int tpl_sz;
  while(!enif_is_empty_list(env, lstTail)) {
    if(!enif_get_list_cell(env, lstTail, &lstHead, &lstTail)) Badarg("Options");
    if(!enif_get_tuple(env, lstHead, &tpl_sz, &tpl) || tpl_sz != 2) Badarg("Options");
    if(enif_is_identical(tpl[0], enif_make_atom(env, "width"))) {
  if(!enif_get_int(env, tpl[1], &width)) Badarg("width"); // int
    } else     if(enif_is_identical(tpl[0], enif_make_atom(env, "precision"))) {
  if(!enif_get_int(env, tpl[1], &precision)) Badarg("precision"); // int
    } else        Badarg("Options");
  };
  if(!This) throw wxe_badarg("This");
  This->SetColFormatFloat(col,width,precision);

}

// wxGrid::SetColFormatCustom
void wxGrid_SetColFormatCustom(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd)
{
  ErlNifEnv *env = Ecmd.env;
  ERL_NIF_TERM * argv = Ecmd.args;
  wxGrid *This;
  This = (wxGrid *) memenv->getPtr(env, argv[0], "This");
  int col;
  if(!enif_get_int(env, argv[1], &col)) Badarg("col"); // int
  ErlNifBinary typeName_bin;
  wxString typeName;
  if(!enif_inspect_binary(env, argv[2], &typeName_bin)) Badarg("typeName");
  typeName = wxString(typeName_bin.data, wxConvUTF8, typeName_bin.size);
  if(!This) throw wxe_badarg("This");
  This->SetColFormatCustom(col,typeName);

}

// wxGrid::SetColLabelAlignment
void wxGrid_SetColLabelAlignment(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd)
{
  ErlNifEnv *env = Ecmd.env;
  ERL_NIF_TERM * argv = Ecmd.args;
  wxGrid *This;
  This = (wxGrid *) memenv->getPtr(env, argv[0], "This");
  int horiz;
  if(!enif_get_int(env, argv[1], &horiz)) Badarg("horiz"); // int
  int vert;
  if(!enif_get_int(env, argv[2], &vert)) Badarg("vert"); // int
  if(!This) throw wxe_badarg("This");
  This->SetColLabelAlignment(horiz,vert);

}

// wxGrid::SetColLabelSize
void wxGrid_SetColLabelSize(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd)
{
  ErlNifEnv *env = Ecmd.env;
  ERL_NIF_TERM * argv = Ecmd.args;
  wxGrid *This;
  This = (wxGrid *) memenv->getPtr(env, argv[0], "This");
  int height;
  if(!enif_get_int(env, argv[1], &height)) Badarg("height"); // int
  if(!This) throw wxe_badarg("This");
  This->SetColLabelSize(height);

}

// wxGrid::SetColLabelValue
void wxGrid_SetColLabelValue(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd)
{
  ErlNifEnv *env = Ecmd.env;
  ERL_NIF_TERM * argv = Ecmd.args;
  wxGrid *This;
  This = (wxGrid *) memenv->getPtr(env, argv[0], "This");
  int col;
  if(!enif_get_int(env, argv[1], &col)) Badarg("col"); // int
  ErlNifBinary value_bin;
  wxString value;
  if(!enif_inspect_binary(env, argv[2], &value_bin)) Badarg("value");
  value = wxString(value_bin.data, wxConvUTF8, value_bin.size);
  if(!This) throw wxe_badarg("This");
  This->SetColLabelValue(col,value);

}

// wxGrid::SetColMinimalWidth
void wxGrid_SetColMinimalWidth(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd)
{
  ErlNifEnv *env = Ecmd.env;
  ERL_NIF_TERM * argv = Ecmd.args;
  wxGrid *This;
  This = (wxGrid *) memenv->getPtr(env, argv[0], "This");
  int col;
  if(!enif_get_int(env, argv[1], &col)) Badarg("col"); // int
  int width;
  if(!enif_get_int(env, argv[2], &width)) Badarg("width"); // int
  if(!This) throw wxe_badarg("This");
  This->SetColMinimalWidth(col,width);

}

// wxGrid::SetColMinimalAcceptableWidth
void wxGrid_SetColMinimalAcceptableWidth(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd)
{
  ErlNifEnv *env = Ecmd.env;
  ERL_NIF_TERM * argv = Ecmd.args;
  wxGrid *This;
  This = (wxGrid *) memenv->getPtr(env, argv[0], "This");
  int width;
  if(!enif_get_int(env, argv[1], &width)) Badarg("width"); // int
  if(!This) throw wxe_badarg("This");
  This->SetColMinimalAcceptableWidth(width);

}

// wxGrid::SetColSize
void wxGrid_SetColSize(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd)
{
  ErlNifEnv *env = Ecmd.env;
  ERL_NIF_TERM * argv = Ecmd.args;
  wxGrid *This;
  This = (wxGrid *) memenv->getPtr(env, argv[0], "This");
  int col;
  if(!enif_get_int(env, argv[1], &col)) Badarg("col"); // int
  int width;
  if(!enif_get_int(env, argv[2], &width)) Badarg("width"); // int
  if(!This) throw wxe_badarg("This");
  This->SetColSize(col,width);

}

// wxGrid::SetDefaultCellAlignment
void wxGrid_SetDefaultCellAlignment(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd)
{
  ErlNifEnv *env = Ecmd.env;
  ERL_NIF_TERM * argv = Ecmd.args;
  wxGrid *This;
  This = (wxGrid *) memenv->getPtr(env, argv[0], "This");
  int horiz;
  if(!enif_get_int(env, argv[1], &horiz)) Badarg("horiz"); // int
  int vert;
  if(!enif_get_int(env, argv[2], &vert)) Badarg("vert"); // int
  if(!This) throw wxe_badarg("This");
  This->SetDefaultCellAlignment(horiz,vert);

}

// wxGrid::SetDefaultCellBackgroundColour
void wxGrid_SetDefaultCellBackgroundColour(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd)
{
  ErlNifEnv *env = Ecmd.env;
  ERL_NIF_TERM * argv = Ecmd.args;
  wxGrid *This;
  This = (wxGrid *) memenv->getPtr(env, argv[0], "This");
  const ERL_NIF_TERM *colour_t;
  int colour_sz;
  if(!enif_get_tuple(env, argv[1], &colour_sz, &colour_t)) Badarg("colour");
  int colourR;
  if(!enif_get_int(env, colour_t[0], &colourR)) Badarg("colour");
  int colourG;
  if(!enif_get_int(env, colour_t[1], &colourG)) Badarg("colour");
  int colourB;
  if(!enif_get_int(env, colour_t[2], &colourB)) Badarg("colour");
  int colourA;
  if(!enif_get_int(env, colour_t[3], &colourA)) Badarg("colour");
  wxColour colour = wxColour(colourR,colourG,colourB,colourA);
  if(!This) throw wxe_badarg("This");
  This->SetDefaultCellBackgroundColour(colour);

}

// wxGrid::SetDefaultCellFont
void wxGrid_SetDefaultCellFont(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd)
{
  ErlNifEnv *env = Ecmd.env;
  ERL_NIF_TERM * argv = Ecmd.args;
  wxGrid *This;
  This = (wxGrid *) memenv->getPtr(env, argv[0], "This");
  wxFont *font;
  font = (wxFont *) memenv->getPtr(env, argv[1], "font");
  if(!This) throw wxe_badarg("This");
  This->SetDefaultCellFont(*font);

}

// wxGrid::SetDefaultCellTextColour
void wxGrid_SetDefaultCellTextColour(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd)
{
  ErlNifEnv *env = Ecmd.env;
  ERL_NIF_TERM * argv = Ecmd.args;
  wxGrid *This;
  This = (wxGrid *) memenv->getPtr(env, argv[0], "This");
  const ERL_NIF_TERM *colour_t;
  int colour_sz;
  if(!enif_get_tuple(env, argv[1], &colour_sz, &colour_t)) Badarg("colour");
  int colourR;
  if(!enif_get_int(env, colour_t[0], &colourR)) Badarg("colour");
  int colourG;
  if(!enif_get_int(env, colour_t[1], &colourG)) Badarg("colour");
  int colourB;
  if(!enif_get_int(env, colour_t[2], &colourB)) Badarg("colour");
  int colourA;
  if(!enif_get_int(env, colour_t[3], &colourA)) Badarg("colour");
  wxColour colour = wxColour(colourR,colourG,colourB,colourA);
  if(!This) throw wxe_badarg("This");
  This->SetDefaultCellTextColour(colour);

}

// wxGrid::SetDefaultEditor
void wxGrid_SetDefaultEditor(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd)
{
  ErlNifEnv *env = Ecmd.env;
  ERL_NIF_TERM * argv = Ecmd.args;
  wxGrid *This;
  This = (wxGrid *) memenv->getPtr(env, argv[0], "This");
  wxGridCellEditor *editor;
  editor = (wxGridCellEditor *) memenv->getPtr(env, argv[1], "editor");
  if(!This) throw wxe_badarg("This");
  This->SetDefaultEditor(editor);

}

// wxGrid::SetDefaultRenderer
void wxGrid_SetDefaultRenderer(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd)
{
  ErlNifEnv *env = Ecmd.env;
  ERL_NIF_TERM * argv = Ecmd.args;
  wxGrid *This;
  This = (wxGrid *) memenv->getPtr(env, argv[0], "This");
  wxGridCellRenderer *renderer;
  renderer = (wxGridCellRenderer *) memenv->getPtr(env, argv[1], "renderer");
  if(!This) throw wxe_badarg("This");
  This->SetDefaultRenderer(renderer);

}

// wxGrid::SetDefaultColSize
void wxGrid_SetDefaultColSize(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd)
{
  bool resizeExistingCols=false;
  ErlNifEnv *env = Ecmd.env;
  ERL_NIF_TERM * argv = Ecmd.args;
  wxGrid *This;
  This = (wxGrid *) memenv->getPtr(env, argv[0], "This");
  int width;
  if(!enif_get_int(env, argv[1], &width)) Badarg("width"); // int
  ERL_NIF_TERM lstHead, lstTail;
  lstTail = argv[2];
  if(!enif_is_list(env, lstTail)) Badarg("Options");
  const ERL_NIF_TERM *tpl;
  int tpl_sz;
  while(!enif_is_empty_list(env, lstTail)) {
    if(!enif_get_list_cell(env, lstTail, &lstHead, &lstTail)) Badarg("Options");
    if(!enif_get_tuple(env, lstHead, &tpl_sz, &tpl) || tpl_sz != 2) Badarg("Options");
    if(enif_is_identical(tpl[0], enif_make_atom(env, "resizeExistingCols"))) {
  resizeExistingCols = enif_is_identical(tpl[1], WXE_ATOM_true);
    } else        Badarg("Options");
  };
  if(!This) throw wxe_badarg("This");
  This->SetDefaultColSize(width,resizeExistingCols);

}

// wxGrid::SetDefaultRowSize
void wxGrid_SetDefaultRowSize(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd)
{
  bool resizeExistingRows=false;
  ErlNifEnv *env = Ecmd.env;
  ERL_NIF_TERM * argv = Ecmd.args;
  wxGrid *This;
  This = (wxGrid *) memenv->getPtr(env, argv[0], "This");
  int height;
  if(!enif_get_int(env, argv[1], &height)) Badarg("height"); // int
  ERL_NIF_TERM lstHead, lstTail;
  lstTail = argv[2];
  if(!enif_is_list(env, lstTail)) Badarg("Options");
  const ERL_NIF_TERM *tpl;
  int tpl_sz;
  while(!enif_is_empty_list(env, lstTail)) {
    if(!enif_get_list_cell(env, lstTail, &lstHead, &lstTail)) Badarg("Options");
    if(!enif_get_tuple(env, lstHead, &tpl_sz, &tpl) || tpl_sz != 2) Badarg("Options");
    if(enif_is_identical(tpl[0], enif_make_atom(env, "resizeExistingRows"))) {
  resizeExistingRows = enif_is_identical(tpl[1], WXE_ATOM_true);
    } else        Badarg("Options");
  };
  if(!This) throw wxe_badarg("This");
  This->SetDefaultRowSize(height,resizeExistingRows);

}

// wxGrid::SetGridCursor
void wxGrid_SetGridCursor_2(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd)
{
  ErlNifEnv *env = Ecmd.env;
  ERL_NIF_TERM * argv = Ecmd.args;
  wxGrid *This;
  This = (wxGrid *) memenv->getPtr(env, argv[0], "This");
  int row;
  if(!enif_get_int(env, argv[1], &row)) Badarg("row"); // int
  int col;
  if(!enif_get_int(env, argv[2], &col)) Badarg("col"); // int
  if(!This) throw wxe_badarg("This");
  This->SetGridCursor(row,col);

}

// wxGrid::SetGridCursor
void wxGrid_SetGridCursor_1(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd)
{
  ErlNifEnv *env = Ecmd.env;
  ERL_NIF_TERM * argv = Ecmd.args;
  wxGrid *This;
  This = (wxGrid *) memenv->getPtr(env, argv[0], "This");
  const ERL_NIF_TERM *coords_t;
  int coords_sz;
  if(!enif_get_tuple(env, argv[1], &coords_sz, &coords_t)) Badarg("coords");
  int coordsR;
  if(!enif_get_int(env, coords_t[0], &coordsR)) Badarg("coords");
  int coordsC;
  if(!enif_get_int(env, coords_t[1], &coordsC)) Badarg("coords");
  wxGridCellCoords coords = wxGridCellCoords(coordsR,coordsC);
  if(!This) throw wxe_badarg("This");
  This->SetGridCursor(coords);

}

// wxGrid::SetGridLineColour
void wxGrid_SetGridLineColour(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd)
{
  ErlNifEnv *env = Ecmd.env;
  ERL_NIF_TERM * argv = Ecmd.args;
  wxGrid *This;
  This = (wxGrid *) memenv->getPtr(env, argv[0], "This");
  const ERL_NIF_TERM *colour_t;
  int colour_sz;
  if(!enif_get_tuple(env, argv[1], &colour_sz, &colour_t)) Badarg("colour");
  int colourR;
  if(!enif_get_int(env, colour_t[0], &colourR)) Badarg("colour");
  int colourG;
  if(!enif_get_int(env, colour_t[1], &colourG)) Badarg("colour");
  int colourB;
  if(!enif_get_int(env, colour_t[2], &colourB)) Badarg("colour");
  int colourA;
  if(!enif_get_int(env, colour_t[3], &colourA)) Badarg("colour");
  wxColour colour = wxColour(colourR,colourG,colourB,colourA);
  if(!This) throw wxe_badarg("This");
  This->SetGridLineColour(colour);

}

// wxGrid::SetLabelBackgroundColour
void wxGrid_SetLabelBackgroundColour(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd)
{
  ErlNifEnv *env = Ecmd.env;
  ERL_NIF_TERM * argv = Ecmd.args;
  wxGrid *This;
  This = (wxGrid *) memenv->getPtr(env, argv[0], "This");
  const ERL_NIF_TERM *colour_t;
  int colour_sz;
  if(!enif_get_tuple(env, argv[1], &colour_sz, &colour_t)) Badarg("colour");
  int colourR;
  if(!enif_get_int(env, colour_t[0], &colourR)) Badarg("colour");
  int colourG;
  if(!enif_get_int(env, colour_t[1], &colourG)) Badarg("colour");
  int colourB;
  if(!enif_get_int(env, colour_t[2], &colourB)) Badarg("colour");
  int colourA;
  if(!enif_get_int(env, colour_t[3], &colourA)) Badarg("colour");
  wxColour colour = wxColour(colourR,colourG,colourB,colourA);
  if(!This) throw wxe_badarg("This");
  This->SetLabelBackgroundColour(colour);

}

// wxGrid::SetLabelFont
void wxGrid_SetLabelFont(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd)
{
  ErlNifEnv *env = Ecmd.env;
  ERL_NIF_TERM * argv = Ecmd.args;
  wxGrid *This;
  This = (wxGrid *) memenv->getPtr(env, argv[0], "This");
  wxFont *font;
  font = (wxFont *) memenv->getPtr(env, argv[1], "font");
  if(!This) throw wxe_badarg("This");
  This->SetLabelFont(*font);

}

// wxGrid::SetLabelTextColour
void wxGrid_SetLabelTextColour(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd)
{
  ErlNifEnv *env = Ecmd.env;
  ERL_NIF_TERM * argv = Ecmd.args;
  wxGrid *This;
  This = (wxGrid *) memenv->getPtr(env, argv[0], "This");
  const ERL_NIF_TERM *colour_t;
  int colour_sz;
  if(!enif_get_tuple(env, argv[1], &colour_sz, &colour_t)) Badarg("colour");
  int colourR;
  if(!enif_get_int(env, colour_t[0], &colourR)) Badarg("colour");
  int colourG;
  if(!enif_get_int(env, colour_t[1], &colourG)) Badarg("colour");
  int colourB;
  if(!enif_get_int(env, colour_t[2], &colourB)) Badarg("colour");
  int colourA;
  if(!enif_get_int(env, colour_t[3], &colourA)) Badarg("colour");
  wxColour colour = wxColour(colourR,colourG,colourB,colourA);
  if(!This) throw wxe_badarg("This");
  This->SetLabelTextColour(colour);

}

// wxGrid::SetMargins
void wxGrid_SetMargins(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd)
{
  ErlNifEnv *env = Ecmd.env;
  ERL_NIF_TERM * argv = Ecmd.args;
  wxGrid *This;
  This = (wxGrid *) memenv->getPtr(env, argv[0], "This");
  int extraWidth;
  if(!enif_get_int(env, argv[1], &extraWidth)) Badarg("extraWidth"); // int
  int extraHeight;
  if(!enif_get_int(env, argv[2], &extraHeight)) Badarg("extraHeight"); // int
  if(!This) throw wxe_badarg("This");
  This->SetMargins(extraWidth,extraHeight);

}

// wxGrid::SetReadOnly
void wxGrid_SetReadOnly(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd)
{
  bool isReadOnly=true;
  ErlNifEnv *env = Ecmd.env;
  ERL_NIF_TERM * argv = Ecmd.args;
  wxGrid *This;
  This = (wxGrid *) memenv->getPtr(env, argv[0], "This");
  int row;
  if(!enif_get_int(env, argv[1], &row)) Badarg("row"); // int
  int col;
  if(!enif_get_int(env, argv[2], &col)) Badarg("col"); // int
  ERL_NIF_TERM lstHead, lstTail;
  lstTail = argv[3];
  if(!enif_is_list(env, lstTail)) Badarg("Options");
  const ERL_NIF_TERM *tpl;
  int tpl_sz;
  while(!enif_is_empty_list(env, lstTail)) {
    if(!enif_get_list_cell(env, lstTail, &lstHead, &lstTail)) Badarg("Options");
    if(!enif_get_tuple(env, lstHead, &tpl_sz, &tpl) || tpl_sz != 2) Badarg("Options");
    if(enif_is_identical(tpl[0], enif_make_atom(env, "isReadOnly"))) {
  isReadOnly = enif_is_identical(tpl[1], WXE_ATOM_true);
    } else        Badarg("Options");
  };
  if(!This) throw wxe_badarg("This");
  This->SetReadOnly(row,col,isReadOnly);

}

// wxGrid::SetRowAttr
void wxGrid_SetRowAttr(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd)
{
  ErlNifEnv *env = Ecmd.env;
  ERL_NIF_TERM * argv = Ecmd.args;
  wxGrid *This;
  This = (wxGrid *) memenv->getPtr(env, argv[0], "This");
  int row;
  if(!enif_get_int(env, argv[1], &row)) Badarg("row"); // int
  wxGridCellAttr *attr;
  attr = (wxGridCellAttr *) memenv->getPtr(env, argv[2], "attr");
  if(!This) throw wxe_badarg("This");
  This->SetRowAttr(row,attr);

}

// wxGrid::SetRowLabelAlignment
void wxGrid_SetRowLabelAlignment(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd)
{
  ErlNifEnv *env = Ecmd.env;
  ERL_NIF_TERM * argv = Ecmd.args;
  wxGrid *This;
  This = (wxGrid *) memenv->getPtr(env, argv[0], "This");
  int horiz;
  if(!enif_get_int(env, argv[1], &horiz)) Badarg("horiz"); // int
  int vert;
  if(!enif_get_int(env, argv[2], &vert)) Badarg("vert"); // int
  if(!This) throw wxe_badarg("This");
  This->SetRowLabelAlignment(horiz,vert);

}

// wxGrid::SetRowLabelSize
void wxGrid_SetRowLabelSize(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd)
{
  ErlNifEnv *env = Ecmd.env;
  ERL_NIF_TERM * argv = Ecmd.args;
  wxGrid *This;
  This = (wxGrid *) memenv->getPtr(env, argv[0], "This");
  int width;
  if(!enif_get_int(env, argv[1], &width)) Badarg("width"); // int
  if(!This) throw wxe_badarg("This");
  This->SetRowLabelSize(width);

}

// wxGrid::SetRowLabelValue
void wxGrid_SetRowLabelValue(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd)
{
  ErlNifEnv *env = Ecmd.env;
  ERL_NIF_TERM * argv = Ecmd.args;
  wxGrid *This;
  This = (wxGrid *) memenv->getPtr(env, argv[0], "This");
  int row;
  if(!enif_get_int(env, argv[1], &row)) Badarg("row"); // int
  ErlNifBinary value_bin;
  wxString value;
  if(!enif_inspect_binary(env, argv[2], &value_bin)) Badarg("value");
  value = wxString(value_bin.data, wxConvUTF8, value_bin.size);
  if(!This) throw wxe_badarg("This");
  This->SetRowLabelValue(row,value);

}

// wxGrid::SetRowMinimalHeight
void wxGrid_SetRowMinimalHeight(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd)
{
  ErlNifEnv *env = Ecmd.env;
  ERL_NIF_TERM * argv = Ecmd.args;
  wxGrid *This;
  This = (wxGrid *) memenv->getPtr(env, argv[0], "This");
  int row;
  if(!enif_get_int(env, argv[1], &row)) Badarg("row"); // int
  int height;
  if(!enif_get_int(env, argv[2], &height)) Badarg("height"); // int
  if(!This) throw wxe_badarg("This");
  This->SetRowMinimalHeight(row,height);

}

// wxGrid::SetRowMinimalAcceptableHeight
void wxGrid_SetRowMinimalAcceptableHeight(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd)
{
  ErlNifEnv *env = Ecmd.env;
  ERL_NIF_TERM * argv = Ecmd.args;
  wxGrid *This;
  This = (wxGrid *) memenv->getPtr(env, argv[0], "This");
  int height;
  if(!enif_get_int(env, argv[1], &height)) Badarg("height"); // int
  if(!This) throw wxe_badarg("This");
  This->SetRowMinimalAcceptableHeight(height);

}

// wxGrid::SetRowSize
void wxGrid_SetRowSize(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd)
{
  ErlNifEnv *env = Ecmd.env;
  ERL_NIF_TERM * argv = Ecmd.args;
  wxGrid *This;
  This = (wxGrid *) memenv->getPtr(env, argv[0], "This");
  int row;
  if(!enif_get_int(env, argv[1], &row)) Badarg("row"); // int
  int height;
  if(!enif_get_int(env, argv[2], &height)) Badarg("height"); // int
  if(!This) throw wxe_badarg("This");
  This->SetRowSize(row,height);

}

// wxGrid::SetScrollLineX
void wxGrid_SetScrollLineX(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd)
{
  ErlNifEnv *env = Ecmd.env;
  ERL_NIF_TERM * argv = Ecmd.args;
  wxGrid *This;
  This = (wxGrid *) memenv->getPtr(env, argv[0], "This");
  int x;
  if(!enif_get_int(env, argv[1], &x)) Badarg("x"); // int
  if(!This) throw wxe_badarg("This");
  This->SetScrollLineX(x);

}

// wxGrid::SetScrollLineY
void wxGrid_SetScrollLineY(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd)
{
  ErlNifEnv *env = Ecmd.env;
  ERL_NIF_TERM * argv = Ecmd.args;
  wxGrid *This;
  This = (wxGrid *) memenv->getPtr(env, argv[0], "This");
  int y;
  if(!enif_get_int(env, argv[1], &y)) Badarg("y"); // int
  if(!This) throw wxe_badarg("This");
  This->SetScrollLineY(y);

}

// wxGrid::SetSelectionBackground
void wxGrid_SetSelectionBackground(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd)
{
  ErlNifEnv *env = Ecmd.env;
  ERL_NIF_TERM * argv = Ecmd.args;
  wxGrid *This;
  This = (wxGrid *) memenv->getPtr(env, argv[0], "This");
  const ERL_NIF_TERM *c_t;
  int c_sz;
  if(!enif_get_tuple(env, argv[1], &c_sz, &c_t)) Badarg("c");
  int cR;
  if(!enif_get_int(env, c_t[0], &cR)) Badarg("c");
  int cG;
  if(!enif_get_int(env, c_t[1], &cG)) Badarg("c");
  int cB;
  if(!enif_get_int(env, c_t[2], &cB)) Badarg("c");
  int cA;
  if(!enif_get_int(env, c_t[3], &cA)) Badarg("c");
  wxColour c = wxColour(cR,cG,cB,cA);
  if(!This) throw wxe_badarg("This");
  This->SetSelectionBackground(c);

}

// wxGrid::SetSelectionForeground
void wxGrid_SetSelectionForeground(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd)
{
  ErlNifEnv *env = Ecmd.env;
  ERL_NIF_TERM * argv = Ecmd.args;
  wxGrid *This;
  This = (wxGrid *) memenv->getPtr(env, argv[0], "This");
  const ERL_NIF_TERM *c_t;
  int c_sz;
  if(!enif_get_tuple(env, argv[1], &c_sz, &c_t)) Badarg("c");
  int cR;
  if(!enif_get_int(env, c_t[0], &cR)) Badarg("c");
  int cG;
  if(!enif_get_int(env, c_t[1], &cG)) Badarg("c");
  int cB;
  if(!enif_get_int(env, c_t[2], &cB)) Badarg("c");
  int cA;
  if(!enif_get_int(env, c_t[3], &cA)) Badarg("c");
  wxColour c = wxColour(cR,cG,cB,cA);
  if(!This) throw wxe_badarg("This");
  This->SetSelectionForeground(c);

}

// wxGrid::SetSelectionMode
void wxGrid_SetSelectionMode(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd)
{
  ErlNifEnv *env = Ecmd.env;
  ERL_NIF_TERM * argv = Ecmd.args;
  wxGrid *This;
  This = (wxGrid *) memenv->getPtr(env, argv[0], "This");
  wxGrid::wxGridSelectionModes selmode;
  if(!enif_get_int(env, argv[1], (int *) &selmode)) Badarg("selmode"); // enum
  if(!This) throw wxe_badarg("This");
  This->SetSelectionMode(selmode);

}

// wxGrid::ShowCellEditControl
void wxGrid_ShowCellEditControl(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd)
{
  ErlNifEnv *env = Ecmd.env;
  ERL_NIF_TERM * argv = Ecmd.args;
  wxGrid *This;
  This = (wxGrid *) memenv->getPtr(env, argv[0], "This");
  if(!This) throw wxe_badarg("This");
  This->ShowCellEditControl();

}

// wxGrid::XToCol
void wxGrid_XToCol(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd)
{
  bool clipToMinMax=false;
  ErlNifEnv *env = Ecmd.env;
  ERL_NIF_TERM * argv = Ecmd.args;
  wxGrid *This;
  This = (wxGrid *) memenv->getPtr(env, argv[0], "This");
  int x;
  if(!enif_get_int(env, argv[1], &x)) Badarg("x"); // int
  ERL_NIF_TERM lstHead, lstTail;
  lstTail = argv[2];
  if(!enif_is_list(env, lstTail)) Badarg("Options");
  const ERL_NIF_TERM *tpl;
  int tpl_sz;
  while(!enif_is_empty_list(env, lstTail)) {
    if(!enif_get_list_cell(env, lstTail, &lstHead, &lstTail)) Badarg("Options");
    if(!enif_get_tuple(env, lstHead, &tpl_sz, &tpl) || tpl_sz != 2) Badarg("Options");
    if(enif_is_identical(tpl[0], enif_make_atom(env, "clipToMinMax"))) {
  clipToMinMax = enif_is_identical(tpl[1], WXE_ATOM_true);
    } else        Badarg("Options");
  };
  if(!This) throw wxe_badarg("This");
  int Result = This->XToCol(x,clipToMinMax);
  wxeReturn rt = wxeReturn(memenv, Ecmd.caller, true);
  rt.send(  rt.make_int(Result));

}

// wxGrid::XToEdgeOfCol
void wxGrid_XToEdgeOfCol(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd)
{
  ErlNifEnv *env = Ecmd.env;
  ERL_NIF_TERM * argv = Ecmd.args;
  wxGrid *This;
  This = (wxGrid *) memenv->getPtr(env, argv[0], "This");
  int x;
  if(!enif_get_int(env, argv[1], &x)) Badarg("x"); // int
  if(!This) throw wxe_badarg("This");
  int Result = This->XToEdgeOfCol(x);
  wxeReturn rt = wxeReturn(memenv, Ecmd.caller, true);
  rt.send(  rt.make_int(Result));

}

// wxGrid::YToEdgeOfRow
void wxGrid_YToEdgeOfRow(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd)
{
  ErlNifEnv *env = Ecmd.env;
  ERL_NIF_TERM * argv = Ecmd.args;
  wxGrid *This;
  This = (wxGrid *) memenv->getPtr(env, argv[0], "This");
  int y;
  if(!enif_get_int(env, argv[1], &y)) Badarg("y"); // int
  if(!This) throw wxe_badarg("This");
  int Result = This->YToEdgeOfRow(y);
  wxeReturn rt = wxeReturn(memenv, Ecmd.caller, true);
  rt.send(  rt.make_int(Result));

}

// wxGrid::YToRow
void wxGrid_YToRow(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd)
{
  bool clipToMinMax=false;
  ErlNifEnv *env = Ecmd.env;
  ERL_NIF_TERM * argv = Ecmd.args;
  wxGrid *This;
  This = (wxGrid *) memenv->getPtr(env, argv[0], "This");
  int y;
  if(!enif_get_int(env, argv[1], &y)) Badarg("y"); // int
  ERL_NIF_TERM lstHead, lstTail;
  lstTail = argv[2];
  if(!enif_is_list(env, lstTail)) Badarg("Options");
  const ERL_NIF_TERM *tpl;
  int tpl_sz;
  while(!enif_is_empty_list(env, lstTail)) {
    if(!enif_get_list_cell(env, lstTail, &lstHead, &lstTail)) Badarg("Options");
    if(!enif_get_tuple(env, lstHead, &tpl_sz, &tpl) || tpl_sz != 2) Badarg("Options");
    if(enif_is_identical(tpl[0], enif_make_atom(env, "clipToMinMax"))) {
  clipToMinMax = enif_is_identical(tpl[1], WXE_ATOM_true);
    } else        Badarg("Options");
  };
  if(!This) throw wxe_badarg("This");
  int Result = This->YToRow(y,clipToMinMax);
  wxeReturn rt = wxeReturn(memenv, Ecmd.caller, true);
  rt.send(  rt.make_int(Result));

}

// wxGridBagSizer::wxGridBagSizer
void wxGridBagSizer_new(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd)
{
  int vgap=0;
  int hgap=0;
  ErlNifEnv *env = Ecmd.env;
  ERL_NIF_TERM * argv = Ecmd.args;
  ERL_NIF_TERM lstHead, lstTail;
  lstTail = argv[0];
  if(!enif_is_list(env, lstTail)) Badarg("Options");
  const ERL_NIF_TERM *tpl;
  int tpl_sz;
  while(!enif_is_empty_list(env, lstTail)) {
    if(!enif_get_list_cell(env, lstTail, &lstHead, &lstTail)) Badarg("Options");
    if(!enif_get_tuple(env, lstHead, &tpl_sz, &tpl) || tpl_sz != 2) Badarg("Options");
    if(enif_is_identical(tpl[0], enif_make_atom(env, "vgap"))) {
  if(!enif_get_int(env, tpl[1], &vgap)) Badarg("vgap"); // int
    } else     if(enif_is_identical(tpl[0], enif_make_atom(env, "hgap"))) {
  if(!enif_get_int(env, tpl[1], &hgap)) Badarg("hgap"); // int
    } else        Badarg("Options");
  };
  wxGridBagSizer * Result = new EwxGridBagSizer(vgap,hgap);
  app->newPtr((void *) Result, 1, memenv);
  wxeReturn rt = wxeReturn(memenv, Ecmd.caller, true);
  rt.send(  rt.make_ref(app->getRef((void *)Result,memenv), "wxGridBagSizer"));

}

// wxGridBagSizer::Add
void wxGridBagSizer_Add_3(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd)
{
  wxGBSpan span= wxDefaultSpan;
  int flag=0;
  int border=0;
  wxObject * userData=NULL;
  ErlNifEnv *env = Ecmd.env;
  ERL_NIF_TERM * argv = Ecmd.args;
  wxGridBagSizer *This;
  This = (wxGridBagSizer *) memenv->getPtr(env, argv[0], "This");
  ERL_NIF_TERM window_type;
  void * window = memenv->getPtr(env, argv[1], "window", &window_type);
  const ERL_NIF_TERM *pos_t;
  int pos_sz;
  if(!enif_get_tuple(env, argv[2], &pos_sz, &pos_t)) Badarg("pos");
  int posR;
  if(!enif_get_int(env, pos_t[0], &posR)) Badarg("pos");
  int posC;
  if(!enif_get_int(env, pos_t[1], &posC)) Badarg("pos");
  wxGBPosition pos = wxGBPosition(posR,posC);
  ERL_NIF_TERM lstHead, lstTail;
  lstTail = argv[3];
  if(!enif_is_list(env, lstTail)) Badarg("Options");
  const ERL_NIF_TERM *tpl;
  int tpl_sz;
  while(!enif_is_empty_list(env, lstTail)) {
    if(!enif_get_list_cell(env, lstTail, &lstHead, &lstTail)) Badarg("Options");
    if(!enif_get_tuple(env, lstHead, &tpl_sz, &tpl) || tpl_sz != 2) Badarg("Options");
    if(enif_is_identical(tpl[0], enif_make_atom(env, "span"))) {
  const ERL_NIF_TERM *span_t;
  int span_sz;
  if(!enif_get_tuple(env, tpl[1], &span_sz, &span_t)) Badarg("span");
  int spanRS;
  if(!enif_get_int(env, span_t[0], &spanRS)) Badarg("span");
  int spanCS;
  if(!enif_get_int(env, span_t[1], &spanCS)) Badarg("span");
  span = wxGBSpan(spanRS,spanCS);
    } else     if(enif_is_identical(tpl[0], enif_make_atom(env, "flag"))) {
  if(!enif_get_int(env, tpl[1], &flag)) Badarg("flag"); // int
    } else     if(enif_is_identical(tpl[0], enif_make_atom(env, "border"))) {
  if(!enif_get_int(env, tpl[1], &border)) Badarg("border"); // int
    } else     if(enif_is_identical(tpl[0], enif_make_atom(env, "userData"))) {
  userData = (wxObject *) memenv->getPtr(env, tpl[1], "userData");
    } else        Badarg("Options");
  };
  if(!This) throw wxe_badarg("This");
  wxSizerItem * Result;
  if(enif_is_identical(window_type, WXE_ATOM_wxWindow))
   Result =  (wxSizerItem*)This->Add(static_cast<wxWindow*> (window),pos,span,flag,border,userData);
  else if(enif_is_identical(window_type, WXE_ATOM_wxSizer))
   Result =  (wxSizerItem*)This->Add(static_cast<wxSizer*> (window),pos,span,flag,border,userData);
  else throw wxe_badarg("window");
  wxeReturn rt = wxeReturn(memenv, Ecmd.caller, true);
  rt.send(  rt.make_ref(app->getRef((void *)Result,memenv), "wxSizerItem"));

}

// wxGridBagSizer::Add
void wxGridBagSizer_Add_1(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd)
{
  ErlNifEnv *env = Ecmd.env;
  ERL_NIF_TERM * argv = Ecmd.args;
  wxGridBagSizer *This;
  This = (wxGridBagSizer *) memenv->getPtr(env, argv[0], "This");
  wxGBSizerItem *item;
  item = (wxGBSizerItem *) memenv->getPtr(env, argv[1], "item");
  if(!This) throw wxe_badarg("This");
  wxSizerItem * Result = (wxSizerItem*)This->Add(item);
  wxeReturn rt = wxeReturn(memenv, Ecmd.caller, true);
  rt.send(  rt.make_ref(app->getRef((void *)Result,memenv), "wxSizerItem"));

}

// wxGridBagSizer::Add
void wxGridBagSizer_Add_4(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd)
{
  wxGBSpan span= wxDefaultSpan;
  int flag=0;
  int border=0;
  wxObject * userData=NULL;
  ErlNifEnv *env = Ecmd.env;
  ERL_NIF_TERM * argv = Ecmd.args;
  wxGridBagSizer *This;
  This = (wxGridBagSizer *) memenv->getPtr(env, argv[0], "This");
  int width;
  if(!enif_get_int(env, argv[1], &width)) Badarg("width"); // int
  int height;
  if(!enif_get_int(env, argv[2], &height)) Badarg("height"); // int
  const ERL_NIF_TERM *pos_t;
  int pos_sz;
  if(!enif_get_tuple(env, argv[3], &pos_sz, &pos_t)) Badarg("pos");
  int posR;
  if(!enif_get_int(env, pos_t[0], &posR)) Badarg("pos");
  int posC;
  if(!enif_get_int(env, pos_t[1], &posC)) Badarg("pos");
  wxGBPosition pos = wxGBPosition(posR,posC);
  ERL_NIF_TERM lstHead, lstTail;
  lstTail = argv[4];
  if(!enif_is_list(env, lstTail)) Badarg("Options");
  const ERL_NIF_TERM *tpl;
  int tpl_sz;
  while(!enif_is_empty_list(env, lstTail)) {
    if(!enif_get_list_cell(env, lstTail, &lstHead, &lstTail)) Badarg("Options");
    if(!enif_get_tuple(env, lstHead, &tpl_sz, &tpl) || tpl_sz != 2) Badarg("Options");
    if(enif_is_identical(tpl[0], enif_make_atom(env, "span"))) {
  const ERL_NIF_TERM *span_t;
  int span_sz;
  if(!enif_get_tuple(env, tpl[1], &span_sz, &span_t)) Badarg("span");
  int spanRS;
  if(!enif_get_int(env, span_t[0], &spanRS)) Badarg("span");
  int spanCS;
  if(!enif_get_int(env, span_t[1], &spanCS)) Badarg("span");
  span = wxGBSpan(spanRS,spanCS);
    } else     if(enif_is_identical(tpl[0], enif_make_atom(env, "flag"))) {
  if(!enif_get_int(env, tpl[1], &flag)) Badarg("flag"); // int
    } else     if(enif_is_identical(tpl[0], enif_make_atom(env, "border"))) {
  if(!enif_get_int(env, tpl[1], &border)) Badarg("border"); // int
    } else     if(enif_is_identical(tpl[0], enif_make_atom(env, "userData"))) {
  userData = (wxObject *) memenv->getPtr(env, tpl[1], "userData");
    } else        Badarg("Options");
  };
  if(!This) throw wxe_badarg("This");
  wxSizerItem * Result = (wxSizerItem*)This->Add(width,height,pos,span,flag,border,userData);
  wxeReturn rt = wxeReturn(memenv, Ecmd.caller, true);
  rt.send(  rt.make_ref(app->getRef((void *)Result,memenv), "wxSizerItem"));

}

// wxGridBagSizer::CalcMin
void wxGridBagSizer_CalcMin(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd)
{
  ErlNifEnv *env = Ecmd.env;
  ERL_NIF_TERM * argv = Ecmd.args;
  wxGridBagSizer *This;
  This = (wxGridBagSizer *) memenv->getPtr(env, argv[0], "This");
  if(!This) throw wxe_badarg("This");
  wxSize Result = This->CalcMin();
  wxeReturn rt = wxeReturn(memenv, Ecmd.caller, true);
  rt.send(  rt.make(Result));

}

// wxGridBagSizer::CheckForIntersection
void wxGridBagSizer_CheckForIntersection_2(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd)
{
  wxGBSizerItem * excludeItem=NULL;
  ErlNifEnv *env = Ecmd.env;
  ERL_NIF_TERM * argv = Ecmd.args;
  wxGridBagSizer *This;
  This = (wxGridBagSizer *) memenv->getPtr(env, argv[0], "This");
  wxGBSizerItem *item;
  item = (wxGBSizerItem *) memenv->getPtr(env, argv[1], "item");
  ERL_NIF_TERM lstHead, lstTail;
  lstTail = argv[2];
  if(!enif_is_list(env, lstTail)) Badarg("Options");
  const ERL_NIF_TERM *tpl;
  int tpl_sz;
  while(!enif_is_empty_list(env, lstTail)) {
    if(!enif_get_list_cell(env, lstTail, &lstHead, &lstTail)) Badarg("Options");
    if(!enif_get_tuple(env, lstHead, &tpl_sz, &tpl) || tpl_sz != 2) Badarg("Options");
    if(enif_is_identical(tpl[0], enif_make_atom(env, "excludeItem"))) {
  excludeItem = (wxGBSizerItem *) memenv->getPtr(env, tpl[1], "excludeItem");
    } else        Badarg("Options");
  };
  if(!This) throw wxe_badarg("This");
  bool Result = This->CheckForIntersection(item,excludeItem);
  wxeReturn rt = wxeReturn(memenv, Ecmd.caller, true);
  rt.send(  rt.make_bool(Result));

}

// wxGridBagSizer::CheckForIntersection
void wxGridBagSizer_CheckForIntersection_3(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd)
{
  wxGBSizerItem * excludeItem=NULL;
  ErlNifEnv *env = Ecmd.env;
  ERL_NIF_TERM * argv = Ecmd.args;
  wxGridBagSizer *This;
  This = (wxGridBagSizer *) memenv->getPtr(env, argv[0], "This");
  const ERL_NIF_TERM *pos_t;
  int pos_sz;
  if(!enif_get_tuple(env, argv[1], &pos_sz, &pos_t)) Badarg("pos");
  int posR;
  if(!enif_get_int(env, pos_t[0], &posR)) Badarg("pos");
  int posC;
  if(!enif_get_int(env, pos_t[1], &posC)) Badarg("pos");
  wxGBPosition pos = wxGBPosition(posR,posC);
  const ERL_NIF_TERM *span_t;
  int span_sz;
  if(!enif_get_tuple(env, argv[2], &span_sz, &span_t)) Badarg("span");
  int spanRS;
  if(!enif_get_int(env, span_t[0], &spanRS)) Badarg("span");
  int spanCS;
  if(!enif_get_int(env, span_t[1], &spanCS)) Badarg("span");
  wxGBSpan span = wxGBSpan(spanRS,spanCS);
  ERL_NIF_TERM lstHead, lstTail;
  lstTail = argv[3];
  if(!enif_is_list(env, lstTail)) Badarg("Options");
  const ERL_NIF_TERM *tpl;
  int tpl_sz;
  while(!enif_is_empty_list(env, lstTail)) {
    if(!enif_get_list_cell(env, lstTail, &lstHead, &lstTail)) Badarg("Options");
    if(!enif_get_tuple(env, lstHead, &tpl_sz, &tpl) || tpl_sz != 2) Badarg("Options");
    if(enif_is_identical(tpl[0], enif_make_atom(env, "excludeItem"))) {
  excludeItem = (wxGBSizerItem *) memenv->getPtr(env, tpl[1], "excludeItem");
    } else        Badarg("Options");
  };
  if(!This) throw wxe_badarg("This");
  bool Result = This->CheckForIntersection(pos,span,excludeItem);
  wxeReturn rt = wxeReturn(memenv, Ecmd.caller, true);
  rt.send(  rt.make_bool(Result));

}

// wxGridBagSizer::FindItem
void wxGridBagSizer_FindItem(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd)
{
  ErlNifEnv *env = Ecmd.env;
  ERL_NIF_TERM * argv = Ecmd.args;
  wxGridBagSizer *This;
  This = (wxGridBagSizer *) memenv->getPtr(env, argv[0], "This");
  ERL_NIF_TERM window_type;
  void * window = memenv->getPtr(env, argv[1], "window", &window_type);
  if(!This) throw wxe_badarg("This");
  wxGBSizerItem * Result;
  if(enif_is_identical(window_type, WXE_ATOM_wxWindow))
   Result =  (wxGBSizerItem*)This->FindItem(static_cast<wxWindow*> (window));
  else if(enif_is_identical(window_type, WXE_ATOM_wxSizer))
   Result =  (wxGBSizerItem*)This->FindItem(static_cast<wxSizer*> (window));
  else throw wxe_badarg("window");
  wxeReturn rt = wxeReturn(memenv, Ecmd.caller, true);
  rt.send(  rt.make_ref(app->getRef((void *)Result,memenv), "wxGBSizerItem"));

}

// wxGridBagSizer::FindItemAtPoint
void wxGridBagSizer_FindItemAtPoint(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd)
{
  ErlNifEnv *env = Ecmd.env;
  ERL_NIF_TERM * argv = Ecmd.args;
  wxGridBagSizer *This;
  This = (wxGridBagSizer *) memenv->getPtr(env, argv[0], "This");
  const ERL_NIF_TERM *pt_t;
  int pt_sz;
  if(!enif_get_tuple(env, argv[1], &pt_sz, &pt_t)) Badarg("pt");
  int ptX;
  if(!enif_get_int(env, pt_t[0], &ptX)) Badarg("pt");
  int ptY;
  if(!enif_get_int(env, pt_t[1], &ptY)) Badarg("pt");
  wxPoint pt = wxPoint(ptX,ptY);
  if(!This) throw wxe_badarg("This");
  wxGBSizerItem * Result = (wxGBSizerItem*)This->FindItemAtPoint(pt);
  wxeReturn rt = wxeReturn(memenv, Ecmd.caller, true);
  rt.send(  rt.make_ref(app->getRef((void *)Result,memenv), "wxGBSizerItem"));

}

// wxGridBagSizer::FindItemAtPosition
void wxGridBagSizer_FindItemAtPosition(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd)
{
  ErlNifEnv *env = Ecmd.env;
  ERL_NIF_TERM * argv = Ecmd.args;
  wxGridBagSizer *This;
  This = (wxGridBagSizer *) memenv->getPtr(env, argv[0], "This");
  const ERL_NIF_TERM *pos_t;
  int pos_sz;
  if(!enif_get_tuple(env, argv[1], &pos_sz, &pos_t)) Badarg("pos");
  int posR;
  if(!enif_get_int(env, pos_t[0], &posR)) Badarg("pos");
  int posC;
  if(!enif_get_int(env, pos_t[1], &posC)) Badarg("pos");
  wxGBPosition pos = wxGBPosition(posR,posC);
  if(!This) throw wxe_badarg("This");
  wxGBSizerItem * Result = (wxGBSizerItem*)This->FindItemAtPosition(pos);
  wxeReturn rt = wxeReturn(memenv, Ecmd.caller, true);
  rt.send(  rt.make_ref(app->getRef((void *)Result,memenv), "wxGBSizerItem"));

}

// wxGridBagSizer::FindItemWithData
void wxGridBagSizer_FindItemWithData(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd)
{
  ErlNifEnv *env = Ecmd.env;
  ERL_NIF_TERM * argv = Ecmd.args;
  wxGridBagSizer *This;
  This = (wxGridBagSizer *) memenv->getPtr(env, argv[0], "This");
  wxObject *userData;
  userData = (wxObject *) memenv->getPtr(env, argv[1], "userData");
  if(!This) throw wxe_badarg("This");
  wxGBSizerItem * Result = (wxGBSizerItem*)This->FindItemWithData(userData);
  wxeReturn rt = wxeReturn(memenv, Ecmd.caller, true);
  rt.send(  rt.make_ref(app->getRef((void *)Result,memenv), "wxGBSizerItem"));

}

// wxGridBagSizer::GetCellSize
void wxGridBagSizer_GetCellSize(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd)
{
  ErlNifEnv *env = Ecmd.env;
  ERL_NIF_TERM * argv = Ecmd.args;
  wxGridBagSizer *This;
  This = (wxGridBagSizer *) memenv->getPtr(env, argv[0], "This");
  int row;
  if(!enif_get_int(env, argv[1], &row)) Badarg("row"); // int
  int col;
  if(!enif_get_int(env, argv[2], &col)) Badarg("col"); // int
  if(!This) throw wxe_badarg("This");
  wxSize Result = This->GetCellSize(row,col);
  wxeReturn rt = wxeReturn(memenv, Ecmd.caller, true);
  rt.send(  rt.make(Result));

}

// wxGridBagSizer::GetEmptyCellSize
void wxGridBagSizer_GetEmptyCellSize(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd)
{
  ErlNifEnv *env = Ecmd.env;
  ERL_NIF_TERM * argv = Ecmd.args;
  wxGridBagSizer *This;
  This = (wxGridBagSizer *) memenv->getPtr(env, argv[0], "This");
  if(!This) throw wxe_badarg("This");
  wxSize Result = This->GetEmptyCellSize();
  wxeReturn rt = wxeReturn(memenv, Ecmd.caller, true);
  rt.send(  rt.make(Result));

}

// wxGridBagSizer::GetItemPosition
void wxGridBagSizer_GetItemPosition_1_0(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd)
{
  ErlNifEnv *env = Ecmd.env;
  ERL_NIF_TERM * argv = Ecmd.args;
  wxGridBagSizer *This;
  This = (wxGridBagSizer *) memenv->getPtr(env, argv[0], "This");
  ERL_NIF_TERM window_type;
  void * window = memenv->getPtr(env, argv[1], "window", &window_type);
  if(!This) throw wxe_badarg("This");
  wxGBPosition Result;
  if(enif_is_identical(window_type, WXE_ATOM_wxWindow))
   Result =  This->GetItemPosition(static_cast<wxWindow*> (window));
  else if(enif_is_identical(window_type, WXE_ATOM_wxSizer))
   Result =  This->GetItemPosition(static_cast<wxSizer*> (window));
  else throw wxe_badarg("window");
  wxeReturn rt = wxeReturn(memenv, Ecmd.caller, true);
  rt.send(  rt.make(Result));

}

// wxGridBagSizer::GetItemPosition
void wxGridBagSizer_GetItemPosition_1_1(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd)
{
  ErlNifEnv *env = Ecmd.env;
  ERL_NIF_TERM * argv = Ecmd.args;
  wxGridBagSizer *This;
  This = (wxGridBagSizer *) memenv->getPtr(env, argv[0], "This");
  size_t index;
  if(!wxe_get_size_t(env, argv[1], &index)) Badarg("index");
  if(!This) throw wxe_badarg("This");
  wxGBPosition Result = This->GetItemPosition(index);
  wxeReturn rt = wxeReturn(memenv, Ecmd.caller, true);
  rt.send(  rt.make(Result));

}

// wxGridBagSizer::GetItemSpan
void wxGridBagSizer_GetItemSpan_1_0(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd)
{
  ErlNifEnv *env = Ecmd.env;
  ERL_NIF_TERM * argv = Ecmd.args;
  wxGridBagSizer *This;
  This = (wxGridBagSizer *) memenv->getPtr(env, argv[0], "This");
  ERL_NIF_TERM window_type;
  void * window = memenv->getPtr(env, argv[1], "window", &window_type);
  if(!This) throw wxe_badarg("This");
  wxGBSpan Result;
  if(enif_is_identical(window_type, WXE_ATOM_wxWindow))
   Result =  This->GetItemSpan(static_cast<wxWindow*> (window));
  else if(enif_is_identical(window_type, WXE_ATOM_wxSizer))
   Result =  This->GetItemSpan(static_cast<wxSizer*> (window));
  else throw wxe_badarg("window");
  wxeReturn rt = wxeReturn(memenv, Ecmd.caller, true);
  rt.send(  rt.make(Result));

}

// wxGridBagSizer::GetItemSpan
void wxGridBagSizer_GetItemSpan_1_1(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd)
{
  ErlNifEnv *env = Ecmd.env;
  ERL_NIF_TERM * argv = Ecmd.args;
  wxGridBagSizer *This;
  This = (wxGridBagSizer *) memenv->getPtr(env, argv[0], "This");
  size_t index;
  if(!wxe_get_size_t(env, argv[1], &index)) Badarg("index");
  if(!This) throw wxe_badarg("This");
  wxGBSpan Result = This->GetItemSpan(index);
  wxeReturn rt = wxeReturn(memenv, Ecmd.caller, true);
  rt.send(  rt.make(Result));

}

// wxGridBagSizer::SetEmptyCellSize
void wxGridBagSizer_SetEmptyCellSize(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd)
{
  ErlNifEnv *env = Ecmd.env;
  ERL_NIF_TERM * argv = Ecmd.args;
  wxGridBagSizer *This;
  This = (wxGridBagSizer *) memenv->getPtr(env, argv[0], "This");
  const ERL_NIF_TERM *sz_t;
  int sz_sz;
  if(!enif_get_tuple(env, argv[1], &sz_sz, &sz_t)) Badarg("sz");
  int szW;
  if(!enif_get_int(env, sz_t[0], &szW)) Badarg("sz");
  int szH;
  if(!enif_get_int(env, sz_t[1], &szH)) Badarg("sz");
  wxSize sz = wxSize(szW,szH);
  if(!This) throw wxe_badarg("This");
  This->SetEmptyCellSize(sz);

}

// wxGridBagSizer::SetItemPosition
void wxGridBagSizer_SetItemPosition_2_0(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd)
{
  ErlNifEnv *env = Ecmd.env;
  ERL_NIF_TERM * argv = Ecmd.args;
  wxGridBagSizer *This;
  This = (wxGridBagSizer *) memenv->getPtr(env, argv[0], "This");
  ERL_NIF_TERM window_type;
  void * window = memenv->getPtr(env, argv[1], "window", &window_type);
  const ERL_NIF_TERM *pos_t;
  int pos_sz;
  if(!enif_get_tuple(env, argv[2], &pos_sz, &pos_t)) Badarg("pos");
  int posR;
  if(!enif_get_int(env, pos_t[0], &posR)) Badarg("pos");
  int posC;
  if(!enif_get_int(env, pos_t[1], &posC)) Badarg("pos");
  wxGBPosition pos = wxGBPosition(posR,posC);
  if(!This) throw wxe_badarg("This");
  bool Result;
  if(enif_is_identical(window_type, WXE_ATOM_wxWindow))
   Result =  This->SetItemPosition(static_cast<wxWindow*> (window),pos);
  else if(enif_is_identical(window_type, WXE_ATOM_wxSizer))
   Result =  This->SetItemPosition(static_cast<wxSizer*> (window),pos);
  else throw wxe_badarg("window");
  wxeReturn rt = wxeReturn(memenv, Ecmd.caller, true);
  rt.send(  rt.make_bool(Result));

}

// wxGridBagSizer::SetItemPosition
void wxGridBagSizer_SetItemPosition_2_1(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd)
{
  ErlNifEnv *env = Ecmd.env;
  ERL_NIF_TERM * argv = Ecmd.args;
  wxGridBagSizer *This;
  This = (wxGridBagSizer *) memenv->getPtr(env, argv[0], "This");
  size_t index;
  if(!wxe_get_size_t(env, argv[1], &index)) Badarg("index");
  const ERL_NIF_TERM *pos_t;
  int pos_sz;
  if(!enif_get_tuple(env, argv[2], &pos_sz, &pos_t)) Badarg("pos");
  int posR;
  if(!enif_get_int(env, pos_t[0], &posR)) Badarg("pos");
  int posC;
  if(!enif_get_int(env, pos_t[1], &posC)) Badarg("pos");
  wxGBPosition pos = wxGBPosition(posR,posC);
  if(!This) throw wxe_badarg("This");
  bool Result = This->SetItemPosition(index,pos);
  wxeReturn rt = wxeReturn(memenv, Ecmd.caller, true);
  rt.send(  rt.make_bool(Result));

}

// wxGridBagSizer::SetItemSpan
void wxGridBagSizer_SetItemSpan_2_0(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd)
{
  ErlNifEnv *env = Ecmd.env;
  ERL_NIF_TERM * argv = Ecmd.args;
  wxGridBagSizer *This;
  This = (wxGridBagSizer *) memenv->getPtr(env, argv[0], "This");
  ERL_NIF_TERM window_type;
  void * window = memenv->getPtr(env, argv[1], "window", &window_type);
  const ERL_NIF_TERM *span_t;
  int span_sz;
  if(!enif_get_tuple(env, argv[2], &span_sz, &span_t)) Badarg("span");
  int spanRS;
  if(!enif_get_int(env, span_t[0], &spanRS)) Badarg("span");
  int spanCS;
  if(!enif_get_int(env, span_t[1], &spanCS)) Badarg("span");
  wxGBSpan span = wxGBSpan(spanRS,spanCS);
  if(!This) throw wxe_badarg("This");
  bool Result;
  if(enif_is_identical(window_type, WXE_ATOM_wxWindow))
   Result =  This->SetItemSpan(static_cast<wxWindow*> (window),span);
  else if(enif_is_identical(window_type, WXE_ATOM_wxSizer))
   Result =  This->SetItemSpan(static_cast<wxSizer*> (window),span);
  else throw wxe_badarg("window");
  wxeReturn rt = wxeReturn(memenv, Ecmd.caller, true);
  rt.send(  rt.make_bool(Result));

}

// wxGridBagSizer::SetItemSpan
void wxGridBagSizer_SetItemSpan_2_1(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd)
{
  ErlNifEnv *env = Ecmd.env;
  ERL_NIF_TERM * argv = Ecmd.args;
  wxGridBagSizer *This;
  This = (wxGridBagSizer *) memenv->getPtr(env, argv[0], "This");
  size_t index;
  if(!wxe_get_size_t(env, argv[1], &index)) Badarg("index");
  const ERL_NIF_TERM *span_t;
  int span_sz;
  if(!enif_get_tuple(env, argv[2], &span_sz, &span_t)) Badarg("span");
  int spanRS;
  if(!enif_get_int(env, span_t[0], &spanRS)) Badarg("span");
  int spanCS;
  if(!enif_get_int(env, span_t[1], &spanCS)) Badarg("span");
  wxGBSpan span = wxGBSpan(spanRS,spanCS);
  if(!This) throw wxe_badarg("This");
  bool Result = This->SetItemSpan(index,span);
  wxeReturn rt = wxeReturn(memenv, Ecmd.caller, true);
  rt.send(  rt.make_bool(Result));

}

// wxGridCellAttr::SetTextColour
void wxGridCellAttr_SetTextColour(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd)
{
  ErlNifEnv *env = Ecmd.env;
  ERL_NIF_TERM * argv = Ecmd.args;
  wxGridCellAttr *This;
  This = (wxGridCellAttr *) memenv->getPtr(env, argv[0], "This");
  const ERL_NIF_TERM *colText_t;
  int colText_sz;
  if(!enif_get_tuple(env, argv[1], &colText_sz, &colText_t)) Badarg("colText");
  int colTextR;
  if(!enif_get_int(env, colText_t[0], &colTextR)) Badarg("colText");
  int colTextG;
  if(!enif_get_int(env, colText_t[1], &colTextG)) Badarg("colText");
  int colTextB;
  if(!enif_get_int(env, colText_t[2], &colTextB)) Badarg("colText");
  int colTextA;
  if(!enif_get_int(env, colText_t[3], &colTextA)) Badarg("colText");
  wxColour colText = wxColour(colTextR,colTextG,colTextB,colTextA);
  if(!This) throw wxe_badarg("This");
  This->SetTextColour(colText);

}

// wxGridCellAttr::SetBackgroundColour
void wxGridCellAttr_SetBackgroundColour(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd)
{
  ErlNifEnv *env = Ecmd.env;
  ERL_NIF_TERM * argv = Ecmd.args;
  wxGridCellAttr *This;
  This = (wxGridCellAttr *) memenv->getPtr(env, argv[0], "This");
  const ERL_NIF_TERM *colBack_t;
  int colBack_sz;
  if(!enif_get_tuple(env, argv[1], &colBack_sz, &colBack_t)) Badarg("colBack");
  int colBackR;
  if(!enif_get_int(env, colBack_t[0], &colBackR)) Badarg("colBack");
  int colBackG;
  if(!enif_get_int(env, colBack_t[1], &colBackG)) Badarg("colBack");
  int colBackB;
  if(!enif_get_int(env, colBack_t[2], &colBackB)) Badarg("colBack");
  int colBackA;
  if(!enif_get_int(env, colBack_t[3], &colBackA)) Badarg("colBack");
  wxColour colBack = wxColour(colBackR,colBackG,colBackB,colBackA);
  if(!This) throw wxe_badarg("This");
  This->SetBackgroundColour(colBack);

}

// wxGridCellAttr::SetFont
void wxGridCellAttr_SetFont(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd)
{
  ErlNifEnv *env = Ecmd.env;
  ERL_NIF_TERM * argv = Ecmd.args;
  wxGridCellAttr *This;
  This = (wxGridCellAttr *) memenv->getPtr(env, argv[0], "This");
  wxFont *font;
  font = (wxFont *) memenv->getPtr(env, argv[1], "font");
  if(!This) throw wxe_badarg("This");
  This->SetFont(*font);

}

// wxGridCellAttr::SetAlignment
void wxGridCellAttr_SetAlignment(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd)
{
  ErlNifEnv *env = Ecmd.env;
  ERL_NIF_TERM * argv = Ecmd.args;
  wxGridCellAttr *This;
  This = (wxGridCellAttr *) memenv->getPtr(env, argv[0], "This");
  int hAlign;
  if(!enif_get_int(env, argv[1], &hAlign)) Badarg("hAlign"); // int
  int vAlign;
  if(!enif_get_int(env, argv[2], &vAlign)) Badarg("vAlign"); // int
  if(!This) throw wxe_badarg("This");
  This->SetAlignment(hAlign,vAlign);

}

// wxGridCellAttr::SetReadOnly
void wxGridCellAttr_SetReadOnly(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd)
{
  bool isReadOnly=true;
  ErlNifEnv *env = Ecmd.env;
  ERL_NIF_TERM * argv = Ecmd.args;
  wxGridCellAttr *This;
  This = (wxGridCellAttr *) memenv->getPtr(env, argv[0], "This");
  ERL_NIF_TERM lstHead, lstTail;
  lstTail = argv[1];
  if(!enif_is_list(env, lstTail)) Badarg("Options");
  const ERL_NIF_TERM *tpl;
  int tpl_sz;
  while(!enif_is_empty_list(env, lstTail)) {
    if(!enif_get_list_cell(env, lstTail, &lstHead, &lstTail)) Badarg("Options");
    if(!enif_get_tuple(env, lstHead, &tpl_sz, &tpl) || tpl_sz != 2) Badarg("Options");
    if(enif_is_identical(tpl[0], enif_make_atom(env, "isReadOnly"))) {
  isReadOnly = enif_is_identical(tpl[1], WXE_ATOM_true);
    } else        Badarg("Options");
  };
  if(!This) throw wxe_badarg("This");
  This->SetReadOnly(isReadOnly);

}

// wxGridCellAttr::SetRenderer
void wxGridCellAttr_SetRenderer(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd)
{
  ErlNifEnv *env = Ecmd.env;
  ERL_NIF_TERM * argv = Ecmd.args;
  wxGridCellAttr *This;
  This = (wxGridCellAttr *) memenv->getPtr(env, argv[0], "This");
  wxGridCellRenderer *renderer;
  renderer = (wxGridCellRenderer *) memenv->getPtr(env, argv[1], "renderer");
  if(!This) throw wxe_badarg("This");
  This->SetRenderer(renderer);

}

// wxGridCellAttr::SetEditor
void wxGridCellAttr_SetEditor(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd)
{
  ErlNifEnv *env = Ecmd.env;
  ERL_NIF_TERM * argv = Ecmd.args;
  wxGridCellAttr *This;
  This = (wxGridCellAttr *) memenv->getPtr(env, argv[0], "This");
  wxGridCellEditor *editor;
  editor = (wxGridCellEditor *) memenv->getPtr(env, argv[1], "editor");
  if(!This) throw wxe_badarg("This");
  This->SetEditor(editor);

}

// wxGridCellAttr::HasTextColour
void wxGridCellAttr_HasTextColour(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd)
{
  ErlNifEnv *env = Ecmd.env;
  ERL_NIF_TERM * argv = Ecmd.args;
  wxGridCellAttr *This;
  This = (wxGridCellAttr *) memenv->getPtr(env, argv[0], "This");
  if(!This) throw wxe_badarg("This");
  bool Result = This->HasTextColour();
  wxeReturn rt = wxeReturn(memenv, Ecmd.caller, true);
  rt.send(  rt.make_bool(Result));

}

// wxGridCellAttr::HasBackgroundColour
void wxGridCellAttr_HasBackgroundColour(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd)
{
  ErlNifEnv *env = Ecmd.env;
  ERL_NIF_TERM * argv = Ecmd.args;
  wxGridCellAttr *This;
  This = (wxGridCellAttr *) memenv->getPtr(env, argv[0], "This");
  if(!This) throw wxe_badarg("This");
  bool Result = This->HasBackgroundColour();
  wxeReturn rt = wxeReturn(memenv, Ecmd.caller, true);
  rt.send(  rt.make_bool(Result));

}

// wxGridCellAttr::HasFont
void wxGridCellAttr_HasFont(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd)
{
  ErlNifEnv *env = Ecmd.env;
  ERL_NIF_TERM * argv = Ecmd.args;
  wxGridCellAttr *This;
  This = (wxGridCellAttr *) memenv->getPtr(env, argv[0], "This");
  if(!This) throw wxe_badarg("This");
  bool Result = This->HasFont();
  wxeReturn rt = wxeReturn(memenv, Ecmd.caller, true);
  rt.send(  rt.make_bool(Result));

}

// wxGridCellAttr::HasAlignment
void wxGridCellAttr_HasAlignment(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd)
{
  ErlNifEnv *env = Ecmd.env;
  ERL_NIF_TERM * argv = Ecmd.args;
  wxGridCellAttr *This;
  This = (wxGridCellAttr *) memenv->getPtr(env, argv[0], "This");
  if(!This) throw wxe_badarg("This");
  bool Result = This->HasAlignment();
  wxeReturn rt = wxeReturn(memenv, Ecmd.caller, true);
  rt.send(  rt.make_bool(Result));

}

// wxGridCellAttr::HasRenderer
void wxGridCellAttr_HasRenderer(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd)
{
  ErlNifEnv *env = Ecmd.env;
  ERL_NIF_TERM * argv = Ecmd.args;
  wxGridCellAttr *This;
  This = (wxGridCellAttr *) memenv->getPtr(env, argv[0], "This");
  if(!This) throw wxe_badarg("This");
  bool Result = This->HasRenderer();
  wxeReturn rt = wxeReturn(memenv, Ecmd.caller, true);
  rt.send(  rt.make_bool(Result));

}

// wxGridCellAttr::HasEditor
void wxGridCellAttr_HasEditor(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd)
{
  ErlNifEnv *env = Ecmd.env;
  ERL_NIF_TERM * argv = Ecmd.args;
  wxGridCellAttr *This;
  This = (wxGridCellAttr *) memenv->getPtr(env, argv[0], "This");
  if(!This) throw wxe_badarg("This");
  bool Result = This->HasEditor();
  wxeReturn rt = wxeReturn(memenv, Ecmd.caller, true);
  rt.send(  rt.make_bool(Result));

}

// wxGridCellAttr::GetTextColour
void wxGridCellAttr_GetTextColour(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd)
{
  ErlNifEnv *env = Ecmd.env;
  ERL_NIF_TERM * argv = Ecmd.args;
  wxGridCellAttr *This;
  This = (wxGridCellAttr *) memenv->getPtr(env, argv[0], "This");
  if(!This) throw wxe_badarg("This");
  const wxColour * Result = &This->GetTextColour();
  wxeReturn rt = wxeReturn(memenv, Ecmd.caller, true);
  rt.send(  rt.make((*Result)));

}

// wxGridCellAttr::GetBackgroundColour
void wxGridCellAttr_GetBackgroundColour(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd)
{
  ErlNifEnv *env = Ecmd.env;
  ERL_NIF_TERM * argv = Ecmd.args;
  wxGridCellAttr *This;
  This = (wxGridCellAttr *) memenv->getPtr(env, argv[0], "This");
  if(!This) throw wxe_badarg("This");
  const wxColour * Result = &This->GetBackgroundColour();
  wxeReturn rt = wxeReturn(memenv, Ecmd.caller, true);
  rt.send(  rt.make((*Result)));

}

// wxGridCellAttr::GetFont
void wxGridCellAttr_GetFont(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd)
{
  ErlNifEnv *env = Ecmd.env;
  ERL_NIF_TERM * argv = Ecmd.args;
  wxGridCellAttr *This;
  This = (wxGridCellAttr *) memenv->getPtr(env, argv[0], "This");
  if(!This) throw wxe_badarg("This");
  const wxFont * Result = &This->GetFont();
  wxeReturn rt = wxeReturn(memenv, Ecmd.caller, true);
  rt.send(  rt.make_ref(app->getRef((void *)Result,memenv), "wxFont"));

}

// wxGridCellAttr::GetAlignment
void wxGridCellAttr_GetAlignment(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd)
{
  int hAlign;
  int vAlign;
  ErlNifEnv *env = Ecmd.env;
  ERL_NIF_TERM * argv = Ecmd.args;
  wxGridCellAttr *This;
  This = (wxGridCellAttr *) memenv->getPtr(env, argv[0], "This");
  if(!This) throw wxe_badarg("This");
  This->GetAlignment(&hAlign,&vAlign);
  wxeReturn rt = wxeReturn(memenv, Ecmd.caller, true);
  ERL_NIF_TERM msg = enif_make_tuple2(rt.env,
  rt.make_int(hAlign),
  rt.make_int(vAlign));
  rt.send(msg);

}

// wxGridCellAttr::GetRenderer
void wxGridCellAttr_GetRenderer(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd)
{
  ErlNifEnv *env = Ecmd.env;
  ERL_NIF_TERM * argv = Ecmd.args;
  wxGridCellAttr *This;
  This = (wxGridCellAttr *) memenv->getPtr(env, argv[0], "This");
  wxGrid *grid;
  grid = (wxGrid *) memenv->getPtr(env, argv[1], "grid");
  int row;
  if(!enif_get_int(env, argv[2], &row)) Badarg("row"); // int
  int col;
  if(!enif_get_int(env, argv[3], &col)) Badarg("col"); // int
  if(!This) throw wxe_badarg("This");
  wxGridCellRenderer * Result = (wxGridCellRenderer*)This->GetRenderer(grid,row,col);
  wxeReturn rt = wxeReturn(memenv, Ecmd.caller, true);
  rt.send(  rt.make_ref(app->getRef((void *)Result,memenv), "wxGridCellRenderer"));

}

// wxGridCellAttr::GetEditor
void wxGridCellAttr_GetEditor(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd)
{
  ErlNifEnv *env = Ecmd.env;
  ERL_NIF_TERM * argv = Ecmd.args;
  wxGridCellAttr *This;
  This = (wxGridCellAttr *) memenv->getPtr(env, argv[0], "This");
  wxGrid *grid;
  grid = (wxGrid *) memenv->getPtr(env, argv[1], "grid");
  int row;
  if(!enif_get_int(env, argv[2], &row)) Badarg("row"); // int
  int col;
  if(!enif_get_int(env, argv[3], &col)) Badarg("col"); // int
  if(!This) throw wxe_badarg("This");
  wxGridCellEditor * Result = (wxGridCellEditor*)This->GetEditor(grid,row,col);
  wxeReturn rt = wxeReturn(memenv, Ecmd.caller, true);
  rt.send(  rt.make_ref(app->getRef((void *)Result,memenv), "wxGridCellEditor"));

}

// wxGridCellAttr::IsReadOnly
void wxGridCellAttr_IsReadOnly(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd)
{
  ErlNifEnv *env = Ecmd.env;
  ERL_NIF_TERM * argv = Ecmd.args;
  wxGridCellAttr *This;
  This = (wxGridCellAttr *) memenv->getPtr(env, argv[0], "This");
  if(!This) throw wxe_badarg("This");
  bool Result = This->IsReadOnly();
  wxeReturn rt = wxeReturn(memenv, Ecmd.caller, true);
  rt.send(  rt.make_bool(Result));

}

// wxGridCellAttr::SetDefAttr
void wxGridCellAttr_SetDefAttr(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd)
{
  ErlNifEnv *env = Ecmd.env;
  ERL_NIF_TERM * argv = Ecmd.args;
  wxGridCellAttr *This;
  This = (wxGridCellAttr *) memenv->getPtr(env, argv[0], "This");
  wxGridCellAttr *defAttr;
  defAttr = (wxGridCellAttr *) memenv->getPtr(env, argv[1], "defAttr");
  if(!This) throw wxe_badarg("This");
  This->SetDefAttr(defAttr);

}

// wxGridCellBoolEditor::wxGridCellBoolEditor
void wxGridCellBoolEditor_new(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd)
{
  wxGridCellBoolEditor * Result = new wxGridCellBoolEditor();
  app->newPtr((void *) Result, 25, memenv);
  wxeReturn rt = wxeReturn(memenv, Ecmd.caller, true);
  rt.send(  rt.make_ref(app->getRef((void *)Result,memenv), "wxGridCellBoolEditor"));

}

// wxGridCellBoolEditor::IsTrueValue
void wxGridCellBoolEditor_IsTrueValue(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd)
{
  ErlNifEnv *env = Ecmd.env;
  ERL_NIF_TERM * argv = Ecmd.args;
  ErlNifBinary value_bin;
  wxString value;
  if(!enif_inspect_binary(env, argv[0], &value_bin)) Badarg("value");
  value = wxString(value_bin.data, wxConvUTF8, value_bin.size);
  bool Result = wxGridCellBoolEditor::IsTrueValue(value);
  wxeReturn rt = wxeReturn(memenv, Ecmd.caller, true);
  rt.send(  rt.make_bool(Result));

}

// wxGridCellBoolEditor::UseStringValues
void wxGridCellBoolEditor_UseStringValues(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd)
{
  wxString valueTrue= "1";
  wxString valueFalse= wxEmptyString;
  ErlNifEnv *env = Ecmd.env;
  ERL_NIF_TERM * argv = Ecmd.args;
  ERL_NIF_TERM lstHead, lstTail;
  lstTail = argv[0];
  if(!enif_is_list(env, lstTail)) Badarg("Options");
  const ERL_NIF_TERM *tpl;
  int tpl_sz;
  while(!enif_is_empty_list(env, lstTail)) {
    if(!enif_get_list_cell(env, lstTail, &lstHead, &lstTail)) Badarg("Options");
    if(!enif_get_tuple(env, lstHead, &tpl_sz, &tpl) || tpl_sz != 2) Badarg("Options");
    if(enif_is_identical(tpl[0], enif_make_atom(env, "valueTrue"))) {
  ErlNifBinary valueTrue_bin;
  if(!enif_inspect_binary(env, tpl[1], &valueTrue_bin)) Badarg("valueTrue");
  valueTrue = wxString(valueTrue_bin.data, wxConvUTF8, valueTrue_bin.size);
    } else     if(enif_is_identical(tpl[0], enif_make_atom(env, "valueFalse"))) {
  ErlNifBinary valueFalse_bin;
  if(!enif_inspect_binary(env, tpl[1], &valueFalse_bin)) Badarg("valueFalse");
  valueFalse = wxString(valueFalse_bin.data, wxConvUTF8, valueFalse_bin.size);
    } else        Badarg("Options");
  };
  wxGridCellBoolEditor::UseStringValues(valueTrue,valueFalse);

}

// wxGridCellBoolEditor::destroy
void wxGridCellBoolEditor_destroy(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd)
{
 ErlNifEnv *env = Ecmd.env;
 ERL_NIF_TERM * argv = Ecmd.args;
  wxGridCellBoolEditor *This;
  This = (wxGridCellBoolEditor *) memenv->getPtr(env, argv[0], "This");
 if(This) {   ((WxeApp *) wxTheApp)->clearPtr((void *) This);
   delete This;}
}

// wxGridCellBoolRenderer::wxGridCellBoolRenderer
void wxGridCellBoolRenderer_new(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd)
{
  wxGridCellBoolRenderer * Result = new wxGridCellBoolRenderer();
  app->newPtr((void *) Result, 24, memenv);
  wxeReturn rt = wxeReturn(memenv, Ecmd.caller, true);
  rt.send(  rt.make_ref(app->getRef((void *)Result,memenv), "wxGridCellBoolRenderer"));

}

// wxGridCellBoolRenderer::destroy
void wxGridCellBoolRenderer_destroy(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd)
{
 ErlNifEnv *env = Ecmd.env;
 ERL_NIF_TERM * argv = Ecmd.args;
  wxGridCellBoolRenderer *This;
  This = (wxGridCellBoolRenderer *) memenv->getPtr(env, argv[0], "This");
 if(This) {   ((WxeApp *) wxTheApp)->clearPtr((void *) This);
   delete This;}
}

// wxGridCellChoiceEditor::wxGridCellChoiceEditor
void wxGridCellChoiceEditor_new(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd)
{
  bool allowOthers=false;
  ErlNifEnv *env = Ecmd.env;
  ERL_NIF_TERM * argv = Ecmd.args;
  ERL_NIF_TERM choicesHead, choicesTail;
  ErlNifBinary choices_bin;
  wxArrayString choices;
  choicesTail = argv[0];
  while(!enif_is_empty_list(env, choicesTail)) {
    if(!enif_get_list_cell(env, choicesTail, &choicesHead, &choicesTail)) Badarg("choices");
    if(!enif_inspect_binary(env, choicesHead, &choices_bin)) Badarg("choices");
    choices.Add(wxString(choices_bin.data, wxConvUTF8, choices_bin.size));
  };
  ERL_NIF_TERM lstHead, lstTail;
  lstTail = argv[1];
  if(!enif_is_list(env, lstTail)) Badarg("Options");
  const ERL_NIF_TERM *tpl;
  int tpl_sz;
  while(!enif_is_empty_list(env, lstTail)) {
    if(!enif_get_list_cell(env, lstTail, &lstHead, &lstTail)) Badarg("Options");
    if(!enif_get_tuple(env, lstHead, &tpl_sz, &tpl) || tpl_sz != 2) Badarg("Options");
    if(enif_is_identical(tpl[0], enif_make_atom(env, "allowOthers"))) {
  allowOthers = enif_is_identical(tpl[1], WXE_ATOM_true);
    } else        Badarg("Options");
  };
  wxGridCellChoiceEditor * Result = new wxGridCellChoiceEditor(choices,allowOthers);
  app->newPtr((void *) Result, 30, memenv);
  wxeReturn rt = wxeReturn(memenv, Ecmd.caller, true);
  rt.send(  rt.make_ref(app->getRef((void *)Result,memenv), "wxGridCellChoiceEditor"));

}

// wxGridCellChoiceEditor::SetParameters
void wxGridCellChoiceEditor_SetParameters(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd)
{
  ErlNifEnv *env = Ecmd.env;
  ERL_NIF_TERM * argv = Ecmd.args;
  wxGridCellChoiceEditor *This;
  This = (wxGridCellChoiceEditor *) memenv->getPtr(env, argv[0], "This");
  ErlNifBinary params_bin;
  wxString params;
  if(!enif_inspect_binary(env, argv[1], &params_bin)) Badarg("params");
  params = wxString(params_bin.data, wxConvUTF8, params_bin.size);
  if(!This) throw wxe_badarg("This");
  This->SetParameters(params);

}

// wxGridCellChoiceEditor::destroy
void wxGridCellChoiceEditor_destroy(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd)
{
 ErlNifEnv *env = Ecmd.env;
 ERL_NIF_TERM * argv = Ecmd.args;
  wxGridCellChoiceEditor *This;
  This = (wxGridCellChoiceEditor *) memenv->getPtr(env, argv[0], "This");
 if(This) {   ((WxeApp *) wxTheApp)->clearPtr((void *) This);
   delete This;}
}

// wxGridCellEditor::Create
void wxGridCellEditor_Create(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd)
{
  ErlNifEnv *env = Ecmd.env;
  ERL_NIF_TERM * argv = Ecmd.args;
  wxGridCellEditor *This;
  This = (wxGridCellEditor *) memenv->getPtr(env, argv[0], "This");
  wxWindow *parent;
  parent = (wxWindow *) memenv->getPtr(env, argv[1], "parent");
  int id;
  if(!enif_get_int(env, argv[2], &id)) Badarg("id"); // wxWindowID
  wxEvtHandler *evtHandler;
  evtHandler = (wxEvtHandler *) memenv->getPtr(env, argv[3], "evtHandler");
  if(!This) throw wxe_badarg("This");
  This->Create(parent,id,evtHandler);

}

// wxGridCellEditor::IsCreated
void wxGridCellEditor_IsCreated(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd)
{
  ErlNifEnv *env = Ecmd.env;
  ERL_NIF_TERM * argv = Ecmd.args;
  wxGridCellEditor *This;
  This = (wxGridCellEditor *) memenv->getPtr(env, argv[0], "This");
  if(!This) throw wxe_badarg("This");
  bool Result = This->IsCreated();
  wxeReturn rt = wxeReturn(memenv, Ecmd.caller, true);
  rt.send(  rt.make_bool(Result));

}

// wxGridCellEditor::SetSize
void wxGridCellEditor_SetSize(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd)
{
  ErlNifEnv *env = Ecmd.env;
  ERL_NIF_TERM * argv = Ecmd.args;
  wxGridCellEditor *This;
  This = (wxGridCellEditor *) memenv->getPtr(env, argv[0], "This");
  const ERL_NIF_TERM *rect_t;
  int rect_sz;
  if(!enif_get_tuple(env, argv[1], &rect_sz, &rect_t)) Badarg("rect");
  int rectX;
  if(!enif_get_int(env, rect_t[0], &rectX)) Badarg("rect");
  int rectY;
  if(!enif_get_int(env, rect_t[1], &rectY)) Badarg("rect");
  int rectW;
  if(!enif_get_int(env, rect_t[2], &rectW)) Badarg("rect");
  int rectH;
  if(!enif_get_int(env, rect_t[3], &rectH)) Badarg("rect");
  wxRect rect = wxRect(rectX,rectY,rectW,rectH);
  if(!This) throw wxe_badarg("This");
  This->SetSize(rect);

}

// wxGridCellEditor::Show
void wxGridCellEditor_Show(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd)
{
  wxGridCellAttr * attr=NULL;
  ErlNifEnv *env = Ecmd.env;
  ERL_NIF_TERM * argv = Ecmd.args;
  wxGridCellEditor *This;
  This = (wxGridCellEditor *) memenv->getPtr(env, argv[0], "This");
  bool show;
  show = enif_is_identical(argv[1], WXE_ATOM_true);
  ERL_NIF_TERM lstHead, lstTail;
  lstTail = argv[2];
  if(!enif_is_list(env, lstTail)) Badarg("Options");
  const ERL_NIF_TERM *tpl;
  int tpl_sz;
  while(!enif_is_empty_list(env, lstTail)) {
    if(!enif_get_list_cell(env, lstTail, &lstHead, &lstTail)) Badarg("Options");
    if(!enif_get_tuple(env, lstHead, &tpl_sz, &tpl) || tpl_sz != 2) Badarg("Options");
    if(enif_is_identical(tpl[0], enif_make_atom(env, "attr"))) {
  attr = (wxGridCellAttr *) memenv->getPtr(env, tpl[1], "attr");
    } else        Badarg("Options");
  };
  if(!This) throw wxe_badarg("This");
  This->Show(show,attr);

}

// wxGridCellEditor::Reset
void wxGridCellEditor_Reset(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd)
{
  ErlNifEnv *env = Ecmd.env;
  ERL_NIF_TERM * argv = Ecmd.args;
  wxGridCellEditor *This;
  This = (wxGridCellEditor *) memenv->getPtr(env, argv[0], "This");
  if(!This) throw wxe_badarg("This");
  This->Reset();

}

// wxGridCellEditor::StartingKey
void wxGridCellEditor_StartingKey(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd)
{
  ErlNifEnv *env = Ecmd.env;
  ERL_NIF_TERM * argv = Ecmd.args;
  wxGridCellEditor *This;
  This = (wxGridCellEditor *) memenv->getPtr(env, argv[0], "This");
  wxKeyEvent *event;
  event = (wxKeyEvent *) memenv->getPtr(env, argv[1], "event");
  if(!This) throw wxe_badarg("This");
  This->StartingKey(*event);

}

// wxGridCellEditor::StartingClick
void wxGridCellEditor_StartingClick(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd)
{
  ErlNifEnv *env = Ecmd.env;
  ERL_NIF_TERM * argv = Ecmd.args;
  wxGridCellEditor *This;
  This = (wxGridCellEditor *) memenv->getPtr(env, argv[0], "This");
  if(!This) throw wxe_badarg("This");
  This->StartingClick();

}

// wxGridCellEditor::HandleReturn
void wxGridCellEditor_HandleReturn(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd)
{
  ErlNifEnv *env = Ecmd.env;
  ERL_NIF_TERM * argv = Ecmd.args;
  wxGridCellEditor *This;
  This = (wxGridCellEditor *) memenv->getPtr(env, argv[0], "This");
  wxKeyEvent *event;
  event = (wxKeyEvent *) memenv->getPtr(env, argv[1], "event");
  if(!This) throw wxe_badarg("This");
  This->HandleReturn(*event);

}

// wxGridCellFloatEditor::wxGridCellFloatEditor
void wxGridCellFloatEditor_new(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd)
{
  int width=-1;
  int precision=-1;
  int format=wxGRID_FLOAT_FORMAT_DEFAULT;
  ErlNifEnv *env = Ecmd.env;
  ERL_NIF_TERM * argv = Ecmd.args;
  ERL_NIF_TERM lstHead, lstTail;
  lstTail = argv[0];
  if(!enif_is_list(env, lstTail)) Badarg("Options");
  const ERL_NIF_TERM *tpl;
  int tpl_sz;
  while(!enif_is_empty_list(env, lstTail)) {
    if(!enif_get_list_cell(env, lstTail, &lstHead, &lstTail)) Badarg("Options");
    if(!enif_get_tuple(env, lstHead, &tpl_sz, &tpl) || tpl_sz != 2) Badarg("Options");
    if(enif_is_identical(tpl[0], enif_make_atom(env, "width"))) {
  if(!enif_get_int(env, tpl[1], &width)) Badarg("width"); // int
    } else     if(enif_is_identical(tpl[0], enif_make_atom(env, "precision"))) {
  if(!enif_get_int(env, tpl[1], &precision)) Badarg("precision"); // int
    } else     if(enif_is_identical(tpl[0], enif_make_atom(env, "format"))) {
  if(!enif_get_int(env, tpl[1], &format)) Badarg("format"); // int
    } else        Badarg("Options");
  };
  wxGridCellFloatEditor * Result = new wxGridCellFloatEditor(width,precision,format);
  app->newPtr((void *) Result, 27, memenv);
  wxeReturn rt = wxeReturn(memenv, Ecmd.caller, true);
  rt.send(  rt.make_ref(app->getRef((void *)Result,memenv), "wxGridCellFloatEditor"));

}

// wxGridCellFloatEditor::SetParameters
void wxGridCellFloatEditor_SetParameters(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd)
{
  ErlNifEnv *env = Ecmd.env;
  ERL_NIF_TERM * argv = Ecmd.args;
  wxGridCellFloatEditor *This;
  This = (wxGridCellFloatEditor *) memenv->getPtr(env, argv[0], "This");
  ErlNifBinary params_bin;
  wxString params;
  if(!enif_inspect_binary(env, argv[1], &params_bin)) Badarg("params");
  params = wxString(params_bin.data, wxConvUTF8, params_bin.size);
  if(!This) throw wxe_badarg("This");
  This->SetParameters(params);

}

// wxGridCellFloatEditor::destroy
void wxGridCellFloatEditor_destroy(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd)
{
 ErlNifEnv *env = Ecmd.env;
 ERL_NIF_TERM * argv = Ecmd.args;
  wxGridCellFloatEditor *This;
  This = (wxGridCellFloatEditor *) memenv->getPtr(env, argv[0], "This");
 if(This) {   ((WxeApp *) wxTheApp)->clearPtr((void *) This);
   delete This;}
}

// wxGridCellFloatRenderer::wxGridCellFloatRenderer
void wxGridCellFloatRenderer_new(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd)
{
  int width=-1;
  int precision=-1;
  int format=wxGRID_FLOAT_FORMAT_DEFAULT;
  ErlNifEnv *env = Ecmd.env;
  ERL_NIF_TERM * argv = Ecmd.args;
  ERL_NIF_TERM lstHead, lstTail;
  lstTail = argv[0];
  if(!enif_is_list(env, lstTail)) Badarg("Options");
  const ERL_NIF_TERM *tpl;
  int tpl_sz;
  while(!enif_is_empty_list(env, lstTail)) {
    if(!enif_get_list_cell(env, lstTail, &lstHead, &lstTail)) Badarg("Options");
    if(!enif_get_tuple(env, lstHead, &tpl_sz, &tpl) || tpl_sz != 2) Badarg("Options");
    if(enif_is_identical(tpl[0], enif_make_atom(env, "width"))) {
  if(!enif_get_int(env, tpl[1], &width)) Badarg("width"); // int
    } else     if(enif_is_identical(tpl[0], enif_make_atom(env, "precision"))) {
  if(!enif_get_int(env, tpl[1], &precision)) Badarg("precision"); // int
    } else     if(enif_is_identical(tpl[0], enif_make_atom(env, "format"))) {
  if(!enif_get_int(env, tpl[1], &format)) Badarg("format"); // int
    } else        Badarg("Options");
  };
  wxGridCellFloatRenderer * Result = new wxGridCellFloatRenderer(width,precision,format);
  app->newPtr((void *) Result, 26, memenv);
  wxeReturn rt = wxeReturn(memenv, Ecmd.caller, true);
  rt.send(  rt.make_ref(app->getRef((void *)Result,memenv), "wxGridCellFloatRenderer"));

}

// wxGridCellFloatRenderer::GetPrecision
void wxGridCellFloatRenderer_GetPrecision(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd)
{
  ErlNifEnv *env = Ecmd.env;
  ERL_NIF_TERM * argv = Ecmd.args;
  wxGridCellFloatRenderer *This;
  This = (wxGridCellFloatRenderer *) memenv->getPtr(env, argv[0], "This");
  if(!This) throw wxe_badarg("This");
  int Result = This->GetPrecision();
  wxeReturn rt = wxeReturn(memenv, Ecmd.caller, true);
  rt.send(  rt.make_int(Result));

}

// wxGridCellFloatRenderer::GetWidth
void wxGridCellFloatRenderer_GetWidth(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd)
{
  ErlNifEnv *env = Ecmd.env;
  ERL_NIF_TERM * argv = Ecmd.args;
  wxGridCellFloatRenderer *This;
  This = (wxGridCellFloatRenderer *) memenv->getPtr(env, argv[0], "This");
  if(!This) throw wxe_badarg("This");
  int Result = This->GetWidth();
  wxeReturn rt = wxeReturn(memenv, Ecmd.caller, true);
  rt.send(  rt.make_int(Result));

}

// wxGridCellFloatRenderer::SetParameters
void wxGridCellFloatRenderer_SetParameters(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd)
{
  ErlNifEnv *env = Ecmd.env;
  ERL_NIF_TERM * argv = Ecmd.args;
  wxGridCellFloatRenderer *This;
  This = (wxGridCellFloatRenderer *) memenv->getPtr(env, argv[0], "This");
  ErlNifBinary params_bin;
  wxString params;
  if(!enif_inspect_binary(env, argv[1], &params_bin)) Badarg("params");
  params = wxString(params_bin.data, wxConvUTF8, params_bin.size);
  if(!This) throw wxe_badarg("This");
  This->SetParameters(params);

}

// wxGridCellFloatRenderer::SetPrecision
void wxGridCellFloatRenderer_SetPrecision(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd)
{
  ErlNifEnv *env = Ecmd.env;
  ERL_NIF_TERM * argv = Ecmd.args;
  wxGridCellFloatRenderer *This;
  This = (wxGridCellFloatRenderer *) memenv->getPtr(env, argv[0], "This");
  int precision;
  if(!enif_get_int(env, argv[1], &precision)) Badarg("precision"); // int
  if(!This) throw wxe_badarg("This");
  This->SetPrecision(precision);

}

// wxGridCellFloatRenderer::SetWidth
void wxGridCellFloatRenderer_SetWidth(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd)
{
  ErlNifEnv *env = Ecmd.env;
  ERL_NIF_TERM * argv = Ecmd.args;
  wxGridCellFloatRenderer *This;
  This = (wxGridCellFloatRenderer *) memenv->getPtr(env, argv[0], "This");
  int width;
  if(!enif_get_int(env, argv[1], &width)) Badarg("width"); // int
  if(!This) throw wxe_badarg("This");
  This->SetWidth(width);

}

// wxGridCellFloatRenderer::destroy
void wxGridCellFloatRenderer_destroy(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd)
{
 ErlNifEnv *env = Ecmd.env;
 ERL_NIF_TERM * argv = Ecmd.args;
  wxGridCellFloatRenderer *This;
  This = (wxGridCellFloatRenderer *) memenv->getPtr(env, argv[0], "This");
 if(This) {   ((WxeApp *) wxTheApp)->clearPtr((void *) This);
   delete This;}
}

// wxGridCellNumberEditor::wxGridCellNumberEditor
void wxGridCellNumberEditor_new(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd)
{
  int min=-1;
  int max=-1;
  ErlNifEnv *env = Ecmd.env;
  ERL_NIF_TERM * argv = Ecmd.args;
  ERL_NIF_TERM lstHead, lstTail;
  lstTail = argv[0];
  if(!enif_is_list(env, lstTail)) Badarg("Options");
  const ERL_NIF_TERM *tpl;
  int tpl_sz;
  while(!enif_is_empty_list(env, lstTail)) {
    if(!enif_get_list_cell(env, lstTail, &lstHead, &lstTail)) Badarg("Options");
    if(!enif_get_tuple(env, lstHead, &tpl_sz, &tpl) || tpl_sz != 2) Badarg("Options");
    if(enif_is_identical(tpl[0], enif_make_atom(env, "min"))) {
  if(!enif_get_int(env, tpl[1], &min)) Badarg("min"); // int
    } else     if(enif_is_identical(tpl[0], enif_make_atom(env, "max"))) {
  if(!enif_get_int(env, tpl[1], &max)) Badarg("max"); // int
    } else        Badarg("Options");
  };
  wxGridCellNumberEditor * Result = new wxGridCellNumberEditor(min,max);
  app->newPtr((void *) Result, 32, memenv);
  wxeReturn rt = wxeReturn(memenv, Ecmd.caller, true);
  rt.send(  rt.make_ref(app->getRef((void *)Result,memenv), "wxGridCellNumberEditor"));

}

// wxGridCellNumberEditor::GetValue
void wxGridCellNumberEditor_GetValue(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd)
{
  ErlNifEnv *env = Ecmd.env;
  ERL_NIF_TERM * argv = Ecmd.args;
  wxGridCellNumberEditor *This;
  This = (wxGridCellNumberEditor *) memenv->getPtr(env, argv[0], "This");
  if(!This) throw wxe_badarg("This");
  wxString Result = This->GetValue();
  wxeReturn rt = wxeReturn(memenv, Ecmd.caller, true);
  rt.send(  rt.make(Result));

}

// wxGridCellNumberEditor::SetParameters
void wxGridCellNumberEditor_SetParameters(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd)
{
  ErlNifEnv *env = Ecmd.env;
  ERL_NIF_TERM * argv = Ecmd.args;
  wxGridCellNumberEditor *This;
  This = (wxGridCellNumberEditor *) memenv->getPtr(env, argv[0], "This");
  ErlNifBinary params_bin;
  wxString params;
  if(!enif_inspect_binary(env, argv[1], &params_bin)) Badarg("params");
  params = wxString(params_bin.data, wxConvUTF8, params_bin.size);
  if(!This) throw wxe_badarg("This");
  This->SetParameters(params);

}

// wxGridCellNumberEditor::destroy
void wxGridCellNumberEditor_destroy(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd)
{
 ErlNifEnv *env = Ecmd.env;
 ERL_NIF_TERM * argv = Ecmd.args;
  wxGridCellNumberEditor *This;
  This = (wxGridCellNumberEditor *) memenv->getPtr(env, argv[0], "This");
 if(This) {   ((WxeApp *) wxTheApp)->clearPtr((void *) This);
   delete This;}
}

// wxGridCellNumberRenderer::wxGridCellNumberRenderer
void wxGridCellNumberRenderer_new(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd)
{
  wxGridCellNumberRenderer * Result = new wxGridCellNumberRenderer();
  app->newPtr((void *) Result, 31, memenv);
  wxeReturn rt = wxeReturn(memenv, Ecmd.caller, true);
  rt.send(  rt.make_ref(app->getRef((void *)Result,memenv), "wxGridCellNumberRenderer"));

}

// wxGridCellNumberRenderer::destroy
void wxGridCellNumberRenderer_destroy(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd)
{
 ErlNifEnv *env = Ecmd.env;
 ERL_NIF_TERM * argv = Ecmd.args;
  wxGridCellNumberRenderer *This;
  This = (wxGridCellNumberRenderer *) memenv->getPtr(env, argv[0], "This");
 if(This) {   ((WxeApp *) wxTheApp)->clearPtr((void *) This);
   delete This;}
}

// wxGridCellRenderer::Draw
void wxGridCellRenderer_Draw(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd)
{
  ErlNifEnv *env = Ecmd.env;
  ERL_NIF_TERM * argv = Ecmd.args;
  wxGridCellRenderer *This;
  This = (wxGridCellRenderer *) memenv->getPtr(env, argv[0], "This");
  wxGrid *grid;
  grid = (wxGrid *) memenv->getPtr(env, argv[1], "grid");
  wxGridCellAttr *attr;
  attr = (wxGridCellAttr *) memenv->getPtr(env, argv[2], "attr");
  wxDC *dc;
  dc = (wxDC *) memenv->getPtr(env, argv[3], "dc");
  const ERL_NIF_TERM *rect_t;
  int rect_sz;
  if(!enif_get_tuple(env, argv[4], &rect_sz, &rect_t)) Badarg("rect");
  int rectX;
  if(!enif_get_int(env, rect_t[0], &rectX)) Badarg("rect");
  int rectY;
  if(!enif_get_int(env, rect_t[1], &rectY)) Badarg("rect");
  int rectW;
  if(!enif_get_int(env, rect_t[2], &rectW)) Badarg("rect");
  int rectH;
  if(!enif_get_int(env, rect_t[3], &rectH)) Badarg("rect");
  wxRect rect = wxRect(rectX,rectY,rectW,rectH);
  int row;
  if(!enif_get_int(env, argv[5], &row)) Badarg("row"); // int
  int col;
  if(!enif_get_int(env, argv[6], &col)) Badarg("col"); // int
  bool isSelected;
  isSelected = enif_is_identical(argv[7], WXE_ATOM_true);
  if(!This) throw wxe_badarg("This");
  This->Draw(*grid,*attr,*dc,rect,row,col,isSelected);

}

// wxGridCellRenderer::GetBestSize
void wxGridCellRenderer_GetBestSize(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd)
{
  ErlNifEnv *env = Ecmd.env;
  ERL_NIF_TERM * argv = Ecmd.args;
  wxGridCellRenderer *This;
  This = (wxGridCellRenderer *) memenv->getPtr(env, argv[0], "This");
  wxGrid *grid;
  grid = (wxGrid *) memenv->getPtr(env, argv[1], "grid");
  wxGridCellAttr *attr;
  attr = (wxGridCellAttr *) memenv->getPtr(env, argv[2], "attr");
  wxDC *dc;
  dc = (wxDC *) memenv->getPtr(env, argv[3], "dc");
  int row;
  if(!enif_get_int(env, argv[4], &row)) Badarg("row"); // int
  int col;
  if(!enif_get_int(env, argv[5], &col)) Badarg("col"); // int
  if(!This) throw wxe_badarg("This");
  wxSize Result = This->GetBestSize(*grid,*attr,*dc,row,col);
  wxeReturn rt = wxeReturn(memenv, Ecmd.caller, true);
  rt.send(  rt.make(Result));

}

// wxGridCellStringRenderer::wxGridCellStringRenderer
void wxGridCellStringRenderer_new(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd)
{
  wxGridCellStringRenderer * Result = new wxGridCellStringRenderer();
  app->newPtr((void *) Result, 28, memenv);
  wxeReturn rt = wxeReturn(memenv, Ecmd.caller, true);
  rt.send(  rt.make_ref(app->getRef((void *)Result,memenv), "wxGridCellStringRenderer"));

}

// wxGridCellStringRenderer::destroy
void wxGridCellStringRenderer_destroy(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd)
{
 ErlNifEnv *env = Ecmd.env;
 ERL_NIF_TERM * argv = Ecmd.args;
  wxGridCellStringRenderer *This;
  This = (wxGridCellStringRenderer *) memenv->getPtr(env, argv[0], "This");
 if(This) {   ((WxeApp *) wxTheApp)->clearPtr((void *) This);
   delete This;}
}

// wxGridCellTextEditor::wxGridCellTextEditor
void wxGridCellTextEditor_new(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd)
{
  size_t maxChars=0;
  ErlNifEnv *env = Ecmd.env;
  ERL_NIF_TERM * argv = Ecmd.args;
  ERL_NIF_TERM lstHead, lstTail;
  lstTail = argv[0];
  if(!enif_is_list(env, lstTail)) Badarg("Options");
  const ERL_NIF_TERM *tpl;
  int tpl_sz;
  while(!enif_is_empty_list(env, lstTail)) {
    if(!enif_get_list_cell(env, lstTail, &lstHead, &lstTail)) Badarg("Options");
    if(!enif_get_tuple(env, lstHead, &tpl_sz, &tpl) || tpl_sz != 2) Badarg("Options");
    if(enif_is_identical(tpl[0], enif_make_atom(env, "maxChars"))) {
  if(!wxe_get_size_t(env, tpl[1], &maxChars)) Badarg("maxChars");
    } else        Badarg("Options");
  };
  wxGridCellTextEditor * Result = new wxGridCellTextEditor(maxChars);
  app->newPtr((void *) Result, 29, memenv);
  wxeReturn rt = wxeReturn(memenv, Ecmd.caller, true);
  rt.send(  rt.make_ref(app->getRef((void *)Result,memenv), "wxGridCellTextEditor"));

}

// wxGridCellTextEditor::SetParameters
void wxGridCellTextEditor_SetParameters(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd)
{
  ErlNifEnv *env = Ecmd.env;
  ERL_NIF_TERM * argv = Ecmd.args;
  wxGridCellTextEditor *This;
  This = (wxGridCellTextEditor *) memenv->getPtr(env, argv[0], "This");
  ErlNifBinary params_bin;
  wxString params;
  if(!enif_inspect_binary(env, argv[1], &params_bin)) Badarg("params");
  params = wxString(params_bin.data, wxConvUTF8, params_bin.size);
  if(!This) throw wxe_badarg("This");
  This->SetParameters(params);

}

// wxGridCellTextEditor::destroy
void wxGridCellTextEditor_destroy(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd)
{
 ErlNifEnv *env = Ecmd.env;
 ERL_NIF_TERM * argv = Ecmd.args;
  wxGridCellTextEditor *This;
  This = (wxGridCellTextEditor *) memenv->getPtr(env, argv[0], "This");
 if(This) {   ((WxeApp *) wxTheApp)->clearPtr((void *) This);
   delete This;}
}

// wxGridEvent::AltDown
void wxGridEvent_AltDown(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd)
{
  ErlNifEnv *env = Ecmd.env;
  ERL_NIF_TERM * argv = Ecmd.args;
  wxGridEvent *This;
  This = (wxGridEvent *) memenv->getPtr(env, argv[0], "This");
  if(!This) throw wxe_badarg("This");
  bool Result = This->AltDown();
  wxeReturn rt = wxeReturn(memenv, Ecmd.caller, true);
  rt.send(  rt.make_bool(Result));

}

// wxGridEvent::ControlDown
void wxGridEvent_ControlDown(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd)
{
  ErlNifEnv *env = Ecmd.env;
  ERL_NIF_TERM * argv = Ecmd.args;
  wxGridEvent *This;
  This = (wxGridEvent *) memenv->getPtr(env, argv[0], "This");
  if(!This) throw wxe_badarg("This");
  bool Result = This->ControlDown();
  wxeReturn rt = wxeReturn(memenv, Ecmd.caller, true);
  rt.send(  rt.make_bool(Result));

}

// wxGridEvent::GetCol
void wxGridEvent_GetCol(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd)
{
  ErlNifEnv *env = Ecmd.env;
  ERL_NIF_TERM * argv = Ecmd.args;
  wxGridEvent *This;
  This = (wxGridEvent *) memenv->getPtr(env, argv[0], "This");
  if(!This) throw wxe_badarg("This");
  int Result = This->GetCol();
  wxeReturn rt = wxeReturn(memenv, Ecmd.caller, true);
  rt.send(  rt.make_int(Result));

}

// wxGridEvent::GetPosition
void wxGridEvent_GetPosition(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd)
{
  ErlNifEnv *env = Ecmd.env;
  ERL_NIF_TERM * argv = Ecmd.args;
  wxGridEvent *This;
  This = (wxGridEvent *) memenv->getPtr(env, argv[0], "This");
  if(!This) throw wxe_badarg("This");
  wxPoint Result = This->GetPosition();
  wxeReturn rt = wxeReturn(memenv, Ecmd.caller, true);
  rt.send(  rt.make(Result));

}

// wxGridEvent::GetRow
void wxGridEvent_GetRow(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd)
{
  ErlNifEnv *env = Ecmd.env;
  ERL_NIF_TERM * argv = Ecmd.args;
  wxGridEvent *This;
  This = (wxGridEvent *) memenv->getPtr(env, argv[0], "This");
  if(!This) throw wxe_badarg("This");
  int Result = This->GetRow();
  wxeReturn rt = wxeReturn(memenv, Ecmd.caller, true);
  rt.send(  rt.make_int(Result));

}

// wxGridEvent::MetaDown
void wxGridEvent_MetaDown(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd)
{
  ErlNifEnv *env = Ecmd.env;
  ERL_NIF_TERM * argv = Ecmd.args;
  wxGridEvent *This;
  This = (wxGridEvent *) memenv->getPtr(env, argv[0], "This");
  if(!This) throw wxe_badarg("This");
  bool Result = This->MetaDown();
  wxeReturn rt = wxeReturn(memenv, Ecmd.caller, true);
  rt.send(  rt.make_bool(Result));

}

// wxGridEvent::Selecting
void wxGridEvent_Selecting(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd)
{
  ErlNifEnv *env = Ecmd.env;
  ERL_NIF_TERM * argv = Ecmd.args;
  wxGridEvent *This;
  This = (wxGridEvent *) memenv->getPtr(env, argv[0], "This");
  if(!This) throw wxe_badarg("This");
  bool Result = This->Selecting();
  wxeReturn rt = wxeReturn(memenv, Ecmd.caller, true);
  rt.send(  rt.make_bool(Result));

}

// wxGridEvent::ShiftDown
void wxGridEvent_ShiftDown(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd)
{
  ErlNifEnv *env = Ecmd.env;
  ERL_NIF_TERM * argv = Ecmd.args;
  wxGridEvent *This;
  This = (wxGridEvent *) memenv->getPtr(env, argv[0], "This");
  if(!This) throw wxe_badarg("This");
  bool Result = This->ShiftDown();
  wxeReturn rt = wxeReturn(memenv, Ecmd.caller, true);
  rt.send(  rt.make_bool(Result));

}

// wxGridSizer::wxGridSizer
void wxGridSizer_new_3_0(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd)
{
  ErlNifEnv *env = Ecmd.env;
  ERL_NIF_TERM * argv = Ecmd.args;
  int cols;
  if(!enif_get_int(env, argv[0], &cols)) Badarg("cols"); // int
  int vgap;
  if(!enif_get_int(env, argv[1], &vgap)) Badarg("vgap"); // int
  int hgap;
  if(!enif_get_int(env, argv[2], &hgap)) Badarg("hgap"); // int
  wxGridSizer * Result = new EwxGridSizer(cols,vgap,hgap);
  app->newPtr((void *) Result, 1, memenv);
  wxeReturn rt = wxeReturn(memenv, Ecmd.caller, true);
  rt.send(  rt.make_ref(app->getRef((void *)Result,memenv), "wxGridSizer"));

}

// wxGridSizer::wxGridSizer
void wxGridSizer_new_2(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd)
{
  wxSize gap= wxSize(0, 0);
  ErlNifEnv *env = Ecmd.env;
  ERL_NIF_TERM * argv = Ecmd.args;
  int cols;
  if(!enif_get_int(env, argv[0], &cols)) Badarg("cols"); // int
  ERL_NIF_TERM lstHead, lstTail;
  lstTail = argv[1];
  if(!enif_is_list(env, lstTail)) Badarg("Options");
  const ERL_NIF_TERM *tpl;
  int tpl_sz;
  while(!enif_is_empty_list(env, lstTail)) {
    if(!enif_get_list_cell(env, lstTail, &lstHead, &lstTail)) Badarg("Options");
    if(!enif_get_tuple(env, lstHead, &tpl_sz, &tpl) || tpl_sz != 2) Badarg("Options");
    if(enif_is_identical(tpl[0], enif_make_atom(env, "gap"))) {
  const ERL_NIF_TERM *gap_t;
  int gap_sz;
  if(!enif_get_tuple(env, tpl[1], &gap_sz, &gap_t)) Badarg("gap");
  int gapW;
  if(!enif_get_int(env, gap_t[0], &gapW)) Badarg("gap");
  int gapH;
  if(!enif_get_int(env, gap_t[1], &gapH)) Badarg("gap");
  gap = wxSize(gapW,gapH);
    } else        Badarg("Options");
  };
  wxGridSizer * Result = new EwxGridSizer(cols,gap);
  app->newPtr((void *) Result, 1, memenv);
  wxeReturn rt = wxeReturn(memenv, Ecmd.caller, true);
  rt.send(  rt.make_ref(app->getRef((void *)Result,memenv), "wxGridSizer"));

}

// wxGridSizer::wxGridSizer
void wxGridSizer_new_4(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd)
{
  ErlNifEnv *env = Ecmd.env;
  ERL_NIF_TERM * argv = Ecmd.args;
  int rows;
  if(!enif_get_int(env, argv[0], &rows)) Badarg("rows"); // int
  int cols;
  if(!enif_get_int(env, argv[1], &cols)) Badarg("cols"); // int
  int vgap;
  if(!enif_get_int(env, argv[2], &vgap)) Badarg("vgap"); // int
  int hgap;
  if(!enif_get_int(env, argv[3], &hgap)) Badarg("hgap"); // int
  wxGridSizer * Result = new EwxGridSizer(rows,cols,vgap,hgap);
  app->newPtr((void *) Result, 1, memenv);
  wxeReturn rt = wxeReturn(memenv, Ecmd.caller, true);
  rt.send(  rt.make_ref(app->getRef((void *)Result,memenv), "wxGridSizer"));

}

// wxGridSizer::wxGridSizer
void wxGridSizer_new_3_1(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd)
{
  ErlNifEnv *env = Ecmd.env;
  ERL_NIF_TERM * argv = Ecmd.args;
  int rows;
  if(!enif_get_int(env, argv[0], &rows)) Badarg("rows"); // int
  int cols;
  if(!enif_get_int(env, argv[1], &cols)) Badarg("cols"); // int
  const ERL_NIF_TERM *gap_t;
  int gap_sz;
  if(!enif_get_tuple(env, argv[2], &gap_sz, &gap_t)) Badarg("gap");
  int gapW;
  if(!enif_get_int(env, gap_t[0], &gapW)) Badarg("gap");
  int gapH;
  if(!enif_get_int(env, gap_t[1], &gapH)) Badarg("gap");
  wxSize gap = wxSize(gapW,gapH);
  wxGridSizer * Result = new EwxGridSizer(rows,cols,gap);
  app->newPtr((void *) Result, 1, memenv);
  wxeReturn rt = wxeReturn(memenv, Ecmd.caller, true);
  rt.send(  rt.make_ref(app->getRef((void *)Result,memenv), "wxGridSizer"));

}

// wxGridSizer::GetCols
void wxGridSizer_GetCols(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd)
{
  ErlNifEnv *env = Ecmd.env;
  ERL_NIF_TERM * argv = Ecmd.args;
  wxGridSizer *This;
  This = (wxGridSizer *) memenv->getPtr(env, argv[0], "This");
  if(!This) throw wxe_badarg("This");
  int Result = This->GetCols();
  wxeReturn rt = wxeReturn(memenv, Ecmd.caller, true);
  rt.send(  rt.make_int(Result));

}

// wxGridSizer::GetHGap
void wxGridSizer_GetHGap(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd)
{
  ErlNifEnv *env = Ecmd.env;
  ERL_NIF_TERM * argv = Ecmd.args;
  wxGridSizer *This;
  This = (wxGridSizer *) memenv->getPtr(env, argv[0], "This");
  if(!This) throw wxe_badarg("This");
  int Result = This->GetHGap();
  wxeReturn rt = wxeReturn(memenv, Ecmd.caller, true);
  rt.send(  rt.make_int(Result));

}

// wxGridSizer::GetRows
void wxGridSizer_GetRows(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd)
{
  ErlNifEnv *env = Ecmd.env;
  ERL_NIF_TERM * argv = Ecmd.args;
  wxGridSizer *This;
  This = (wxGridSizer *) memenv->getPtr(env, argv[0], "This");
  if(!This) throw wxe_badarg("This");
  int Result = This->GetRows();
  wxeReturn rt = wxeReturn(memenv, Ecmd.caller, true);
  rt.send(  rt.make_int(Result));

}

// wxGridSizer::GetVGap
void wxGridSizer_GetVGap(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd)
{
  ErlNifEnv *env = Ecmd.env;
  ERL_NIF_TERM * argv = Ecmd.args;
  wxGridSizer *This;
  This = (wxGridSizer *) memenv->getPtr(env, argv[0], "This");
  if(!This) throw wxe_badarg("This");
  int Result = This->GetVGap();
  wxeReturn rt = wxeReturn(memenv, Ecmd.caller, true);
  rt.send(  rt.make_int(Result));

}

// wxGridSizer::SetCols
void wxGridSizer_SetCols(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd)
{
  ErlNifEnv *env = Ecmd.env;
  ERL_NIF_TERM * argv = Ecmd.args;
  wxGridSizer *This;
  This = (wxGridSizer *) memenv->getPtr(env, argv[0], "This");
  int cols;
  if(!enif_get_int(env, argv[1], &cols)) Badarg("cols"); // int
  if(!This) throw wxe_badarg("This");
  This->SetCols(cols);

}

// wxGridSizer::SetHGap
void wxGridSizer_SetHGap(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd)
{
  ErlNifEnv *env = Ecmd.env;
  ERL_NIF_TERM * argv = Ecmd.args;
  wxGridSizer *This;
  This = (wxGridSizer *) memenv->getPtr(env, argv[0], "This");
  int gap;
  if(!enif_get_int(env, argv[1], &gap)) Badarg("gap"); // int
  if(!This) throw wxe_badarg("This");
  This->SetHGap(gap);

}

// wxGridSizer::SetRows
void wxGridSizer_SetRows(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd)
{
  ErlNifEnv *env = Ecmd.env;
  ERL_NIF_TERM * argv = Ecmd.args;
  wxGridSizer *This;
  This = (wxGridSizer *) memenv->getPtr(env, argv[0], "This");
  int rows;
  if(!enif_get_int(env, argv[1], &rows)) Badarg("rows"); // int
  if(!This) throw wxe_badarg("This");
  This->SetRows(rows);

}

// wxGridSizer::SetVGap
void wxGridSizer_SetVGap(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd)
{
  ErlNifEnv *env = Ecmd.env;
  ERL_NIF_TERM * argv = Ecmd.args;
  wxGridSizer *This;
  This = (wxGridSizer *) memenv->getPtr(env, argv[0], "This");
  int gap;
  if(!enif_get_int(env, argv[1], &gap)) Badarg("gap"); // int
  if(!This) throw wxe_badarg("This");
  This->SetVGap(gap);

}

