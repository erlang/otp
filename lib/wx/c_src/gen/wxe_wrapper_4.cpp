/*
 * %CopyrightBegin%
 *
 * Copyright Ericsson AB 2008-2022. All Rights Reserved.
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

// wxHelpEvent::GetOrigin
void wxHelpEvent_GetOrigin(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd)
{
  ErlNifEnv *env = Ecmd.env;
  ERL_NIF_TERM * argv = Ecmd.args;
  wxHelpEvent *This;
  This = (wxHelpEvent *) memenv->getPtr(env, argv[0], "This");
  if(!This) throw wxe_badarg("This");
  int Result = This->GetOrigin();
  wxeReturn rt = wxeReturn(memenv, Ecmd.caller, true);
  rt.send(  rt.make_int(Result));

}

// wxHelpEvent::GetPosition
void wxHelpEvent_GetPosition(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd)
{
  ErlNifEnv *env = Ecmd.env;
  ERL_NIF_TERM * argv = Ecmd.args;
  wxHelpEvent *This;
  This = (wxHelpEvent *) memenv->getPtr(env, argv[0], "This");
  if(!This) throw wxe_badarg("This");
  const wxPoint * Result = &This->GetPosition();
  wxeReturn rt = wxeReturn(memenv, Ecmd.caller, true);
  rt.send(  rt.make((*Result)));

}

// wxHelpEvent::SetOrigin
void wxHelpEvent_SetOrigin(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd)
{
  ErlNifEnv *env = Ecmd.env;
  ERL_NIF_TERM * argv = Ecmd.args;
  wxHelpEvent *This;
  This = (wxHelpEvent *) memenv->getPtr(env, argv[0], "This");
  wxHelpEvent::Origin origin;
  if(!enif_get_int(env, argv[1], (int *) &origin)) Badarg("origin"); // enum
  if(!This) throw wxe_badarg("This");
  This->SetOrigin(origin);

}

// wxHelpEvent::SetPosition
void wxHelpEvent_SetPosition(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd)
{
  ErlNifEnv *env = Ecmd.env;
  ERL_NIF_TERM * argv = Ecmd.args;
  wxHelpEvent *This;
  This = (wxHelpEvent *) memenv->getPtr(env, argv[0], "This");
  const ERL_NIF_TERM *pt_t;
  int pt_sz;
  if(!enif_get_tuple(env, argv[1], &pt_sz, &pt_t)) Badarg("pt");
  int ptX;
  if(!enif_get_int(env, pt_t[0], &ptX)) Badarg("pt");
  int ptY;
  if(!enif_get_int(env, pt_t[1], &ptY)) Badarg("pt");
  wxPoint pt = wxPoint(ptX,ptY);
  if(!This) throw wxe_badarg("This");
  This->SetPosition(pt);

}

// wxHtmlEasyPrinting::wxHtmlEasyPrinting
void wxHtmlEasyPrinting_new(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd)
{
  wxString name= "Printing";
  wxWindow * parentWindow=NULL;
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
    if(enif_is_identical(tpl[0], enif_make_atom(env, "name"))) {
  ErlNifBinary name_bin;
  if(!enif_inspect_binary(env, tpl[1], &name_bin)) Badarg("name");
  name = wxString(name_bin.data, wxConvUTF8, name_bin.size);
    } else     if(enif_is_identical(tpl[0], enif_make_atom(env, "parentWindow"))) {
  parentWindow = (wxWindow *) memenv->getPtr(env, tpl[1], "parentWindow");
    } else        Badarg("Options");
  };
  wxHtmlEasyPrinting * Result = new EwxHtmlEasyPrinting(name,parentWindow);
  app->newPtr((void *) Result, 1, memenv);
  wxeReturn rt = wxeReturn(memenv, Ecmd.caller, true);
  rt.send(  rt.make_ref(app->getRef((void *)Result,memenv), "wxHtmlEasyPrinting"));

}

// wxHtmlEasyPrinting::GetPrintData
void wxHtmlEasyPrinting_GetPrintData(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd)
{
  ErlNifEnv *env = Ecmd.env;
  ERL_NIF_TERM * argv = Ecmd.args;
  wxHtmlEasyPrinting *This;
  This = (wxHtmlEasyPrinting *) memenv->getPtr(env, argv[0], "This");
  if(!This) throw wxe_badarg("This");
  wxPrintData * Result = (wxPrintData*)This->GetPrintData();
  wxeReturn rt = wxeReturn(memenv, Ecmd.caller, true);
  rt.send(  rt.make_ref(app->getRef((void *)Result,memenv), "wxPrintData"));

}

// wxHtmlEasyPrinting::GetPageSetupData
void wxHtmlEasyPrinting_GetPageSetupData(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd)
{
  ErlNifEnv *env = Ecmd.env;
  ERL_NIF_TERM * argv = Ecmd.args;
  wxHtmlEasyPrinting *This;
  This = (wxHtmlEasyPrinting *) memenv->getPtr(env, argv[0], "This");
  if(!This) throw wxe_badarg("This");
  wxPageSetupDialogData * Result = (wxPageSetupDialogData*)This->GetPageSetupData();
  wxeReturn rt = wxeReturn(memenv, Ecmd.caller, true);
  rt.send(  rt.make_ref(app->getRef((void *)Result,memenv), "wxPageSetupDialogData"));

}

// wxHtmlEasyPrinting::PreviewFile
void wxHtmlEasyPrinting_PreviewFile(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd)
{
  ErlNifEnv *env = Ecmd.env;
  ERL_NIF_TERM * argv = Ecmd.args;
  wxHtmlEasyPrinting *This;
  This = (wxHtmlEasyPrinting *) memenv->getPtr(env, argv[0], "This");
  ErlNifBinary htmlfile_bin;
  wxString htmlfile;
  if(!enif_inspect_binary(env, argv[1], &htmlfile_bin)) Badarg("htmlfile");
  htmlfile = wxString(htmlfile_bin.data, wxConvUTF8, htmlfile_bin.size);
  if(!This) throw wxe_badarg("This");
  bool Result = This->PreviewFile(htmlfile);
  wxeReturn rt = wxeReturn(memenv, Ecmd.caller, true);
  rt.send(  rt.make_bool(Result));

}

// wxHtmlEasyPrinting::PreviewText
void wxHtmlEasyPrinting_PreviewText(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd)
{
  wxString basepath= wxEmptyString;
  ErlNifEnv *env = Ecmd.env;
  ERL_NIF_TERM * argv = Ecmd.args;
  wxHtmlEasyPrinting *This;
  This = (wxHtmlEasyPrinting *) memenv->getPtr(env, argv[0], "This");
  ErlNifBinary htmltext_bin;
  wxString htmltext;
  if(!enif_inspect_binary(env, argv[1], &htmltext_bin)) Badarg("htmltext");
  htmltext = wxString(htmltext_bin.data, wxConvUTF8, htmltext_bin.size);
  ERL_NIF_TERM lstHead, lstTail;
  lstTail = argv[2];
  if(!enif_is_list(env, lstTail)) Badarg("Options");
  const ERL_NIF_TERM *tpl;
  int tpl_sz;
  while(!enif_is_empty_list(env, lstTail)) {
    if(!enif_get_list_cell(env, lstTail, &lstHead, &lstTail)) Badarg("Options");
    if(!enif_get_tuple(env, lstHead, &tpl_sz, &tpl) || tpl_sz != 2) Badarg("Options");
    if(enif_is_identical(tpl[0], enif_make_atom(env, "basepath"))) {
  ErlNifBinary basepath_bin;
  if(!enif_inspect_binary(env, tpl[1], &basepath_bin)) Badarg("basepath");
  basepath = wxString(basepath_bin.data, wxConvUTF8, basepath_bin.size);
    } else        Badarg("Options");
  };
  if(!This) throw wxe_badarg("This");
  bool Result = This->PreviewText(htmltext,basepath);
  wxeReturn rt = wxeReturn(memenv, Ecmd.caller, true);
  rt.send(  rt.make_bool(Result));

}

// wxHtmlEasyPrinting::PrintFile
void wxHtmlEasyPrinting_PrintFile(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd)
{
  ErlNifEnv *env = Ecmd.env;
  ERL_NIF_TERM * argv = Ecmd.args;
  wxHtmlEasyPrinting *This;
  This = (wxHtmlEasyPrinting *) memenv->getPtr(env, argv[0], "This");
  ErlNifBinary htmlfile_bin;
  wxString htmlfile;
  if(!enif_inspect_binary(env, argv[1], &htmlfile_bin)) Badarg("htmlfile");
  htmlfile = wxString(htmlfile_bin.data, wxConvUTF8, htmlfile_bin.size);
  if(!This) throw wxe_badarg("This");
  bool Result = This->PrintFile(htmlfile);
  wxeReturn rt = wxeReturn(memenv, Ecmd.caller, true);
  rt.send(  rt.make_bool(Result));

}

// wxHtmlEasyPrinting::PrintText
void wxHtmlEasyPrinting_PrintText(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd)
{
  wxString basepath= wxEmptyString;
  ErlNifEnv *env = Ecmd.env;
  ERL_NIF_TERM * argv = Ecmd.args;
  wxHtmlEasyPrinting *This;
  This = (wxHtmlEasyPrinting *) memenv->getPtr(env, argv[0], "This");
  ErlNifBinary htmltext_bin;
  wxString htmltext;
  if(!enif_inspect_binary(env, argv[1], &htmltext_bin)) Badarg("htmltext");
  htmltext = wxString(htmltext_bin.data, wxConvUTF8, htmltext_bin.size);
  ERL_NIF_TERM lstHead, lstTail;
  lstTail = argv[2];
  if(!enif_is_list(env, lstTail)) Badarg("Options");
  const ERL_NIF_TERM *tpl;
  int tpl_sz;
  while(!enif_is_empty_list(env, lstTail)) {
    if(!enif_get_list_cell(env, lstTail, &lstHead, &lstTail)) Badarg("Options");
    if(!enif_get_tuple(env, lstHead, &tpl_sz, &tpl) || tpl_sz != 2) Badarg("Options");
    if(enif_is_identical(tpl[0], enif_make_atom(env, "basepath"))) {
  ErlNifBinary basepath_bin;
  if(!enif_inspect_binary(env, tpl[1], &basepath_bin)) Badarg("basepath");
  basepath = wxString(basepath_bin.data, wxConvUTF8, basepath_bin.size);
    } else        Badarg("Options");
  };
  if(!This) throw wxe_badarg("This");
  bool Result = This->PrintText(htmltext,basepath);
  wxeReturn rt = wxeReturn(memenv, Ecmd.caller, true);
  rt.send(  rt.make_bool(Result));

}

// wxHtmlEasyPrinting::PageSetup
void wxHtmlEasyPrinting_PageSetup(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd)
{
  ErlNifEnv *env = Ecmd.env;
  ERL_NIF_TERM * argv = Ecmd.args;
  wxHtmlEasyPrinting *This;
  This = (wxHtmlEasyPrinting *) memenv->getPtr(env, argv[0], "This");
  if(!This) throw wxe_badarg("This");
  This->PageSetup();

}

// wxHtmlEasyPrinting::SetFonts
void wxHtmlEasyPrinting_SetFonts(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd)
{
  unsigned int sizesLen;
  std::vector <int> sizes;
  ErlNifEnv *env = Ecmd.env;
  ERL_NIF_TERM * argv = Ecmd.args;
  wxHtmlEasyPrinting *This;
  This = (wxHtmlEasyPrinting *) memenv->getPtr(env, argv[0], "This");
  ErlNifBinary normal_face_bin;
  wxString normal_face;
  if(!enif_inspect_binary(env, argv[1], &normal_face_bin)) Badarg("normal_face");
  normal_face = wxString(normal_face_bin.data, wxConvUTF8, normal_face_bin.size);
  ErlNifBinary fixed_face_bin;
  wxString fixed_face;
  if(!enif_inspect_binary(env, argv[2], &fixed_face_bin)) Badarg("fixed_face");
  fixed_face = wxString(fixed_face_bin.data, wxConvUTF8, fixed_face_bin.size);
  ERL_NIF_TERM lstHead, lstTail;
  lstTail = argv[3];
  if(!enif_is_list(env, lstTail)) Badarg("Options");
  const ERL_NIF_TERM *tpl;
  int tpl_sz;
  while(!enif_is_empty_list(env, lstTail)) {
    if(!enif_get_list_cell(env, lstTail, &lstHead, &lstTail)) Badarg("Options");
    if(!enif_get_tuple(env, lstHead, &tpl_sz, &tpl) || tpl_sz != 2) Badarg("Options");
    if(enif_is_identical(tpl[0], enif_make_atom(env, "sizes"))) {
  int sizes_tmp;
  ERL_NIF_TERM sizesHead, sizesTail;
  if(!enif_get_list_length(env, tpl[1], &sizesLen)) Badarg("sizes");
  sizesTail = tpl[1];
  while(!enif_is_empty_list(env, sizesTail)) {
    if(!enif_get_list_cell(env, sizesTail, &sizesHead, &sizesTail)) Badarg("sizes");
    if(!enif_get_int(env, sizesHead, &sizes_tmp)) Badarg("sizes");
    sizes.push_back( (int) sizes_tmp);
  };
    } else        Badarg("Options");
  };
  if(!This) throw wxe_badarg("This");
  This->SetFonts(normal_face,fixed_face, sizes.empty() ? (int *) NULL : sizes.data());

}

// wxHtmlEasyPrinting::SetHeader
void wxHtmlEasyPrinting_SetHeader(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd)
{
  int pg=wxPAGE_ALL;
  ErlNifEnv *env = Ecmd.env;
  ERL_NIF_TERM * argv = Ecmd.args;
  wxHtmlEasyPrinting *This;
  This = (wxHtmlEasyPrinting *) memenv->getPtr(env, argv[0], "This");
  ErlNifBinary header_bin;
  wxString header;
  if(!enif_inspect_binary(env, argv[1], &header_bin)) Badarg("header");
  header = wxString(header_bin.data, wxConvUTF8, header_bin.size);
  ERL_NIF_TERM lstHead, lstTail;
  lstTail = argv[2];
  if(!enif_is_list(env, lstTail)) Badarg("Options");
  const ERL_NIF_TERM *tpl;
  int tpl_sz;
  while(!enif_is_empty_list(env, lstTail)) {
    if(!enif_get_list_cell(env, lstTail, &lstHead, &lstTail)) Badarg("Options");
    if(!enif_get_tuple(env, lstHead, &tpl_sz, &tpl) || tpl_sz != 2) Badarg("Options");
    if(enif_is_identical(tpl[0], enif_make_atom(env, "pg"))) {
  if(!enif_get_int(env, tpl[1], &pg)) Badarg("pg"); // int
    } else        Badarg("Options");
  };
  if(!This) throw wxe_badarg("This");
  This->SetHeader(header,pg);

}

// wxHtmlEasyPrinting::SetFooter
void wxHtmlEasyPrinting_SetFooter(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd)
{
  int pg=wxPAGE_ALL;
  ErlNifEnv *env = Ecmd.env;
  ERL_NIF_TERM * argv = Ecmd.args;
  wxHtmlEasyPrinting *This;
  This = (wxHtmlEasyPrinting *) memenv->getPtr(env, argv[0], "This");
  ErlNifBinary footer_bin;
  wxString footer;
  if(!enif_inspect_binary(env, argv[1], &footer_bin)) Badarg("footer");
  footer = wxString(footer_bin.data, wxConvUTF8, footer_bin.size);
  ERL_NIF_TERM lstHead, lstTail;
  lstTail = argv[2];
  if(!enif_is_list(env, lstTail)) Badarg("Options");
  const ERL_NIF_TERM *tpl;
  int tpl_sz;
  while(!enif_is_empty_list(env, lstTail)) {
    if(!enif_get_list_cell(env, lstTail, &lstHead, &lstTail)) Badarg("Options");
    if(!enif_get_tuple(env, lstHead, &tpl_sz, &tpl) || tpl_sz != 2) Badarg("Options");
    if(enif_is_identical(tpl[0], enif_make_atom(env, "pg"))) {
  if(!enif_get_int(env, tpl[1], &pg)) Badarg("pg"); // int
    } else        Badarg("Options");
  };
  if(!This) throw wxe_badarg("This");
  This->SetFooter(footer,pg);

}

// wxHtmlLinkEvent::GetLinkInfo
void wxHtmlLinkEvent_GetLinkInfo(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd)
{
  ErlNifEnv *env = Ecmd.env;
  ERL_NIF_TERM * argv = Ecmd.args;
  wxHtmlLinkEvent *This;
  This = (wxHtmlLinkEvent *) memenv->getPtr(env, argv[0], "This");
  if(!This) throw wxe_badarg("This");
  const wxHtmlLinkInfo * Result = &This->GetLinkInfo();
  wxeReturn rt = wxeReturn(memenv, Ecmd.caller, true);
  rt.send(  rt.make(Result));

}

// wxHtmlWindow::wxHtmlWindow
void wxHtmlWindow_new_0(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd)
{
  wxHtmlWindow * Result = new EwxHtmlWindow();
  app->newPtr((void *) Result, 0, memenv);
  wxeReturn rt = wxeReturn(memenv, Ecmd.caller, true);
  rt.send(  rt.make_ref(app->getRef((void *)Result,memenv), "wxHtmlWindow"));

}

// wxHtmlWindow::wxHtmlWindow
void wxHtmlWindow_new_2(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd)
{
  wxWindowID id=wxID_ANY;
  wxPoint pos= wxDefaultPosition;
  wxSize size= wxDefaultSize;
  long style=wxHW_DEFAULT_STYLE;
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
    } else        Badarg("Options");
  };
  wxHtmlWindow * Result = new EwxHtmlWindow(parent,id,pos,size,style);
  app->newPtr((void *) Result, 0, memenv);
  wxeReturn rt = wxeReturn(memenv, Ecmd.caller, true);
  rt.send(  rt.make_ref(app->getRef((void *)Result,memenv), "wxHtmlWindow"));

}

// wxHtmlWindow::AppendToPage
void wxHtmlWindow_AppendToPage(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd)
{
  ErlNifEnv *env = Ecmd.env;
  ERL_NIF_TERM * argv = Ecmd.args;
  wxHtmlWindow *This;
  This = (wxHtmlWindow *) memenv->getPtr(env, argv[0], "This");
  ErlNifBinary source_bin;
  wxString source;
  if(!enif_inspect_binary(env, argv[1], &source_bin)) Badarg("source");
  source = wxString(source_bin.data, wxConvUTF8, source_bin.size);
  if(!This) throw wxe_badarg("This");
  bool Result = This->AppendToPage(source);
  wxeReturn rt = wxeReturn(memenv, Ecmd.caller, true);
  rt.send(  rt.make_bool(Result));

}

// wxHtmlWindow::GetOpenedAnchor
void wxHtmlWindow_GetOpenedAnchor(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd)
{
  ErlNifEnv *env = Ecmd.env;
  ERL_NIF_TERM * argv = Ecmd.args;
  wxHtmlWindow *This;
  This = (wxHtmlWindow *) memenv->getPtr(env, argv[0], "This");
  if(!This) throw wxe_badarg("This");
  wxString Result = This->GetOpenedAnchor();
  wxeReturn rt = wxeReturn(memenv, Ecmd.caller, true);
  rt.send(  rt.make(Result));

}

// wxHtmlWindow::GetOpenedPage
void wxHtmlWindow_GetOpenedPage(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd)
{
  ErlNifEnv *env = Ecmd.env;
  ERL_NIF_TERM * argv = Ecmd.args;
  wxHtmlWindow *This;
  This = (wxHtmlWindow *) memenv->getPtr(env, argv[0], "This");
  if(!This) throw wxe_badarg("This");
  wxString Result = This->GetOpenedPage();
  wxeReturn rt = wxeReturn(memenv, Ecmd.caller, true);
  rt.send(  rt.make(Result));

}

// wxHtmlWindow::GetOpenedPageTitle
void wxHtmlWindow_GetOpenedPageTitle(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd)
{
  ErlNifEnv *env = Ecmd.env;
  ERL_NIF_TERM * argv = Ecmd.args;
  wxHtmlWindow *This;
  This = (wxHtmlWindow *) memenv->getPtr(env, argv[0], "This");
  if(!This) throw wxe_badarg("This");
  wxString Result = This->GetOpenedPageTitle();
  wxeReturn rt = wxeReturn(memenv, Ecmd.caller, true);
  rt.send(  rt.make(Result));

}

// wxHtmlWindow::GetRelatedFrame
void wxHtmlWindow_GetRelatedFrame(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd)
{
  ErlNifEnv *env = Ecmd.env;
  ERL_NIF_TERM * argv = Ecmd.args;
  wxHtmlWindow *This;
  This = (wxHtmlWindow *) memenv->getPtr(env, argv[0], "This");
  if(!This) throw wxe_badarg("This");
  wxFrame * Result = (wxFrame*)This->GetRelatedFrame();
  wxeReturn rt = wxeReturn(memenv, Ecmd.caller, true);
  rt.send(  rt.make_ref(app->getRef((void *)Result,memenv), "wxFrame"));

}

// wxHtmlWindow::HistoryBack
void wxHtmlWindow_HistoryBack(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd)
{
  ErlNifEnv *env = Ecmd.env;
  ERL_NIF_TERM * argv = Ecmd.args;
  wxHtmlWindow *This;
  This = (wxHtmlWindow *) memenv->getPtr(env, argv[0], "This");
  if(!This) throw wxe_badarg("This");
  bool Result = This->HistoryBack();
  wxeReturn rt = wxeReturn(memenv, Ecmd.caller, true);
  rt.send(  rt.make_bool(Result));

}

// wxHtmlWindow::HistoryCanBack
void wxHtmlWindow_HistoryCanBack(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd)
{
  ErlNifEnv *env = Ecmd.env;
  ERL_NIF_TERM * argv = Ecmd.args;
  wxHtmlWindow *This;
  This = (wxHtmlWindow *) memenv->getPtr(env, argv[0], "This");
  if(!This) throw wxe_badarg("This");
  bool Result = This->HistoryCanBack();
  wxeReturn rt = wxeReturn(memenv, Ecmd.caller, true);
  rt.send(  rt.make_bool(Result));

}

// wxHtmlWindow::HistoryCanForward
void wxHtmlWindow_HistoryCanForward(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd)
{
  ErlNifEnv *env = Ecmd.env;
  ERL_NIF_TERM * argv = Ecmd.args;
  wxHtmlWindow *This;
  This = (wxHtmlWindow *) memenv->getPtr(env, argv[0], "This");
  if(!This) throw wxe_badarg("This");
  bool Result = This->HistoryCanForward();
  wxeReturn rt = wxeReturn(memenv, Ecmd.caller, true);
  rt.send(  rt.make_bool(Result));

}

// wxHtmlWindow::HistoryClear
void wxHtmlWindow_HistoryClear(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd)
{
  ErlNifEnv *env = Ecmd.env;
  ERL_NIF_TERM * argv = Ecmd.args;
  wxHtmlWindow *This;
  This = (wxHtmlWindow *) memenv->getPtr(env, argv[0], "This");
  if(!This) throw wxe_badarg("This");
  This->HistoryClear();

}

// wxHtmlWindow::HistoryForward
void wxHtmlWindow_HistoryForward(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd)
{
  ErlNifEnv *env = Ecmd.env;
  ERL_NIF_TERM * argv = Ecmd.args;
  wxHtmlWindow *This;
  This = (wxHtmlWindow *) memenv->getPtr(env, argv[0], "This");
  if(!This) throw wxe_badarg("This");
  bool Result = This->HistoryForward();
  wxeReturn rt = wxeReturn(memenv, Ecmd.caller, true);
  rt.send(  rt.make_bool(Result));

}

// wxHtmlWindow::LoadFile
void wxHtmlWindow_LoadFile(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd)
{
  ErlNifEnv *env = Ecmd.env;
  ERL_NIF_TERM * argv = Ecmd.args;
  wxHtmlWindow *This;
  This = (wxHtmlWindow *) memenv->getPtr(env, argv[0], "This");
  ErlNifBinary filename_bin;
  wxString filenameStr;
  if(!enif_inspect_binary(env, argv[1], &filename_bin)) Badarg("filename");
  filenameStr = wxString(filename_bin.data, wxConvUTF8, filename_bin.size);
  wxFileName  filename = wxFileName(filenameStr);
  if(!This) throw wxe_badarg("This");
  bool Result = This->LoadFile(filename);
  wxeReturn rt = wxeReturn(memenv, Ecmd.caller, true);
  rt.send(  rt.make_bool(Result));

}

// wxHtmlWindow::LoadPage
void wxHtmlWindow_LoadPage(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd)
{
  ErlNifEnv *env = Ecmd.env;
  ERL_NIF_TERM * argv = Ecmd.args;
  wxHtmlWindow *This;
  This = (wxHtmlWindow *) memenv->getPtr(env, argv[0], "This");
  ErlNifBinary location_bin;
  wxString location;
  if(!enif_inspect_binary(env, argv[1], &location_bin)) Badarg("location");
  location = wxString(location_bin.data, wxConvUTF8, location_bin.size);
  if(!This) throw wxe_badarg("This");
  bool Result = This->LoadPage(location);
  wxeReturn rt = wxeReturn(memenv, Ecmd.caller, true);
  rt.send(  rt.make_bool(Result));

}

// wxHtmlWindow::SelectAll
void wxHtmlWindow_SelectAll(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd)
{
  ErlNifEnv *env = Ecmd.env;
  ERL_NIF_TERM * argv = Ecmd.args;
  wxHtmlWindow *This;
  This = (wxHtmlWindow *) memenv->getPtr(env, argv[0], "This");
  if(!This) throw wxe_badarg("This");
  This->SelectAll();

}

// wxHtmlWindow::SelectionToText
void wxHtmlWindow_SelectionToText(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd)
{
  ErlNifEnv *env = Ecmd.env;
  ERL_NIF_TERM * argv = Ecmd.args;
  wxHtmlWindow *This;
  This = (wxHtmlWindow *) memenv->getPtr(env, argv[0], "This");
  if(!This) throw wxe_badarg("This");
  wxString Result = This->SelectionToText();
  wxeReturn rt = wxeReturn(memenv, Ecmd.caller, true);
  rt.send(  rt.make(Result));

}

// wxHtmlWindow::SelectLine
void wxHtmlWindow_SelectLine(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd)
{
  ErlNifEnv *env = Ecmd.env;
  ERL_NIF_TERM * argv = Ecmd.args;
  wxHtmlWindow *This;
  This = (wxHtmlWindow *) memenv->getPtr(env, argv[0], "This");
  const ERL_NIF_TERM *pos_t;
  int pos_sz;
  if(!enif_get_tuple(env, argv[1], &pos_sz, &pos_t)) Badarg("pos");
  int posX;
  if(!enif_get_int(env, pos_t[0], &posX)) Badarg("pos");
  int posY;
  if(!enif_get_int(env, pos_t[1], &posY)) Badarg("pos");
  wxPoint pos = wxPoint(posX,posY);
  if(!This) throw wxe_badarg("This");
  This->SelectLine(pos);

}

// wxHtmlWindow::SelectWord
void wxHtmlWindow_SelectWord(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd)
{
  ErlNifEnv *env = Ecmd.env;
  ERL_NIF_TERM * argv = Ecmd.args;
  wxHtmlWindow *This;
  This = (wxHtmlWindow *) memenv->getPtr(env, argv[0], "This");
  const ERL_NIF_TERM *pos_t;
  int pos_sz;
  if(!enif_get_tuple(env, argv[1], &pos_sz, &pos_t)) Badarg("pos");
  int posX;
  if(!enif_get_int(env, pos_t[0], &posX)) Badarg("pos");
  int posY;
  if(!enif_get_int(env, pos_t[1], &posY)) Badarg("pos");
  wxPoint pos = wxPoint(posX,posY);
  if(!This) throw wxe_badarg("This");
  This->SelectWord(pos);

}

// wxHtmlWindow::SetBorders
void wxHtmlWindow_SetBorders(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd)
{
  ErlNifEnv *env = Ecmd.env;
  ERL_NIF_TERM * argv = Ecmd.args;
  wxHtmlWindow *This;
  This = (wxHtmlWindow *) memenv->getPtr(env, argv[0], "This");
  int b;
  if(!enif_get_int(env, argv[1], &b)) Badarg("b"); // int
  if(!This) throw wxe_badarg("This");
  This->SetBorders(b);

}

// wxHtmlWindow::SetFonts
void wxHtmlWindow_SetFonts(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd)
{
  unsigned int sizesLen;
  std::vector <int> sizes;
  ErlNifEnv *env = Ecmd.env;
  ERL_NIF_TERM * argv = Ecmd.args;
  wxHtmlWindow *This;
  This = (wxHtmlWindow *) memenv->getPtr(env, argv[0], "This");
  ErlNifBinary normal_face_bin;
  wxString normal_face;
  if(!enif_inspect_binary(env, argv[1], &normal_face_bin)) Badarg("normal_face");
  normal_face = wxString(normal_face_bin.data, wxConvUTF8, normal_face_bin.size);
  ErlNifBinary fixed_face_bin;
  wxString fixed_face;
  if(!enif_inspect_binary(env, argv[2], &fixed_face_bin)) Badarg("fixed_face");
  fixed_face = wxString(fixed_face_bin.data, wxConvUTF8, fixed_face_bin.size);
  ERL_NIF_TERM lstHead, lstTail;
  lstTail = argv[3];
  if(!enif_is_list(env, lstTail)) Badarg("Options");
  const ERL_NIF_TERM *tpl;
  int tpl_sz;
  while(!enif_is_empty_list(env, lstTail)) {
    if(!enif_get_list_cell(env, lstTail, &lstHead, &lstTail)) Badarg("Options");
    if(!enif_get_tuple(env, lstHead, &tpl_sz, &tpl) || tpl_sz != 2) Badarg("Options");
    if(enif_is_identical(tpl[0], enif_make_atom(env, "sizes"))) {
  int sizes_tmp;
  ERL_NIF_TERM sizesHead, sizesTail;
  if(!enif_get_list_length(env, tpl[1], &sizesLen)) Badarg("sizes");
  sizesTail = tpl[1];
  while(!enif_is_empty_list(env, sizesTail)) {
    if(!enif_get_list_cell(env, sizesTail, &sizesHead, &sizesTail)) Badarg("sizes");
    if(!enif_get_int(env, sizesHead, &sizes_tmp)) Badarg("sizes");
    sizes.push_back( (int) sizes_tmp);
  };
    } else        Badarg("Options");
  };
  if(!This) throw wxe_badarg("This");
  This->SetFonts(normal_face,fixed_face, sizes.empty() ? NULL : sizes.data());

}

// wxHtmlWindow::SetPage
void wxHtmlWindow_SetPage(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd)
{
  ErlNifEnv *env = Ecmd.env;
  ERL_NIF_TERM * argv = Ecmd.args;
  wxHtmlWindow *This;
  This = (wxHtmlWindow *) memenv->getPtr(env, argv[0], "This");
  ErlNifBinary source_bin;
  wxString source;
  if(!enif_inspect_binary(env, argv[1], &source_bin)) Badarg("source");
  source = wxString(source_bin.data, wxConvUTF8, source_bin.size);
  if(!This) throw wxe_badarg("This");
  bool Result = This->SetPage(source);
  wxeReturn rt = wxeReturn(memenv, Ecmd.caller, true);
  rt.send(  rt.make_bool(Result));

}

// wxHtmlWindow::SetRelatedFrame
void wxHtmlWindow_SetRelatedFrame(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd)
{
  ErlNifEnv *env = Ecmd.env;
  ERL_NIF_TERM * argv = Ecmd.args;
  wxHtmlWindow *This;
  This = (wxHtmlWindow *) memenv->getPtr(env, argv[0], "This");
  wxFrame *frame;
  frame = (wxFrame *) memenv->getPtr(env, argv[1], "frame");
  ErlNifBinary format_bin;
  wxString format;
  if(!enif_inspect_binary(env, argv[2], &format_bin)) Badarg("format");
  format = wxString(format_bin.data, wxConvUTF8, format_bin.size);
  if(!This) throw wxe_badarg("This");
  This->SetRelatedFrame(frame,format);

}

// wxHtmlWindow::SetRelatedStatusBar
void wxHtmlWindow_SetRelatedStatusBar_1(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd)
{
  ErlNifEnv *env = Ecmd.env;
  ERL_NIF_TERM * argv = Ecmd.args;
  wxHtmlWindow *This;
  This = (wxHtmlWindow *) memenv->getPtr(env, argv[0], "This");
  int index;
  if(!enif_get_int(env, argv[1], &index)) Badarg("index"); // int
  if(!This) throw wxe_badarg("This");
  This->SetRelatedStatusBar(index);

}

// wxHtmlWindow::SetRelatedStatusBar
void wxHtmlWindow_SetRelatedStatusBar_2(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd)
{
  int index=0;
  ErlNifEnv *env = Ecmd.env;
  ERL_NIF_TERM * argv = Ecmd.args;
  wxHtmlWindow *This;
  This = (wxHtmlWindow *) memenv->getPtr(env, argv[0], "This");
  wxStatusBar *statusbar;
  statusbar = (wxStatusBar *) memenv->getPtr(env, argv[1], "statusbar");
  ERL_NIF_TERM lstHead, lstTail;
  lstTail = argv[2];
  if(!enif_is_list(env, lstTail)) Badarg("Options");
  const ERL_NIF_TERM *tpl;
  int tpl_sz;
  while(!enif_is_empty_list(env, lstTail)) {
    if(!enif_get_list_cell(env, lstTail, &lstHead, &lstTail)) Badarg("Options");
    if(!enif_get_tuple(env, lstHead, &tpl_sz, &tpl) || tpl_sz != 2) Badarg("Options");
    if(enif_is_identical(tpl[0], enif_make_atom(env, "index"))) {
  if(!enif_get_int(env, tpl[1], &index)) Badarg("index"); // int
    } else        Badarg("Options");
  };
  if(!This) throw wxe_badarg("This");
  This->SetRelatedStatusBar(statusbar,index);

}

// wxHtmlWindow::ToText
void wxHtmlWindow_ToText(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd)
{
  ErlNifEnv *env = Ecmd.env;
  ERL_NIF_TERM * argv = Ecmd.args;
  wxHtmlWindow *This;
  This = (wxHtmlWindow *) memenv->getPtr(env, argv[0], "This");
  if(!This) throw wxe_badarg("This");
  wxString Result = This->ToText();
  wxeReturn rt = wxeReturn(memenv, Ecmd.caller, true);
  rt.send(  rt.make(Result));

}

// wxIcon::wxIcon
void wxIcon_new_0(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd)
{
  wxIcon * Result = new EwxIcon();
  app->newPtr((void *) Result, 1, memenv);
  wxeReturn rt = wxeReturn(memenv, Ecmd.caller, true);
  rt.send(  rt.make_ref(app->getRef((void *)Result,memenv), "wxIcon"));

}

// wxIcon::wxIcon
void wxIcon_new_1(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd)
{
  ErlNifEnv *env = Ecmd.env;
  ERL_NIF_TERM * argv = Ecmd.args;
  wxIcon *icon;
  icon = (wxIcon *) memenv->getPtr(env, argv[0], "icon");
  wxIcon * Result = new EwxIcon(*icon);
  app->newPtr((void *) Result, 1, memenv);
  wxeReturn rt = wxeReturn(memenv, Ecmd.caller, true);
  rt.send(  rt.make_ref(app->getRef((void *)Result,memenv), "wxIcon"));

}

// wxIcon::wxIcon
void wxIcon_new_2(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd)
{
 wxBitmapType type=wxICON_DEFAULT_TYPE;
  int desiredWidth=-1;
  int desiredHeight=-1;
  ErlNifEnv *env = Ecmd.env;
  ERL_NIF_TERM * argv = Ecmd.args;
  ErlNifBinary name_bin;
  wxString name;
  if(!enif_inspect_binary(env, argv[0], &name_bin)) Badarg("name");
  name = wxString(name_bin.data, wxConvUTF8, name_bin.size);
  ERL_NIF_TERM lstHead, lstTail;
  lstTail = argv[1];
  if(!enif_is_list(env, lstTail)) Badarg("Options");
  const ERL_NIF_TERM *tpl;
  int tpl_sz;
  while(!enif_is_empty_list(env, lstTail)) {
    if(!enif_get_list_cell(env, lstTail, &lstHead, &lstTail)) Badarg("Options");
    if(!enif_get_tuple(env, lstHead, &tpl_sz, &tpl) || tpl_sz != 2) Badarg("Options");
    if(enif_is_identical(tpl[0], enif_make_atom(env, "type"))) {
  if(!enif_get_int(env, tpl[1], (int *) &type)) Badarg("type"); // enum
    } else     if(enif_is_identical(tpl[0], enif_make_atom(env, "desiredWidth"))) {
  if(!enif_get_int(env, tpl[1], &desiredWidth)) Badarg("desiredWidth"); // int
    } else     if(enif_is_identical(tpl[0], enif_make_atom(env, "desiredHeight"))) {
  if(!enif_get_int(env, tpl[1], &desiredHeight)) Badarg("desiredHeight"); // int
    } else        Badarg("Options");
  };
  wxIcon * Result = new EwxIcon(name,type,desiredWidth,desiredHeight);
  app->newPtr((void *) Result, 1, memenv);
  wxeReturn rt = wxeReturn(memenv, Ecmd.caller, true);
  rt.send(  rt.make_ref(app->getRef((void *)Result,memenv), "wxIcon"));

}

// wxIcon::CopyFromBitmap
void wxIcon_CopyFromBitmap(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd)
{
  ErlNifEnv *env = Ecmd.env;
  ERL_NIF_TERM * argv = Ecmd.args;
  wxIcon *This;
  This = (wxIcon *) memenv->getPtr(env, argv[0], "This");
  wxBitmap *bmp;
  bmp = (wxBitmap *) memenv->getPtr(env, argv[1], "bmp");
  if(!This) throw wxe_badarg("This");
  This->CopyFromBitmap(*bmp);

}

// wxIconBundle::wxIconBundle
void wxIconBundle_new_0(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd)
{
  wxIconBundle * Result = new EwxIconBundle();
  app->newPtr((void *) Result, 62, memenv);
  wxeReturn rt = wxeReturn(memenv, Ecmd.caller, true);
  rt.send(  rt.make_ref(app->getRef((void *)Result,memenv), "wxIconBundle"));

}

// wxIconBundle::wxIconBundle
void wxIconBundle_new_1_1(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd)
{
  ErlNifEnv *env = Ecmd.env;
  ERL_NIF_TERM * argv = Ecmd.args;
  ErlNifBinary file_bin;
  wxString file;
  if(!enif_inspect_binary(env, argv[0], &file_bin)) Badarg("file");
  file = wxString(file_bin.data, wxConvUTF8, file_bin.size);
  wxIconBundle * Result = new EwxIconBundle(file);
  app->newPtr((void *) Result, 62, memenv);
  wxeReturn rt = wxeReturn(memenv, Ecmd.caller, true);
  rt.send(  rt.make_ref(app->getRef((void *)Result,memenv), "wxIconBundle"));

}

// wxIconBundle::wxIconBundle
void wxIconBundle_new_2(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd)
{
  ErlNifEnv *env = Ecmd.env;
  ERL_NIF_TERM * argv = Ecmd.args;
  ErlNifBinary file_bin;
  wxString file;
  if(!enif_inspect_binary(env, argv[0], &file_bin)) Badarg("file");
  file = wxString(file_bin.data, wxConvUTF8, file_bin.size);
  wxBitmapType type;
  if(!enif_get_int(env, argv[1], (int *) &type)) Badarg("type"); // enum
  wxIconBundle * Result = new EwxIconBundle(file,type);
  app->newPtr((void *) Result, 62, memenv);
  wxeReturn rt = wxeReturn(memenv, Ecmd.caller, true);
  rt.send(  rt.make_ref(app->getRef((void *)Result,memenv), "wxIconBundle"));

}

// wxIconBundle::wxIconBundle
void wxIconBundle_new_1_0(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd)
{
  ErlNifEnv *env = Ecmd.env;
  ERL_NIF_TERM * argv = Ecmd.args;
  ERL_NIF_TERM ic_type;
  void * ic = memenv->getPtr(env, argv[0], "ic", &ic_type);
  wxIconBundle * Result;
  if(enif_is_identical(ic_type, WXE_ATOM_wxIconBundle))
    Result = new EwxIconBundle(* static_cast<wxIconBundle*> (ic));
  else if(enif_is_identical(ic_type, WXE_ATOM_wxIcon))
    Result = new EwxIconBundle(* static_cast<wxIcon*> (ic));
  else throw wxe_badarg("ic");
  app->newPtr((void *) Result, 62, memenv);
  wxeReturn rt = wxeReturn(memenv, Ecmd.caller, true);
  rt.send(  rt.make_ref(app->getRef((void *)Result,memenv), "wxIconBundle"));

}

// wxIconBundle::~wxIconBundle
void wxIconBundle_destruct(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd)
{
 ErlNifEnv *env = Ecmd.env;
 ERL_NIF_TERM * argv = Ecmd.args;
  wxIconBundle *This;
  This = (wxIconBundle *) memenv->getPtr(env, argv[0], "This");
 if(This) {   ((WxeApp *) wxTheApp)->clearPtr((void *) This);
   delete This;}
}

// wxIconBundle::AddIcon
void wxIconBundle_AddIcon_1_0(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd)
{
  ErlNifEnv *env = Ecmd.env;
  ERL_NIF_TERM * argv = Ecmd.args;
  wxIconBundle *This;
  This = (wxIconBundle *) memenv->getPtr(env, argv[0], "This");
  ErlNifBinary file_bin;
  wxString file;
  if(!enif_inspect_binary(env, argv[1], &file_bin)) Badarg("file");
  file = wxString(file_bin.data, wxConvUTF8, file_bin.size);
  if(!This) throw wxe_badarg("This");
  This->AddIcon(file);

}

// wxIconBundle::AddIcon
void wxIconBundle_AddIcon_2(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd)
{
  ErlNifEnv *env = Ecmd.env;
  ERL_NIF_TERM * argv = Ecmd.args;
  wxIconBundle *This;
  This = (wxIconBundle *) memenv->getPtr(env, argv[0], "This");
  ErlNifBinary file_bin;
  wxString file;
  if(!enif_inspect_binary(env, argv[1], &file_bin)) Badarg("file");
  file = wxString(file_bin.data, wxConvUTF8, file_bin.size);
  wxBitmapType type;
  if(!enif_get_int(env, argv[2], (int *) &type)) Badarg("type"); // enum
  if(!This) throw wxe_badarg("This");
  This->AddIcon(file,type);

}

// wxIconBundle::AddIcon
void wxIconBundle_AddIcon_1_1(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd)
{
  ErlNifEnv *env = Ecmd.env;
  ERL_NIF_TERM * argv = Ecmd.args;
  wxIconBundle *This;
  This = (wxIconBundle *) memenv->getPtr(env, argv[0], "This");
  wxIcon *icon;
  icon = (wxIcon *) memenv->getPtr(env, argv[1], "icon");
  if(!This) throw wxe_badarg("This");
  This->AddIcon(*icon);

}

// wxIconBundle::GetIcon
void wxIconBundle_GetIcon_2(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd)
{
  int flags=wxIconBundle::FALLBACK_SYSTEM;
  ErlNifEnv *env = Ecmd.env;
  ERL_NIF_TERM * argv = Ecmd.args;
  wxIconBundle *This;
  This = (wxIconBundle *) memenv->getPtr(env, argv[0], "This");
  const ERL_NIF_TERM *size_t;
  int size_sz;
  if(!enif_get_tuple(env, argv[1], &size_sz, &size_t)) Badarg("size");
  int sizeW;
  if(!enif_get_int(env, size_t[0], &sizeW)) Badarg("size");
  int sizeH;
  if(!enif_get_int(env, size_t[1], &sizeH)) Badarg("size");
  wxSize size = wxSize(sizeW,sizeH);
  ERL_NIF_TERM lstHead, lstTail;
  lstTail = argv[2];
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
  wxIcon * Result = new wxIcon(This->GetIcon(size,flags)); app->newPtr((void *) Result,3, memenv);;
  wxeReturn rt = wxeReturn(memenv, Ecmd.caller, true);
  rt.send(  rt.make_ref(app->getRef((void *)Result,memenv), "wxIcon"));

}

// wxIconBundle::GetIcon
void wxIconBundle_GetIcon_1(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd)
{
  wxCoord size=wxDefaultCoord;
  int flags=wxIconBundle::FALLBACK_SYSTEM;
  ErlNifEnv *env = Ecmd.env;
  ERL_NIF_TERM * argv = Ecmd.args;
  wxIconBundle *This;
  This = (wxIconBundle *) memenv->getPtr(env, argv[0], "This");
  ERL_NIF_TERM lstHead, lstTail;
  lstTail = argv[1];
  if(!enif_is_list(env, lstTail)) Badarg("Options");
  const ERL_NIF_TERM *tpl;
  int tpl_sz;
  while(!enif_is_empty_list(env, lstTail)) {
    if(!enif_get_list_cell(env, lstTail, &lstHead, &lstTail)) Badarg("Options");
    if(!enif_get_tuple(env, lstHead, &tpl_sz, &tpl) || tpl_sz != 2) Badarg("Options");
    if(enif_is_identical(tpl[0], enif_make_atom(env, "size"))) {
  if(!enif_get_int(env, tpl[1], &size)) Badarg("size"); // wxCoord
    } else     if(enif_is_identical(tpl[0], enif_make_atom(env, "flags"))) {
  if(!enif_get_int(env, tpl[1], &flags)) Badarg("flags"); // int
    } else        Badarg("Options");
  };
  if(!This) throw wxe_badarg("This");
  wxIcon * Result = new wxIcon(This->GetIcon(size,flags)); app->newPtr((void *) Result,3, memenv);;
  wxeReturn rt = wxeReturn(memenv, Ecmd.caller, true);
  rt.send(  rt.make_ref(app->getRef((void *)Result,memenv), "wxIcon"));

}

// wxIconizeEvent::IsIconized
void wxIconizeEvent_IsIconized(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd)
{
  ErlNifEnv *env = Ecmd.env;
  ERL_NIF_TERM * argv = Ecmd.args;
  wxIconizeEvent *This;
  This = (wxIconizeEvent *) memenv->getPtr(env, argv[0], "This");
  if(!This) throw wxe_badarg("This");
  bool Result = This->IsIconized();
  wxeReturn rt = wxeReturn(memenv, Ecmd.caller, true);
  rt.send(  rt.make_bool(Result));

}

// wxIdleEvent::GetMode
void wxIdleEvent_GetMode(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd)
{
  int Result = wxIdleEvent::GetMode();
  wxeReturn rt = wxeReturn(memenv, Ecmd.caller, true);
  rt.send(  rt.make_int(Result));

}

// wxIdleEvent::RequestMore
void wxIdleEvent_RequestMore(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd)
{
  bool needMore=true;
  ErlNifEnv *env = Ecmd.env;
  ERL_NIF_TERM * argv = Ecmd.args;
  wxIdleEvent *This;
  This = (wxIdleEvent *) memenv->getPtr(env, argv[0], "This");
  ERL_NIF_TERM lstHead, lstTail;
  lstTail = argv[1];
  if(!enif_is_list(env, lstTail)) Badarg("Options");
  const ERL_NIF_TERM *tpl;
  int tpl_sz;
  while(!enif_is_empty_list(env, lstTail)) {
    if(!enif_get_list_cell(env, lstTail, &lstHead, &lstTail)) Badarg("Options");
    if(!enif_get_tuple(env, lstHead, &tpl_sz, &tpl) || tpl_sz != 2) Badarg("Options");
    if(enif_is_identical(tpl[0], enif_make_atom(env, "needMore"))) {
  needMore = enif_is_identical(tpl[1], WXE_ATOM_true);
    } else        Badarg("Options");
  };
  if(!This) throw wxe_badarg("This");
  This->RequestMore(needMore);

}

// wxIdleEvent::MoreRequested
void wxIdleEvent_MoreRequested(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd)
{
  ErlNifEnv *env = Ecmd.env;
  ERL_NIF_TERM * argv = Ecmd.args;
  wxIdleEvent *This;
  This = (wxIdleEvent *) memenv->getPtr(env, argv[0], "This");
  if(!This) throw wxe_badarg("This");
  bool Result = This->MoreRequested();
  wxeReturn rt = wxeReturn(memenv, Ecmd.caller, true);
  rt.send(  rt.make_bool(Result));

}

// wxIdleEvent::SetMode
void wxIdleEvent_SetMode(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd)
{
  ErlNifEnv *env = Ecmd.env;
  ERL_NIF_TERM * argv = Ecmd.args;
  wxIdleMode mode;
  if(!enif_get_int(env, argv[0], (int *) &mode)) Badarg("mode"); // enum
  wxIdleEvent::SetMode(mode);

}

// wxImage::wxImage
void wxImage_new_0(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd)
{
  wxImage * Result = new EwxImage();
  app->newPtr((void *) Result, 1, memenv);
  wxeReturn rt = wxeReturn(memenv, Ecmd.caller, true);
  rt.send(  rt.make_ref(app->getRef((void *)Result,memenv), "wxImage"));

}

// wxImage::wxImage
void wxImage_new_3_1(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd)
{
  bool clear=true;
  ErlNifEnv *env = Ecmd.env;
  ERL_NIF_TERM * argv = Ecmd.args;
  int width;
  if(!enif_get_int(env, argv[0], &width)) Badarg("width"); // int
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
    if(enif_is_identical(tpl[0], enif_make_atom(env, "clear"))) {
  clear = enif_is_identical(tpl[1], WXE_ATOM_true);
    } else        Badarg("Options");
  };
  wxImage * Result = new EwxImage(width,height,clear);
  app->newPtr((void *) Result, 1, memenv);
  wxeReturn rt = wxeReturn(memenv, Ecmd.caller, true);
  rt.send(  rt.make_ref(app->getRef((void *)Result,memenv), "wxImage"));

}

// wxImage::wxImage
void wxImage_new_2_2(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd)
{
  bool clear=true;
  ErlNifEnv *env = Ecmd.env;
  ERL_NIF_TERM * argv = Ecmd.args;
  const ERL_NIF_TERM *sz_t;
  int sz_sz;
  if(!enif_get_tuple(env, argv[0], &sz_sz, &sz_t)) Badarg("sz");
  int szW;
  if(!enif_get_int(env, sz_t[0], &szW)) Badarg("sz");
  int szH;
  if(!enif_get_int(env, sz_t[1], &szH)) Badarg("sz");
  wxSize sz = wxSize(szW,szH);
  ERL_NIF_TERM lstHead, lstTail;
  lstTail = argv[1];
  if(!enif_is_list(env, lstTail)) Badarg("Options");
  const ERL_NIF_TERM *tpl;
  int tpl_sz;
  while(!enif_is_empty_list(env, lstTail)) {
    if(!enif_get_list_cell(env, lstTail, &lstHead, &lstTail)) Badarg("Options");
    if(!enif_get_tuple(env, lstHead, &tpl_sz, &tpl) || tpl_sz != 2) Badarg("Options");
    if(enif_is_identical(tpl[0], enif_make_atom(env, "clear"))) {
  clear = enif_is_identical(tpl[1], WXE_ATOM_true);
    } else        Badarg("Options");
  };
  wxImage * Result = new EwxImage(sz,clear);
  app->newPtr((void *) Result, 1, memenv);
  wxeReturn rt = wxeReturn(memenv, Ecmd.caller, true);
  rt.send(  rt.make_ref(app->getRef((void *)Result,memenv), "wxImage"));

}

// wxImage::wxImage
void wxImage_new_3_0(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd)
{
  ErlNifEnv *env = Ecmd.env;
  ERL_NIF_TERM * argv = Ecmd.args;
  int width;
  if(!enif_get_int(env, argv[0], &width)) Badarg("width"); // int
  int height;
  if(!enif_get_int(env, argv[1], &height)) Badarg("height"); // int
  unsigned char * data;
  ErlNifBinary data_bin;
  if(!enif_inspect_binary(env, argv[2], &data_bin)) Badarg("data");
  data = (unsigned char *) malloc(data_bin.size);
  memcpy(data,data_bin.data,data_bin.size);
  wxImage * Result = new EwxImage(width,height,data);
  app->newPtr((void *) Result, 1, memenv);
  wxeReturn rt = wxeReturn(memenv, Ecmd.caller, true);
  rt.send(  rt.make_ref(app->getRef((void *)Result,memenv), "wxImage"));

}

// wxImage::wxImage
void wxImage_new_2_1(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd)
{
  ErlNifEnv *env = Ecmd.env;
  ERL_NIF_TERM * argv = Ecmd.args;
  const ERL_NIF_TERM *sz_t;
  int sz_sz;
  if(!enif_get_tuple(env, argv[0], &sz_sz, &sz_t)) Badarg("sz");
  int szW;
  if(!enif_get_int(env, sz_t[0], &szW)) Badarg("sz");
  int szH;
  if(!enif_get_int(env, sz_t[1], &szH)) Badarg("sz");
  wxSize sz = wxSize(szW,szH);
  unsigned char * data;
  ErlNifBinary data_bin;
  if(!enif_inspect_binary(env, argv[1], &data_bin)) Badarg("data");
  data = (unsigned char *) malloc(data_bin.size);
  memcpy(data,data_bin.data,data_bin.size);
  wxImage * Result = new EwxImage(sz,data);
  app->newPtr((void *) Result, 1, memenv);
  wxeReturn rt = wxeReturn(memenv, Ecmd.caller, true);
  rt.send(  rt.make_ref(app->getRef((void *)Result,memenv), "wxImage"));

}

// wxImage::wxImage
void wxImage_new_4(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd)
{
  ErlNifEnv *env = Ecmd.env;
  ERL_NIF_TERM * argv = Ecmd.args;
  int width;
  if(!enif_get_int(env, argv[0], &width)) Badarg("width"); // int
  int height;
  if(!enif_get_int(env, argv[1], &height)) Badarg("height"); // int
  unsigned char * data;
  ErlNifBinary data_bin;
  if(!enif_inspect_binary(env, argv[2], &data_bin)) Badarg("data");
  data = (unsigned char *) malloc(data_bin.size);
  memcpy(data,data_bin.data,data_bin.size);
  unsigned char * alpha;
  ErlNifBinary alpha_bin;
  if(!enif_inspect_binary(env, argv[3], &alpha_bin)) Badarg("alpha");
  alpha = (unsigned char *) malloc(alpha_bin.size);
  memcpy(alpha,alpha_bin.data,alpha_bin.size);
  wxImage * Result = new EwxImage(width,height,data,alpha);
  app->newPtr((void *) Result, 1, memenv);
  wxeReturn rt = wxeReturn(memenv, Ecmd.caller, true);
  rt.send(  rt.make_ref(app->getRef((void *)Result,memenv), "wxImage"));

}

// wxImage::wxImage
void wxImage_new_3_3(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd)
{
  ErlNifEnv *env = Ecmd.env;
  ERL_NIF_TERM * argv = Ecmd.args;
  const ERL_NIF_TERM *sz_t;
  int sz_sz;
  if(!enif_get_tuple(env, argv[0], &sz_sz, &sz_t)) Badarg("sz");
  int szW;
  if(!enif_get_int(env, sz_t[0], &szW)) Badarg("sz");
  int szH;
  if(!enif_get_int(env, sz_t[1], &szH)) Badarg("sz");
  wxSize sz = wxSize(szW,szH);
  unsigned char * data;
  ErlNifBinary data_bin;
  if(!enif_inspect_binary(env, argv[1], &data_bin)) Badarg("data");
  data = (unsigned char *) malloc(data_bin.size);
  memcpy(data,data_bin.data,data_bin.size);
  unsigned char * alpha;
  ErlNifBinary alpha_bin;
  if(!enif_inspect_binary(env, argv[2], &alpha_bin)) Badarg("alpha");
  alpha = (unsigned char *) malloc(alpha_bin.size);
  memcpy(alpha,alpha_bin.data,alpha_bin.size);
  wxImage * Result = new EwxImage(sz,data,alpha);
  app->newPtr((void *) Result, 1, memenv);
  wxeReturn rt = wxeReturn(memenv, Ecmd.caller, true);
  rt.send(  rt.make_ref(app->getRef((void *)Result,memenv), "wxImage"));

}

// wxImage::wxImage
void wxImage_new_2_0(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd)
{
 wxBitmapType type=wxBITMAP_TYPE_ANY;
  int index=-1;
  ErlNifEnv *env = Ecmd.env;
  ERL_NIF_TERM * argv = Ecmd.args;
  ErlNifBinary name_bin;
  wxString name;
  if(!enif_inspect_binary(env, argv[0], &name_bin)) Badarg("name");
  name = wxString(name_bin.data, wxConvUTF8, name_bin.size);
  ERL_NIF_TERM lstHead, lstTail;
  lstTail = argv[1];
  if(!enif_is_list(env, lstTail)) Badarg("Options");
  const ERL_NIF_TERM *tpl;
  int tpl_sz;
  while(!enif_is_empty_list(env, lstTail)) {
    if(!enif_get_list_cell(env, lstTail, &lstHead, &lstTail)) Badarg("Options");
    if(!enif_get_tuple(env, lstHead, &tpl_sz, &tpl) || tpl_sz != 2) Badarg("Options");
    if(enif_is_identical(tpl[0], enif_make_atom(env, "type"))) {
  if(!enif_get_int(env, tpl[1], (int *) &type)) Badarg("type"); // enum
    } else     if(enif_is_identical(tpl[0], enif_make_atom(env, "index"))) {
  if(!enif_get_int(env, tpl[1], &index)) Badarg("index"); // int
    } else        Badarg("Options");
  };
  wxImage * Result = new EwxImage(name,type,index);
  app->newPtr((void *) Result, 1, memenv);
  wxeReturn rt = wxeReturn(memenv, Ecmd.caller, true);
  rt.send(  rt.make_ref(app->getRef((void *)Result,memenv), "wxImage"));

}

// wxImage::wxImage
void wxImage_new_3_2(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd)
{
  int index=-1;
  ErlNifEnv *env = Ecmd.env;
  ERL_NIF_TERM * argv = Ecmd.args;
  ErlNifBinary name_bin;
  wxString name;
  if(!enif_inspect_binary(env, argv[0], &name_bin)) Badarg("name");
  name = wxString(name_bin.data, wxConvUTF8, name_bin.size);
  ErlNifBinary mimetype_bin;
  wxString mimetype;
  if(!enif_inspect_binary(env, argv[1], &mimetype_bin)) Badarg("mimetype");
  mimetype = wxString(mimetype_bin.data, wxConvUTF8, mimetype_bin.size);
  ERL_NIF_TERM lstHead, lstTail;
  lstTail = argv[2];
  if(!enif_is_list(env, lstTail)) Badarg("Options");
  const ERL_NIF_TERM *tpl;
  int tpl_sz;
  while(!enif_is_empty_list(env, lstTail)) {
    if(!enif_get_list_cell(env, lstTail, &lstHead, &lstTail)) Badarg("Options");
    if(!enif_get_tuple(env, lstHead, &tpl_sz, &tpl) || tpl_sz != 2) Badarg("Options");
    if(enif_is_identical(tpl[0], enif_make_atom(env, "index"))) {
  if(!enif_get_int(env, tpl[1], &index)) Badarg("index"); // int
    } else        Badarg("Options");
  };
  wxImage * Result = new EwxImage(name,mimetype,index);
  app->newPtr((void *) Result, 1, memenv);
  wxeReturn rt = wxeReturn(memenv, Ecmd.caller, true);
  rt.send(  rt.make_ref(app->getRef((void *)Result,memenv), "wxImage"));

}

// wxImage::Blur
void wxImage_Blur(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd)
{
  ErlNifEnv *env = Ecmd.env;
  ERL_NIF_TERM * argv = Ecmd.args;
  wxImage *This;
  This = (wxImage *) memenv->getPtr(env, argv[0], "This");
  int blurRadius;
  if(!enif_get_int(env, argv[1], &blurRadius)) Badarg("blurRadius"); // int
  if(!This) throw wxe_badarg("This");
  wxImage * Result = new EwxImage(This->Blur(blurRadius)); app->newPtr((void *) Result,3, memenv);;
  wxeReturn rt = wxeReturn(memenv, Ecmd.caller, true);
  rt.send(  rt.make_ref(app->getRef((void *)Result,memenv), "wxImage"));

}

// wxImage::BlurHorizontal
void wxImage_BlurHorizontal(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd)
{
  ErlNifEnv *env = Ecmd.env;
  ERL_NIF_TERM * argv = Ecmd.args;
  wxImage *This;
  This = (wxImage *) memenv->getPtr(env, argv[0], "This");
  int blurRadius;
  if(!enif_get_int(env, argv[1], &blurRadius)) Badarg("blurRadius"); // int
  if(!This) throw wxe_badarg("This");
  wxImage * Result = new EwxImage(This->BlurHorizontal(blurRadius)); app->newPtr((void *) Result,3, memenv);;
  wxeReturn rt = wxeReturn(memenv, Ecmd.caller, true);
  rt.send(  rt.make_ref(app->getRef((void *)Result,memenv), "wxImage"));

}

// wxImage::BlurVertical
void wxImage_BlurVertical(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd)
{
  ErlNifEnv *env = Ecmd.env;
  ERL_NIF_TERM * argv = Ecmd.args;
  wxImage *This;
  This = (wxImage *) memenv->getPtr(env, argv[0], "This");
  int blurRadius;
  if(!enif_get_int(env, argv[1], &blurRadius)) Badarg("blurRadius"); // int
  if(!This) throw wxe_badarg("This");
  wxImage * Result = new EwxImage(This->BlurVertical(blurRadius)); app->newPtr((void *) Result,3, memenv);;
  wxeReturn rt = wxeReturn(memenv, Ecmd.caller, true);
  rt.send(  rt.make_ref(app->getRef((void *)Result,memenv), "wxImage"));

}

// wxImage::ConvertAlphaToMask
void wxImage_ConvertAlphaToMask_1(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd)
{
  unsigned int threshold=wxIMAGE_ALPHA_THRESHOLD;
  ErlNifEnv *env = Ecmd.env;
  ERL_NIF_TERM * argv = Ecmd.args;
  wxImage *This;
  This = (wxImage *) memenv->getPtr(env, argv[0], "This");
  ERL_NIF_TERM lstHead, lstTail;
  lstTail = argv[1];
  if(!enif_is_list(env, lstTail)) Badarg("Options");
  const ERL_NIF_TERM *tpl;
  int tpl_sz;
  while(!enif_is_empty_list(env, lstTail)) {
    if(!enif_get_list_cell(env, lstTail, &lstHead, &lstTail)) Badarg("Options");
    if(!enif_get_tuple(env, lstHead, &tpl_sz, &tpl) || tpl_sz != 2) Badarg("Options");
    if(enif_is_identical(tpl[0], enif_make_atom(env, "threshold"))) {
  if(!enif_get_uint(env, tpl[1], &threshold)) Badarg("threshold");
    } else        Badarg("Options");
  };
  if(!This) throw wxe_badarg("This");
  bool Result = This->ConvertAlphaToMask(threshold);
  wxeReturn rt = wxeReturn(memenv, Ecmd.caller, true);
  rt.send(  rt.make_bool(Result));

}

// wxImage::ConvertAlphaToMask
void wxImage_ConvertAlphaToMask_4(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd)
{
  unsigned int threshold=wxIMAGE_ALPHA_THRESHOLD;
  ErlNifEnv *env = Ecmd.env;
  ERL_NIF_TERM * argv = Ecmd.args;
  wxImage *This;
  This = (wxImage *) memenv->getPtr(env, argv[0], "This");
  unsigned int mr;
  if(!enif_get_uint(env, argv[1], &mr)) Badarg("mr");
  unsigned int mg;
  if(!enif_get_uint(env, argv[2], &mg)) Badarg("mg");
  unsigned int mb;
  if(!enif_get_uint(env, argv[3], &mb)) Badarg("mb");
  ERL_NIF_TERM lstHead, lstTail;
  lstTail = argv[4];
  if(!enif_is_list(env, lstTail)) Badarg("Options");
  const ERL_NIF_TERM *tpl;
  int tpl_sz;
  while(!enif_is_empty_list(env, lstTail)) {
    if(!enif_get_list_cell(env, lstTail, &lstHead, &lstTail)) Badarg("Options");
    if(!enif_get_tuple(env, lstHead, &tpl_sz, &tpl) || tpl_sz != 2) Badarg("Options");
    if(enif_is_identical(tpl[0], enif_make_atom(env, "threshold"))) {
  if(!enif_get_uint(env, tpl[1], &threshold)) Badarg("threshold");
    } else        Badarg("Options");
  };
  if(!This) throw wxe_badarg("This");
  bool Result = This->ConvertAlphaToMask(mr,mg,mb,threshold);
  wxeReturn rt = wxeReturn(memenv, Ecmd.caller, true);
  rt.send(  rt.make_bool(Result));

}

// wxImage::ConvertToGreyscale
void wxImage_ConvertToGreyscale_3(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd)
{
  ErlNifEnv *env = Ecmd.env;
  ERL_NIF_TERM * argv = Ecmd.args;
  wxImage *This;
  This = (wxImage *) memenv->getPtr(env, argv[0], "This");
  double weight_r;
  if(!wxe_get_double(env, argv[1], &weight_r)) Badarg("weight_r");
  double weight_g;
  if(!wxe_get_double(env, argv[2], &weight_g)) Badarg("weight_g");
  double weight_b;
  if(!wxe_get_double(env, argv[3], &weight_b)) Badarg("weight_b");
  if(!This) throw wxe_badarg("This");
  wxImage * Result = new EwxImage(This->ConvertToGreyscale(weight_r,weight_g,weight_b)); app->newPtr((void *) Result,3, memenv);;
  wxeReturn rt = wxeReturn(memenv, Ecmd.caller, true);
  rt.send(  rt.make_ref(app->getRef((void *)Result,memenv), "wxImage"));

}

// wxImage::ConvertToGreyscale
void wxImage_ConvertToGreyscale_0(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd)
{
  ErlNifEnv *env = Ecmd.env;
  ERL_NIF_TERM * argv = Ecmd.args;
  wxImage *This;
  This = (wxImage *) memenv->getPtr(env, argv[0], "This");
  if(!This) throw wxe_badarg("This");
  wxImage * Result = new EwxImage(This->ConvertToGreyscale()); app->newPtr((void *) Result,3, memenv);;
  wxeReturn rt = wxeReturn(memenv, Ecmd.caller, true);
  rt.send(  rt.make_ref(app->getRef((void *)Result,memenv), "wxImage"));

}

// wxImage::ConvertToMono
void wxImage_ConvertToMono(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd)
{
  ErlNifEnv *env = Ecmd.env;
  ERL_NIF_TERM * argv = Ecmd.args;
  wxImage *This;
  This = (wxImage *) memenv->getPtr(env, argv[0], "This");
  unsigned int r;
  if(!enif_get_uint(env, argv[1], &r)) Badarg("r");
  unsigned int g;
  if(!enif_get_uint(env, argv[2], &g)) Badarg("g");
  unsigned int b;
  if(!enif_get_uint(env, argv[3], &b)) Badarg("b");
  if(!This) throw wxe_badarg("This");
  wxImage * Result = new EwxImage(This->ConvertToMono(r,g,b)); app->newPtr((void *) Result,3, memenv);;
  wxeReturn rt = wxeReturn(memenv, Ecmd.caller, true);
  rt.send(  rt.make_ref(app->getRef((void *)Result,memenv), "wxImage"));

}

// wxImage::Copy
void wxImage_Copy(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd)
{
  ErlNifEnv *env = Ecmd.env;
  ERL_NIF_TERM * argv = Ecmd.args;
  wxImage *This;
  This = (wxImage *) memenv->getPtr(env, argv[0], "This");
  if(!This) throw wxe_badarg("This");
  wxImage * Result = new EwxImage(This->Copy()); app->newPtr((void *) Result,3, memenv);;
  wxeReturn rt = wxeReturn(memenv, Ecmd.caller, true);
  rt.send(  rt.make_ref(app->getRef((void *)Result,memenv), "wxImage"));

}

// wxImage::Create
void wxImage_Create_3_1(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd)
{
  bool clear=true;
  ErlNifEnv *env = Ecmd.env;
  ERL_NIF_TERM * argv = Ecmd.args;
  wxImage *This;
  This = (wxImage *) memenv->getPtr(env, argv[0], "This");
  int width;
  if(!enif_get_int(env, argv[1], &width)) Badarg("width"); // int
  int height;
  if(!enif_get_int(env, argv[2], &height)) Badarg("height"); // int
  ERL_NIF_TERM lstHead, lstTail;
  lstTail = argv[3];
  if(!enif_is_list(env, lstTail)) Badarg("Options");
  const ERL_NIF_TERM *tpl;
  int tpl_sz;
  while(!enif_is_empty_list(env, lstTail)) {
    if(!enif_get_list_cell(env, lstTail, &lstHead, &lstTail)) Badarg("Options");
    if(!enif_get_tuple(env, lstHead, &tpl_sz, &tpl) || tpl_sz != 2) Badarg("Options");
    if(enif_is_identical(tpl[0], enif_make_atom(env, "clear"))) {
  clear = enif_is_identical(tpl[1], WXE_ATOM_true);
    } else        Badarg("Options");
  };
  if(!This) throw wxe_badarg("This");
  bool Result = This->Create(width,height,clear);
  wxeReturn rt = wxeReturn(memenv, Ecmd.caller, true);
  rt.send(  rt.make_bool(Result));

}

// wxImage::Create
void wxImage_Create_2_1(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd)
{
  bool clear=true;
  ErlNifEnv *env = Ecmd.env;
  ERL_NIF_TERM * argv = Ecmd.args;
  wxImage *This;
  This = (wxImage *) memenv->getPtr(env, argv[0], "This");
  const ERL_NIF_TERM *sz_t;
  int sz_sz;
  if(!enif_get_tuple(env, argv[1], &sz_sz, &sz_t)) Badarg("sz");
  int szW;
  if(!enif_get_int(env, sz_t[0], &szW)) Badarg("sz");
  int szH;
  if(!enif_get_int(env, sz_t[1], &szH)) Badarg("sz");
  wxSize sz = wxSize(szW,szH);
  ERL_NIF_TERM lstHead, lstTail;
  lstTail = argv[2];
  if(!enif_is_list(env, lstTail)) Badarg("Options");
  const ERL_NIF_TERM *tpl;
  int tpl_sz;
  while(!enif_is_empty_list(env, lstTail)) {
    if(!enif_get_list_cell(env, lstTail, &lstHead, &lstTail)) Badarg("Options");
    if(!enif_get_tuple(env, lstHead, &tpl_sz, &tpl) || tpl_sz != 2) Badarg("Options");
    if(enif_is_identical(tpl[0], enif_make_atom(env, "clear"))) {
  clear = enif_is_identical(tpl[1], WXE_ATOM_true);
    } else        Badarg("Options");
  };
  if(!This) throw wxe_badarg("This");
  bool Result = This->Create(sz,clear);
  wxeReturn rt = wxeReturn(memenv, Ecmd.caller, true);
  rt.send(  rt.make_bool(Result));

}

// wxImage::Create
void wxImage_Create_3_0(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd)
{
  ErlNifEnv *env = Ecmd.env;
  ERL_NIF_TERM * argv = Ecmd.args;
  wxImage *This;
  This = (wxImage *) memenv->getPtr(env, argv[0], "This");
  int width;
  if(!enif_get_int(env, argv[1], &width)) Badarg("width"); // int
  int height;
  if(!enif_get_int(env, argv[2], &height)) Badarg("height"); // int
  unsigned char * data;
  ErlNifBinary data_bin;
  if(!enif_inspect_binary(env, argv[3], &data_bin)) Badarg("data");
  data = (unsigned char *) malloc(data_bin.size);
  memcpy(data,data_bin.data,data_bin.size);
  if(!This) throw wxe_badarg("This");
  bool Result = This->Create(width,height,data);
  wxeReturn rt = wxeReturn(memenv, Ecmd.caller, true);
  rt.send(  rt.make_bool(Result));

}

// wxImage::Create
void wxImage_Create_2_0(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd)
{
  ErlNifEnv *env = Ecmd.env;
  ERL_NIF_TERM * argv = Ecmd.args;
  wxImage *This;
  This = (wxImage *) memenv->getPtr(env, argv[0], "This");
  const ERL_NIF_TERM *sz_t;
  int sz_sz;
  if(!enif_get_tuple(env, argv[1], &sz_sz, &sz_t)) Badarg("sz");
  int szW;
  if(!enif_get_int(env, sz_t[0], &szW)) Badarg("sz");
  int szH;
  if(!enif_get_int(env, sz_t[1], &szH)) Badarg("sz");
  wxSize sz = wxSize(szW,szH);
  unsigned char * data;
  ErlNifBinary data_bin;
  if(!enif_inspect_binary(env, argv[2], &data_bin)) Badarg("data");
  data = (unsigned char *) malloc(data_bin.size);
  memcpy(data,data_bin.data,data_bin.size);
  if(!This) throw wxe_badarg("This");
  bool Result = This->Create(sz,data);
  wxeReturn rt = wxeReturn(memenv, Ecmd.caller, true);
  rt.send(  rt.make_bool(Result));

}

// wxImage::Create
void wxImage_Create_4(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd)
{
  ErlNifEnv *env = Ecmd.env;
  ERL_NIF_TERM * argv = Ecmd.args;
  wxImage *This;
  This = (wxImage *) memenv->getPtr(env, argv[0], "This");
  int width;
  if(!enif_get_int(env, argv[1], &width)) Badarg("width"); // int
  int height;
  if(!enif_get_int(env, argv[2], &height)) Badarg("height"); // int
  unsigned char * data;
  ErlNifBinary data_bin;
  if(!enif_inspect_binary(env, argv[3], &data_bin)) Badarg("data");
  data = (unsigned char *) malloc(data_bin.size);
  memcpy(data,data_bin.data,data_bin.size);
  unsigned char * alpha;
  ErlNifBinary alpha_bin;
  if(!enif_inspect_binary(env, argv[4], &alpha_bin)) Badarg("alpha");
  alpha = (unsigned char *) malloc(alpha_bin.size);
  memcpy(alpha,alpha_bin.data,alpha_bin.size);
  if(!This) throw wxe_badarg("This");
  bool Result = This->Create(width,height,data,alpha);
  wxeReturn rt = wxeReturn(memenv, Ecmd.caller, true);
  rt.send(  rt.make_bool(Result));

}

// wxImage::Create
void wxImage_Create_3_2(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd)
{
  ErlNifEnv *env = Ecmd.env;
  ERL_NIF_TERM * argv = Ecmd.args;
  wxImage *This;
  This = (wxImage *) memenv->getPtr(env, argv[0], "This");
  const ERL_NIF_TERM *sz_t;
  int sz_sz;
  if(!enif_get_tuple(env, argv[1], &sz_sz, &sz_t)) Badarg("sz");
  int szW;
  if(!enif_get_int(env, sz_t[0], &szW)) Badarg("sz");
  int szH;
  if(!enif_get_int(env, sz_t[1], &szH)) Badarg("sz");
  wxSize sz = wxSize(szW,szH);
  unsigned char * data;
  ErlNifBinary data_bin;
  if(!enif_inspect_binary(env, argv[2], &data_bin)) Badarg("data");
  data = (unsigned char *) malloc(data_bin.size);
  memcpy(data,data_bin.data,data_bin.size);
  unsigned char * alpha;
  ErlNifBinary alpha_bin;
  if(!enif_inspect_binary(env, argv[3], &alpha_bin)) Badarg("alpha");
  alpha = (unsigned char *) malloc(alpha_bin.size);
  memcpy(alpha,alpha_bin.data,alpha_bin.size);
  if(!This) throw wxe_badarg("This");
  bool Result = This->Create(sz,data,alpha);
  wxeReturn rt = wxeReturn(memenv, Ecmd.caller, true);
  rt.send(  rt.make_bool(Result));

}

// wxImage::Destroy
void wxImage_Destroy(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd)
{
  ErlNifEnv *env = Ecmd.env;
  ERL_NIF_TERM * argv = Ecmd.args;
  wxImage *This;
  This = (wxImage *) memenv->getPtr(env, argv[0], "This");
  if(!This) throw wxe_badarg("This");
  This->Destroy();

}

// wxImage::FindFirstUnusedColour
void wxImage_FindFirstUnusedColour(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd)
{
  unsigned char r;
  unsigned char g;
  unsigned char b;
  unsigned int startR=1;
  unsigned int startG=0;
  unsigned int startB=0;
  ErlNifEnv *env = Ecmd.env;
  ERL_NIF_TERM * argv = Ecmd.args;
  wxImage *This;
  This = (wxImage *) memenv->getPtr(env, argv[0], "This");
  ERL_NIF_TERM lstHead, lstTail;
  lstTail = argv[1];
  if(!enif_is_list(env, lstTail)) Badarg("Options");
  const ERL_NIF_TERM *tpl;
  int tpl_sz;
  while(!enif_is_empty_list(env, lstTail)) {
    if(!enif_get_list_cell(env, lstTail, &lstHead, &lstTail)) Badarg("Options");
    if(!enif_get_tuple(env, lstHead, &tpl_sz, &tpl) || tpl_sz != 2) Badarg("Options");
    if(enif_is_identical(tpl[0], enif_make_atom(env, "startR"))) {
  if(!enif_get_uint(env, tpl[1], &startR)) Badarg("startR");
    } else     if(enif_is_identical(tpl[0], enif_make_atom(env, "startG"))) {
  if(!enif_get_uint(env, tpl[1], &startG)) Badarg("startG");
    } else     if(enif_is_identical(tpl[0], enif_make_atom(env, "startB"))) {
  if(!enif_get_uint(env, tpl[1], &startB)) Badarg("startB");
    } else        Badarg("Options");
  };
  if(!This) throw wxe_badarg("This");
  bool Result = This->FindFirstUnusedColour(&r,&g,&b,startR,startG,startB);
  wxeReturn rt = wxeReturn(memenv, Ecmd.caller, true);
  ERL_NIF_TERM msg = enif_make_tuple4(rt.env,
  rt.make_bool(Result),
    rt.make_uint(r),
  rt.make_uint(g),
  rt.make_uint(b));
  rt.send(msg);

}

// wxImage::GetImageExtWildcard
void wxImage_GetImageExtWildcard(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd)
{
  wxString Result = wxImage::GetImageExtWildcard();
  wxeReturn rt = wxeReturn(memenv, Ecmd.caller, true);
  rt.send(  rt.make(Result));

}

// wxImage::GetAlpha
void wxImage_GetAlpha_0(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd)
{
  ErlNifEnv *env = Ecmd.env;
  ERL_NIF_TERM * argv = Ecmd.args;
  wxImage *This;
  This = (wxImage *) memenv->getPtr(env, argv[0], "This");
  if(!This) throw wxe_badarg("This");
  char * Result = (char*)This->GetAlpha();
  wxeReturn rt = wxeReturn(memenv, Ecmd.caller, true);
  rt.send(  rt.make_binary(Result, (This->GetWidth()*This->GetHeight())));

}

// wxImage::GetAlpha
void wxImage_GetAlpha_2(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd)
{
  ErlNifEnv *env = Ecmd.env;
  ERL_NIF_TERM * argv = Ecmd.args;
  wxImage *This;
  This = (wxImage *) memenv->getPtr(env, argv[0], "This");
  int x;
  if(!enif_get_int(env, argv[1], &x)) Badarg("x"); // int
  int y;
  if(!enif_get_int(env, argv[2], &y)) Badarg("y"); // int
  if(!This) throw wxe_badarg("This");
  char Result = This->GetAlpha(x,y);
  wxeReturn rt = wxeReturn(memenv, Ecmd.caller, true);
  rt.send(  rt.make_uint(Result));

}

// wxImage::GetBlue
void wxImage_GetBlue(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd)
{
  ErlNifEnv *env = Ecmd.env;
  ERL_NIF_TERM * argv = Ecmd.args;
  wxImage *This;
  This = (wxImage *) memenv->getPtr(env, argv[0], "This");
  int x;
  if(!enif_get_int(env, argv[1], &x)) Badarg("x"); // int
  int y;
  if(!enif_get_int(env, argv[2], &y)) Badarg("y"); // int
  if(!This) throw wxe_badarg("This");
  char Result = This->GetBlue(x,y);
  wxeReturn rt = wxeReturn(memenv, Ecmd.caller, true);
  rt.send(  rt.make_uint(Result));

}

// wxImage::GetData
void wxImage_GetData(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd)
{
  ErlNifEnv *env = Ecmd.env;
  ERL_NIF_TERM * argv = Ecmd.args;
  wxImage *This;
  This = (wxImage *) memenv->getPtr(env, argv[0], "This");
  if(!This) throw wxe_badarg("This");
  char * Result = (char*)This->GetData();
  wxeReturn rt = wxeReturn(memenv, Ecmd.caller, true);
  rt.send(  rt.make_binary(Result, (This->GetWidth()*This->GetHeight()*3)));

}

// wxImage::GetGreen
void wxImage_GetGreen(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd)
{
  ErlNifEnv *env = Ecmd.env;
  ERL_NIF_TERM * argv = Ecmd.args;
  wxImage *This;
  This = (wxImage *) memenv->getPtr(env, argv[0], "This");
  int x;
  if(!enif_get_int(env, argv[1], &x)) Badarg("x"); // int
  int y;
  if(!enif_get_int(env, argv[2], &y)) Badarg("y"); // int
  if(!This) throw wxe_badarg("This");
  char Result = This->GetGreen(x,y);
  wxeReturn rt = wxeReturn(memenv, Ecmd.caller, true);
  rt.send(  rt.make_uint(Result));

}

// wxImage::GetImageCount
void wxImage_GetImageCount(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd)
{
 wxBitmapType type=wxBITMAP_TYPE_ANY;
  ErlNifEnv *env = Ecmd.env;
  ERL_NIF_TERM * argv = Ecmd.args;
  ErlNifBinary filename_bin;
  wxString filename;
  if(!enif_inspect_binary(env, argv[0], &filename_bin)) Badarg("filename");
  filename = wxString(filename_bin.data, wxConvUTF8, filename_bin.size);
  ERL_NIF_TERM lstHead, lstTail;
  lstTail = argv[1];
  if(!enif_is_list(env, lstTail)) Badarg("Options");
  const ERL_NIF_TERM *tpl;
  int tpl_sz;
  while(!enif_is_empty_list(env, lstTail)) {
    if(!enif_get_list_cell(env, lstTail, &lstHead, &lstTail)) Badarg("Options");
    if(!enif_get_tuple(env, lstHead, &tpl_sz, &tpl) || tpl_sz != 2) Badarg("Options");
    if(enif_is_identical(tpl[0], enif_make_atom(env, "type"))) {
  if(!enif_get_int(env, tpl[1], (int *) &type)) Badarg("type"); // enum
    } else        Badarg("Options");
  };
  int Result = wxImage::GetImageCount(filename,type);
  wxeReturn rt = wxeReturn(memenv, Ecmd.caller, true);
  rt.send(  rt.make_int(Result));

}

// wxImage::GetHeight
void wxImage_GetHeight(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd)
{
  ErlNifEnv *env = Ecmd.env;
  ERL_NIF_TERM * argv = Ecmd.args;
  wxImage *This;
  This = (wxImage *) memenv->getPtr(env, argv[0], "This");
  if(!This) throw wxe_badarg("This");
  int Result = This->GetHeight();
  wxeReturn rt = wxeReturn(memenv, Ecmd.caller, true);
  rt.send(  rt.make_int(Result));

}

// wxImage::GetMaskBlue
void wxImage_GetMaskBlue(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd)
{
  ErlNifEnv *env = Ecmd.env;
  ERL_NIF_TERM * argv = Ecmd.args;
  wxImage *This;
  This = (wxImage *) memenv->getPtr(env, argv[0], "This");
  if(!This) throw wxe_badarg("This");
  char Result = This->GetMaskBlue();
  wxeReturn rt = wxeReturn(memenv, Ecmd.caller, true);
  rt.send(  rt.make_uint(Result));

}

// wxImage::GetMaskGreen
void wxImage_GetMaskGreen(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd)
{
  ErlNifEnv *env = Ecmd.env;
  ERL_NIF_TERM * argv = Ecmd.args;
  wxImage *This;
  This = (wxImage *) memenv->getPtr(env, argv[0], "This");
  if(!This) throw wxe_badarg("This");
  char Result = This->GetMaskGreen();
  wxeReturn rt = wxeReturn(memenv, Ecmd.caller, true);
  rt.send(  rt.make_uint(Result));

}

// wxImage::GetMaskRed
void wxImage_GetMaskRed(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd)
{
  ErlNifEnv *env = Ecmd.env;
  ERL_NIF_TERM * argv = Ecmd.args;
  wxImage *This;
  This = (wxImage *) memenv->getPtr(env, argv[0], "This");
  if(!This) throw wxe_badarg("This");
  char Result = This->GetMaskRed();
  wxeReturn rt = wxeReturn(memenv, Ecmd.caller, true);
  rt.send(  rt.make_uint(Result));

}

// wxImage::GetOrFindMaskColour
void wxImage_GetOrFindMaskColour(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd)
{
  unsigned char r;
  unsigned char g;
  unsigned char b;
  ErlNifEnv *env = Ecmd.env;
  ERL_NIF_TERM * argv = Ecmd.args;
  wxImage *This;
  This = (wxImage *) memenv->getPtr(env, argv[0], "This");
  if(!This) throw wxe_badarg("This");
  bool Result = This->GetOrFindMaskColour(&r,&g,&b);
  wxeReturn rt = wxeReturn(memenv, Ecmd.caller, true);
  ERL_NIF_TERM msg = enif_make_tuple4(rt.env,
  rt.make_bool(Result),
    rt.make_uint(r),
  rt.make_uint(g),
  rt.make_uint(b));
  rt.send(msg);

}

// wxImage::GetPalette
void wxImage_GetPalette(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd)
{
  ErlNifEnv *env = Ecmd.env;
  ERL_NIF_TERM * argv = Ecmd.args;
  wxImage *This;
  This = (wxImage *) memenv->getPtr(env, argv[0], "This");
  if(!This) throw wxe_badarg("This");
  const wxPalette * Result = &This->GetPalette();
  wxeReturn rt = wxeReturn(memenv, Ecmd.caller, true);
  rt.send(  rt.make_ref(app->getRef((void *)Result,memenv), "wxPalette"));

}

// wxImage::GetRed
void wxImage_GetRed(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd)
{
  ErlNifEnv *env = Ecmd.env;
  ERL_NIF_TERM * argv = Ecmd.args;
  wxImage *This;
  This = (wxImage *) memenv->getPtr(env, argv[0], "This");
  int x;
  if(!enif_get_int(env, argv[1], &x)) Badarg("x"); // int
  int y;
  if(!enif_get_int(env, argv[2], &y)) Badarg("y"); // int
  if(!This) throw wxe_badarg("This");
  char Result = This->GetRed(x,y);
  wxeReturn rt = wxeReturn(memenv, Ecmd.caller, true);
  rt.send(  rt.make_uint(Result));

}

// wxImage::GetSubImage
void wxImage_GetSubImage(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd)
{
  ErlNifEnv *env = Ecmd.env;
  ERL_NIF_TERM * argv = Ecmd.args;
  wxImage *This;
  This = (wxImage *) memenv->getPtr(env, argv[0], "This");
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
  wxImage * Result = new EwxImage(This->GetSubImage(rect)); app->newPtr((void *) Result,3, memenv);;
  wxeReturn rt = wxeReturn(memenv, Ecmd.caller, true);
  rt.send(  rt.make_ref(app->getRef((void *)Result,memenv), "wxImage"));

}

// wxImage::GetWidth
void wxImage_GetWidth(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd)
{
  ErlNifEnv *env = Ecmd.env;
  ERL_NIF_TERM * argv = Ecmd.args;
  wxImage *This;
  This = (wxImage *) memenv->getPtr(env, argv[0], "This");
  if(!This) throw wxe_badarg("This");
  int Result = This->GetWidth();
  wxeReturn rt = wxeReturn(memenv, Ecmd.caller, true);
  rt.send(  rt.make_int(Result));

}

// wxImage::HasAlpha
void wxImage_HasAlpha(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd)
{
  ErlNifEnv *env = Ecmd.env;
  ERL_NIF_TERM * argv = Ecmd.args;
  wxImage *This;
  This = (wxImage *) memenv->getPtr(env, argv[0], "This");
  if(!This) throw wxe_badarg("This");
  bool Result = This->HasAlpha();
  wxeReturn rt = wxeReturn(memenv, Ecmd.caller, true);
  rt.send(  rt.make_bool(Result));

}

// wxImage::HasMask
void wxImage_HasMask(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd)
{
  ErlNifEnv *env = Ecmd.env;
  ERL_NIF_TERM * argv = Ecmd.args;
  wxImage *This;
  This = (wxImage *) memenv->getPtr(env, argv[0], "This");
  if(!This) throw wxe_badarg("This");
  bool Result = This->HasMask();
  wxeReturn rt = wxeReturn(memenv, Ecmd.caller, true);
  rt.send(  rt.make_bool(Result));

}

// wxImage::GetOption
void wxImage_GetOption(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd)
{
  ErlNifEnv *env = Ecmd.env;
  ERL_NIF_TERM * argv = Ecmd.args;
  wxImage *This;
  This = (wxImage *) memenv->getPtr(env, argv[0], "This");
  ErlNifBinary name_bin;
  wxString name;
  if(!enif_inspect_binary(env, argv[1], &name_bin)) Badarg("name");
  name = wxString(name_bin.data, wxConvUTF8, name_bin.size);
  if(!This) throw wxe_badarg("This");
  wxString Result = This->GetOption(name);
  wxeReturn rt = wxeReturn(memenv, Ecmd.caller, true);
  rt.send(  rt.make(Result));

}

// wxImage::GetOptionInt
void wxImage_GetOptionInt(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd)
{
  ErlNifEnv *env = Ecmd.env;
  ERL_NIF_TERM * argv = Ecmd.args;
  wxImage *This;
  This = (wxImage *) memenv->getPtr(env, argv[0], "This");
  ErlNifBinary name_bin;
  wxString name;
  if(!enif_inspect_binary(env, argv[1], &name_bin)) Badarg("name");
  name = wxString(name_bin.data, wxConvUTF8, name_bin.size);
  if(!This) throw wxe_badarg("This");
  int Result = This->GetOptionInt(name);
  wxeReturn rt = wxeReturn(memenv, Ecmd.caller, true);
  rt.send(  rt.make_int(Result));

}

// wxImage::HasOption
void wxImage_HasOption(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd)
{
  ErlNifEnv *env = Ecmd.env;
  ERL_NIF_TERM * argv = Ecmd.args;
  wxImage *This;
  This = (wxImage *) memenv->getPtr(env, argv[0], "This");
  ErlNifBinary name_bin;
  wxString name;
  if(!enif_inspect_binary(env, argv[1], &name_bin)) Badarg("name");
  name = wxString(name_bin.data, wxConvUTF8, name_bin.size);
  if(!This) throw wxe_badarg("This");
  bool Result = This->HasOption(name);
  wxeReturn rt = wxeReturn(memenv, Ecmd.caller, true);
  rt.send(  rt.make_bool(Result));

}

// wxImage::InitAlpha
void wxImage_InitAlpha(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd)
{
  ErlNifEnv *env = Ecmd.env;
  ERL_NIF_TERM * argv = Ecmd.args;
  wxImage *This;
  This = (wxImage *) memenv->getPtr(env, argv[0], "This");
  if(!This) throw wxe_badarg("This");
  This->InitAlpha();

}

// wxImage::InitStandardHandlers
void wxImage_InitStandardHandlers(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd)
{
  wxImage::InitStandardHandlers();

}

// wxImage::IsTransparent
void wxImage_IsTransparent(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd)
{
  unsigned int threshold=wxIMAGE_ALPHA_THRESHOLD;
  ErlNifEnv *env = Ecmd.env;
  ERL_NIF_TERM * argv = Ecmd.args;
  wxImage *This;
  This = (wxImage *) memenv->getPtr(env, argv[0], "This");
  int x;
  if(!enif_get_int(env, argv[1], &x)) Badarg("x"); // int
  int y;
  if(!enif_get_int(env, argv[2], &y)) Badarg("y"); // int
  ERL_NIF_TERM lstHead, lstTail;
  lstTail = argv[3];
  if(!enif_is_list(env, lstTail)) Badarg("Options");
  const ERL_NIF_TERM *tpl;
  int tpl_sz;
  while(!enif_is_empty_list(env, lstTail)) {
    if(!enif_get_list_cell(env, lstTail, &lstHead, &lstTail)) Badarg("Options");
    if(!enif_get_tuple(env, lstHead, &tpl_sz, &tpl) || tpl_sz != 2) Badarg("Options");
    if(enif_is_identical(tpl[0], enif_make_atom(env, "threshold"))) {
  if(!enif_get_uint(env, tpl[1], &threshold)) Badarg("threshold");
    } else        Badarg("Options");
  };
  if(!This) throw wxe_badarg("This");
  bool Result = This->IsTransparent(x,y,threshold);
  wxeReturn rt = wxeReturn(memenv, Ecmd.caller, true);
  rt.send(  rt.make_bool(Result));

}

// wxImage::LoadFile
void wxImage_LoadFile_2(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd)
{
 wxBitmapType type=wxBITMAP_TYPE_ANY;
  int index=-1;
  ErlNifEnv *env = Ecmd.env;
  ERL_NIF_TERM * argv = Ecmd.args;
  wxImage *This;
  This = (wxImage *) memenv->getPtr(env, argv[0], "This");
  ErlNifBinary name_bin;
  wxString name;
  if(!enif_inspect_binary(env, argv[1], &name_bin)) Badarg("name");
  name = wxString(name_bin.data, wxConvUTF8, name_bin.size);
  ERL_NIF_TERM lstHead, lstTail;
  lstTail = argv[2];
  if(!enif_is_list(env, lstTail)) Badarg("Options");
  const ERL_NIF_TERM *tpl;
  int tpl_sz;
  while(!enif_is_empty_list(env, lstTail)) {
    if(!enif_get_list_cell(env, lstTail, &lstHead, &lstTail)) Badarg("Options");
    if(!enif_get_tuple(env, lstHead, &tpl_sz, &tpl) || tpl_sz != 2) Badarg("Options");
    if(enif_is_identical(tpl[0], enif_make_atom(env, "type"))) {
  if(!enif_get_int(env, tpl[1], (int *) &type)) Badarg("type"); // enum
    } else     if(enif_is_identical(tpl[0], enif_make_atom(env, "index"))) {
  if(!enif_get_int(env, tpl[1], &index)) Badarg("index"); // int
    } else        Badarg("Options");
  };
  if(!This) throw wxe_badarg("This");
  bool Result = This->LoadFile(name,type,index);
  wxeReturn rt = wxeReturn(memenv, Ecmd.caller, true);
  rt.send(  rt.make_bool(Result));

}

// wxImage::LoadFile
void wxImage_LoadFile_3(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd)
{
  int index=-1;
  ErlNifEnv *env = Ecmd.env;
  ERL_NIF_TERM * argv = Ecmd.args;
  wxImage *This;
  This = (wxImage *) memenv->getPtr(env, argv[0], "This");
  ErlNifBinary name_bin;
  wxString name;
  if(!enif_inspect_binary(env, argv[1], &name_bin)) Badarg("name");
  name = wxString(name_bin.data, wxConvUTF8, name_bin.size);
  ErlNifBinary mimetype_bin;
  wxString mimetype;
  if(!enif_inspect_binary(env, argv[2], &mimetype_bin)) Badarg("mimetype");
  mimetype = wxString(mimetype_bin.data, wxConvUTF8, mimetype_bin.size);
  ERL_NIF_TERM lstHead, lstTail;
  lstTail = argv[3];
  if(!enif_is_list(env, lstTail)) Badarg("Options");
  const ERL_NIF_TERM *tpl;
  int tpl_sz;
  while(!enif_is_empty_list(env, lstTail)) {
    if(!enif_get_list_cell(env, lstTail, &lstHead, &lstTail)) Badarg("Options");
    if(!enif_get_tuple(env, lstHead, &tpl_sz, &tpl) || tpl_sz != 2) Badarg("Options");
    if(enif_is_identical(tpl[0], enif_make_atom(env, "index"))) {
  if(!enif_get_int(env, tpl[1], &index)) Badarg("index"); // int
    } else        Badarg("Options");
  };
  if(!This) throw wxe_badarg("This");
  bool Result = This->LoadFile(name,mimetype,index);
  wxeReturn rt = wxeReturn(memenv, Ecmd.caller, true);
  rt.send(  rt.make_bool(Result));

}

// wxImage::IsOk
void wxImage_IsOk(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd)
{
  ErlNifEnv *env = Ecmd.env;
  ERL_NIF_TERM * argv = Ecmd.args;
  wxImage *This;
  This = (wxImage *) memenv->getPtr(env, argv[0], "This");
  if(!This) throw wxe_badarg("This");
  bool Result = This->IsOk();
  wxeReturn rt = wxeReturn(memenv, Ecmd.caller, true);
  rt.send(  rt.make_bool(Result));

}

// wxImage::RemoveHandler
void wxImage_RemoveHandler(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd)
{
  ErlNifEnv *env = Ecmd.env;
  ERL_NIF_TERM * argv = Ecmd.args;
  ErlNifBinary name_bin;
  wxString name;
  if(!enif_inspect_binary(env, argv[0], &name_bin)) Badarg("name");
  name = wxString(name_bin.data, wxConvUTF8, name_bin.size);
  bool Result = wxImage::RemoveHandler(name);
  wxeReturn rt = wxeReturn(memenv, Ecmd.caller, true);
  rt.send(  rt.make_bool(Result));

}

// wxImage::Mirror
void wxImage_Mirror(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd)
{
  bool horizontally=true;
  ErlNifEnv *env = Ecmd.env;
  ERL_NIF_TERM * argv = Ecmd.args;
  wxImage *This;
  This = (wxImage *) memenv->getPtr(env, argv[0], "This");
  ERL_NIF_TERM lstHead, lstTail;
  lstTail = argv[1];
  if(!enif_is_list(env, lstTail)) Badarg("Options");
  const ERL_NIF_TERM *tpl;
  int tpl_sz;
  while(!enif_is_empty_list(env, lstTail)) {
    if(!enif_get_list_cell(env, lstTail, &lstHead, &lstTail)) Badarg("Options");
    if(!enif_get_tuple(env, lstHead, &tpl_sz, &tpl) || tpl_sz != 2) Badarg("Options");
    if(enif_is_identical(tpl[0], enif_make_atom(env, "horizontally"))) {
  horizontally = enif_is_identical(tpl[1], WXE_ATOM_true);
    } else        Badarg("Options");
  };
  if(!This) throw wxe_badarg("This");
  wxImage * Result = new EwxImage(This->Mirror(horizontally)); app->newPtr((void *) Result,3, memenv);;
  wxeReturn rt = wxeReturn(memenv, Ecmd.caller, true);
  rt.send(  rt.make_ref(app->getRef((void *)Result,memenv), "wxImage"));

}

// wxImage::Replace
void wxImage_Replace(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd)
{
  ErlNifEnv *env = Ecmd.env;
  ERL_NIF_TERM * argv = Ecmd.args;
  wxImage *This;
  This = (wxImage *) memenv->getPtr(env, argv[0], "This");
  unsigned int r1;
  if(!enif_get_uint(env, argv[1], &r1)) Badarg("r1");
  unsigned int g1;
  if(!enif_get_uint(env, argv[2], &g1)) Badarg("g1");
  unsigned int b1;
  if(!enif_get_uint(env, argv[3], &b1)) Badarg("b1");
  unsigned int r2;
  if(!enif_get_uint(env, argv[4], &r2)) Badarg("r2");
  unsigned int g2;
  if(!enif_get_uint(env, argv[5], &g2)) Badarg("g2");
  unsigned int b2;
  if(!enif_get_uint(env, argv[6], &b2)) Badarg("b2");
  if(!This) throw wxe_badarg("This");
  This->Replace(r1,g1,b1,r2,g2,b2);

}

// wxImage::Rescale
void wxImage_Rescale(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd)
{
 wxImageResizeQuality quality=wxIMAGE_QUALITY_NORMAL;
  ErlNifEnv *env = Ecmd.env;
  ERL_NIF_TERM * argv = Ecmd.args;
  wxImage *This;
  This = (wxImage *) memenv->getPtr(env, argv[0], "This");
  int width;
  if(!enif_get_int(env, argv[1], &width)) Badarg("width"); // int
  int height;
  if(!enif_get_int(env, argv[2], &height)) Badarg("height"); // int
  ERL_NIF_TERM lstHead, lstTail;
  lstTail = argv[3];
  if(!enif_is_list(env, lstTail)) Badarg("Options");
  const ERL_NIF_TERM *tpl;
  int tpl_sz;
  while(!enif_is_empty_list(env, lstTail)) {
    if(!enif_get_list_cell(env, lstTail, &lstHead, &lstTail)) Badarg("Options");
    if(!enif_get_tuple(env, lstHead, &tpl_sz, &tpl) || tpl_sz != 2) Badarg("Options");
    if(enif_is_identical(tpl[0], enif_make_atom(env, "quality"))) {
  if(!enif_get_int(env, tpl[1], (int *) &quality)) Badarg("quality"); // enum
    } else        Badarg("Options");
  };
  if(!This) throw wxe_badarg("This");
  wxImage * Result = &This->Rescale(width,height,quality);
  wxeReturn rt = wxeReturn(memenv, Ecmd.caller, true);
  rt.send(  rt.make_ref(app->getRef((void *)Result,memenv), "wxImage"));

}

// wxImage::Resize
void wxImage_Resize(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd)
{
  int r=-1;
  int g=-1;
  int b=-1;
  ErlNifEnv *env = Ecmd.env;
  ERL_NIF_TERM * argv = Ecmd.args;
  wxImage *This;
  This = (wxImage *) memenv->getPtr(env, argv[0], "This");
  const ERL_NIF_TERM *size_t;
  int size_sz;
  if(!enif_get_tuple(env, argv[1], &size_sz, &size_t)) Badarg("size");
  int sizeW;
  if(!enif_get_int(env, size_t[0], &sizeW)) Badarg("size");
  int sizeH;
  if(!enif_get_int(env, size_t[1], &sizeH)) Badarg("size");
  wxSize size = wxSize(sizeW,sizeH);
  const ERL_NIF_TERM *pos_t;
  int pos_sz;
  if(!enif_get_tuple(env, argv[2], &pos_sz, &pos_t)) Badarg("pos");
  int posX;
  if(!enif_get_int(env, pos_t[0], &posX)) Badarg("pos");
  int posY;
  if(!enif_get_int(env, pos_t[1], &posY)) Badarg("pos");
  wxPoint pos = wxPoint(posX,posY);
  ERL_NIF_TERM lstHead, lstTail;
  lstTail = argv[3];
  if(!enif_is_list(env, lstTail)) Badarg("Options");
  const ERL_NIF_TERM *tpl;
  int tpl_sz;
  while(!enif_is_empty_list(env, lstTail)) {
    if(!enif_get_list_cell(env, lstTail, &lstHead, &lstTail)) Badarg("Options");
    if(!enif_get_tuple(env, lstHead, &tpl_sz, &tpl) || tpl_sz != 2) Badarg("Options");
    if(enif_is_identical(tpl[0], enif_make_atom(env, "r"))) {
  if(!enif_get_int(env, tpl[1], &r)) Badarg("r"); // int
    } else     if(enif_is_identical(tpl[0], enif_make_atom(env, "g"))) {
  if(!enif_get_int(env, tpl[1], &g)) Badarg("g"); // int
    } else     if(enif_is_identical(tpl[0], enif_make_atom(env, "b"))) {
  if(!enif_get_int(env, tpl[1], &b)) Badarg("b"); // int
    } else        Badarg("Options");
  };
  if(!This) throw wxe_badarg("This");
  wxImage * Result = &This->Resize(size,pos,r,g,b);
  wxeReturn rt = wxeReturn(memenv, Ecmd.caller, true);
  rt.send(  rt.make_ref(app->getRef((void *)Result,memenv), "wxImage"));

}

// wxImage::Rotate
void wxImage_Rotate(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd)
{
  bool interpolating=true;
  wxPoint *offset_after_rotation=NULL; wxPoint offset_after_rotationTmp;
  ErlNifEnv *env = Ecmd.env;
  ERL_NIF_TERM * argv = Ecmd.args;
  wxImage *This;
  This = (wxImage *) memenv->getPtr(env, argv[0], "This");
  double angle;
  if(!wxe_get_double(env, argv[1], &angle)) Badarg("angle");
  const ERL_NIF_TERM *rotationCentre_t;
  int rotationCentre_sz;
  if(!enif_get_tuple(env, argv[2], &rotationCentre_sz, &rotationCentre_t)) Badarg("rotationCentre");
  int rotationCentreX;
  if(!enif_get_int(env, rotationCentre_t[0], &rotationCentreX)) Badarg("rotationCentre");
  int rotationCentreY;
  if(!enif_get_int(env, rotationCentre_t[1], &rotationCentreY)) Badarg("rotationCentre");
  wxPoint rotationCentre = wxPoint(rotationCentreX,rotationCentreY);
  ERL_NIF_TERM lstHead, lstTail;
  lstTail = argv[3];
  if(!enif_is_list(env, lstTail)) Badarg("Options");
  const ERL_NIF_TERM *tpl;
  int tpl_sz;
  while(!enif_is_empty_list(env, lstTail)) {
    if(!enif_get_list_cell(env, lstTail, &lstHead, &lstTail)) Badarg("Options");
    if(!enif_get_tuple(env, lstHead, &tpl_sz, &tpl) || tpl_sz != 2) Badarg("Options");
    if(enif_is_identical(tpl[0], enif_make_atom(env, "interpolating"))) {
  interpolating = enif_is_identical(tpl[1], WXE_ATOM_true);
    } else     if(enif_is_identical(tpl[0], enif_make_atom(env, "offset_after_rotation"))) {
  const ERL_NIF_TERM *offset_after_rotation_t;
  int offset_after_rotation_sz;
  if(!enif_get_tuple(env, tpl[1], &offset_after_rotation_sz, &offset_after_rotation_t)) Badarg("offset_after_rotation");
  int offset_after_rotationX;
  if(!enif_get_int(env, offset_after_rotation_t[0], &offset_after_rotationX)) Badarg("offset_after_rotation");
  int offset_after_rotationY;
  if(!enif_get_int(env, offset_after_rotation_t[1], &offset_after_rotationY)) Badarg("offset_after_rotation");
  offset_after_rotationTmp = wxPoint(offset_after_rotationX,offset_after_rotationY); offset_after_rotation = & offset_after_rotationTmp;
    } else        Badarg("Options");
  };
  if(!This) throw wxe_badarg("This");
  wxImage * Result = new EwxImage(This->Rotate(angle,rotationCentre,interpolating,offset_after_rotation)); app->newPtr((void *) Result,3, memenv);;
  wxeReturn rt = wxeReturn(memenv, Ecmd.caller, true);
  rt.send(  rt.make_ref(app->getRef((void *)Result,memenv), "wxImage"));

}

// wxImage::RotateHue
void wxImage_RotateHue(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd)
{
  ErlNifEnv *env = Ecmd.env;
  ERL_NIF_TERM * argv = Ecmd.args;
  wxImage *This;
  This = (wxImage *) memenv->getPtr(env, argv[0], "This");
  double angle;
  if(!wxe_get_double(env, argv[1], &angle)) Badarg("angle");
  if(!This) throw wxe_badarg("This");
  This->RotateHue(angle);

}

// wxImage::Rotate90
void wxImage_Rotate90(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd)
{
  bool clockwise=true;
  ErlNifEnv *env = Ecmd.env;
  ERL_NIF_TERM * argv = Ecmd.args;
  wxImage *This;
  This = (wxImage *) memenv->getPtr(env, argv[0], "This");
  ERL_NIF_TERM lstHead, lstTail;
  lstTail = argv[1];
  if(!enif_is_list(env, lstTail)) Badarg("Options");
  const ERL_NIF_TERM *tpl;
  int tpl_sz;
  while(!enif_is_empty_list(env, lstTail)) {
    if(!enif_get_list_cell(env, lstTail, &lstHead, &lstTail)) Badarg("Options");
    if(!enif_get_tuple(env, lstHead, &tpl_sz, &tpl) || tpl_sz != 2) Badarg("Options");
    if(enif_is_identical(tpl[0], enif_make_atom(env, "clockwise"))) {
  clockwise = enif_is_identical(tpl[1], WXE_ATOM_true);
    } else        Badarg("Options");
  };
  if(!This) throw wxe_badarg("This");
  wxImage * Result = new EwxImage(This->Rotate90(clockwise)); app->newPtr((void *) Result,3, memenv);;
  wxeReturn rt = wxeReturn(memenv, Ecmd.caller, true);
  rt.send(  rt.make_ref(app->getRef((void *)Result,memenv), "wxImage"));

}

// wxImage::SaveFile
void wxImage_SaveFile_2_0(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd)
{
  ErlNifEnv *env = Ecmd.env;
  ERL_NIF_TERM * argv = Ecmd.args;
  wxImage *This;
  This = (wxImage *) memenv->getPtr(env, argv[0], "This");
  ErlNifBinary name_bin;
  wxString name;
  if(!enif_inspect_binary(env, argv[1], &name_bin)) Badarg("name");
  name = wxString(name_bin.data, wxConvUTF8, name_bin.size);
  wxBitmapType type;
  if(!enif_get_int(env, argv[2], (int *) &type)) Badarg("type"); // enum
  if(!This) throw wxe_badarg("This");
  bool Result = This->SaveFile(name,type);
  wxeReturn rt = wxeReturn(memenv, Ecmd.caller, true);
  rt.send(  rt.make_bool(Result));

}

// wxImage::SaveFile
void wxImage_SaveFile_2_1(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd)
{
  ErlNifEnv *env = Ecmd.env;
  ERL_NIF_TERM * argv = Ecmd.args;
  wxImage *This;
  This = (wxImage *) memenv->getPtr(env, argv[0], "This");
  ErlNifBinary name_bin;
  wxString name;
  if(!enif_inspect_binary(env, argv[1], &name_bin)) Badarg("name");
  name = wxString(name_bin.data, wxConvUTF8, name_bin.size);
  ErlNifBinary mimetype_bin;
  wxString mimetype;
  if(!enif_inspect_binary(env, argv[2], &mimetype_bin)) Badarg("mimetype");
  mimetype = wxString(mimetype_bin.data, wxConvUTF8, mimetype_bin.size);
  if(!This) throw wxe_badarg("This");
  bool Result = This->SaveFile(name,mimetype);
  wxeReturn rt = wxeReturn(memenv, Ecmd.caller, true);
  rt.send(  rt.make_bool(Result));

}

// wxImage::SaveFile
void wxImage_SaveFile_1(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd)
{
  ErlNifEnv *env = Ecmd.env;
  ERL_NIF_TERM * argv = Ecmd.args;
  wxImage *This;
  This = (wxImage *) memenv->getPtr(env, argv[0], "This");
  ErlNifBinary name_bin;
  wxString name;
  if(!enif_inspect_binary(env, argv[1], &name_bin)) Badarg("name");
  name = wxString(name_bin.data, wxConvUTF8, name_bin.size);
  if(!This) throw wxe_badarg("This");
  bool Result = This->SaveFile(name);
  wxeReturn rt = wxeReturn(memenv, Ecmd.caller, true);
  rt.send(  rt.make_bool(Result));

}

// wxImage::Scale
void wxImage_Scale(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd)
{
 wxImageResizeQuality quality=wxIMAGE_QUALITY_NORMAL;
  ErlNifEnv *env = Ecmd.env;
  ERL_NIF_TERM * argv = Ecmd.args;
  wxImage *This;
  This = (wxImage *) memenv->getPtr(env, argv[0], "This");
  int width;
  if(!enif_get_int(env, argv[1], &width)) Badarg("width"); // int
  int height;
  if(!enif_get_int(env, argv[2], &height)) Badarg("height"); // int
  ERL_NIF_TERM lstHead, lstTail;
  lstTail = argv[3];
  if(!enif_is_list(env, lstTail)) Badarg("Options");
  const ERL_NIF_TERM *tpl;
  int tpl_sz;
  while(!enif_is_empty_list(env, lstTail)) {
    if(!enif_get_list_cell(env, lstTail, &lstHead, &lstTail)) Badarg("Options");
    if(!enif_get_tuple(env, lstHead, &tpl_sz, &tpl) || tpl_sz != 2) Badarg("Options");
    if(enif_is_identical(tpl[0], enif_make_atom(env, "quality"))) {
  if(!enif_get_int(env, tpl[1], (int *) &quality)) Badarg("quality"); // enum
    } else        Badarg("Options");
  };
  if(!This) throw wxe_badarg("This");
  wxImage * Result = new EwxImage(This->Scale(width,height,quality)); app->newPtr((void *) Result,3, memenv);;
  wxeReturn rt = wxeReturn(memenv, Ecmd.caller, true);
  rt.send(  rt.make_ref(app->getRef((void *)Result,memenv), "wxImage"));

}

// wxImage::Size
void wxImage_Size(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd)
{
  int r=-1;
  int g=-1;
  int b=-1;
  ErlNifEnv *env = Ecmd.env;
  ERL_NIF_TERM * argv = Ecmd.args;
  wxImage *This;
  This = (wxImage *) memenv->getPtr(env, argv[0], "This");
  const ERL_NIF_TERM *size_t;
  int size_sz;
  if(!enif_get_tuple(env, argv[1], &size_sz, &size_t)) Badarg("size");
  int sizeW;
  if(!enif_get_int(env, size_t[0], &sizeW)) Badarg("size");
  int sizeH;
  if(!enif_get_int(env, size_t[1], &sizeH)) Badarg("size");
  wxSize size = wxSize(sizeW,sizeH);
  const ERL_NIF_TERM *pos_t;
  int pos_sz;
  if(!enif_get_tuple(env, argv[2], &pos_sz, &pos_t)) Badarg("pos");
  int posX;
  if(!enif_get_int(env, pos_t[0], &posX)) Badarg("pos");
  int posY;
  if(!enif_get_int(env, pos_t[1], &posY)) Badarg("pos");
  wxPoint pos = wxPoint(posX,posY);
  ERL_NIF_TERM lstHead, lstTail;
  lstTail = argv[3];
  if(!enif_is_list(env, lstTail)) Badarg("Options");
  const ERL_NIF_TERM *tpl;
  int tpl_sz;
  while(!enif_is_empty_list(env, lstTail)) {
    if(!enif_get_list_cell(env, lstTail, &lstHead, &lstTail)) Badarg("Options");
    if(!enif_get_tuple(env, lstHead, &tpl_sz, &tpl) || tpl_sz != 2) Badarg("Options");
    if(enif_is_identical(tpl[0], enif_make_atom(env, "r"))) {
  if(!enif_get_int(env, tpl[1], &r)) Badarg("r"); // int
    } else     if(enif_is_identical(tpl[0], enif_make_atom(env, "g"))) {
  if(!enif_get_int(env, tpl[1], &g)) Badarg("g"); // int
    } else     if(enif_is_identical(tpl[0], enif_make_atom(env, "b"))) {
  if(!enif_get_int(env, tpl[1], &b)) Badarg("b"); // int
    } else        Badarg("Options");
  };
  if(!This) throw wxe_badarg("This");
  wxImage * Result = new EwxImage(This->Size(size,pos,r,g,b)); app->newPtr((void *) Result,3, memenv);;
  wxeReturn rt = wxeReturn(memenv, Ecmd.caller, true);
  rt.send(  rt.make_ref(app->getRef((void *)Result,memenv), "wxImage"));

}

// wxImage::SetAlpha
void wxImage_SetAlpha_1(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd)
{
  ErlNifEnv *env = Ecmd.env;
  ERL_NIF_TERM * argv = Ecmd.args;
  wxImage *This;
  This = (wxImage *) memenv->getPtr(env, argv[0], "This");
  unsigned char * alpha;
  ErlNifBinary alpha_bin;
  if(!enif_inspect_binary(env, argv[1], &alpha_bin)) Badarg("alpha");
  alpha = (unsigned char *) malloc(alpha_bin.size);
  memcpy(alpha,alpha_bin.data,alpha_bin.size);
  if(!This) throw wxe_badarg("This");
  This->SetAlpha(alpha);

}

// wxImage::SetAlpha
void wxImage_SetAlpha_3(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd)
{
  ErlNifEnv *env = Ecmd.env;
  ERL_NIF_TERM * argv = Ecmd.args;
  wxImage *This;
  This = (wxImage *) memenv->getPtr(env, argv[0], "This");
  int x;
  if(!enif_get_int(env, argv[1], &x)) Badarg("x"); // int
  int y;
  if(!enif_get_int(env, argv[2], &y)) Badarg("y"); // int
  unsigned int alpha;
  if(!enif_get_uint(env, argv[3], &alpha)) Badarg("alpha");
  if(!This) throw wxe_badarg("This");
  This->SetAlpha(x,y,alpha);

}

// wxImage::SetData
void wxImage_SetData_1(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd)
{
  ErlNifEnv *env = Ecmd.env;
  ERL_NIF_TERM * argv = Ecmd.args;
  wxImage *This;
  This = (wxImage *) memenv->getPtr(env, argv[0], "This");
  unsigned char * data;
  ErlNifBinary data_bin;
  if(!enif_inspect_binary(env, argv[1], &data_bin)) Badarg("data");
  data = (unsigned char *) malloc(data_bin.size);
  memcpy(data,data_bin.data,data_bin.size);
  if(!This) throw wxe_badarg("This");
  This->SetData(data);

}

// wxImage::SetData
void wxImage_SetData_3(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd)
{
  ErlNifEnv *env = Ecmd.env;
  ERL_NIF_TERM * argv = Ecmd.args;
  wxImage *This;
  This = (wxImage *) memenv->getPtr(env, argv[0], "This");
  unsigned char * data;
  ErlNifBinary data_bin;
  if(!enif_inspect_binary(env, argv[1], &data_bin)) Badarg("data");
  data = (unsigned char *) malloc(data_bin.size);
  memcpy(data,data_bin.data,data_bin.size);
  int new_width;
  if(!enif_get_int(env, argv[2], &new_width)) Badarg("new_width"); // int
  int new_height;
  if(!enif_get_int(env, argv[3], &new_height)) Badarg("new_height"); // int
  if(!This) throw wxe_badarg("This");
  This->SetData(data,new_width,new_height);

}

// wxImage::SetMask
void wxImage_SetMask(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd)
{
  bool mask=true;
  ErlNifEnv *env = Ecmd.env;
  ERL_NIF_TERM * argv = Ecmd.args;
  wxImage *This;
  This = (wxImage *) memenv->getPtr(env, argv[0], "This");
  ERL_NIF_TERM lstHead, lstTail;
  lstTail = argv[1];
  if(!enif_is_list(env, lstTail)) Badarg("Options");
  const ERL_NIF_TERM *tpl;
  int tpl_sz;
  while(!enif_is_empty_list(env, lstTail)) {
    if(!enif_get_list_cell(env, lstTail, &lstHead, &lstTail)) Badarg("Options");
    if(!enif_get_tuple(env, lstHead, &tpl_sz, &tpl) || tpl_sz != 2) Badarg("Options");
    if(enif_is_identical(tpl[0], enif_make_atom(env, "mask"))) {
  mask = enif_is_identical(tpl[1], WXE_ATOM_true);
    } else        Badarg("Options");
  };
  if(!This) throw wxe_badarg("This");
  This->SetMask(mask);

}

// wxImage::SetMaskColour
void wxImage_SetMaskColour(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd)
{
  ErlNifEnv *env = Ecmd.env;
  ERL_NIF_TERM * argv = Ecmd.args;
  wxImage *This;
  This = (wxImage *) memenv->getPtr(env, argv[0], "This");
  unsigned int red;
  if(!enif_get_uint(env, argv[1], &red)) Badarg("red");
  unsigned int green;
  if(!enif_get_uint(env, argv[2], &green)) Badarg("green");
  unsigned int blue;
  if(!enif_get_uint(env, argv[3], &blue)) Badarg("blue");
  if(!This) throw wxe_badarg("This");
  This->SetMaskColour(red,green,blue);

}

// wxImage::SetMaskFromImage
void wxImage_SetMaskFromImage(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd)
{
  ErlNifEnv *env = Ecmd.env;
  ERL_NIF_TERM * argv = Ecmd.args;
  wxImage *This;
  This = (wxImage *) memenv->getPtr(env, argv[0], "This");
  wxImage *mask;
  mask = (wxImage *) memenv->getPtr(env, argv[1], "mask");
  unsigned int mr;
  if(!enif_get_uint(env, argv[2], &mr)) Badarg("mr");
  unsigned int mg;
  if(!enif_get_uint(env, argv[3], &mg)) Badarg("mg");
  unsigned int mb;
  if(!enif_get_uint(env, argv[4], &mb)) Badarg("mb");
  if(!This) throw wxe_badarg("This");
  bool Result = This->SetMaskFromImage(*mask,mr,mg,mb);
  wxeReturn rt = wxeReturn(memenv, Ecmd.caller, true);
  rt.send(  rt.make_bool(Result));

}

// wxImage::SetOption
void wxImage_SetOption_2_1(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd)
{
  ErlNifEnv *env = Ecmd.env;
  ERL_NIF_TERM * argv = Ecmd.args;
  wxImage *This;
  This = (wxImage *) memenv->getPtr(env, argv[0], "This");
  ErlNifBinary name_bin;
  wxString name;
  if(!enif_inspect_binary(env, argv[1], &name_bin)) Badarg("name");
  name = wxString(name_bin.data, wxConvUTF8, name_bin.size);
  ErlNifBinary value_bin;
  wxString value;
  if(!enif_inspect_binary(env, argv[2], &value_bin)) Badarg("value");
  value = wxString(value_bin.data, wxConvUTF8, value_bin.size);
  if(!This) throw wxe_badarg("This");
  This->SetOption(name,value);

}

// wxImage::SetOption
void wxImage_SetOption_2_0(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd)
{
  ErlNifEnv *env = Ecmd.env;
  ERL_NIF_TERM * argv = Ecmd.args;
  wxImage *This;
  This = (wxImage *) memenv->getPtr(env, argv[0], "This");
  ErlNifBinary name_bin;
  wxString name;
  if(!enif_inspect_binary(env, argv[1], &name_bin)) Badarg("name");
  name = wxString(name_bin.data, wxConvUTF8, name_bin.size);
  int value;
  if(!enif_get_int(env, argv[2], &value)) Badarg("value"); // int
  if(!This) throw wxe_badarg("This");
  This->SetOption(name,value);

}

// wxImage::SetPalette
void wxImage_SetPalette(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd)
{
  ErlNifEnv *env = Ecmd.env;
  ERL_NIF_TERM * argv = Ecmd.args;
  wxImage *This;
  This = (wxImage *) memenv->getPtr(env, argv[0], "This");
  wxPalette *palette;
  palette = (wxPalette *) memenv->getPtr(env, argv[1], "palette");
  if(!This) throw wxe_badarg("This");
  This->SetPalette(*palette);

}

// wxImage::SetRGB
void wxImage_SetRGB_5(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd)
{
  ErlNifEnv *env = Ecmd.env;
  ERL_NIF_TERM * argv = Ecmd.args;
  wxImage *This;
  This = (wxImage *) memenv->getPtr(env, argv[0], "This");
  int x;
  if(!enif_get_int(env, argv[1], &x)) Badarg("x"); // int
  int y;
  if(!enif_get_int(env, argv[2], &y)) Badarg("y"); // int
  unsigned int r;
  if(!enif_get_uint(env, argv[3], &r)) Badarg("r");
  unsigned int g;
  if(!enif_get_uint(env, argv[4], &g)) Badarg("g");
  unsigned int b;
  if(!enif_get_uint(env, argv[5], &b)) Badarg("b");
  if(!This) throw wxe_badarg("This");
  This->SetRGB(x,y,r,g,b);

}

// wxImage::SetRGB
void wxImage_SetRGB_4(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd)
{
  ErlNifEnv *env = Ecmd.env;
  ERL_NIF_TERM * argv = Ecmd.args;
  wxImage *This;
  This = (wxImage *) memenv->getPtr(env, argv[0], "This");
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
  unsigned int red;
  if(!enif_get_uint(env, argv[2], &red)) Badarg("red");
  unsigned int green;
  if(!enif_get_uint(env, argv[3], &green)) Badarg("green");
  unsigned int blue;
  if(!enif_get_uint(env, argv[4], &blue)) Badarg("blue");
  if(!This) throw wxe_badarg("This");
  This->SetRGB(rect,red,green,blue);

}

// wxImageList::wxImageList
void wxImageList_new_0(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd)
{
  wxImageList * Result = new EwxImageList();
  app->newPtr((void *) Result, 1, memenv);
  wxeReturn rt = wxeReturn(memenv, Ecmd.caller, true);
  rt.send(  rt.make_ref(app->getRef((void *)Result,memenv), "wxImageList"));

}

// wxImageList::wxImageList
void wxImageList_new_3(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd)
{
  bool mask=true;
  int initialCount=1;
  ErlNifEnv *env = Ecmd.env;
  ERL_NIF_TERM * argv = Ecmd.args;
  int width;
  if(!enif_get_int(env, argv[0], &width)) Badarg("width"); // int
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
    if(enif_is_identical(tpl[0], enif_make_atom(env, "mask"))) {
  mask = enif_is_identical(tpl[1], WXE_ATOM_true);
    } else     if(enif_is_identical(tpl[0], enif_make_atom(env, "initialCount"))) {
  if(!enif_get_int(env, tpl[1], &initialCount)) Badarg("initialCount"); // int
    } else        Badarg("Options");
  };
  wxImageList * Result = new EwxImageList(width,height,mask,initialCount);
  app->newPtr((void *) Result, 1, memenv);
  wxeReturn rt = wxeReturn(memenv, Ecmd.caller, true);
  rt.send(  rt.make_ref(app->getRef((void *)Result,memenv), "wxImageList"));

}

// wxImageList::Add
void wxImageList_Add_2_0(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd)
{
  ErlNifEnv *env = Ecmd.env;
  ERL_NIF_TERM * argv = Ecmd.args;
  wxImageList *This;
  This = (wxImageList *) memenv->getPtr(env, argv[0], "This");
  wxBitmap *bitmap;
  bitmap = (wxBitmap *) memenv->getPtr(env, argv[1], "bitmap");
  wxBitmap *mask;
  mask = (wxBitmap *) memenv->getPtr(env, argv[2], "mask");
  if(!This) throw wxe_badarg("This");
  int Result = This->Add(*bitmap,*mask);
  wxeReturn rt = wxeReturn(memenv, Ecmd.caller, true);
  rt.send(  rt.make_int(Result));

}

// wxImageList::Add
void wxImageList_Add_2_1(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd)
{
  ErlNifEnv *env = Ecmd.env;
  ERL_NIF_TERM * argv = Ecmd.args;
  wxImageList *This;
  This = (wxImageList *) memenv->getPtr(env, argv[0], "This");
  wxBitmap *bitmap;
  bitmap = (wxBitmap *) memenv->getPtr(env, argv[1], "bitmap");
  const ERL_NIF_TERM *maskColour_t;
  int maskColour_sz;
  if(!enif_get_tuple(env, argv[2], &maskColour_sz, &maskColour_t)) Badarg("maskColour");
  int maskColourR;
  if(!enif_get_int(env, maskColour_t[0], &maskColourR)) Badarg("maskColour");
  int maskColourG;
  if(!enif_get_int(env, maskColour_t[1], &maskColourG)) Badarg("maskColour");
  int maskColourB;
  if(!enif_get_int(env, maskColour_t[2], &maskColourB)) Badarg("maskColour");
  int maskColourA;
  if(!enif_get_int(env, maskColour_t[3], &maskColourA)) Badarg("maskColour");
  wxColour maskColour = wxColour(maskColourR,maskColourG,maskColourB,maskColourA);
  if(!This) throw wxe_badarg("This");
  int Result = This->Add(*bitmap,maskColour);
  wxeReturn rt = wxeReturn(memenv, Ecmd.caller, true);
  rt.send(  rt.make_int(Result));

}

// wxImageList::Add
void wxImageList_Add_1(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd)
{
  ErlNifEnv *env = Ecmd.env;
  ERL_NIF_TERM * argv = Ecmd.args;
  wxImageList *This;
  This = (wxImageList *) memenv->getPtr(env, argv[0], "This");
  ERL_NIF_TERM icon_type;
  void * icon = memenv->getPtr(env, argv[1], "icon", &icon_type);
  if(!This) throw wxe_badarg("This");
  int Result;
  if(enif_is_identical(icon_type, WXE_ATOM_wxIcon))
   Result =  This->Add(* static_cast<wxIcon*> (icon));
  else if(enif_is_identical(icon_type, WXE_ATOM_wxBitmap))
   Result =  This->Add(* static_cast<wxBitmap*> (icon));
  else throw wxe_badarg("icon");
  wxeReturn rt = wxeReturn(memenv, Ecmd.caller, true);
  rt.send(  rt.make_int(Result));

}

// wxImageList::Create
void wxImageList_Create(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd)
{
  bool mask=true;
  int initialCount=1;
  ErlNifEnv *env = Ecmd.env;
  ERL_NIF_TERM * argv = Ecmd.args;
  wxImageList *This;
  This = (wxImageList *) memenv->getPtr(env, argv[0], "This");
  int width;
  if(!enif_get_int(env, argv[1], &width)) Badarg("width"); // int
  int height;
  if(!enif_get_int(env, argv[2], &height)) Badarg("height"); // int
  ERL_NIF_TERM lstHead, lstTail;
  lstTail = argv[3];
  if(!enif_is_list(env, lstTail)) Badarg("Options");
  const ERL_NIF_TERM *tpl;
  int tpl_sz;
  while(!enif_is_empty_list(env, lstTail)) {
    if(!enif_get_list_cell(env, lstTail, &lstHead, &lstTail)) Badarg("Options");
    if(!enif_get_tuple(env, lstHead, &tpl_sz, &tpl) || tpl_sz != 2) Badarg("Options");
    if(enif_is_identical(tpl[0], enif_make_atom(env, "mask"))) {
  mask = enif_is_identical(tpl[1], WXE_ATOM_true);
    } else     if(enif_is_identical(tpl[0], enif_make_atom(env, "initialCount"))) {
  if(!enif_get_int(env, tpl[1], &initialCount)) Badarg("initialCount"); // int
    } else        Badarg("Options");
  };
  if(!This) throw wxe_badarg("This");
  bool Result = This->Create(width,height,mask,initialCount);
  wxeReturn rt = wxeReturn(memenv, Ecmd.caller, true);
  rt.send(  rt.make_bool(Result));

}

// wxImageList::Draw
void wxImageList_Draw(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd)
{
  int flags=wxIMAGELIST_DRAW_NORMAL;
  bool solidBackground=false;
  ErlNifEnv *env = Ecmd.env;
  ERL_NIF_TERM * argv = Ecmd.args;
  wxImageList *This;
  This = (wxImageList *) memenv->getPtr(env, argv[0], "This");
  int index;
  if(!enif_get_int(env, argv[1], &index)) Badarg("index"); // int
  wxDC *dc;
  dc = (wxDC *) memenv->getPtr(env, argv[2], "dc");
  int x;
  if(!enif_get_int(env, argv[3], &x)) Badarg("x"); // int
  int y;
  if(!enif_get_int(env, argv[4], &y)) Badarg("y"); // int
  ERL_NIF_TERM lstHead, lstTail;
  lstTail = argv[5];
  if(!enif_is_list(env, lstTail)) Badarg("Options");
  const ERL_NIF_TERM *tpl;
  int tpl_sz;
  while(!enif_is_empty_list(env, lstTail)) {
    if(!enif_get_list_cell(env, lstTail, &lstHead, &lstTail)) Badarg("Options");
    if(!enif_get_tuple(env, lstHead, &tpl_sz, &tpl) || tpl_sz != 2) Badarg("Options");
    if(enif_is_identical(tpl[0], enif_make_atom(env, "flags"))) {
  if(!enif_get_int(env, tpl[1], &flags)) Badarg("flags"); // int
    } else     if(enif_is_identical(tpl[0], enif_make_atom(env, "solidBackground"))) {
  solidBackground = enif_is_identical(tpl[1], WXE_ATOM_true);
    } else        Badarg("Options");
  };
  if(!This) throw wxe_badarg("This");
  bool Result = This->Draw(index,*dc,x,y,flags,solidBackground);
  wxeReturn rt = wxeReturn(memenv, Ecmd.caller, true);
  rt.send(  rt.make_bool(Result));

}

// wxImageList::GetBitmap
void wxImageList_GetBitmap(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd)
{
  ErlNifEnv *env = Ecmd.env;
  ERL_NIF_TERM * argv = Ecmd.args;
  wxImageList *This;
  This = (wxImageList *) memenv->getPtr(env, argv[0], "This");
  int index;
  if(!enif_get_int(env, argv[1], &index)) Badarg("index"); // int
  if(!This) throw wxe_badarg("This");
  wxBitmap * Result = new wxBitmap(This->GetBitmap(index)); app->newPtr((void *) Result,3, memenv);;
  wxeReturn rt = wxeReturn(memenv, Ecmd.caller, true);
  rt.send(  rt.make_ref(app->getRef((void *)Result,memenv), "wxBitmap"));

}

// wxImageList::GetIcon
void wxImageList_GetIcon(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd)
{
  ErlNifEnv *env = Ecmd.env;
  ERL_NIF_TERM * argv = Ecmd.args;
  wxImageList *This;
  This = (wxImageList *) memenv->getPtr(env, argv[0], "This");
  int index;
  if(!enif_get_int(env, argv[1], &index)) Badarg("index"); // int
  if(!This) throw wxe_badarg("This");
  wxIcon * Result = new wxIcon(This->GetIcon(index)); app->newPtr((void *) Result,3, memenv);;
  wxeReturn rt = wxeReturn(memenv, Ecmd.caller, true);
  rt.send(  rt.make_ref(app->getRef((void *)Result,memenv), "wxIcon"));

}

// wxImageList::GetImageCount
void wxImageList_GetImageCount(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd)
{
  ErlNifEnv *env = Ecmd.env;
  ERL_NIF_TERM * argv = Ecmd.args;
  wxImageList *This;
  This = (wxImageList *) memenv->getPtr(env, argv[0], "This");
  if(!This) throw wxe_badarg("This");
  int Result = This->GetImageCount();
  wxeReturn rt = wxeReturn(memenv, Ecmd.caller, true);
  rt.send(  rt.make_int(Result));

}

// wxImageList::GetSize
void wxImageList_GetSize(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd)
{
  int width;
  int height;
  ErlNifEnv *env = Ecmd.env;
  ERL_NIF_TERM * argv = Ecmd.args;
  wxImageList *This;
  This = (wxImageList *) memenv->getPtr(env, argv[0], "This");
  int index;
  if(!enif_get_int(env, argv[1], &index)) Badarg("index"); // int
  if(!This) throw wxe_badarg("This");
  bool Result = This->GetSize(index,width,height);
  wxeReturn rt = wxeReturn(memenv, Ecmd.caller, true);
  ERL_NIF_TERM msg = enif_make_tuple3(rt.env,
  rt.make_bool(Result),
    rt.make_int(width),
  rt.make_int(height));
  rt.send(msg);

}

// wxImageList::Remove
void wxImageList_Remove(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd)
{
  ErlNifEnv *env = Ecmd.env;
  ERL_NIF_TERM * argv = Ecmd.args;
  wxImageList *This;
  This = (wxImageList *) memenv->getPtr(env, argv[0], "This");
  int index;
  if(!enif_get_int(env, argv[1], &index)) Badarg("index"); // int
  if(!This) throw wxe_badarg("This");
  bool Result = This->Remove(index);
  wxeReturn rt = wxeReturn(memenv, Ecmd.caller, true);
  rt.send(  rt.make_bool(Result));

}

// wxImageList::RemoveAll
void wxImageList_RemoveAll(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd)
{
  ErlNifEnv *env = Ecmd.env;
  ERL_NIF_TERM * argv = Ecmd.args;
  wxImageList *This;
  This = (wxImageList *) memenv->getPtr(env, argv[0], "This");
  if(!This) throw wxe_badarg("This");
  bool Result = This->RemoveAll();
  wxeReturn rt = wxeReturn(memenv, Ecmd.caller, true);
  rt.send(  rt.make_bool(Result));

}

// wxImageList::Replace
void wxImageList_Replace_3(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd)
{
  ErlNifEnv *env = Ecmd.env;
  ERL_NIF_TERM * argv = Ecmd.args;
  wxImageList *This;
  This = (wxImageList *) memenv->getPtr(env, argv[0], "This");
  int index;
  if(!enif_get_int(env, argv[1], &index)) Badarg("index"); // int
  wxBitmap *bitmap;
  bitmap = (wxBitmap *) memenv->getPtr(env, argv[2], "bitmap");
  wxBitmap *mask;
  mask = (wxBitmap *) memenv->getPtr(env, argv[3], "mask");
  if(!This) throw wxe_badarg("This");
  bool Result = This->Replace(index,*bitmap,*mask);
  wxeReturn rt = wxeReturn(memenv, Ecmd.caller, true);
  rt.send(  rt.make_bool(Result));

}

// wxImageList::Replace
void wxImageList_Replace_2(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd)
{
  ErlNifEnv *env = Ecmd.env;
  ERL_NIF_TERM * argv = Ecmd.args;
  wxImageList *This;
  This = (wxImageList *) memenv->getPtr(env, argv[0], "This");
  int index;
  if(!enif_get_int(env, argv[1], &index)) Badarg("index"); // int
  ERL_NIF_TERM icon_type;
  void * icon = memenv->getPtr(env, argv[2], "icon", &icon_type);
  if(!This) throw wxe_badarg("This");
  bool Result;
  if(enif_is_identical(icon_type, WXE_ATOM_wxIcon))
   Result =  This->Replace(index,* static_cast<wxIcon*> (icon));
  else if(enif_is_identical(icon_type, WXE_ATOM_wxBitmap))
   Result =  This->Replace(index,* static_cast<wxBitmap*> (icon));
  else throw wxe_badarg("icon");
  wxeReturn rt = wxeReturn(memenv, Ecmd.caller, true);
  rt.send(  rt.make_bool(Result));

}

// wxJoystickEvent::ButtonDown
void wxJoystickEvent_ButtonDown(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd)
{
  int but=wxJOY_BUTTON_ANY;
  ErlNifEnv *env = Ecmd.env;
  ERL_NIF_TERM * argv = Ecmd.args;
  wxJoystickEvent *This;
  This = (wxJoystickEvent *) memenv->getPtr(env, argv[0], "This");
  ERL_NIF_TERM lstHead, lstTail;
  lstTail = argv[1];
  if(!enif_is_list(env, lstTail)) Badarg("Options");
  const ERL_NIF_TERM *tpl;
  int tpl_sz;
  while(!enif_is_empty_list(env, lstTail)) {
    if(!enif_get_list_cell(env, lstTail, &lstHead, &lstTail)) Badarg("Options");
    if(!enif_get_tuple(env, lstHead, &tpl_sz, &tpl) || tpl_sz != 2) Badarg("Options");
    if(enif_is_identical(tpl[0], enif_make_atom(env, "but"))) {
  if(!enif_get_int(env, tpl[1], &but)) Badarg("but"); // int
    } else        Badarg("Options");
  };
  if(!This) throw wxe_badarg("This");
  bool Result = This->ButtonDown(but);
  wxeReturn rt = wxeReturn(memenv, Ecmd.caller, true);
  rt.send(  rt.make_bool(Result));

}

// wxJoystickEvent::ButtonIsDown
void wxJoystickEvent_ButtonIsDown(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd)
{
  int but=wxJOY_BUTTON_ANY;
  ErlNifEnv *env = Ecmd.env;
  ERL_NIF_TERM * argv = Ecmd.args;
  wxJoystickEvent *This;
  This = (wxJoystickEvent *) memenv->getPtr(env, argv[0], "This");
  ERL_NIF_TERM lstHead, lstTail;
  lstTail = argv[1];
  if(!enif_is_list(env, lstTail)) Badarg("Options");
  const ERL_NIF_TERM *tpl;
  int tpl_sz;
  while(!enif_is_empty_list(env, lstTail)) {
    if(!enif_get_list_cell(env, lstTail, &lstHead, &lstTail)) Badarg("Options");
    if(!enif_get_tuple(env, lstHead, &tpl_sz, &tpl) || tpl_sz != 2) Badarg("Options");
    if(enif_is_identical(tpl[0], enif_make_atom(env, "but"))) {
  if(!enif_get_int(env, tpl[1], &but)) Badarg("but"); // int
    } else        Badarg("Options");
  };
  if(!This) throw wxe_badarg("This");
  bool Result = This->ButtonIsDown(but);
  wxeReturn rt = wxeReturn(memenv, Ecmd.caller, true);
  rt.send(  rt.make_bool(Result));

}

// wxJoystickEvent::ButtonUp
void wxJoystickEvent_ButtonUp(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd)
{
  int but=wxJOY_BUTTON_ANY;
  ErlNifEnv *env = Ecmd.env;
  ERL_NIF_TERM * argv = Ecmd.args;
  wxJoystickEvent *This;
  This = (wxJoystickEvent *) memenv->getPtr(env, argv[0], "This");
  ERL_NIF_TERM lstHead, lstTail;
  lstTail = argv[1];
  if(!enif_is_list(env, lstTail)) Badarg("Options");
  const ERL_NIF_TERM *tpl;
  int tpl_sz;
  while(!enif_is_empty_list(env, lstTail)) {
    if(!enif_get_list_cell(env, lstTail, &lstHead, &lstTail)) Badarg("Options");
    if(!enif_get_tuple(env, lstHead, &tpl_sz, &tpl) || tpl_sz != 2) Badarg("Options");
    if(enif_is_identical(tpl[0], enif_make_atom(env, "but"))) {
  if(!enif_get_int(env, tpl[1], &but)) Badarg("but"); // int
    } else        Badarg("Options");
  };
  if(!This) throw wxe_badarg("This");
  bool Result = This->ButtonUp(but);
  wxeReturn rt = wxeReturn(memenv, Ecmd.caller, true);
  rt.send(  rt.make_bool(Result));

}

// wxJoystickEvent::GetButtonChange
void wxJoystickEvent_GetButtonChange(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd)
{
  ErlNifEnv *env = Ecmd.env;
  ERL_NIF_TERM * argv = Ecmd.args;
  wxJoystickEvent *This;
  This = (wxJoystickEvent *) memenv->getPtr(env, argv[0], "This");
  if(!This) throw wxe_badarg("This");
  int Result = This->GetButtonChange();
  wxeReturn rt = wxeReturn(memenv, Ecmd.caller, true);
  rt.send(  rt.make_int(Result));

}

// wxJoystickEvent::GetButtonState
void wxJoystickEvent_GetButtonState(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd)
{
  ErlNifEnv *env = Ecmd.env;
  ERL_NIF_TERM * argv = Ecmd.args;
  wxJoystickEvent *This;
  This = (wxJoystickEvent *) memenv->getPtr(env, argv[0], "This");
  if(!This) throw wxe_badarg("This");
  int Result = This->GetButtonState();
  wxeReturn rt = wxeReturn(memenv, Ecmd.caller, true);
  rt.send(  rt.make_int(Result));

}

// wxJoystickEvent::GetJoystick
void wxJoystickEvent_GetJoystick(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd)
{
  ErlNifEnv *env = Ecmd.env;
  ERL_NIF_TERM * argv = Ecmd.args;
  wxJoystickEvent *This;
  This = (wxJoystickEvent *) memenv->getPtr(env, argv[0], "This");
  if(!This) throw wxe_badarg("This");
  int Result = This->GetJoystick();
  wxeReturn rt = wxeReturn(memenv, Ecmd.caller, true);
  rt.send(  rt.make_int(Result));

}

// wxJoystickEvent::GetPosition
void wxJoystickEvent_GetPosition(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd)
{
  ErlNifEnv *env = Ecmd.env;
  ERL_NIF_TERM * argv = Ecmd.args;
  wxJoystickEvent *This;
  This = (wxJoystickEvent *) memenv->getPtr(env, argv[0], "This");
  if(!This) throw wxe_badarg("This");
  wxPoint Result = This->GetPosition();
  wxeReturn rt = wxeReturn(memenv, Ecmd.caller, true);
  rt.send(  rt.make(Result));

}

// wxJoystickEvent::GetZPosition
void wxJoystickEvent_GetZPosition(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd)
{
  ErlNifEnv *env = Ecmd.env;
  ERL_NIF_TERM * argv = Ecmd.args;
  wxJoystickEvent *This;
  This = (wxJoystickEvent *) memenv->getPtr(env, argv[0], "This");
  if(!This) throw wxe_badarg("This");
  int Result = This->GetZPosition();
  wxeReturn rt = wxeReturn(memenv, Ecmd.caller, true);
  rt.send(  rt.make_int(Result));

}

// wxJoystickEvent::IsButton
void wxJoystickEvent_IsButton(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd)
{
  ErlNifEnv *env = Ecmd.env;
  ERL_NIF_TERM * argv = Ecmd.args;
  wxJoystickEvent *This;
  This = (wxJoystickEvent *) memenv->getPtr(env, argv[0], "This");
  if(!This) throw wxe_badarg("This");
  bool Result = This->IsButton();
  wxeReturn rt = wxeReturn(memenv, Ecmd.caller, true);
  rt.send(  rt.make_bool(Result));

}

// wxJoystickEvent::IsMove
void wxJoystickEvent_IsMove(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd)
{
  ErlNifEnv *env = Ecmd.env;
  ERL_NIF_TERM * argv = Ecmd.args;
  wxJoystickEvent *This;
  This = (wxJoystickEvent *) memenv->getPtr(env, argv[0], "This");
  if(!This) throw wxe_badarg("This");
  bool Result = This->IsMove();
  wxeReturn rt = wxeReturn(memenv, Ecmd.caller, true);
  rt.send(  rt.make_bool(Result));

}

// wxJoystickEvent::IsZMove
void wxJoystickEvent_IsZMove(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd)
{
  ErlNifEnv *env = Ecmd.env;
  ERL_NIF_TERM * argv = Ecmd.args;
  wxJoystickEvent *This;
  This = (wxJoystickEvent *) memenv->getPtr(env, argv[0], "This");
  if(!This) throw wxe_badarg("This");
  bool Result = This->IsZMove();
  wxeReturn rt = wxeReturn(memenv, Ecmd.caller, true);
  rt.send(  rt.make_bool(Result));

}

// wxKeyEvent::AltDown
void wxKeyEvent_AltDown(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd)
{
  ErlNifEnv *env = Ecmd.env;
  ERL_NIF_TERM * argv = Ecmd.args;
  wxKeyEvent *This;
  This = (wxKeyEvent *) memenv->getPtr(env, argv[0], "This");
  if(!This) throw wxe_badarg("This");
  bool Result = This->AltDown();
  wxeReturn rt = wxeReturn(memenv, Ecmd.caller, true);
  rt.send(  rt.make_bool(Result));

}

// wxKeyEvent::CmdDown
void wxKeyEvent_CmdDown(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd)
{
  ErlNifEnv *env = Ecmd.env;
  ERL_NIF_TERM * argv = Ecmd.args;
  wxKeyEvent *This;
  This = (wxKeyEvent *) memenv->getPtr(env, argv[0], "This");
  if(!This) throw wxe_badarg("This");
  bool Result = This->CmdDown();
  wxeReturn rt = wxeReturn(memenv, Ecmd.caller, true);
  rt.send(  rt.make_bool(Result));

}

// wxKeyEvent::ControlDown
void wxKeyEvent_ControlDown(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd)
{
  ErlNifEnv *env = Ecmd.env;
  ERL_NIF_TERM * argv = Ecmd.args;
  wxKeyEvent *This;
  This = (wxKeyEvent *) memenv->getPtr(env, argv[0], "This");
  if(!This) throw wxe_badarg("This");
  bool Result = This->ControlDown();
  wxeReturn rt = wxeReturn(memenv, Ecmd.caller, true);
  rt.send(  rt.make_bool(Result));

}

// wxKeyEvent::GetKeyCode
void wxKeyEvent_GetKeyCode(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd)
{
  ErlNifEnv *env = Ecmd.env;
  ERL_NIF_TERM * argv = Ecmd.args;
  wxKeyEvent *This;
  This = (wxKeyEvent *) memenv->getPtr(env, argv[0], "This");
  if(!This) throw wxe_badarg("This");
  int Result = This->GetKeyCode();
  wxeReturn rt = wxeReturn(memenv, Ecmd.caller, true);
  rt.send(  rt.make_int(Result));

}

// wxKeyEvent::GetModifiers
void wxKeyEvent_GetModifiers(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd)
{
  ErlNifEnv *env = Ecmd.env;
  ERL_NIF_TERM * argv = Ecmd.args;
  wxKeyEvent *This;
  This = (wxKeyEvent *) memenv->getPtr(env, argv[0], "This");
  if(!This) throw wxe_badarg("This");
  int Result = This->GetModifiers();
  wxeReturn rt = wxeReturn(memenv, Ecmd.caller, true);
  rt.send(  rt.make_int(Result));

}

// wxKeyEvent::GetPosition
void wxKeyEvent_GetPosition(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd)
{
  ErlNifEnv *env = Ecmd.env;
  ERL_NIF_TERM * argv = Ecmd.args;
  wxKeyEvent *This;
  This = (wxKeyEvent *) memenv->getPtr(env, argv[0], "This");
  if(!This) throw wxe_badarg("This");
  wxPoint Result = This->GetPosition();
  wxeReturn rt = wxeReturn(memenv, Ecmd.caller, true);
  rt.send(  rt.make(Result));

}

// wxKeyEvent::GetRawKeyCode
void wxKeyEvent_GetRawKeyCode(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd)
{
  ErlNifEnv *env = Ecmd.env;
  ERL_NIF_TERM * argv = Ecmd.args;
  wxKeyEvent *This;
  This = (wxKeyEvent *) memenv->getPtr(env, argv[0], "This");
  if(!This) throw wxe_badarg("This");
  int Result = This->GetRawKeyCode();
  wxeReturn rt = wxeReturn(memenv, Ecmd.caller, true);
  rt.send(  rt.make_uint(Result));

}

// wxKeyEvent::GetRawKeyFlags
void wxKeyEvent_GetRawKeyFlags(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd)
{
  ErlNifEnv *env = Ecmd.env;
  ERL_NIF_TERM * argv = Ecmd.args;
  wxKeyEvent *This;
  This = (wxKeyEvent *) memenv->getPtr(env, argv[0], "This");
  if(!This) throw wxe_badarg("This");
  int Result = This->GetRawKeyFlags();
  wxeReturn rt = wxeReturn(memenv, Ecmd.caller, true);
  rt.send(  rt.make_uint(Result));

}

// wxKeyEvent::GetUnicodeKey
void wxKeyEvent_GetUnicodeKey(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd)
{
  ErlNifEnv *env = Ecmd.env;
  ERL_NIF_TERM * argv = Ecmd.args;
  wxKeyEvent *This;
  This = (wxKeyEvent *) memenv->getPtr(env, argv[0], "This");
  if(!This) throw wxe_badarg("This");
  wxChar Result = This->GetUnicodeKey();
  wxeReturn rt = wxeReturn(memenv, Ecmd.caller, true);
  rt.send(  rt.make_int(Result));

}

// wxKeyEvent::GetX
void wxKeyEvent_GetX(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd)
{
  ErlNifEnv *env = Ecmd.env;
  ERL_NIF_TERM * argv = Ecmd.args;
  wxKeyEvent *This;
  This = (wxKeyEvent *) memenv->getPtr(env, argv[0], "This");
  if(!This) throw wxe_badarg("This");
  wxCoord Result = This->GetX();
  wxeReturn rt = wxeReturn(memenv, Ecmd.caller, true);
  rt.send(  rt.make_int(Result));

}

// wxKeyEvent::GetY
void wxKeyEvent_GetY(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd)
{
  ErlNifEnv *env = Ecmd.env;
  ERL_NIF_TERM * argv = Ecmd.args;
  wxKeyEvent *This;
  This = (wxKeyEvent *) memenv->getPtr(env, argv[0], "This");
  if(!This) throw wxe_badarg("This");
  wxCoord Result = This->GetY();
  wxeReturn rt = wxeReturn(memenv, Ecmd.caller, true);
  rt.send(  rt.make_int(Result));

}

// wxKeyEvent::HasModifiers
void wxKeyEvent_HasModifiers(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd)
{
  ErlNifEnv *env = Ecmd.env;
  ERL_NIF_TERM * argv = Ecmd.args;
  wxKeyEvent *This;
  This = (wxKeyEvent *) memenv->getPtr(env, argv[0], "This");
  if(!This) throw wxe_badarg("This");
  bool Result = This->HasModifiers();
  wxeReturn rt = wxeReturn(memenv, Ecmd.caller, true);
  rt.send(  rt.make_bool(Result));

}

// wxKeyEvent::MetaDown
void wxKeyEvent_MetaDown(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd)
{
  ErlNifEnv *env = Ecmd.env;
  ERL_NIF_TERM * argv = Ecmd.args;
  wxKeyEvent *This;
  This = (wxKeyEvent *) memenv->getPtr(env, argv[0], "This");
  if(!This) throw wxe_badarg("This");
  bool Result = This->MetaDown();
  wxeReturn rt = wxeReturn(memenv, Ecmd.caller, true);
  rt.send(  rt.make_bool(Result));

}

// wxKeyEvent::ShiftDown
void wxKeyEvent_ShiftDown(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd)
{
  ErlNifEnv *env = Ecmd.env;
  ERL_NIF_TERM * argv = Ecmd.args;
  wxKeyEvent *This;
  This = (wxKeyEvent *) memenv->getPtr(env, argv[0], "This");
  if(!This) throw wxe_badarg("This");
  bool Result = This->ShiftDown();
  wxeReturn rt = wxeReturn(memenv, Ecmd.caller, true);
  rt.send(  rt.make_bool(Result));

}

// wxLayoutAlgorithm::wxLayoutAlgorithm
void wxLayoutAlgorithm_new(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd)
{
  wxLayoutAlgorithm * Result = new EwxLayoutAlgorithm();
  app->newPtr((void *) Result, 1, memenv);
  wxeReturn rt = wxeReturn(memenv, Ecmd.caller, true);
  rt.send(  rt.make_ref(app->getRef((void *)Result,memenv), "wxLayoutAlgorithm"));

}

// wxLayoutAlgorithm::LayoutFrame
void wxLayoutAlgorithm_LayoutFrame(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd)
{
  wxWindow * mainWindow=NULL;
  ErlNifEnv *env = Ecmd.env;
  ERL_NIF_TERM * argv = Ecmd.args;
  wxLayoutAlgorithm *This;
  This = (wxLayoutAlgorithm *) memenv->getPtr(env, argv[0], "This");
  wxFrame *frame;
  frame = (wxFrame *) memenv->getPtr(env, argv[1], "frame");
  ERL_NIF_TERM lstHead, lstTail;
  lstTail = argv[2];
  if(!enif_is_list(env, lstTail)) Badarg("Options");
  const ERL_NIF_TERM *tpl;
  int tpl_sz;
  while(!enif_is_empty_list(env, lstTail)) {
    if(!enif_get_list_cell(env, lstTail, &lstHead, &lstTail)) Badarg("Options");
    if(!enif_get_tuple(env, lstHead, &tpl_sz, &tpl) || tpl_sz != 2) Badarg("Options");
    if(enif_is_identical(tpl[0], enif_make_atom(env, "mainWindow"))) {
  mainWindow = (wxWindow *) memenv->getPtr(env, tpl[1], "mainWindow");
    } else        Badarg("Options");
  };
  if(!This) throw wxe_badarg("This");
  bool Result = This->LayoutFrame(frame,mainWindow);
  wxeReturn rt = wxeReturn(memenv, Ecmd.caller, true);
  rt.send(  rt.make_bool(Result));

}

// wxLayoutAlgorithm::LayoutMDIFrame
void wxLayoutAlgorithm_LayoutMDIFrame(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd)
{
  wxRect *rect=NULL; wxRect rectTmp;
  ErlNifEnv *env = Ecmd.env;
  ERL_NIF_TERM * argv = Ecmd.args;
  wxLayoutAlgorithm *This;
  This = (wxLayoutAlgorithm *) memenv->getPtr(env, argv[0], "This");
  wxMDIParentFrame *frame;
  frame = (wxMDIParentFrame *) memenv->getPtr(env, argv[1], "frame");
  ERL_NIF_TERM lstHead, lstTail;
  lstTail = argv[2];
  if(!enif_is_list(env, lstTail)) Badarg("Options");
  const ERL_NIF_TERM *tpl;
  int tpl_sz;
  while(!enif_is_empty_list(env, lstTail)) {
    if(!enif_get_list_cell(env, lstTail, &lstHead, &lstTail)) Badarg("Options");
    if(!enif_get_tuple(env, lstHead, &tpl_sz, &tpl) || tpl_sz != 2) Badarg("Options");
    if(enif_is_identical(tpl[0], enif_make_atom(env, "rect"))) {
  const ERL_NIF_TERM *rect_t;
  int rect_sz;
  if(!enif_get_tuple(env, tpl[1], &rect_sz, &rect_t)) Badarg("rect");
  int rectX;
  if(!enif_get_int(env, rect_t[0], &rectX)) Badarg("rect");
  int rectY;
  if(!enif_get_int(env, rect_t[1], &rectY)) Badarg("rect");
  int rectW;
  if(!enif_get_int(env, rect_t[2], &rectW)) Badarg("rect");
  int rectH;
  if(!enif_get_int(env, rect_t[3], &rectH)) Badarg("rect");
  rectTmp = wxRect(rectX,rectY,rectW,rectH); rect = & rectTmp;
    } else        Badarg("Options");
  };
  if(!This) throw wxe_badarg("This");
  bool Result = This->LayoutMDIFrame(frame,rect);
  wxeReturn rt = wxeReturn(memenv, Ecmd.caller, true);
  rt.send(  rt.make_bool(Result));

}

// wxLayoutAlgorithm::LayoutWindow
void wxLayoutAlgorithm_LayoutWindow(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd)
{
  wxWindow * mainWindow=NULL;
  ErlNifEnv *env = Ecmd.env;
  ERL_NIF_TERM * argv = Ecmd.args;
  wxLayoutAlgorithm *This;
  This = (wxLayoutAlgorithm *) memenv->getPtr(env, argv[0], "This");
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
    if(enif_is_identical(tpl[0], enif_make_atom(env, "mainWindow"))) {
  mainWindow = (wxWindow *) memenv->getPtr(env, tpl[1], "mainWindow");
    } else        Badarg("Options");
  };
  if(!This) throw wxe_badarg("This");
  bool Result = This->LayoutWindow(parent,mainWindow);
  wxeReturn rt = wxeReturn(memenv, Ecmd.caller, true);
  rt.send(  rt.make_bool(Result));

}

// wxListBox::wxListBox
void wxListBox_new_0(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd)
{
  wxListBox * Result = new EwxListBox();
  app->newPtr((void *) Result, 0, memenv);
  wxeReturn rt = wxeReturn(memenv, Ecmd.caller, true);
  rt.send(  rt.make_ref(app->getRef((void *)Result,memenv), "wxListBox"));

}

// wxListBox::wxListBox
void wxListBox_new_3(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd)
{
  wxPoint pos= wxDefaultPosition;
  wxSize size= wxDefaultSize;
  wxArrayString choices;
  long style=0;
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
    } else     if(enif_is_identical(tpl[0], enif_make_atom(env, "choices"))) {
  ERL_NIF_TERM choicesHead, choicesTail;
  ErlNifBinary choices_bin;
  choicesTail = tpl[1];
  while(!enif_is_empty_list(env, choicesTail)) {
    if(!enif_get_list_cell(env, choicesTail, &choicesHead, &choicesTail)) Badarg("choices");
    if(!enif_inspect_binary(env, choicesHead, &choices_bin)) Badarg("choices");
    choices.Add(wxString(choices_bin.data, wxConvUTF8, choices_bin.size));
  };
    } else     if(enif_is_identical(tpl[0], enif_make_atom(env, "style"))) {
  if(!enif_get_long(env, tpl[1], &style)) Badarg("style");
    } else     if(enif_is_identical(tpl[0], enif_make_atom(env, "validator"))) {
  validator = (wxValidator *) memenv->getPtr(env, tpl[1], "validator");
    } else        Badarg("Options");
  };
  wxListBox * Result = new EwxListBox(parent,id,pos,size,choices,style,*validator);
  app->newPtr((void *) Result, 0, memenv);
  wxeReturn rt = wxeReturn(memenv, Ecmd.caller, true);
  rt.send(  rt.make_ref(app->getRef((void *)Result,memenv), "wxListBox"));

}

// wxListBox::Create
void wxListBox_Create(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd)
{
  long style=0;
  const wxValidator * validator= &wxDefaultValidator;
  ErlNifEnv *env = Ecmd.env;
  ERL_NIF_TERM * argv = Ecmd.args;
  wxListBox *This;
  This = (wxListBox *) memenv->getPtr(env, argv[0], "This");
  wxWindow *parent;
  parent = (wxWindow *) memenv->getPtr(env, argv[1], "parent");
  int id;
  if(!enif_get_int(env, argv[2], &id)) Badarg("id"); // wxWindowID
  const ERL_NIF_TERM *pos_t;
  int pos_sz;
  if(!enif_get_tuple(env, argv[3], &pos_sz, &pos_t)) Badarg("pos");
  int posX;
  if(!enif_get_int(env, pos_t[0], &posX)) Badarg("pos");
  int posY;
  if(!enif_get_int(env, pos_t[1], &posY)) Badarg("pos");
  wxPoint pos = wxPoint(posX,posY);
  const ERL_NIF_TERM *size_t;
  int size_sz;
  if(!enif_get_tuple(env, argv[4], &size_sz, &size_t)) Badarg("size");
  int sizeW;
  if(!enif_get_int(env, size_t[0], &sizeW)) Badarg("size");
  int sizeH;
  if(!enif_get_int(env, size_t[1], &sizeH)) Badarg("size");
  wxSize size = wxSize(sizeW,sizeH);
  ERL_NIF_TERM choicesHead, choicesTail;
  ErlNifBinary choices_bin;
  wxArrayString choices;
  choicesTail = argv[5];
  while(!enif_is_empty_list(env, choicesTail)) {
    if(!enif_get_list_cell(env, choicesTail, &choicesHead, &choicesTail)) Badarg("choices");
    if(!enif_inspect_binary(env, choicesHead, &choices_bin)) Badarg("choices");
    choices.Add(wxString(choices_bin.data, wxConvUTF8, choices_bin.size));
  };
  ERL_NIF_TERM lstHead, lstTail;
  lstTail = argv[6];
  if(!enif_is_list(env, lstTail)) Badarg("Options");
  const ERL_NIF_TERM *tpl;
  int tpl_sz;
  while(!enif_is_empty_list(env, lstTail)) {
    if(!enif_get_list_cell(env, lstTail, &lstHead, &lstTail)) Badarg("Options");
    if(!enif_get_tuple(env, lstHead, &tpl_sz, &tpl) || tpl_sz != 2) Badarg("Options");
    if(enif_is_identical(tpl[0], enif_make_atom(env, "style"))) {
  if(!enif_get_long(env, tpl[1], &style)) Badarg("style");
    } else     if(enif_is_identical(tpl[0], enif_make_atom(env, "validator"))) {
  validator = (wxValidator *) memenv->getPtr(env, tpl[1], "validator");
    } else        Badarg("Options");
  };
  if(!This) throw wxe_badarg("This");
  bool Result = This->Create(parent,id,pos,size,choices,style,*validator);
  wxeReturn rt = wxeReturn(memenv, Ecmd.caller, true);
  rt.send(  rt.make_bool(Result));

}

// wxListBox::Deselect
void wxListBox_Deselect(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd)
{
  ErlNifEnv *env = Ecmd.env;
  ERL_NIF_TERM * argv = Ecmd.args;
  wxListBox *This;
  This = (wxListBox *) memenv->getPtr(env, argv[0], "This");
  int n;
  if(!enif_get_int(env, argv[1], &n)) Badarg("n"); // int
  if(!This) throw wxe_badarg("This");
  This->Deselect(n);

}

// wxListBox::GetSelections
void wxListBox_GetSelections(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd)
{
  wxArrayInt selections;
  ErlNifEnv *env = Ecmd.env;
  ERL_NIF_TERM * argv = Ecmd.args;
  wxListBox *This;
  This = (wxListBox *) memenv->getPtr(env, argv[0], "This");
  if(!This) throw wxe_badarg("This");
  int Result = This->GetSelections(selections);
  wxeReturn rt = wxeReturn(memenv, Ecmd.caller, true);
  ERL_NIF_TERM msg = enif_make_tuple2(rt.env,
  rt.make_int(Result),
    rt.make(selections));
  rt.send(msg);

}

// wxListBox::InsertItems
void wxListBox_InsertItems(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd)
{
  ErlNifEnv *env = Ecmd.env;
  ERL_NIF_TERM * argv = Ecmd.args;
  wxListBox *This;
  This = (wxListBox *) memenv->getPtr(env, argv[0], "This");
  ERL_NIF_TERM itemsHead, itemsTail;
  ErlNifBinary items_bin;
  wxArrayString items;
  itemsTail = argv[1];
  while(!enif_is_empty_list(env, itemsTail)) {
    if(!enif_get_list_cell(env, itemsTail, &itemsHead, &itemsTail)) Badarg("items");
    if(!enif_inspect_binary(env, itemsHead, &items_bin)) Badarg("items");
    items.Add(wxString(items_bin.data, wxConvUTF8, items_bin.size));
  };
  unsigned int pos;
  if(!enif_get_uint(env, argv[2], &pos)) Badarg("pos");
  if(!This) throw wxe_badarg("This");
  This->InsertItems(items,pos);

}

// wxListBox::IsSelected
void wxListBox_IsSelected(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd)
{
  ErlNifEnv *env = Ecmd.env;
  ERL_NIF_TERM * argv = Ecmd.args;
  wxListBox *This;
  This = (wxListBox *) memenv->getPtr(env, argv[0], "This");
  int n;
  if(!enif_get_int(env, argv[1], &n)) Badarg("n"); // int
  if(!This) throw wxe_badarg("This");
  bool Result = This->IsSelected(n);
  wxeReturn rt = wxeReturn(memenv, Ecmd.caller, true);
  rt.send(  rt.make_bool(Result));

}

// wxListBox::Set
void wxListBox_Set(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd)
{
  ErlNifEnv *env = Ecmd.env;
  ERL_NIF_TERM * argv = Ecmd.args;
  wxListBox *This;
  This = (wxListBox *) memenv->getPtr(env, argv[0], "This");
  ERL_NIF_TERM itemsHead, itemsTail;
  ErlNifBinary items_bin;
  wxArrayString items;
  itemsTail = argv[1];
  while(!enif_is_empty_list(env, itemsTail)) {
    if(!enif_get_list_cell(env, itemsTail, &itemsHead, &itemsTail)) Badarg("items");
    if(!enif_inspect_binary(env, itemsHead, &items_bin)) Badarg("items");
    items.Add(wxString(items_bin.data, wxConvUTF8, items_bin.size));
  };
  if(!This) throw wxe_badarg("This");
  This->Set(items);

}

// wxListBox::HitTest
void wxListBox_HitTest_1(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd)
{
  ErlNifEnv *env = Ecmd.env;
  ERL_NIF_TERM * argv = Ecmd.args;
  wxListBox *This;
  This = (wxListBox *) memenv->getPtr(env, argv[0], "This");
  const ERL_NIF_TERM *point_t;
  int point_sz;
  if(!enif_get_tuple(env, argv[1], &point_sz, &point_t)) Badarg("point");
  int pointX;
  if(!enif_get_int(env, point_t[0], &pointX)) Badarg("point");
  int pointY;
  if(!enif_get_int(env, point_t[1], &pointY)) Badarg("point");
  wxPoint point = wxPoint(pointX,pointY);
  if(!This) throw wxe_badarg("This");
  int Result = This->HitTest(point);
  wxeReturn rt = wxeReturn(memenv, Ecmd.caller, true);
  rt.send(  rt.make_int(Result));

}

// wxListBox::HitTest
void wxListBox_HitTest_2(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd)
{
  ErlNifEnv *env = Ecmd.env;
  ERL_NIF_TERM * argv = Ecmd.args;
  wxListBox *This;
  This = (wxListBox *) memenv->getPtr(env, argv[0], "This");
  int x;
  if(!enif_get_int(env, argv[1], &x)) Badarg("x"); // int
  int y;
  if(!enif_get_int(env, argv[2], &y)) Badarg("y"); // int
  if(!This) throw wxe_badarg("This");
  int Result = This->HitTest(x,y);
  wxeReturn rt = wxeReturn(memenv, Ecmd.caller, true);
  rt.send(  rt.make_int(Result));

}

// wxListBox::SetFirstItem
void wxListBox_SetFirstItem_1_0(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd)
{
  ErlNifEnv *env = Ecmd.env;
  ERL_NIF_TERM * argv = Ecmd.args;
  wxListBox *This;
  This = (wxListBox *) memenv->getPtr(env, argv[0], "This");
  int n;
  if(!enif_get_int(env, argv[1], &n)) Badarg("n"); // int
  if(!This) throw wxe_badarg("This");
  This->SetFirstItem(n);

}

// wxListBox::SetFirstItem
void wxListBox_SetFirstItem_1_1(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd)
{
  ErlNifEnv *env = Ecmd.env;
  ERL_NIF_TERM * argv = Ecmd.args;
  wxListBox *This;
  This = (wxListBox *) memenv->getPtr(env, argv[0], "This");
  ErlNifBinary string_bin;
  wxString string;
  if(!enif_inspect_binary(env, argv[1], &string_bin)) Badarg("string");
  string = wxString(string_bin.data, wxConvUTF8, string_bin.size);
  if(!This) throw wxe_badarg("This");
  This->SetFirstItem(string);

}


void wxListCtrl_new_0(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd)
{
  wxListCtrl * Result = new EwxListCtrl();
  app->newPtr((void *) Result, 0, memenv);
  wxeReturn rt = wxeReturn(memenv, Ecmd.caller, true);
  rt.send(  rt.make_ref(app->getRef((void *)Result,memenv), "wxListCtrl"));
}


  // skipped wxListCtrl_new_2
// wxListCtrl::Arrange
void wxListCtrl_Arrange(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd)
{
  int flag=wxLIST_ALIGN_DEFAULT;
  ErlNifEnv *env = Ecmd.env;
  ERL_NIF_TERM * argv = Ecmd.args;
  wxListCtrl *This;
  This = (wxListCtrl *) memenv->getPtr(env, argv[0], "This");
  ERL_NIF_TERM lstHead, lstTail;
  lstTail = argv[1];
  if(!enif_is_list(env, lstTail)) Badarg("Options");
  const ERL_NIF_TERM *tpl;
  int tpl_sz;
  while(!enif_is_empty_list(env, lstTail)) {
    if(!enif_get_list_cell(env, lstTail, &lstHead, &lstTail)) Badarg("Options");
    if(!enif_get_tuple(env, lstHead, &tpl_sz, &tpl) || tpl_sz != 2) Badarg("Options");
    if(enif_is_identical(tpl[0], enif_make_atom(env, "flag"))) {
  if(!enif_get_int(env, tpl[1], &flag)) Badarg("flag"); // int
    } else        Badarg("Options");
  };
  if(!This) throw wxe_badarg("This");
  bool Result = This->Arrange(flag);
  wxeReturn rt = wxeReturn(memenv, Ecmd.caller, true);
  rt.send(  rt.make_bool(Result));

}

// wxListCtrl::AssignImageList
void wxListCtrl_AssignImageList(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd)
{
  ErlNifEnv *env = Ecmd.env;
  ERL_NIF_TERM * argv = Ecmd.args;
  wxListCtrl *This;
  This = (wxListCtrl *) memenv->getPtr(env, argv[0], "This");
  wxImageList *imageList;
  imageList = (wxImageList *) memenv->getPtr(env, argv[1], "imageList");
  int which;
  if(!enif_get_int(env, argv[2], &which)) Badarg("which"); // int
  if(!This) throw wxe_badarg("This");
  This->AssignImageList(imageList,which);

}

// wxListCtrl::ClearAll
void wxListCtrl_ClearAll(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd)
{
  ErlNifEnv *env = Ecmd.env;
  ERL_NIF_TERM * argv = Ecmd.args;
  wxListCtrl *This;
  This = (wxListCtrl *) memenv->getPtr(env, argv[0], "This");
  if(!This) throw wxe_badarg("This");
  This->ClearAll();

}


// wxListCtrl::Create
void wxListCtrl_Create(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd)
{
  wxWindowID winid=wxID_ANY;
  wxPoint pos= wxDefaultPosition;
  wxSize size= wxDefaultSize;
  long style=wxLC_ICON;
  const wxValidator * validator= &wxDefaultValidator;
  ErlNifEnv *env = Ecmd.env;
  ERL_NIF_TERM * argv = Ecmd.args;

  int onGetItemText = 0, onGetItemAttr = 0, onGetItemColumnImage = 0;

  EwxListCtrl *This;
  This = (EwxListCtrl *) memenv->getPtr(env, argv[0], "This");
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
    if(enif_is_identical(tpl[0], enif_make_atom(env, "winid"))) {
      if(!enif_get_int(env, tpl[1], &winid)) Badarg("winid"); // "wxWindowID"
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
    } else     if(enif_is_identical(tpl[0], enif_make_atom(env, "onGetItemText"))) {
      if(!enif_get_int(env, tpl[1], &onGetItemText)) Badarg("onGetItemText");
    } else     if(enif_is_identical(tpl[0], enif_make_atom(env, "onGetItemAttr"))) {
      if(!enif_get_int(env, tpl[1], &onGetItemAttr)) Badarg("onGetItemAttr");
    } else     if(enif_is_identical(tpl[0], enif_make_atom(env, "onGetItemColumnImage"))) {
      if(!enif_get_int(env, tpl[1], &onGetItemColumnImage)) Badarg("onGetItemColumnImage");
    } else      Badarg("Options");
  };
  if(!This) throw wxe_badarg(0);
  bool Result = This->Create(parent,winid,pos,size,style,*validator);

  This->onGetItemText = onGetItemText;
  This->onGetItemAttr = onGetItemAttr;
  This->onGetItemColumnImage = onGetItemColumnImage;
  This->me_ref = memenv->me_ref;

  wxeReturn rt = wxeReturn(memenv, Ecmd.caller, true);
  rt.send(  rt.make_bool(Result));

}

// wxListCtrl::DeleteAllItems
void wxListCtrl_DeleteAllItems(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd)
{
  ErlNifEnv *env = Ecmd.env;
  ERL_NIF_TERM * argv = Ecmd.args;
  wxListCtrl *This;
  This = (wxListCtrl *) memenv->getPtr(env, argv[0], "This");
  if(!This) throw wxe_badarg("This");
  bool Result = This->DeleteAllItems();
  wxeReturn rt = wxeReturn(memenv, Ecmd.caller, true);
  rt.send(  rt.make_bool(Result));

}

// wxListCtrl::DeleteColumn
void wxListCtrl_DeleteColumn(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd)
{
  ErlNifEnv *env = Ecmd.env;
  ERL_NIF_TERM * argv = Ecmd.args;
  wxListCtrl *This;
  This = (wxListCtrl *) memenv->getPtr(env, argv[0], "This");
  int col;
  if(!enif_get_int(env, argv[1], &col)) Badarg("col"); // int
  if(!This) throw wxe_badarg("This");
  bool Result = This->DeleteColumn(col);
  wxeReturn rt = wxeReturn(memenv, Ecmd.caller, true);
  rt.send(  rt.make_bool(Result));

}

// wxListCtrl::DeleteItem
void wxListCtrl_DeleteItem(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd)
{
  ErlNifEnv *env = Ecmd.env;
  ERL_NIF_TERM * argv = Ecmd.args;
  wxListCtrl *This;
  This = (wxListCtrl *) memenv->getPtr(env, argv[0], "This");
  long item;
  if(!enif_get_long(env, argv[1], &item)) Badarg("item");
  if(!This) throw wxe_badarg("This");
  bool Result = This->DeleteItem(item);
  wxeReturn rt = wxeReturn(memenv, Ecmd.caller, true);
  rt.send(  rt.make_bool(Result));

}

// wxListCtrl::EditLabel
void wxListCtrl_EditLabel(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd)
{
  ErlNifEnv *env = Ecmd.env;
  ERL_NIF_TERM * argv = Ecmd.args;
  wxListCtrl *This;
  This = (wxListCtrl *) memenv->getPtr(env, argv[0], "This");
  long item;
  if(!enif_get_long(env, argv[1], &item)) Badarg("item");
  if(!This) throw wxe_badarg("This");
  wxTextCtrl * Result = (wxTextCtrl*)This->EditLabel(item);
  wxeReturn rt = wxeReturn(memenv, Ecmd.caller, true);
  rt.send(  rt.make_ref(app->getRef((void *)Result,memenv), "wxTextCtrl"));

}

// wxListCtrl::EnsureVisible
void wxListCtrl_EnsureVisible(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd)
{
  ErlNifEnv *env = Ecmd.env;
  ERL_NIF_TERM * argv = Ecmd.args;
  wxListCtrl *This;
  This = (wxListCtrl *) memenv->getPtr(env, argv[0], "This");
  long item;
  if(!enif_get_long(env, argv[1], &item)) Badarg("item");
  if(!This) throw wxe_badarg("This");
  bool Result = This->EnsureVisible(item);
  wxeReturn rt = wxeReturn(memenv, Ecmd.caller, true);
  rt.send(  rt.make_bool(Result));

}

// wxListCtrl::FindItem
void wxListCtrl_FindItem_3_0(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd)
{
  bool partial=false;
  ErlNifEnv *env = Ecmd.env;
  ERL_NIF_TERM * argv = Ecmd.args;
  wxListCtrl *This;
  This = (wxListCtrl *) memenv->getPtr(env, argv[0], "This");
  long start;
  if(!enif_get_long(env, argv[1], &start)) Badarg("start");
  ErlNifBinary str_bin;
  wxString str;
  if(!enif_inspect_binary(env, argv[2], &str_bin)) Badarg("str");
  str = wxString(str_bin.data, wxConvUTF8, str_bin.size);
  ERL_NIF_TERM lstHead, lstTail;
  lstTail = argv[3];
  if(!enif_is_list(env, lstTail)) Badarg("Options");
  const ERL_NIF_TERM *tpl;
  int tpl_sz;
  while(!enif_is_empty_list(env, lstTail)) {
    if(!enif_get_list_cell(env, lstTail, &lstHead, &lstTail)) Badarg("Options");
    if(!enif_get_tuple(env, lstHead, &tpl_sz, &tpl) || tpl_sz != 2) Badarg("Options");
    if(enif_is_identical(tpl[0], enif_make_atom(env, "partial"))) {
  partial = enif_is_identical(tpl[1], WXE_ATOM_true);
    } else        Badarg("Options");
  };
  if(!This) throw wxe_badarg("This");
  long Result = This->FindItem(start,str,partial);
  wxeReturn rt = wxeReturn(memenv, Ecmd.caller, true);
  rt.send(  rt.make_int(Result));

}

// wxListCtrl::FindItem
void wxListCtrl_FindItem_3_1(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd)
{
  ErlNifEnv *env = Ecmd.env;
  ERL_NIF_TERM * argv = Ecmd.args;
  wxListCtrl *This;
  This = (wxListCtrl *) memenv->getPtr(env, argv[0], "This");
  long start;
  if(!enif_get_long(env, argv[1], &start)) Badarg("start");
  const ERL_NIF_TERM *pt_t;
  int pt_sz;
  if(!enif_get_tuple(env, argv[2], &pt_sz, &pt_t)) Badarg("pt");
  int ptX;
  if(!enif_get_int(env, pt_t[0], &ptX)) Badarg("pt");
  int ptY;
  if(!enif_get_int(env, pt_t[1], &ptY)) Badarg("pt");
  wxPoint pt = wxPoint(ptX,ptY);
  int direction;
  if(!enif_get_int(env, argv[3], &direction)) Badarg("direction"); // int
  if(!This) throw wxe_badarg("This");
  long Result = This->FindItem(start,pt,direction);
  wxeReturn rt = wxeReturn(memenv, Ecmd.caller, true);
  rt.send(  rt.make_int(Result));

}

// wxListCtrl::GetColumn
void wxListCtrl_GetColumn(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd)
{
  ErlNifEnv *env = Ecmd.env;
  ERL_NIF_TERM * argv = Ecmd.args;
  wxListCtrl *This;
  This = (wxListCtrl *) memenv->getPtr(env, argv[0], "This");
  int col;
  if(!enif_get_int(env, argv[1], &col)) Badarg("col"); // int
  wxListItem *item;
  item = (wxListItem *) memenv->getPtr(env, argv[2], "item");
  if(!This) throw wxe_badarg("This");
  bool Result = This->GetColumn(col,*item);
  wxeReturn rt = wxeReturn(memenv, Ecmd.caller, true);
  rt.send(  rt.make_bool(Result));

}

// wxListCtrl::GetColumnCount
void wxListCtrl_GetColumnCount(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd)
{
  ErlNifEnv *env = Ecmd.env;
  ERL_NIF_TERM * argv = Ecmd.args;
  wxListCtrl *This;
  This = (wxListCtrl *) memenv->getPtr(env, argv[0], "This");
  if(!This) throw wxe_badarg("This");
  int Result = This->GetColumnCount();
  wxeReturn rt = wxeReturn(memenv, Ecmd.caller, true);
  rt.send(  rt.make_int(Result));

}

// wxListCtrl::GetColumnWidth
void wxListCtrl_GetColumnWidth(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd)
{
  ErlNifEnv *env = Ecmd.env;
  ERL_NIF_TERM * argv = Ecmd.args;
  wxListCtrl *This;
  This = (wxListCtrl *) memenv->getPtr(env, argv[0], "This");
  int col;
  if(!enif_get_int(env, argv[1], &col)) Badarg("col"); // int
  if(!This) throw wxe_badarg("This");
  int Result = This->GetColumnWidth(col);
  wxeReturn rt = wxeReturn(memenv, Ecmd.caller, true);
  rt.send(  rt.make_int(Result));

}

// wxListCtrl::GetCountPerPage
void wxListCtrl_GetCountPerPage(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd)
{
  ErlNifEnv *env = Ecmd.env;
  ERL_NIF_TERM * argv = Ecmd.args;
  wxListCtrl *This;
  This = (wxListCtrl *) memenv->getPtr(env, argv[0], "This");
  if(!This) throw wxe_badarg("This");
  int Result = This->GetCountPerPage();
  wxeReturn rt = wxeReturn(memenv, Ecmd.caller, true);
  rt.send(  rt.make_int(Result));

}

// wxListCtrl::GetEditControl
void wxListCtrl_GetEditControl(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd)
{
  ErlNifEnv *env = Ecmd.env;
  ERL_NIF_TERM * argv = Ecmd.args;
  wxListCtrl *This;
  This = (wxListCtrl *) memenv->getPtr(env, argv[0], "This");
  if(!This) throw wxe_badarg("This");
  wxTextCtrl * Result = (wxTextCtrl*)This->GetEditControl();
  wxeReturn rt = wxeReturn(memenv, Ecmd.caller, true);
  rt.send(  rt.make_ref(app->getRef((void *)Result,memenv), "wxTextCtrl"));

}

// wxListCtrl::GetImageList
void wxListCtrl_GetImageList(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd)
{
  ErlNifEnv *env = Ecmd.env;
  ERL_NIF_TERM * argv = Ecmd.args;
  wxListCtrl *This;
  This = (wxListCtrl *) memenv->getPtr(env, argv[0], "This");
  int which;
  if(!enif_get_int(env, argv[1], &which)) Badarg("which"); // int
  if(!This) throw wxe_badarg("This");
  wxImageList * Result = (wxImageList*)This->GetImageList(which);
  wxeReturn rt = wxeReturn(memenv, Ecmd.caller, true);
  rt.send(  rt.make_ref(app->getRef((void *)Result,memenv), "wxImageList"));

}

// wxListCtrl::GetItem
void wxListCtrl_GetItem(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd)
{
  ErlNifEnv *env = Ecmd.env;
  ERL_NIF_TERM * argv = Ecmd.args;
  wxListCtrl *This;
  This = (wxListCtrl *) memenv->getPtr(env, argv[0], "This");
  wxListItem *info;
  info = (wxListItem *) memenv->getPtr(env, argv[1], "info");
  if(!This) throw wxe_badarg("This");
  bool Result = This->GetItem(*info);
  wxeReturn rt = wxeReturn(memenv, Ecmd.caller, true);
  rt.send(  rt.make_bool(Result));

}

// wxListCtrl::GetItemBackgroundColour
void wxListCtrl_GetItemBackgroundColour(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd)
{
  ErlNifEnv *env = Ecmd.env;
  ERL_NIF_TERM * argv = Ecmd.args;
  wxListCtrl *This;
  This = (wxListCtrl *) memenv->getPtr(env, argv[0], "This");
  long item;
  if(!enif_get_long(env, argv[1], &item)) Badarg("item");
  if(!This) throw wxe_badarg("This");
  wxColour Result = This->GetItemBackgroundColour(item);
  wxeReturn rt = wxeReturn(memenv, Ecmd.caller, true);
  rt.send(  rt.make(Result));

}

// wxListCtrl::GetItemCount
void wxListCtrl_GetItemCount(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd)
{
  ErlNifEnv *env = Ecmd.env;
  ERL_NIF_TERM * argv = Ecmd.args;
  wxListCtrl *This;
  This = (wxListCtrl *) memenv->getPtr(env, argv[0], "This");
  if(!This) throw wxe_badarg("This");
  int Result = This->GetItemCount();
  wxeReturn rt = wxeReturn(memenv, Ecmd.caller, true);
  rt.send(  rt.make_int(Result));

}

// wxListCtrl::GetItemData
void wxListCtrl_GetItemData(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd)
{
  ErlNifEnv *env = Ecmd.env;
  ERL_NIF_TERM * argv = Ecmd.args;
  wxListCtrl *This;
  This = (wxListCtrl *) memenv->getPtr(env, argv[0], "This");
  long item;
  if(!enif_get_long(env, argv[1], &item)) Badarg("item");
  if(!This) throw wxe_badarg("This");
  wxUIntPtr Result = This->GetItemData(item);
  wxeReturn rt = wxeReturn(memenv, Ecmd.caller, true);
  rt.send(  rt.make_int(Result));

}

// wxListCtrl::GetItemFont
void wxListCtrl_GetItemFont(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd)
{
  ErlNifEnv *env = Ecmd.env;
  ERL_NIF_TERM * argv = Ecmd.args;
  wxListCtrl *This;
  This = (wxListCtrl *) memenv->getPtr(env, argv[0], "This");
  long item;
  if(!enif_get_long(env, argv[1], &item)) Badarg("item");
  if(!This) throw wxe_badarg("This");
  wxFont * Result = new wxFont(This->GetItemFont(item)); app->newPtr((void *) Result,3, memenv);;
  wxeReturn rt = wxeReturn(memenv, Ecmd.caller, true);
  rt.send(  rt.make_ref(app->getRef((void *)Result,memenv), "wxFont"));

}

// wxListCtrl::GetItemPosition
void wxListCtrl_GetItemPosition(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd)
{
  wxPoint pos;
  ErlNifEnv *env = Ecmd.env;
  ERL_NIF_TERM * argv = Ecmd.args;
  wxListCtrl *This;
  This = (wxListCtrl *) memenv->getPtr(env, argv[0], "This");
  long item;
  if(!enif_get_long(env, argv[1], &item)) Badarg("item");
  if(!This) throw wxe_badarg("This");
  bool Result = This->GetItemPosition(item,pos);
  wxeReturn rt = wxeReturn(memenv, Ecmd.caller, true);
  ERL_NIF_TERM msg = enif_make_tuple2(rt.env,
  rt.make_bool(Result),
    rt.make(pos));
  rt.send(msg);

}

// wxListCtrl::GetItemRect
void wxListCtrl_GetItemRect(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd)
{
  wxRect rect;
  int code=wxLIST_RECT_BOUNDS;
  ErlNifEnv *env = Ecmd.env;
  ERL_NIF_TERM * argv = Ecmd.args;
  wxListCtrl *This;
  This = (wxListCtrl *) memenv->getPtr(env, argv[0], "This");
  long item;
  if(!enif_get_long(env, argv[1], &item)) Badarg("item");
  ERL_NIF_TERM lstHead, lstTail;
  lstTail = argv[2];
  if(!enif_is_list(env, lstTail)) Badarg("Options");
  const ERL_NIF_TERM *tpl;
  int tpl_sz;
  while(!enif_is_empty_list(env, lstTail)) {
    if(!enif_get_list_cell(env, lstTail, &lstHead, &lstTail)) Badarg("Options");
    if(!enif_get_tuple(env, lstHead, &tpl_sz, &tpl) || tpl_sz != 2) Badarg("Options");
    if(enif_is_identical(tpl[0], enif_make_atom(env, "code"))) {
  if(!enif_get_int(env, tpl[1], &code)) Badarg("code"); // int
    } else        Badarg("Options");
  };
  if(!This) throw wxe_badarg("This");
  bool Result = This->GetItemRect(item,rect,code);
  wxeReturn rt = wxeReturn(memenv, Ecmd.caller, true);
  ERL_NIF_TERM msg = enif_make_tuple2(rt.env,
  rt.make_bool(Result),
    rt.make(rect));
  rt.send(msg);

}

// wxListCtrl::GetItemSpacing
void wxListCtrl_GetItemSpacing(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd)
{
  ErlNifEnv *env = Ecmd.env;
  ERL_NIF_TERM * argv = Ecmd.args;
  wxListCtrl *This;
  This = (wxListCtrl *) memenv->getPtr(env, argv[0], "This");
  if(!This) throw wxe_badarg("This");
  wxSize Result = This->GetItemSpacing();
  wxeReturn rt = wxeReturn(memenv, Ecmd.caller, true);
  rt.send(  rt.make(Result));

}

// wxListCtrl::GetItemState
void wxListCtrl_GetItemState(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd)
{
  ErlNifEnv *env = Ecmd.env;
  ERL_NIF_TERM * argv = Ecmd.args;
  wxListCtrl *This;
  This = (wxListCtrl *) memenv->getPtr(env, argv[0], "This");
  long item;
  if(!enif_get_long(env, argv[1], &item)) Badarg("item");
  long stateMask;
  if(!enif_get_long(env, argv[2], &stateMask)) Badarg("stateMask");
  if(!This) throw wxe_badarg("This");
  int Result = This->GetItemState(item,stateMask);
  wxeReturn rt = wxeReturn(memenv, Ecmd.caller, true);
  rt.send(  rt.make_int(Result));

}

// wxListCtrl::GetItemText
void wxListCtrl_GetItemText(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd)
{
  int col=0;
  ErlNifEnv *env = Ecmd.env;
  ERL_NIF_TERM * argv = Ecmd.args;
  wxListCtrl *This;
  This = (wxListCtrl *) memenv->getPtr(env, argv[0], "This");
  long item;
  if(!enif_get_long(env, argv[1], &item)) Badarg("item");
  ERL_NIF_TERM lstHead, lstTail;
  lstTail = argv[2];
  if(!enif_is_list(env, lstTail)) Badarg("Options");
  const ERL_NIF_TERM *tpl;
  int tpl_sz;
  while(!enif_is_empty_list(env, lstTail)) {
    if(!enif_get_list_cell(env, lstTail, &lstHead, &lstTail)) Badarg("Options");
    if(!enif_get_tuple(env, lstHead, &tpl_sz, &tpl) || tpl_sz != 2) Badarg("Options");
    if(enif_is_identical(tpl[0], enif_make_atom(env, "col"))) {
  if(!enif_get_int(env, tpl[1], &col)) Badarg("col"); // int
    } else        Badarg("Options");
  };
  if(!This) throw wxe_badarg("This");
  wxString Result = This->GetItemText(item,col);
  wxeReturn rt = wxeReturn(memenv, Ecmd.caller, true);
  rt.send(  rt.make(Result));

}

// wxListCtrl::GetItemTextColour
void wxListCtrl_GetItemTextColour(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd)
{
  ErlNifEnv *env = Ecmd.env;
  ERL_NIF_TERM * argv = Ecmd.args;
  wxListCtrl *This;
  This = (wxListCtrl *) memenv->getPtr(env, argv[0], "This");
  long item;
  if(!enif_get_long(env, argv[1], &item)) Badarg("item");
  if(!This) throw wxe_badarg("This");
  wxColour Result = This->GetItemTextColour(item);
  wxeReturn rt = wxeReturn(memenv, Ecmd.caller, true);
  rt.send(  rt.make(Result));

}

// wxListCtrl::GetNextItem
void wxListCtrl_GetNextItem(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd)
{
  int geometry=wxLIST_NEXT_ALL;
  int state=wxLIST_STATE_DONTCARE;
  ErlNifEnv *env = Ecmd.env;
  ERL_NIF_TERM * argv = Ecmd.args;
  wxListCtrl *This;
  This = (wxListCtrl *) memenv->getPtr(env, argv[0], "This");
  long item;
  if(!enif_get_long(env, argv[1], &item)) Badarg("item");
  ERL_NIF_TERM lstHead, lstTail;
  lstTail = argv[2];
  if(!enif_is_list(env, lstTail)) Badarg("Options");
  const ERL_NIF_TERM *tpl;
  int tpl_sz;
  while(!enif_is_empty_list(env, lstTail)) {
    if(!enif_get_list_cell(env, lstTail, &lstHead, &lstTail)) Badarg("Options");
    if(!enif_get_tuple(env, lstHead, &tpl_sz, &tpl) || tpl_sz != 2) Badarg("Options");
    if(enif_is_identical(tpl[0], enif_make_atom(env, "geometry"))) {
  if(!enif_get_int(env, tpl[1], &geometry)) Badarg("geometry"); // int
    } else     if(enif_is_identical(tpl[0], enif_make_atom(env, "state"))) {
  if(!enif_get_int(env, tpl[1], &state)) Badarg("state"); // int
    } else        Badarg("Options");
  };
  if(!This) throw wxe_badarg("This");
  long Result = This->GetNextItem(item,geometry,state);
  wxeReturn rt = wxeReturn(memenv, Ecmd.caller, true);
  rt.send(  rt.make_int(Result));

}

// wxListCtrl::GetSelectedItemCount
void wxListCtrl_GetSelectedItemCount(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd)
{
  ErlNifEnv *env = Ecmd.env;
  ERL_NIF_TERM * argv = Ecmd.args;
  wxListCtrl *This;
  This = (wxListCtrl *) memenv->getPtr(env, argv[0], "This");
  if(!This) throw wxe_badarg("This");
  int Result = This->GetSelectedItemCount();
  wxeReturn rt = wxeReturn(memenv, Ecmd.caller, true);
  rt.send(  rt.make_int(Result));

}

// wxListCtrl::GetTextColour
void wxListCtrl_GetTextColour(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd)
{
  ErlNifEnv *env = Ecmd.env;
  ERL_NIF_TERM * argv = Ecmd.args;
  wxListCtrl *This;
  This = (wxListCtrl *) memenv->getPtr(env, argv[0], "This");
  if(!This) throw wxe_badarg("This");
  wxColour Result = This->GetTextColour();
  wxeReturn rt = wxeReturn(memenv, Ecmd.caller, true);
  rt.send(  rt.make(Result));

}

// wxListCtrl::GetTopItem
void wxListCtrl_GetTopItem(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd)
{
  ErlNifEnv *env = Ecmd.env;
  ERL_NIF_TERM * argv = Ecmd.args;
  wxListCtrl *This;
  This = (wxListCtrl *) memenv->getPtr(env, argv[0], "This");
  if(!This) throw wxe_badarg("This");
  long Result = This->GetTopItem();
  wxeReturn rt = wxeReturn(memenv, Ecmd.caller, true);
  rt.send(  rt.make_int(Result));

}

// wxListCtrl::GetViewRect
void wxListCtrl_GetViewRect(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd)
{
  ErlNifEnv *env = Ecmd.env;
  ERL_NIF_TERM * argv = Ecmd.args;
  wxListCtrl *This;
  This = (wxListCtrl *) memenv->getPtr(env, argv[0], "This");
  if(!This) throw wxe_badarg("This");
  wxRect Result = This->GetViewRect();
  wxeReturn rt = wxeReturn(memenv, Ecmd.caller, true);
  rt.send(  rt.make(Result));

}

// wxListCtrl::HitTest
void wxListCtrl_HitTest(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd)
{
  int flags;
  long ptrSubItem;
  ErlNifEnv *env = Ecmd.env;
  ERL_NIF_TERM * argv = Ecmd.args;
  wxListCtrl *This;
  This = (wxListCtrl *) memenv->getPtr(env, argv[0], "This");
  const ERL_NIF_TERM *point_t;
  int point_sz;
  if(!enif_get_tuple(env, argv[1], &point_sz, &point_t)) Badarg("point");
  int pointX;
  if(!enif_get_int(env, point_t[0], &pointX)) Badarg("point");
  int pointY;
  if(!enif_get_int(env, point_t[1], &pointY)) Badarg("point");
  wxPoint point = wxPoint(pointX,pointY);
  if(!This) throw wxe_badarg("This");
  long Result = This->HitTest(point,flags,&ptrSubItem);
  wxeReturn rt = wxeReturn(memenv, Ecmd.caller, true);
  ERL_NIF_TERM msg = enif_make_tuple3(rt.env,
  rt.make_int(Result),
    rt.make_int(flags),
  rt.make_int(ptrSubItem));
  rt.send(msg);

}

// wxListCtrl::InsertColumn
void wxListCtrl_InsertColumn_2(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd)
{
  ErlNifEnv *env = Ecmd.env;
  ERL_NIF_TERM * argv = Ecmd.args;
  wxListCtrl *This;
  This = (wxListCtrl *) memenv->getPtr(env, argv[0], "This");
  long col;
  if(!enif_get_long(env, argv[1], &col)) Badarg("col");
  wxListItem *info;
  info = (wxListItem *) memenv->getPtr(env, argv[2], "info");
  if(!This) throw wxe_badarg("This");
  long Result = This->InsertColumn(col,*info);
  wxeReturn rt = wxeReturn(memenv, Ecmd.caller, true);
  rt.send(  rt.make_int(Result));

}

// wxListCtrl::InsertColumn
void wxListCtrl_InsertColumn_3(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd)
{
  int format=wxLIST_FORMAT_LEFT;
  int width=wxLIST_AUTOSIZE;
  ErlNifEnv *env = Ecmd.env;
  ERL_NIF_TERM * argv = Ecmd.args;
  wxListCtrl *This;
  This = (wxListCtrl *) memenv->getPtr(env, argv[0], "This");
  long col;
  if(!enif_get_long(env, argv[1], &col)) Badarg("col");
  ErlNifBinary heading_bin;
  wxString heading;
  if(!enif_inspect_binary(env, argv[2], &heading_bin)) Badarg("heading");
  heading = wxString(heading_bin.data, wxConvUTF8, heading_bin.size);
  ERL_NIF_TERM lstHead, lstTail;
  lstTail = argv[3];
  if(!enif_is_list(env, lstTail)) Badarg("Options");
  const ERL_NIF_TERM *tpl;
  int tpl_sz;
  while(!enif_is_empty_list(env, lstTail)) {
    if(!enif_get_list_cell(env, lstTail, &lstHead, &lstTail)) Badarg("Options");
    if(!enif_get_tuple(env, lstHead, &tpl_sz, &tpl) || tpl_sz != 2) Badarg("Options");
    if(enif_is_identical(tpl[0], enif_make_atom(env, "format"))) {
  if(!enif_get_int(env, tpl[1], &format)) Badarg("format"); // int
    } else     if(enif_is_identical(tpl[0], enif_make_atom(env, "width"))) {
  if(!enif_get_int(env, tpl[1], &width)) Badarg("width"); // int
    } else        Badarg("Options");
  };
  if(!This) throw wxe_badarg("This");
  long Result = This->InsertColumn(col,heading,format,width);
  wxeReturn rt = wxeReturn(memenv, Ecmd.caller, true);
  rt.send(  rt.make_int(Result));

}

// wxListCtrl::InsertItem
void wxListCtrl_InsertItem_1(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd)
{
  ErlNifEnv *env = Ecmd.env;
  ERL_NIF_TERM * argv = Ecmd.args;
  wxListCtrl *This;
  This = (wxListCtrl *) memenv->getPtr(env, argv[0], "This");
  wxListItem *info;
  info = (wxListItem *) memenv->getPtr(env, argv[1], "info");
  if(!This) throw wxe_badarg("This");
  long Result = This->InsertItem(*info);
  wxeReturn rt = wxeReturn(memenv, Ecmd.caller, true);
  rt.send(  rt.make_int(Result));

}

// wxListCtrl::InsertItem
void wxListCtrl_InsertItem_2_1(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd)
{
  ErlNifEnv *env = Ecmd.env;
  ERL_NIF_TERM * argv = Ecmd.args;
  wxListCtrl *This;
  This = (wxListCtrl *) memenv->getPtr(env, argv[0], "This");
  long index;
  if(!enif_get_long(env, argv[1], &index)) Badarg("index");
  ErlNifBinary label_bin;
  wxString label;
  if(!enif_inspect_binary(env, argv[2], &label_bin)) Badarg("label");
  label = wxString(label_bin.data, wxConvUTF8, label_bin.size);
  if(!This) throw wxe_badarg("This");
  long Result = This->InsertItem(index,label);
  wxeReturn rt = wxeReturn(memenv, Ecmd.caller, true);
  rt.send(  rt.make_int(Result));

}

// wxListCtrl::InsertItem
void wxListCtrl_InsertItem_2_0(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd)
{
  ErlNifEnv *env = Ecmd.env;
  ERL_NIF_TERM * argv = Ecmd.args;
  wxListCtrl *This;
  This = (wxListCtrl *) memenv->getPtr(env, argv[0], "This");
  long index;
  if(!enif_get_long(env, argv[1], &index)) Badarg("index");
  int imageIndex;
  if(!enif_get_int(env, argv[2], &imageIndex)) Badarg("imageIndex"); // int
  if(!This) throw wxe_badarg("This");
  long Result = This->InsertItem(index,imageIndex);
  wxeReturn rt = wxeReturn(memenv, Ecmd.caller, true);
  rt.send(  rt.make_int(Result));

}

// wxListCtrl::InsertItem
void wxListCtrl_InsertItem_3(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd)
{
  ErlNifEnv *env = Ecmd.env;
  ERL_NIF_TERM * argv = Ecmd.args;
  wxListCtrl *This;
  This = (wxListCtrl *) memenv->getPtr(env, argv[0], "This");
  long index;
  if(!enif_get_long(env, argv[1], &index)) Badarg("index");
  ErlNifBinary label_bin;
  wxString label;
  if(!enif_inspect_binary(env, argv[2], &label_bin)) Badarg("label");
  label = wxString(label_bin.data, wxConvUTF8, label_bin.size);
  int imageIndex;
  if(!enif_get_int(env, argv[3], &imageIndex)) Badarg("imageIndex"); // int
  if(!This) throw wxe_badarg("This");
  long Result = This->InsertItem(index,label,imageIndex);
  wxeReturn rt = wxeReturn(memenv, Ecmd.caller, true);
  rt.send(  rt.make_int(Result));

}

// wxListCtrl::RefreshItem
void wxListCtrl_RefreshItem(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd)
{
  ErlNifEnv *env = Ecmd.env;
  ERL_NIF_TERM * argv = Ecmd.args;
  wxListCtrl *This;
  This = (wxListCtrl *) memenv->getPtr(env, argv[0], "This");
  long item;
  if(!enif_get_long(env, argv[1], &item)) Badarg("item");
  if(!This) throw wxe_badarg("This");
  This->RefreshItem(item);

}

// wxListCtrl::RefreshItems
void wxListCtrl_RefreshItems(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd)
{
  ErlNifEnv *env = Ecmd.env;
  ERL_NIF_TERM * argv = Ecmd.args;
  wxListCtrl *This;
  This = (wxListCtrl *) memenv->getPtr(env, argv[0], "This");
  long itemFrom;
  if(!enif_get_long(env, argv[1], &itemFrom)) Badarg("itemFrom");
  long itemTo;
  if(!enif_get_long(env, argv[2], &itemTo)) Badarg("itemTo");
  if(!This) throw wxe_badarg("This");
  This->RefreshItems(itemFrom,itemTo);

}

// wxListCtrl::ScrollList
void wxListCtrl_ScrollList(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd)
{
  ErlNifEnv *env = Ecmd.env;
  ERL_NIF_TERM * argv = Ecmd.args;
  wxListCtrl *This;
  This = (wxListCtrl *) memenv->getPtr(env, argv[0], "This");
  int dx;
  if(!enif_get_int(env, argv[1], &dx)) Badarg("dx"); // int
  int dy;
  if(!enif_get_int(env, argv[2], &dy)) Badarg("dy"); // int
  if(!This) throw wxe_badarg("This");
  bool Result = This->ScrollList(dx,dy);
  wxeReturn rt = wxeReturn(memenv, Ecmd.caller, true);
  rt.send(  rt.make_bool(Result));

}

// wxListCtrl::SetBackgroundColour
void wxListCtrl_SetBackgroundColour(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd)
{
  ErlNifEnv *env = Ecmd.env;
  ERL_NIF_TERM * argv = Ecmd.args;
  wxListCtrl *This;
  This = (wxListCtrl *) memenv->getPtr(env, argv[0], "This");
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
  bool Result = This->SetBackgroundColour(col);
  wxeReturn rt = wxeReturn(memenv, Ecmd.caller, true);
  rt.send(  rt.make_bool(Result));

}

// wxListCtrl::SetColumn
void wxListCtrl_SetColumn(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd)
{
  ErlNifEnv *env = Ecmd.env;
  ERL_NIF_TERM * argv = Ecmd.args;
  wxListCtrl *This;
  This = (wxListCtrl *) memenv->getPtr(env, argv[0], "This");
  int col;
  if(!enif_get_int(env, argv[1], &col)) Badarg("col"); // int
  wxListItem *item;
  item = (wxListItem *) memenv->getPtr(env, argv[2], "item");
  if(!This) throw wxe_badarg("This");
  bool Result = This->SetColumn(col,*item);
  wxeReturn rt = wxeReturn(memenv, Ecmd.caller, true);
  rt.send(  rt.make_bool(Result));

}

// wxListCtrl::SetColumnWidth
void wxListCtrl_SetColumnWidth(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd)
{
  ErlNifEnv *env = Ecmd.env;
  ERL_NIF_TERM * argv = Ecmd.args;
  wxListCtrl *This;
  This = (wxListCtrl *) memenv->getPtr(env, argv[0], "This");
  int col;
  if(!enif_get_int(env, argv[1], &col)) Badarg("col"); // int
  int width;
  if(!enif_get_int(env, argv[2], &width)) Badarg("width"); // int
  if(!This) throw wxe_badarg("This");
  bool Result = This->SetColumnWidth(col,width);
  wxeReturn rt = wxeReturn(memenv, Ecmd.caller, true);
  rt.send(  rt.make_bool(Result));

}

// wxListCtrl::SetImageList
void wxListCtrl_SetImageList(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd)
{
  ErlNifEnv *env = Ecmd.env;
  ERL_NIF_TERM * argv = Ecmd.args;
  wxListCtrl *This;
  This = (wxListCtrl *) memenv->getPtr(env, argv[0], "This");
  wxImageList *imageList;
  imageList = (wxImageList *) memenv->getPtr(env, argv[1], "imageList");
  int which;
  if(!enif_get_int(env, argv[2], &which)) Badarg("which"); // int
  if(!This) throw wxe_badarg("This");
  This->SetImageList(imageList,which);

}

// wxListCtrl::SetItem
void wxListCtrl_SetItem_1(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd)
{
  ErlNifEnv *env = Ecmd.env;
  ERL_NIF_TERM * argv = Ecmd.args;
  wxListCtrl *This;
  This = (wxListCtrl *) memenv->getPtr(env, argv[0], "This");
  wxListItem *info;
  info = (wxListItem *) memenv->getPtr(env, argv[1], "info");
  if(!This) throw wxe_badarg("This");
  bool Result = This->SetItem(*info);
  wxeReturn rt = wxeReturn(memenv, Ecmd.caller, true);
  rt.send(  rt.make_bool(Result));

}

// wxListCtrl::SetItem
void wxListCtrl_SetItem_4(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd)
{
  int imageId=-1;
  ErlNifEnv *env = Ecmd.env;
  ERL_NIF_TERM * argv = Ecmd.args;
  wxListCtrl *This;
  This = (wxListCtrl *) memenv->getPtr(env, argv[0], "This");
  long index;
  if(!enif_get_long(env, argv[1], &index)) Badarg("index");
  int column;
  if(!enif_get_int(env, argv[2], &column)) Badarg("column"); // int
  ErlNifBinary label_bin;
  wxString label;
  if(!enif_inspect_binary(env, argv[3], &label_bin)) Badarg("label");
  label = wxString(label_bin.data, wxConvUTF8, label_bin.size);
  ERL_NIF_TERM lstHead, lstTail;
  lstTail = argv[4];
  if(!enif_is_list(env, lstTail)) Badarg("Options");
  const ERL_NIF_TERM *tpl;
  int tpl_sz;
  while(!enif_is_empty_list(env, lstTail)) {
    if(!enif_get_list_cell(env, lstTail, &lstHead, &lstTail)) Badarg("Options");
    if(!enif_get_tuple(env, lstHead, &tpl_sz, &tpl) || tpl_sz != 2) Badarg("Options");
    if(enif_is_identical(tpl[0], enif_make_atom(env, "imageId"))) {
  if(!enif_get_int(env, tpl[1], &imageId)) Badarg("imageId"); // int
    } else        Badarg("Options");
  };
  if(!This) throw wxe_badarg("This");
  bool Result = This->SetItem(index,column,label,imageId);
  wxeReturn rt = wxeReturn(memenv, Ecmd.caller, true);
  rt.send(  rt.make_bool(Result));

}

// wxListCtrl::SetItemBackgroundColour
void wxListCtrl_SetItemBackgroundColour(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd)
{
  ErlNifEnv *env = Ecmd.env;
  ERL_NIF_TERM * argv = Ecmd.args;
  wxListCtrl *This;
  This = (wxListCtrl *) memenv->getPtr(env, argv[0], "This");
  long item;
  if(!enif_get_long(env, argv[1], &item)) Badarg("item");
  const ERL_NIF_TERM *col_t;
  int col_sz;
  if(!enif_get_tuple(env, argv[2], &col_sz, &col_t)) Badarg("col");
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
  This->SetItemBackgroundColour(item,col);

}

// wxListCtrl::SetItemCount
void wxListCtrl_SetItemCount(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd)
{
  ErlNifEnv *env = Ecmd.env;
  ERL_NIF_TERM * argv = Ecmd.args;
  wxListCtrl *This;
  This = (wxListCtrl *) memenv->getPtr(env, argv[0], "This");
  long count;
  if(!enif_get_long(env, argv[1], &count)) Badarg("count");
  if(!This) throw wxe_badarg("This");
  This->SetItemCount(count);

}

// wxListCtrl::SetItemData
void wxListCtrl_SetItemData(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd)
{
  ErlNifEnv *env = Ecmd.env;
  ERL_NIF_TERM * argv = Ecmd.args;
  wxListCtrl *This;
  This = (wxListCtrl *) memenv->getPtr(env, argv[0], "This");
  long item;
  if(!enif_get_long(env, argv[1], &item)) Badarg("item");
  long data;
  if(!enif_get_long(env, argv[2], &data)) Badarg("data");
  if(!This) throw wxe_badarg("This");
  bool Result = This->SetItemData(item,data);
  wxeReturn rt = wxeReturn(memenv, Ecmd.caller, true);
  rt.send(  rt.make_bool(Result));

}

// wxListCtrl::SetItemFont
void wxListCtrl_SetItemFont(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd)
{
  ErlNifEnv *env = Ecmd.env;
  ERL_NIF_TERM * argv = Ecmd.args;
  wxListCtrl *This;
  This = (wxListCtrl *) memenv->getPtr(env, argv[0], "This");
  long item;
  if(!enif_get_long(env, argv[1], &item)) Badarg("item");
  wxFont *font;
  font = (wxFont *) memenv->getPtr(env, argv[2], "font");
  if(!This) throw wxe_badarg("This");
  This->SetItemFont(item,*font);

}

// wxListCtrl::SetItemImage
void wxListCtrl_SetItemImage(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd)
{
  int selImage=-1;
  ErlNifEnv *env = Ecmd.env;
  ERL_NIF_TERM * argv = Ecmd.args;
  wxListCtrl *This;
  This = (wxListCtrl *) memenv->getPtr(env, argv[0], "This");
  long item;
  if(!enif_get_long(env, argv[1], &item)) Badarg("item");
  int image;
  if(!enif_get_int(env, argv[2], &image)) Badarg("image"); // int
  ERL_NIF_TERM lstHead, lstTail;
  lstTail = argv[3];
  if(!enif_is_list(env, lstTail)) Badarg("Options");
  const ERL_NIF_TERM *tpl;
  int tpl_sz;
  while(!enif_is_empty_list(env, lstTail)) {
    if(!enif_get_list_cell(env, lstTail, &lstHead, &lstTail)) Badarg("Options");
    if(!enif_get_tuple(env, lstHead, &tpl_sz, &tpl) || tpl_sz != 2) Badarg("Options");
    if(enif_is_identical(tpl[0], enif_make_atom(env, "selImage"))) {
  if(!enif_get_int(env, tpl[1], &selImage)) Badarg("selImage"); // int
    } else        Badarg("Options");
  };
  if(!This) throw wxe_badarg("This");
  bool Result = This->SetItemImage(item,image,selImage);
  wxeReturn rt = wxeReturn(memenv, Ecmd.caller, true);
  rt.send(  rt.make_bool(Result));

}

// wxListCtrl::SetItemColumnImage
void wxListCtrl_SetItemColumnImage(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd)
{
  ErlNifEnv *env = Ecmd.env;
  ERL_NIF_TERM * argv = Ecmd.args;
  wxListCtrl *This;
  This = (wxListCtrl *) memenv->getPtr(env, argv[0], "This");
  long item;
  if(!enif_get_long(env, argv[1], &item)) Badarg("item");
  long column;
  if(!enif_get_long(env, argv[2], &column)) Badarg("column");
  int image;
  if(!enif_get_int(env, argv[3], &image)) Badarg("image"); // int
  if(!This) throw wxe_badarg("This");
  bool Result = This->SetItemColumnImage(item,column,image);
  wxeReturn rt = wxeReturn(memenv, Ecmd.caller, true);
  rt.send(  rt.make_bool(Result));

}

// wxListCtrl::SetItemPosition
void wxListCtrl_SetItemPosition(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd)
{
  ErlNifEnv *env = Ecmd.env;
  ERL_NIF_TERM * argv = Ecmd.args;
  wxListCtrl *This;
  This = (wxListCtrl *) memenv->getPtr(env, argv[0], "This");
  long item;
  if(!enif_get_long(env, argv[1], &item)) Badarg("item");
  const ERL_NIF_TERM *pos_t;
  int pos_sz;
  if(!enif_get_tuple(env, argv[2], &pos_sz, &pos_t)) Badarg("pos");
  int posX;
  if(!enif_get_int(env, pos_t[0], &posX)) Badarg("pos");
  int posY;
  if(!enif_get_int(env, pos_t[1], &posY)) Badarg("pos");
  wxPoint pos = wxPoint(posX,posY);
  if(!This) throw wxe_badarg("This");
  bool Result = This->SetItemPosition(item,pos);
  wxeReturn rt = wxeReturn(memenv, Ecmd.caller, true);
  rt.send(  rt.make_bool(Result));

}

// wxListCtrl::SetItemState
void wxListCtrl_SetItemState(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd)
{
  ErlNifEnv *env = Ecmd.env;
  ERL_NIF_TERM * argv = Ecmd.args;
  wxListCtrl *This;
  This = (wxListCtrl *) memenv->getPtr(env, argv[0], "This");
  long item;
  if(!enif_get_long(env, argv[1], &item)) Badarg("item");
  long state;
  if(!enif_get_long(env, argv[2], &state)) Badarg("state");
  long stateMask;
  if(!enif_get_long(env, argv[3], &stateMask)) Badarg("stateMask");
  if(!This) throw wxe_badarg("This");
  bool Result = This->SetItemState(item,state,stateMask);
  wxeReturn rt = wxeReturn(memenv, Ecmd.caller, true);
  rt.send(  rt.make_bool(Result));

}

// wxListCtrl::SetItemText
void wxListCtrl_SetItemText(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd)
{
  ErlNifEnv *env = Ecmd.env;
  ERL_NIF_TERM * argv = Ecmd.args;
  wxListCtrl *This;
  This = (wxListCtrl *) memenv->getPtr(env, argv[0], "This");
  long item;
  if(!enif_get_long(env, argv[1], &item)) Badarg("item");
  ErlNifBinary text_bin;
  wxString text;
  if(!enif_inspect_binary(env, argv[2], &text_bin)) Badarg("text");
  text = wxString(text_bin.data, wxConvUTF8, text_bin.size);
  if(!This) throw wxe_badarg("This");
  This->SetItemText(item,text);

}

// wxListCtrl::SetItemTextColour
void wxListCtrl_SetItemTextColour(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd)
{
  ErlNifEnv *env = Ecmd.env;
  ERL_NIF_TERM * argv = Ecmd.args;
  wxListCtrl *This;
  This = (wxListCtrl *) memenv->getPtr(env, argv[0], "This");
  long item;
  if(!enif_get_long(env, argv[1], &item)) Badarg("item");
  const ERL_NIF_TERM *col_t;
  int col_sz;
  if(!enif_get_tuple(env, argv[2], &col_sz, &col_t)) Badarg("col");
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
  This->SetItemTextColour(item,col);

}

// wxListCtrl::SetSingleStyle
void wxListCtrl_SetSingleStyle(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd)
{
  bool add=true;
  ErlNifEnv *env = Ecmd.env;
  ERL_NIF_TERM * argv = Ecmd.args;
  wxListCtrl *This;
  This = (wxListCtrl *) memenv->getPtr(env, argv[0], "This");
  long style;
  if(!enif_get_long(env, argv[1], &style)) Badarg("style");
  ERL_NIF_TERM lstHead, lstTail;
  lstTail = argv[2];
  if(!enif_is_list(env, lstTail)) Badarg("Options");
  const ERL_NIF_TERM *tpl;
  int tpl_sz;
  while(!enif_is_empty_list(env, lstTail)) {
    if(!enif_get_list_cell(env, lstTail, &lstHead, &lstTail)) Badarg("Options");
    if(!enif_get_tuple(env, lstHead, &tpl_sz, &tpl) || tpl_sz != 2) Badarg("Options");
    if(enif_is_identical(tpl[0], enif_make_atom(env, "add"))) {
  add = enif_is_identical(tpl[1], WXE_ATOM_true);
    } else        Badarg("Options");
  };
  if(!This) throw wxe_badarg("This");
  This->SetSingleStyle(style,add);

}

// wxListCtrl::SetTextColour
void wxListCtrl_SetTextColour(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd)
{
  ErlNifEnv *env = Ecmd.env;
  ERL_NIF_TERM * argv = Ecmd.args;
  wxListCtrl *This;
  This = (wxListCtrl *) memenv->getPtr(env, argv[0], "This");
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
  This->SetTextColour(col);

}

// wxListCtrl::SetWindowStyleFlag
void wxListCtrl_SetWindowStyleFlag(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd)
{
  ErlNifEnv *env = Ecmd.env;
  ERL_NIF_TERM * argv = Ecmd.args;
  wxListCtrl *This;
  This = (wxListCtrl *) memenv->getPtr(env, argv[0], "This");
  long style;
  if(!enif_get_long(env, argv[1], &style)) Badarg("style");
  if(!This) throw wxe_badarg("This");
  This->SetWindowStyleFlag(style);

}


void wxListCtrl_SortItems(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd)
{
  ErlNifEnv *env = Ecmd.env;
  ERL_NIF_TERM * argv = Ecmd.args;
  EwxListCtrl *This;
  This = (EwxListCtrl *) memenv->getPtr(env, argv[0], "This");
  // wxListCtrlCompare *fn;
  // fn = (wxListCtrlCompare *) memenv->getPtr(env, argv[1]);
  callbackInfo cb = callbackInfo();
  cb.me_ref = memenv->me_ref;
  if(!enif_get_int(env, argv[1], &cb.callbackID)) Badarg("CallBack");
  // long data;
  // if(!enif_get_long(env, argv[2], &data)) Badarg("data");
  if(!This) throw wxe_badarg(0);
  bool Result = This->SortItems(wxEListCtrlCompare, (wxeIntPtr) &cb);
  wxeReturn rt = wxeReturn(memenv, Ecmd.caller, true);
  rt.send(  rt.make_bool(Result));
  wxeReturn rt2 = wxeReturn(memenv, memenv->owner, false);
  rt2.send( enif_make_tuple2(rt2.env,
			     rt2.make_atom("wx_delete_cb"),
			     rt2.make_int(cb.callbackID)));
}

// wxListEvent::GetCacheFrom
void wxListEvent_GetCacheFrom(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd)
{
  ErlNifEnv *env = Ecmd.env;
  ERL_NIF_TERM * argv = Ecmd.args;
  wxListEvent *This;
  This = (wxListEvent *) memenv->getPtr(env, argv[0], "This");
  if(!This) throw wxe_badarg("This");
  long Result = This->GetCacheFrom();
  wxeReturn rt = wxeReturn(memenv, Ecmd.caller, true);
  rt.send(  rt.make_int(Result));

}

// wxListEvent::GetCacheTo
void wxListEvent_GetCacheTo(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd)
{
  ErlNifEnv *env = Ecmd.env;
  ERL_NIF_TERM * argv = Ecmd.args;
  wxListEvent *This;
  This = (wxListEvent *) memenv->getPtr(env, argv[0], "This");
  if(!This) throw wxe_badarg("This");
  long Result = This->GetCacheTo();
  wxeReturn rt = wxeReturn(memenv, Ecmd.caller, true);
  rt.send(  rt.make_int(Result));

}

// wxListEvent::GetKeyCode
void wxListEvent_GetKeyCode(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd)
{
  ErlNifEnv *env = Ecmd.env;
  ERL_NIF_TERM * argv = Ecmd.args;
  wxListEvent *This;
  This = (wxListEvent *) memenv->getPtr(env, argv[0], "This");
  if(!This) throw wxe_badarg("This");
  int Result = This->GetKeyCode();
  wxeReturn rt = wxeReturn(memenv, Ecmd.caller, true);
  rt.send(  rt.make_int(Result));

}

// wxListEvent::GetIndex
void wxListEvent_GetIndex(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd)
{
  ErlNifEnv *env = Ecmd.env;
  ERL_NIF_TERM * argv = Ecmd.args;
  wxListEvent *This;
  This = (wxListEvent *) memenv->getPtr(env, argv[0], "This");
  if(!This) throw wxe_badarg("This");
  long Result = This->GetIndex();
  wxeReturn rt = wxeReturn(memenv, Ecmd.caller, true);
  rt.send(  rt.make_int(Result));

}

// wxListEvent::GetColumn
void wxListEvent_GetColumn(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd)
{
  ErlNifEnv *env = Ecmd.env;
  ERL_NIF_TERM * argv = Ecmd.args;
  wxListEvent *This;
  This = (wxListEvent *) memenv->getPtr(env, argv[0], "This");
  if(!This) throw wxe_badarg("This");
  int Result = This->GetColumn();
  wxeReturn rt = wxeReturn(memenv, Ecmd.caller, true);
  rt.send(  rt.make_int(Result));

}

// wxListEvent::GetPoint
void wxListEvent_GetPoint(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd)
{
  ErlNifEnv *env = Ecmd.env;
  ERL_NIF_TERM * argv = Ecmd.args;
  wxListEvent *This;
  This = (wxListEvent *) memenv->getPtr(env, argv[0], "This");
  if(!This) throw wxe_badarg("This");
  wxPoint Result = This->GetPoint();
  wxeReturn rt = wxeReturn(memenv, Ecmd.caller, true);
  rt.send(  rt.make(Result));

}

// wxListEvent::GetLabel
void wxListEvent_GetLabel(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd)
{
  ErlNifEnv *env = Ecmd.env;
  ERL_NIF_TERM * argv = Ecmd.args;
  wxListEvent *This;
  This = (wxListEvent *) memenv->getPtr(env, argv[0], "This");
  if(!This) throw wxe_badarg("This");
  const wxString Result = This->GetLabel();
  wxeReturn rt = wxeReturn(memenv, Ecmd.caller, true);
  rt.send(  rt.make(Result));

}

// wxListEvent::GetText
void wxListEvent_GetText(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd)
{
  ErlNifEnv *env = Ecmd.env;
  ERL_NIF_TERM * argv = Ecmd.args;
  wxListEvent *This;
  This = (wxListEvent *) memenv->getPtr(env, argv[0], "This");
  if(!This) throw wxe_badarg("This");
  const wxString Result = This->GetText();
  wxeReturn rt = wxeReturn(memenv, Ecmd.caller, true);
  rt.send(  rt.make(Result));

}

// wxListEvent::GetImage
void wxListEvent_GetImage(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd)
{
  ErlNifEnv *env = Ecmd.env;
  ERL_NIF_TERM * argv = Ecmd.args;
  wxListEvent *This;
  This = (wxListEvent *) memenv->getPtr(env, argv[0], "This");
  if(!This) throw wxe_badarg("This");
  int Result = This->GetImage();
  wxeReturn rt = wxeReturn(memenv, Ecmd.caller, true);
  rt.send(  rt.make_int(Result));

}

// wxListEvent::GetData
void wxListEvent_GetData(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd)
{
  ErlNifEnv *env = Ecmd.env;
  ERL_NIF_TERM * argv = Ecmd.args;
  wxListEvent *This;
  This = (wxListEvent *) memenv->getPtr(env, argv[0], "This");
  if(!This) throw wxe_badarg("This");
  wxUIntPtr Result = This->GetData();
  wxeReturn rt = wxeReturn(memenv, Ecmd.caller, true);
  rt.send(  rt.make_int(Result));

}

// wxListEvent::GetMask
void wxListEvent_GetMask(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd)
{
  ErlNifEnv *env = Ecmd.env;
  ERL_NIF_TERM * argv = Ecmd.args;
  wxListEvent *This;
  This = (wxListEvent *) memenv->getPtr(env, argv[0], "This");
  if(!This) throw wxe_badarg("This");
  long Result = This->GetMask();
  wxeReturn rt = wxeReturn(memenv, Ecmd.caller, true);
  rt.send(  rt.make_int(Result));

}

// wxListEvent::GetItem
void wxListEvent_GetItem(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd)
{
  ErlNifEnv *env = Ecmd.env;
  ERL_NIF_TERM * argv = Ecmd.args;
  wxListEvent *This;
  This = (wxListEvent *) memenv->getPtr(env, argv[0], "This");
  if(!This) throw wxe_badarg("This");
  const wxListItem * Result = &This->GetItem();
  wxeReturn rt = wxeReturn(memenv, Ecmd.caller, true);
  rt.send(  rt.make_ref(app->getRef((void *)Result,memenv), "wxListItem"));

}

// wxListEvent::IsEditCancelled
void wxListEvent_IsEditCancelled(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd)
{
  ErlNifEnv *env = Ecmd.env;
  ERL_NIF_TERM * argv = Ecmd.args;
  wxListEvent *This;
  This = (wxListEvent *) memenv->getPtr(env, argv[0], "This");
  if(!This) throw wxe_badarg("This");
  bool Result = This->IsEditCancelled();
  wxeReturn rt = wxeReturn(memenv, Ecmd.caller, true);
  rt.send(  rt.make_bool(Result));

}

// wxListItem::wxListItem
void wxListItem_new_0(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd)
{
  wxListItem * Result = new EwxListItem();
  app->newPtr((void *) Result, 1, memenv);
  wxeReturn rt = wxeReturn(memenv, Ecmd.caller, true);
  rt.send(  rt.make_ref(app->getRef((void *)Result,memenv), "wxListItem"));

}

// wxListItem::wxListItem
void wxListItem_new_1(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd)
{
  ErlNifEnv *env = Ecmd.env;
  ERL_NIF_TERM * argv = Ecmd.args;
  wxListItem *item;
  item = (wxListItem *) memenv->getPtr(env, argv[0], "item");
  wxListItem * Result = new EwxListItem(*item);
  app->newPtr((void *) Result, 1, memenv);
  wxeReturn rt = wxeReturn(memenv, Ecmd.caller, true);
  rt.send(  rt.make_ref(app->getRef((void *)Result,memenv), "wxListItem"));

}

// wxListItem::Clear
void wxListItem_Clear(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd)
{
  ErlNifEnv *env = Ecmd.env;
  ERL_NIF_TERM * argv = Ecmd.args;
  wxListItem *This;
  This = (wxListItem *) memenv->getPtr(env, argv[0], "This");
  if(!This) throw wxe_badarg("This");
  This->Clear();

}

// wxListItem::GetAlign
void wxListItem_GetAlign(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd)
{
  ErlNifEnv *env = Ecmd.env;
  ERL_NIF_TERM * argv = Ecmd.args;
  wxListItem *This;
  This = (wxListItem *) memenv->getPtr(env, argv[0], "This");
  if(!This) throw wxe_badarg("This");
  int Result = This->GetAlign();
  wxeReturn rt = wxeReturn(memenv, Ecmd.caller, true);
  rt.send(  rt.make_int(Result));

}

// wxListItem::GetBackgroundColour
void wxListItem_GetBackgroundColour(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd)
{
  ErlNifEnv *env = Ecmd.env;
  ERL_NIF_TERM * argv = Ecmd.args;
  wxListItem *This;
  This = (wxListItem *) memenv->getPtr(env, argv[0], "This");
  if(!This) throw wxe_badarg("This");
  wxColour Result = This->GetBackgroundColour();
  wxeReturn rt = wxeReturn(memenv, Ecmd.caller, true);
  rt.send(  rt.make(Result));

}

// wxListItem::GetColumn
void wxListItem_GetColumn(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd)
{
  ErlNifEnv *env = Ecmd.env;
  ERL_NIF_TERM * argv = Ecmd.args;
  wxListItem *This;
  This = (wxListItem *) memenv->getPtr(env, argv[0], "This");
  if(!This) throw wxe_badarg("This");
  int Result = This->GetColumn();
  wxeReturn rt = wxeReturn(memenv, Ecmd.caller, true);
  rt.send(  rt.make_int(Result));

}

// wxListItem::GetFont
void wxListItem_GetFont(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd)
{
  ErlNifEnv *env = Ecmd.env;
  ERL_NIF_TERM * argv = Ecmd.args;
  wxListItem *This;
  This = (wxListItem *) memenv->getPtr(env, argv[0], "This");
  if(!This) throw wxe_badarg("This");
  wxFont * Result = new wxFont(This->GetFont()); app->newPtr((void *) Result,3, memenv);;
  wxeReturn rt = wxeReturn(memenv, Ecmd.caller, true);
  rt.send(  rt.make_ref(app->getRef((void *)Result,memenv), "wxFont"));

}

// wxListItem::GetId
void wxListItem_GetId(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd)
{
  ErlNifEnv *env = Ecmd.env;
  ERL_NIF_TERM * argv = Ecmd.args;
  wxListItem *This;
  This = (wxListItem *) memenv->getPtr(env, argv[0], "This");
  if(!This) throw wxe_badarg("This");
  long Result = This->GetId();
  wxeReturn rt = wxeReturn(memenv, Ecmd.caller, true);
  rt.send(  rt.make_int(Result));

}

// wxListItem::GetImage
void wxListItem_GetImage(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd)
{
  ErlNifEnv *env = Ecmd.env;
  ERL_NIF_TERM * argv = Ecmd.args;
  wxListItem *This;
  This = (wxListItem *) memenv->getPtr(env, argv[0], "This");
  if(!This) throw wxe_badarg("This");
  int Result = This->GetImage();
  wxeReturn rt = wxeReturn(memenv, Ecmd.caller, true);
  rt.send(  rt.make_int(Result));

}

// wxListItem::GetMask
void wxListItem_GetMask(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd)
{
  ErlNifEnv *env = Ecmd.env;
  ERL_NIF_TERM * argv = Ecmd.args;
  wxListItem *This;
  This = (wxListItem *) memenv->getPtr(env, argv[0], "This");
  if(!This) throw wxe_badarg("This");
  long Result = This->GetMask();
  wxeReturn rt = wxeReturn(memenv, Ecmd.caller, true);
  rt.send(  rt.make_int(Result));

}

// wxListItem::GetState
void wxListItem_GetState(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd)
{
  ErlNifEnv *env = Ecmd.env;
  ERL_NIF_TERM * argv = Ecmd.args;
  wxListItem *This;
  This = (wxListItem *) memenv->getPtr(env, argv[0], "This");
  if(!This) throw wxe_badarg("This");
  long Result = This->GetState();
  wxeReturn rt = wxeReturn(memenv, Ecmd.caller, true);
  rt.send(  rt.make_int(Result));

}

// wxListItem::GetText
void wxListItem_GetText(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd)
{
  ErlNifEnv *env = Ecmd.env;
  ERL_NIF_TERM * argv = Ecmd.args;
  wxListItem *This;
  This = (wxListItem *) memenv->getPtr(env, argv[0], "This");
  if(!This) throw wxe_badarg("This");
  const wxString Result = This->GetText();
  wxeReturn rt = wxeReturn(memenv, Ecmd.caller, true);
  rt.send(  rt.make(Result));

}

// wxListItem::GetTextColour
void wxListItem_GetTextColour(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd)
{
  ErlNifEnv *env = Ecmd.env;
  ERL_NIF_TERM * argv = Ecmd.args;
  wxListItem *This;
  This = (wxListItem *) memenv->getPtr(env, argv[0], "This");
  if(!This) throw wxe_badarg("This");
  wxColour Result = This->GetTextColour();
  wxeReturn rt = wxeReturn(memenv, Ecmd.caller, true);
  rt.send(  rt.make(Result));

}

// wxListItem::GetWidth
void wxListItem_GetWidth(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd)
{
  ErlNifEnv *env = Ecmd.env;
  ERL_NIF_TERM * argv = Ecmd.args;
  wxListItem *This;
  This = (wxListItem *) memenv->getPtr(env, argv[0], "This");
  if(!This) throw wxe_badarg("This");
  int Result = This->GetWidth();
  wxeReturn rt = wxeReturn(memenv, Ecmd.caller, true);
  rt.send(  rt.make_int(Result));

}

// wxListItem::SetAlign
void wxListItem_SetAlign(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd)
{
  ErlNifEnv *env = Ecmd.env;
  ERL_NIF_TERM * argv = Ecmd.args;
  wxListItem *This;
  This = (wxListItem *) memenv->getPtr(env, argv[0], "This");
  wxListColumnFormat align;
  if(!enif_get_int(env, argv[1], (int *) &align)) Badarg("align"); // enum
  if(!This) throw wxe_badarg("This");
  This->SetAlign(align);

}

// wxListItem::SetBackgroundColour
void wxListItem_SetBackgroundColour(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd)
{
  ErlNifEnv *env = Ecmd.env;
  ERL_NIF_TERM * argv = Ecmd.args;
  wxListItem *This;
  This = (wxListItem *) memenv->getPtr(env, argv[0], "This");
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

// wxListItem::SetColumn
void wxListItem_SetColumn(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd)
{
  ErlNifEnv *env = Ecmd.env;
  ERL_NIF_TERM * argv = Ecmd.args;
  wxListItem *This;
  This = (wxListItem *) memenv->getPtr(env, argv[0], "This");
  int col;
  if(!enif_get_int(env, argv[1], &col)) Badarg("col"); // int
  if(!This) throw wxe_badarg("This");
  This->SetColumn(col);

}

// wxListItem::SetFont
void wxListItem_SetFont(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd)
{
  ErlNifEnv *env = Ecmd.env;
  ERL_NIF_TERM * argv = Ecmd.args;
  wxListItem *This;
  This = (wxListItem *) memenv->getPtr(env, argv[0], "This");
  wxFont *font;
  font = (wxFont *) memenv->getPtr(env, argv[1], "font");
  if(!This) throw wxe_badarg("This");
  This->SetFont(*font);

}

// wxListItem::SetId
void wxListItem_SetId(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd)
{
  ErlNifEnv *env = Ecmd.env;
  ERL_NIF_TERM * argv = Ecmd.args;
  wxListItem *This;
  This = (wxListItem *) memenv->getPtr(env, argv[0], "This");
  long id;
  if(!enif_get_long(env, argv[1], &id)) Badarg("id");
  if(!This) throw wxe_badarg("This");
  This->SetId(id);

}

// wxListItem::SetImage
void wxListItem_SetImage(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd)
{
  ErlNifEnv *env = Ecmd.env;
  ERL_NIF_TERM * argv = Ecmd.args;
  wxListItem *This;
  This = (wxListItem *) memenv->getPtr(env, argv[0], "This");
  int image;
  if(!enif_get_int(env, argv[1], &image)) Badarg("image"); // int
  if(!This) throw wxe_badarg("This");
  This->SetImage(image);

}

// wxListItem::SetMask
void wxListItem_SetMask(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd)
{
  ErlNifEnv *env = Ecmd.env;
  ERL_NIF_TERM * argv = Ecmd.args;
  wxListItem *This;
  This = (wxListItem *) memenv->getPtr(env, argv[0], "This");
  long mask;
  if(!enif_get_long(env, argv[1], &mask)) Badarg("mask");
  if(!This) throw wxe_badarg("This");
  This->SetMask(mask);

}

// wxListItem::SetState
void wxListItem_SetState(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd)
{
  ErlNifEnv *env = Ecmd.env;
  ERL_NIF_TERM * argv = Ecmd.args;
  wxListItem *This;
  This = (wxListItem *) memenv->getPtr(env, argv[0], "This");
  long state;
  if(!enif_get_long(env, argv[1], &state)) Badarg("state");
  if(!This) throw wxe_badarg("This");
  This->SetState(state);

}

// wxListItem::SetStateMask
void wxListItem_SetStateMask(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd)
{
  ErlNifEnv *env = Ecmd.env;
  ERL_NIF_TERM * argv = Ecmd.args;
  wxListItem *This;
  This = (wxListItem *) memenv->getPtr(env, argv[0], "This");
  long stateMask;
  if(!enif_get_long(env, argv[1], &stateMask)) Badarg("stateMask");
  if(!This) throw wxe_badarg("This");
  This->SetStateMask(stateMask);

}

// wxListItem::SetText
void wxListItem_SetText(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd)
{
  ErlNifEnv *env = Ecmd.env;
  ERL_NIF_TERM * argv = Ecmd.args;
  wxListItem *This;
  This = (wxListItem *) memenv->getPtr(env, argv[0], "This");
  ErlNifBinary text_bin;
  wxString text;
  if(!enif_inspect_binary(env, argv[1], &text_bin)) Badarg("text");
  text = wxString(text_bin.data, wxConvUTF8, text_bin.size);
  if(!This) throw wxe_badarg("This");
  This->SetText(text);

}

// wxListItem::SetTextColour
void wxListItem_SetTextColour(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd)
{
  ErlNifEnv *env = Ecmd.env;
  ERL_NIF_TERM * argv = Ecmd.args;
  wxListItem *This;
  This = (wxListItem *) memenv->getPtr(env, argv[0], "This");
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

// wxListItem::SetWidth
void wxListItem_SetWidth(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd)
{
  ErlNifEnv *env = Ecmd.env;
  ERL_NIF_TERM * argv = Ecmd.args;
  wxListItem *This;
  This = (wxListItem *) memenv->getPtr(env, argv[0], "This");
  int width;
  if(!enif_get_int(env, argv[1], &width)) Badarg("width"); // int
  if(!This) throw wxe_badarg("This");
  This->SetWidth(width);

}

// wxListItemAttr::wxListItemAttr
void wxListItemAttr_new_0(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd)
{
  wxListItemAttr * Result = new wxListItemAttr();
  app->newPtr((void *) Result, 102, memenv);
  wxeReturn rt = wxeReturn(memenv, Ecmd.caller, true);
  rt.send(  rt.make_ref(app->getRef((void *)Result,memenv), "wxListItemAttr"));

}

// wxListItemAttr::wxListItemAttr
void wxListItemAttr_new_3(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd)
{
  ErlNifEnv *env = Ecmd.env;
  ERL_NIF_TERM * argv = Ecmd.args;
  const ERL_NIF_TERM *colText_t;
  int colText_sz;
  if(!enif_get_tuple(env, argv[0], &colText_sz, &colText_t)) Badarg("colText");
  int colTextR;
  if(!enif_get_int(env, colText_t[0], &colTextR)) Badarg("colText");
  int colTextG;
  if(!enif_get_int(env, colText_t[1], &colTextG)) Badarg("colText");
  int colTextB;
  if(!enif_get_int(env, colText_t[2], &colTextB)) Badarg("colText");
  int colTextA;
  if(!enif_get_int(env, colText_t[3], &colTextA)) Badarg("colText");
  wxColour colText = wxColour(colTextR,colTextG,colTextB,colTextA);
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
  wxFont *font;
  font = (wxFont *) memenv->getPtr(env, argv[2], "font");
  wxListItemAttr * Result = new wxListItemAttr(colText,colBack,*font);
  app->newPtr((void *) Result, 102, memenv);
  wxeReturn rt = wxeReturn(memenv, Ecmd.caller, true);
  rt.send(  rt.make_ref(app->getRef((void *)Result,memenv), "wxListItemAttr"));

}

// wxListItemAttr::GetBackgroundColour
void wxListItemAttr_GetBackgroundColour(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd)
{
  ErlNifEnv *env = Ecmd.env;
  ERL_NIF_TERM * argv = Ecmd.args;
  wxListItemAttr *This;
  This = (wxListItemAttr *) memenv->getPtr(env, argv[0], "This");
  if(!This) throw wxe_badarg("This");
  const wxColour * Result = &This->GetBackgroundColour();
  wxeReturn rt = wxeReturn(memenv, Ecmd.caller, true);
  rt.send(  rt.make((*Result)));

}

// wxListItemAttr::GetFont
void wxListItemAttr_GetFont(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd)
{
  ErlNifEnv *env = Ecmd.env;
  ERL_NIF_TERM * argv = Ecmd.args;
  wxListItemAttr *This;
  This = (wxListItemAttr *) memenv->getPtr(env, argv[0], "This");
  if(!This) throw wxe_badarg("This");
  const wxFont * Result = &This->GetFont();
  wxeReturn rt = wxeReturn(memenv, Ecmd.caller, true);
  rt.send(  rt.make_ref(app->getRef((void *)Result,memenv), "wxFont"));

}

// wxListItemAttr::GetTextColour
void wxListItemAttr_GetTextColour(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd)
{
  ErlNifEnv *env = Ecmd.env;
  ERL_NIF_TERM * argv = Ecmd.args;
  wxListItemAttr *This;
  This = (wxListItemAttr *) memenv->getPtr(env, argv[0], "This");
  if(!This) throw wxe_badarg("This");
  const wxColour * Result = &This->GetTextColour();
  wxeReturn rt = wxeReturn(memenv, Ecmd.caller, true);
  rt.send(  rt.make((*Result)));

}

// wxListItemAttr::HasBackgroundColour
void wxListItemAttr_HasBackgroundColour(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd)
{
  ErlNifEnv *env = Ecmd.env;
  ERL_NIF_TERM * argv = Ecmd.args;
  wxListItemAttr *This;
  This = (wxListItemAttr *) memenv->getPtr(env, argv[0], "This");
  if(!This) throw wxe_badarg("This");
  bool Result = This->HasBackgroundColour();
  wxeReturn rt = wxeReturn(memenv, Ecmd.caller, true);
  rt.send(  rt.make_bool(Result));

}

// wxListItemAttr::HasFont
void wxListItemAttr_HasFont(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd)
{
  ErlNifEnv *env = Ecmd.env;
  ERL_NIF_TERM * argv = Ecmd.args;
  wxListItemAttr *This;
  This = (wxListItemAttr *) memenv->getPtr(env, argv[0], "This");
  if(!This) throw wxe_badarg("This");
  bool Result = This->HasFont();
  wxeReturn rt = wxeReturn(memenv, Ecmd.caller, true);
  rt.send(  rt.make_bool(Result));

}

// wxListItemAttr::HasTextColour
void wxListItemAttr_HasTextColour(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd)
{
  ErlNifEnv *env = Ecmd.env;
  ERL_NIF_TERM * argv = Ecmd.args;
  wxListItemAttr *This;
  This = (wxListItemAttr *) memenv->getPtr(env, argv[0], "This");
  if(!This) throw wxe_badarg("This");
  bool Result = This->HasTextColour();
  wxeReturn rt = wxeReturn(memenv, Ecmd.caller, true);
  rt.send(  rt.make_bool(Result));

}

// wxListItemAttr::SetBackgroundColour
void wxListItemAttr_SetBackgroundColour(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd)
{
  ErlNifEnv *env = Ecmd.env;
  ERL_NIF_TERM * argv = Ecmd.args;
  wxListItemAttr *This;
  This = (wxListItemAttr *) memenv->getPtr(env, argv[0], "This");
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

// wxListItemAttr::SetFont
void wxListItemAttr_SetFont(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd)
{
  ErlNifEnv *env = Ecmd.env;
  ERL_NIF_TERM * argv = Ecmd.args;
  wxListItemAttr *This;
  This = (wxListItemAttr *) memenv->getPtr(env, argv[0], "This");
  wxFont *font;
  font = (wxFont *) memenv->getPtr(env, argv[1], "font");
  if(!This) throw wxe_badarg("This");
  This->SetFont(*font);

}

// wxListItemAttr::SetTextColour
void wxListItemAttr_SetTextColour(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd)
{
  ErlNifEnv *env = Ecmd.env;
  ERL_NIF_TERM * argv = Ecmd.args;
  wxListItemAttr *This;
  This = (wxListItemAttr *) memenv->getPtr(env, argv[0], "This");
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

// wxListItemAttr::destroy
void wxListItemAttr_destroy(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd)
{
 ErlNifEnv *env = Ecmd.env;
 ERL_NIF_TERM * argv = Ecmd.args;
  wxListItemAttr *This;
  This = (wxListItemAttr *) memenv->getPtr(env, argv[0], "This");
 if(This) {   ((WxeApp *) wxTheApp)->clearPtr((void *) This);
   delete This;}
}

// wxListView::ClearColumnImage
void wxListView_ClearColumnImage(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd)
{
  ErlNifEnv *env = Ecmd.env;
  ERL_NIF_TERM * argv = Ecmd.args;
  wxListView *This;
  This = (wxListView *) memenv->getPtr(env, argv[0], "This");
  int col;
  if(!enif_get_int(env, argv[1], &col)) Badarg("col"); // int
  if(!This) throw wxe_badarg("This");
  This->ClearColumnImage(col);

}

// wxListView::Focus
void wxListView_Focus(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd)
{
  ErlNifEnv *env = Ecmd.env;
  ERL_NIF_TERM * argv = Ecmd.args;
  wxListView *This;
  This = (wxListView *) memenv->getPtr(env, argv[0], "This");
  long index;
  if(!enif_get_long(env, argv[1], &index)) Badarg("index");
  if(!This) throw wxe_badarg("This");
  This->Focus(index);

}

// wxListView::GetFirstSelected
void wxListView_GetFirstSelected(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd)
{
  ErlNifEnv *env = Ecmd.env;
  ERL_NIF_TERM * argv = Ecmd.args;
  wxListView *This;
  This = (wxListView *) memenv->getPtr(env, argv[0], "This");
  if(!This) throw wxe_badarg("This");
  long Result = This->GetFirstSelected();
  wxeReturn rt = wxeReturn(memenv, Ecmd.caller, true);
  rt.send(  rt.make_int(Result));

}

// wxListView::GetFocusedItem
void wxListView_GetFocusedItem(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd)
{
  ErlNifEnv *env = Ecmd.env;
  ERL_NIF_TERM * argv = Ecmd.args;
  wxListView *This;
  This = (wxListView *) memenv->getPtr(env, argv[0], "This");
  if(!This) throw wxe_badarg("This");
  long Result = This->GetFocusedItem();
  wxeReturn rt = wxeReturn(memenv, Ecmd.caller, true);
  rt.send(  rt.make_int(Result));

}

// wxListView::GetNextSelected
void wxListView_GetNextSelected(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd)
{
  ErlNifEnv *env = Ecmd.env;
  ERL_NIF_TERM * argv = Ecmd.args;
  wxListView *This;
  This = (wxListView *) memenv->getPtr(env, argv[0], "This");
  long item;
  if(!enif_get_long(env, argv[1], &item)) Badarg("item");
  if(!This) throw wxe_badarg("This");
  long Result = This->GetNextSelected(item);
  wxeReturn rt = wxeReturn(memenv, Ecmd.caller, true);
  rt.send(  rt.make_int(Result));

}

// wxListView::IsSelected
void wxListView_IsSelected(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd)
{
  ErlNifEnv *env = Ecmd.env;
  ERL_NIF_TERM * argv = Ecmd.args;
  wxListView *This;
  This = (wxListView *) memenv->getPtr(env, argv[0], "This");
  long index;
  if(!enif_get_long(env, argv[1], &index)) Badarg("index");
  if(!This) throw wxe_badarg("This");
  bool Result = This->IsSelected(index);
  wxeReturn rt = wxeReturn(memenv, Ecmd.caller, true);
  rt.send(  rt.make_bool(Result));

}

// wxListView::Select
void wxListView_Select(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd)
{
  bool on=true;
  ErlNifEnv *env = Ecmd.env;
  ERL_NIF_TERM * argv = Ecmd.args;
  wxListView *This;
  This = (wxListView *) memenv->getPtr(env, argv[0], "This");
  long n;
  if(!enif_get_long(env, argv[1], &n)) Badarg("n");
  ERL_NIF_TERM lstHead, lstTail;
  lstTail = argv[2];
  if(!enif_is_list(env, lstTail)) Badarg("Options");
  const ERL_NIF_TERM *tpl;
  int tpl_sz;
  while(!enif_is_empty_list(env, lstTail)) {
    if(!enif_get_list_cell(env, lstTail, &lstHead, &lstTail)) Badarg("Options");
    if(!enif_get_tuple(env, lstHead, &tpl_sz, &tpl) || tpl_sz != 2) Badarg("Options");
    if(enif_is_identical(tpl[0], enif_make_atom(env, "on"))) {
  on = enif_is_identical(tpl[1], WXE_ATOM_true);
    } else        Badarg("Options");
  };
  if(!This) throw wxe_badarg("This");
  This->Select(n,on);

}

// wxListView::SetColumnImage
void wxListView_SetColumnImage(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd)
{
  ErlNifEnv *env = Ecmd.env;
  ERL_NIF_TERM * argv = Ecmd.args;
  wxListView *This;
  This = (wxListView *) memenv->getPtr(env, argv[0], "This");
  int col;
  if(!enif_get_int(env, argv[1], &col)) Badarg("col"); // int
  int image;
  if(!enif_get_int(env, argv[2], &image)) Badarg("image"); // int
  if(!This) throw wxe_badarg("This");
  This->SetColumnImage(col,image);

}

// wxListbook::wxListbook
void wxListbook_new_0(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd)
{
  wxListbook * Result = new EwxListbook();
  app->newPtr((void *) Result, 0, memenv);
  wxeReturn rt = wxeReturn(memenv, Ecmd.caller, true);
  rt.send(  rt.make_ref(app->getRef((void *)Result,memenv), "wxListbook"));

}

// wxListbook::wxListbook
void wxListbook_new_3(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd)
{
  wxPoint pos= wxDefaultPosition;
  wxSize size= wxDefaultSize;
  long style=0;
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
  wxListbook * Result = new EwxListbook(parent,id,pos,size,style);
  app->newPtr((void *) Result, 0, memenv);
  wxeReturn rt = wxeReturn(memenv, Ecmd.caller, true);
  rt.send(  rt.make_ref(app->getRef((void *)Result,memenv), "wxListbook"));

}

// wxListbook::AddPage
void wxListbook_AddPage(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd)
{
  bool bSelect=false;
  int imageId=wxBookCtrlBase::NO_IMAGE;
  ErlNifEnv *env = Ecmd.env;
  ERL_NIF_TERM * argv = Ecmd.args;
  wxListbook *This;
  This = (wxListbook *) memenv->getPtr(env, argv[0], "This");
  wxWindow *page;
  page = (wxWindow *) memenv->getPtr(env, argv[1], "page");
  ErlNifBinary text_bin;
  wxString text;
  if(!enif_inspect_binary(env, argv[2], &text_bin)) Badarg("text");
  text = wxString(text_bin.data, wxConvUTF8, text_bin.size);
  ERL_NIF_TERM lstHead, lstTail;
  lstTail = argv[3];
  if(!enif_is_list(env, lstTail)) Badarg("Options");
  const ERL_NIF_TERM *tpl;
  int tpl_sz;
  while(!enif_is_empty_list(env, lstTail)) {
    if(!enif_get_list_cell(env, lstTail, &lstHead, &lstTail)) Badarg("Options");
    if(!enif_get_tuple(env, lstHead, &tpl_sz, &tpl) || tpl_sz != 2) Badarg("Options");
    if(enif_is_identical(tpl[0], enif_make_atom(env, "bSelect"))) {
  bSelect = enif_is_identical(tpl[1], WXE_ATOM_true);
    } else     if(enif_is_identical(tpl[0], enif_make_atom(env, "imageId"))) {
  if(!enif_get_int(env, tpl[1], &imageId)) Badarg("imageId"); // int
    } else        Badarg("Options");
  };
  if(!This) throw wxe_badarg("This");
  bool Result = This->AddPage(page,text,bSelect,imageId);
  wxeReturn rt = wxeReturn(memenv, Ecmd.caller, true);
  rt.send(  rt.make_bool(Result));

}

// wxListbook::AdvanceSelection
void wxListbook_AdvanceSelection(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd)
{
  bool forward=true;
  ErlNifEnv *env = Ecmd.env;
  ERL_NIF_TERM * argv = Ecmd.args;
  wxListbook *This;
  This = (wxListbook *) memenv->getPtr(env, argv[0], "This");
  ERL_NIF_TERM lstHead, lstTail;
  lstTail = argv[1];
  if(!enif_is_list(env, lstTail)) Badarg("Options");
  const ERL_NIF_TERM *tpl;
  int tpl_sz;
  while(!enif_is_empty_list(env, lstTail)) {
    if(!enif_get_list_cell(env, lstTail, &lstHead, &lstTail)) Badarg("Options");
    if(!enif_get_tuple(env, lstHead, &tpl_sz, &tpl) || tpl_sz != 2) Badarg("Options");
    if(enif_is_identical(tpl[0], enif_make_atom(env, "forward"))) {
  forward = enif_is_identical(tpl[1], WXE_ATOM_true);
    } else        Badarg("Options");
  };
  if(!This) throw wxe_badarg("This");
  This->AdvanceSelection(forward);

}

// wxListbook::AssignImageList
void wxListbook_AssignImageList(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd)
{
  ErlNifEnv *env = Ecmd.env;
  ERL_NIF_TERM * argv = Ecmd.args;
  wxListbook *This;
  This = (wxListbook *) memenv->getPtr(env, argv[0], "This");
  wxImageList *imageList;
  imageList = (wxImageList *) memenv->getPtr(env, argv[1], "imageList");
  if(!This) throw wxe_badarg("This");
  This->AssignImageList(imageList);

}

// wxListbook::Create
void wxListbook_Create(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd)
{
  wxPoint pos= wxDefaultPosition;
  wxSize size= wxDefaultSize;
  long style=0;
  ErlNifEnv *env = Ecmd.env;
  ERL_NIF_TERM * argv = Ecmd.args;
  wxListbook *This;
  This = (wxListbook *) memenv->getPtr(env, argv[0], "This");
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
  bool Result = This->Create(parent,id,pos,size,style);
  wxeReturn rt = wxeReturn(memenv, Ecmd.caller, true);
  rt.send(  rt.make_bool(Result));

}

// wxListbook::DeleteAllPages
void wxListbook_DeleteAllPages(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd)
{
  ErlNifEnv *env = Ecmd.env;
  ERL_NIF_TERM * argv = Ecmd.args;
  wxListbook *This;
  This = (wxListbook *) memenv->getPtr(env, argv[0], "This");
  if(!This) throw wxe_badarg("This");
  bool Result = This->DeleteAllPages();
  wxeReturn rt = wxeReturn(memenv, Ecmd.caller, true);
  rt.send(  rt.make_bool(Result));

}

// wxListbook::GetCurrentPage
void wxListbook_GetCurrentPage(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd)
{
  ErlNifEnv *env = Ecmd.env;
  ERL_NIF_TERM * argv = Ecmd.args;
  wxListbook *This;
  This = (wxListbook *) memenv->getPtr(env, argv[0], "This");
  if(!This) throw wxe_badarg("This");
  wxWindow * Result = (wxWindow*)This->GetCurrentPage();
  wxeReturn rt = wxeReturn(memenv, Ecmd.caller, true);
  rt.send(  rt.make_ref(app->getRef((void *)Result,memenv), "wxWindow"));

}

// wxListbook::GetImageList
void wxListbook_GetImageList(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd)
{
  ErlNifEnv *env = Ecmd.env;
  ERL_NIF_TERM * argv = Ecmd.args;
  wxListbook *This;
  This = (wxListbook *) memenv->getPtr(env, argv[0], "This");
  if(!This) throw wxe_badarg("This");
  wxImageList * Result = (wxImageList*)This->GetImageList();
  wxeReturn rt = wxeReturn(memenv, Ecmd.caller, true);
  rt.send(  rt.make_ref(app->getRef((void *)Result,memenv), "wxImageList"));

}

// wxListbook::GetPage
void wxListbook_GetPage(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd)
{
  ErlNifEnv *env = Ecmd.env;
  ERL_NIF_TERM * argv = Ecmd.args;
  wxListbook *This;
  This = (wxListbook *) memenv->getPtr(env, argv[0], "This");
  size_t page;
  if(!wxe_get_size_t(env, argv[1], &page)) Badarg("page");
  if(!This) throw wxe_badarg("This");
  wxWindow * Result = (wxWindow*)This->GetPage(page);
  wxeReturn rt = wxeReturn(memenv, Ecmd.caller, true);
  rt.send(  rt.make_ref(app->getRef((void *)Result,memenv), "wxWindow"));

}

// wxListbook::GetPageCount
void wxListbook_GetPageCount(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd)
{
  ErlNifEnv *env = Ecmd.env;
  ERL_NIF_TERM * argv = Ecmd.args;
  wxListbook *This;
  This = (wxListbook *) memenv->getPtr(env, argv[0], "This");
  if(!This) throw wxe_badarg("This");
  size_t Result = This->GetPageCount();
  wxeReturn rt = wxeReturn(memenv, Ecmd.caller, true);
  rt.send(  rt.make_int(Result));

}

// wxListbook::GetPageImage
void wxListbook_GetPageImage(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd)
{
  ErlNifEnv *env = Ecmd.env;
  ERL_NIF_TERM * argv = Ecmd.args;
  wxListbook *This;
  This = (wxListbook *) memenv->getPtr(env, argv[0], "This");
  size_t nPage;
  if(!wxe_get_size_t(env, argv[1], &nPage)) Badarg("nPage");
  if(!This) throw wxe_badarg("This");
  int Result = This->GetPageImage(nPage);
  wxeReturn rt = wxeReturn(memenv, Ecmd.caller, true);
  rt.send(  rt.make_int(Result));

}

// wxListbook::GetPageText
void wxListbook_GetPageText(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd)
{
  ErlNifEnv *env = Ecmd.env;
  ERL_NIF_TERM * argv = Ecmd.args;
  wxListbook *This;
  This = (wxListbook *) memenv->getPtr(env, argv[0], "This");
  size_t nPage;
  if(!wxe_get_size_t(env, argv[1], &nPage)) Badarg("nPage");
  if(!This) throw wxe_badarg("This");
  wxString Result = This->GetPageText(nPage);
  wxeReturn rt = wxeReturn(memenv, Ecmd.caller, true);
  rt.send(  rt.make(Result));

}

// wxListbook::GetSelection
void wxListbook_GetSelection(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd)
{
  ErlNifEnv *env = Ecmd.env;
  ERL_NIF_TERM * argv = Ecmd.args;
  wxListbook *This;
  This = (wxListbook *) memenv->getPtr(env, argv[0], "This");
  if(!This) throw wxe_badarg("This");
  int Result = This->GetSelection();
  wxeReturn rt = wxeReturn(memenv, Ecmd.caller, true);
  rt.send(  rt.make_int(Result));

}

// wxListbook::HitTest
void wxListbook_HitTest(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd)
{
  long flags;
  ErlNifEnv *env = Ecmd.env;
  ERL_NIF_TERM * argv = Ecmd.args;
  wxListbook *This;
  This = (wxListbook *) memenv->getPtr(env, argv[0], "This");
  const ERL_NIF_TERM *pt_t;
  int pt_sz;
  if(!enif_get_tuple(env, argv[1], &pt_sz, &pt_t)) Badarg("pt");
  int ptX;
  if(!enif_get_int(env, pt_t[0], &ptX)) Badarg("pt");
  int ptY;
  if(!enif_get_int(env, pt_t[1], &ptY)) Badarg("pt");
  wxPoint pt = wxPoint(ptX,ptY);
  if(!This) throw wxe_badarg("This");
  int Result = This->HitTest(pt,&flags);
  wxeReturn rt = wxeReturn(memenv, Ecmd.caller, true);
  ERL_NIF_TERM msg = enif_make_tuple2(rt.env,
  rt.make_int(Result),
    rt.make_int(flags));
  rt.send(msg);

}

// wxListbook::InsertPage
void wxListbook_InsertPage(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd)
{
  bool bSelect=false;
  int imageId=wxBookCtrlBase::NO_IMAGE;
  ErlNifEnv *env = Ecmd.env;
  ERL_NIF_TERM * argv = Ecmd.args;
  wxListbook *This;
  This = (wxListbook *) memenv->getPtr(env, argv[0], "This");
  size_t index;
  if(!wxe_get_size_t(env, argv[1], &index)) Badarg("index");
  wxWindow *page;
  page = (wxWindow *) memenv->getPtr(env, argv[2], "page");
  ErlNifBinary text_bin;
  wxString text;
  if(!enif_inspect_binary(env, argv[3], &text_bin)) Badarg("text");
  text = wxString(text_bin.data, wxConvUTF8, text_bin.size);
  ERL_NIF_TERM lstHead, lstTail;
  lstTail = argv[4];
  if(!enif_is_list(env, lstTail)) Badarg("Options");
  const ERL_NIF_TERM *tpl;
  int tpl_sz;
  while(!enif_is_empty_list(env, lstTail)) {
    if(!enif_get_list_cell(env, lstTail, &lstHead, &lstTail)) Badarg("Options");
    if(!enif_get_tuple(env, lstHead, &tpl_sz, &tpl) || tpl_sz != 2) Badarg("Options");
    if(enif_is_identical(tpl[0], enif_make_atom(env, "bSelect"))) {
  bSelect = enif_is_identical(tpl[1], WXE_ATOM_true);
    } else     if(enif_is_identical(tpl[0], enif_make_atom(env, "imageId"))) {
  if(!enif_get_int(env, tpl[1], &imageId)) Badarg("imageId"); // int
    } else        Badarg("Options");
  };
  if(!This) throw wxe_badarg("This");
  bool Result = This->InsertPage(index,page,text,bSelect,imageId);
  wxeReturn rt = wxeReturn(memenv, Ecmd.caller, true);
  rt.send(  rt.make_bool(Result));

}

// wxListbook::SetImageList
void wxListbook_SetImageList(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd)
{
  ErlNifEnv *env = Ecmd.env;
  ERL_NIF_TERM * argv = Ecmd.args;
  wxListbook *This;
  This = (wxListbook *) memenv->getPtr(env, argv[0], "This");
  wxImageList *imageList;
  imageList = (wxImageList *) memenv->getPtr(env, argv[1], "imageList");
  if(!This) throw wxe_badarg("This");
  This->SetImageList(imageList);

}

// wxListbook::SetPageSize
void wxListbook_SetPageSize(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd)
{
  ErlNifEnv *env = Ecmd.env;
  ERL_NIF_TERM * argv = Ecmd.args;
  wxListbook *This;
  This = (wxListbook *) memenv->getPtr(env, argv[0], "This");
  const ERL_NIF_TERM *size_t;
  int size_sz;
  if(!enif_get_tuple(env, argv[1], &size_sz, &size_t)) Badarg("size");
  int sizeW;
  if(!enif_get_int(env, size_t[0], &sizeW)) Badarg("size");
  int sizeH;
  if(!enif_get_int(env, size_t[1], &sizeH)) Badarg("size");
  wxSize size = wxSize(sizeW,sizeH);
  if(!This) throw wxe_badarg("This");
  This->SetPageSize(size);

}

// wxListbook::SetPageImage
void wxListbook_SetPageImage(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd)
{
  ErlNifEnv *env = Ecmd.env;
  ERL_NIF_TERM * argv = Ecmd.args;
  wxListbook *This;
  This = (wxListbook *) memenv->getPtr(env, argv[0], "This");
  size_t page;
  if(!wxe_get_size_t(env, argv[1], &page)) Badarg("page");
  int image;
  if(!enif_get_int(env, argv[2], &image)) Badarg("image"); // int
  if(!This) throw wxe_badarg("This");
  bool Result = This->SetPageImage(page,image);
  wxeReturn rt = wxeReturn(memenv, Ecmd.caller, true);
  rt.send(  rt.make_bool(Result));

}

// wxListbook::SetPageText
void wxListbook_SetPageText(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd)
{
  ErlNifEnv *env = Ecmd.env;
  ERL_NIF_TERM * argv = Ecmd.args;
  wxListbook *This;
  This = (wxListbook *) memenv->getPtr(env, argv[0], "This");
  size_t page;
  if(!wxe_get_size_t(env, argv[1], &page)) Badarg("page");
  ErlNifBinary text_bin;
  wxString text;
  if(!enif_inspect_binary(env, argv[2], &text_bin)) Badarg("text");
  text = wxString(text_bin.data, wxConvUTF8, text_bin.size);
  if(!This) throw wxe_badarg("This");
  bool Result = This->SetPageText(page,text);
  wxeReturn rt = wxeReturn(memenv, Ecmd.caller, true);
  rt.send(  rt.make_bool(Result));

}

// wxListbook::SetSelection
void wxListbook_SetSelection(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd)
{
  ErlNifEnv *env = Ecmd.env;
  ERL_NIF_TERM * argv = Ecmd.args;
  wxListbook *This;
  This = (wxListbook *) memenv->getPtr(env, argv[0], "This");
  size_t page;
  if(!wxe_get_size_t(env, argv[1], &page)) Badarg("page");
  if(!This) throw wxe_badarg("This");
  int Result = This->SetSelection(page);
  wxeReturn rt = wxeReturn(memenv, Ecmd.caller, true);
  rt.send(  rt.make_int(Result));

}

// wxListbook::ChangeSelection
void wxListbook_ChangeSelection(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd)
{
  ErlNifEnv *env = Ecmd.env;
  ERL_NIF_TERM * argv = Ecmd.args;
  wxListbook *This;
  This = (wxListbook *) memenv->getPtr(env, argv[0], "This");
  size_t page;
  if(!wxe_get_size_t(env, argv[1], &page)) Badarg("page");
  if(!This) throw wxe_badarg("This");
  int Result = This->ChangeSelection(page);
  wxeReturn rt = wxeReturn(memenv, Ecmd.caller, true);
  rt.send(  rt.make_int(Result));

}

// wxLocale::wxLocale
void wxLocale_new_0(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd)
{
  wxLocale * Result = new EwxLocale();
  app->newPtr((void *) Result, 234, memenv);
  wxeReturn rt = wxeReturn(memenv, Ecmd.caller, true);
  rt.send(  rt.make_ref(app->getRef((void *)Result,memenv), "wxLocale"));

}

// wxLocale::wxLocale
void wxLocale_new_2_0(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd)
{
  int flags=wxLOCALE_LOAD_DEFAULT;
  ErlNifEnv *env = Ecmd.env;
  ERL_NIF_TERM * argv = Ecmd.args;
  int language;
  if(!enif_get_int(env, argv[0], &language)) Badarg("language"); // int
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
  wxLocale * Result = new EwxLocale(language,flags);
  app->newPtr((void *) Result, 234, memenv);
  wxeReturn rt = wxeReturn(memenv, Ecmd.caller, true);
  rt.send(  rt.make_ref(app->getRef((void *)Result,memenv), "wxLocale"));

}

// wxLocale::wxLocale
void wxLocale_new_2_1(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd)
{
  wxString shortName= wxEmptyString;
  wxString locale= wxEmptyString;
  bool bLoadDefault=true;
  ErlNifEnv *env = Ecmd.env;
  ERL_NIF_TERM * argv = Ecmd.args;
  ErlNifBinary name_bin;
  wxString name;
  if(!enif_inspect_binary(env, argv[0], &name_bin)) Badarg("name");
  name = wxString(name_bin.data, wxConvUTF8, name_bin.size);
  ERL_NIF_TERM lstHead, lstTail;
  lstTail = argv[1];
  if(!enif_is_list(env, lstTail)) Badarg("Options");
  const ERL_NIF_TERM *tpl;
  int tpl_sz;
  while(!enif_is_empty_list(env, lstTail)) {
    if(!enif_get_list_cell(env, lstTail, &lstHead, &lstTail)) Badarg("Options");
    if(!enif_get_tuple(env, lstHead, &tpl_sz, &tpl) || tpl_sz != 2) Badarg("Options");
    if(enif_is_identical(tpl[0], enif_make_atom(env, "shortName"))) {
  ErlNifBinary shortName_bin;
  if(!enif_inspect_binary(env, tpl[1], &shortName_bin)) Badarg("shortName");
  shortName = wxString(shortName_bin.data, wxConvUTF8, shortName_bin.size);
    } else     if(enif_is_identical(tpl[0], enif_make_atom(env, "locale"))) {
  ErlNifBinary locale_bin;
  if(!enif_inspect_binary(env, tpl[1], &locale_bin)) Badarg("locale");
  locale = wxString(locale_bin.data, wxConvUTF8, locale_bin.size);
    } else     if(enif_is_identical(tpl[0], enif_make_atom(env, "bLoadDefault"))) {
  bLoadDefault = enif_is_identical(tpl[1], WXE_ATOM_true);
    } else        Badarg("Options");
  };
  wxLocale * Result = new EwxLocale(name,shortName,locale,bLoadDefault);
  app->newPtr((void *) Result, 234, memenv);
  wxeReturn rt = wxeReturn(memenv, Ecmd.caller, true);
  rt.send(  rt.make_ref(app->getRef((void *)Result,memenv), "wxLocale"));

}

// wxLocale::~wxLocale
void wxLocale_destruct(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd)
{
 ErlNifEnv *env = Ecmd.env;
 ERL_NIF_TERM * argv = Ecmd.args;
  wxLocale *This;
  This = (wxLocale *) memenv->getPtr(env, argv[0], "This");
 if(This) {   ((WxeApp *) wxTheApp)->clearPtr((void *) This);
   delete This;}
}

// wxLocale::Init
void wxLocale_Init_1(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd)
{
  int language=wxLANGUAGE_DEFAULT;
  int flags=wxLOCALE_LOAD_DEFAULT;
  ErlNifEnv *env = Ecmd.env;
  ERL_NIF_TERM * argv = Ecmd.args;
  wxLocale *This;
  This = (wxLocale *) memenv->getPtr(env, argv[0], "This");
  ERL_NIF_TERM lstHead, lstTail;
  lstTail = argv[1];
  if(!enif_is_list(env, lstTail)) Badarg("Options");
  const ERL_NIF_TERM *tpl;
  int tpl_sz;
  while(!enif_is_empty_list(env, lstTail)) {
    if(!enif_get_list_cell(env, lstTail, &lstHead, &lstTail)) Badarg("Options");
    if(!enif_get_tuple(env, lstHead, &tpl_sz, &tpl) || tpl_sz != 2) Badarg("Options");
    if(enif_is_identical(tpl[0], enif_make_atom(env, "language"))) {
  if(!enif_get_int(env, tpl[1], &language)) Badarg("language"); // int
    } else     if(enif_is_identical(tpl[0], enif_make_atom(env, "flags"))) {
  if(!enif_get_int(env, tpl[1], &flags)) Badarg("flags"); // int
    } else        Badarg("Options");
  };
  if(!This) throw wxe_badarg("This");
  bool Result = This->Init(language,flags);
  wxeReturn rt = wxeReturn(memenv, Ecmd.caller, true);
  rt.send(  rt.make_bool(Result));

}

// wxLocale::Init
void wxLocale_Init_2(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd)
{
  wxString shortName= wxEmptyString;
  wxString locale= wxEmptyString;
  bool bLoadDefault=true;
  ErlNifEnv *env = Ecmd.env;
  ERL_NIF_TERM * argv = Ecmd.args;
  wxLocale *This;
  This = (wxLocale *) memenv->getPtr(env, argv[0], "This");
  ErlNifBinary name_bin;
  wxString name;
  if(!enif_inspect_binary(env, argv[1], &name_bin)) Badarg("name");
  name = wxString(name_bin.data, wxConvUTF8, name_bin.size);
  ERL_NIF_TERM lstHead, lstTail;
  lstTail = argv[2];
  if(!enif_is_list(env, lstTail)) Badarg("Options");
  const ERL_NIF_TERM *tpl;
  int tpl_sz;
  while(!enif_is_empty_list(env, lstTail)) {
    if(!enif_get_list_cell(env, lstTail, &lstHead, &lstTail)) Badarg("Options");
    if(!enif_get_tuple(env, lstHead, &tpl_sz, &tpl) || tpl_sz != 2) Badarg("Options");
    if(enif_is_identical(tpl[0], enif_make_atom(env, "shortName"))) {
  ErlNifBinary shortName_bin;
  if(!enif_inspect_binary(env, tpl[1], &shortName_bin)) Badarg("shortName");
  shortName = wxString(shortName_bin.data, wxConvUTF8, shortName_bin.size);
    } else     if(enif_is_identical(tpl[0], enif_make_atom(env, "locale"))) {
  ErlNifBinary locale_bin;
  if(!enif_inspect_binary(env, tpl[1], &locale_bin)) Badarg("locale");
  locale = wxString(locale_bin.data, wxConvUTF8, locale_bin.size);
    } else     if(enif_is_identical(tpl[0], enif_make_atom(env, "bLoadDefault"))) {
  bLoadDefault = enif_is_identical(tpl[1], WXE_ATOM_true);
    } else        Badarg("Options");
  };
  if(!This) throw wxe_badarg("This");
  bool Result = This->Init(name,shortName,locale,bLoadDefault);
  wxeReturn rt = wxeReturn(memenv, Ecmd.caller, true);
  rt.send(  rt.make_bool(Result));

}

// wxLocale::AddCatalog
void wxLocale_AddCatalog_1(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd)
{
  ErlNifEnv *env = Ecmd.env;
  ERL_NIF_TERM * argv = Ecmd.args;
  wxLocale *This;
  This = (wxLocale *) memenv->getPtr(env, argv[0], "This");
  ErlNifBinary domain_bin;
  wxString domain;
  if(!enif_inspect_binary(env, argv[1], &domain_bin)) Badarg("domain");
  domain = wxString(domain_bin.data, wxConvUTF8, domain_bin.size);
  if(!This) throw wxe_badarg("This");
  bool Result = This->AddCatalog(domain);
  wxeReturn rt = wxeReturn(memenv, Ecmd.caller, true);
  rt.send(  rt.make_bool(Result));

}

// wxLocale::AddCatalog
void wxLocale_AddCatalog_2(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd)
{
  ErlNifEnv *env = Ecmd.env;
  ERL_NIF_TERM * argv = Ecmd.args;
  wxLocale *This;
  This = (wxLocale *) memenv->getPtr(env, argv[0], "This");
  ErlNifBinary domain_bin;
  wxString domain;
  if(!enif_inspect_binary(env, argv[1], &domain_bin)) Badarg("domain");
  domain = wxString(domain_bin.data, wxConvUTF8, domain_bin.size);
  wxLanguage msgIdLanguage;
  if(!enif_get_int(env, argv[2], (int *) &msgIdLanguage)) Badarg("msgIdLanguage"); // enum
  if(!This) throw wxe_badarg("This");
  bool Result = This->AddCatalog(domain,msgIdLanguage);
  wxeReturn rt = wxeReturn(memenv, Ecmd.caller, true);
  rt.send(  rt.make_bool(Result));

}

// wxLocale::AddCatalog
void wxLocale_AddCatalog_3(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd)
{
  ErlNifEnv *env = Ecmd.env;
  ERL_NIF_TERM * argv = Ecmd.args;
  wxLocale *This;
  This = (wxLocale *) memenv->getPtr(env, argv[0], "This");
  ErlNifBinary domain_bin;
  wxString domain;
  if(!enif_inspect_binary(env, argv[1], &domain_bin)) Badarg("domain");
  domain = wxString(domain_bin.data, wxConvUTF8, domain_bin.size);
  wxLanguage msgIdLanguage;
  if(!enif_get_int(env, argv[2], (int *) &msgIdLanguage)) Badarg("msgIdLanguage"); // enum
  ErlNifBinary msgIdCharset_bin;
  wxString msgIdCharset;
  if(!enif_inspect_binary(env, argv[3], &msgIdCharset_bin)) Badarg("msgIdCharset");
  msgIdCharset = wxString(msgIdCharset_bin.data, wxConvUTF8, msgIdCharset_bin.size);
  if(!This) throw wxe_badarg("This");
  bool Result = This->AddCatalog(domain,msgIdLanguage,msgIdCharset);
  wxeReturn rt = wxeReturn(memenv, Ecmd.caller, true);
  rt.send(  rt.make_bool(Result));

}

// wxLocale::AddCatalogLookupPathPrefix
void wxLocale_AddCatalogLookupPathPrefix(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd)
{
  ErlNifEnv *env = Ecmd.env;
  ERL_NIF_TERM * argv = Ecmd.args;
  ErlNifBinary prefix_bin;
  wxString prefix;
  if(!enif_inspect_binary(env, argv[0], &prefix_bin)) Badarg("prefix");
  prefix = wxString(prefix_bin.data, wxConvUTF8, prefix_bin.size);
  wxLocale::AddCatalogLookupPathPrefix(prefix);

}

// wxLocale::GetCanonicalName
void wxLocale_GetCanonicalName(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd)
{
  ErlNifEnv *env = Ecmd.env;
  ERL_NIF_TERM * argv = Ecmd.args;
  wxLocale *This;
  This = (wxLocale *) memenv->getPtr(env, argv[0], "This");
  if(!This) throw wxe_badarg("This");
  wxString Result = This->GetCanonicalName();
  wxeReturn rt = wxeReturn(memenv, Ecmd.caller, true);
  rt.send(  rt.make(Result));

}

// wxLocale::GetLanguage
void wxLocale_GetLanguage(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd)
{
  ErlNifEnv *env = Ecmd.env;
  ERL_NIF_TERM * argv = Ecmd.args;
  wxLocale *This;
  This = (wxLocale *) memenv->getPtr(env, argv[0], "This");
  if(!This) throw wxe_badarg("This");
  int Result = This->GetLanguage();
  wxeReturn rt = wxeReturn(memenv, Ecmd.caller, true);
  rt.send(  rt.make_int(Result));

}

// wxLocale::GetLanguageName
void wxLocale_GetLanguageName(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd)
{
  ErlNifEnv *env = Ecmd.env;
  ERL_NIF_TERM * argv = Ecmd.args;
  int lang;
  if(!enif_get_int(env, argv[0], &lang)) Badarg("lang"); // int
  wxString Result = wxLocale::GetLanguageName(lang);
  wxeReturn rt = wxeReturn(memenv, Ecmd.caller, true);
  rt.send(  rt.make(Result));

}

// wxLocale::GetLocale
void wxLocale_GetLocale(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd)
{
  ErlNifEnv *env = Ecmd.env;
  ERL_NIF_TERM * argv = Ecmd.args;
  wxLocale *This;
  This = (wxLocale *) memenv->getPtr(env, argv[0], "This");
  if(!This) throw wxe_badarg("This");
  const wxString Result = This->GetLocale();
  wxeReturn rt = wxeReturn(memenv, Ecmd.caller, true);
  rt.send(  rt.make(Result));

}

// wxLocale::GetName
void wxLocale_GetName(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd)
{
  ErlNifEnv *env = Ecmd.env;
  ERL_NIF_TERM * argv = Ecmd.args;
  wxLocale *This;
  This = (wxLocale *) memenv->getPtr(env, argv[0], "This");
  if(!This) throw wxe_badarg("This");
  const wxString Result = This->GetName();
  wxeReturn rt = wxeReturn(memenv, Ecmd.caller, true);
  rt.send(  rt.make(Result));

}

// wxLocale::GetString
void wxLocale_GetString_2(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd)
{
  wxString szDomain= wxEmptyString;
  ErlNifEnv *env = Ecmd.env;
  ERL_NIF_TERM * argv = Ecmd.args;
  wxLocale *This;
  This = (wxLocale *) memenv->getPtr(env, argv[0], "This");
  ErlNifBinary origString_bin;
  wxString origString;
  if(!enif_inspect_binary(env, argv[1], &origString_bin)) Badarg("origString");
  origString = wxString(origString_bin.data, wxConvUTF8, origString_bin.size);
  ERL_NIF_TERM lstHead, lstTail;
  lstTail = argv[2];
  if(!enif_is_list(env, lstTail)) Badarg("Options");
  const ERL_NIF_TERM *tpl;
  int tpl_sz;
  while(!enif_is_empty_list(env, lstTail)) {
    if(!enif_get_list_cell(env, lstTail, &lstHead, &lstTail)) Badarg("Options");
    if(!enif_get_tuple(env, lstHead, &tpl_sz, &tpl) || tpl_sz != 2) Badarg("Options");
    if(enif_is_identical(tpl[0], enif_make_atom(env, "szDomain"))) {
  ErlNifBinary szDomain_bin;
  if(!enif_inspect_binary(env, tpl[1], &szDomain_bin)) Badarg("szDomain");
  szDomain = wxString(szDomain_bin.data, wxConvUTF8, szDomain_bin.size);
    } else        Badarg("Options");
  };
  if(!This) throw wxe_badarg("This");
  const wxString Result = This->GetString(origString,szDomain);
  wxeReturn rt = wxeReturn(memenv, Ecmd.caller, true);
  rt.send(  rt.make(Result));

}

// wxLocale::GetString
void wxLocale_GetString_4(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd)
{
  wxString szDomain= wxEmptyString;
  ErlNifEnv *env = Ecmd.env;
  ERL_NIF_TERM * argv = Ecmd.args;
  wxLocale *This;
  This = (wxLocale *) memenv->getPtr(env, argv[0], "This");
  ErlNifBinary origString_bin;
  wxString origString;
  if(!enif_inspect_binary(env, argv[1], &origString_bin)) Badarg("origString");
  origString = wxString(origString_bin.data, wxConvUTF8, origString_bin.size);
  ErlNifBinary origString2_bin;
  wxString origString2;
  if(!enif_inspect_binary(env, argv[2], &origString2_bin)) Badarg("origString2");
  origString2 = wxString(origString2_bin.data, wxConvUTF8, origString2_bin.size);
  unsigned int n;
  if(!enif_get_uint(env, argv[3], &n)) Badarg("n");
  ERL_NIF_TERM lstHead, lstTail;
  lstTail = argv[4];
  if(!enif_is_list(env, lstTail)) Badarg("Options");
  const ERL_NIF_TERM *tpl;
  int tpl_sz;
  while(!enif_is_empty_list(env, lstTail)) {
    if(!enif_get_list_cell(env, lstTail, &lstHead, &lstTail)) Badarg("Options");
    if(!enif_get_tuple(env, lstHead, &tpl_sz, &tpl) || tpl_sz != 2) Badarg("Options");
    if(enif_is_identical(tpl[0], enif_make_atom(env, "szDomain"))) {
  ErlNifBinary szDomain_bin;
  if(!enif_inspect_binary(env, tpl[1], &szDomain_bin)) Badarg("szDomain");
  szDomain = wxString(szDomain_bin.data, wxConvUTF8, szDomain_bin.size);
    } else        Badarg("Options");
  };
  if(!This) throw wxe_badarg("This");
  const wxString Result = This->GetString(origString,origString2,n,szDomain);
  wxeReturn rt = wxeReturn(memenv, Ecmd.caller, true);
  rt.send(  rt.make(Result));

}

// wxLocale::GetHeaderValue
void wxLocale_GetHeaderValue(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd)
{
  wxString szDomain= wxEmptyString;
  ErlNifEnv *env = Ecmd.env;
  ERL_NIF_TERM * argv = Ecmd.args;
  wxLocale *This;
  This = (wxLocale *) memenv->getPtr(env, argv[0], "This");
  ErlNifBinary header_bin;
  wxString header;
  if(!enif_inspect_binary(env, argv[1], &header_bin)) Badarg("header");
  header = wxString(header_bin.data, wxConvUTF8, header_bin.size);
  ERL_NIF_TERM lstHead, lstTail;
  lstTail = argv[2];
  if(!enif_is_list(env, lstTail)) Badarg("Options");
  const ERL_NIF_TERM *tpl;
  int tpl_sz;
  while(!enif_is_empty_list(env, lstTail)) {
    if(!enif_get_list_cell(env, lstTail, &lstHead, &lstTail)) Badarg("Options");
    if(!enif_get_tuple(env, lstHead, &tpl_sz, &tpl) || tpl_sz != 2) Badarg("Options");
    if(enif_is_identical(tpl[0], enif_make_atom(env, "szDomain"))) {
  ErlNifBinary szDomain_bin;
  if(!enif_inspect_binary(env, tpl[1], &szDomain_bin)) Badarg("szDomain");
  szDomain = wxString(szDomain_bin.data, wxConvUTF8, szDomain_bin.size);
    } else        Badarg("Options");
  };
  if(!This) throw wxe_badarg("This");
  wxString Result = This->GetHeaderValue(header,szDomain);
  wxeReturn rt = wxeReturn(memenv, Ecmd.caller, true);
  rt.send(  rt.make(Result));

}

// wxLocale::GetSysName
void wxLocale_GetSysName(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd)
{
  ErlNifEnv *env = Ecmd.env;
  ERL_NIF_TERM * argv = Ecmd.args;
  wxLocale *This;
  This = (wxLocale *) memenv->getPtr(env, argv[0], "This");
  if(!This) throw wxe_badarg("This");
  wxString Result = This->GetSysName();
  wxeReturn rt = wxeReturn(memenv, Ecmd.caller, true);
  rt.send(  rt.make(Result));

}

// wxLocale::GetSystemEncoding
void wxLocale_GetSystemEncoding(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd)
{
  int Result = wxLocale::GetSystemEncoding();
  wxeReturn rt = wxeReturn(memenv, Ecmd.caller, true);
  rt.send(  rt.make_int(Result));

}

// wxLocale::GetSystemEncodingName
void wxLocale_GetSystemEncodingName(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd)
{
  wxString Result = wxLocale::GetSystemEncodingName();
  wxeReturn rt = wxeReturn(memenv, Ecmd.caller, true);
  rt.send(  rt.make(Result));

}

// wxLocale::GetSystemLanguage
void wxLocale_GetSystemLanguage(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd)
{
  int Result = wxLocale::GetSystemLanguage();
  wxeReturn rt = wxeReturn(memenv, Ecmd.caller, true);
  rt.send(  rt.make_int(Result));

}

// wxLocale::IsLoaded
void wxLocale_IsLoaded(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd)
{
  ErlNifEnv *env = Ecmd.env;
  ERL_NIF_TERM * argv = Ecmd.args;
  wxLocale *This;
  This = (wxLocale *) memenv->getPtr(env, argv[0], "This");
  ErlNifBinary domain_bin;
  wxString domain;
  if(!enif_inspect_binary(env, argv[1], &domain_bin)) Badarg("domain");
  domain = wxString(domain_bin.data, wxConvUTF8, domain_bin.size);
  if(!This) throw wxe_badarg("This");
  bool Result = This->IsLoaded(domain);
  wxeReturn rt = wxeReturn(memenv, Ecmd.caller, true);
  rt.send(  rt.make_bool(Result));

}

// wxLocale::IsOk
void wxLocale_IsOk(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd)
{
  ErlNifEnv *env = Ecmd.env;
  ERL_NIF_TERM * argv = Ecmd.args;
  wxLocale *This;
  This = (wxLocale *) memenv->getPtr(env, argv[0], "This");
  if(!This) throw wxe_badarg("This");
  bool Result = This->IsOk();
  wxeReturn rt = wxeReturn(memenv, Ecmd.caller, true);
  rt.send(  rt.make_bool(Result));

}

// wxLogNull::wxLogNull
void wxLogNull_new(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd)
{
  wxLogNull * Result = new wxLogNull();
  app->newPtr((void *) Result, 230, memenv);
  wxeReturn rt = wxeReturn(memenv, Ecmd.caller, true);
  rt.send(  rt.make_ref(app->getRef((void *)Result,memenv), "wxLogNull"));

}

// wxLogNull::~wxLogNull
void wxLogNull_destruct(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd)
{
 ErlNifEnv *env = Ecmd.env;
 ERL_NIF_TERM * argv = Ecmd.args;
  wxLogNull *This;
  This = (wxLogNull *) memenv->getPtr(env, argv[0], "This");
 if(This) {   ((WxeApp *) wxTheApp)->clearPtr((void *) This);
   delete This;}
}

