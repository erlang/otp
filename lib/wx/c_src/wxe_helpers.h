/*
 * %CopyrightBegin%
 *
 * Copyright Ericsson AB 2014. All Rights Reserved.
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

#ifndef _WXE_HELPER_H
#define	_WXE_HELPER_H

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
    virtual ~wxeCommand(); // Use Delete()

    wxeCommand * Save() {ref_count++; return this; };
    void Delete() {if(--ref_count < 1) delete this;};

    ErlDrvTermData   caller;
    ErlDrvTermData   port;
    WXEBinRef *      bin[3];
    char *           buffer;
    int              len;
    int              op;
    int              ref_count;
};

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
    ~intList() {
	intListElement *head = list;
	while(head) {
	    intListElement *tail=head->cdr;
	    delete head;
	    head = tail;
	} };
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

class wxETreeItemData : public wxTreeItemData
{
 public:
    wxETreeItemData(int sz, char * data);
    ~wxETreeItemData();

    int size;
    char * bin;
};

#endif
