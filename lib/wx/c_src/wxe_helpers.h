/*
 * %CopyrightBegin%
 *
 * Copyright Ericsson AB 2014-2016. All Rights Reserved.
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

class wxeCommand
{
 public:
    wxeCommand();
    virtual ~wxeCommand(); // Use Delete()

    wxeCommand * Save(int Op) { op = Op; return this; };
    void Delete();

    ErlDrvTermData   caller;
    ErlDrvTermData   port;
    WXEBinRef        bin[3];
    char *           buffer;
    int              len;
    int              op;
    char             c_buf[64];  // 64b covers 90% of usage
};

class wxeFifo {
 public:
    wxeFifo(unsigned int size);
    virtual ~wxeFifo();

    void Add(int fc, char * cbuf,int buflen, wxe_data *);
    void Append(wxeCommand *Other);

    wxeCommand * Get();
    wxeCommand * Peek(unsigned int *item);

    void Realloc();
    void Strip();
    unsigned int Cleanup(unsigned int peek=0);

    unsigned int cb_start;
    unsigned int m_max;
    unsigned int m_first;
    unsigned int m_n;
    unsigned int m_orig_sz;
    wxeCommand *m_q;
    wxeCommand *m_old;
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
