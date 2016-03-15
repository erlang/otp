/*
 * %CopyrightBegin%
 * 
 * Copyright Ericsson AB 1997-2016. All Rights Reserved.
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

#define UNICODE 1
#define _UNICODE 1
#include <tchar.h>
#include <stdio.h>
#ifdef HAVE_CONFIG_H
#  include "config.h"
#endif
#include "sys.h"
#include <windowsx.h>
#include "resource.h"
#include "erl_version.h"
#include <commdlg.h>
#include <commctrl.h>
#include "erl_driver.h"
#include "win_con.h"

#define ALLOC(X) malloc(X)
#define REALLOC(X,Y) realloc(X,Y)
#define FREE(X) free(X)

#if SIZEOF_VOID_P == 8
#define WIN64 1
#ifndef GCL_HBRBACKGROUND
#define GCL_HBRBACKGROUND GCLP_HBRBACKGROUND
#endif
#define DIALOG_PROC_RET INT_PTR
#define CF_HOOK_RET INT_PTR
#define CC_HOOK_RET INT_PTR
#define OFN_HOOK_RET INT_PTR
#else
#define DIALOG_PROC_RET BOOL
#define CF_HOOK_RET UINT
#define CC_HOOK_RET UINT
#define OFN_HOOK_RET UINT
#endif


#ifndef STATE_SYSTEM_INVISIBLE
/* Mingw problem with oleacc.h and WIN32_LEAN_AND_MEAN */
#define STATE_SYSTEM_INVISIBLE 0x00008000
#endif

#define WM_CONTEXT      (0x0401)
#define WM_CONBEEP      (0x0402)
#define WM_SAVE_PREFS   (0x0403)

#define USER_KEY TEXT("Software\\Ericsson\\Erlang\\") TEXT(ERLANG_VERSION)

#define FRAME_HEIGHT ((2*GetSystemMetrics(SM_CYEDGE))+(2*GetSystemMetrics(SM_CYFRAME))+GetSystemMetrics(SM_CYCAPTION))
#define FRAME_WIDTH  (2*GetSystemMetrics(SM_CXFRAME)+(2*GetSystemMetrics(SM_CXFRAME))+GetSystemMetrics(SM_CXVSCROLL))

#define LINE_LENGTH canvasColumns
#define COL(_l) ((_l) % LINE_LENGTH)
#define LINE(_l) ((_l) / LINE_LENGTH)

#ifdef UNICODE
/*
 * We use a character in the invalid unicode range
 */
#define SET_CURSOR (0xD8FF) 
#else
/*
 * XXX There is no escape to send a character 0x80.  Fortunately, 
 * the ttsl driver currently replaces 0x80 with an octal sequence.
 */
#define SET_CURSOR (0x80)
#endif

#define SCAN_CODE_BREAK 0x46	/* scan code for Ctrl-Break */


typedef struct ScreenLine_s {
    struct ScreenLine_s* next;
    struct ScreenLine_s* prev;
    int width;
#ifdef HARDDEBUG
    int allocated;
#endif
    int newline; /* Ends with hard newline: 1, wrapped at end: 0 */
    TCHAR *text;
} ScreenLine_t;

extern Uint32 *lbuf;		/* The current line buffer */
extern int llen;		/* The current line length */
extern int lpos;

HANDLE console_input_event;
HANDLE console_thread = NULL;

#define DEF_CANVAS_COLUMNS 80
#define DEF_CANVAS_ROWS 26

#define BUFSIZE 4096
#define MAXBUFSIZE 32768
typedef struct {
    TCHAR *data; 
    int size;
    int wrPos;
    int rdPos;
} buffer_t;

static buffer_t inbuf;
static buffer_t outbuf;

static CHOOSEFONT cf;

static TCHAR szFrameClass[] = TEXT("FrameClass");
static TCHAR szClientClass[] = TEXT("ClientClass");
static HWND hFrameWnd;
static HWND hClientWnd;
static HWND hTBWnd;
static HWND hComboWnd;
static HANDLE console_input;
static HANDLE console_output;
static int cxChar,cyChar, cxCharMax;
static int cxClient,cyClient;
static int cyToolBar;
static int iVscrollPos,iHscrollPos;
static int iVscrollMax,iHscrollMax;
static int nBufLines;
static int cur_x;
static int cur_y;
static int canvasColumns = DEF_CANVAS_COLUMNS;
static int canvasRows = DEF_CANVAS_ROWS;
static ScreenLine_t *buffer_top,*buffer_bottom;
static ScreenLine_t* cur_line;
static POINT editBeg,editEnd;
static BOOL fSelecting = FALSE;
static BOOL fTextSelected = FALSE;
static HKEY key;
static BOOL has_key = FALSE;
static LOGFONT logfont;
static DWORD fgColor;
static DWORD bkgColor;
static FILE *logfile = NULL;
static RECT winPos;
static BOOL toolbarVisible;
static BOOL destroyed = FALSE;

static int lines_to_save = 1000; /* Maximum number of screen lines to save. */

#define TITLE_BUF_SZ 256

struct title_buf {
    TCHAR *name;
    TCHAR buf[TITLE_BUF_SZ];
};

static TCHAR *erlang_window_title = TEXT("Erlang");

static unsigned __stdcall ConThreadInit(LPVOID param);
static LRESULT CALLBACK ClientWndProc(HWND hwnd, UINT iMsg, WPARAM wParam, LPARAM lParam);
static LRESULT CALLBACK FrameWndProc(HWND hwnd, UINT iMsg, WPARAM wParam, LPARAM lParam);
static DIALOG_PROC_RET CALLBACK AboutDlgProc(HWND hDlg, UINT iMsg, WPARAM wParam, LPARAM lParam);
static ScreenLine_t *ConNewLine(void);
static void DeleteTopLine(void);
static void ensure_line_below(void);
static ScreenLine_t *GetLineFromY(int y);
static void LoadUserPreferences(void);
static void SaveUserPreferences(void);
static void set_scroll_info(HWND hwnd);
static void ConCarriageFeed(int);
static void ConScrollScreen(void);
static BOOL ConChooseFont(HWND hwnd);
static void ConFontInitialize(HWND hwnd);
static void ConSetFont(HWND hwnd);
static void ConChooseColor(HWND hwnd);
static void DrawSelection(HWND hwnd, POINT pt1, POINT pt2);
static void InvertSelectionArea(HWND hwnd);
static void OnEditCopy(HWND hwnd);
static void OnEditPaste(HWND hwnd);
static void OnEditSelAll(HWND hwnd);
static void GetFileName(HWND hwnd, TCHAR *pFile);
static void OpenLogFile(HWND hwnd);
static void CloseLogFile(HWND hwnd);
static void LogFileWrite(TCHAR *buf, int n);
static int write_inbuf(TCHAR *data, int n);
static void init_buffers(void);
static void AddToCmdHistory(void);
static int write_outbuf(TCHAR *data, int num_chars);
static void ConDrawText(HWND hwnd);
static BOOL (WINAPI *ctrl_handler)(DWORD);
static HWND InitToolBar(HWND hwndParent); 
static void window_title(struct title_buf *);
static void free_window_title(struct title_buf *);
static void Client_OnMouseMove(HWND hwnd, int x, int y, UINT keyFlags);

#define CON_VPRINTF_BUF_INC_SIZE 1024

static erts_dsprintf_buf_t *
grow_con_vprintf_buf(erts_dsprintf_buf_t *dsbufp, size_t need)
{
    char *buf;
    size_t size;

    ASSERT(dsbufp);

    if (!dsbufp->str) {
	size = (((need + CON_VPRINTF_BUF_INC_SIZE - 1)
		 / CON_VPRINTF_BUF_INC_SIZE)
		* CON_VPRINTF_BUF_INC_SIZE);
	buf = (char *) ALLOC(size * sizeof(char));
    }
    else {
	size_t free_size = dsbufp->size - dsbufp->str_len;

	if (need <= free_size)
	    return dsbufp;

	size = need - free_size + CON_VPRINTF_BUF_INC_SIZE;
	size = (((size + CON_VPRINTF_BUF_INC_SIZE - 1)
		 / CON_VPRINTF_BUF_INC_SIZE)
		* CON_VPRINTF_BUF_INC_SIZE);
	size += dsbufp->size;
	buf = (char *) REALLOC((void *) dsbufp->str,
			       size * sizeof(char));
    }
    if (!buf)
	return NULL;
    if (buf != dsbufp->str)
	dsbufp->str = buf;
    dsbufp->size = size;
    return dsbufp;
}

static int con_vprintf(char *format, va_list arg_list)
{
    int res,i;
    erts_dsprintf_buf_t dsbuf = ERTS_DSPRINTF_BUF_INITER(grow_con_vprintf_buf);
    res = erts_vdsprintf(&dsbuf, format, arg_list);
    if (res >= 0) {
	TCHAR *tmp = ALLOC(dsbuf.str_len*sizeof(TCHAR));
	for (i=0;i<dsbuf.str_len;++i) {
	    tmp[i] = dsbuf.str[i];
	}
	write_outbuf(tmp, dsbuf.str_len);
	FREE(tmp);
    }
    if (dsbuf.str)
      FREE((void *) dsbuf.str);
    return res;
}

void
ConInit(void)
{
    unsigned tid;
    
    console_input = CreateSemaphore(NULL, 0, 1, NULL);
    console_output = CreateSemaphore(NULL, 0, 1, NULL);
    console_input_event = CreateManualEvent(FALSE);
    console_thread = (HANDLE *) _beginthreadex(NULL, 0, 
					       ConThreadInit,
					       0, 0, &tid);

    /* Make all erts_*printf on stdout and stderr use con_vprintf */
    erts_printf_stdout_func = con_vprintf;
    erts_printf_stderr_func = con_vprintf;
}

/*
  ConNormalExit() is called from erts_exit() when the emulator
  is stopping. If the exit has not been initiated by this 
  console thread (WM_DESTROY or ID_BREAK), the function must 
  invoke the console thread to save the user preferences.
*/
void
ConNormalExit(void)
{
  if (!destroyed)
    SendMessage(hFrameWnd, WM_SAVE_PREFS, 0L, 0L);
}

void
ConWaitForExit(void)
{
    ConPrintf("\n\nAbnormal termination\n");
    WaitForSingleObject(console_thread, INFINITE);
}

void ConSetCtrlHandler(BOOL (WINAPI *handler)(DWORD))
{
    ctrl_handler = handler;
}

int ConPutChar(Uint32 c)
{
    TCHAR sbuf[1];
#ifdef HARDDEBUG
    fprintf(stderr,"ConPutChar: %d\n",(int) c);
    fflush(stderr);
#endif
    sbuf[0] = c;
    write_outbuf(sbuf, 1);
    return 1;
}

static int GetXFromLine(HDC hdc, int hscroll, int xpos,ScreenLine_t *pLine)
{
   SIZE size;
   int hscrollPix = hscroll * cxChar; 

   if (pLine == NULL) {
       return 0;
   }

   if (pLine->width < xpos) {
       return (canvasColumns-hscroll)*cxChar;
   }
   /* Not needed (?): SelectObject(hdc,CreateFontIndirect(&logfont)); */
   if (GetTextExtentPoint32(hdc,pLine->text,xpos,&size)) {
#ifdef HARDDEBUG
       fprintf(stderr,"size.cx:%d\n",(int)size.cx);
       fflush(stderr);
#endif
       if (hscrollPix >= size.cx) {
	   return 0;
       }
       return ((int) size.cx) - hscrollPix;
   } else {
       return (xpos-hscroll)*cxChar;
   }
}

static int GetXFromCurrentY(HDC hdc, int hscroll, int xpos) {
    return GetXFromLine(hdc, hscroll, xpos, GetLineFromY(cur_y));
}

void ConSetCursor(int from, int to)
{   TCHAR cmd[9];
    int *p;
    //DebugBreak();
    cmd[0] = SET_CURSOR;
    /*
     * XXX Expect trouble on CPUs which don't allow misaligned read and writes.
     */
    p = (int *)&cmd[1];
    *p++ = from;
    *p = to;
    write_outbuf(cmd, 1 + (2*sizeof(int)/sizeof(TCHAR)));
}

void ConPrintf(char *format, ...)
{
    va_list va;

    va_start(va, format);
    (void) con_vprintf(format, va);
    va_end(va);
}

void ConBeep(void)
{
    SendMessage(hClientWnd, WM_CONBEEP, 0L, 0L);
}

int ConReadInput(Uint32 *data, int num_chars)
{
    TCHAR *buf;
    int nread;
    WaitForSingleObject(console_input,INFINITE);
    nread = num_chars = min(num_chars,inbuf.wrPos-inbuf.rdPos);
    buf = &inbuf.data[inbuf.rdPos];
    inbuf.rdPos += nread;
    while (nread--) 
        *data++ = *buf++;
    if (inbuf.rdPos >= inbuf.wrPos) {
        inbuf.rdPos = 0;
        inbuf.wrPos = 0;
        ResetEvent(console_input_event);
    }
    ReleaseSemaphore(console_input,1,NULL);
    return num_chars;
}

int ConGetKey(void)
{
    Uint32 c;
    WaitForSingleObject(console_input,INFINITE);
    ResetEvent(console_input_event);
    inbuf.rdPos = inbuf.wrPos = 0;
    ReleaseSemaphore(console_input,1,NULL);
    WaitForSingleObject(console_input_event,INFINITE);
    ConReadInput(&c, 1);
    return (int) c;
}

int ConGetColumns(void) 
{
    return (int) canvasColumns; /* 32bit atomic on windows */
}

int ConGetRows(void) {
    return (int) canvasRows;
}


static HINSTANCE hInstance;
extern HMODULE beam_module;

static unsigned __stdcall
ConThreadInit(LPVOID param)
{
    MSG msg;
    WNDCLASSEX wndclass;
    int iCmdShow;
    STARTUPINFO StartupInfo;
    HACCEL hAccel;
    int x, y, w, h;
    struct title_buf title;

    /*DebugBreak();*/
    hInstance = GetModuleHandle(NULL);
    StartupInfo.dwFlags = 0;
    GetStartupInfo(&StartupInfo);
    iCmdShow = StartupInfo.dwFlags & STARTF_USESHOWWINDOW ?
	StartupInfo.wShowWindow : SW_SHOWDEFAULT;

    LoadUserPreferences();

    /* frame window class */
    wndclass.cbSize	        = sizeof (wndclass);	
    wndclass.style          = CS_HREDRAW | CS_VREDRAW | CS_BYTEALIGNCLIENT;
    wndclass.lpfnWndProc    = FrameWndProc;
    wndclass.cbClsExtra     = 0;
    wndclass.cbWndExtra     = 0;
    wndclass.hInstance      = hInstance;
    wndclass.hIcon          = LoadIcon (hInstance, MAKEINTRESOURCE(1));
    wndclass.hCursor        = LoadCursor (NULL, IDC_ARROW);
    wndclass.hbrBackground  = NULL;
    wndclass.lpszMenuName   = NULL;
    wndclass.lpszClassName  = szFrameClass;
    wndclass.hIconSm	    = LoadIcon (hInstance, MAKEINTRESOURCE(1));
    RegisterClassExW (&wndclass);

    /* client window class */
    wndclass.cbSize	        = sizeof (wndclass);	
    wndclass.style          = CS_HREDRAW | CS_VREDRAW | CS_OWNDC;
    wndclass.lpfnWndProc    = ClientWndProc;
    wndclass.cbClsExtra     = 0;
    wndclass.cbWndExtra     = 0;
    wndclass.hInstance      = hInstance;
    wndclass.hIcon          = LoadIcon (hInstance, MAKEINTRESOURCE(1));
    wndclass.hCursor        = LoadCursor (NULL, IDC_ARROW);
    wndclass.hbrBackground  = CreateSolidBrush(bkgColor);
    wndclass.lpszMenuName   = NULL;
    wndclass.lpszClassName  = szClientClass;
    wndclass.hIconSm	    = LoadIcon (hInstance, MAKEINTRESOURCE(1));
    RegisterClassExW (&wndclass);

    InitCommonControls();
    init_buffers();

    nBufLines = 0;
    buffer_top = cur_line = ConNewLine();
    cur_line->next = buffer_bottom = ConNewLine();
    buffer_bottom->prev = cur_line;

    /* Create Frame Window */
    window_title(&title);
    hFrameWnd = CreateWindowEx(0, szFrameClass, title.name,
			       WS_OVERLAPPEDWINDOW,CW_USEDEFAULT,
			       CW_USEDEFAULT,CW_USEDEFAULT,CW_USEDEFAULT,	
			       NULL,LoadMenu(beam_module,MAKEINTRESOURCE(1)),
			       hInstance,NULL);
    free_window_title(&title);

    /* XXX OTP-5522:
       The window position is not saved correctly and if the window
       is closed when minimized, it's not possible to start werl again
       with the window open. Temporary fix so far is to ignore saved values
       and always start with initial settings. */
    /* Original:   if (winPos.left == -1) {  */
    /* Temporary:  if (1) { */
    if (1) {

	/* initial window position */
	x = 0;
	y = 0;
	w = cxChar*LINE_LENGTH+FRAME_WIDTH+GetSystemMetrics(SM_CXVSCROLL);
	h = cyChar*30+FRAME_HEIGHT;
    } else {
	/* saved window position */
	x = winPos.left;
	y = winPos.top;
	w = winPos.right - x;
	h = winPos.bottom - y;
    }
    SetWindowPos(hFrameWnd, NULL, x, y, w, h, SWP_NOZORDER);

    ShowWindow(hFrameWnd, iCmdShow);
    UpdateWindow(hFrameWnd);

    hAccel = LoadAccelerators(beam_module,MAKEINTRESOURCE(1));

    ReleaseSemaphore(console_input, 1, NULL);
    ReleaseSemaphore(console_output, 1, NULL);


    /* Main message loop */
    while (GetMessage (&msg, NULL, 0, 0))
    {
        if (!TranslateAccelerator(hFrameWnd,hAccel,&msg)) 
        {
            TranslateMessage (&msg);
            DispatchMessage (&msg);
        }
    }
    /*
       PostQuitMessage() results in WM_QUIT which makes GetMessage()
       return 0 (which stops the main loop). Before we return from
       the console thread, the ctrl_handler is called to do erts_exit.
    */
    (*ctrl_handler)(CTRL_CLOSE_EVENT);
    return msg.wParam;
}

static LRESULT CALLBACK
FrameWndProc(HWND hwnd, UINT iMsg, WPARAM wParam, LPARAM lParam)
{
    RECT r;
    int cy,i,bufsize;
    TCHAR c;
    unsigned long l;
    TCHAR buf[128];
    struct title_buf title;

    switch (iMsg) {         
    case WM_CREATE:
        /* client window creation */
	window_title(&title);
        hClientWnd = CreateWindowEx(WS_EX_CLIENTEDGE, szClientClass, title.name,
				    WS_CHILD|WS_VISIBLE|WS_VSCROLL|WS_HSCROLL,
				    CW_USEDEFAULT, CW_USEDEFAULT,
				    CW_USEDEFAULT, CW_USEDEFAULT,		
				    hwnd, (HMENU)0, hInstance, NULL);
	free_window_title(&title);
        hTBWnd = InitToolBar(hwnd);
        UpdateWindow (hClientWnd);
        return 0;
    case WM_SIZE :
        if (IsWindowVisible(hTBWnd)) {
            SendMessage(hTBWnd,TB_AUTOSIZE,0,0L);
            GetWindowRect(hTBWnd,&r);
            cy = r.bottom-r.top;
        } else cy = 0;
        MoveWindow(hClientWnd,0,cy,LOWORD(lParam),HIWORD(lParam)-cy,TRUE);
        return 0;
    case WM_ERASEBKGND:
        return 1;
    case WM_SETFOCUS :
        CreateCaret(hClientWnd, NULL, cxChar, cyChar);
        SetCaretPos(GetXFromCurrentY(GetDC(hClientWnd),iHscrollPos,cur_x), (cur_y-iVscrollPos)*cyChar);
        ShowCaret(hClientWnd);
        return 0;
    case WM_KILLFOCUS:
        HideCaret(hClientWnd);
        DestroyCaret();
        return 0;
    case WM_INITMENUPOPUP :
        if (lParam == 0)	/* File popup menu */
        {
            EnableMenuItem((HMENU)wParam, IDMENU_STARTLOG,
			   logfile ? MF_GRAYED : MF_ENABLED);
            EnableMenuItem((HMENU)wParam, IDMENU_STOPLOG,
			   logfile ? MF_ENABLED : MF_GRAYED);
            return 0;
	}
        else if (lParam == 1)	/* Edit popup menu */
        {
            EnableMenuItem((HMENU)wParam, IDMENU_COPY,
			   fTextSelected ? MF_ENABLED : MF_GRAYED);
            EnableMenuItem((HMENU)wParam, IDMENU_PASTE,
			   IsClipboardFormatAvailable(CF_TEXT) ? MF_ENABLED : MF_GRAYED);
            return 0;
        }
        else if (lParam == 3)	/* View popup menu */
        {
            CheckMenuItem((HMENU)wParam,IDMENU_TOOLBAR,
			  IsWindowVisible(hTBWnd) ? MF_CHECKED : MF_UNCHECKED);
            return 0;
        }
        break;
    case WM_NOTIFY: 
        switch (((LPNMHDR) lParam)->code) { 
        case TTN_NEEDTEXT: 
            {    
		LPTOOLTIPTEXT lpttt; 
		lpttt = (LPTOOLTIPTEXT) lParam; 
		lpttt->hinst = hInstance;
		/* check for combobox handle */
		if (lpttt->uFlags&TTF_IDISHWND) {
		    if ((lpttt->hdr.idFrom == (UINT) hComboWnd)) {
			lstrcpy(lpttt->lpszText,TEXT("Command History"));
			break;
		    }
		}
		/* check for toolbar buttons */
		switch (lpttt->hdr.idFrom) { 
                case IDMENU_COPY: 
                    lstrcpy(lpttt->lpszText,TEXT("Copy (Ctrl+C)")); 
                    break; 
                case IDMENU_PASTE: 
                    lstrcpy(lpttt->lpszText,TEXT("Paste (Ctrl+V)")); 
                    break; 
		case IDMENU_FONT: 
                    lstrcpy(lpttt->lpszText,TEXT("Fonts")); 
                    break; 
		case IDMENU_ABOUT: 
                    lstrcpy(lpttt->lpszText,TEXT("Help")); 
                    break; 
		} 
	    }
        }
        break; 
    case WM_COMMAND:
	switch(LOWORD(wParam))
	{
	case IDMENU_STARTLOG:
            OpenLogFile(hwnd);
	    return 0;
	case IDMENU_STOPLOG:
            CloseLogFile(hwnd);
	    return 0;
	case IDMENU_EXIT:
	    SendMessage(hwnd, WM_CLOSE, 0, 0L);
	    return 0;
	case IDMENU_COPY:
	    if (fTextSelected)
                OnEditCopy(hClientWnd);
	    return 0;
	case IDMENU_PASTE:
	    OnEditPaste(hClientWnd);
	    return 0;
        case IDMENU_SELALL:
            OnEditSelAll(hClientWnd);
            return 0;
	case IDMENU_FONT:
	    if (ConChooseFont(hClientWnd)) {
		ConSetFont(hClientWnd);
	    }
	    SaveUserPreferences();
	    return 0;
        case IDMENU_SELECTBKG:
            ConChooseColor(hClientWnd);
	    SaveUserPreferences();
            return 0;
        case IDMENU_TOOLBAR:
            if (toolbarVisible) {
                ShowWindow(hTBWnd,SW_HIDE);
		toolbarVisible = FALSE;
            } else {
                ShowWindow(hTBWnd,SW_SHOW);
		toolbarVisible = TRUE;
	    }
            GetClientRect(hwnd,&r);
            PostMessage(hwnd,WM_SIZE,0,MAKELPARAM(r.right,r.bottom));
            return 0;
        case IDMENU_ABOUT:
            DialogBox(beam_module,TEXT("AboutBox"),hwnd,AboutDlgProc);
            return 0;
        case ID_COMBOBOX:
            switch (HIWORD(wParam)) {
            case CBN_SELENDOK:
                i = SendMessage(hComboWnd,CB_GETCURSEL,0,0);
                if (i != CB_ERR) {
                    buf[0] = 0x01; /* CTRL+A */
                    buf[1] = 0x0B; /* CTRL+K */
                    bufsize = SendMessage(hComboWnd,CB_GETLBTEXT,i,(LPARAM)&buf[2]);
                    if (bufsize != CB_ERR)
                        write_inbuf(buf,bufsize+2);
                    SetFocus(hwnd);
                }
                break;
            case CBN_SELENDCANCEL:
                break;
            }
	    break;
	case ID_BREAK:		  /* CTRL+BRK */
	    /* pass on break char if the ctrl_handler is disabled */
	    if ((*ctrl_handler)(CTRL_C_EVENT) == FALSE) {
		c = 0x03;
		write_inbuf(&c,1);
	    }
	    return 0;
        }
        break;
    case WM_KEYDOWN :
	switch (wParam) {
	case VK_UP: c = 'P'-'@'; break;
	case VK_DOWN : c = 'N'-'@'; break;
	case VK_RIGHT : c = 'F'-'@'; break;
	case VK_LEFT : c = 'B'-'@'; break;
	case VK_DELETE : c = 'D' -'@'; break;
	case VK_HOME : c = 'A'-'@'; break;
	case VK_END : c = 'E'-'@'; break;
        case VK_RETURN : AddToCmdHistory(); return 0;
	case VK_PRIOR :   /* PageUp */
	    PostMessage(hClientWnd, WM_VSCROLL, SB_PAGEUP, 0);
	    return 0;
	case VK_NEXT :   /* PageDown */
	    PostMessage(hClientWnd, WM_VSCROLL, SB_PAGEDOWN, 0);
	    return 0;
	default: return 0;
	}
        write_inbuf(&c, 1);
	return 0;
    case WM_MOUSEWHEEL:
      {
	int delta = GET_WHEEL_DELTA_WPARAM(wParam);
	if (delta < 0) {
	  PostMessage(hClientWnd, WM_VSCROLL, MAKELONG(SB_THUMBTRACK,
						       (iVscrollPos + 5)),0);
	} else {
	  WORD pos = ((iVscrollPos - 5) < 0) ? 0 : (iVscrollPos - 5);
	  PostMessage(hClientWnd, WM_VSCROLL, MAKELONG(SB_THUMBTRACK,pos),0);
	}
      return 0;
      }
    case WM_CHAR:
	c = (TCHAR)wParam;
        write_inbuf(&c,1);
	return 0;
    case WM_CLOSE :
	break;
    case WM_DESTROY :
	SaveUserPreferences();
	destroyed = TRUE;
	PostQuitMessage(0);
	return 0;
    case WM_SAVE_PREFS :
	SaveUserPreferences();
	return 0;
    }
    return DefWindowProc(hwnd, iMsg, wParam, lParam);
}

static BOOL
Client_OnCreate(HWND hwnd, LPCREATESTRUCT lpCreateStruct)
{
    ConFontInitialize(hwnd);
    cur_x = cur_y = 0;
    iVscrollPos = 0;
    iHscrollPos = 0;
    return TRUE;
}       

static void
Client_OnPaint(HWND hwnd)
{
    ScreenLine_t *pLine;
    int x,y,i,iTop,iBot;
    PAINTSTRUCT ps;
    RECT rcInvalid;
    HDC hdc;

    hdc = BeginPaint(hwnd, &ps);
    rcInvalid = ps.rcPaint;
    hdc = ps.hdc;
    iTop = max(0, iVscrollPos + rcInvalid.top/cyChar);
    iBot = min(nBufLines, iVscrollPos + rcInvalid.bottom/cyChar+1);
    pLine = GetLineFromY(iTop);
    for (i = iTop; i < iBot && pLine != NULL; i++) {
      	y = cyChar*(i-iVscrollPos);
        x = -cxChar*iHscrollPos;
	TextOut(hdc, x, y, &pLine->text[0], pLine->width);
        pLine = pLine->next;
    }
    if (fTextSelected || fSelecting) {
	InvertSelectionArea(hwnd);
    }
    SetCaretPos(GetXFromCurrentY(hdc,iHscrollPos,cur_x), (cur_y-iVscrollPos)*cyChar);
    EndPaint(hwnd, &ps);
}
#ifdef HARDDEBUG
static void dump_linebufs(void) {
    char *buff;
    ScreenLine_t *s = buffer_top;
    fprintf(stderr,"LinebufDump------------------------\n");
    while(s) {
	if (s == buffer_top) fprintf(stderr,"BT-> ");
	if (s == buffer_bottom) fprintf(stderr,"BB-> ");
	if (s == cur_line) fprintf(stderr,"CL-> ");

	buff = (char *) ALLOC(s->width+1);
	memcpy(buff,s->text,s->width);
	buff[s->width] = '\0';
	fprintf(stderr,"{\"%s\",%d,%d}\n",buff,s->newline,s->allocated);
	FREE(buff);
	s = s->next;
    }
    fprintf(stderr,"LinebufDumpEnd---------------------\n");
    fflush(stderr);
}
#endif	    

static void reorganize_linebufs(HWND hwnd) {
    ScreenLine_t *otop = buffer_top;
    ScreenLine_t *obot = buffer_bottom;
    ScreenLine_t *next;
    int i,cpos;

    cpos = 0;
    i = nBufLines - cur_y;
    while (i > 1) {
	cpos += obot->width;
	obot = obot->prev;
	i--;
    }
    cpos += (obot->width - cur_x);
#ifdef HARDDEBUG
    fprintf(stderr,"nBufLines = %d, cur_x = %d, cur_y = %d, cpos = %d\n",
	    nBufLines,cur_x,cur_y,cpos);
    fflush(stderr);
#endif
    

    nBufLines = 0;
    buffer_top = cur_line = ConNewLine();
    cur_line->next = buffer_bottom = ConNewLine();
    buffer_bottom->prev = cur_line;
    
    cur_x = cur_y = 0; 
    iVscrollPos = 0;
    iHscrollPos = 0;

    while(otop) {
	for(i=0;i<otop->width;++i) {
	    cur_line->text[cur_x] = otop->text[i];
	    cur_x++;
            if (cur_x > cur_line->width)
		cur_line->width = cur_x; 
	    if (GetXFromCurrentY(GetDC(hwnd),0,cur_x) + cxChar > 
		(LINE_LENGTH * cxChar)) {
                ConCarriageFeed(0);
	    }
	}
	if (otop->newline) {
	    ConCarriageFeed(1);
            /*ConScrollScreen();*/
	}
	next = otop->next;
	FREE(otop->text);
	FREE(otop);
	otop = next;
    }
    while (cpos) {
	cur_x--;
	if (cur_x < 0) {
	    cur_y--;
	    cur_line = cur_line->prev;
	    cur_x = cur_line->width-1;
	}
	cpos--;
    }
    SetCaretPos(GetXFromCurrentY(GetDC(hwnd),iHscrollPos,cur_x), (cur_y-iVscrollPos)*cyChar);
#ifdef HARDDEBUG
    fprintf(stderr,"canvasColumns = %d,nBufLines = %d, cur_x = %d, cur_y = %d\n",
	    canvasColumns,nBufLines,cur_x,cur_y);
    fflush(stderr);
#endif
}
	    

static void
Client_OnSize(HWND hwnd, UINT state, int cx, int cy)
{
    RECT r;
    SCROLLBARINFO sbi;
    int w,h,columns;
    int scrollheight;
    cxClient = cx;
    cyClient = cy;
    set_scroll_info(hwnd);
    GetClientRect(hwnd,&r);
    w = r.right - r.left;
    h = r.bottom - r.top;
    sbi.cbSize = sizeof(SCROLLBARINFO);
    if (!GetScrollBarInfo(hwnd, OBJID_HSCROLL,&sbi) || 
	(sbi.rgstate[0] & STATE_SYSTEM_INVISIBLE)) {
	scrollheight = 0;
    } else {
	scrollheight = sbi.rcScrollBar.bottom - sbi.rcScrollBar.top;
    }
    canvasRows = (h - scrollheight) / cyChar;
    if (canvasRows < DEF_CANVAS_ROWS) {
	canvasRows = DEF_CANVAS_ROWS;
    }
    columns = (w - GetSystemMetrics(SM_CXVSCROLL)) /cxChar;
    if (columns < DEF_CANVAS_COLUMNS)
	columns = DEF_CANVAS_COLUMNS;
    if (columns != canvasColumns) {
	canvasColumns = columns;
	/*dump_linebufs();*/
	reorganize_linebufs(hwnd);
	fSelecting = fTextSelected = FALSE;
	InvalidateRect(hwnd, NULL, TRUE);
#ifdef HARDDEBUG
	fprintf(stderr,"Paint: cols = %d, rows = %d\n",canvasColumns,canvasRows);
	fflush(stderr);
#endif
    }
    
    SetCaretPos(GetXFromCurrentY(GetDC(hwnd),iHscrollPos,cur_x), (cur_y-iVscrollPos)*cyChar);
}

static void calc_charpoint_from_point(HDC dc, int x, int y, int y_offset, POINT *pt) 
{
    int r;
    int hscrollPix = iHscrollPos * cxChar;

    pt->y = y/cyChar + iVscrollPos + y_offset;

    if (x > (LINE_LENGTH-iHscrollPos) * cxChar) {
	x =  (LINE_LENGTH-iHscrollPos) * cxChar;
    }
    if (pt->y - y_offset > 0 && GetLineFromY(pt->y - y_offset) == NULL) {
	pt->y = nBufLines - 1 + y_offset;
	pt->x = GetLineFromY(pt->y - y_offset)->width;
    } else {
	for (pt->x = 1; 
	     (r = GetXFromLine(dc, 0, pt->x, GetLineFromY(pt->y - y_offset))) != 0 && 
		 (r - hscrollPix) < x; 
	     ++(pt->x))
	    ;
	if ((r - hscrollPix) > x)
	    --(pt->x);
#ifdef HARD_SEL_DEBUG
	fprintf(stderr,"pt->x = %d, iHscrollPos = %d\n",(int) pt->x, iHscrollPos);
	fflush(stderr);
#endif
	if (pt->x <= 0) {
	    pt->x = x/cxChar + iHscrollPos;
	}
    }
}


static void
Client_OnLButtonDown(HWND hwnd, BOOL fDoubleClick, int x, int y, UINT keyFlags)
{
    int r;
    SetFocus(GetParent(hwnd));	/* In case combobox steals the focus */
#ifdef HARD_SEL_DEBUG
    fprintf(stderr,"OnLButtonDown fSelecting = %d, fTextSelected = %d:\n",
	    fSelecting,fTextSelected);
    fflush(stderr);
#endif
    if (fTextSelected) {
	InvertSelectionArea(hwnd);
    }
    fTextSelected = FALSE;

    calc_charpoint_from_point(GetDC(hwnd), x, y, 0, &editBeg);

    editEnd.x = editBeg.x;
    editEnd.y = editBeg.y + 1;
    fSelecting = TRUE;
    SetCapture(hwnd);
}

static void
Client_OnRButtonDown(HWND hwnd, BOOL fDoubleClick, int x, int y, UINT keyFlags)
{
    if (fTextSelected) {
	fSelecting = TRUE;
	Client_OnMouseMove(hwnd,x,y,keyFlags);
	fSelecting = FALSE;
    }
}

static void
Client_OnLButtonUp(HWND hwnd, int x, int y, UINT keyFlags)
{
#ifdef HARD_SEL_DEBUG
    fprintf(stderr,"OnLButtonUp fSelecting = %d, fTextSelected = %d:\n",
	    fSelecting,fTextSelected);
    fprintf(stderr,"(Beg.x = %d, Beg.y = %d, " 
	    "End.x = %d, End.y = %d)\n",editBeg.x,editBeg.y,
	    editEnd.x,editEnd.y);
#endif
    if (fSelecting && 
	!(editBeg.x == editEnd.x && editBeg.y == (editEnd.y - 1))) {
	fTextSelected = TRUE;
    }
#ifdef HARD_SEL_DEBUG
    fprintf(stderr,"OnLButtonUp fTextSelected = %d:\n",
	    fTextSelected);
    fflush(stderr);
#endif
    fSelecting = FALSE;
    ReleaseCapture();
}

#define EMPTY_RECT(R) \
(((R).bottom - (R).top == 0) || ((R).right - (R).left == 0))
#define ABS(X) (((X)< 0) ? -1 * (X) : X) 
#define DIFF(A,B) ABS(((int)(A)) - ((int)(B)))

static int diff_sel_area(RECT old[3], RECT new[3], RECT result[6])
{
    int absposold = old[0].left + old[0].top * canvasColumns;
    int absposnew = new[0].left + new[0].top * canvasColumns;
    int absendold = absposold, absendnew = absposnew;
    int i, x, ret = 0;
    int abspos[2],absend[2];
    for(i = 0; i < 3; ++i) {
	if (!EMPTY_RECT(old[i])) {
	    absendold += (old[i].right - old[i].left) * 
		(old[i].bottom - old[i].top);
	} 
	if (!EMPTY_RECT(new[i])) {
	    absendnew += (new[i].right - new[i].left) * 
		(new[i].bottom - new[i].top);
	}
    }
    abspos[0] = min(absposold, absposnew);
    absend[0] = DIFF(absposold, absposnew) + abspos[0];
    abspos[1] = min(absendold, absendnew);
    absend[1] = DIFF(absendold, absendnew) + abspos[1];
#ifdef HARD_SEL_DEBUG
    fprintf(stderr,"abspos[0] = %d, absend[0] = %d, abspos[1] = %d, absend[1] = %d\n",abspos[0],absend[0],abspos[1],absend[1]);
    fflush(stderr);
#endif
    i = 0;
    for (x = 0; x < 2; ++x) {
	if (abspos[x] != absend[x]) {
	    int consumed = 0;
	    result[i].left = abspos[x] % canvasColumns;
	    result[i].top = abspos[x] / canvasColumns;
	    result[i].bottom = result[i].top + 1;
	    if ((absend[x] - abspos[x]) + result[i].left < canvasColumns) {
#ifdef HARD_SEL_DEBUG
		fprintf(stderr,"Nowrap, %d < canvasColumns\n",
			(absend[x] - abspos[x]) + result[i].left);
		fflush(stderr);
#endif
		result[i].right = (absend[x] - abspos[x]) + result[i].left;
		consumed += result[i].right - result[i].left; 
	    } else {
#ifdef HARD_SEL_DEBUG
		fprintf(stderr,"Wrap, %d >= canvasColumns\n",
			(absend[x] - abspos[x]) + result[i].left);
		fflush(stderr);
#endif
		result[i].right = canvasColumns;
		consumed += result[i].right - result[i].left;
		if (absend[x] - abspos[x] - consumed >= canvasColumns) {
		    ++i;
		    result[i].top = result[i-1].bottom;
		    result[i].left = 0;
		    result[i].right = canvasColumns;
		    result[i].bottom = (absend[x] - abspos[x] - consumed) / canvasColumns + result[i].top;
		    consumed += (result[i].bottom - result[i].top) * canvasColumns;
		}
		if (absend[x] - abspos[x] - consumed > 0) {
		    ++i;
		    result[i].top = result[i-1].bottom;
		    result[i].bottom = result[i].top + 1;
		    result[i].left = 0;
		    result[i].right = absend[x] - abspos[x] - consumed;
		}
	    }
	    ++i;
	}
    }
#ifdef HARD_SEL_DEBUG
    if (i > 2) {
	int x;
	fprintf(stderr,"i = %d\n",i);
	fflush(stderr);
	for (x = 0; x < i; ++x) {
	    fprintf(stderr, "result[%d]: top = %d, left = %d, "
		    "bottom = %d. right = %d\n",
		    x, result[x].top, result[x].left,
		    result[x].bottom, result[x].right);
	}
    }
#endif
    return i;
}
    


static void calc_sel_area(RECT rects[3], POINT beg, POINT end) 
{
    /* These are not really rects and points, these are character
       based positions, need to be multiplied by cxChar and cyChar to
       make up canvas coordinates */
    memset(rects,0,3*sizeof(RECT));
    rects[0].left = beg.x;
    rects[0].top = beg.y;
    rects[0].bottom = beg.y+1;
    if (end.y - beg.y == 1) { /* Only one row */
	rects[0].right = end.x;
	goto out;
    }
    rects[0].right = canvasColumns;
    if (end.y - beg.y > 2) { 
	rects[1].left = 0;
	rects[1].top = rects[0].bottom;
	rects[1].right = canvasColumns;
	rects[1].bottom = end.y - 1;
    }
    rects[2].left = 0;
    rects[2].top = end.y - 1;
    rects[2].bottom = end.y;
    rects[2].right = end.x;

 out:
#ifdef HARD_SEL_DEBUG
    {
	int i;
	fprintf(stderr,"beg.x = %d, beg.y = %d, end.x = %d, end.y = %d\n",
		beg.x,beg.y,end.x,end.y);
	for (i = 0; i < 3; ++i) {
	    fprintf(stderr,"[%d] left = %d, top = %d, "
		    "right = %d, bottom = %d\n",
		    i, rects[i].left, rects[i].top, 
		    rects[i].right, rects[i].bottom);
	}
	fflush(stderr);
    }
#endif
    return;
}

static void calc_sel_area_turned(RECT rects[3], POINT eBeg, POINT eEnd) {
    POINT from,to;
    if (eBeg.y >=  eEnd.y || 
	(eBeg.y == eEnd.y - 1 && eBeg.x > eEnd.x)) {
#ifdef HARD_SEL_DEBUG
	fprintf(stderr,"Reverting (Beg.x = %d, Beg.y = %d, " 
		"End.x = %d, End.y = %d)\n",eBeg.x,eBeg.y,
		eEnd.x,eEnd.y);
	fflush(stderr);
#endif
	from.x = eEnd.x;
	from.y = eEnd.y - 1;
	to.x = eBeg.x;
	to.y = eBeg.y + 1; 
	calc_sel_area(rects,from,to);
    } else {
	calc_sel_area(rects,eBeg,eEnd);
    }
}
 

static void InvertSelectionArea(HWND hwnd)
{
    RECT rects[3];
    POINT from,to;
    int i;
    calc_sel_area_turned(rects,editBeg,editEnd);
    for (i = 0; i < 3; ++i) {
	if (!EMPTY_RECT(rects[i])) {
	    from.x = rects[i].left;
	    to.x = rects[i].right;
	    from.y = rects[i].top;
	    to.y = rects[i].bottom;
	    DrawSelection(hwnd,from,to);
	}
    }
}

static void
Client_OnMouseMove(HWND hwnd, int x, int y, UINT keyFlags)
{
    if (fSelecting) {
	RECT rold[3], rnew[3], rupdate[6];
	int num_updates,i,r;
	POINT from,to;
	calc_sel_area_turned(rold,editBeg,editEnd);

	calc_charpoint_from_point(GetDC(hwnd), x, y, 1, &editEnd);

	calc_sel_area_turned(rnew,editBeg,editEnd);
	num_updates = diff_sel_area(rold,rnew,rupdate);
	for (i = 0; i < num_updates;++i) {
	    from.x = rupdate[i].left;
	    to.x = rupdate[i].right;
	    from.y = rupdate[i].top;
	    to.y = rupdate[i].bottom;
#ifdef HARD_SEL_DEBUG
	    fprintf(stderr,"from: x=%d,y=%d, to: x=%d, y=%d\n",
		    from.x, from.y,to.x,to.y);
	    fflush(stderr);
#endif
	    DrawSelection(hwnd,from,to);
	}
    }
}

static void
Client_OnVScroll(HWND hwnd, HWND hwndCtl, UINT code, int pos)
{
    int iVscroll;

    switch(code) {
    case SB_LINEDOWN:
	iVscroll = 1;
	break;
    case SB_LINEUP:
	iVscroll = -1;
	break;
    case SB_PAGEDOWN:
	iVscroll = max(1, cyClient/cyChar);
	break;
    case SB_PAGEUP:
	iVscroll = min(-1, -cyClient/cyChar);
	break;
    case SB_THUMBTRACK:
	iVscroll = pos - iVscrollPos;
	break;
    default:
	iVscroll = 0;
    }
    iVscroll = max(-iVscrollPos, min(iVscroll, iVscrollMax-iVscrollPos));
    if (iVscroll != 0) {
	iVscrollPos += iVscroll;
	ScrollWindowEx(hwnd, 0, -cyChar*iVscroll, NULL, NULL,
		       NULL, NULL, SW_ERASE | SW_INVALIDATE);
	SetScrollPos(hwnd, SB_VERT, iVscrollPos, TRUE);
	iVscroll = GetScrollPos(hwnd, SB_VERT);
	UpdateWindow(hwnd);
    }
}

static void
Client_OnHScroll(HWND hwnd, HWND hwndCtl, UINT code, int pos)
{
    int iHscroll, curCharWidth = cxClient/cxChar;

    switch(code) {
    case SB_LINEDOWN:
	iHscroll = 1;
	break;
    case SB_LINEUP:
	iHscroll = -1;
	break;
    case SB_PAGEDOWN:
	iHscroll = max(1,curCharWidth-1);
	break;
    case SB_PAGEUP:
	iHscroll = min(-1,-(curCharWidth-1));
	break;
    case SB_THUMBTRACK:
	iHscroll = pos - iHscrollPos;
	break;
    default:
	iHscroll = 0;
    }
    iHscroll = max(-iHscrollPos, min(iHscroll, iHscrollMax-iHscrollPos-(curCharWidth-1)));
    if (iHscroll != 0) {
	iHscrollPos += iHscroll;
	ScrollWindow(hwnd, -cxChar*iHscroll, 0, NULL, NULL);
	SetScrollPos(hwnd, SB_HORZ, iHscrollPos, TRUE);
	UpdateWindow(hwnd);
    }
}

static LRESULT CALLBACK
ClientWndProc(HWND hwnd, UINT iMsg, WPARAM wParam, LPARAM lParam)
{
    switch (iMsg) {
	HANDLE_MSG(hwnd, WM_CREATE, Client_OnCreate);
	HANDLE_MSG(hwnd, WM_SIZE, Client_OnSize);
	HANDLE_MSG(hwnd, WM_PAINT, Client_OnPaint);
	HANDLE_MSG(hwnd, WM_LBUTTONDOWN, Client_OnLButtonDown);
	HANDLE_MSG(hwnd, WM_RBUTTONDOWN, Client_OnRButtonDown);
	HANDLE_MSG(hwnd, WM_LBUTTONUP, Client_OnLButtonUp);
	HANDLE_MSG(hwnd, WM_MOUSEMOVE, Client_OnMouseMove);
	HANDLE_MSG(hwnd, WM_VSCROLL, Client_OnVScroll);
	HANDLE_MSG(hwnd, WM_HSCROLL, Client_OnHScroll);
    case WM_CONBEEP:
        if (0) Beep(440, 400);
        return 0;
    case WM_CONTEXT:
        ConDrawText(hwnd);
        return 0;
    case WM_CLOSE:
        break;
    case WM_DESTROY:
        PostQuitMessage(0);
        return 0;
    }
    return DefWindowProc (hwnd, iMsg, wParam, lParam);
}

static void
LoadUserPreferences(void) 
{
    DWORD size;
    DWORD res;
    DWORD type;

    /* default prefs */
    GetObject(GetStockObject(SYSTEM_FIXED_FONT),sizeof(LOGFONT),(PSTR)&logfont);
    fgColor = GetSysColor(COLOR_WINDOWTEXT);
    bkgColor = GetSysColor(COLOR_WINDOW);
    winPos.left = -1;
    toolbarVisible = TRUE;

    if (RegCreateKeyEx(HKEY_CURRENT_USER, USER_KEY, 0, 0,
		       REG_OPTION_NON_VOLATILE, KEY_ALL_ACCESS, NULL,
		       &key, &res) != ERROR_SUCCESS)
        return;
    has_key = TRUE;
    if (res == REG_CREATED_NEW_KEY)
	return;
    size = sizeof(logfont);
    res = RegQueryValueEx(key,TEXT("Font"),NULL,&type,(LPBYTE)&logfont,&size);
    size = sizeof(fgColor);
    res = RegQueryValueEx(key,TEXT("FgColor"),NULL,&type,(LPBYTE)&fgColor,&size);
    size = sizeof(bkgColor);
    res = RegQueryValueEx(key,TEXT("BkColor"),NULL,&type,(LPBYTE)&bkgColor,&size);
    size = sizeof(winPos);
    res = RegQueryValueEx(key,TEXT("Pos"),NULL,&type,(LPBYTE)&winPos,&size);
    size = sizeof(toolbarVisible);
    res = RegQueryValueEx(key,TEXT("Toolbar"),NULL,&type,(LPBYTE)&toolbarVisible,&size);
}

static void
SaveUserPreferences(void)
{  
    WINDOWPLACEMENT wndPlace;

    if (has_key == TRUE) {
        RegSetValueEx(key,TEXT("Font"),0,REG_BINARY,(CONST BYTE *)&logfont,sizeof(LOGFONT));
        RegSetValueEx(key,TEXT("FgColor"),0,REG_DWORD,(CONST BYTE *)&fgColor,sizeof(fgColor));
        RegSetValueEx(key,TEXT("BkColor"),0,REG_DWORD,(CONST BYTE *)&bkgColor,sizeof(bkgColor));
        RegSetValueEx(key,TEXT("Toolbar"),0,REG_DWORD,(CONST BYTE *)&toolbarVisible,sizeof(toolbarVisible));

	wndPlace.length = sizeof(WINDOWPLACEMENT);
	GetWindowPlacement(hFrameWnd,&wndPlace);
	/* If wndPlace.showCmd == SW_MINIMIZE, then the window is minimized.
	   We don't care, wndPlace.rcNormalPosition always holds the last known position. */
	winPos = wndPlace.rcNormalPosition;
	RegSetValueEx(key,TEXT("Pos"),0,REG_BINARY,(CONST BYTE *)&winPos,sizeof(winPos));
    }
}


static void
set_scroll_info(HWND hwnd)
{
    SCROLLINFO info;
    int hScrollBy;
    /*
     * Set vertical scrolling range and scroll box position.
     */

    iVscrollMax = nBufLines-1;
    iVscrollPos = min(iVscrollPos, iVscrollMax);
    info.cbSize = sizeof(info); 
    info.fMask = SIF_PAGE|SIF_RANGE|SIF_POS;
    info.nMin = 0;
    info.nPos = iVscrollPos; 
    info.nPage = min(cyClient/cyChar, iVscrollMax);
    info.nMax = iVscrollMax;
    SetScrollInfo(hwnd, SB_VERT, &info, TRUE);

    /*
     * Set horizontal scrolling range and scroll box position.
     */ 

    iHscrollMax = LINE_LENGTH-1;
    hScrollBy = max(0, (iHscrollPos - (iHscrollMax-cxClient/cxChar))*cxChar);
    iHscrollPos = min(iHscrollPos, iHscrollMax);
    info.nPos = iHscrollPos; 
    info.nPage = cxClient/cxChar;
    info.nMax = iHscrollMax;
    SetScrollInfo(hwnd, SB_HORZ, &info, TRUE);
    /*ScrollWindow(hwnd, hScrollBy, 0, NULL, NULL);*/
}


static void
ensure_line_below(void)
{
    if (cur_line->next == NULL) {
	if (nBufLines >= lines_to_save) {
	    ScreenLine_t* pLine = buffer_top->next;
	    FREE(buffer_top->text);
	    FREE(buffer_top);
	    buffer_top = pLine;
	    buffer_top->prev = NULL;
	    nBufLines--;
	}
	cur_line->next = ConNewLine();
	cur_line->next->prev = cur_line;
	buffer_bottom = cur_line->next;
	set_scroll_info(hClientWnd);
    }
}

static ScreenLine_t*
ConNewLine(void)
{
    ScreenLine_t *pLine;

    pLine = (ScreenLine_t *)ALLOC(sizeof(ScreenLine_t));
    if (!pLine)
	return NULL;
    pLine->text = (TCHAR *) ALLOC(canvasColumns * sizeof(TCHAR));
#ifdef HARDDEBUG
    pLine->allocated = canvasColumns;
#endif
    pLine->width = 0;
    pLine->prev = pLine->next = NULL;
    pLine->newline = 0;
    nBufLines++;
    return pLine;
}

static ScreenLine_t*
GetLineFromY(int y)
{
    ScreenLine_t *pLine = buffer_top;
    int i;

    for (i = 0; i < nBufLines && pLine != NULL; i++) {
        if (i == y)
	    return pLine;
        pLine  = pLine->next;        
    }
    return NULL;
}    

void ConCarriageFeed(int hard_newline)
{
    cur_x = 0;
    ensure_line_below();
    cur_line->newline = hard_newline;
    cur_line = cur_line->next;
    if (cur_y < nBufLines-1) {
	cur_y++;
    } else if (iVscrollPos > 0) {
	iVscrollPos--;
    }
}

/*
 * Scroll screen if cursor is not visible.
 */
static void
ConScrollScreen(void)
{
    if (cur_y >= iVscrollPos + cyClient/cyChar) {
	int iVscroll;

	iVscroll = cur_y - iVscrollPos - cyClient/cyChar + 1;
	iVscrollPos += iVscroll;
	ScrollWindowEx(hClientWnd, 0, -cyChar*iVscroll, NULL, NULL,
		     NULL, NULL, SW_ERASE | SW_INVALIDATE);
	SetScrollPos(hClientWnd, SB_VERT, iVscrollPos, TRUE);
	UpdateWindow(hClientWnd);
    }
}

static void
DrawSelection(HWND hwnd, POINT pt1, POINT pt2)
{
    HDC hdc;
    int width,height;
#ifdef HARD_SEL_DEBUG
    fprintf(stderr,"pt1.x = %d, pt1.y = %d, pt2.x = %d, pt2.y = %d\n",
	    (int) pt1.x, (int) pt1.y, (int) pt2.x, (int) pt2.y);
#endif
    pt1.x = GetXFromLine(GetDC(hwnd),iHscrollPos,pt1.x,GetLineFromY(pt1.y));
    pt2.x = GetXFromLine(GetDC(hwnd),iHscrollPos,pt2.x,GetLineFromY(pt2.y-1));
    pt1.y -= iVscrollPos;
    pt2.y -= iVscrollPos;
    pt1.y *= cyChar;
    pt2.y *= cyChar;
#ifdef HARD_SEL_DEBUG
    fprintf(stderr,"pt1.x = %d, pt1.y = %d, pt2.x = %d, pt2.y = %d\n",
	    (int) pt1.x, (int) pt1.y, (int) pt2.x, (int) pt2.y);
    fflush(stderr);
#endif
    width = pt2.x-pt1.x;
    height = pt2.y - pt1.y;
    hdc = GetDC(hwnd);
    PatBlt(hdc,pt1.x,pt1.y,width,height,DSTINVERT);
    ReleaseDC(hwnd,hdc);
}

static void
OnEditCopy(HWND hwnd)
{
    HGLOBAL hMem;
    TCHAR *pMem;
    ScreenLine_t *pLine;
    RECT rects[3];
    POINT from,to;
    int i,j,sum,len;
    if (editBeg.y >=  editEnd.y || 
	(editBeg.y == editEnd.y - 1 && editBeg.x > editEnd.x)) {
#ifdef HARD_SEL_DEBUG
	fprintf(stderr,"CopyReverting (Beg.x = %d, Beg.y = %d, " 
		"End.x = %d, End.y = %d)\n",editBeg.x,editBeg.y,
		editEnd.x,editEnd.y);
	fflush(stderr);
#endif
	from.x = editEnd.x;
	from.y = editEnd.y - 1;
	to.x = editBeg.x;
	to.y = editBeg.y + 1; 
	calc_sel_area(rects,from,to);
    } else {
	calc_sel_area(rects,editBeg,editEnd);
    }
    sum = 1;
    for (i = 0; i < 3; ++i) {
	if (!EMPTY_RECT(rects[i])) {
	    pLine = GetLineFromY(rects[i].top);
	    for (j = rects[i].top; j < rects[i].bottom ;++j) {
		if (pLine == NULL) {
		    sum += 2;
		    break;
		}
		if (pLine->width > rects[i].left) {
		    sum += (pLine->width < rects[i].right) ?
			pLine->width -  rects[i].left : 
			rects[i].right - rects[i].left;
		}
		if(pLine->newline && rects[i].right >= pLine->width) {
		    sum += 2;
		}
		pLine = pLine->next;
	    }
	}
    }
#ifdef HARD_SEL_DEBUG
    fprintf(stderr,"sum = %d\n",sum);
    fflush(stderr);
#endif
    hMem = GlobalAlloc(GHND, sum * sizeof(TCHAR));
    pMem = GlobalLock(hMem);
    for (i = 0; i < 3; ++i) {
	if (!EMPTY_RECT(rects[i])) {
	    pLine = GetLineFromY(rects[i].top);
	    for (j = rects[i].top; j < rects[i].bottom; ++j) {
		if (pLine == NULL) {
		    memcpy(pMem,TEXT("\r\n"),2 * sizeof(TCHAR));
		    pMem += 2;
		    break;
		}
		if (pLine->width > rects[i].left) {
		    len = (pLine->width < rects[i].right) ?
			pLine->width -  rects[i].left : 
			rects[i].right - rects[i].left;
		    memcpy(pMem,pLine->text + rects[i].left,len * sizeof(TCHAR));
		    pMem +=len;
		}
		if(pLine->newline && rects[i].right >= pLine->width) {
		    memcpy(pMem,TEXT("\r\n"),2 * sizeof(TCHAR));
		    pMem += 2;
		}
		pLine = pLine->next;
	    }
	}
    }
    *pMem = TEXT('\0');
    /* Flash de selection area to give user feedback about copying */
    InvertSelectionArea(hwnd);
    Sleep(100);
    InvertSelectionArea(hwnd);

    OpenClipboard(hwnd);
    EmptyClipboard();
    GlobalUnlock(hMem);
    SetClipboardData(CF_UNICODETEXT,hMem);
    CloseClipboard();
}

/* XXX:PaN Tchar or char? */
static void
OnEditPaste(HWND hwnd)
{
    HANDLE hClipMem;
    TCHAR *pClipMem,*pMem,*pMem2;
    if (!OpenClipboard(hwnd))
	return;
    if ((hClipMem = GetClipboardData(CF_UNICODETEXT)) != NULL) {
        pClipMem = GlobalLock(hClipMem);
        pMem = (TCHAR *)ALLOC(GlobalSize(hClipMem) * sizeof(TCHAR));
        pMem2 = pMem;
        while ((*pMem2 = *pClipMem) != TEXT('\0')) {
            if (*pClipMem == TEXT('\r'))
                *pMem2 = TEXT('\n');
            ++pMem2;
	    ++pClipMem;
        }
        GlobalUnlock(hClipMem);
        write_inbuf(pMem, _tcsclen(pMem));
    }
    CloseClipboard();
}

static void
OnEditSelAll(HWND hwnd)
{
    editBeg.x = 0;
    editBeg.y = 0;
    editEnd.x = LINE_LENGTH-1;
    editEnd.y = cur_y;
    fTextSelected = TRUE;
    InvalidateRect(hwnd, NULL, TRUE);
}

CF_HOOK_RET APIENTRY CFHookProc(HWND hDlg,UINT iMsg,WPARAM wParam,LPARAM lParam)
{
    /* Hook procedure for font dialog box */
    HWND hOwner;
    RECT rc,rcOwner,rcDlg;
    switch (iMsg) {
    case WM_INITDIALOG:
        /* center dialogbox within its owner window */
        if ((hOwner = GetParent(hDlg)) == NULL) 
            hOwner = GetDesktopWindow(); 
        GetWindowRect(hOwner, &rcOwner); 
        GetWindowRect(hDlg, &rcDlg); 
        CopyRect(&rc, &rcOwner); 
        OffsetRect(&rcDlg, -rcDlg.left, -rcDlg.top); 
        OffsetRect(&rc, -rc.left, -rc.top); 
        OffsetRect(&rc, -rcDlg.right, -rcDlg.bottom); 
        SetWindowPos(hDlg,HWND_TOP,rcOwner.left + (rc.right / 2), 
		     rcOwner.top + (rc.bottom / 2),0,0,SWP_NOSIZE); 
        return (CF_HOOK_RET) 1;
    default:
        break;
    }
    return (CF_HOOK_RET) 0; /* Let the default procedure process the message */
}

static BOOL
ConChooseFont(HWND hwnd)
{
    HDC hdc;
    hdc = GetDC(hwnd);
    cf.lStructSize = sizeof(CHOOSEFONT);
    cf.hwndOwner = hwnd;
    cf.hDC = NULL;
    cf.lpLogFont = &logfont;
    cf.iPointSize = 0;
    cf.Flags = CF_INITTOLOGFONTSTRUCT|CF_SCREENFONTS|CF_FIXEDPITCHONLY|CF_EFFECTS|CF_ENABLEHOOK;
    cf.rgbColors = GetTextColor(hdc);
    cf.lCustData = 0L;
    cf.lpfnHook = CFHookProc;
    cf.lpTemplateName = NULL;
    cf.hInstance = NULL;
    cf.lpszStyle = NULL;
    cf.nFontType = 0;
    cf.nSizeMin = 0;
    cf.nSizeMax = 0;
    ReleaseDC(hwnd,hdc);
    return ChooseFont(&cf);
}

static void
ConFontInitialize(HWND hwnd)
{
    HDC hdc;
    TEXTMETRIC tm;
    HFONT hFont;

    hFont = CreateFontIndirect(&logfont);
    hdc = GetDC(hwnd);
    SelectObject(hdc, hFont); 
    SetTextColor(hdc,fgColor);
    SetBkColor(hdc,bkgColor);
    GetTextMetrics(hdc, &tm);
    cxChar = tm.tmAveCharWidth;
    cxCharMax = tm.tmMaxCharWidth;
    cyChar = tm.tmHeight + tm.tmExternalLeading;
    ReleaseDC(hwnd, hdc);
}

static void
ConSetFont(HWND hwnd)
{
    HDC hdc;
    TEXTMETRIC tm;
    HFONT hFontNew;
	
    hFontNew = CreateFontIndirect(&logfont); 
    SendMessage(hComboWnd,WM_SETFONT,(WPARAM)hFontNew,
		MAKELPARAM(1,0));
    hdc = GetDC(hwnd);
    DeleteObject(SelectObject(hdc, hFontNew));
    GetTextMetrics(hdc, &tm);
    cxChar = tm.tmAveCharWidth;
    cxCharMax = tm.tmMaxCharWidth;
    cyChar = tm.tmHeight + tm.tmExternalLeading;
    fgColor = cf.rgbColors;
    SetTextColor(hdc,fgColor);
    ReleaseDC(hwnd, hdc);
    set_scroll_info(hwnd);
    HideCaret(hwnd);
    if (DestroyCaret()) {
        CreateCaret(hwnd, NULL, cxChar, cyChar);
        SetCaretPos(GetXFromCurrentY(hdc,iHscrollPos,cur_x), (cur_y-iVscrollPos)*cyChar); 
    }
    ShowCaret(hwnd);
    InvalidateRect(hwnd, NULL, TRUE);
}

CC_HOOK_RET APIENTRY
CCHookProc(HWND hDlg,UINT iMsg,WPARAM wParam,LPARAM lParam)
{
    /* Hook procedure for choose color dialog box */
    HWND hOwner;
    RECT rc,rcOwner,rcDlg;
    switch (iMsg) {
    case WM_INITDIALOG:
        /* center dialogbox within its owner window */
        if ((hOwner = GetParent(hDlg)) == NULL) 
            hOwner = GetDesktopWindow(); 
        GetWindowRect(hOwner, &rcOwner); 
        GetWindowRect(hDlg, &rcDlg); 
        CopyRect(&rc, &rcOwner); 
        OffsetRect(&rcDlg, -rcDlg.left, -rcDlg.top); 
        OffsetRect(&rc, -rc.left, -rc.top); 
        OffsetRect(&rc, -rcDlg.right, -rcDlg.bottom); 
        SetWindowPos(hDlg,HWND_TOP,rcOwner.left + (rc.right / 2), 
		     rcOwner.top + (rc.bottom / 2),0,0,SWP_NOSIZE); 
        return (CC_HOOK_RET) 1;
    default:
        break;
    }
    return (CC_HOOK_RET) 0;			/* Let the default procedure process the message */
}

void ConChooseColor(HWND hwnd)
{
    CHOOSECOLOR cc;                 
    static COLORREF acrCustClr[16];                      
    HBRUSH hbrush;
    HDC hdc;

    /* Initialize CHOOSECOLOR */
    ZeroMemory(&cc, sizeof(CHOOSECOLOR));
    cc.lStructSize = sizeof(CHOOSECOLOR);
    cc.hwndOwner = hwnd;
    cc.lpCustColors = (LPDWORD) acrCustClr;
    cc.rgbResult = bkgColor;
    cc.lpfnHook = CCHookProc;
    cc.Flags = CC_FULLOPEN|CC_RGBINIT|CC_SOLIDCOLOR|CC_ENABLEHOOK;
 
    if (ChooseColor(&cc)==TRUE) {
        bkgColor = cc.rgbResult;
        hdc = GetDC(hwnd);
        SetBkColor(hdc,bkgColor);
        ReleaseDC(hwnd,hdc);
        hbrush = CreateSolidBrush(bkgColor);
        DeleteObject((HBRUSH)SetClassLong(hClientWnd,GCL_HBRBACKGROUND,(LONG)hbrush));
        InvalidateRect(hwnd,NULL,TRUE);		
    }    
}

OFN_HOOK_RET APIENTRY OFNHookProc(HWND hwndDlg,UINT iMsg,
				  WPARAM wParam,LPARAM lParam)
{
    /* Hook procedure for open file dialog box */
    HWND hOwner,hDlg;
    RECT rc,rcOwner,rcDlg;
    hDlg = GetParent(hwndDlg);
    switch (iMsg) {
    case WM_INITDIALOG:
        /* center dialogbox within its owner window */
        if ((hOwner = GetParent(hDlg)) == NULL) 
            hOwner = GetDesktopWindow(); 
        GetWindowRect(hOwner, &rcOwner); 
        GetWindowRect(hDlg, &rcDlg); 
        CopyRect(&rc, &rcOwner); 
        OffsetRect(&rcDlg, -rcDlg.left, -rcDlg.top); 
        OffsetRect(&rc, -rc.left, -rc.top); 
        OffsetRect(&rc, -rcDlg.right, -rcDlg.bottom); 
        SetWindowPos(hDlg,HWND_TOP,rcOwner.left + (rc.right / 2), 
		     rcOwner.top + (rc.bottom / 2),0,0,SWP_NOSIZE); 
        return (OFN_HOOK_RET) 1;
    default:
        break;
    }
    return (OFN_HOOK_RET) 0; /* the let default procedure process the message */
}

static void
GetFileName(HWND hwnd, TCHAR *pFile)
{
    /* Open the File Open dialog box and */
    /* retrieve the file name            */
    OPENFILENAME ofn;
    TCHAR szFilterSpec [128] = TEXT("logfiles (*.log)\0*.log\0All files (*.*)\0*.*\0\0");
    #define MAXFILENAME 256
    TCHAR szFileName[MAXFILENAME];
    TCHAR szFileTitle[MAXFILENAME];

    /* these need to be filled in */
    _tcscpy(szFileName, TEXT("erlshell.log"));   
    _tcscpy(szFileTitle, TEXT("")); /* must be NULL */

    ofn.lStructSize       = sizeof(OPENFILENAME);
    ofn.hwndOwner         = NULL;
    ofn.lpstrFilter       = szFilterSpec;
    ofn.lpstrCustomFilter = NULL;
    ofn.nMaxCustFilter    = 0;
    ofn.nFilterIndex      = 0;
    ofn.lpstrFile         = szFileName;
    ofn.nMaxFile          = MAXFILENAME;
    ofn.lpstrInitialDir   = NULL;
    ofn.lpstrFileTitle    = szFileTitle;
    ofn.nMaxFileTitle     = MAXFILENAME;
    ofn.lpstrTitle        = TEXT("Open logfile");
    ofn.lpstrDefExt       = TEXT("log");
    ofn.Flags             = OFN_CREATEPROMPT|OFN_HIDEREADONLY|OFN_EXPLORER|OFN_ENABLEHOOK|OFN_NOCHANGEDIR; /* OFN_NOCHANGEDIR only works in Vista :( */
    ofn.lpfnHook          = OFNHookProc;
   
    if (!GetOpenFileName ((LPOPENFILENAME)&ofn)){
        *pFile = TEXT('\0');
    } else {
        _tcscpy(pFile, ofn.lpstrFile);
    }
}

void OpenLogFile(HWND hwnd)
{
    /* open a file for logging */
    TCHAR filename[_MAX_PATH];

    GetFileName(hwnd, filename);
    if (filename[0] == '\0')
        return;
    if (NULL == (logfile = _tfopen(filename,TEXT("w,ccs=UNICODE"))))
        return;
}

void CloseLogFile(HWND hwnd)
{
    /* close log file */
    fclose(logfile);
    logfile = NULL;
}

void LogFileWrite(TCHAR *buf, int num_chars)
{
    /* write to logfile */
    int from,to;
    while (num_chars-- > 0) {     
        switch (*buf) {
        case SET_CURSOR:
            buf++;
            from = *((int *)buf);
            buf += sizeof(int)/sizeof(TCHAR);
            to = *((int *)buf);
            buf += (sizeof(int)/sizeof(TCHAR))-1;
            num_chars -= 2 * (sizeof(int)/sizeof(TCHAR));
	    // Wont seek in Unicode file, sorry...
            // fseek(logfile,to-from *sizeof(TCHAR),SEEK_CUR);
            break;
        default:
            _fputtc(*buf,logfile);
            break;
        }
        buf++;
    }
}

static void
init_buffers(void)
{
    inbuf.data = (TCHAR *) ALLOC(BUFSIZE * sizeof(TCHAR));
    outbuf.data = (TCHAR *) ALLOC(BUFSIZE * sizeof(TCHAR));
    inbuf.size = BUFSIZE;
    inbuf.rdPos = inbuf.wrPos = 0;
    outbuf.size = BUFSIZE;
    outbuf.rdPos = outbuf.wrPos = 0;
}

static int
check_realloc(buffer_t *buf, int num_chars)
{
    if (buf->wrPos + num_chars >= buf->size) {
	if (buf->size > MAXBUFSIZE)
	    return 0;
	buf->size += num_chars + BUFSIZE;
	if (!(buf->data = (TCHAR *)REALLOC(buf->data, buf->size * sizeof(TCHAR)))) {
	    buf->size = buf->rdPos = buf->wrPos = 0;
	    return 0;
        }
    }
    return 1;
}

static int
write_inbuf(TCHAR *data, int num_chars)
{
    TCHAR *buf;
    int nwrite;
    WaitForSingleObject(console_input,INFINITE);
    if (!check_realloc(&inbuf,num_chars)) {
        ReleaseSemaphore(console_input,1,NULL);
        return -1;
    }
    buf = &inbuf.data[inbuf.wrPos];
    inbuf.wrPos += num_chars; 
    nwrite = num_chars;
    while (nwrite--) 
        *buf++ = *data++;
    SetEvent(console_input_event);
    ReleaseSemaphore(console_input,1,NULL);
    return num_chars;
}

static int 
write_outbuf(TCHAR *data, int num_chars)
{
    TCHAR *buf;
    int nwrite;

    WaitForSingleObject(console_output,INFINITE);
    if (!check_realloc(&outbuf, num_chars)) {
        ReleaseSemaphore(console_output,1,NULL);
        return -1;
    }
    if (outbuf.rdPos == outbuf.wrPos)
        PostMessage(hClientWnd, WM_CONTEXT, 0L, 0L);
    buf = &outbuf.data[outbuf.wrPos];
    outbuf.wrPos += num_chars; 
    nwrite = num_chars;
    while (nwrite--) 
        *buf++ = *data++;
    ReleaseSemaphore(console_output,1,NULL);
    return num_chars;
}

DIALOG_PROC_RET CALLBACK AboutDlgProc(HWND hDlg, UINT iMsg, WPARAM wParam, LPARAM lParam)
{
    HWND hOwner;
    RECT rc,rcOwner,rcDlg;

    switch (iMsg) {
    case WM_INITDIALOG:
        /* center dialogbox within its owner window */
        if ((hOwner = GetParent(hDlg)) == NULL) 
            hOwner = GetDesktopWindow(); 
        GetWindowRect(hOwner, &rcOwner); 
        GetWindowRect(hDlg, &rcDlg); 
        CopyRect(&rc, &rcOwner); 
        OffsetRect(&rcDlg, -rcDlg.left, -rcDlg.top); 
        OffsetRect(&rc, -rc.left, -rc.top); 
        OffsetRect(&rc, -rcDlg.right, -rcDlg.bottom); 
        SetWindowPos(hDlg,HWND_TOP,rcOwner.left + (rc.right / 2), 
		     rcOwner.top + (rc.bottom / 2),0,0,SWP_NOSIZE); 
        SetDlgItemText(hDlg, ID_VERSIONSTRING,
		       TEXT("Erlang emulator version ") TEXT(ERLANG_VERSION)); 
        return (DIALOG_PROC_RET) TRUE;
    case WM_COMMAND:
        switch (LOWORD(wParam)) {
        case IDOK:
        case IDCANCEL:
            EndDialog(hDlg,0);
            return (DIALOG_PROC_RET) TRUE;
        }
        break;
    }
    return (DIALOG_PROC_RET) FALSE;
}

static void
ConDrawText(HWND hwnd)
{
    int num_chars;
    int nchars;
    TCHAR *buf;
    int from, to;
    int dl;
    int dc;
    RECT rc;

    WaitForSingleObject(console_output, INFINITE);
    nchars = 0;
    num_chars = outbuf.wrPos - outbuf.rdPos;
    buf = &outbuf.data[outbuf.rdPos];
    if (logfile != NULL)
	LogFileWrite(buf, num_chars);


#ifdef HARDDEBUG
    {
	TCHAR *bu = (TCHAR *) ALLOC((num_chars+1) * sizeof(TCHAR));
	memcpy(bu,buf,num_chars * sizeof(TCHAR));
	bu[num_chars]='\0';
	fprintf(stderr,TEXT("ConDrawText\"%s\"\n"),bu);
	FREE(bu);
	fflush(stderr);
    }
#endif
    /*
     * Don't draw any text in the window; just update the line buffers
     * and invalidate the appropriate part of the window.  The window
     * will be updated on the next WM_PAINT message.
     */

    while (num_chars-- > 0) {     
        switch (*buf) {
        case '\r':
            break;
        case '\n':
            if (nchars > 0) {
		rc.left = GetXFromCurrentY(GetDC(hwnd),iHscrollPos,cur_x - nchars);
		rc.right = rc.left + cxCharMax*nchars;
		rc.top = cyChar * (cur_y-iVscrollPos);
		rc.bottom = rc.top + cyChar;
		InvalidateRect(hwnd, &rc, TRUE);
                nchars = 0;
	    }
	    ConCarriageFeed(1);
            ConScrollScreen();
            break;
        case SET_CURSOR:
            if (nchars > 0) { 
		rc.left = GetXFromCurrentY(GetDC(hwnd),iHscrollPos,cur_x - nchars);
		rc.right = rc.left + cxCharMax*nchars;
		rc.top = cyChar * (cur_y-iVscrollPos);
		rc.bottom = rc.top + cyChar;
		InvalidateRect(hwnd, &rc, TRUE);
                nchars = 0;
            }
            buf++;
            from = *((int *)buf);
            buf += sizeof(int)/sizeof(TCHAR);
            to = *((int *)buf);
            buf += (sizeof(int)/sizeof(TCHAR))-1;
            num_chars -= 2 * (sizeof(int)/sizeof(TCHAR));
	    while (to > from) {
		cur_x++;
		if (GetXFromCurrentY(GetDC(hwnd),0,cur_x)+cxChar > 
		    (LINE_LENGTH * cxChar)) {
		    cur_x = 0;
		    cur_y++;
		    ensure_line_below();
		    cur_line = cur_line->next;
		}
		from++;
	    }
	    while (to < from) {
		cur_x--;
		if (cur_x < 0) {
		    cur_y--;
		    cur_line = cur_line->prev;
		    cur_x = cur_line->width-1;
		}
		from--;
	    }
		
            break;
        default:
            nchars++;
	    cur_line->text[cur_x] = *buf;
	    cur_x++;
            if (cur_x > cur_line->width)
		cur_line->width = cur_x; 
	    if (GetXFromCurrentY(GetDC(hwnd),0,cur_x)+cxChar > 
		(LINE_LENGTH * cxChar)) {
                if (nchars > 0) { 
		    rc.left = GetXFromCurrentY(GetDC(hwnd),iHscrollPos,cur_x - nchars);
                    rc.right = rc.left + cxCharMax*nchars;
                    rc.top = cyChar * (cur_y-iVscrollPos);
                    rc.bottom = rc.top + cyChar;
		    InvalidateRect(hwnd, &rc, TRUE);
                }
                ConCarriageFeed(0);
                nchars = 0;
            }
        }
        buf++;
    }
    if (nchars > 0) {
	rc.left = GetXFromCurrentY(GetDC(hwnd),iHscrollPos,cur_x - nchars);
	rc.right = rc.left + cxCharMax*nchars;
	rc.top = cyChar * (cur_y-iVscrollPos);
	rc.bottom = rc.top + cyChar;
	InvalidateRect(hwnd, &rc, TRUE);
    }
    ConScrollScreen();
    SetCaretPos(GetXFromCurrentY(GetDC(hwnd),iHscrollPos,cur_x), (cur_y-iVscrollPos)*cyChar);
    outbuf.wrPos = outbuf.rdPos = 0;
    ReleaseSemaphore(console_output, 1, NULL);
}

static void
AddToCmdHistory(void)
{
    int i;
    int size;
    Uint32 *buf;
    wchar_t cmdBuf[128];

    if (llen != 0) {
	for (i = 0, size = 0; i < llen-1; i++) {
	    /*
	     * Find end of prompt.
	     */
	    if ((lbuf[i] == '>') && lbuf[i+1] == ' ') {
		buf = &lbuf[i+2];
		size = llen-i-2;
		break;
	    }
	}
	if (size > 0 && size < 128) {
	    for (i = 0;i < size; ++i) {
		cmdBuf[i] = (wchar_t) buf[i];
	    }
	    cmdBuf[size] = 0;
	    SendMessage(hComboWnd,CB_INSERTSTRING,0,(LPARAM)cmdBuf);
	}
    }
}

/*static TBBUTTON tbb[] =
{
    0,          0,              TBSTATE_ENABLED, TBSTYLE_SEP,    0, 0, 0, 0,
    0,          0,              TBSTATE_ENABLED, TBSTYLE_SEP,    0, 0, 0, 0,
    0,          0,              TBSTATE_ENABLED, TBSTYLE_SEP,    0, 0, 0, 0,
    0,          0,              TBSTATE_ENABLED, TBSTYLE_SEP,    0, 0, 0, 0,
    0,          0,              TBSTATE_ENABLED, TBSTYLE_SEP,    0, 0, 0, 0,
    0,          0,              TBSTATE_ENABLED, TBSTYLE_SEP,    0, 0, 0, 0,
    0,          0,              TBSTATE_ENABLED, TBSTYLE_SEP,    0, 0, 0, 0,
    0,          0,              TBSTATE_ENABLED, TBSTYLE_SEP,    0, 0, 0, 0,
    0,          0,              TBSTATE_ENABLED, TBSTYLE_SEP,    0, 0, 0, 0,
    0,          0,              TBSTATE_ENABLED, TBSTYLE_SEP,    0, 0, 0, 0,
    0,          0,              TBSTATE_ENABLED, TBSTYLE_SEP,    0, 0, 0, 0,
    0,          0,              TBSTATE_ENABLED, TBSTYLE_SEP,    0, 0, 0, 0,
    0,          0,              TBSTATE_ENABLED, TBSTYLE_SEP,    0, 0, 0, 0,
    0,          0,              TBSTATE_ENABLED, TBSTYLE_SEP,    0, 0, 0, 0,
    0,          0,              TBSTATE_ENABLED, TBSTYLE_SEP,    0, 0, 0, 0,
    0,          0,              TBSTATE_ENABLED, TBSTYLE_SEP,    0, 0, 0, 0,
    0,          0,              TBSTATE_ENABLED, TBSTYLE_SEP,    0, 0, 0, 0,
    0,          0,              TBSTATE_ENABLED, TBSTYLE_SEP,    0, 0, 0, 0,
    0,          0,              TBSTATE_ENABLED, TBSTYLE_SEP,    0, 0, 0, 0,
    0,          0,              TBSTATE_ENABLED, TBSTYLE_SEP,    0, 0, 0, 0,
    0,          0,              TBSTATE_ENABLED, TBSTYLE_SEP,    0, 0, 0, 0,
    0,          0,              TBSTATE_ENABLED, TBSTYLE_SEP,    0, 0, 0, 0,
    0,          0,              TBSTATE_ENABLED, TBSTYLE_SEP,    0, 0, 0, 0,
    0,          0,              TBSTATE_ENABLED, TBSTYLE_SEP,    0, 0, 0, 0,
    0,          0,              TBSTATE_ENABLED, TBSTYLE_SEP,    0, 0, 0, 0,
    0,          IDMENU_COPY,	TBSTATE_ENABLED, TBSTYLE_AUTOSIZE, 0, 0, 0, 0,
    1,	        IDMENU_PASTE,	TBSTATE_ENABLED, TBSTYLE_AUTOSIZE, 0, 0, 0, 0,
    2,	        IDMENU_FONT,	TBSTATE_ENABLED, TBSTYLE_AUTOSIZE, 0, 0, 0, 0,
    3,	        IDMENU_ABOUT,	TBSTATE_ENABLED, TBSTYLE_AUTOSIZE, 0, 0, 0, 0,
    0,          0,              TBSTATE_ENABLED, TBSTYLE_SEP,    0, 0, 0, 0,
    };*/
static TBBUTTON tbb[] =
{
  {0,          0,              TBSTATE_ENABLED, TBSTYLE_SEP},
  {0,          0,              TBSTATE_ENABLED, TBSTYLE_SEP},
  {0,          0,              TBSTATE_ENABLED, TBSTYLE_SEP},
  {0,          0,              TBSTATE_ENABLED, TBSTYLE_SEP},
  {0,          0,              TBSTATE_ENABLED, TBSTYLE_SEP},
  {0,          0,              TBSTATE_ENABLED, TBSTYLE_SEP},
  {0,          0,              TBSTATE_ENABLED, TBSTYLE_SEP},
  {0,          0,              TBSTATE_ENABLED, TBSTYLE_SEP},
  {0,          0,              TBSTATE_ENABLED, TBSTYLE_SEP},
  {0,          0,              TBSTATE_ENABLED, TBSTYLE_SEP},
  {0,          0,              TBSTATE_ENABLED, TBSTYLE_SEP},
  {0,          0,              TBSTATE_ENABLED, TBSTYLE_SEP},
  {0,          0,              TBSTATE_ENABLED, TBSTYLE_SEP},
  {0,          0,              TBSTATE_ENABLED, TBSTYLE_SEP},
  {0,          0,              TBSTATE_ENABLED, TBSTYLE_SEP},
  {0,          0,              TBSTATE_ENABLED, TBSTYLE_SEP},
  {0,          0,              TBSTATE_ENABLED, TBSTYLE_SEP},
  {0,          0,              TBSTATE_ENABLED, TBSTYLE_SEP},
  {0,          0,              TBSTATE_ENABLED, TBSTYLE_SEP},
  {0,          0,              TBSTATE_ENABLED, TBSTYLE_SEP},
  {0,          0,              TBSTATE_ENABLED, TBSTYLE_SEP},
  {0,          0,              TBSTATE_ENABLED, TBSTYLE_SEP},
  {0,          0,              TBSTATE_ENABLED, TBSTYLE_SEP},
  {0,          0,              TBSTATE_ENABLED, TBSTYLE_SEP},
  {0,          0,              TBSTATE_ENABLED, TBSTYLE_SEP},
  {0,          IDMENU_COPY,    TBSTATE_ENABLED, TBSTYLE_AUTOSIZE},
  {1,	       IDMENU_PASTE,   TBSTATE_ENABLED, TBSTYLE_AUTOSIZE},
  {2,	       IDMENU_FONT,    TBSTATE_ENABLED, TBSTYLE_AUTOSIZE},
  {3,	       IDMENU_ABOUT,   TBSTATE_ENABLED, TBSTYLE_AUTOSIZE},
  {0,          0,              TBSTATE_ENABLED, TBSTYLE_SEP} 
};

static TBADDBITMAP tbbitmap =
{
    HINST_COMMCTRL, IDB_STD_SMALL_COLOR,
};

#ifdef HARDDEBUG
/* For really hard GUI startup debugging, place DEBUGBOX() macros in code
   and get modal message boxes with the line number. */
static void debug_box(int line) {
  TCHAR buff[1024];
  swprintf(buff,1024,TEXT("DBG:%d"),line);
  MessageBox(NULL,buff,TEXT("DBG"),MB_OK|MB_APPLMODAL);
}

#define DEBUGBOX() debug_box(__LINE__)  
#endif

static HWND
InitToolBar(HWND hwndParent) 
{ 
    int x,y,cx;
    HWND hwndTB,hwndTT; 
    RECT r;
    TOOLINFO ti;
    HFONT hFontNew;
    DWORD backgroundColor = GetSysColor(COLOR_BTNFACE);
    COLORMAP colorMap;
    colorMap.from = RGB(192, 192, 192);
    colorMap.to = backgroundColor;
    /* Create toolbar window with tooltips */
    hwndTB = CreateWindowEx(0,TOOLBARCLASSNAME,(TCHAR *)NULL,
			    WS_CHILD|CCS_TOP|WS_CLIPSIBLINGS|TBSTYLE_TOOLTIPS,
			    0,0,0,0,hwndParent,
			    (HMENU)2,hInstance,NULL); 
    SendMessage(hwndTB,TB_BUTTONSTRUCTSIZE,
		(WPARAM) sizeof(TBBUTTON),0); 
    tbbitmap.hInst = NULL;
    tbbitmap.nID   = (UINT) CreateMappedBitmap(beam_module, 1,0, &colorMap, 1);
    SendMessage(hwndTB, TB_ADDBITMAP, (WPARAM) 4, 
		(LPARAM) &tbbitmap); 

    SendMessage(hwndTB,TB_ADDBUTTONS, (WPARAM) 30,
		(LPARAM) tbb); 
    if (toolbarVisible)
	ShowWindow(hwndTB, SW_SHOW); 

    /* Create combobox window */
    SendMessage(hwndTB,TB_GETITEMRECT,0,(LPARAM)&r);
    x = r.left; y = r.top;
    SendMessage(hwndTB,TB_GETITEMRECT,23,(LPARAM)&r);
    cx = r.right - x + 1;
    hComboWnd = CreateWindow(TEXT("combobox"),NULL,WS_VSCROLL|WS_CHILD|WS_VISIBLE|CBS_DROPDOWNLIST,
			     x,y,cx,100,hwndParent,(HMENU)ID_COMBOBOX, hInstance,NULL);
    SetParent(hComboWnd,hwndTB);
    hFontNew = CreateFontIndirect(&logfont); 
    SendMessage(hComboWnd,WM_SETFONT,(WPARAM)hFontNew,
		MAKELPARAM(1,0));

    /* Add tooltip for combo box */
    ZeroMemory(&ti,sizeof(TOOLINFO));
    ti.cbSize = sizeof(TOOLINFO);
    ti.uFlags = TTF_IDISHWND|TTF_CENTERTIP|TTF_SUBCLASS;
    ti.hwnd = hwndTB;;
    ti.uId = (UINT)hComboWnd;
    ti.lpszText = LPSTR_TEXTCALLBACK;
    hwndTT = (HWND)SendMessage(hwndTB,TB_GETTOOLTIPS,0,0);
    SendMessage(hwndTT,TTM_ADDTOOL,0,(LPARAM)&ti);

    return hwndTB; 
} 

static void
window_title(struct title_buf *tbuf)
{
    int res, i;
    size_t bufsz = TITLE_BUF_SZ;
    unsigned char charbuff[TITLE_BUF_SZ];

    res = erl_drv_getenv("ERL_WINDOW_TITLE", charbuff, &bufsz);
    if (res < 0)
	tbuf->name = erlang_window_title;
    else if (res == 0) { 
	for (i = 0; i < bufsz; ++i) {
	    tbuf->buf[i] = charbuff[i];
	}
        tbuf->buf[bufsz - 1] = 0;
	tbuf->name = &tbuf->buf[0];
    } else {
	char *buf = ALLOC(bufsz);
	if (!buf)
	    tbuf->name = erlang_window_title;
	else {
	    while (1) {
		char *newbuf;
		res = erl_drv_getenv("ERL_WINDOW_TITLE", buf, &bufsz);
		if (res <= 0) {
		    if (res == 0) {
			TCHAR *wbuf = ALLOC(bufsz *sizeof(TCHAR));
			for (i = 0; i < bufsz ; ++i) {
			    wbuf[i] = buf[i];
			}
			wbuf[bufsz - 1] = 0;
			FREE(buf);
			tbuf->name = wbuf;
		    } else {
			tbuf->name = erlang_window_title;
			FREE(buf);
		    }
		    break;
		}
		newbuf = REALLOC(buf, bufsz);
		if (newbuf)
		    buf = newbuf;
		else {
		    tbuf->name = erlang_window_title;
		    FREE(buf);
		    break;
		}
	    }
	}
    }
}

static void
free_window_title(struct title_buf *tbuf)
{
    if (tbuf->name != erlang_window_title && tbuf->name != &tbuf->buf[0])
	FREE(tbuf->name);
}
