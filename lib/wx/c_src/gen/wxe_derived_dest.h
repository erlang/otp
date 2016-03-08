/*
 * %CopyrightBegin%
 *
 * Copyright Ericsson AB 2008-2016. All Rights Reserved.
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

class EwxWindow : public wxWindow {
 public: ~EwxWindow() {((WxeApp *)wxTheApp)->clearPtr(this);};
 EwxWindow(wxWindow * parent,wxWindowID id,const wxPoint& pos,const wxSize& size,long style) : wxWindow(parent,id,pos,size,style) {};
 EwxWindow() : wxWindow() {};
};

class EwxFrame : public wxFrame {
 public: ~EwxFrame() {((WxeApp *)wxTheApp)->clearPtr(this);};
 EwxFrame(wxWindow * parent,wxWindowID id,const wxString& title,const wxPoint& pos,const wxSize& size,long style) : wxFrame(parent,id,title,pos,size,style) {};
 EwxFrame() : wxFrame() {};
};

class EwxMiniFrame : public wxMiniFrame {
 public: ~EwxMiniFrame() {((WxeApp *)wxTheApp)->clearPtr(this);};
 EwxMiniFrame(wxWindow * parent,wxWindowID id,const wxString& title,const wxPoint& pos,const wxSize& size,long style) : wxMiniFrame(parent,id,title,pos,size,style) {};
 EwxMiniFrame() : wxMiniFrame() {};
};

class EwxSplashScreen : public wxSplashScreen {
 public: ~EwxSplashScreen() {((WxeApp *)wxTheApp)->clearPtr(this);};
 EwxSplashScreen(const wxBitmap& bitmap,long splashStyle,int milliseconds,wxWindow * parent,wxWindowID id,const wxPoint& pos,const wxSize& size,long style) : wxSplashScreen(bitmap,splashStyle,milliseconds,parent,id,pos,size,style) {};
 EwxSplashScreen() : wxSplashScreen() {};
};

class EwxPanel : public wxPanel {
 public: ~EwxPanel() {((WxeApp *)wxTheApp)->clearPtr(this);};
 EwxPanel(wxWindow * parent,int x,int y,int width,int height,long style) : wxPanel(parent,x,y,width,height,style) {};
 EwxPanel(wxWindow * parent,wxWindowID winid,const wxPoint& pos,const wxSize& size,long style) : wxPanel(parent,winid,pos,size,style) {};
 EwxPanel() : wxPanel() {};
};

class EwxScrolledWindow : public wxScrolledWindow {
 public: ~EwxScrolledWindow() {((WxeApp *)wxTheApp)->clearPtr(this);};
 EwxScrolledWindow(wxWindow * parent,wxWindowID winid,const wxPoint& pos,const wxSize& size,long style) : wxScrolledWindow(parent,winid,pos,size,style) {};
 EwxScrolledWindow() : wxScrolledWindow() {};
};

class EwxSashWindow : public wxSashWindow {
 public: ~EwxSashWindow() {((WxeApp *)wxTheApp)->clearPtr(this);};
 EwxSashWindow(wxWindow * parent,wxWindowID id,const wxPoint& pos,const wxSize& size,long style) : wxSashWindow(parent,id,pos,size,style) {};
 EwxSashWindow() : wxSashWindow() {};
};

class EwxSashLayoutWindow : public wxSashLayoutWindow {
 public: ~EwxSashLayoutWindow() {((WxeApp *)wxTheApp)->clearPtr(this);};
 EwxSashLayoutWindow(wxWindow * parent,wxWindowID id,const wxPoint& pos,const wxSize& size,long style) : wxSashLayoutWindow(parent,id,pos,size,style) {};
 EwxSashLayoutWindow() : wxSashLayoutWindow() {};
};

class EwxGrid : public wxGrid {
 public: ~EwxGrid() {((WxeApp *)wxTheApp)->clearPtr(this);};
 EwxGrid(wxWindow * parent,int x,int y,int w,int h,long style) : wxGrid(parent,x,y,w,h,style) {};
 EwxGrid(wxWindow * parent,wxWindowID id,const wxPoint& pos,const wxSize& size,long style) : wxGrid(parent,id,pos,size,style) {};
 EwxGrid() : wxGrid() {};
};

class EwxMirrorDC : public wxMirrorDC {
 public: ~EwxMirrorDC() {((WxeApp *)wxTheApp)->clearPtr(this);};
 EwxMirrorDC(wxDC& dc,bool mirror) : wxMirrorDC(dc,mirror) {};
};

class EwxScreenDC : public wxScreenDC {
 public: ~EwxScreenDC() {((WxeApp *)wxTheApp)->clearPtr(this);};
 EwxScreenDC() : wxScreenDC() {};
};

#if wxUSE_POSTSCRIPT
class EwxPostScriptDC : public wxPostScriptDC {
 public: ~EwxPostScriptDC() {((WxeApp *)wxTheApp)->clearPtr(this);};
 EwxPostScriptDC(const wxPrintData& printData) : wxPostScriptDC(printData) {};
 EwxPostScriptDC() : wxPostScriptDC() {};
};
#endif // wxUSE_POSTSCRIPT

class EwxWindowDC : public wxWindowDC {
 public: ~EwxWindowDC() {((WxeApp *)wxTheApp)->clearPtr(this);};
 EwxWindowDC(wxWindow * win) : wxWindowDC(win) {};
#if !wxCHECK_VERSION(2,9,0)
 EwxWindowDC() : wxWindowDC() {};
#endif
};

class EwxClientDC : public wxClientDC {
 public: ~EwxClientDC() {((WxeApp *)wxTheApp)->clearPtr(this);};
 EwxClientDC(wxWindow * win) : wxClientDC(win) {};
#if !wxCHECK_VERSION(2,9,0)
 EwxClientDC() : wxClientDC() {};
#endif
};

class EwxPaintDC : public wxPaintDC {
 public: ~EwxPaintDC() {((WxeApp *)wxTheApp)->clearPtr(this);};
 EwxPaintDC(wxWindow * win) : wxPaintDC(win) {};
#if !wxCHECK_VERSION(2,9,0)
 EwxPaintDC() : wxPaintDC() {};
#endif
};

class EwxMemoryDC : public wxMemoryDC {
 public: ~EwxMemoryDC() {((WxeApp *)wxTheApp)->clearPtr(this);};
 EwxMemoryDC(wxDC * dc) : wxMemoryDC(dc) {};
 EwxMemoryDC(wxBitmap& dc) : wxMemoryDC(dc) {};
 EwxMemoryDC() : wxMemoryDC() {};
};

class EwxBufferedDC : public wxBufferedDC {
 public: ~EwxBufferedDC() {((WxeApp *)wxTheApp)->clearPtr(this);};
 EwxBufferedDC(wxDC * dc,const wxSize& area,int style) : wxBufferedDC(dc,area,style) {};
 EwxBufferedDC(wxDC * dc,wxBitmap& buffer,int style) : wxBufferedDC(dc,buffer,style) {};
 EwxBufferedDC() : wxBufferedDC() {};
};

class EwxBufferedPaintDC : public wxBufferedPaintDC {
 public: ~EwxBufferedPaintDC() {((WxeApp *)wxTheApp)->clearPtr(this);};
 EwxBufferedPaintDC(wxWindow * window,wxBitmap& buffer,int style) : wxBufferedPaintDC(window,buffer,style) {};
 EwxBufferedPaintDC(wxWindow * window,int style) : wxBufferedPaintDC(window,style) {};
};

#if wxUSE_GRAPHICS_CONTEXT
class EwxGraphicsObject : public wxGraphicsObject {
 public: ~EwxGraphicsObject() {((WxeApp *)wxTheApp)->clearPtr(this);};
};
#endif // wxUSE_GRAPHICS_CONTEXT

#if wxUSE_GRAPHICS_CONTEXT
class EwxGraphicsContext : public wxGraphicsContext {
 public: ~EwxGraphicsContext() {((WxeApp *)wxTheApp)->clearPtr(this);};
};
#endif // wxUSE_GRAPHICS_CONTEXT

class EwxMenuBar : public wxMenuBar {
 public: ~EwxMenuBar() {((WxeApp *)wxTheApp)->clearPtr(this);};
 EwxMenuBar(long style) : wxMenuBar(style) {};
 EwxMenuBar() : wxMenuBar() {};
};

class EwxMenu : public wxMenu {
 public: ~EwxMenu() {((WxeApp *)wxTheApp)->clearPtr(this);};
 EwxMenu(const wxString& title,long style) : wxMenu(title,style) {};
 EwxMenu(long style) : wxMenu(style) {};
};

class EwxMenuItem : public wxMenuItem {
 public: ~EwxMenuItem() {((WxeApp *)wxTheApp)->clearPtr(this);};
 EwxMenuItem(wxMenu * parentMenu,int id,const wxString& text,const wxString& help,wxItemKind kind,wxMenu * subMenu) : wxMenuItem(parentMenu,id,text,help,kind,subMenu) {};
};

class EwxStatusBar : public wxStatusBar {
 public: ~EwxStatusBar() {((WxeApp *)wxTheApp)->clearPtr(this);};
 EwxStatusBar(wxWindow * parent,wxWindowID winid,long style) : wxStatusBar(parent,winid,style) {};
 EwxStatusBar() : wxStatusBar() {};
};

class EwxBitmap : public wxBitmap {
 public: ~EwxBitmap() {((WxeApp *)wxTheApp)->clearPtr(this);};
 EwxBitmap(const char * bits,int width,int height,int depth) : wxBitmap(bits,width,height,depth) {};
 EwxBitmap(int width,int height,int depth) : wxBitmap(width,height,depth) {};
 EwxBitmap(const wxString& filename,wxBitmapType type) : wxBitmap(filename,type) {};
 EwxBitmap(const wxImage& image,int depth) : wxBitmap(image,depth) {};
 EwxBitmap() : wxBitmap() {};
 EwxBitmap(wxBitmap copy) : wxBitmap(copy) {};
};

class EwxIcon : public wxIcon {
 public: ~EwxIcon() {((WxeApp *)wxTheApp)->clearPtr(this);};
 EwxIcon(const wxString& filename,wxBitmapType type,int desiredWidth,int desiredHeight) : wxIcon(filename,type,desiredWidth,desiredHeight) {};
 EwxIcon(const wxIconLocation& loc) : wxIcon(loc) {};
 EwxIcon() : wxIcon() {};
 EwxIcon(wxIcon copy) : wxIcon(copy) {};
};

class EwxCursor : public wxCursor {
 public: ~EwxCursor() {((WxeApp *)wxTheApp)->clearPtr(this);};
#if !wxCHECK_VERSION(2,9,0)
 EwxCursor(const char * bits,int width,int height,int hotSpotX,int hotSpotY) : wxCursor(bits,width,height,hotSpotX,hotSpotY) {};
#endif
 EwxCursor(int cursorId) : wxCursor(cursorId) {};
 EwxCursor(const wxImage& image) : wxCursor(image) {};
 EwxCursor() : wxCursor() {};
};

class EwxMask : public wxMask {
 public: ~EwxMask() {((WxeApp *)wxTheApp)->clearPtr(this);};
 EwxMask(const wxBitmap& bitmap,int paletteIndex) : wxMask(bitmap,paletteIndex) {};
 EwxMask(const wxBitmap& bitmap,const wxColour& colour) : wxMask(bitmap,colour) {};
 EwxMask(const wxBitmap& bitmap) : wxMask(bitmap) {};
 EwxMask() : wxMask() {};
};

class EwxImage : public wxImage {
 public: ~EwxImage() {((WxeApp *)wxTheApp)->clearPtr(this);};
 EwxImage(int width,int height,unsigned char * data,unsigned char * alpha,bool static_data) : wxImage(width,height,data,alpha,static_data) {};
 EwxImage(int width,int height,unsigned char * data,bool static_data) : wxImage(width,height,data,static_data) {};
 EwxImage(int width,int height,bool clear) : wxImage(width,height,clear) {};
 EwxImage(const wxString& name,const wxString& mimetype,int index) : wxImage(name,mimetype,index) {};
 EwxImage(const wxString& name,long type,int index) : wxImage(name,type,index) {};
 EwxImage() : wxImage() {};
 EwxImage(wxImage copy) : wxImage(copy) {};
};

class EwxBrush : public wxBrush {
 public: ~EwxBrush() {((WxeApp *)wxTheApp)->clearPtr(this);};
 EwxBrush(const wxColour& colour,int style) : wxBrush(colour,style) {};
 EwxBrush(const wxBitmap& stippleBitmap) : wxBrush(stippleBitmap) {};
 EwxBrush() : wxBrush() {};
};

class EwxPen : public wxPen {
 public: ~EwxPen() {((WxeApp *)wxTheApp)->clearPtr(this);};
 EwxPen(const wxColour& colour,int width,int style) : wxPen(colour,width,style) {};
 EwxPen() : wxPen() {};
};

class EwxRegion : public wxRegion {
 public: ~EwxRegion() {((WxeApp *)wxTheApp)->clearPtr(this);};
 EwxRegion(wxCoord x,wxCoord y,wxCoord w,wxCoord h) : wxRegion(x,y,w,h) {};
 EwxRegion(const wxPoint& topLeft,const wxPoint& bottomRight) : wxRegion(topLeft,bottomRight) {};
 EwxRegion(const wxBitmap& bmp) : wxRegion(bmp) {};
 EwxRegion(const wxRect& rect) : wxRegion(rect) {};
 EwxRegion() : wxRegion() {};
};

class EwxAcceleratorTable : public wxAcceleratorTable {
 public: ~EwxAcceleratorTable() {((WxeApp *)wxTheApp)->clearPtr(this);};
 EwxAcceleratorTable(int n,const wxAcceleratorEntry * entries) : wxAcceleratorTable(n,entries) {};
 EwxAcceleratorTable() : wxAcceleratorTable() {};
};

class EwxCaret : public wxCaret {
 public: ~EwxCaret() {((WxeApp *)wxTheApp)->clearPtr(this);};
 EwxCaret(wxWindow * window,int width,int height) : wxCaret(window,width,height) {};
 EwxCaret(wxWindow * window,const wxSize& size) : wxCaret(window,size) {};
};

class EwxSizerItem : public wxSizerItem {
 public: ~EwxSizerItem() {((WxeApp *)wxTheApp)->clearPtr(this);};
 EwxSizerItem(int width,int height,int proportion,int flag,int border,wxObject * userData) : wxSizerItem(width,height,proportion,flag,border,userData) {};
 EwxSizerItem(wxWindow * window,int proportion,int flag,int border,wxObject * userData) : wxSizerItem(window,proportion,flag,border,userData) {};
 EwxSizerItem(wxSizer * window,int proportion,int flag,int border,wxObject * userData) : wxSizerItem(window,proportion,flag,border,userData) {};
 EwxSizerItem(int width,int height,const wxSizerFlags& flags) : wxSizerItem(width,height,flags) {};
 EwxSizerItem(wxWindow * window,const wxSizerFlags& flags) : wxSizerItem(window,flags) {};
 EwxSizerItem(wxSizer * window,const wxSizerFlags& flags) : wxSizerItem(window,flags) {};
 EwxSizerItem() : wxSizerItem() {};
};

class EwxBoxSizer : public wxBoxSizer {
 public: ~EwxBoxSizer() {((WxeApp *)wxTheApp)->clearPtr(this);};
 EwxBoxSizer(int orient) : wxBoxSizer(orient) {};
};

class EwxStaticBoxSizer : public wxStaticBoxSizer {
 public: ~EwxStaticBoxSizer() {((WxeApp *)wxTheApp)->clearPtr(this);};
 EwxStaticBoxSizer(int orient,wxWindow * win,const wxString& label) : wxStaticBoxSizer(orient,win,label) {};
 EwxStaticBoxSizer(wxStaticBox * box,int orient) : wxStaticBoxSizer(box,orient) {};
};

class EwxGridSizer : public wxGridSizer {
 public: ~EwxGridSizer() {((WxeApp *)wxTheApp)->clearPtr(this);};
 EwxGridSizer(int rows,int cols,int vgap,int hgap) : wxGridSizer(rows,cols,vgap,hgap) {};
 EwxGridSizer(int cols,int vgap,int hgap) : wxGridSizer(cols,vgap,hgap) {};
};

class EwxFlexGridSizer : public wxFlexGridSizer {
 public: ~EwxFlexGridSizer() {((WxeApp *)wxTheApp)->clearPtr(this);};
 EwxFlexGridSizer(int rows,int cols,int vgap,int hgap) : wxFlexGridSizer(rows,cols,vgap,hgap) {};
 EwxFlexGridSizer(int cols,int vgap,int hgap) : wxFlexGridSizer(cols,vgap,hgap) {};
};

class EwxGridBagSizer : public wxGridBagSizer {
 public: ~EwxGridBagSizer() {((WxeApp *)wxTheApp)->clearPtr(this);};
 EwxGridBagSizer(int vgap,int hgap) : wxGridBagSizer(vgap,hgap) {};
};

class EwxStdDialogButtonSizer : public wxStdDialogButtonSizer {
 public: ~EwxStdDialogButtonSizer() {((WxeApp *)wxTheApp)->clearPtr(this);};
 EwxStdDialogButtonSizer() : wxStdDialogButtonSizer() {};
};

class EwxFont : public wxFont {
 public: ~EwxFont() {((WxeApp *)wxTheApp)->clearPtr(this);};
 EwxFont(int size,wxFontFamily family,wxFontStyle style,int weight,bool underlined,const wxString& face,wxFontEncoding encoding) : wxFont(size,family,style,weight,underlined,face,encoding) {};
 EwxFont(const wxString& fontname) : wxFont(fontname) {};
 EwxFont() : wxFont() {};
 EwxFont(wxFont copy) : wxFont(copy) {};
};

class EwxToolTip : public wxToolTip {
 public: ~EwxToolTip() {((WxeApp *)wxTheApp)->clearPtr(this);};
 EwxToolTip(const wxString& tip) : wxToolTip(tip) {};
};

class EwxButton : public wxButton {
 public: ~EwxButton() {((WxeApp *)wxTheApp)->clearPtr(this);};
 EwxButton(wxWindow * parent,wxWindowID id,const wxString& label,const wxPoint& pos,const wxSize& size,long style,const wxValidator& validator) : wxButton(parent,id,label,pos,size,style,validator) {};
 EwxButton() : wxButton() {};
};

class EwxBitmapButton : public wxBitmapButton {
 public: ~EwxBitmapButton() {((WxeApp *)wxTheApp)->clearPtr(this);};
 EwxBitmapButton(wxWindow * parent,wxWindowID id,const wxBitmap& bitmap,const wxPoint& pos,const wxSize& size,long style,const wxValidator& validator) : wxBitmapButton(parent,id,bitmap,pos,size,style,validator) {};
 EwxBitmapButton() : wxBitmapButton() {};
};

class EwxToggleButton : public wxToggleButton {
 public: ~EwxToggleButton() {((WxeApp *)wxTheApp)->clearPtr(this);};
 EwxToggleButton(wxWindow * parent,wxWindowID id,const wxString& label,const wxPoint& pos,const wxSize& size,long style,const wxValidator& validator) : wxToggleButton(parent,id,label,pos,size,style,validator) {};
 EwxToggleButton() : wxToggleButton() {};
};

class EwxCalendarCtrl : public wxCalendarCtrl {
 public: ~EwxCalendarCtrl() {((WxeApp *)wxTheApp)->clearPtr(this);};
 EwxCalendarCtrl(wxWindow * parent,wxWindowID id,const wxDateTime& date,const wxPoint& pos,const wxSize& size,long style) : wxCalendarCtrl(parent,id,date,pos,size,style) {};
 EwxCalendarCtrl() : wxCalendarCtrl() {};
};

class EwxCheckBox : public wxCheckBox {
 public: ~EwxCheckBox() {((WxeApp *)wxTheApp)->clearPtr(this);};
 EwxCheckBox(wxWindow * parent,wxWindowID id,const wxString& label,const wxPoint& pos,const wxSize& size,long style,const wxValidator& validator) : wxCheckBox(parent,id,label,pos,size,style,validator) {};
 EwxCheckBox() : wxCheckBox() {};
};

class EwxCheckListBox : public wxCheckListBox {
 public: ~EwxCheckListBox() {((WxeApp *)wxTheApp)->clearPtr(this);};
 EwxCheckListBox(wxWindow * parent,wxWindowID id,const wxPoint& pos,const wxSize& size,const wxArrayString& choices,long style,const wxValidator& validator) : wxCheckListBox(parent,id,pos,size,choices,style,validator) {};
 EwxCheckListBox() : wxCheckListBox() {};
};

class EwxChoice : public wxChoice {
 public: ~EwxChoice() {((WxeApp *)wxTheApp)->clearPtr(this);};
 EwxChoice(wxWindow * parent,wxWindowID id,const wxPoint& pos,const wxSize& size,const wxArrayString& choices,long style,const wxValidator& validator) : wxChoice(parent,id,pos,size,choices,style,validator) {};
 EwxChoice() : wxChoice() {};
};

class EwxComboBox : public wxComboBox {
 public: ~EwxComboBox() {((WxeApp *)wxTheApp)->clearPtr(this);};
 EwxComboBox(wxWindow * parent,wxWindowID id,const wxString& value,const wxPoint& pos,const wxSize& size,const wxArrayString& choices,long style,const wxValidator& validator) : wxComboBox(parent,id,value,pos,size,choices,style,validator) {};
 EwxComboBox() : wxComboBox() {};
};

class EwxGauge : public wxGauge {
 public: ~EwxGauge() {((WxeApp *)wxTheApp)->clearPtr(this);};
 EwxGauge(wxWindow * parent,wxWindowID id,int range,const wxPoint& pos,const wxSize& size,long style,const wxValidator& validator) : wxGauge(parent,id,range,pos,size,style,validator) {};
 EwxGauge() : wxGauge() {};
};

class EwxGenericDirCtrl : public wxGenericDirCtrl {
 public: ~EwxGenericDirCtrl() {((WxeApp *)wxTheApp)->clearPtr(this);};
 EwxGenericDirCtrl(wxWindow * parent,const wxWindowID id,const wxString& dir,const wxPoint& pos,const wxSize& size,long style,const wxString& filter,int defaultFilter) : wxGenericDirCtrl(parent,id,dir,pos,size,style,filter,defaultFilter) {};
 EwxGenericDirCtrl() : wxGenericDirCtrl() {};
};

class EwxStaticBox : public wxStaticBox {
 public: ~EwxStaticBox() {((WxeApp *)wxTheApp)->clearPtr(this);};
 EwxStaticBox(wxWindow * parent,wxWindowID id,const wxString& label,const wxPoint& pos,const wxSize& size,long style) : wxStaticBox(parent,id,label,pos,size,style) {};
 EwxStaticBox() : wxStaticBox() {};
};

class EwxStaticLine : public wxStaticLine {
 public: ~EwxStaticLine() {((WxeApp *)wxTheApp)->clearPtr(this);};
 EwxStaticLine(wxWindow * parent,wxWindowID id,const wxPoint& pos,const wxSize& size,long style) : wxStaticLine(parent,id,pos,size,style) {};
 EwxStaticLine() : wxStaticLine() {};
};

class EwxListBox : public wxListBox {
 public: ~EwxListBox() {((WxeApp *)wxTheApp)->clearPtr(this);};
 EwxListBox(wxWindow * parent,wxWindowID id,const wxPoint& pos,const wxSize& size,const wxArrayString& choices,long style,const wxValidator& validator) : wxListBox(parent,id,pos,size,choices,style,validator) {};
 EwxListBox() : wxListBox() {};
};


class EwxListCtrl : public wxListCtrl {
 public: ~EwxListCtrl();
 EwxListCtrl(wxWindow * parent,wxWindowID winid,const wxPoint& pos,const wxSize& size,long style,const wxValidator& validator) : wxListCtrl(parent,winid,pos,size,style,validator) {};
 EwxListCtrl() : wxListCtrl() {};

 int onGetItemText;
 int onGetItemAttr;
 int onGetItemColumnImage;
 ErlDrvTermData port;

 private:
 virtual wxString OnGetItemText(long item, long col) const;
 virtual wxListItemAttr* OnGetItemAttr(long item) const;
 virtual int OnGetItemImage(long item) const;
 virtual int OnGetItemColumnImage(long item, long column) const;
};

class EwxListItem : public wxListItem {
 public: ~EwxListItem() {((WxeApp *)wxTheApp)->clearPtr(this);};
 EwxListItem(const wxListItem& item) : wxListItem(item) {};
 EwxListItem() : wxListItem() {};
};

class EwxImageList : public wxImageList {
 public: ~EwxImageList() {((WxeApp *)wxTheApp)->clearPtr(this);};
 EwxImageList(int width,int height,bool mask,int initialCount) : wxImageList(width,height,mask,initialCount) {};
 EwxImageList() : wxImageList() {};
};

class EwxTextCtrl : public wxTextCtrl {
 public: ~EwxTextCtrl() {((WxeApp *)wxTheApp)->clearPtr(this);};
 EwxTextCtrl(wxWindow * parent,wxWindowID id,const wxString& value,const wxPoint& pos,const wxSize& size,long style,const wxValidator& validator) : wxTextCtrl(parent,id,value,pos,size,style,validator) {};
 EwxTextCtrl() : wxTextCtrl() {};
};

class EwxNotebook : public wxNotebook {
 public: ~EwxNotebook() {((WxeApp *)wxTheApp)->clearPtr(this);};
 EwxNotebook(wxWindow * parent,wxWindowID winid,const wxPoint& pos,const wxSize& size,long style) : wxNotebook(parent,winid,pos,size,style) {};
 EwxNotebook() : wxNotebook() {};
};

class EwxChoicebook : public wxChoicebook {
 public: ~EwxChoicebook() {((WxeApp *)wxTheApp)->clearPtr(this);};
 EwxChoicebook(wxWindow * parent,wxWindowID id,const wxPoint& pos,const wxSize& size,long style) : wxChoicebook(parent,id,pos,size,style) {};
 EwxChoicebook() : wxChoicebook() {};
};

class EwxToolbook : public wxToolbook {
 public: ~EwxToolbook() {((WxeApp *)wxTheApp)->clearPtr(this);};
 EwxToolbook(wxWindow * parent,wxWindowID id,const wxPoint& pos,const wxSize& size,long style) : wxToolbook(parent,id,pos,size,style) {};
 EwxToolbook() : wxToolbook() {};
};

class EwxListbook : public wxListbook {
 public: ~EwxListbook() {((WxeApp *)wxTheApp)->clearPtr(this);};
 EwxListbook(wxWindow * parent,wxWindowID id,const wxPoint& pos,const wxSize& size,long style) : wxListbook(parent,id,pos,size,style) {};
 EwxListbook() : wxListbook() {};
};

class EwxTreebook : public wxTreebook {
 public: ~EwxTreebook() {((WxeApp *)wxTheApp)->clearPtr(this);};
 EwxTreebook(wxWindow * parent,wxWindowID id,const wxPoint& pos,const wxSize& size,long style) : wxTreebook(parent,id,pos,size,style) {};
 EwxTreebook() : wxTreebook() {};
};

class EwxTreeCtrl : public wxTreeCtrl {
 public: ~EwxTreeCtrl() {((WxeApp *)wxTheApp)->clearPtr(this);};
 EwxTreeCtrl(wxWindow * parent,wxWindowID id,const wxPoint& pos,const wxSize& size,long style,const wxValidator& validator) : wxTreeCtrl(parent,id,pos,size,style,validator) {};
 EwxTreeCtrl() : wxTreeCtrl() {};
};

class EwxScrollBar : public wxScrollBar {
 public: ~EwxScrollBar() {((WxeApp *)wxTheApp)->clearPtr(this);};
 EwxScrollBar(wxWindow * parent,wxWindowID id,const wxPoint& pos,const wxSize& size,long style,const wxValidator& validator) : wxScrollBar(parent,id,pos,size,style,validator) {};
 EwxScrollBar() : wxScrollBar() {};
};

class EwxSpinButton : public wxSpinButton {
 public: ~EwxSpinButton() {((WxeApp *)wxTheApp)->clearPtr(this);};
 EwxSpinButton(wxWindow * parent,wxWindowID id,const wxPoint& pos,const wxSize& size,long style) : wxSpinButton(parent,id,pos,size,style) {};
 EwxSpinButton() : wxSpinButton() {};
};

class EwxSpinCtrl : public wxSpinCtrl {
 public: ~EwxSpinCtrl() {((WxeApp *)wxTheApp)->clearPtr(this);};
 EwxSpinCtrl(wxWindow * parent,wxWindowID id,const wxString& value,const wxPoint& pos,const wxSize& size,long style,int min,int max,int initial) : wxSpinCtrl(parent,id,value,pos,size,style,min,max,initial) {};
 EwxSpinCtrl() : wxSpinCtrl() {};
};

class EwxStaticText : public wxStaticText {
 public: ~EwxStaticText() {((WxeApp *)wxTheApp)->clearPtr(this);};
 EwxStaticText(wxWindow * parent,wxWindowID id,const wxString& label,const wxPoint& pos,const wxSize& size,long style) : wxStaticText(parent,id,label,pos,size,style) {};
 EwxStaticText() : wxStaticText() {};
};

class EwxStaticBitmap : public wxStaticBitmap {
 public: ~EwxStaticBitmap() {((WxeApp *)wxTheApp)->clearPtr(this);};
 EwxStaticBitmap(wxWindow * parent,wxWindowID id,const wxBitmap& label,const wxPoint& pos,const wxSize& size,long style) : wxStaticBitmap(parent,id,label,pos,size,style) {};
 EwxStaticBitmap() : wxStaticBitmap() {};
};

class EwxRadioBox : public wxRadioBox {
 public: ~EwxRadioBox() {((WxeApp *)wxTheApp)->clearPtr(this);};
 EwxRadioBox(wxWindow * parent,wxWindowID id,const wxString& title,const wxPoint& pos,const wxSize& size,const wxArrayString& choices,int majorDim,long style,const wxValidator& val) : wxRadioBox(parent,id,title,pos,size,choices,majorDim,style,val) {};
};

class EwxRadioButton : public wxRadioButton {
 public: ~EwxRadioButton() {((WxeApp *)wxTheApp)->clearPtr(this);};
 EwxRadioButton(wxWindow * parent,wxWindowID id,const wxString& label,const wxPoint& pos,const wxSize& size,long style,const wxValidator& validator) : wxRadioButton(parent,id,label,pos,size,style,validator) {};
 EwxRadioButton() : wxRadioButton() {};
};

class EwxSlider : public wxSlider {
 public: ~EwxSlider() {((WxeApp *)wxTheApp)->clearPtr(this);};
 EwxSlider(wxWindow * parent,wxWindowID id,int value,int minValue,int maxValue,const wxPoint& pos,const wxSize& size,long style,const wxValidator& validator) : wxSlider(parent,id,value,minValue,maxValue,pos,size,style,validator) {};
 EwxSlider() : wxSlider() {};
};

class EwxDialog : public wxDialog {
 public: ~EwxDialog() {((WxeApp *)wxTheApp)->clearPtr(this);};
 EwxDialog(wxWindow * parent,wxWindowID id,const wxString& title,const wxPoint& pos,const wxSize& size,long style) : wxDialog(parent,id,title,pos,size,style) {};
 EwxDialog() : wxDialog() {};
};

class EwxColourDialog : public wxColourDialog {
 public: ~EwxColourDialog() {((WxeApp *)wxTheApp)->clearPtr(this);};
 EwxColourDialog(wxWindow * parent,wxColourData * data) : wxColourDialog(parent,data) {};
 EwxColourDialog() : wxColourDialog() {};
};

class EwxColourData : public wxColourData {
 public: ~EwxColourData() {((WxeApp *)wxTheApp)->clearPtr(this);};
 EwxColourData(const wxColourData& data) : wxColourData(data) {};
 EwxColourData() : wxColourData() {};
};

class EwxPalette : public wxPalette {
 public: ~EwxPalette() {((WxeApp *)wxTheApp)->clearPtr(this);};
 EwxPalette(int n,const unsigned char * red,const unsigned char * green,const unsigned char * blue) : wxPalette(n,red,green,blue) {};
 EwxPalette() : wxPalette() {};
};

class EwxDirDialog : public wxDirDialog {
 public: ~EwxDirDialog() {((WxeApp *)wxTheApp)->clearPtr(this);};
 EwxDirDialog(wxWindow * parent,const wxString& title,const wxString& defaultPath,long style,const wxPoint& pos,const wxSize& sz) : wxDirDialog(parent,title,defaultPath,style,pos,sz) {};
};

class EwxFileDialog : public wxFileDialog {
 public: ~EwxFileDialog() {((WxeApp *)wxTheApp)->clearPtr(this);};
 EwxFileDialog(wxWindow * parent,const wxString& message,const wxString& defaultDir,const wxString& defaultFile,const wxString& wildCard,long style,const wxPoint& pos,const wxSize& sz) : wxFileDialog(parent,message,defaultDir,defaultFile,wildCard,style,pos,sz) {};
};

class EwxFilePickerCtrl : public wxFilePickerCtrl {
 public: ~EwxFilePickerCtrl() {((WxeApp *)wxTheApp)->clearPtr(this);};
 EwxFilePickerCtrl(wxWindow * parent,wxWindowID id,const wxString& path,const wxString& message,const wxString& wildcard,const wxPoint& pos,const wxSize& size,long style,const wxValidator& validator) : wxFilePickerCtrl(parent,id,path,message,wildcard,pos,size,style,validator) {};
 EwxFilePickerCtrl() : wxFilePickerCtrl() {};
};

class EwxDirPickerCtrl : public wxDirPickerCtrl {
 public: ~EwxDirPickerCtrl() {((WxeApp *)wxTheApp)->clearPtr(this);};
 EwxDirPickerCtrl(wxWindow * parent,wxWindowID id,const wxString& path,const wxString& message,const wxPoint& pos,const wxSize& size,long style,const wxValidator& validator) : wxDirPickerCtrl(parent,id,path,message,pos,size,style,validator) {};
 EwxDirPickerCtrl() : wxDirPickerCtrl() {};
};

class EwxColourPickerCtrl : public wxColourPickerCtrl {
 public: ~EwxColourPickerCtrl() {((WxeApp *)wxTheApp)->clearPtr(this);};
 EwxColourPickerCtrl(wxWindow * parent,wxWindowID id,const wxColour& col,const wxPoint& pos,const wxSize& size,long style,const wxValidator& validator) : wxColourPickerCtrl(parent,id,col,pos,size,style,validator) {};
 EwxColourPickerCtrl() : wxColourPickerCtrl() {};
};

class EwxDatePickerCtrl : public wxDatePickerCtrl {
 public: ~EwxDatePickerCtrl() {((WxeApp *)wxTheApp)->clearPtr(this);};
 EwxDatePickerCtrl(wxWindow * parent,wxWindowID id,const wxDateTime& date,const wxPoint& pos,const wxSize& size,long style,const wxValidator& validator) : wxDatePickerCtrl(parent,id,date,pos,size,style,validator) {};
 EwxDatePickerCtrl() : wxDatePickerCtrl() {};
};

class EwxFontPickerCtrl : public wxFontPickerCtrl {
 public: ~EwxFontPickerCtrl() {((WxeApp *)wxTheApp)->clearPtr(this);};
 EwxFontPickerCtrl(wxWindow * parent,wxWindowID id,const wxFont& initial,const wxPoint& pos,const wxSize& size,long style,const wxValidator& validator) : wxFontPickerCtrl(parent,id,initial,pos,size,style,validator) {};
 EwxFontPickerCtrl() : wxFontPickerCtrl() {};
};

class EwxFindReplaceDialog : public wxFindReplaceDialog {
 public: ~EwxFindReplaceDialog() {((WxeApp *)wxTheApp)->clearPtr(this);};
 EwxFindReplaceDialog(wxWindow * parent,wxFindReplaceData * data,const wxString& title,int style) : wxFindReplaceDialog(parent,data,title,style) {};
 EwxFindReplaceDialog() : wxFindReplaceDialog() {};
};

class EwxFindReplaceData : public wxFindReplaceData {
 public: ~EwxFindReplaceData() {((WxeApp *)wxTheApp)->clearPtr(this);};
 EwxFindReplaceData(unsigned int flags) : wxFindReplaceData(flags) {};
 EwxFindReplaceData() : wxFindReplaceData() {};
};

class EwxMultiChoiceDialog : public wxMultiChoiceDialog {
 public: ~EwxMultiChoiceDialog() {((WxeApp *)wxTheApp)->clearPtr(this);};
 EwxMultiChoiceDialog(wxWindow * parent,const wxString& message,const wxString& caption,const wxArrayString& choices,long style,const wxPoint& pos) : wxMultiChoiceDialog(parent,message,caption,choices,style,pos) {};
 EwxMultiChoiceDialog() : wxMultiChoiceDialog() {};
};

class EwxSingleChoiceDialog : public wxSingleChoiceDialog {
 public: ~EwxSingleChoiceDialog() {((WxeApp *)wxTheApp)->clearPtr(this);};
 EwxSingleChoiceDialog(wxWindow * parent,const wxString& message,const wxString& caption,const wxArrayString& choices,char ** clientData,long style,const wxPoint& pos) : wxSingleChoiceDialog(parent,message,caption,choices,clientData,style,pos) {};
 EwxSingleChoiceDialog() : wxSingleChoiceDialog() {};
};

class EwxTextEntryDialog : public wxTextEntryDialog {
 public: ~EwxTextEntryDialog() {((WxeApp *)wxTheApp)->clearPtr(this);};
 EwxTextEntryDialog(wxWindow * parent,const wxString& message,const wxString& caption,const wxString& value,long style,const wxPoint& pos) : wxTextEntryDialog(parent,message,caption,value,style,pos) {};
};

class EwxPasswordEntryDialog : public wxPasswordEntryDialog {
 public: ~EwxPasswordEntryDialog() {((WxeApp *)wxTheApp)->clearPtr(this);};
 EwxPasswordEntryDialog(wxWindow * parent,const wxString& message,const wxString& caption,const wxString& value,long style,const wxPoint& pos) : wxPasswordEntryDialog(parent,message,caption,value,style,pos) {};
};

class EwxFontData : public wxFontData {
 public: ~EwxFontData() {((WxeApp *)wxTheApp)->clearPtr(this);};
 EwxFontData(const wxFontData& data) : wxFontData(data) {};
 EwxFontData() : wxFontData() {};
};

class EwxFontDialog : public wxFontDialog {
 public: ~EwxFontDialog() {((WxeApp *)wxTheApp)->clearPtr(this);};
 EwxFontDialog(wxWindow * parent,const wxFontData& data) : wxFontDialog(parent,data) {};
 EwxFontDialog() : wxFontDialog() {};
};

class EwxProgressDialog : public wxProgressDialog {
 public: ~EwxProgressDialog() {((WxeApp *)wxTheApp)->clearPtr(this);};
 EwxProgressDialog(const wxString& title,const wxString& message,int maximum,wxWindow * parent,int style) : wxProgressDialog(title,message,maximum,parent,style) {};
};

class EwxMessageDialog : public wxMessageDialog {
 public: ~EwxMessageDialog() {((WxeApp *)wxTheApp)->clearPtr(this);};
 EwxMessageDialog(wxWindow * parent,const wxString& message,const wxString& caption,long style,const wxPoint& pos) : wxMessageDialog(parent,message,caption,style,pos) {};
};

class EwxPageSetupDialog : public wxPageSetupDialog {
 public: ~EwxPageSetupDialog() {((WxeApp *)wxTheApp)->clearPtr(this);};
 EwxPageSetupDialog(wxWindow * parent,wxPageSetupDialogData * data) : wxPageSetupDialog(parent,data) {};
};

class EwxPageSetupDialogData : public wxPageSetupDialogData {
 public: ~EwxPageSetupDialogData() {((WxeApp *)wxTheApp)->clearPtr(this);};
 EwxPageSetupDialogData(const wxPrintData& printData) : wxPageSetupDialogData(printData) {};
 EwxPageSetupDialogData(const wxPageSetupDialogData& printData) : wxPageSetupDialogData(printData) {};
 EwxPageSetupDialogData() : wxPageSetupDialogData() {};
};

class EwxPrintDialog : public wxPrintDialog {
 public: ~EwxPrintDialog() {((WxeApp *)wxTheApp)->clearPtr(this);};
 EwxPrintDialog(wxWindow * parent,wxPrintDialogData * data) : wxPrintDialog(parent,data) {};
 EwxPrintDialog(wxWindow * parent,wxPrintData * data) : wxPrintDialog(parent,data) {};
};

class EwxPrintDialogData : public wxPrintDialogData {
 public: ~EwxPrintDialogData() {((WxeApp *)wxTheApp)->clearPtr(this);};
 EwxPrintDialogData(const wxPrintDialogData& dialogData) : wxPrintDialogData(dialogData) {};
 EwxPrintDialogData(const wxPrintData& dialogData) : wxPrintDialogData(dialogData) {};
 EwxPrintDialogData() : wxPrintDialogData() {};
};

class EwxPrintData : public wxPrintData {
 public: ~EwxPrintData() {((WxeApp *)wxTheApp)->clearPtr(this);};
 EwxPrintData(const wxPrintData& printData) : wxPrintData(printData) {};
 EwxPrintData() : wxPrintData() {};
};

class EwxPrintPreview : public wxPrintPreview {
 public: ~EwxPrintPreview() {((WxeApp *)wxTheApp)->clearPtr(this);};
 EwxPrintPreview(wxPrintout * printout,wxPrintout * printoutForPrinting,wxPrintData * data) : wxPrintPreview(printout,printoutForPrinting,data) {};
 EwxPrintPreview(wxPrintout * printout,wxPrintout * printoutForPrinting,wxPrintDialogData * data) : wxPrintPreview(printout,printoutForPrinting,data) {};
};

class EwxPreviewFrame : public wxPreviewFrame {
 public: ~EwxPreviewFrame() {((WxeApp *)wxTheApp)->clearPtr(this);};
 EwxPreviewFrame(wxPrintPreview * preview,wxWindow * parent,const wxString& title,const wxPoint& pos,const wxSize& size,long style) : wxPreviewFrame(preview,parent,title,pos,size,style) {};
};

class EwxPreviewControlBar : public wxPreviewControlBar {
 public: ~EwxPreviewControlBar() {((WxeApp *)wxTheApp)->clearPtr(this);};
 EwxPreviewControlBar(wxPrintPreview * preview,long buttons,wxWindow * parent,const wxPoint& pos,const wxSize& size,long style) : wxPreviewControlBar(preview,buttons,parent,pos,size,style) {};
};

class EwxPrinter : public wxPrinter {
 public: ~EwxPrinter() {((WxeApp *)wxTheApp)->clearPtr(this);};
 EwxPrinter(wxPrintDialogData * data) : wxPrinter(data) {};
};

class EwxXmlResource : public wxXmlResource {
 public: ~EwxXmlResource() {((WxeApp *)wxTheApp)->clearPtr(this);};
 EwxXmlResource(const wxString& filemask,int flags,const wxString& domain) : wxXmlResource(filemask,flags,domain) {};
 EwxXmlResource(int flags,const wxString& domain) : wxXmlResource(flags,domain) {};
};

class EwxHtmlEasyPrinting : public wxHtmlEasyPrinting {
 public: ~EwxHtmlEasyPrinting() {((WxeApp *)wxTheApp)->clearPtr(this);};
 EwxHtmlEasyPrinting(const wxString& name,wxWindow * parentWindow) : wxHtmlEasyPrinting(name,parentWindow) {};
};

class EwxGLCanvas : public wxGLCanvas {
 public: ~EwxGLCanvas() {deleteActiveGL(this);((WxeApp *)wxTheApp)->clearPtr(this);};
 EwxGLCanvas(wxWindow * parent,const wxGLContext * shared,wxWindowID id,const wxPoint& pos,const wxSize& size,long style,const wxString& name,int * attribList,const wxPalette& palette) : wxGLCanvas(parent,shared,id,pos,size,style,name,attribList,palette) {};
 EwxGLCanvas(wxWindow * parent,const wxGLCanvas * shared,wxWindowID id,const wxPoint& pos,const wxSize& size,long style,const wxString& name,int * attribList,const wxPalette& palette) : wxGLCanvas(parent,shared,id,pos,size,style,name,attribList,palette) {};
 EwxGLCanvas(wxWindow * parent,wxWindowID id,const wxPoint& pos,const wxSize& size,long style,const wxString& name,int * attribList,const wxPalette& palette) : wxGLCanvas(parent,id,pos,size,style,name,attribList,palette) {};
};

#if wxUSE_AUI
class EwxAuiManager : public wxAuiManager {
 public: ~EwxAuiManager() {((WxeApp *)wxTheApp)->clearPtr(this);};
 EwxAuiManager(wxWindow * managed_wnd,unsigned int flags) : wxAuiManager(managed_wnd,flags) {};
};
#endif // wxUSE_AUI

#if wxUSE_AUI
class EwxAuiNotebook : public wxAuiNotebook {
 public: ~EwxAuiNotebook() {((WxeApp *)wxTheApp)->clearPtr(this);};
 EwxAuiNotebook(wxWindow * parent,wxWindowID id,const wxPoint& pos,const wxSize& size,long style) : wxAuiNotebook(parent,id,pos,size,style) {};
 EwxAuiNotebook() : wxAuiNotebook() {};
};
#endif // wxUSE_AUI

class EwxMDIParentFrame : public wxMDIParentFrame {
 public: ~EwxMDIParentFrame() {((WxeApp *)wxTheApp)->clearPtr(this);};
 EwxMDIParentFrame(wxWindow * parent,wxWindowID id,const wxString& title,const wxPoint& pos,const wxSize& size,long style) : wxMDIParentFrame(parent,id,title,pos,size,style) {};
 EwxMDIParentFrame() : wxMDIParentFrame() {};
};

class EwxMDIChildFrame : public wxMDIChildFrame {
 public: ~EwxMDIChildFrame() {((WxeApp *)wxTheApp)->clearPtr(this);};
 EwxMDIChildFrame(wxMDIParentFrame * parent,wxWindowID id,const wxString& title,const wxPoint& pos,const wxSize& size,long style) : wxMDIChildFrame(parent,id,title,pos,size,style) {};
 EwxMDIChildFrame() : wxMDIChildFrame() {};
};

class EwxMDIClientWindow : public wxMDIClientWindow {
 public: ~EwxMDIClientWindow() {((WxeApp *)wxTheApp)->clearPtr(this);};
#if !wxCHECK_VERSION(2,9,0)
 EwxMDIClientWindow(wxMDIParentFrame * parent,long style) : wxMDIClientWindow(parent,style) {};
#endif
 EwxMDIClientWindow() : wxMDIClientWindow() {};
};

class EwxLayoutAlgorithm : public wxLayoutAlgorithm {
 public: ~EwxLayoutAlgorithm() {((WxeApp *)wxTheApp)->clearPtr(this);};
 EwxLayoutAlgorithm() : wxLayoutAlgorithm() {};
};

class EwxPrintout : public wxPrintout {
 public: ~EwxPrintout() {((WxeApp *)wxTheApp)->clearPtr(this);};
 EwxPrintout(const wxString& title) : wxPrintout(title) {};
};

class EwxStyledTextCtrl : public wxStyledTextCtrl {
 public: ~EwxStyledTextCtrl() {((WxeApp *)wxTheApp)->clearPtr(this);};
 EwxStyledTextCtrl(wxWindow * parent,wxWindowID id,const wxPoint& pos,const wxSize& size,long style) : wxStyledTextCtrl(parent,id,pos,size,style) {};
 EwxStyledTextCtrl() : wxStyledTextCtrl() {};
};

class EwxClipboard : public wxClipboard {
 public: ~EwxClipboard() {((WxeApp *)wxTheApp)->clearPtr(this);};
 EwxClipboard() : wxClipboard() {};
};

class EwxSplitterWindow : public wxSplitterWindow {
 public: ~EwxSplitterWindow() {((WxeApp *)wxTheApp)->clearPtr(this);};
 EwxSplitterWindow(wxWindow * parent,wxWindowID id,const wxPoint& pos,const wxSize& size,long style) : wxSplitterWindow(parent,id,pos,size,style) {};
 EwxSplitterWindow() : wxSplitterWindow() {};
};

class EwxHtmlWindow : public wxHtmlWindow {
 public: ~EwxHtmlWindow() {((WxeApp *)wxTheApp)->clearPtr(this);};
 EwxHtmlWindow(wxWindow * parent,wxWindowID id,const wxPoint& pos,const wxSize& size,long style) : wxHtmlWindow(parent,id,pos,size,style) {};
 EwxHtmlWindow() : wxHtmlWindow() {};
};

class EwxTaskBarIcon : public wxTaskBarIcon {
 public: ~EwxTaskBarIcon() {((WxeApp *)wxTheApp)->clearPtr(this);};
 EwxTaskBarIcon() : wxTaskBarIcon() {};
};

class EwxLocale : public wxLocale {
 public: ~EwxLocale() {((WxeApp *)wxTheApp)->clearPtr(this);};
 EwxLocale(int language,int flags) : wxLocale(language,flags) {};
 EwxLocale() : wxLocale() {};
};

#if wxUSE_POPUPWIN
class EwxPopupWindow : public wxPopupWindow {
 public: ~EwxPopupWindow() {((WxeApp *)wxTheApp)->clearPtr(this);};
 EwxPopupWindow(wxWindow * parent,int flags) : wxPopupWindow(parent,flags) {};
 EwxPopupWindow() : wxPopupWindow() {};
};
#endif // wxUSE_POPUPWIN

#if wxUSE_POPUPWIN
class EwxPopupTransientWindow : public wxPopupTransientWindow {
 public: ~EwxPopupTransientWindow() {((WxeApp *)wxTheApp)->clearPtr(this);};
 EwxPopupTransientWindow(wxWindow * parent,int style) : wxPopupTransientWindow(parent,style) {};
 EwxPopupTransientWindow() : wxPopupTransientWindow() {};
};
#endif // wxUSE_POPUPWIN

class EwxDCOverlay : public wxDCOverlay {
 public: ~EwxDCOverlay() {((WxeApp *)wxTheApp)->clearPtr(this);};
 EwxDCOverlay(wxOverlay& overlay,wxWindowDC * dc,int x,int y,int width,int height) : wxDCOverlay(overlay,dc,x,y,width,height) {};
 EwxDCOverlay(wxOverlay& overlay,wxWindowDC * dc) : wxDCOverlay(overlay,dc) {};
};

