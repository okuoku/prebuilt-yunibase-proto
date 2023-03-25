;;; -*- mode: scheme; coding: utf-8; -*-
;;;
;;; user.scm - Win32 API wrapper library
;;;
;;;   Copyright (c) 2010-2015  Takashi Kato  <ktakashi@ymail.com>
;;;
;;;   Redistribution and use in source and binary forms, with or without
;;;   modification, are permitted provided that the following conditions
;;;   are met:
;;;
;;;   1. Redistributions of source code must retain the above copyright
;;;      notice, this list of conditions and the following disclaimer.
;;;
;;;   2. Redistributions in binary form must reproduce the above copyright
;;;      notice, this list of conditions and the following disclaimer in the
;;;      documentation and/or other materials provided with the distribution.
;;;
;;;   THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS
;;;   "AS IS" AND ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT
;;;   LIMITED TO, THE IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR
;;;   A PARTICULAR PURPOSE ARE DISCLAIMED. IN NO EVENT SHALL THE COPYRIGHT
;;;   OWNER OR CONTRIBUTORS BE LIABLE FOR ANY DIRECT, INDIRECT, INCIDENTAL,
;;;   SPECIAL, EXEMPLARY, OR CONSEQUENTIAL DAMAGES (INCLUDING, BUT NOT LIMITED
;;;   TO, PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES; LOSS OF USE, DATA, OR
;;;   PROFITS; OR BUSINESS INTERRUPTION) HOWEVER CAUSED AND ON ANY THEORY OF
;;;   LIABILITY, WHETHER IN CONTRACT, STRICT LIABILITY, OR TORT (INCLUDING
;;;   NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF THE USE OF THIS
;;;   SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.
;;;

;; based on Cygwin's winuser.h
#!nounbound
(library (win32 user)
    (export WNDPROC
	    IDI_APPLICATION IDI_HAND IDI_QUESTION IDI_EXCLAMATION
	    IDI_ASTERISK IDI_WINLOGO IDI_SHIELD

	    IDC_ARROW IDC_IBEAM IDC_WAIT IDC_CROSS IDC_UPARROW
	    IDC_SIZE IDC_ICON IDC_SIZENWSE IDC_SIZENESW IDC_SIZEWE
	    IDC_SIZENS IDC_SIZEALL IDC_NO IDC_HAND IDC_APPSTARTING
	    IDC_HELP

	    WNDCLASSEX
	    PAINTSTRUCT
	    MSG
	    CREATESTRUCT
	    MENUITEMINFO
	    NMHDR
	    DRAWITEMSTRUCT
	    message-box
	    create-window-ex
	    create-window
	    load-icon
	    destroy-icon
	    load-cursor
	    load-image
	    register-class-ex
	    get-class-info-ex
	    post-quit-message
	    def-window-proc
	    call-window-proc
	    show-window
	    update-window
	    is-window-visible
	    get-parent
	    get-message
	    peek-message
	    translate-message
	    dispatch-message
	    get-message-pos
	    tabbed-text-out
	    get-dc
	    get-dc-ex
	    release-dc
	    begin-paint
	    end-paint
	    get-client-rect
	    get-update-rect
	    invalidate-rect
	    inflate-rect
	    is-rect-empty
	    fill-rect
	    set-rect
	    move-window
	    destroy-window
	    send-message
	    post-message
	    create-menu
	    create-popup-menu
	    destroy-menu
	    append-menu
	    delete-menu
	    get-system-menu
	    set-menu
	    draw-menu-bar
	    insert-menu-item
	    set-menu-item-info
	    
	    draw-text
	    set-window-text
	    get-window-text
	    get-window-text-length
	    get-window-rect
	    set-window-pos

	    set-cursor
	    get-cusor-pos
	    set-capture
	    release-capture
	    set-focus

	    set-window-long-ptr
	    get-window-long-ptr

	    get-sys-color
	    get-sys-color-brush
	    set-sys-colors

	    SCROLLINFO
	    get-scroll-info
	    set-scroll-info
	    scroll-window-ex

	    system-parameters-info
	    create-caret
	    show-caret
	    hide-caret
	    destroy-caret
	    set-caret-pos
	    get-caret-pos

	    TIMERPROC
	    set-timer kill-timer

	    screen-to-client
	    get-key-state

	    ;; for custom extension
	    (rename (user32 *windows-user32-module*))
	    )
    (import (rnrs)
	    (rename (sagittarius) (define-constant defconst))
	    (sagittarius ffi)
	    (win32 defs))

  (define user32 (open-win32-module "user32.dll"))
  (define WNDPROC callback)
  (define-syntax define-constant
    (syntax-rules ()
      ((_ name value)
       (begin
	 (export name)
	 (defconst name value)))))
  (define-syntax define-export
    (syntax-rules ()
      ((_ name value)
       (begin
	 (export name)
	 (define name value)))))

  ;; windows messages
  (define-constant WM_NULL #x0000)
  (define-constant WM_CREATE #x0001)
  (define-constant WM_DESTROY #x0002)
  (define-constant WM_MOVE #x0003)
  (define-constant WM_SIZE #x0005)
  (define-constant WM_ACTIVATE #x0006)
  (define-constant WA_INACTIVE 0)
  (define-constant WA_ACTIVE 1)
  (define-constant WA_CLICKACTIVE 2)
  (define-constant WM_SETFOCUS #x0007)
  (define-constant WM_KILLFOCUS #x0008)
  (define-constant WM_ENABLE #x000A)
  (define-constant WM_SETREDRAW #x000B)
  (define-constant WM_SETTEXT #x000C)
  (define-constant WM_GETTEXT #x000D)
  (define-constant WM_GETTEXTLENGTH #x000E)
  (define-constant WM_PAINT #x000F)
  (define-constant WM_CLOSE #x0010)

  (define-constant WM_QUERYENDSESSION #x0011)
  (define-constant WM_QUERYOPEN #x0013)
  (define-constant WM_ENDSESSION #x0016)
  (define-constant WM_QUIT #x0012)
  (define-constant WM_ERASEBKGND #x0014)
  (define-constant WM_SYSCOLORCHANGE #x0015)
  (define-constant WM_SHOWWINDOW #x0018)
  (define-constant WM_WININICHANGE #x001A)
  (define-constant WM_SETTINGCHANGE WM_WININICHANGE)
  (define-constant WM_DEVMODECHANGE #x001B)
  (define-constant WM_ACTIVATEAPP #x001C)
  (define-constant WM_FONTCHANGE #x001D)
  (define-constant WM_TIMECHANGE #x001E)
  (define-constant WM_CANCELMODE #x001F)
  (define-constant WM_SETCURSOR #x0020)
  (define-constant WM_MOUSEACTIVATE #x0021)
  (define-constant WM_CHILDACTIVATE #x0022)
  (define-constant WM_QUEUESYNC #x0023)
  (define-constant WM_GETMINMAXINFO #x0024)
  (define-constant WM_PAINTICON #x0026)
  (define-constant WM_ICONERASEBKGND #x0027)
  (define-constant WM_NEXTDLGCTL #x0028)
  (define-constant WM_SPOOLERSTATUS #x002A)
  (define-constant WM_DRAWITEM #x002B)

  (define-c-struct DRAWITEMSTRUCT
    (UINT        CtlType)
    (UINT        CtlID)
    (UINT        itemID)
    (UINT        itemAction)
    (UINT        itemState)
    (HWND        hwndItem)
    (HDC         hDC)
    (struct RECT rcItem)
    (ULONG_PTR   itemData))

  (define-constant WM_MEASUREITEM #x002C)
  (define-constant WM_DELETEITEM #x002D)
  (define-constant WM_VKEYTOITEM #x002E)
  (define-constant WM_CHARTOITEM #x002F)
  (define-constant WM_SETFONT #x0030)
  (define-constant WM_GETFONT #x0031)
  (define-constant WM_SETHOTKEY #x0032)
  (define-constant WM_GETHOTKEY #x0033)
  (define-constant WM_QUERYDRAGICON #x0037)
  (define-constant WM_COMPAREITEM #x0039)
  (define-constant WM_GETOBJECT #x003D)
  (define-constant WM_COMPACTING #x0041)
  (define-constant WM_COMMNOTIFY #x0044)
  (define-constant WM_WINDOWPOSCHANGING #x0046)
  (define-constant WM_WINDOWPOSCHANGED #x0047)
  (define-constant WM_POWER #x0048)
  (define-constant WM_COPYDATA #x004A)
  (define-constant WM_CANCELJOURNAL #x004B)
  (define-constant WM_NOTIFY #x004E)
  (define-constant WM_INPUTLANGCHANGEREQUEST #x0050)
  (define-constant WM_INPUTLANGCHANGE #x0051)
  (define-constant WM_TCARD #x0052)
  (define-constant WM_HELP #x0053)
  (define-constant WM_USERCHANGED #x0054)
  (define-constant WM_NOTIFYFORMAT #x0055)
  (define-constant WM_CONTEXTMENU #x007B)
  (define-constant WM_STYLECHANGING #x007C)
  (define-constant WM_STYLECHANGED #x007D)
  (define-constant WM_DISPLAYCHANGE #x007E)
  (define-constant WM_GETICON #x007F)
  (define-constant WM_SETICON #x0080)
  (define-constant WM_NCCREATE #x0081)
  (define-constant WM_NCDESTROY #x0082)
  (define-constant WM_NCCALCSIZE #x0083)
  (define-constant WM_NCHITTEST #x0084)
  (define-constant WM_NCPAINT #x0085)
  (define-constant WM_NCACTIVATE #x0086)
  (define-constant WM_GETDLGCODE #x0087)
  (define-constant WM_SYNCPAINT #x0088)
  (define-constant WM_NCMOUSEMOVE #x00A0)
  (define-constant WM_NCLBUTTONDOWN #x00A1)
  (define-constant WM_NCLBUTTONUP #x00A2)
  (define-constant WM_NCLBUTTONDBLCLK #x00A3)
  (define-constant WM_NCRBUTTONDOWN #x00A4)
  (define-constant WM_NCRBUTTONUP #x00A5)
  (define-constant WM_NCRBUTTONDBLCLK #x00A6)
  (define-constant WM_NCMBUTTONDOWN #x00A7)
  (define-constant WM_NCMBUTTONUP #x00A8)
  (define-constant WM_NCMBUTTONDBLCLK #x00A9)
  (define-constant WM_NCXBUTTONDOWN #x00AB)
  (define-constant WM_NCXBUTTONUP #x00AC)
  (define-constant WM_NCXBUTTONDBLCLK #x00AD)
  (define-constant WM_INPUT #x00FF)
  (define-constant WM_KEYFIRST #x0100)
  (define-constant WM_KEYDOWN #x0100)
  (define-constant WM_KEYUP #x0101)
  (define-constant WM_CHAR #x0102)
  (define-constant WM_DEADCHAR #x0103)
  (define-constant WM_SYSKEYDOWN #x0104)
  (define-constant WM_SYSKEYUP #x0105)
  (define-constant WM_SYSCHAR #x0106)
  (define-constant WM_SYSDEADCHAR #x0107)
  (define-constant WM_UNICHAR #x0109)
  (define-constant WM_KEYLAST #x0109)
  (define-constant WM_IME_STARTCOMPOSITION #x010D)
  (define-constant WM_IME_ENDCOMPOSITION #x010E)
  (define-constant WM_IME_COMPOSITION #x010F)
  (define-constant WM_IME_KEYLAST #x010F)
  (define-constant WM_INITDIALOG #x0110)
  (define-constant WM_COMMAND #x0111)
  (define-constant WM_SYSCOMMAND #x0112)
  (define-constant WM_TIMER #x0113)
  (define-constant WM_HSCROLL #x0114)
  (define-constant WM_VSCROLL #x0115)
  (define-constant WM_INITMENU #x0116)
  (define-constant WM_INITMENUPOPUP #x0117)
  (define-constant WM_MENUSELECT #x011F)
  (define-constant WM_MENUCHAR #x0120)
  (define-constant WM_ENTERIDLE #x0121)
  (define-constant WM_MENURBUTTONUP #x0122)
  (define-constant WM_MENUDRAG #x0123)
  (define-constant WM_MENUGETOBJECT #x0124)
  (define-constant WM_UNINITMENUPOPUP #x0125)
  (define-constant WM_MENUCOMMAND #x0126)
  (define-constant WM_CHANGEUISTATE #x0127)
  (define-constant WM_UPDATEUISTATE #x0128)
  (define-constant WM_QUERYUISTATE #x0129)

  (define-constant WM_CTLCOLORMSGBOX #x0132)
  (define-constant WM_CTLCOLOREDIT #x0133)
  (define-constant WM_CTLCOLORLISTBOX #x0134)
  (define-constant WM_CTLCOLORBTN #x0135)
  (define-constant WM_CTLCOLORDLG #x0136)
  (define-constant WM_CTLCOLORSCROLLBAR #x0137)
  (define-constant WM_CTLCOLORSTATIC #x0138)

  (define-constant WM_MOUSEFIRST #x0200)
  (define-constant WM_MOUSEMOVE #x0200)
  (define-constant WM_LBUTTONDOWN #x0201)
  (define-constant WM_LBUTTONUP #x0202)
  (define-constant WM_LBUTTONDBLCLK #x0203)
  (define-constant WM_RBUTTONDOWN #x0204)
  (define-constant WM_RBUTTONUP #x0205)
  (define-constant WM_RBUTTONDBLCLK #x0206)
  (define-constant WM_MBUTTONDOWN #x0207)
  (define-constant WM_MBUTTONUP #x0208)
  (define-constant WM_MBUTTONDBLCLK #x0209)
  (define-constant WM_MOUSEWHEEL #x020A)
  (define-constant WM_XBUTTONDOWN #x020B)
  (define-constant WM_XBUTTONUP #x020C)
  (define-constant WM_XBUTTONDBLCLK #x020D)
  (define-constant WM_MOUSELAST #x020D)

  (define-constant WM_PARENTNOTIFY #x0210)
  (define-constant WM_ENTERMENULOOP #x0211)
  (define-constant WM_EXITMENULOOP #x0212)
  (define-constant WM_NEXTMENU #x0213)
  (define-constant WM_SIZING #x0214)
  (define-constant WM_CAPTURECHANGED #x0215)
  (define-constant WM_MOVING #x0216)
  (define-constant WM_POWERBROADCAST #x0218)
  (define-constant WM_DEVICECHANGE #x0219)

  (define-constant WM_MDICREATE #x0220)
  (define-constant WM_MDIDESTROY #x0221)
  (define-constant WM_MDIACTIVATE #x0222)
  (define-constant WM_MDIRESTORE #x0223)
  (define-constant WM_MDINEXT #x0224)
  (define-constant WM_MDIMAXIMIZE #x0225)
  (define-constant WM_MDITILE #x0226)
  (define-constant WM_MDICASCADE #x0227)
  (define-constant WM_MDIICONARRANGE #x0228)
  (define-constant WM_MDIGETACTIVE #x0229)

  (define-constant WM_MDISETMENU #x0230)
  (define-constant WM_ENTERSIZEMOVE #x0231)
  (define-constant WM_EXITSIZEMOVE #x0232)
  (define-constant WM_DROPFILES #x0233)
  (define-constant WM_MDIREFRESHMENU #x0234)
  ;; #if WINVER >= #x0602)
  (define-constant WM_POINTERDEVICECHANGE #x238)
  (define-constant WM_POINTERDEVICEINRANGE #x239)
  (define-constant WM_POINTERDEVICEOUTOFRANGE #x23a)
  ;; #endif
  ;; #if WINVER >= #x0601)
  (define-constant WM_TOUCH #x0240)
  ;; #endif
  ;; #if WINVER >= #x0602)
  (define-constant WM_NCPOINTERUPDATE #x0241)
  (define-constant WM_NCPOINTERDOWN #x0242)
  (define-constant WM_NCPOINTERUP #x0243)
  (define-constant WM_POINTERUPDATE #x0245)
  (define-constant WM_POINTERDOWN #x0246)
  (define-constant WM_POINTERUP #x0247)
  (define-constant WM_POINTERENTER #x0249)
  (define-constant WM_POINTERLEAVE #x024a)
  (define-constant WM_POINTERACTIVATE #x024b)
  (define-constant WM_POINTERCAPTURECHANGED #x024c)
  (define-constant WM_TOUCHHITTESTING #x024d)
  (define-constant WM_POINTERWHEEL #x024e)
  (define-constant WM_POINTERHWHEEL #x024f)
  ;; #endif

  (define-constant WM_IME_SETCONTEXT #x0281)
  (define-constant WM_IME_NOTIFY #x0282)
  (define-constant WM_IME_CONTROL #x0283)
  (define-constant WM_IME_COMPOSITIONFULL #x0284)
  (define-constant WM_IME_SELECT #x0285)
  (define-constant WM_IME_CHAR #x0286)
  (define-constant WM_IME_REQUEST #x0288)
  (define-constant WM_IME_KEYDOWN #x0290)
  (define-constant WM_IME_KEYUP #x0291)

  (define-constant WM_MOUSEHOVER #x02A1)
  (define-constant WM_MOUSELEAVE #x02A3)
  (define-constant WM_NCMOUSEHOVER #x02A0)
  (define-constant WM_NCMOUSELEAVE #x02A2)
  (define-constant WM_WTSSESSION_CHANGE #x02B1)
  (define-constant WM_TABLET_FIRST #x02c0)
  (define-constant WM_TABLET_LAST #x02df)
  (define-constant WM_CUT #x0300)
  (define-constant WM_COPY #x0301)
  (define-constant WM_PASTE #x0302)
  (define-constant WM_CLEAR #x0303)
  (define-constant WM_UNDO #x0304)
  (define-constant WM_RENDERFORMAT #x0305)
  (define-constant WM_RENDERALLFORMATS #x0306)
  (define-constant WM_DESTROYCLIPBOARD #x0307)
  (define-constant WM_DRAWCLIPBOARD #x0308)
  (define-constant WM_PAINTCLIPBOARD #x0309)
  (define-constant WM_VSCROLLCLIPBOARD #x030A)
  (define-constant WM_SIZECLIPBOARD #x030B)
  (define-constant WM_ASKCBFORMATNAME #x030C)
  (define-constant WM_CHANGECBCHAIN #x030D)
  (define-constant WM_HSCROLLCLIPBOARD #x030E)
  (define-constant WM_QUERYNEWPALETTE #x030F)
  (define-constant WM_PALETTEISCHANGING #x0310)
  (define-constant WM_PALETTECHANGED #x0311)
  (define-constant WM_HOTKEY #x0312)
  (define-constant WM_PRINT #x0317)
  (define-constant WM_PRINTCLIENT #x0318)
  (define-constant WM_APPCOMMAND #x0319)
  (define-constant WM_THEMECHANGED #x031A)
  (define-constant WM_CLIPBOARDUPDATE #x031d)
  ;; #if _WIN32_WINNT >= #x0600)
  (define-constant WM_DWMCOMPOSITIONCHANGED #x031e)
  (define-constant WM_DWMNCRENDERINGCHANGED #x031f)
  (define-constant WM_DWMCOLORIZATIONCOLORCHANGED #x0320)
  (define-constant WM_DWMWINDOWMAXIMIZEDCHANGE #x0321)
  ;; #endif
  ;; #if _WIN32_WINNT >= #x0601)
  (define-constant WM_DWMSENDICONICTHUMBNAIL #x0323)
  (define-constant WM_DWMSENDICONICLIVEPREVIEWBITMAP #x0326)
  ;; #endif
  ;; #if WINVER >= #x0600)
  (define-constant WM_GETTITLEBARINFOEX #x033f)
  ;; #endif
  (define-constant WM_HANDHELDFIRST #x0358)
  (define-constant WM_HANDHELDLAST #x035F)
  (define-constant WM_AFXFIRST #x0360)
  (define-constant WM_AFXLAST #x037F)
  (define-constant WM_PENWINFIRST #x0380)
  (define-constant WM_PENWINLAST #x038F)
  (define-constant WM_APP #x8000)
  (define-constant WM_USER #x0400)

  ;; EM
  (define-constant EM_GETSEL #x00B0)
  (define-constant EM_SETSEL #x00B1)
  (define-constant EM_GETRECT #x00B2)
  (define-constant EM_SETRECT #x00B3)
  (define-constant EM_SETRECTNP #x00B4)
  (define-constant EM_SCROLL #x00B5)
  (define-constant EM_LINESCROLL #x00B6)
  (define-constant EM_SCROLLCARET #x00B7)
  (define-constant EM_GETMODIFY #x00B8)
  (define-constant EM_SETMODIFY #x00B9)
  (define-constant EM_GETLINECOUNT #x00BA)
  (define-constant EM_LINEINDEX #x00BB)
  (define-constant EM_SETHANDLE #x00BC)
  (define-constant EM_GETHANDLE #x00BD)
  (define-constant EM_GETTHUMB #x00BE)
  (define-constant EM_LINELENGTH #x00C1)
  (define-constant EM_REPLACESEL #x00C2)
  (define-constant EM_GETLINE #x00C4)
  (define-constant EM_LIMITTEXT #x00C5)
  (define-constant EM_CANUNDO #x00C6)
  (define-constant EM_UNDO #x00C7)
  (define-constant EM_FMTLINES #x00C8)
  (define-constant EM_LINEFROMCHAR #x00C9)
  (define-constant EM_SETTABSTOPS #x00CB)
  (define-constant EM_SETPASSWORDCHAR #x00CC)
  (define-constant EM_EMPTYUNDOBUFFER #x00CD)
  (define-constant EM_GETFIRSTVISIBLELINE #x00CE)
  (define-constant EM_SETREADONLY #x00CF)
  (define-constant EM_SETWORDBREAKPROC #x00D0)
  (define-constant EM_GETWORDBREAKPROC #x00D1)
  (define-constant EM_GETPASSWORDCHAR #x00D2)
  (define-constant EM_SETMARGINS #x00D3)
  (define-constant EM_GETMARGINS #x00D4)
  (define-constant EM_SETLIMITTEXT EM_LIMITTEXT)
  (define-constant EM_GETLIMITTEXT #x00D5)
  (define-constant EM_POSFROMCHAR #x00D6)
  (define-constant EM_CHARFROMPOS #x00D7)
  (define-constant EM_SETIMESTATUS #x00D8)
  (define-constant EM_GETIMESTATUS #x00D9)

  (define-constant MF_ENABLED 0)
  (define-constant MF_GRAYED 1)
  (define-constant MF_DISABLED 2)
  (define-constant MF_BITMAP 4)
  (define-constant MF_CHECKED 8)
  (define-constant MF_POPUP 16)
  (define-constant MF_MENUBARBREAK 32)
  (define-constant MF_MENUBREAK 64)
  (define-constant MF_OWNERDRAW 256)
  (define-constant MF_STRING 0)

  ;; window style
  (define-constant CS_VREDRAW 1)
  (define-constant CS_HREDRAW 2)

  (define-constant CW_USEDEFAULT #x80000000)

  (define-constant WS_OVERLAPPED #x00000000)
  (define-constant WS_POPUP #x80000000)
  (define-constant WS_CHILD #x40000000)
  (define-constant WS_MINIMIZE #x20000000)
  (define-constant WS_VISIBLE #x10000000)
  (define-constant WS_DISABLED #x08000000)
  (define-constant WS_CLIPSIBLINGS #x04000000)
  (define-constant WS_CLIPCHILDREN #x02000000)
  (define-constant WS_MAXIMIZE #x01000000)
  (define-constant WS_CAPTION #x00C00000)
  (define-constant WS_BORDER #x00800000)
  (define-constant WS_DLGFRAME #x00400000)
  (define-constant WS_VSCROLL #x00200000)
  (define-constant WS_HSCROLL #x00100000)
  (define-constant WS_SYSMENU #x00080000)
  (define-constant WS_THICKFRAME #x00040000)
  (define-constant WS_GROUP #x00020000)
  (define-constant WS_TABSTOP #x00010000)
  (define-constant WS_MINIMIZEBOX #x00020000)
  (define-constant WS_MAXIMIZEBOX #x00010000)
  (define-constant WS_TILED WS_OVERLAPPED)
  (define-constant WS_ICONIC WS_MINIMIZE)
  (define-constant WS_SIZEBOX WS_THICKFRAME)
  (define-constant WS_OVERLAPPEDWINDOW
    (bitwise-ior WS_OVERLAPPED WS_CAPTION WS_SYSMENU
		 WS_THICKFRAME WS_MINIMIZEBOX WS_MAXIMIZEBOX))
  (define-constant WS_TILEDWINDOW WS_OVERLAPPEDWINDOW)
  (define-constant WS_POPUPWINDOW (bitwise-ior WS_POPUP WS_BORDER WS_SYSMENU))
  (define-constant WS_CHILDWINDOW WS_CHILD)

  (define-constant WS_EX_DLGMODALFRAME #x00000001)
  (define-constant WS_EX_NOPARENTNOTIFY #x00000004)
  (define-constant WS_EX_TOPMOST #x00000008)
  (define-constant WS_EX_ACCEPTFILES #x00000010)
  (define-constant WS_EX_TRANSPARENT #x00000020)
  (define-constant WS_EX_MDICHILD #x00000040)
  (define-constant WS_EX_TOOLWINDOW #x00000080)
  (define-constant WS_EX_WINDOWEDGE #x00000100)
  (define-constant WS_EX_CLIENTEDGE #x00000200)
  (define-constant WS_EX_CONTEXTHELP #x00000400)
  (define-constant WS_EX_RIGHT #x00001000)
  (define-constant WS_EX_LEFT #x00000000)
  (define-constant WS_EX_RTLREADING #x00002000)
  (define-constant WS_EX_LTRREADING #x00000000)
  (define-constant WS_EX_LEFTSCROLLBAR #x00004000)
  (define-constant WS_EX_RIGHTSCROLLBAR #x00000000)
  (define-constant WS_EX_CONTROLPARENT #x00010000)
  (define-constant WS_EX_STATICEDGE #x00020000)
  (define-constant WS_EX_APPWINDOW #x00040000)
  (define-constant WS_EX_OVERLAPPEDWINDOW (bitwise-ior WS_EX_WINDOWEDGE WS_EX_CLIENTEDGE))
  (define-constant WS_EX_PALETTEWINDOW (bitwise-ior WS_EX_WINDOWEDGE WS_EX_TOOLWINDOW WS_EX_TOPMOST))
  (define-constant WS_EX_LAYERED #x00080000)
  (define-constant WS_EX_NOINHERITLAYOUT #x00100000)
  (define-constant WS_EX_LAYOUTRTL #x00400000)
  (define-constant WS_EX_COMPOSITED #x02000000)
  (define-constant WS_EX_NOACTIVATE #x08000000)

  (define-constant ES_LEFT   	  #x0000)
  (define-constant ES_CENTER 	  #x0001)
  (define-constant ES_RIGHT  	  #x0002)
  (define-constant ES_MULTILINE   #x0004)
  (define-constant ES_UPPERCASE   #x0008)
  (define-constant ES_LOWERCASE   #x0010)
  (define-constant ES_PASSWORD    #x0020)
  (define-constant ES_AUTOVSCROLL #x0040)
  (define-constant ES_AUTOHSCROLL #x0080)
  (define-constant ES_NOHIDESEL   #x0100)
  (define-constant ES_OEMCONVERT  #x0400)
  (define-constant ES_READONLY    #x0800)
  (define-constant ES_WANTRETURN  #x1000)
  (define-constant ES_NUMBER      #x2000)

  (define-constant EM_GETSEL               #x00B0)
  (define-constant EM_SETSEL               #x00B1)
  (define-constant EM_GETRECT              #x00B2)
  (define-constant EM_SETRECT              #x00B3)
  (define-constant EM_SETRECTNP            #x00B4)
  (define-constant EM_SCROLL               #x00B5)
  (define-constant EM_LINESCROLL           #x00B6)
  (define-constant EM_SCROLLCARET          #x00B7)
  (define-constant EM_GETMODIFY            #x00B8)
  (define-constant EM_SETMODIFY            #x00B9)
  (define-constant EM_GETLINECOUNT         #x00BA)
  (define-constant EM_LINEINDEX            #x00BB)
  (define-constant EM_SETHANDLE            #x00BC)
  (define-constant EM_GETHANDLE            #x00BD)
  (define-constant EM_GETTHUMB             #x00BE)
  (define-constant EM_LINELENGTH           #x00C1)
  (define-constant EM_REPLACESEL           #x00C2)
  (define-constant EM_GETLINE              #x00C4)
  (define-constant EM_LIMITTEXT            #x00C5)
  (define-constant EM_CANUNDO              #x00C6)
  (define-constant EM_UNDO                 #x00C7)
  (define-constant EM_FMTLINES             #x00C8)
  (define-constant EM_LINEFROMCHAR         #x00C9)
  (define-constant EM_SETTABSTOPS          #x00CB)
  (define-constant EM_SETPASSWORDCHAR      #x00CC)
  (define-constant EM_EMPTYUNDOBUFFER      #x00CD)
  (define-constant EM_GETFIRSTVISIBLELINE  #x00CE)
  (define-constant EM_SETREADONLY          #x00CF)
  (define-constant EM_SETWORDBREAKPROC     #x00D0)
  (define-constant EM_GETWORDBREAKPROC     #x00D1)
  (define-constant EM_GETPASSWORDCHAR      #x00D2)
  (define-constant EM_SETMARGINS           #x00D3)
  (define-constant EM_GETMARGINS           #x00D4)
  (define-constant EM_SETLIMITTEXT         EM_LIMITTEXT) ;win40 Name change
  (define-constant EM_GETLIMITTEXT         #x00D5)
  (define-constant EM_POSFROMCHAR          #x00D6)
  (define-constant EM_CHARFROMPOS          #x00D7)
  (define-constant EM_SETIMESTATUS         #x00D8)
  (define-constant EM_GETIMESTATUS         #x00D9)
  (define-constant EM_ENABLEFEATURE        #x00DA)

  (define-constant EN_SETFOCUS     #x0100)
  (define-constant EN_KILLFOCUS    #x0200)
  (define-constant EN_CHANGE 	   #x0300)
  (define-constant EN_UPDATE 	   #x0400)
  (define-constant EN_ERRSPACE     #x0500)
  (define-constant EN_MAXTEXT 	   #x0501)
  (define-constant EN_HSCROLL 	   #x0601)
  (define-constant EN_VSCROLL 	   #x0602)
  (define-constant EN_ALIGN_LTR_EC #x0700)
  (define-constant EN_ALIGN_RTL_EC #x0701)

  (define-constant SB_HORZ 0)
  (define-constant SB_VERT 1)
  (define-constant SB_CTL 2)
  (define-constant SB_BOTH 3)

  (define-constant SB_LINEUP 0)
  (define-constant SB_LINELEFT 0)
  (define-constant SB_LINEDOWN 1)
  (define-constant SB_LINERIGHT 1)
  (define-constant SB_PAGEUP 2)
  (define-constant SB_PAGELEFT 2)
  (define-constant SB_PAGEDOWN 3)
  (define-constant SB_PAGERIGHT 3)
  (define-constant SB_THUMBPOSITION 4)
  (define-constant SB_THUMBTRACK 5)
  (define-constant SB_TOP 6)
  (define-constant SB_LEFT 6)
  (define-constant SB_BOTTOM 7)
  (define-constant SB_RIGHT 7)
  (define-constant SB_ENDSCROLL 8)

  (define-constant SW_HIDE 0)
  (define-constant SW_SHOWNORMAL 1)
  (define-constant SW_NORMAL 1)
  (define-constant SW_SHOWMINIMIZED 2)
  (define-constant SW_SHOWMAXIMIZED 3)
  (define-constant SW_MAXIMIZE 3)
  (define-constant SW_SHOWNOACTIVATE 4)
  (define-constant SW_SHOW 5)
  (define-constant SW_MINIMIZE 6)
  (define-constant SW_SHOWMINNOACTIVE 7)
  (define-constant SW_SHOWNA 8)
  (define-constant SW_RESTORE 9)
  (define-constant SW_SHOWDEFAULT 10)
  (define-constant SW_FORCEMINIMIZE 11)
  (define-constant SW_MAX 11)
  (define-constant HIDE_WINDOW 0)
  (define-constant SHOW_OPENWINDOW 1)
  (define-constant SHOW_ICONWINDOW 2)
  (define-constant SHOW_FULLSCREEN 3)
  (define-constant SHOW_OPENNOACTIVATE 4)
  (define-constant SW_PARENTCLOSING 1)
  (define-constant SW_OTHERZOOM 2)
  (define-constant SW_PARENTOPENING 3)
  (define-constant SW_OTHERUNZOOM 4)

  (define-constant MB_ICONQUESTION 32)
  (define-constant MB_OK 0)
  (define-constant MB_OKCANCEL 1)
  (define-constant MB_YESNOCANCEL 3)
  (define-constant MB_YESNO 4)


  (define-constant IDOK 1)
  (define-constant IDCANCEL 2)
  (define-constant IDABORT 3)
  (define-constant IDRETRY 4)
  (define-constant IDIGNORE 5)
  (define-constant IDYES 6)
  (define-constant IDNO 7)

  ;; load option
  (define-constant IMAGE_BITMAP 0)
  (define-constant IMAGE_ICON 1)
  (define-constant IMAGE_CURSOR 2)
  (define-constant IMAGE_ENHMETAFILE 3)

  ;; OEM images
  (define-export OBM_CLOSE           (integer->pointer 32754))
  (define-export OBM_UPARROW         (integer->pointer 32753))
  (define-export OBM_DNARROW         (integer->pointer 32752))
  (define-export OBM_RGARROW         (integer->pointer 32751))
  (define-export OBM_LFARROW         (integer->pointer 32750))
  (define-export OBM_REDUCE          (integer->pointer 32749))
  (define-export OBM_ZOOM            (integer->pointer 32748))
  (define-export OBM_RESTORE         (integer->pointer 32747))
  (define-export OBM_REDUCED         (integer->pointer 32746))
  (define-export OBM_ZOOMD           (integer->pointer 32745))
  (define-export OBM_RESTORED        (integer->pointer 32744))
  (define-export OBM_UPARROWD        (integer->pointer 32743))
  (define-export OBM_DNARROWD        (integer->pointer 32742))
  (define-export OBM_RGARROWD        (integer->pointer 32741))
  (define-export OBM_LFARROWD        (integer->pointer 32740))
  (define-export OBM_MNARROW         (integer->pointer 32739))
  (define-export OBM_COMBO           (integer->pointer 32738))
  (define-export OBM_UPARROWI        (integer->pointer 32737))
  (define-export OBM_DNARROWI        (integer->pointer 32736))
  (define-export OBM_RGARROWI        (integer->pointer 32735))
  (define-export OBM_LFARROWI        (integer->pointer 32734))
  (define-export OBM_OLD_CLOSE       (integer->pointer 32767))
  (define-export OBM_SIZE            (integer->pointer 32766))
  (define-export OBM_OLD_UPARROW     (integer->pointer 32765))
  (define-export OBM_OLD_DNARROW     (integer->pointer 32764))
  (define-export OBM_OLD_RGARROW     (integer->pointer 32763))
  (define-export OBM_OLD_LFARROW     (integer->pointer 32762))
  (define-export OBM_BTSIZE          (integer->pointer 32761))
  (define-export OBM_CHECK           (integer->pointer 32760))
  (define-export OBM_CHECKBOXES      (integer->pointer 32759))
  (define-export OBM_BTNCORNERS      (integer->pointer 32758))
  (define-export OBM_OLD_REDUCE      (integer->pointer 32757))
  (define-export OBM_OLD_ZOOM        (integer->pointer 32756))
  (define-export OBM_OLD_RESTORE     (integer->pointer 32755))
  (define-export OCR_NORMAL          (integer->pointer 32512))
  (define-export OCR_IBEAM           (integer->pointer 32513))
  (define-export OCR_WAIT            (integer->pointer 32514))
  (define-export OCR_CROSS           (integer->pointer 32515))
  (define-export OCR_UP              (integer->pointer 32516))
  (define-export OCR_SIZE            (integer->pointer 32640)) ;/* OBSOLETE: use OCR_SIZEALL */
  (define-export OCR_ICON            (integer->pointer 32641)) ;/* OBSOLETE: use OCR_NORMAL */
  (define-export OCR_SIZENWSE        (integer->pointer 32642))
  (define-export OCR_SIZENESW        (integer->pointer 32643))
  (define-export OCR_SIZEWE          (integer->pointer 32644))
  (define-export OCR_SIZENS          (integer->pointer 32645))
  (define-export OCR_SIZEALL         (integer->pointer 32646))
  (define-export OCR_ICOCUR          (integer->pointer 32647)) ;/* OBSOLETE: use OIC_WINLOGO */
  (define-export OCR_NO              (integer->pointer 32648))
  (define-export OCR_HAND            (integer->pointer 32649))
  (define-export OCR_APPSTARTING     (integer->pointer 32650))
  (define-export OIC_SAMPLE          (integer->pointer 32512))
  (define-export OIC_HAND            (integer->pointer 32513))
  (define-export OIC_QUES            (integer->pointer 32514))
  (define-export OIC_BANG            (integer->pointer 32515))
  (define-export OIC_NOTE            (integer->pointer 32516))
  (define-export OIC_WINLOGO         (integer->pointer 32517))
  (define-export OIC_WARNING         OIC_BANG)
  (define-export OIC_ERROR           OIC_HAND)
  (define-export OIC_INFORMATION     OIC_NOTE)
  (define-export OIC_SHIELD          (integer->pointer 32518))

  (define-constant LR_DEFAULTCOLOR     #x00000000)
  (define-constant LR_MONOCHROME       #x00000001)
  (define-constant LR_COLOR            #x00000002)
  (define-constant LR_COPYRETURNORG    #x00000004)
  (define-constant LR_COPYDELETEORG    #x00000008)
  (define-constant LR_LOADFROMFILE     #x00000010)
  (define-constant LR_LOADTRANSPARENT  #x00000020)
  (define-constant LR_DEFAULTSIZE      #x00000040)
  (define-constant LR_VGACOLOR         #x00000080)
  (define-constant LR_LOADMAP3DCOLORS  #x00001000)
  (define-constant LR_CREATEDIBSECTION #x00002000)
  (define-constant LR_COPYFROMRESOURCE #x00004000)
  (define-constant LR_SHARED           #x00008000)

  (define IDI_APPLICATION (integer->pointer 32512))
  (define IDI_HAND (integer->pointer 32513))
  (define IDI_QUESTION (integer->pointer 32514))
  (define IDI_EXCLAMATION (integer->pointer 32515))
  (define IDI_ASTERISK (integer->pointer 32516))
  (define IDI_WINLOGO (integer->pointer 32517))
  (define IDI_SHIELD (integer->pointer 32518))

  (define-constant MIIM_STATE 1)
  (define-constant MIIM_ID 2)
  (define-constant MIIM_SUBMENU 4)
  (define-constant MIIM_CHECKMARKS 8)
  (define-constant MIIM_TYPE 16)

  (define-constant MFT_STRING 0)

  (define IDC_ARROW    	  (integer->pointer 32512))
  (define IDC_IBEAM    	  (integer->pointer 32513))
  (define IDC_WAIT     	  (integer->pointer 32514))
  (define IDC_CROSS    	  (integer->pointer 32515))
  (define IDC_UPARROW  	  (integer->pointer 32516))
  (define IDC_SIZE     	  (integer->pointer 32640))
  (define IDC_ICON     	  (integer->pointer 32641))
  (define IDC_SIZENWSE 	  (integer->pointer 32642))
  (define IDC_SIZENESW 	  (integer->pointer 32643))
  (define IDC_SIZEWE   	  (integer->pointer 32644))
  (define IDC_SIZENS   	  (integer->pointer 32645))
  (define IDC_SIZEALL  	  (integer->pointer 32646))
  (define IDC_NO       	  (integer->pointer 32648))
  (define IDC_HAND     	  (integer->pointer 32649))
  (define IDC_APPSTARTING (integer->pointer 32650))
  (define IDC_HELP        (integer->pointer 32651))

  (define-c-struct WNDCLASSEX
    (UINT         cbSize)
    (UINT         style)
    (WNDPROC      lpfnWndProc)
    (int          cbClsExtra)
    (int          cbWndExtra)
    (HINSTANCE    hInstance)
    (HICON        hIcon)
    (HCURSOR      hCursor)
    (HBRUSH       hbrBackground)
    (LPCWSTR      lpszMenuName)
    (LPCWSTR      lpszClassName)
    (HICON        hIconSm))

  (define-constant LPWNDCLASSEX void*)
  (define-constant PWNDCLASSEX void*)

  (define-c-struct PAINTSTRUCT
    (HDC  hdc)
    (BOOL fErace)
    (struct RECT rcPaint)
    (BOOL fRestore)
    (BOOL fIncUpdate)
    (BYTE array 32 rgbReserved))
  (define-constant LPPAINTSTRUCT void*)

  (define-c-struct MSG
      (HWND   hwnd)
      (UINT   message)
      (WPARAM wParam)
      (LPARAM lParam)
      (DWORD  time)
      (struct POINT pt))
  (define-constant LPMSG void*)
  (define-constant PMSG void*)

  (define-c-struct CREATESTRUCT
    (LPVOID	lpCreateParams)
    (HINSTANCE	hInstance)
    (HMENU	hMenu)
    (HWND	hwndParent)
    (int	cy)
    (int	cx)
    (int	y)
    (int	x)
    (LONG	style)
    (LPCSTR	lpszName)
    (LPCSTR	lpszClass)
    (DWORD	dwExStyle))
  (define-constant LPCREATESTRUCT void*)

  (define-c-struct MENUITEMINFO
    (UINT      cbSize)
    (UINT      fMask)
    (UINT      fType)
    (UINT      fState)
    (UINT      wID)
    (HMENU     hSubMenu)
    (HBITMAP   hbmpChecked)
    (HBITMAP   hbmpUnchecked)
    (ULONG_PTR dwItemData)
    (LPWSTR    dwTypeData)
    (UINT      cch)
    (HBITMAP   hbmpItem))
  (define-c-typedef MENUITEMINFO (* LPMENUITEMINFO) (* LPCMENUITEMINFO))
  
  (define-constant MIIM_STATE       #x00000001)
  (define-constant MIIM_ID          #x00000002)
  (define-constant MIIM_SUBMENU     #x00000004)
  (define-constant MIIM_CHECKMARKS  #x00000008)
  (define-constant MIIM_TYPE        #x00000010)
  (define-constant MIIM_DATA        #x00000020)
  (define-constant MIIM_STRING      #x00000040)
  (define-constant MIIM_BITMAP      #x00000080)
  (define-constant MIIM_FTYPE       #x00000100)
  
  (define-c-struct NMHDR
    (HWND     hwndFrom)
    (UINT_PTR idFrom)
    (UINT     code))
  (define-constant LPNMHDR void*)

  (define message-box
    (c-function user32
		int MessageBoxW (HWND LPCWSTR LPCWSTR UINT)))

  (define create-window-ex
    (c-function user32
		HWND CreateWindowExW
		(DWORD LPCWSTR LPCWSTR DWORD UINT UINT UINT
		       UINT HWND HMENU HINSTANCE LPVOID)))

  (define (create-window a b c d e f g h i j k)
    (create-window-ex 0 a b c d e f g h i j k))

  (define load-icon
    (c-function user32
		HICON LoadIconW (HINSTANCE LPCWSTR)))
  (define destroy-icon
    (c-function user32 BOOL DestroyIcon (HICON)))

  (define load-cursor
    (c-function user32
		HCURSOR LoadCursorW (HINSTANCE LPCWSTR)))

  (define load-image
    (c-function user32
		HANDLE LoadImageW (HINSTANCE LPCWSTR UINT int int UINT)))

  (define register-class-ex
    (c-function user32
		ATOM RegisterClassExW (void*)))

  (define get-class-info-ex
    (c-function user32
		BOOL GetClassInfoExW (void* LPCWSTR void*)))

  (define post-quit-message (c-function user32 void PostQuitMessage (int)))

  (define def-window-proc
    (c-function user32 LRESULT DefWindowProcW (HWND UINT WPARAM LPARAM)))

  (define call-window-proc
    (c-function user32
		LRESULT CallWindowProcW (WNDPROC HWND UINT WPARAM LPARAM)))

  (define show-window (c-function user32 BOOL ShowWindow (HWND int)))

  (define update-window (c-function user32 BOOL UpdateWindow (HWND)))

  (define get-parent (c-function user32 HWND GetParent (HWND)))

  (define is-window-visible (c-function user32 BOOL IsWindowVisible (HWND)))

  (define get-message
    (c-function user32 int GetMessageW (LPMSG HWND UINT UINT)))

  (define peek-message
    (c-function user32 BOOL GetMessageW (LPMSG HWND UINT UINT UINT)))

  (define translate-message (c-function user32 BOOL TranslateMessage (void*)))

  (define dispatch-message (c-function user32 LONG DispatchMessageW (void*)))

  (define get-message-pos (c-function user32 DWORD GetMessagePos ()))
  
  (define tabbed-text-out
    (c-function user32 LONG TabbedTextOutW
		(HDC int int LPCWSTR int int LPINT int)))

  (define get-dc (c-function user32 HDC GetDC (HWND)))

  (define get-dc-ex (c-function user32 HDC GetDCEx (HWND HRGN DWORD)))

  (define release-dc (c-function user32 int ReleaseDC (HWND HDC)))

  (define begin-paint (c-function user32 HDC BeginPaint (HWND LPPAINTSTRUCT)))

  (define end-paint (c-function user32 BOOL EndPaint (HWND void*)))

  (define get-client-rect
    (c-function user32 BOOL GetClientRect (HWND LPRECT)))
  (define get-update-rect
    (c-function user32 BOOL GetUpdateRect (HWND LPRECT BOOL)))
  (define invalidate-rect
    (c-function user32 BOOL InvalidateRect (HWND LPRECT BOOL)))
  (define inflate-rect
    (c-function user32 BOOL InflateRect (LPRECT int int)))

  (define is-rect-empty (c-function user32 BOOL IsRectEmpty (LPRECT)))

  (define fill-rect (c-function user32 int FillRect (HDC LPRECT HBRUSH)))
  (define set-rect (c-function user32 BOOL SetRect (LPRECT int int int int)))

  (define move-window
    (c-function user32 BOOL MoveWindow (HWND int int int int BOOL)))

  (define destroy-window (c-function user32 BOOL DestroyWindow (HWND)))

  (define send-message
    (c-function user32 LRESULT SendMessageW (HWND UINT WPARAM LPARAM)))

  (define post-message
    (c-function user32 LRESULT PostMessageW (HWND UINT WPARAM LPARAM)))

  (define create-menu (c-function user32 HMENU CreateMenu ()))

  (define create-popup-menu (c-function user32 HMENU CreateMenu ()))

  (define destroy-menu (c-function user32 BOOL DestroyMenu (HMENU)))

  (define append-menu
    (c-function user32 BOOL AppendMenuW (HMENU UINT UINT_PTR LPCWSTR)))

  (define delete-menu (c-function user32 BOOL DeleteMenu (HMENU UINT UINT)))

  (define get-system-menu (c-function user32 HMENU GetSystemMenu (HWND BOOL)))

  (define set-menu (c-function user32 BOOL SetMenu (HWND HMENU)))

  (define draw-menu-bar (c-function user32 BOOL DrawMenuBar (HWND)))

  (define insert-menu-item
    (c-function user32 BOOL InsertMenuItemW (HMENU UINT BOOL LPMENUITEMINFO)))

  (define set-menu-item-info
    (c-function user32 BOOL SetMenuItemInfoW (HMENU UINT BOOL LPCMENUITEMINFO)))

  (define draw-text
    (c-function user32 int DrawTextW (HDC LPCWSTR int LPRECT UINT)))

  (define-constant DT_TOP                      #x00000000)
  (define-constant DT_LEFT                     #x00000000)
  (define-constant DT_CENTER                   #x00000001)
  (define-constant DT_RIGHT                    #x00000002)
  (define-constant DT_VCENTER                  #x00000004)
  (define-constant DT_BOTTOM                   #x00000008)
  (define-constant DT_WORDBREAK                #x00000010)
  (define-constant DT_SINGLELINE               #x00000020)
  (define-constant DT_EXPANDTABS               #x00000040)
  (define-constant DT_TABSTOP                  #x00000080)
  (define-constant DT_NOCLIP                   #x00000100)
  (define-constant DT_EXTERNALLEADING          #x00000200)
  (define-constant DT_CALCRECT                 #x00000400)
  (define-constant DT_NOPREFIX                 #x00000800)
  (define-constant DT_INTERNAL                 #x00001000)
  (define-constant DT_EDITCONTROL              #x00002000)
  (define-constant DT_PATH_ELLIPSIS            #x00004000)
  (define-constant DT_END_ELLIPSIS             #x00008000)
  (define-constant DT_MODIFYSTRING             #x00010000)
  (define-constant DT_RTLREADING               #x00020000)
  (define-constant DT_WORD_ELLIPSIS            #x00040000)
  (define-constant DT_NOFULLWIDTHCHARBREAK     #x00080000)
  (define-constant DT_HIDEPREFIX               #x00100000)
  (define-constant DT_PREFIXONLY               #x00200000)
  
  (define set-window-text
    (c-function user32 BOOL SetWindowTextW (HWND LPCWSTR)))

  (define get-window-text
    (c-function user32 int GetWindowTextW (HWND LPWSTR int)))

  (define get-window-text-length
    (c-function user32 int GetWindowTextLengthW (HWND)))

  (define get-window-rect
    (c-function user32 BOOL GetWindowRect (HWND LPRECT)))

  (define set-window-pos
    (c-function user32 BOOL SetWindowPos (HWND HWND int int int int UINT)))

  (define set-cursor (c-function user32 HCURSOR SetCursor (HCURSOR)))
  (define get-cusor-pos (c-function user32 BOOL GetCursorPos (LPPOINT)))
  (define set-capture (c-function user32 HWND SetCapture (HWND)))
  (define release-capture (c-function user32 BOOL ReleaseCapture ()))

  (define set-focus (c-function user32 HWND SetFocus (HWND)))

  ;; button style
  (define-constant BS_PUSHBUTTON      #x00000000)
  (define-constant BS_DEFPUSHBUTTON   #x00000001)
  (define-constant BS_CHECKBOX        #x00000002)
  (define-constant BS_AUTOCHECKBOX    #x00000003)
  (define-constant BS_RADIOBUTTON     #x00000004)
  (define-constant BS_3STATE          #x00000005)
  (define-constant BS_AUTO3STATE      #x00000006)
  (define-constant BS_GROUPBOX        #x00000007)
  (define-constant BS_USERBUTTON      #x00000008)
  (define-constant BS_AUTORADIOBUTTON #x00000009)
  (define-constant BS_PUSHBOX         #x0000000A)
  (define-constant BS_OWNERDRAW       #x0000000B)
  (define-constant BS_TYPEMASK        #x0000000F)
  (define-constant BS_LEFTTEXT        #x00000020)
  (define-constant BS_TEXT            #x00000000)
  (define-constant BS_ICON            #x00000040)
  (define-constant BS_BITMAP   	      #x00000080)
  (define-constant BS_LEFT     	      #x00000100)
  (define-constant BS_RIGHT    	      #x00000200)
  (define-constant BS_CENTER   	      #x00000300)
  (define-constant BS_TOP      	      #x00000400)
  (define-constant BS_BOTTOM   	      #x00000800)
  (define-constant BS_VCENTER  	      #x00000C00)
  (define-constant BS_PUSHLIKE 	      #x00001000)
  (define-constant BS_MULTILINE       #x00002000)
  (define-constant BS_NOTIFY          #x00004000)
  (define-constant BS_FLAT            #x00008000)
  (define-constant BS_RIGHTBUTTON     BS_LEFTTEXT)

  (define-constant BN_CLICKED       0)
  (define-constant BN_PAINT         1)
  (define-constant BN_HILITE        2)
  (define-constant BN_UNHILITE      3)
  (define-constant BN_DISABLE       4)
  (define-constant BN_DOUBLECLICKED 5)
  (define-constant BN_PUSHED        BN_HILITE)
  (define-constant BN_UNPUSHED      BN_UNHILITE)
  (define-constant BN_DBLCLK        BN_DOUBLECLICKED)
  (define-constant BN_SETFOCUS      6)
  (define-constant BN_KILLFOCUS     7)

  (define-constant BM_GETCHECK #x00F0)
  (define-constant BM_SETCHECK #x00F1)
  (define-constant BM_GETSTATE #x00F2)
  (define-constant BM_SETSTATE #x00F3)
  (define-constant BM_SETSTYLE #x00F4)
  (define-constant BM_CLICK    #x00F5)
  (define-constant BM_GETIMAGE #x00F6)
  (define-constant BM_SETIMAGE #x00F7)

  (define-constant BST_UNCHECKED     #x0000)
  (define-constant BST_CHECKED       #x0001)
  (define-constant BST_INDETERMINATE #x0002)
  (define-constant BST_PUSHED        #x0004)
  (define-constant BST_FOCUS         #x0008)

  (cond-expand
   (64bit
    (define set-window-long-ptr
      (c-function user32 LONG SetWindowLongPtrW (HWND int LONG_PTR)))
    (define get-window-long-ptr
      (c-function user32 LONG_PTR GetWindowLongPtrW (HWND int))))
   (32bit
    ;; I want the signature the same like we can pass the pointer object
    (define set-window-long-ptr
      (c-function user32 LONG SetWindowLongW (HWND int LONG_PTR)))
    (define get-window-long-ptr
      (c-function user32 LONG_PTR GetWindowLongW (HWND int)))))

  (define-constant GWL_STYLE -16)
  (define-constant GWL_EXSTYLE -20)
  (define-constant GWLP_WNDPROC -4)
  (define-constant GWLP_HINSTANCE -6)
  (define-constant GWLP_HWNDPARENT -8)
  (define-constant GWLP_USERDATA -21)
  (define-constant GWLP_ID -12)

  ;; list box
  (define-constant LBS_NOTIFY #x0001)
  (define-constant LBS_SORT #x0002)
  (define-constant LBS_NOREDRAW #x0004)
  (define-constant LBS_MULTIPLESEL #x0008)
  (define-constant LBS_OWNERDRAWFIXED #x0010)
  (define-constant LBS_OWNERDRAWVARIABLE #x0020)
  (define-constant LBS_HASSTRINGS #x0040)
  (define-constant LBS_USETABSTOPS #x0080)
  (define-constant LBS_NOINTEGRALHEIGHT #x0100)
  (define-constant LBS_MULTICOLUMN #x0200)
  (define-constant LBS_WANTKEYBOARDINPUT #x0400)
  (define-constant LBS_EXTENDEDSEL #x0800)
  (define-constant LBS_DISABLENOSCROLL #x1000)
  (define-constant LBS_NODATA #x2000)
  (define-constant LBS_NOSEL #x4000)
  (define-constant LBS_COMBOBOX #x8000)
  (define-constant LBS_STANDARD (bitwise-ior LBS_NOTIFY
					     LBS_SORT WS_VSCROLL
					     WS_BORDER))


  (define-constant LB_ADDSTRING #x0180)
  (define-constant LB_INSERTSTRING #x0181)
  (define-constant LB_DELETESTRING #x0182)
  (define-constant LB_SELITEMRANGEEX #x0183)
  (define-constant LB_RESETCONTENT #x0184)
  (define-constant LB_SETSEL #x0185)
  (define-constant LB_SETCURSEL #x0186)
  (define-constant LB_GETSEL #x0187)
  (define-constant LB_GETCURSEL #x0188)
  (define-constant LB_GETTEXT #x0189)
  (define-constant LB_GETTEXTLEN #x018A)
  (define-constant LB_GETCOUNT #x018B)
  (define-constant LB_SELECTSTRING #x018C)
  (define-constant LB_DIR #x018D)
  (define-constant LB_GETTOPINDEX #x018E)
  (define-constant LB_FINDSTRING #x018F)
  (define-constant LB_GETSELCOUNT #x0190)
  (define-constant LB_GETSELITEMS #x0191)
  (define-constant LB_SETTABSTOPS #x0192)
  (define-constant LB_GETHORIZONTALEXTENT #x0193)
  (define-constant LB_SETHORIZONTALEXTENT #x0194)
  (define-constant LB_SETCOLUMNWIDTH #x0195)
  (define-constant LB_ADDFILE #x0196)
  (define-constant LB_SETTOPINDEX #x0197)
  (define-constant LB_GETITEMRECT #x0198)
  (define-constant LB_GETITEMDATA #x0199)
  (define-constant LB_SETITEMDATA #x019A)
  (define-constant LB_SELITEMRANGE #x019B)
  (define-constant LB_SETANCHORINDEX #x019C)
  (define-constant LB_GETANCHORINDEX #x019D)
  (define-constant LB_SETCARETINDEX #x019E)
  (define-constant LB_GETCARETINDEX #x019F)
  (define-constant LB_SETITEMHEIGHT #x01A0)
  (define-constant LB_GETITEMHEIGHT #x01A1)
  (define-constant LB_FINDSTRINGEXACT #x01A2)
  (define-constant LB_SETLOCALE #x01A5)
  (define-constant LB_GETLOCALE #x01A6)
  (define-constant LB_SETCOUNT #x01A7)
  (define-constant LB_INITSTORAGE #x01A8)
  (define-constant LB_ITEMFROMPOINT #x01A9)
  (define-constant LB_GETLISTBOXINFO #x01B2)
  (define-constant LB_MSGMAX #x01B3)

  (define-constant LBN_ERRSPACE -2)
  (define-constant LBN_SELCHANGE 1)
  (define-constant LBN_DBLCLK 2)
  (define-constant LBN_SELCANCEL 3)
  (define-constant LBN_SETFOCUS 4)
  (define-constant LBN_KILLFOCUS 5)

  ;; size
  (define-constant SIZE_RESTORED 0)
  (define-constant SIZE_MINIMIZED 1)
  (define-constant SIZE_MAXIMIZED 2)
  (define-constant SIZE_MAXSHOW 3)
  (define-constant SIZE_MAXHIDE 4)

  ;; key state
  (define-constant MK_LBUTTON #x0001)
  (define-constant MK_RBUTTON #x0002)
  (define-constant MK_SHIFT #x0004)
  (define-constant MK_CONTROL #x0008)
  (define-constant MK_MBUTTON #x0010)
  (define-constant MK_XBUTTON1 #x0020)
  (define-constant MK_XBUTTON2 #x0040)

  ;; virtual keycodes
  (define-constant VK_LBUTTON #x01)
  (define-constant VK_RBUTTON #x02)
  (define-constant VK_CANCEL #x03)
  (define-constant VK_MBUTTON #x04)
  (define-constant VK_XBUTTON1 #x05)
  (define-constant VK_XBUTTON2 #x06)
  (define-constant VK_BACK #x08)
  (define-constant VK_TAB #x09)
  (define-constant VK_CLEAR #x0C)
  (define-constant VK_RETURN #x0D)
  (define-constant VK_SHIFT #x10)
  (define-constant VK_CONTROL #x11)
  (define-constant VK_MENU #x12)
  (define-constant VK_PAUSE #x13)
  (define-constant VK_CAPITAL #x14)
  (define-constant VK_KANA #x15)
  (define-constant VK_HANGEUL #x15)
  (define-constant VK_HANGUL #x15)
  (define-constant VK_JUNJA #x17)
  (define-constant VK_FINAL #x18)
  (define-constant VK_HANJA #x19)
  (define-constant VK_KANJI #x19)
  (define-constant VK_ESCAPE #x1B)
  (define-constant VK_CONVERT #x1C)
  (define-constant VK_NONCONVERT #x1D)
  (define-constant VK_ACCEPT #x1E)
  (define-constant VK_MODECHANGE #x1F)
  (define-constant VK_SPACE #x20)
  (define-constant VK_PRIOR #x21)
  (define-constant VK_NEXT #x22)
  (define-constant VK_END #x23)
  (define-constant VK_HOME #x24)
  (define-constant VK_LEFT #x25)
  (define-constant VK_UP #x26)
  (define-constant VK_RIGHT #x27)
  (define-constant VK_DOWN #x28)
  (define-constant VK_SELECT #x29)
  (define-constant VK_PRINT #x2A)
  (define-constant VK_EXECUTE #x2B)
  (define-constant VK_SNAPSHOT #x2C)
  (define-constant VK_INSERT #x2D)
  (define-constant VK_DELETE #x2E)
  (define-constant VK_HELP #x2F)

  (define-constant VK_LWIN #x5B)
  (define-constant VK_RWIN #x5C)
  (define-constant VK_APPS #x5D)
  (define-constant VK_SLEEP #x5F)
  (define-constant VK_NUMPAD0 #x60)
  (define-constant VK_NUMPAD1 #x61)
  (define-constant VK_NUMPAD2 #x62)
  (define-constant VK_NUMPAD3 #x63)
  (define-constant VK_NUMPAD4 #x64)
  (define-constant VK_NUMPAD5 #x65)
  (define-constant VK_NUMPAD6 #x66)
  (define-constant VK_NUMPAD7 #x67)
  (define-constant VK_NUMPAD8 #x68)
  (define-constant VK_NUMPAD9 #x69)
  (define-constant VK_MULTIPLY #x6A)
  (define-constant VK_ADD #x6B)
  (define-constant VK_SEPARATOR #x6C)
  (define-constant VK_SUBTRACT #x6D)
  (define-constant VK_DECIMAL #x6E)
  (define-constant VK_DIVIDE #x6F)
  (define-constant VK_F1 #x70)
  (define-constant VK_F2 #x71)
  (define-constant VK_F3 #x72)
  (define-constant VK_F4 #x73)
  (define-constant VK_F5 #x74)
  (define-constant VK_F6 #x75)
  (define-constant VK_F7 #x76)
  (define-constant VK_F8 #x77)
  (define-constant VK_F9 #x78)
  (define-constant VK_F10 #x79)
  (define-constant VK_F11 #x7A)
  (define-constant VK_F12 #x7B)
  (define-constant VK_F13 #x7C)
  (define-constant VK_F14 #x7D)
  (define-constant VK_F15 #x7E)
  (define-constant VK_F16 #x7F)
  (define-constant VK_F17 #x80)
  (define-constant VK_F18 #x81)
  (define-constant VK_F19 #x82)
  (define-constant VK_F20 #x83)
  (define-constant VK_F21 #x84)
  (define-constant VK_F22 #x85)
  (define-constant VK_F23 #x86)
  (define-constant VK_F24 #x87)
  (define-constant VK_NUMLOCK #x90)
  (define-constant VK_SCROLL #x91)
  (define-constant VK_OEM_NEC_EQUAL #x92)
  (define-constant VK_OEM_FJ_JISHO #x92)
  (define-constant VK_OEM_FJ_MASSHOU #x93)
  (define-constant VK_OEM_FJ_TOUROKU #x94)
  (define-constant VK_OEM_FJ_LOYA #x95)
  (define-constant VK_OEM_FJ_ROYA #x96)
  (define-constant VK_LSHIFT #xA0)
  (define-constant VK_RSHIFT #xA1)
  (define-constant VK_LCONTROL #xA2)
  (define-constant VK_RCONTROL #xA3)
  (define-constant VK_LMENU #xA4)
  (define-constant VK_RMENU #xA5)
  (define-constant VK_BROWSER_BACK #xA6)
  (define-constant VK_BROWSER_FORWARD #xA7)
  (define-constant VK_BROWSER_REFRESH #xA8)
  (define-constant VK_BROWSER_STOP #xA9)
  (define-constant VK_BROWSER_SEARCH #xAA)
  (define-constant VK_BROWSER_FAVORITES #xAB)
  (define-constant VK_BROWSER_HOME #xAC)
  (define-constant VK_VOLUME_MUTE #xAD)
  (define-constant VK_VOLUME_DOWN #xAE)
  (define-constant VK_VOLUME_UP #xAF)
  (define-constant VK_MEDIA_NEXT_TRACK #xB0)
  (define-constant VK_MEDIA_PREV_TRACK #xB1)
  (define-constant VK_MEDIA_STOP #xB2)
  (define-constant VK_MEDIA_PLAY_PAUSE #xB3)
  (define-constant VK_LAUNCH_MAIL #xB4)
  (define-constant VK_LAUNCH_MEDIA_SELECT #xB5)
  (define-constant VK_LAUNCH_APP1 #xB6)
  (define-constant VK_LAUNCH_APP2 #xB7)
  (define-constant VK_OEM_1 #xBA)
  (define-constant VK_OEM_PLUS #xBB)
  (define-constant VK_OEM_COMMA #xBC)
  (define-constant VK_OEM_MINUS #xBD)
  (define-constant VK_OEM_PERIOD #xBE)
  (define-constant VK_OEM_2 #xBF)
  (define-constant VK_OEM_3 #xC0)
  (define-constant VK_OEM_4 #xDB)
  (define-constant VK_OEM_5 #xDC)
  (define-constant VK_OEM_6 #xDD)
  (define-constant VK_OEM_7 #xDE)
  (define-constant VK_OEM_8 #xDF)
  (define-constant VK_OEM_AX #xE1)
  (define-constant VK_OEM_102 #xE2)
  (define-constant VK_ICO_HELP #xE3)
  (define-constant VK_ICO_00 #xE4)
  (define-constant VK_PROCESSKEY #xE5)
  (define-constant VK_ICO_CLEAR #xE6)
  (define-constant VK_PACKET #xE7)
  (define-constant VK_OEM_RESET #xE9)
  (define-constant VK_OEM_JUMP #xEA)
  (define-constant VK_OEM_PA1 #xEB)
  (define-constant VK_OEM_PA2 #xEC)
  (define-constant VK_OEM_PA3 #xED)
  (define-constant VK_OEM_WSCTRL #xEE)
  (define-constant VK_OEM_CUSEL #xEF)
  (define-constant VK_OEM_ATTN #xF0)
  (define-constant VK_OEM_FINISH #xF1)
  (define-constant VK_OEM_COPY #xF2)
  (define-constant VK_OEM_AUTO #xF3)
  (define-constant VK_OEM_ENLW #xF4)
  (define-constant VK_OEM_BACKTAB #xF5)
  (define-constant VK_ATTN #xF6)
  (define-constant VK_CRSEL #xF7)
  (define-constant VK_EXSEL #xF8)
  (define-constant VK_EREOF #xF9)
  (define-constant VK_PLAY #xFA)
  (define-constant VK_ZOOM #xFB)
  (define-constant VK_NONAME #xFC)
  (define-constant VK_PA1 #xFD)
  (define-constant VK_OEM_CLEAR #xFE)

  (define-constant COLOR_SCROLLBAR 0)
  (define-constant COLOR_BACKGROUND 1)
  (define-constant COLOR_ACTIVECAPTION 2)
  (define-constant COLOR_INACTIVECAPTION 3)
  (define-constant COLOR_MENU 4)
  (define-constant COLOR_WINDOW 5)
  (define-constant COLOR_WINDOWFRAME 6)
  (define-constant COLOR_MENUTEXT 7)
  (define-constant COLOR_WINDOWTEXT 8)
  (define-constant COLOR_CAPTIONTEXT 9)
  (define-constant COLOR_ACTIVEBORDER 10)
  (define-constant COLOR_INACTIVEBORDER 11)
  (define-constant COLOR_APPWORKSPACE 12)
  (define-constant COLOR_HIGHLIGHT 13)
  (define-constant COLOR_HIGHLIGHTTEXT 14)
  (define-constant COLOR_BTNFACE 15)
  (define-constant COLOR_BTNSHADOW 16)
  (define-constant COLOR_GRAYTEXT 17)
  (define-constant COLOR_BTNTEXT 18)
  (define-constant COLOR_INACTIVECAPTIONTEXT 19)
  (define-constant COLOR_BTNHIGHLIGHT 20)

  (define-constant COLOR_3DDKSHADOW 21)
  (define-constant COLOR_3DLIGHT 22)
  (define-constant COLOR_INFOTEXT 23)
  (define-constant COLOR_INFOBK 24)
  (define-constant COLOR_HOTLIGHT 26)
  (define-constant COLOR_GRADIENTACTIVECAPTION 27)
  (define-constant COLOR_GRADIENTINACTIVECAPTION 28)
  (define-constant COLOR_MENUHILIGHT 29)
  (define-constant COLOR_MENUBAR 30)

  (define-constant COLOR_DESKTOP COLOR_BACKGROUND)
  (define-constant COLOR_3DFACE COLOR_BTNFACE)
  (define-constant COLOR_3DSHADOW COLOR_BTNSHADOW)
  (define-constant COLOR_3DHIGHLIGHT COLOR_BTNHIGHLIGHT)
  (define-constant COLOR_3DHILIGHT COLOR_BTNHIGHLIGHT)
  (define-constant COLOR_BTNHILIGHT COLOR_BTNHIGHLIGHT)

  (define get-sys-color (c-function user32 DWORD GetSysColor (int)))
  (define get-sys-color-brush
    (c-function user32 HBRUSH GetSysColorBrush (int)))
  (define set-sys-colors
    ;; int, CONST INT *, CONST COLORREF *
    (c-function user32 BOOL SetSysColors (int void* void*)))

  ;; scroll bar
  (define-c-struct SCROLLINFO
    (UINT cbSize)
    (UINT fMask)
    (int  nMin)
    (int  nMax)
    (UINT nPage)
    (int  nPos)
    (int  nTrackPos))
  (define-constant LPCSCROLLINFO void*)

  (define-constant SIF_RANGE #x0001)
  (define-constant SIF_PAGE #x0002)
  (define-constant SIF_POS #x0004)
  (define-constant SIF_DISABLENOSCROLL #x0008)
  (define-constant SIF_TRACKPOS #x0010)
  (define-constant SIF_ALL (bitwise-ior SIF_RANGE
					SIF_PAGE
					SIF_POS
					SIF_TRACKPOS))

  (define-constant SW_SCROLLCHILDREN #x0001)
  (define-constant SW_INVALIDATE #x0002)
  (define-constant SW_ERASE #x0004)
  (define-constant SW_SMOOTHSCROLL #x0010)

  (define get-scroll-info
    (c-function user32 BOOL GetScrollInfo (HWND int LPCSCROLLINFO)))

  (define set-scroll-info
    (c-function user32 int SetScrollInfo (HWND int LPCSCROLLINFO BOOL)))
  (define scroll-window-ex
    (c-function user32 int ScrollWindowEx
		(HWND int int LPRECT LPRECT HRGN LPRECT UINT)))

  (define system-parameters-info
    (c-function user32 BOOL SystemParametersInfoW (UINT UINT PVOID UINT)))

  (define-constant SPI_GETBEEP #x0001)
  (define-constant SPI_SETBEEP #x0002)
  (define-constant SPI_GETMOUSE #x0003)
  (define-constant SPI_SETMOUSE #x0004)
  (define-constant SPI_GETBORDER #x0005)
  (define-constant SPI_SETBORDER #x0006)
  (define-constant SPI_GETKEYBOARDSPEED #x000A)
  (define-constant SPI_SETKEYBOARDSPEED #x000B)
  (define-constant SPI_LANGDRIVER #x000C)
  (define-constant SPI_ICONHORIZONTALSPACING #x000D)
  (define-constant SPI_GETSCREENSAVETIMEOUT #x000E)
  (define-constant SPI_SETSCREENSAVETIMEOUT #x000F)
  (define-constant SPI_GETSCREENSAVEACTIVE #x0010)
  (define-constant SPI_SETSCREENSAVEACTIVE #x0011)
  (define-constant SPI_GETGRIDGRANULARITY #x0012)
  (define-constant SPI_SETGRIDGRANULARITY #x0013)
  (define-constant SPI_SETDESKWALLPAPER #x0014)
  (define-constant SPI_SETDESKPATTERN #x0015)
  (define-constant SPI_GETKEYBOARDDELAY #x0016)
  (define-constant SPI_SETKEYBOARDDELAY #x0017)
  (define-constant SPI_ICONVERTICALSPACING #x0018)
  (define-constant SPI_GETICONTITLEWRAP #x0019)
  (define-constant SPI_SETICONTITLEWRAP #x001A)
  (define-constant SPI_GETMENUDROPALIGNMENT #x001B)
  (define-constant SPI_SETMENUDROPALIGNMENT #x001C)
  (define-constant SPI_SETDOUBLECLKWIDTH #x001D)
  (define-constant SPI_SETDOUBLECLKHEIGHT #x001E)
  (define-constant SPI_GETICONTITLELOGFONT #x001F)
  (define-constant SPI_SETDOUBLECLICKTIME #x0020)
  (define-constant SPI_SETMOUSEBUTTONSWAP #x0021)
  (define-constant SPI_SETICONTITLELOGFONT #x0022)
  (define-constant SPI_GETFASTTASKSWITCH #x0023)
  (define-constant SPI_SETFASTTASKSWITCH #x0024)
  (define-constant SPI_SETDRAGFULLWINDOWS #x0025)
  (define-constant SPI_GETDRAGFULLWINDOWS #x0026)
  (define-constant SPI_GETNONCLIENTMETRICS #x0029)
  (define-constant SPI_SETNONCLIENTMETRICS #x002A)
  (define-constant SPI_GETMINIMIZEDMETRICS #x002B)
  (define-constant SPI_SETMINIMIZEDMETRICS #x002C)
  (define-constant SPI_GETICONMETRICS #x002D)
  (define-constant SPI_SETICONMETRICS #x002E)
  (define-constant SPI_SETWORKAREA #x002F)
  (define-constant SPI_GETWORKAREA #x0030)
  (define-constant SPI_SETPENWINDOWS #x0031)

  (define-constant SPI_GETHIGHCONTRAST #x0042)
  (define-constant SPI_SETHIGHCONTRAST #x0043)
  (define-constant SPI_GETKEYBOARDPREF #x0044)
  (define-constant SPI_SETKEYBOARDPREF #x0045)
  (define-constant SPI_GETSCREENREADER #x0046)
  (define-constant SPI_SETSCREENREADER #x0047)
  (define-constant SPI_GETANIMATION #x0048)
  (define-constant SPI_SETANIMATION #x0049)
  (define-constant SPI_GETFONTSMOOTHING #x004A)
  (define-constant SPI_SETFONTSMOOTHING #x004B)
  (define-constant SPI_SETDRAGWIDTH #x004C)
  (define-constant SPI_SETDRAGHEIGHT #x004D)
  (define-constant SPI_SETHANDHELD #x004E)
  (define-constant SPI_GETLOWPOWERTIMEOUT #x004F)
  (define-constant SPI_GETPOWEROFFTIMEOUT #x0050)
  (define-constant SPI_SETLOWPOWERTIMEOUT #x0051)
  (define-constant SPI_SETPOWEROFFTIMEOUT #x0052)
  (define-constant SPI_GETLOWPOWERACTIVE #x0053)
  (define-constant SPI_GETPOWEROFFACTIVE #x0054)
  (define-constant SPI_SETLOWPOWERACTIVE #x0055)
  (define-constant SPI_SETPOWEROFFACTIVE #x0056)
  (define-constant SPI_SETCURSORS #x0057)
  (define-constant SPI_SETICONS #x0058)
  (define-constant SPI_GETDEFAULTINPUTLANG #x0059)
  (define-constant SPI_SETDEFAULTINPUTLANG #x005A)
  (define-constant SPI_SETLANGTOGGLE #x005B)
  (define-constant SPI_GETWINDOWSEXTENSION #x005C)
  (define-constant SPI_SETMOUSETRAILS #x005D)
  (define-constant SPI_GETMOUSETRAILS #x005E)
  (define-constant SPI_SETSCREENSAVERRUNNING #x0061)
  ;; #define SPI_SCREENSAVERRUNNING SPI_SETSCREENSAVERRUNNING
  (define-constant SPI_GETFILTERKEYS #x0032)
  (define-constant SPI_SETFILTERKEYS #x0033)
  (define-constant SPI_GETTOGGLEKEYS #x0034)
  (define-constant SPI_SETTOGGLEKEYS #x0035)
  (define-constant SPI_GETMOUSEKEYS #x0036)
  (define-constant SPI_SETMOUSEKEYS #x0037)
  (define-constant SPI_GETSHOWSOUNDS #x0038)
  (define-constant SPI_SETSHOWSOUNDS #x0039)
  (define-constant SPI_GETSTICKYKEYS #x003A)
  (define-constant SPI_SETSTICKYKEYS #x003B)
  (define-constant SPI_GETACCESSTIMEOUT #x003C)
  (define-constant SPI_SETACCESSTIMEOUT #x003D)
  (define-constant SPI_GETSERIALKEYS #x003E)
  (define-constant SPI_SETSERIALKEYS #x003F)
  (define-constant SPI_GETSOUNDSENTRY #x0040)
  (define-constant SPI_SETSOUNDSENTRY #x0041)
  (define-constant SPI_GETSNAPTODEFBUTTON #x005F)
  (define-constant SPI_SETSNAPTODEFBUTTON #x0060)
  (define-constant SPI_GETMOUSEHOVERWIDTH #x0062)
  (define-constant SPI_SETMOUSEHOVERWIDTH #x0063)
  (define-constant SPI_GETMOUSEHOVERHEIGHT #x0064)
  (define-constant SPI_SETMOUSEHOVERHEIGHT #x0065)
  (define-constant SPI_GETMOUSEHOVERTIME #x0066)
  (define-constant SPI_SETMOUSEHOVERTIME #x0067)
  (define-constant SPI_GETWHEELSCROLLLINES #x0068)
  (define-constant SPI_SETWHEELSCROLLLINES #x0069)
  (define-constant SPI_GETMENUSHOWDELAY #x006A)
  (define-constant SPI_SETMENUSHOWDELAY #x006B)
  ;; #if _WIN32_WINNT >= 0x0600
  (define-constant SPI_GETWHEELSCROLLCHARS #x006C)
  (define-constant SPI_SETWHEELSCROLLCHARS #x006D)
  ;; #endif
  (define-constant SPI_GETSHOWIMEUI #x006E)
  (define-constant SPI_SETSHOWIMEUI #x006F)
  (define-constant SPI_GETMOUSESPEED #x0070)
  (define-constant SPI_SETMOUSESPEED #x0071)
  (define-constant SPI_GETSCREENSAVERRUNNING #x0072)
  (define-constant SPI_GETDESKWALLPAPER #x0073)
  ;; #if WINVER >= 0x0600
  (define-constant SPI_GETAUDIODESCRIPTION #x0074)
  (define-constant SPI_SETAUDIODESCRIPTION #x0075)
  (define-constant SPI_GETSCREENSAVESECURE #x0076)
  (define-constant SPI_SETSCREENSAVESECURE #x0077)
  ;; #endif
  ;; #if _WIN32_WINNT >= 0x0601
  (define-constant SPI_GETHUNGAPPTIMEOUT #x0078)
  (define-constant SPI_SETHUNGAPPTIMEOUT #x0079)
  (define-constant SPI_GETWAITTOKILLTIMEOUT #x007a)
  (define-constant SPI_SETWAITTOKILLTIMEOUT #x007b)
  (define-constant SPI_GETWAITTOKILLSERVICETIMEOUT #x007c)
  (define-constant SPI_SETWAITTOKILLSERVICETIMEOUT #x007d)
  (define-constant SPI_GETMOUSEDOCKTHRESHOLD #x007e)
  (define-constant SPI_SETMOUSEDOCKTHRESHOLD #x007f)
  (define-constant SPI_GETPENDOCKTHRESHOLD #x0080)
  (define-constant SPI_SETPENDOCKTHRESHOLD #x0081)
  (define-constant SPI_GETWINARRANGING #x0082)
  (define-constant SPI_SETWINARRANGING #x0083)
  (define-constant SPI_GETMOUSEDRAGOUTTHRESHOLD #x0084)
  (define-constant SPI_SETMOUSEDRAGOUTTHRESHOLD #x0085)
  (define-constant SPI_GETPENDRAGOUTTHRESHOLD #x0086)
  (define-constant SPI_SETPENDRAGOUTTHRESHOLD #x0087)
  (define-constant SPI_GETMOUSESIDEMOVETHRESHOLD #x0088)
  (define-constant SPI_SETMOUSESIDEMOVETHRESHOLD #x0089)
  (define-constant SPI_GETPENSIDEMOVETHRESHOLD #x008a)
  (define-constant SPI_SETPENSIDEMOVETHRESHOLD #x008b)
  (define-constant SPI_GETDRAGFROMMAXIMIZE #x008c)
  (define-constant SPI_SETDRAGFROMMAXIMIZE #x008d)
  (define-constant SPI_GETSNAPSIZING #x008e)
  (define-constant SPI_SETSNAPSIZING #x008f)
  (define-constant SPI_GETDOCKMOVING #x0090)
  (define-constant SPI_SETDOCKMOVING #x0091)
  ;; #endif
  ;; #if WINVER >= 0x0602
  (define-constant SPI_GETTOUCHPREDICTIONPARAMETERS #x009c)
  (define-constant SPI_SETTOUCHPREDICTIONPARAMETERS #x009d)
  (define-constant SPI_GETLOGICALDPIOVERRIDE #x009e)
  (define-constant SPI_SETLOGICALDPIOVERRIDE #x009f)
  (define-constant SPI_GETMOUSECORNERCLIPLENGTH #x00a0)
  (define-constant SPI_SETMOUSECORNERCLIPLENGTH #x00a1)
  (define-constant SPI_GETMENURECT #x00a2)
  (define-constant SPI_SETMENURECT #x00a3)
  ;; #endif

  (define-constant SPI_GETACTIVEWINDOWTRACKING #x1000)
  (define-constant SPI_SETACTIVEWINDOWTRACKING #x1001)
  (define-constant SPI_GETMENUANIMATION #x1002)
  (define-constant SPI_SETMENUANIMATION #x1003)
  (define-constant SPI_GETCOMBOBOXANIMATION #x1004)
  (define-constant SPI_SETCOMBOBOXANIMATION #x1005)
  (define-constant SPI_GETLISTBOXSMOOTHSCROLLING #x1006)
  (define-constant SPI_SETLISTBOXSMOOTHSCROLLING #x1007)
  (define-constant SPI_GETGRADIENTCAPTIONS #x1008)
  (define-constant SPI_SETGRADIENTCAPTIONS #x1009)
  (define-constant SPI_GETKEYBOARDCUES #x100A)
  (define-constant SPI_SETKEYBOARDCUES #x100B)
  ;; #define SPI_GETMENUUNDERLINES SPI_GETKEYBOARDCUES
  ;; #define SPI_SETMENUUNDERLINES SPI_SETKEYBOARDCUES
  (define-constant SPI_GETACTIVEWNDTRKZORDER #x100C)
  (define-constant SPI_SETACTIVEWNDTRKZORDER #x100D)
  (define-constant SPI_GETHOTTRACKING #x100E)
  (define-constant SPI_SETHOTTRACKING #x100F)
  (define-constant SPI_GETMENUFADE #x1012)
  (define-constant SPI_SETMENUFADE #x1013)
  (define-constant SPI_GETSELECTIONFADE #x1014)
  (define-constant SPI_SETSELECTIONFADE #x1015)
  (define-constant SPI_GETTOOLTIPANIMATION #x1016)
  (define-constant SPI_SETTOOLTIPANIMATION #x1017)
  (define-constant SPI_GETTOOLTIPFADE #x1018)
  (define-constant SPI_SETTOOLTIPFADE #x1019)
  (define-constant SPI_GETCURSORSHADOW #x101A)
  (define-constant SPI_SETCURSORSHADOW #x101B)
  (define-constant SPI_GETMOUSESONAR #x101C)
  (define-constant SPI_SETMOUSESONAR #x101D)
  (define-constant SPI_GETMOUSECLICKLOCK #x101E)
  (define-constant SPI_SETMOUSECLICKLOCK #x101F)
  (define-constant SPI_GETMOUSEVANISH #x1020)
  (define-constant SPI_SETMOUSEVANISH #x1021)
  (define-constant SPI_GETFLATMENU #x1022)
  (define-constant SPI_SETFLATMENU #x1023)
  (define-constant SPI_GETDROPSHADOW #x1024)
  (define-constant SPI_SETDROPSHADOW #x1025)
  (define-constant SPI_GETBLOCKSENDINPUTRESETS #x1026)
  (define-constant SPI_SETBLOCKSENDINPUTRESETS #x1027)
  (define-constant SPI_GETUIEFFECTS #x103E)
  (define-constant SPI_SETUIEFFECTS #x103F)
  ;; #if _WIN32_WINNT >= 0x0600
  (define-constant SPI_GETDISABLEOVERLAPPEDCONTENT #x1040)
  (define-constant SPI_SETDISABLEOVERLAPPEDCONTENT #x1041)
  (define-constant SPI_GETCLIENTAREAANIMATION #x1042)
  (define-constant SPI_SETCLIENTAREAANIMATION #x1043)
  (define-constant SPI_GETCLEARTYPE #x1048)
  (define-constant SPI_SETCLEARTYPE #x1049)
  (define-constant SPI_GETSPEECHRECOGNITION #x104a)
  (define-constant SPI_SETSPEECHRECOGNITION #x104b)
  ;; #endif
  ;; #if WINVER >= 0x0601
  (define-constant SPI_GETCARETBROWSING #x104c)
  (define-constant SPI_SETCARETBROWSING #x104d)
  (define-constant SPI_GETTHREADLOCALINPUTSETTINGS #x104e)
  (define-constant SPI_SETTHREADLOCALINPUTSETTINGS #x104f)
  (define-constant SPI_GETSYSTEMLANGUAGEBAR #x1050)
  (define-constant SPI_SETSYSTEMLANGUAGEBAR #x1051)
  ;; #endif
  (define-constant SPI_GETFOREGROUNDLOCKTIMEOUT #x2000)
  (define-constant SPI_SETFOREGROUNDLOCKTIMEOUT #x2001)
  (define-constant SPI_GETACTIVEWNDTRKTIMEOUT #x2002)
  (define-constant SPI_SETACTIVEWNDTRKTIMEOUT #x2003)
  (define-constant SPI_GETFOREGROUNDFLASHCOUNT #x2004)
  (define-constant SPI_SETFOREGROUNDFLASHCOUNT #x2005)
  (define-constant SPI_GETCARETWIDTH #x2006)
  (define-constant SPI_SETCARETWIDTH #x2007)
  (define-constant SPI_GETMOUSECLICKLOCKTIME #x2008)
  (define-constant SPI_SETMOUSECLICKLOCKTIME #x2009)
  (define-constant SPI_GETFONTSMOOTHINGTYPE #x200A)
  (define-constant SPI_SETFONTSMOOTHINGTYPE #x200B)

  (define-constant FE_FONTSMOOTHINGSTANDARD #x0001)
  (define-constant FE_FONTSMOOTHINGCLEARTYPE #x0002)
  (define-constant FE_FONTSMOOTHINGDOCKING #x8000)

  (define-constant SPI_GETFONTSMOOTHINGCONTRAST #x200C)
  (define-constant SPI_SETFONTSMOOTHINGCONTRAST #x200D)
  (define-constant SPI_GETFOCUSBORDERWIDTH #x200E)
  (define-constant SPI_SETFOCUSBORDERWIDTH #x200F)
  (define-constant SPI_GETFOCUSBORDERHEIGHT #x2010)
  (define-constant SPI_SETFOCUSBORDERHEIGHT #x2011)
  (define-constant SPI_GETFONTSMOOTHINGORIENTATION #x2012)
  (define-constant SPI_SETFONTSMOOTHINGORIENTATION #x2013)
  ;; #if _WIN32_WINNT >= 0x0600
  (define-constant SPI_GETMINIMUMHITRADIUS #x2014)
  (define-constant SPI_SETMINIMUMHITRADIUS #x2015)
  (define-constant SPI_GETMESSAGEDURATION #x2016)
  (define-constant SPI_SETMESSAGEDURATION #x2017)
  ;; #endif
  ;; #if WINVER >= 0x0602
  (define-constant SPI_GETCONTACTVISUALIZATION #x2018)
  (define-constant SPI_SETCONTACTVISUALIZATION #x2019)
  (define-constant SPI_GETGESTUREVISUALIZATION #x201a)
  (define-constant SPI_SETGESTUREVISUALIZATION #x201b)
  ;; #endif

  (define-constant MA_ACTIVATE 1)
  (define-constant MA_ACTIVATEANDEAT 2)
  (define-constant MA_NOACTIVATE 3)
  (define-constant MA_NOACTIVATEANDEAT 4)

  (define-constant SWP_ASYNCWINDOWPOS	#x4000)
  (define-constant SWP_DEFERERASE	#x2000)
  (define-constant SWP_DRAWFRAME	#x0020)
  (define-constant SWP_FRAMECHANGED	#x0020)
  (define-constant SWP_HIDEWINDOW	#x0080)
  (define-constant SWP_NOACTIVATE	#x0010)
  (define-constant SWP_NOCOPYBITS	#x0100)
  (define-constant SWP_NOMOVE		#x0002)
  (define-constant SWP_NOOWNERZORDER	#x0200)
  (define-constant SWP_NOREDRAW		#x0008)
  (define-constant SWP_NOREPOSITION	#x0200)
  (define-constant SWP_NOSENDCHANGING	#x0400)
  (define-constant SWP_NOSIZE		#x0001)
  (define-constant SWP_NOZORDER		#x0004)
  (define-constant SWP_SHOWWINDOW	#x0040)

  (define create-caret
    (c-function user32 BOOL CreateCaret (HWND HBITMAP int int)))
  (define show-caret
    (c-function user32 BOOL ShowCaret (HWND)))
  (define hide-caret
    (c-function user32 BOOL HideCaret (HWND)))
  (define destroy-caret
    (c-function user32 BOOL DestroyCaret()))
  (define set-caret-pos
    (c-function user32 BOOL SetCaretPos (int int)))
  (define get-caret-pos
    (c-function user32 BOOL GetCaretPos (LPPOINT)))

  (define TIMERPROC callback)
  (define set-timer
    (c-function user32 HWND SetTimer (HWND UINT_PTR UINT TIMERPROC)))
  (define kill-timer
    (c-function user32 BOOL KillTimer (HWND UINT_PTR)))

  (define screen-to-client
    (c-function user32 BOOL ScreenToClient (HWND LPPOINT)))

  (define get-key-state
    (c-function user32 SHORT GetKeyState (int)))
)
