/*
 * %CopyrightBegin%
 * 
 * Copyright Ericsson AB 2008-2021. All Rights Reserved.
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

#include <stdio.h>
#include <erl_nif.h>

/* Platform specific initialisation stuff */ 
#ifdef _MACOSX

#include <Cocoa/Cocoa.h>
#include <objc/objc-runtime.h>

extern OSErr  CPSSetProcessName (ProcessSerialNumber *psn, char *processname);

void * wxe_ps_init()
{
   return (void *) 0;
}

int is_packaged_app() {
   // Can get lost in when execing around, we use the name instead
   /* if(mainBundle) { */
   /*    return (CFBundleGetValueForInfoDictionaryKey(mainBundle, CFSTR("CFBundlePackageType")) != nil); */
   /* } */
#ifdef MAC_OS_X_VERSION_10_6
   NSString *  appName = [[NSRunningApplication currentApplication] localizedName];
   return (strncmp("beam", [appName UTF8String], 4) != 0);
#else
   return 0;
#endif
}

void * wxe_ps_init2() {
   NSAutoreleasePool *pool;
   ProcessSerialNumber psn;
   size_t app_len = 127;
   char app_title_buf[128];
   char * app_title;
   size_t app_icon_len = 1023;
   char app_icon_buf[1024];

   // Setup and enable gui
   pool = [[NSAutoreleasePool alloc] init];

   if( !is_packaged_app() ) {
      // Undocumented function (but no documented way of doing this exists)
      int res = enif_getenv("WX_APP_TITLE", app_title_buf, &app_len);
      if (res == 0) {
          app_title = app_title_buf;
      } else {
          app_title = NULL;
      }
      if(!GetCurrentProcess(&psn)) {
      	 CPSSetProcessName(&psn, app_title?app_title:"Erlang");
      }
      // Enable setting custom application icon for Mac OS X
      res = enif_getenv("WX_APP_ICON", app_icon_buf, &app_icon_len);
      NSMutableString *file = [[NSMutableString alloc] init];
      if (res == 0) {
          [file appendFormat:@"%s", app_icon_buf];
      } else {
          res = enif_getenv("WX_PRIV_DIR", app_icon_buf, &app_icon_len);
          if(res == 0) {
              [file appendFormat:@"%s/%s", app_icon_buf, "erlang-logo128.png"];
          }
      }
      if(res == 0) {
          // Load and set icon
          NSImage *icon = [[NSImage alloc] initWithContentsOfFile: file];
          [NSApp setApplicationIconImage: icon];
      }
   };

   return pool;
}

/* _MACOSX */
#else
#ifdef _WIN32
#include <windows.h>

void * wxe_ps_init()
{
    int res;
    size_t dir_len = 1023;
    char dir_utf8[1024];
    wchar_t *npath;
    size_t path_len;

    res = enif_getenv("WX_PRIV_DIR", dir_utf8, &dir_len);
    if(res == 0) {
        dir_len = MultiByteToWideChar(CP_UTF8, 0, dir_utf8, dir_len+1, NULL, 0);
        path_len = GetEnvironmentVariableW(L"PATH",NULL,0);
        if( dir_len > 0 && dir_len < 1024 && path_len > 0 ) {
            npath = (wchar_t *) malloc((path_len+dir_len+2)*sizeof(wchar_t));
            if(GetEnvironmentVariableW(L"PATH",npath,path_len) != (path_len-1)) {
                free(npath);
                return NULL;
            }
            npath[path_len-1] = L';';
            if(MultiByteToWideChar(CP_UTF8, 0, dir_utf8, dir_len, npath+path_len, dir_len+1) > 0) {
                SetEnvironmentVariableW(L"PATH",npath);
            }
            free(npath);
        }
    }
    return (void *) 0;
}
#else
void * wxe_ps_init()
{
   return (void *) 0;
}
#endif

void * wxe_ps_init2()
{
   return (void *) 0;
}
#endif
