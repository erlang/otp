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

#include <stdio.h>
#include "wxe_driver.h"

/* Platform specific initialisation stuff */ 
#ifdef _MACOSX

#include <Cocoa/Cocoa.h>
#include <objc/objc-runtime.h>

extern OSErr  CPSSetProcessName (ProcessSerialNumber *psn, char *processname);

void * wxe_ps_init() 
{
   ProcessSerialNumber psn;
   // Enable GUI 
   if(!GetCurrentProcess(&psn)) {
      TransformProcessType(&psn, kProcessTransformToForegroundApplication);
#ifdef  MAC_OS_X_VERSION_10_6
      [[NSRunningApplication currentApplication] activateWithOptions:
       (NSApplicationActivateAllWindows | NSApplicationActivateIgnoringOtherApps)];
#else 
      SetFrontProcess(&psn);
#endif
   }
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

   // Setup and enable gui
   pool = [[NSAutoreleasePool alloc] init];
   
   if( !is_packaged_app() ) {
      // Undocumented function (but no documented way of doing this exists)
      char *app_title = getenv("WX_APP_TITLE");
      if(!GetCurrentProcess(&psn)) {
      	 CPSSetProcessName(&psn, app_title?app_title:"Erlang");
      }
      // Load and set icon
      NSMutableString *file = [[NSMutableString alloc] init];
      [file appendFormat:@"%s/%s", erl_wx_privdir, "erlang-logo64.png"];
      NSImage *icon = [[NSImage alloc] initWithContentsOfFile: file];
      [NSApp setApplicationIconImage: icon];
   };

   return pool;
}

/* _MACOSX */
#else
void * wxe_ps_init()
{
   return (void *) 0;
}
void * wxe_ps_init2()
{
   return (void *) 0;
}
#endif
