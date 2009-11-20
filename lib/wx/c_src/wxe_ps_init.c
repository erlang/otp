/*
 * %CopyrightBegin%
 * 
 * Copyright Ericsson AB 2008-2009. All Rights Reserved.
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

#include <stdio.h>
#include "wxe_driver.h"
/* Platform specific initialisation stuff */ 
#ifdef _MACOSX

#include <Cocoa/Cocoa.h>
#include <objc/objc-runtime.h>

void * wxe_ps_init() 
{
   ProcessSerialNumber psn;
   NSAutoreleasePool *pool;
   // Enable GUI 
   GetCurrentProcess(&psn);
   TransformProcessType(&psn, kProcessTransformToForegroundApplication);
   SetFrontProcess(&psn);
   // Enable Cocoa calls from Carbon app
   NSApplicationLoad();

   // Setup and enable gui
   pool = [[NSAutoreleasePool alloc] init];

   NSApplication *app = [NSApplication sharedApplication];
   // Load and set icon
   
   NSMutableString *file = [[NSMutableString alloc] init];
   [file appendFormat:@"%s/%s", erl_wx_privdir, "erlang-logo64.png"];
   NSImage *icon = [[NSImage alloc] initWithContentsOfFile: file];
   [app setApplicationIconImage: icon];

   return (void *) pool;
}
/* _MACOSX */
#else
void * wxe_ps_init() 
{
   return (void *) 0;
}
#endif 
