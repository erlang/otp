/*
 * %CopyrightBegin%
 *
 * Copyright Ericsson AB 2003-2016. All Rights Reserved.
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
/*
 * Some code just simply does not deserve functions :-)
 * Dead simple installation program to set up init files etc after erlang is 
 * copied to its destination. Also to be used after a patch is applied.
 */ 

#include <windows.h>
#include <stdio.h>
#include <stdlib.h>
#include "init_file.h"

int wmain(int argc, wchar_t **argv) 
{
    int silent = 0;
    int start_sasl = 0;
    wchar_t *root = NULL;
    int i;
    wchar_t buffer[MAX_PATH];
    wchar_t erts_dir[MAX_PATH];
    wchar_t release_dir[MAX_PATH];
    wchar_t bin_dir[MAX_PATH];
    char *tmp;
    char tmp_utf8[MAX_PATH*4];
    wchar_t my_ini_filename[MAX_PATH];
    InitFile *my_ini_file;
    InitSection *my_ini_section;
    char erts_version[MAX_PATH];
    InitFile *ini_file;
    InitSection *ini_section;
    HANDLE module = GetModuleHandle(NULL);
    wchar_t *binaries[] = { L"erl.exe", L"werl.exe", L"erlc.exe",
			    L"dialyzer.exe",
			    L"typer.exe",
			    L"escript.exe", L"ct_run.exe", NULL };
    wchar_t *scripts[] = { L"start_clean.boot", L"start_sasl.boot", L"no_dot_erlang.boot", NULL };
    wchar_t fromname[MAX_PATH];
    wchar_t toname[MAX_PATH];
    size_t  converted;

    for (i = 1; i < argc; ++i) {
	switch(argv[i][0]) {
	case L'-' :
	    switch(argv[i][1]) {
	    case L's' :
		silent = 1;
		break;
	    default:
		fprintf(stderr, "Unknown command switch %S\n",
			argv[i]);
		exit(1);
	    }
	    break;
	default: {
	    if (root != NULL) {
		fprintf(stderr, "Only one root directory can be specified, "
			"parameter %S is illegal\n",
			argv[i]);
		exit(1);
	    }	    
	    root = argv[i];
	    }
	    break;
	}
    }
    if (root == NULL) {
	if (module == NULL) {
	    fprintf(stderr, "Cannot GetModuleHandle()\n");
	    exit(1);
	}

	if (GetModuleFileNameW(module,buffer,MAX_PATH) == 0) {
	    fprintf(stderr,"Could not GetModuleFileName()\n");
	    exit(1);
	}
	i = wcslen(buffer) - 1;
	while ( i >= 0 && buffer[i] != L'\\') {
	    --i;
	}
	if (i < 0) {
	    fprintf(stderr,"GetModuleFileName returned broken path\n");
	    exit(1);
	}
	buffer[i] = L'\0';
	root = buffer;
    }

    if (!silent) {
	char answer[100];
	char *eol;
	start_sasl = 1;
	printf("Do you want a minimal startup instead of sasl [No]: ");
	fflush(stdout);
	if (fgets(answer,100,stdin) == NULL) {
	    fprintf(stderr, "Could not read answer from user.\n");
	    exit(1);
	}
	eol = strchr(answer,'\n');
	if (eol == NULL) {
	    while (getchar() != '\n')
		;
	} else {
	    *eol = '\0';
	}
	if ((eol = strchr(answer, '\r')) != NULL) {
	    *eol = '\0';
	}
	if (_stricmp(answer,"yes") == 0 || _stricmp(answer,"y") == 0) {
	    start_sasl = 0;
	}
    }
    swprintf(my_ini_filename, MAX_PATH, L"%s\\Install.ini", root);
    my_ini_file = load_init_file(my_ini_filename);
    if (my_ini_file == NULL) {
	fprintf(stderr,"Cannot open init file %S\n",my_ini_filename);
	exit(1);
    }
    
    if ((my_ini_section = lookup_init_section(my_ini_file,"Install")) 
	== NULL) {
	fprintf(stderr,"No [Install] section in init file %S\n",
		my_ini_filename);
	exit(1);
    }
    
    if ((tmp = lookup_init_entry(my_ini_section, "VSN")) == NULL) {
	fprintf(stderr,"No key VSN in init file %S\n",
		my_ini_filename);
	exit(1);
    }
    strcpy(erts_version,tmp);
    
    swprintf(erts_dir,MAX_PATH,L"%s\\erts-%S\\bin",root,erts_version);
    if ((tmp = lookup_init_entry(my_ini_section, "SYSTEM_VSN")) == NULL) {
	fprintf(stderr,"No key SYSTEM_VSN in init file %S\n",
		 my_ini_filename);
	exit(1);
    }
    swprintf(release_dir,MAX_PATH,L"%s\\releases\\%S",root,tmp); 

    swprintf(bin_dir,MAX_PATH,L"%s\\bin",root);
    CreateDirectoryW(bin_dir,NULL);

    free_init_file(my_ini_file);
    
    for (i = 0; binaries[i] != NULL; ++i) {
	swprintf(fromname,MAX_PATH,L"%s\\%s",erts_dir,binaries[i]);
	swprintf(toname,MAX_PATH,L"%s\\%s",bin_dir,binaries[i]);
	if (GetFileAttributesW(fromname) == 0xFFFFFFFF) {
	    fprintf(stderr,"Could not find file %S\n",
		    fromname);
	    exit(1);
	}
	if (!CopyFileW(fromname,toname,FALSE)) {
	    fprintf(stderr,"Could not copy file %S to %S\n",
		    fromname,toname);
	    fprintf(stderr,"Continuing installation anyway...\n");
	}
    }
    
    for (i = 0; scripts[i] != NULL; ++i) {
	swprintf(fromname,MAX_PATH,L"%s\\%s",release_dir,scripts[i]);
	swprintf(toname,MAX_PATH,L"%s\\%s",bin_dir,scripts[i]);
	if (GetFileAttributesW(fromname) == 0xFFFFFFFF) {
	    fprintf(stderr,"Could not find file %S\n",
		    fromname);
	    exit(1);
	}
	if (!CopyFileW(fromname,toname,FALSE)) {
	    fprintf(stderr,"Could not copy file %S to %S\n",
		    fromname,toname);
	    fprintf(stderr,"Cannot continue installation, bailing out.\n");
	    exit(1);
	}
    }
    if (start_sasl) {
	swprintf(fromname,MAX_PATH,L"%s\\start_sasl.boot",bin_dir);
    } else {
	swprintf(fromname,MAX_PATH,L"%s\\start_clean.boot",bin_dir);
    }
    swprintf(toname,MAX_PATH,L"%s\\start.boot",bin_dir);
    if (!CopyFileW(fromname,toname,FALSE)) {
	fprintf(stderr,"Could not copy file %S to %S\n",
		fromname,toname);
	fprintf(stderr,"Cannot continue installation, bailing out.\n");
	exit(1);
    }

    /* OBS!!! If the format of the init file is changed, do not forget
       to update release_handler:write_ini_file(...) */
    ini_file = create_init_file();
    ini_section = create_init_section("erlang");
    add_init_section(ini_file,ini_section);
    WideCharToMultiByte(CP_UTF8,0,erts_dir,-1,tmp_utf8,MAX_PATH*4,NULL,NULL);
    add_init_entry(ini_section,"Bindir",tmp_utf8);
    add_init_entry(ini_section,"Progname","erl");
    WideCharToMultiByte(CP_UTF8,0,root,-1,tmp_utf8,MAX_PATH*4,NULL,NULL);
    add_init_entry(ini_section,"Rootdir",tmp_utf8);
    swprintf(fromname,MAX_PATH,L"%s\\erl.ini",erts_dir);
    swprintf(toname,MAX_PATH,L"%s\\erl.ini",bin_dir);
    if (store_init_file(ini_file,fromname) != 0) {
	fprintf(stderr,"Could not create file %S\n",
		fromname);
	fprintf(stderr,"Cannot continue installation, bailing out.\n");
	exit(1);
    }
    if (!CopyFileW(fromname,toname,FALSE)) {
	fprintf(stderr,"Could not copy file %S to %S\n",
		 fromname,toname);
	fprintf(stderr,"Cannot continue installation, bailing out.\n");
	exit(1);
    }
    if (!silent) {
	printf("Erlang %s installed successfully\n", erts_version);
    }
    return 0;
}



