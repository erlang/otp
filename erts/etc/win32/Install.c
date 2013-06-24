/*
 * %CopyrightBegin%
 *
 * Copyright Ericsson AB 2003-2013. All Rights Reserved.
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
/*
 * Some code just simply does not deserve functions :-)
 * Dead simple installation program to set up init files etc after erlang is 
 * copied to its destination. Also to be used after a patch is applied.
 */ 
#include <windows.h>
#include <stdio.h>
#include <stdlib.h>
#include "init_file.h"

int main(int argc, char **argv) 
{
    int silent = 0;
    int start_sasl = 0;
    char *root = NULL;
    int i;
    char buffer[MAX_PATH];
    char erts_dir[MAX_PATH];
    char release_dir[MAX_PATH];
    char bin_dir[MAX_PATH];
    char *tmp;
    char my_ini_filename[MAX_PATH];
    InitFile *my_ini_file;
    InitSection *my_ini_section;
    char version_string[MAX_PATH];
    InitFile *ini_file;
    InitSection *ini_section;
    HANDLE module = GetModuleHandle(NULL);
    char *binaries[] = { "erl.exe", "werl.exe", "erlc.exe",
			 "dialyzer.exe", "typer.exe",
			 "escript.exe", "reltool", "ct_run.exe", NULL };
    char *scripts[] = { "start_clean.boot", "start_sasl.boot", NULL };
    char fromname[MAX_PATH];
    char toname[MAX_PATH];
    

    for (i = 1; i < argc; ++i) {
	switch(argv[i][0]) {
	case '-' :
	    switch(argv[i][1]) {
	    case 's' :
		silent = 1;
		break;
	    default:
		fprintf(stderr, "Unknown command switch %s\n",
			argv[i]);
		exit(1);
	    }
	    break;
	default:
	    if (root != NULL) {
		fprintf(stderr, "Only one root directory can be specified, "
			"parameter %s is illegal\n",
			argv[i]);
		exit(1);
	    }
	    root = argv[i];
	    break;
	}
    }
    if (root == NULL) {
	if (module = NULL) {
	    fprintf(stderr, "Cannot GetModuleHandle()\n");
	    exit(1);
	}

	if (GetModuleFileName(module,buffer,MAX_PATH) == 0) {
	    fprintf(stderr,"Could not GetModuleFileName()\n");
	    exit(1);
	}
	i = strlen(buffer) - 1;
	while ( i >= 0 && buffer[i] != '\\') {
	    --i;
	}
	if (i < 0) {
	    fprintf(stderr,"GetModuleFileName returned broken path\n");
	    exit(1);
	}
	buffer[i] = '\0';
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
    sprintf(my_ini_filename,"%s\\Install.ini",root);
    my_ini_file = load_init_file(my_ini_filename);
    if (my_ini_file == NULL) {
	fprintf(stderr,"Cannot open init file %s\n",my_ini_filename);
	exit(1);
    }
    
    if ((my_ini_section = lookup_init_section(my_ini_file,"Install")) 
	== NULL) {
	fprintf(stderr,"No [Install] section in init file %s\n",
		my_ini_filename);
	exit(1);
    }
    
    if ((tmp = lookup_init_entry(my_ini_section, "VSN")) == NULL) {
	fprintf(stderr,"No key VSN in init file %s\n",
		my_ini_filename);
	exit(1);
    }

    strcpy(version_string,tmp);
    
    sprintf(erts_dir,"%s\\erts-%s\\bin",root,tmp);
    if ((tmp = lookup_init_entry(my_ini_section, "SYSTEM_VSN")) == NULL) {
	fprintf(stderr,"No key SYSTEM_VSN in init file %s\n",
		my_ini_filename);
	exit(1);
    }
    sprintf(release_dir,"%s\\releases\\%s",root,tmp); 

    sprintf(bin_dir,"%s\\bin",root);
    CreateDirectory(bin_dir,NULL);

    free_init_file(my_ini_file);
    
    for (i = 0; binaries[i] != NULL; ++i) {
	sprintf(fromname,"%s\\%s",erts_dir,binaries[i]);
	sprintf(toname,"%s\\%s",bin_dir,binaries[i]);
	if (GetFileAttributes(fromname) == 0xFFFFFFFF) {
	    fprintf(stderr,"Could not find file %s\n",
		    fromname);
	    exit(1);
	}
	if (!CopyFile(fromname,toname,FALSE)) {
	    fprintf(stderr,"Could not copy file %s to %s\n",
		    fromname,toname);
	    fprintf(stderr,"Continuing installation anyway...\n");
	}
    }
    
    for (i = 0; scripts[i] != NULL; ++i) {
	sprintf(fromname,"%s\\%s",release_dir,scripts[i]);
	sprintf(toname,"%s\\%s",bin_dir,scripts[i]);
	if (GetFileAttributes(fromname) == 0xFFFFFFFF) {
	    fprintf(stderr,"Could not find file %s\n",
		    fromname);
	    exit(1);
	}
	if (!CopyFile(fromname,toname,FALSE)) {
	    fprintf(stderr,"Could not copy file %s to %s\n",
		    fromname,toname);
	    fprintf(stderr,"Cannot continue installation, bailing out.\n");
	    exit(1);
	}
    }
    if (start_sasl) {
	sprintf(fromname,"%s\\start_sasl.boot",bin_dir);
    } else {
	sprintf(fromname,"%s\\start_clean.boot",bin_dir);
    }
    sprintf(toname,"%s\\start.boot",bin_dir);
    if (!CopyFile(fromname,toname,FALSE)) {
	fprintf(stderr,"Could not copy file %s to %s\n",
		fromname,toname);
	fprintf(stderr,"Cannot continue installation, bailing out.\n");
	exit(1);
    }

    /* OBS!!! If the format of the init file is changed, do not forget
       to update release_handler:write_ini_file(...) */
    ini_file = create_init_file();
    ini_section = create_init_section("erlang");
    add_init_section(ini_file,ini_section);
    add_init_entry(ini_section,"Bindir",erts_dir);
    add_init_entry(ini_section,"Progname","erl");
    add_init_entry(ini_section,"Rootdir",root);
    sprintf(fromname,"%s\\erl.ini",erts_dir);
    sprintf(toname,"%s\\erl.ini",bin_dir);
    if (store_init_file(ini_file,fromname) != 0) {
	fprintf(stderr,"Could not create file %s\n",
		fromname);
	fprintf(stderr,"Cannot continue installation, bailing out.\n");
	exit(1);
    }
    if (!CopyFile(fromname,toname,FALSE)) {
	fprintf(stderr,"Could not copy file %s to %s\n",
		fromname,toname);
	fprintf(stderr,"Cannot continue installation, bailing out.\n");
	exit(1);
    }
    if (!silent) {
	printf("Erlang %s installed successfully\n", version_string);
    }
    return 0;
}



