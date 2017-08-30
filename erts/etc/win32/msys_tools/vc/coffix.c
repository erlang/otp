/*
 * %CopyrightBegin%
 * 
 * Copyright Ericsson AB 1999-2016. All Rights Reserved.
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
** This mini tool fixes an incompatibility between 
** Microsoft's tools, who dont like the virtual size being put in
** the physical address field, but rely on the raw size field for
** sizing the ".bss" section.
** This fixes some of the problems with linking gcc compiled objects
** together with MSVC dito.
**
** Courtesy DJ Delorie for describing the COFF file format on 
** http://www.delorie.com/djgpp/doc/coff/
** The coff structures are fetched from Microsofts headers though.
*/

#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <stdarg.h>

#include <windows.h>
#include <winnt.h> /* Structure definitions for PE (COFF) */

static int dump_edit(char *filename, int edit);
static int v_printf(char *format, ...);


char *progname;
int verbouse = 0;

int main(int argc, char **argv) 
{
    int findex = 1;
    int edit = 0;
    int ret;

    progname = argv[0];
    if (argc == 1) {
	fprintf(stderr,"Format : %s [-e] [-v] <filename>\n", progname);
	return 1;
    }
    for (findex = 1; 
	 findex < argc && (*argv[findex] == '-' || *argv[findex] == '/');
	 ++findex)
	switch (argv[findex][1]) {
	case 'e':
	case 'E':
	    edit = 1;
	    break;
	case 'v':
	case 'V':
	    verbouse = 1;
	default:
	    fprintf(stderr, "%s: unknown option %s\n", progname, argv[findex]);
	    break;
	}
    if (findex == argc) {
	fprintf(stderr,"%s: No filenames given.\n", progname);
	return 1;
    }
    for(; findex < argc; ++findex)
	if ((ret = dump_edit(argv[findex],edit)) != 0)
	    return ret;
    return 0;
}

int dump_edit(char *filename, int edit) 
{
    FILE *f = fopen(filename, (edit) ? "r+b" : "rb");
    IMAGE_FILE_HEADER filhdr;
    IMAGE_SECTION_HEADER scnhdr;
    int i;

    if (f == NULL) {
	fprintf(stderr, "%s: cannot open %s.\n", progname, filename);
	return 1;
    }
    
    if (fread(&filhdr, sizeof(filhdr), 1, f) == 0) {
	fprintf(stderr,"%s: Could not read COFF header from %s,"
		" is this a PE (COFF) file?\n", progname, filename);
	fclose(f);
	return 1;
    }
    v_printf("File: %s\n", filename);
    v_printf("Magic number: 0x%08x\n", filhdr.Machine);
    v_printf("Number of sections: %d\n",filhdr.NumberOfSections);
    
    if (fseek(f, (long) filhdr.SizeOfOptionalHeader, SEEK_CUR) != 0) {
	fprintf(stderr,"%s: Could not read COFF optional header from %s,"
		" is this a PE (COFF) file?\n", progname, filename);
	fclose(f);
	return 1;
    }
    
    for (i = 0; i < filhdr.NumberOfSections; ++i) {
	if (fread(&scnhdr, sizeof(scnhdr), 1, f) == 0) {
	    fprintf(stderr,"%s: Could not read section header from %s,"
		    " is this a PE (COFF) file?\n", progname, filename);
	    fclose(f);
	    return 1;
	}
	v_printf("Section %s:\n", scnhdr.Name);
	v_printf("Physical address: 0x%08x\n", scnhdr.Misc.PhysicalAddress);
	v_printf("Size: 0x%08x\n", scnhdr.SizeOfRawData);
	if (scnhdr.Misc.PhysicalAddress != 0 &&
	    scnhdr.SizeOfRawData == 0) {
	    printf("Section header %s in file %s will confuse MSC linker, "
		   "virtual size is 0x%08x and raw size is 0\n",
		   scnhdr.Name, filename, scnhdr.Misc.PhysicalAddress, 
		   scnhdr.SizeOfRawData);
	    if (edit) {
		scnhdr.SizeOfRawData = scnhdr.Misc.PhysicalAddress;
		scnhdr.Misc.PhysicalAddress = 0;
		if (fseek(f, (long) -((long)sizeof(scnhdr)), SEEK_CUR) != 0 ||
		    fwrite(&scnhdr, sizeof(scnhdr), 1, f) == 0) {
		    fprintf(stderr,"%s: could not edit file %s.\n",
			    progname, filename);
		    fclose(f);
		    return 1;
		}
		printf("Edited object, virtual size is now 0, and "
		       "raw size is 0x%08x.\n", scnhdr.SizeOfRawData);
	    } else {
		printf("Specify option '-e' to correct the problem.\n");
	    }
	}
    }
    fclose(f);
    return 0;
}
	    

static int v_printf(char *format, ...)
{
    va_list ap;
    int ret = 0;
    if (verbouse) {
	va_start(ap, format);
	ret = vfprintf(stdout, format, ap);
	va_end(ap);
    }
    return ret;
}

