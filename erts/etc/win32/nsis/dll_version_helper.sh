#! /bin/sh
# 
# %CopyrightBegin%
# 
# Copyright Ericsson AB 2007-2011. All Rights Reserved.
# 
# The contents of this file are subject to the Erlang Public License,
# Version 1.1, (the "License"); you may not use this file except in
# compliance with the License. You should have received a copy of the
# Erlang Public License along with this software. If not, it can be
# retrieved online at http://www.erlang.org/.
# 
# Software distributed under the License is distributed on an "AS IS"
# basis, WITHOUT WARRANTY OF ANY KIND, either express or implied. See
# the License for the specific language governing rights and limitations
# under the License.
# 
# %CopyrightEnd%
# 
# This little helper digs out the current version of microsoft CRT
# by compiling hello world and "parsing" the manifest file...

# To debug using a fake version:

# echo "8.0.50727.763"
# exit 0

cat > hello.c <<EOF
#include <windows.h>
#include <stdio.h>

int main(void)
{
    printf("Hello world\n");
    return 0;
}

EOF
cl /MD hello.c  > /dev/null 2>&1
if [ '!' -f hello.exe.manifest ]; then
    # Gah - VC 2010 changes the way it handles DLL's and manifests... Again...
    # need another way of getting the version
    DLLNAME=`dumpbin.exe /imports hello.exe | egrep MSVCR.*dll`
    DLLNAME=`echo $DLLNAME`
    cat > helper.c <<EOF
#include <windows.h>
#include <stdio.h>

#define REQ_MODULE "$DLLNAME"

int main(void)
{
  DWORD dummy;
  DWORD versize;
  int i,n;
  unsigned char *versinfo;
  char buff[100];

  char *vs_verinfo;
  unsigned int vs_ver_size;
  
  struct LANGANDCODEPAGE {
    WORD language;
    WORD codepage;
  } *translate;

  unsigned int tr_size;
  
  if (!(versize = GetFileVersionInfoSize(REQ_MODULE,&dummy))) {
    fprintf(stderr,"No version info size in %s!\n",REQ_MODULE);
    exit(1);
  }
  versinfo=malloc(versize);
  if (!GetFileVersionInfo(REQ_MODULE,dummy,versize,versinfo)) {
    fprintf(stderr,"No version info in %s!\n",REQ_MODULE);
    exit(2);
  }
  if (!VerQueryValue(versinfo,"\\\\VarFileInfo\\\\Translation",&translate,&tr_size)) {
    fprintf(stderr,"No translation info in %s!\n",REQ_MODULE);
    exit(3);
  }
  n = tr_size/sizeof(translate);
  for(i=0; i < n; ++i) {
    sprintf(buff,"\\\\StringFileInfo\\\\%04x%04x\\\\FileVersion",
	    translate[i].language,translate[i].codepage);
    if (VerQueryValue(versinfo,buff,&vs_verinfo,&vs_ver_size)) {
      printf("%s\n",(char *) vs_verinfo);
      return 0;
    }
  } 
  fprintf(stderr,"Failed to find file version of %s\n",REQ_MODULE);
  return 0;
}
EOF
    cl /MD helper.c version.lib > /dev/null 2>&1
    if [ '!' -f helper.exe ]; then
	echo "Failed to build helper program." >&2
	exit 1
    fi
    NAME=$DLLNAME
    VERSION=`./helper`
else
    VERSION=`grep '<assemblyIdentity' hello.exe.manifest | sed 's,.*version=.\([0-9\.]*\).*,\1,g' | grep -v '<'`
    NAME=`grep '<assemblyIdentity' hello.exe.manifest | sed 's,.*name=.[A-Za-z\.]*\([0-9]*\).*,msvcr\1.dll,g' | grep -v '<'`
fi
#rm -f hello.c hello.obj hello.exe hello.exe.manifest helper.c helper.obj helper.exe helper.exe.manifest
if [ "$1" = "-n" ]; then
    ASKEDFOR=$NAME
else
    ASKEDFOR=$VERSION
fi
if [ -z "$ASKEDFOR" ]; then
    exit 1
fi
echo $ASKEDFOR
exit 0
