/*
 * %CopyrightBegin%
 * 
 * Copyright Ericsson AB 2009-2016. All Rights Reserved.
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
 * This is a C version of the erl Bourne shell script
 */

#include "etc_common.h"

#define BOOL int
#define TRUE 1
#define FALSE 0
#define PATHSEP ":"
#define DIRSEP "/"
#define DIRSEPCHAR '/'

static void
error(char* format, ...)
{
    char sbuf[1024];
    va_list ap;

    va_start(ap, format);
    vsprintf(sbuf, format, ap);
    va_end(ap);
    fprintf(stderr, "erl: %s\n", sbuf);
    exit(1);
}

/*
 * Variables.
 */

/*
 * Manage memory
 */

static void *
emalloc(size_t size)
{
    void *p = malloc(size);
    if (p == NULL)
        error("Insufficient memory");
    return p;
}

/*
static void *
erealloc(void *p, size_t size)
{
    void *res = realloc(p, size);
    if (res == NULL)
        error("Insufficient memory");
    return res;
}
*/

static void
efree(void *p)
{
    free(p);
}

static char*
strsave(char* string)
{
    char* p = emalloc(strlen(string)+1);
    strcpy(p, string);
    return p;
}

/*
 * Manage environment variables
 */

static char *
get_env(char *key)
{
    return getenv(key);
}

static void
set_env(char *key, char *value)
{
    size_t size = strlen(key) + 1 + strlen(value) + 1;
    char *str = emalloc(size);
    sprintf(str, "%s=%s", key, value);
    if (putenv(str) != 0)
        error("putenv(\"%s\") failed!", str);
#ifdef HAVE_COPYING_PUTENV
    efree(str);
#endif
}

// /* A realpath look alike */
// static char *
// follow_symlinks(const char *path, char *resolved_path)
// {
//     char tmp[PATH_MAX];
//     int  len; 
// 
//     strcpy(resolved_path, path);
//     
//     for (;;) {
// 	len = readlink(resolved_path, tmp, PATH_MAX);
// 
// 	if (len == -1) {
// 	    if (errno == EINVAL) {
// 		/* Not a symbolic link. use the original name */
// 		break;
// 	    } else {
// 		return NULL;
// 	    }
// 	} else {
// 	    tmp[len] = '\0';
// 	    strcpy(resolved_path, tmp);
// 	}
//     }
//     
//     return resolved_path;
// }

/*
 * Find absolute path to this program
 */

static char *
find_prog(char *origpath)
{
    char relpath[PATH_MAX];
    char abspath[PATH_MAX];

    strcpy(relpath, origpath);

    if (strstr(relpath, DIRSEP) == NULL) {
        /* Just a base name */
        char *envpath;

        envpath = get_env("PATH");
        if (envpath) {
            /* Try to find the executable in the path */
            char dir[PATH_MAX];
            char *beg = envpath;
            char *end;
            int sz;
            DIR *dp;             /* Pointer to directory structure. */
            struct dirent* dirp; /* Pointer to directory entry.     */
            BOOL look_for_sep = TRUE;

            while (look_for_sep) {
                end = strstr(beg, PATHSEP);
                if (end != NULL) {
                    sz = end - beg;
                    strncpy(dir, beg, sz);
                    dir[sz] = '\0';
                } else {
                    sz = strlen(beg);
                    strcpy(dir, beg);
                    look_for_sep = FALSE;
                }
                beg = end + 1;

                dp = opendir(dir);
                if (dp != NULL) {
                    while (TRUE) {
                        dirp = readdir(dp);
                        if (dirp == NULL) {
                            closedir(dp);
                            /* Try next directory in path */
                            break;
                        }

                        if (strcmp(origpath, dirp->d_name) == 0) {
                            /* Wow. We found the executable. */
                            strcpy(relpath, dir);
                            strcat(relpath, DIRSEP);
                            strcat(relpath, dirp->d_name);
                            closedir(dp);
                            look_for_sep = FALSE;
                            break;
                        }
                    }
                }
            }
        }
    }

    if (!realpath(relpath, abspath)) {
        error("Cannot determine real path to erl");
    }

    return strdup(abspath);
}

/*
 * Find bindir
 */

static void
copy_latest_vsn(char *latest_vsn, char *next_vsn)
{
    char *lp;
    char *np;
    BOOL greater;

    /* Find vsn */
    for (lp = latest_vsn+strlen(latest_vsn)-1 ;lp > latest_vsn && *lp != DIRSEPCHAR; --lp)
	;
	
    /* lp =+ length("erts-"); */
    for (np = next_vsn+strlen(next_vsn)-1 ;np > next_vsn && *np != DIRSEPCHAR; --np)
       ;

    /* np =+ length("erts-"); */
    while (TRUE) {
        if (*lp != *np) {
	    if (*np > *lp) {
		greater = TRUE;
	    } else {
		greater = FALSE;
	    }
	    
	    /* Find next dot or eos */
	    while (*lp != '\0' && *np != '\0') {
		lp++;
		np++;
		if (*np == '.' && *lp == '.') {
		    break;
		}
		if (*np == '\0' && *lp == '\0') {
		    break;
		}
		if (*lp == '.' || *lp == '\0') {
  		    greater = TRUE;
		}
		if (*np == '.' || *np == '\0') {
		    greater = FALSE;
		}
	    }
	    if (greater) {
		strcpy(latest_vsn, next_vsn);
	    }
	    return;
	}
	++lp;
	++np;
    }
}

static char *
find_erts_vsn(char *erl_top)
{
    /* List install dir and look for latest erts-vsn */
    DIR *dp;                    /* Pointer to directory structure. */
    struct dirent* dirp;        /* Pointer to directory entry.     */
    char latest_vsn[PATH_MAX];  /* Latest erts-vsn directory name. */

    dp = opendir(erl_top);
    if (dp == NULL) {
        return NULL;
    }

    latest_vsn[0] = '\0';
    for (;;) {
        dirp = readdir(dp);
        if (dirp == NULL) {
            closedir(dp);
            break;
        }
        if (strncmp("erts-", dirp->d_name, 5) == 0) {
	    copy_latest_vsn(latest_vsn, dirp->d_name);
        }
    }

    if (latest_vsn[0] == '\0') {
        return NULL;
    } else {
	char *p = malloc((strlen(erl_top)+1+strlen(latest_vsn)+4+1)*sizeof(char));
        strcpy(p,erl_top);
        strcat(p,DIRSEP);
        strcat(p,latest_vsn);
        strcat(p,DIRSEP);
        strcat(p,"bin");
        return p;
    }
}

static char *
find_bindir(char *erlpath)
{
    /* Assume that the path to erl is absolute and
     * that it is not a symbolic link*/

    char *p;
    char *p2;
    char buffer[PATH_MAX];

    strcpy(buffer, erlpath);

    /* Chop of base name*/
    for (p = buffer+strlen(buffer)-1 ;p >= buffer && *p != DIRSEPCHAR; --p)
        ;
    *p = '\0';
    p--;

    /* Check if dir path is like ...\buffer\erts-vsn\bin */
    for (;p >= buffer && *p != DIRSEPCHAR; --p)
        ;
    p--;
    for (p2 = p;p2 >= buffer && *p2 != DIRSEPCHAR; --p2)
        ;
    p2++;
    if (strncmp(p2, "erts-", 5) == 0) {
	p = strsave(buffer);
	return p;
    }

    /* Assume that dir path is like ...\buffer\bin */
    *++p ='\0'; /* chop off bin dir */

    p = find_erts_vsn(buffer);
    if (p == NULL) {
	return strsave(buffer);
    } else {
	return p;
    }
}

/*
 * main
 */

int
main(int argc, char **argv)
{
    char *p;
    char *abspath;
    char *bindir;            /* Location of executables. */
    char rootdir[PATH_MAX];  /* Root location of Erlang installation. */
    char progname[PATH_MAX]; /* Name of this program. */
    char erlexec[PATH_MAX];  /* Path to erlexec */

    /* Determine progname */
    abspath = find_prog(argv[0]);
    strcpy(progname, abspath);
    for (p = progname+strlen(progname)-1;p >= progname && *p != '/'; --p)
	;

    /* Determine bindir */
    bindir = find_bindir(abspath);

    /* Determine rootdir */
    strcpy(rootdir, bindir);
    for (p = rootdir+strlen(rootdir)-1;p >= rootdir && *p != '/'; --p)
	;
    p--;
    for (;p >= rootdir && *p != '/'; --p)
	;
    *p ='\0';

    /* Update environment */
    set_env("EMU", "beam");
    set_env("PROGNAME", progname);
    set_env("BINDIR", bindir);
    set_env("ROOTDIR", rootdir);

    /* Invoke erlexec */
    strcpy(erlexec, bindir);
    strcat(erlexec, DIRSEP);
    strcat(erlexec, "erlexec");

    efree(abspath);
    efree(bindir);

    execvp(erlexec, argv);
    error("Error %d executing \'%s\'.", errno, erlexec);
    return 2;
}
