#ifndef __RECLAIM_H__
#define __RECLAIM_H__


/* The Erlang release for VxWorks includes a simple mechanism for
   "resource reclamation" at task exit - it allows replacement of the
   functions that open/close "files" and malloc/free memory with versions
   that keep track, to be able to "reclaim" file descriptors and memory
   when a task exits (regardless of *how* it exits).

   The interface to this mechanism is made available via this file,
   with the following caveats:

   - The interface may change (or perhaps even be removed, though that
     isn't likely until VxWorks itself provides similar functionality)
     in future releases - i.e. you must always use the version of this
     file that comes with the Erlang release you are using.

   - Disaster is guaranteed if you use the mechanism incorrectly (see
     below for the correct way), e.g. allocate memory with the "tracking"
     version of malloc() and free it with the "standard" version of free().

   - The mechanism (of course) incurs some performance penalty - thus
     for a simple program you may be better off with careful programming,
     making sure that you do whatever close()/free()/etc calls that are
     appropriate at all exit points (though if you need to guard against
     taskDelete() etc, things get messy...).

   To use the mechanism, simply program your application normally, i.e.
   use open()/close()/malloc()/free() etc as usual, but #include this
   file before any usage of the relevant functions. NOTE: To avoid the
   "disaster" mentioned above, you *must* #include it in *all* (or none)
   of the files that manipulate a particular file descriptor, allocated
   memory area, etc. Finally, note that you can obviously not load your
   application before the Erlang system when using this interface.
*/

/* Sorry, no ANSI prototypes yet... */
extern int save_open(),save_creat(),save_socket(),save_accept(),save_close();
#define open	save_open
#define creat	save_creat
#define socket	save_socket
#define accept	save_accept
#define close	save_close
extern FILE *save_fopen(), *save_fdopen(), *save_freopen();
extern int save_fclose();
#define fopen	save_fopen
#define fdopen	save_fdopen
#define freopen	save_freopen
#define fclose	save_fclose
/* XXX Should do opendir/closedir too... */
extern char *save_malloc(), *save_calloc(), *save_realloc();
extern void save_free(), save_cfree();
#define malloc	save_malloc
#define calloc	save_calloc
#define realloc	save_realloc
#define free	save_free
#define cfree	save_cfree

#endif
