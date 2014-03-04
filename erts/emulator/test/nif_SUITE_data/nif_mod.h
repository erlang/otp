typedef struct call_info_t
{
    struct call_info_t* next;
    unsigned lib_ver;
    int static_cntA;
    int static_cntB;
    char* arg;
    int arg_sz;
    char func_name[1]; /* must be last */
}CallInfo;

#define RT_MAX 5

typedef struct
{
    ErlNifMutex* mtx;
    int ref_cnt;
    CallInfo* call_history;
    ErlNifResourceType* rt_arr[RT_MAX];
}NifModPrivData;

#define NifModPrivData_release(NMPD) \
    do { \
	int is_last; \
	enif_mutex_lock((NMPD)->mtx); \
	is_last = (--(NMPD)->ref_cnt == 0); \
        enif_mutex_unlock((NMPD)->mtx); \
        if (is_last) { \
	    enif_mutex_destroy((NMPD)->mtx); \
	    while ((NMPD)->call_history) { \
	        CallInfo* next = (NMPD)->call_history->next; \
		enif_free((NMPD)->call_history); \
		(NMPD)->call_history = next; \
	    } \
	    enif_free((NMPD)); \
        } \
    }while (0)

