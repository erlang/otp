#include <erl_nif.h>

#include <stdio.h>
#include <stdarg.h>

static ERL_NIF_TERM run(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[])
{
    return enif_make_atom(env, "ok");
}

static ErlNifFunc nif_funcs[] =
{
    {"run", 0, run}
};

static void dtor(void* dtor_data)
{
	/* write marker file */
	FILE *f;
	f = fopen("entry_dtor_marker.txt", "w");
	if(f)
	{
		fprintf(f, "%d.\n", (int)dtor_data);
	}
	fclose(f);
}

ERL_NIF_INIT2(tester,nif_funcs,NULL,NULL,NULL,NULL,dtor,(void*)123)
