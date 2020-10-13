%% @doc Fixtures for chunk links.
%% See http://erlang.org/doc/apps/erl_docgen/inline_tags.html#%3Csee*%3E---see-tags for
%% the description of `seemfa', `seetype', etc link types.
-module(eep48_links).

-export([f/0,
	 module_link/0,
	 app_link/0,
	 app_module_link/0,
	 app_mfa_link/0,
	 external_function_link/0,
	 local_function_link/0,
	 local_type_link/0,
	 external_type_link/0]).

-export([see_module/0,
	 see_app/0,
	 see_app_module/0,
	 see_app_mfa/0,
	 see_external_function/0,
	 see_local_function/0,
	 see_local_type/0,
	 see_external_type/0]).

-export_type([t/0]).

-type t() :: {}.

f() -> ok.

%% @doc Link to this module {@link eep48_links}.
%% Should map to `seeerl'.
module_link() -> ok.

%% @doc Link to application {@link //edoc}.
%% Should map to `seeapp'.
app_link() -> ok.

%% @doc Link to application module {@link //edoc/edoc_doclet}.
%% Should map to `seeerl'.
app_module_link() -> ok.

%% @doc Link to application M:F/A {@link //edoc/edoc:files/2}.
%% Should map to `seemfa'.
app_mfa_link() -> ok.

%% @doc Link to external function {@link eep48_SUITE:suite/0}.
%% Should map to `seemfa'.
external_function_link() -> ok.

%% @doc Link to local function {@link f/0}.
%% Should map to `seemfa'.
local_function_link() -> ok.

%% @doc Local type link {@link t()}.
%% Should map to `seetype'.
local_type_link() -> ok.

%% @doc External type link {@link eep48_links:t()}.
%% Should map to `seetype'.
external_type_link() -> ok.

%% @see eep48_links. Should map to `seeerl'.
see_module() -> ok.

%% @see //edoc. Should map to `seeapp'.
see_app() -> ok.

%% @see //edoc/edoc_doclet. Should map to `seeerl'.
see_app_module() -> ok.

%% @see //edoc/edoc:files/2.
see_app_mfa() -> ok.

%% @see eep48_SUITE:suite/0. Should map to `seemfa'.
see_external_function() -> ok.

%% @see f/0. Should map to `seemfa'.
see_local_function() -> ok.

%% @see t(). Should map to `seetype'.
see_local_type() -> ok.

%% @see eep48_links:t(). Should map to `seetype'.
see_external_type() -> ok.

%% @doc This is referenced from another function with the `equiv' tag.
equiv_target(Arg) -> Arg.

%% @equiv equiv_target(ok)
fun_with_equiv_tag() -> equiv_target(ok).

%% @equiv {<<"arbitrary">>, erlang, "term"}
fun_with_non_call_equiv_tag() -> ok.

%% @equiv equiv_target(ok)
%% @doc This function has equiv, doc, and see tags.
%%
%% And a somewhat long description to check how `shell_docs' renders paragraphs.
%% @end
%% @see equiv_target/1
fun_with_equiv_doc_and_see() -> equiv_target(ok).
