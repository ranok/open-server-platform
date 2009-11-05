%% @author Jacob Torrey <torreyji@clarkson.edu>
%% @copyright 2009 Jacob Torrey <torreyji@clarkson.edu>
%% @doc Provides servlet compilation functionality
-module(osp_compile).

-export([compile/1, distribute/2, servlet_to_app/1]).

%% @doc Generates and compiles a servlet from a .sdf
%% @spec compile(string()) -> {ok, list()} | {error, list()}
compile(Filename) ->
    Basename = lists:last(string:tokens(Filename, "/")),
    ModuleName = string:join(lists:reverse(lists:nthtail(1, lists:reverse(string:tokens(Basename, ".")))), "."),
    Ret = os:cmd("./compile_servlet.pl " ++ Filename), % Generate the servlet
    ModuleAtom = erlang:list_to_atom(ModuleName),
    case Ret of 
	"File does" ++ _ ->
	    {error, ["The file failed to upload correctly"]};
	_ ->
	    case lists:prefix(ModuleName, Ret) of
		true ->
		    CompileRet = compile:file(ModuleName ++ ".erl", [verbose, return]),
		    case CompileRet of
			{ok, ModuleAtom, Warnings} ->
			    {ok, ["File " ++ Filename ++ " compiled with warnings", Warnings]};
			{ok, ModuleAtom} ->
			    {ok, ["File " ++ Filename ++ " compiled successfully"]};
			error ->
			    {error, ["There was an error in the compilation stage"]};
			{error, Errors, Warnings} ->
			    {error, ["There were the errors and warnings in the compilation stage", Errors, Warnings]}
		    end;
		false ->
		    {error, ["Invalid servlet file"]}
	    end
    end.

%% @doc Compiles a servlet and moves it to the application directory if compilation is successful
%% @spec servlet_to_app(string()) -> {ok, list()} | {error, list()}
servlet_to_app(Filename) ->
    Compile = compile(Filename),
    Basename = lists:last(string:tokens(Filename, "/")),
    ModuleName = string:join(lists:reverse(lists:nthtail(1, lists:reverse(string:tokens(Basename, ".")))), "."),
    case Compile of
	{ok, _} = Output ->
	    file:rename(ModuleName ++ ".beam", osp:get_conf('APP_DIR') ++ "/" ++ ModuleName ++ ".beam"),
	    file:delete(ModuleName ++ ".erl");
	{error, _} = Output ->
	    file:delete(ModuleName ++ ".erl")
    end,
    Output.

%% @doc Distributes the application to a given node to be run
%% @spec distribute(atom(), node()) -> ok | {error, atom()}
distribute(Module, Node) ->
    ok.
