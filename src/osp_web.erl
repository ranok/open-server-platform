%% @author Jacob Torrey
%% @copyright 2009 Jacob Torrey <torreyji@clarkson.edu>
-module(osp_web).
-export([start/0, reload/0, stop/1, clusterwide/3]).

-define(CONF_FILE, "include/httpd.conf").

%% @doc Starts Inets and the OSP Web server
%% @spec start() -> {ok, pid()}
start() ->
    {ok, [Conf]} = file:consult(?CONF_FILE),
    {ok, Pid} = inets:start(httpd, Conf),
    Pid.

%% @doc Reloads the webserver configuration from file
%% @spec reload() -> ok
reload() ->
    {ok, [Conf]} = file:consult(?CONF_FILE),
    httpd:reload_config(Conf, non_disturbing).

%% @doc Stops the Inets web service
stop(Pid) ->
    inets:stop(httpd, Pid).

%% @doc Parses an input string into a list of tuples
parse_input(Input) ->
    Args = string:tokens(Input, "&"),
    F = fun(E) ->
		[Key, Value] = string:tokens(E, "="),
		{erlang:list_to_atom(Key), Value}
	end,
    lists:map(F, Args).

%% @doc Converts a UNIX newline to a HTML <br />
nl2br(Str) ->
    Tokens = string:tokens(Str, "$\n"),
    string:join(Tokens, "\n<br />").

clusterwide(Session, _Env, Input) ->
    Args = parse_input(Input),
    {value, {operation, Op}} = lists:keysearch(operation, 1, Args),
    case Op of
	"shutdown" ->
	    mod_esi:deliver(Session, "OSP Shutdown"),
	    osp_admin:shutdown_osp();
	"stats" ->
	    mod_esi:deliver(Session, nl2br(osp_admin:stats_osp()));
	_ ->
	    mod_esi:deliver(Session, "")
    end.
