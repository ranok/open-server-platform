%% @author Jacob Torrey
%% @copyright 2009 Jacob Torrey <torreyji@clarkson.edu>
-module(osp_web).
-export([start/0, reload/0, stop/0, restart/0, clusterwide/3]).

-define(CONF_FILE, "include/httpd.conf").

%% @doc Starts Inets and the OSP Web server
%% @spec start() -> {ok, pid()}
start() ->
    inets:start(),
    {ok, [Conf]} = file:consult(?CONF_FILE),
    inets:start(httpd, Conf).

%% @doc Reloads the webserver configuration from file
%% @spec reload() -> ok
reload() ->
    {ok, [Conf]} = file:consult(?CONF_FILE),
    httpd:reload_config(Conf, non_disturbing).

%% @doc Restarts the web admin panel
%% @spec restart() -> {ok, pid()}
restart() ->
    stop(),
    start().

%% @doc Stops the Inets daemon and the web service
%% @todo This needs to be made more generic for the case of other inets services running
stop() ->
    inets:stop().

%% @doc Parses an input string into a list of tuples
parse_input(Input) ->
    Args = string:tokens(Input, "&"),
    F = fun(E) ->
		[Key, Value] = string:tokens(E, "="),
		{erlang:list_to_atom(Key), Value}
	end,
    lists:map(F, Args).

clusterwide(Session, _Env, Input) ->
    Args = parse_input(Input),
    {value, {operation, Op}} = lists:keysearch(operation, 1, Args),
    io:format("~p~n", [Op]),
    case Op of
	"shutdown" ->
	    mod_esi:deliver(Session, "shutdown"),
	    osp_admin:shutdown_osp();
	_ ->
	    mod_esi:deliver(Session, "")
    end.
