%% @copyright 2008 Jacob Torrey
%% @author Jacob Torrey <torreyji@clarkson.edu>
%% @doc The OSP jumping off module
-module(osp).
-export([start/0, stop/0, join/1, setup/0]).

-include("../include/conf.hrl").

-define(VERSION, "0.4").

%% @doc Starts the first 'master' node
%% @spec init() -> {ok, Pid, []} | {error, Reason}
start() ->
    case ?USEFQDN of
	true ->
	    net_kernel:start([?NODENAME]);
	false ->
	    net_kernel:start([?NODENAME, shortnames])
    end,
    erlang:set_cookie(node(), ?COOKIE),
    application:start(mnesia),
    Pid = osp_broker:start(osp_admin, ?ADMINPORT),
    case Pid of
	{error, Err} ->
	    {error, Err};
	_ ->
	    osp_web:start(),
	    {ok, Pid}
    end.

%% @doc Stops OSP on this node
%% @spec stop() -> ok
stop() ->
    osp_broker:stop(osp_admin),
    osp_broker:shutdown(),
    ok.

%% @doc Setups mnesia for the first time
%% @spec setup() -> ok
setup() ->
    case ?USEFQDN of
	true ->
	    net_kernel:start([?NODENAME]);
	false ->
	    net_kernel:start([?NODENAME, shortnames])
    end,
    erlang:set_cookie(node(), ?COOKIE),
    write_rel(),
    mnesia:create_schema([node()]),
    init:stop().

%% @doc Joins node to an existing OSP cluster
%% @spec join(list()) -> pong | pang
join([Node]) ->
    application:start(sasl),
    application:start(os_mon),
    net_adm:ping(Node).

get_vsn(Module) ->
    AppFile = code:lib_dir(Module) ++ "/ebin/" ++ atom_to_list(Module) ++ ".app",
    {ok, [{application, _App, Attrs}]} = file:consult(AppFile),
    {value, {vsn, Vsn}} = lists:keysearch(vsn, 1, Attrs),
    Vsn.

write_rel() ->
    F = lists:append(["{release, {\"osp_rel\",\"",?VERSION,"\"}, \n",
                      "{erts,\"",erlang:system_info(version),"\"},\n"
                      "[{kernel,\"",get_vsn(kernel),"\"},\n",
                      "{stdlib,\"",get_vsn(stdlib),"\"},\n",
                      "{inets,\"",get_vsn(inets),"\"},\n",
                      "{os_mon,\"",get_vsn(os_mon),"\"},\n",
		      "{sasl,\"",get_vsn(sasl),"\"},\n",
		      "{mnesia,\"",get_vsn(mnesia),"\"},\n",
                      "{osp,\"",?VERSION,"\"}]}.\n"]),
    ok = file:write_file("osp_rel-" ++ ?VERSION ++ ".rel", F),
    systools:make_script("osp_rel-" ++ ?VERSION, [local]).
