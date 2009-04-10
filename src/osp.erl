%% @copyright 2008 Jacob Torrey
%% @author Jacob Torrey <torreyji@clarkson.edu>
%% @doc The OSP jumping off module
-module(osp).
-export([start/0, stop/0, join/1, setup/0]).

-include("../include/conf.hrl").

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
    mnesia:create_schema([node()]),
    init:stop().

%% @doc Joins node to an existing OSP cluster
%% @spec join(list()) -> pong | pang
join([Node]) ->
    application:start(sasl),
    application:start(os_mon),
    net_adm:ping(Node).
