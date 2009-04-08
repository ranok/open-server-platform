%% @copyright 2008 Jacob Torrey
%% @author Jacob Torrey <torreyji@clarkson.edu>
%% @doc The OSP jumping off module
-module(osp).
-export([start/0, stop/0, join/1, get_conf/2, setup/0]).

%% @doc Starts the first 'master' node
%% @spec init() -> {ok, Pid, []} | {error, Reason}
start() ->
    ConfFile = "osp.conf",
    case get_conf(usefqdn, ConfFile) of
	true ->
	    net_kernel:start([get_conf(nodename, ConfFile)]);
	false ->
	    net_kernel:start([get_conf(nodename, ConfFile), shortnames])
    end,
    erlang:set_cookie(node(), get_conf(cookie, ConfFile)),
    Pid = osp_broker:start(osp_admin, get_conf(adminport, ConfFile)),
    case Pid of
	{error, Err} ->
	    {error, Err};
	_ ->
	    {ok, Pid, []}
    end.

%% @doc Stops OSP on this node
%% @spec stop(Reason, State) -> ok
stop() ->
    osp_broker:stop(osp_broker),
    osp_broker:shutdown(),
    ok.

%% @doc Setups mnesia for the first time
%% @spec setup() -> ok
setup() ->
    ConfFile = "osp.conf",
    case get_conf(usefqdn, ConfFile) of
	true ->
	    net_kernel:start([get_conf(nodename, ConfFile)]);
	false ->
	    net_kernel:start([get_conf(nodename, ConfFile), shortnames])
    end,
    erlang:set_cookie(node(), get_conf(cookie, ConfFile)),
    mnesia:create_schema([node()]),
    init:stop().

%% @doc Returns the Search term value from the Conf
%% @spec get_conf(atom(), list()) -> any()
get_conf(Search, ConfFile) ->
    {ok, Conf} = file:consult(ConfFile),
    {value, {Search, Value}} = lists:keysearch(Search, 1, Conf),
    Value.

%% @doc Joins node to an existing OSP cluster
%% @spec join(list()) -> pong | pang
join([Node]) ->
    application:start(sasl),
    application:start(os_mon),
    net_adm:ping(Node).
