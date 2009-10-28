%% @copyright 2009 Jacob Torrey <torreyji@clarkson.edu>
%% @author Jacob Torrey <torreyji@clarkson.edu>
%% @doc The exports for controlling the OSP system as a whole
-module(osp).
-export([start/0, stop/1, join/1, setup/0, gen_docs/0]).

-include("../include/conf.hrl").

-define(VERSION, "0.4").

%% @doc Starts the first 'master' node
%% @spec start() -> {ok, Pid, pid()} | {error, Reason}
start() ->
    case ?USEFQDN of
	true ->
	    net_kernel:start([?NODENAME]);
	false ->
	    net_kernel:start([?NODENAME, shortnames])
    end,
    erlang:set_cookie(node(), ?COOKIE),
    code:add_path(?APP_DIR),
    application:start(mnesia),
    Pid = osp_broker:start(osp_admin, ?ADMINPORT),
    case Pid of
	{error, Err} ->
	    {error, Err};
	_ ->
	    {ok, Pid, osp_web:start()}
    end.

%% @doc Stops OSP on this node
%% @spec stop(Pid) -> ok
stop(Pid) ->
    osp_broker:stop(osp_admin),
    osp_broker:shutdown(),
    osp_web:stop(Pid).

%% @doc Setups mnesia and the OTP rel scripts for the first time
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

%% @doc Dynamically gets the version of all the required applications to run OSP
%% @spec get_vsn(atom()) -> list()
get_vsn(Module) ->
    AppFile = code:lib_dir(Module) ++ "/ebin/" ++ atom_to_list(Module) ++ ".app",
    {ok, [{application, _App, Attrs}]} = file:consult(AppFile),
    {value, {vsn, Vsn}} = lists:keysearch(vsn, 1, Attrs),
    Vsn.

%% @doc Writes the .rel file and generates the boot scripts for the first OSP run
%% @spec write_rel() -> ok
write_rel() ->
    F = lists:append(["{release, {\"osp_rel\",\"",?VERSION,"\"}, \n",
                      "{erts,\"",erlang:system_info(version),"\"},\n"
                      "[{kernel,\"",get_vsn(kernel),"\"},\n",
                      "{stdlib,\"",get_vsn(stdlib),"\"},\n",
                      "{os_mon,\"",get_vsn(os_mon),"\"},\n",
                      "{inets,\"",get_vsn(inets),"\"},\n",
		      "{sasl,\"",get_vsn(sasl),"\"},\n",
                      "{osp,\"",?VERSION,"\"}]}.\n"]),
    ok = file:write_file("osp_rel-" ++ ?VERSION ++ ".rel", F),
    systools:make_script("osp_rel-" ++ ?VERSION, [local]).

%% @doc Provides an export to regenerate the documentation with edoc
%% @spec gen_docs() -> ok
gen_docs() ->
    edoc:application(osp, ".", []).
