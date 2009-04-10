%% @author Jacob Torrey <torreyji@clarkson.edu>
%% @copyright 2008 Jacob Torrey
%% @doc A socket request broker
-module(osp_broker).
-author('Jacob Torrey <torreyji@clarkson.edu>').

-export([start/2, stop/1, shutdown/0]).

% Definitions
-define(TCPOPTS, [{reuseaddr, true}, binary, {packet, 0}, {active, false}]).

%% @doc Shuts down the Mnesia database
%% @spec shutdown() -> ok
shutdown() -> 
    ok.

%% @doc Stops the server broker and all it's children processes
%% @spec stop(atom()) -> ok
stop(Name) ->
    Name ! stop,
    ok.

%% @doc Starts the server named Name on port Port
%% @spec start(atom(), int()) -> ok | {error, Reason}
start(Name, Port) ->
    Start = osp_proto:start(apply(Name, proto, [])),
    case Start(Port) of
	{ok, LSock} ->
	    apply(Name, start_mnesia, []),
	    apply(Name, init, []),
	    Pid = spawn(fun() -> server_loop(Name, LSock) end),
	    CP = osp_proto:set_control(apply(Name, proto, [])),
	    CP(LSock, Pid),
	    erlang:register(Name, Pid),
	    Pid;
	Error ->
	    {error, Error}
    end.

%% @doc The server broker main code
%% @spec server_loop(atom(), socket()) -> none()
server_loop(Name, LSock) ->
    process_flag(trap_exit, true),
    receive
	stop ->
	    Close = osp_proto:close(apply(Name, proto, [])),
	    Close(LSock),
	    apply(Name, cleanup, []),
	    exit(shutdown);
	{'EXIT', Pid, _} ->
	    Address = get(Pid),
	    put(Pid, undefined),
	    put(Address, undefined),
	    server_loop(Name, LSock);
	_ ->
	    server_loop(Name, LSock) % Clean up the mailbox to prevent slowdown
    after 0 ->
	    Accept = osp_proto:accept(apply(Name, proto, [])),
	    case Accept(LSock, 500) of
		{ok, {Address, Port, Packet}} ->
		    case get(Address) of
			undefined ->
			    Pid = spawn_link(Name, server, [{udp, {LSock, Address, Port}}]),
			    Pid ! {packet, Packet},
			    put(Pid, Address),
			    put(Address, Pid);
			Pid ->
		       	    Pid ! {packet, Packet}
		    end,
		    server_loop(Name, LSock);
		{ok, Sock} ->
		    Pid = spawn_link(Name, server, [{tcp, Sock}]),
		    gen_tcp:controlling_process(Sock, Pid),
		    server_loop(Name, LSock);
		{error, timeout} ->
		    server_loop(Name, LSock);
		Error ->
		    error_logger:error_msg("There was an error accepting a socket in ~p's socket broker: ~p~n", [Name, Error]),
		    exit(Error)
	    end
    end.
