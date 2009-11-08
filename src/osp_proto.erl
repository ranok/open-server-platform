%% @author Jacob Torrey <torreyji@clarkson.edu>
%% @copyright 2009 Jacob Torrey <torreyji@clarkson.edu>
%% @doc A module for abstracting protocols from OSP servlets
-module(osp_proto).

% Definitions
-define(TCPOPTS, [{reuseaddr, true}, binary, {packet, 0}, {active, false}]).
-define(UDPOPTS, [binary, {reuseaddr, true}, {active, false}]).
-define(SCTPOPTS, [binary, {reuseadr, true}, {active, false}]).

% Exports
-export([start/1, start/2, set_control/1, accept/1, close/1]).

%% @doc Returnes a function (arity 1) for starting a server socket with passed socket options
%% @spec start(tcp | udp | sctp, list()) -> fun()
start(tcp, Options) ->
    fun(Port) ->
	    gen_tcp:listen(Port, Options)
    end;
start(sctp, Options) ->
    fun(Port) ->
	    gen_sctp:listen(Port, Options)
    end;
start(udp, Options) ->
    fun(Port) ->
	    gen_udp:open(Port, Options)
    end.

%% @doc Returns a function (arity 1) for starting a server socket with default socket options 
%% @spec start(tcp | udp | sctp) -> fun()
start(tcp) ->
    start(tcp, ?TCPOPTS);
start(sctp) ->
    start(sctp, ?SCTPOPTS);
start(udp) ->
    start(udp, ?UDPOPTS).

%% @doc Returns a function that sets the controlling process of Sock to Pid
%% @spec set_control(tcp | udp | sctp) -> fun()
set_control(tcp) ->
    fun(Sock, Pid) ->
	    gen_tcp:controlling_process(Sock, Pid)
    end;
set_control(sctp) ->
    fun(Sock, Pid) ->
	    gen_sctp:controlling_process(Sock, Pid)
    end;
set_control(udp) ->
    fun(Sock, Pid) ->
	    gen_udp:controlling_process(Sock, Pid)
    end.

%% @doc Returns a function to close the socket
close(tcp) ->
    fun(Sock) ->
	    gen_tcp:close(Sock)
    end;
close(sctp) ->
    fun(Sock) ->
	    gen_sctp:close(Sock)
    end;
close(udp) ->
    fun(Sock) ->
	    gen_udp:close(Sock)
    end.

%% @doc Returns a function to accept a socket or connection
accept(tcp) ->
    fun(Sock, Timeout) ->
	    gen_tcp:accept(Sock, Timeout)
    end;
accept(sctp) ->
    fun(Sock, Timeout) ->
	    gen_sctp:recv(Sock, Timeout)
    end;
accept(udp) ->
    fun(Sock, Timeout) ->
	    gen_udp:recv(Sock, 0, Timeout)
    end.
