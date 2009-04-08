%% @author Jacob Torrey <torreyji@clarkson.edu>
%% @copyright 2008 Jacob Torrey
%% @doc A module for abstracting protocols from OSP servlets
-module(osp_proto).

% Definitions
-define(TCPOPTS, [{reuseaddr, true}, binary, {packet, 0}, {active, false}]).
-define(UDPOPTS, [binary, {reuseaddr, true}, {active, false}]).
-define(SCTPOPTS, [binary, {reuseadr, true}, {active, false}]).

% Exports
-export([start/1, set_control/1, accept/1, close/1]).

%% @doc Returns a function (arity 1) for starting a server socket
start(tcp) ->
    fun(Port) ->
	    gen_tcp:listen(Port, ?TCPOPTS)
    end;
start(sctp) ->
    fun(Port) ->
	    gen_sctp:listen(Port, ?SCTPOPTS)
    end;
start(udp) ->
    fun(Port) ->
	    gen_udp:open(Port, ?UDPOPTS)
    end.

%% @doc Returns a function that sets the controlling process of Sock to Pid
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
	    gen_sctp:recv(Sock, 0, Timeout)
    end;
accept(udp) ->
    fun(Sock, Timeout) ->
	    gen_udp:recv(Sock, 0, Timeout)
    end.
