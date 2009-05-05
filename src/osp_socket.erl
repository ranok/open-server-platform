%% @author Jacob Torrey <torreyji@clarkson.edu>
%% @copyright 2009 Jacob Torrey <torreyji@clarkson.edu>
%% @doc Socket operations for OSP programs
-module(osp_socket).

-export([send/2, recv/2, sendf/3, close/1]).

%% @doc A type/protocol inspecific send function
%% @spec spec(tuple(), any()) -> ok
send({udp, {Sock, Addr, Port}}, Data) when is_binary(Data) ->
    gen_udp:send(Sock, Addr, Port, Data);
send({udp, {Sock, Addr, Port}}, Data) when is_list(Data) ->
    gen_udp:send(Sock, Addr, Port, erlang:list_to_binary(Data));
send({udp, {Sock, Addr, Port}}, Data) when is_atom(Data) ->
    gen_udp:send(Sock, Addr, Port, erlang:term_to_binary(Data));
send({tcp, Sock}, Data) when is_binary(Data) ->
    gen_tcp:send(Sock, Data);
send({tcp, Sock}, Data) when is_list(Data) ->
    gen_tcp:send(Sock, erlang:list_to_binary(Data));
send({tcp, Sock}, Data) when is_atom(Data) ->
    gen_tcp:send(Sock, erlang:term_to_binary(Data)).

%% @doc A formatted send that uses the same arguments as io:format
%% @spec sendf(tuple(), list(), list()
sendf(Sock, Template, Args) ->
    send(Sock, io_lib:format(Template, Args)).

%% @doc Closes a socket (protocol inspecific)
%% @spec close(tuple()) -> ok
close({tcp, Sock}) ->
    gen_tcp:close(Sock);
close({udp, {_Sock, _Addr, _Port}}) ->
    ok.

%% @doc A 'safe' receive. A length of 0 will get a 'line' of data
%% @spec recv(tuple(), int()) -> binary() | {error, closed}
recv({tcp, Sock}, Len) ->
    case gen_tcp:recv(Sock, Len) of
	{ok, Bin} ->
	    Bin; % Return the received binary
	{error, closed} ->
	    {error, closed}; % Return the closed atom if the socket is closed
	Error ->
	    error_logger:error_msg("Socket receive error: ~p~n", [Error]),
	    exit(Error) % Uh oh!
    end;
recv({udp, {Sock, Addr, Port}}, 0) ->
    recv({udp, {Sock, Addr, Port}}, <<"">>, 1);
recv({udp, {Sock, Addr, Port}}, Len) ->
    recv({udp, {Sock, Addr, Port}}, <<"">>, Len).

recv({udp, {_Sock, _Addr, _Port}}, Bin, 0) ->
    Bin;
recv({udp, {Sock, Addr, Port}}, Bin, Len) ->
    receive
	{packet, Packet} ->
	    recv({udp, {Sock, Addr, Port}}, list_to_binary([Bin, Packet]), Len - 1);
	{error, closed} ->
	    {error, closed};
	Error ->
	    exit(Error)
    end.
