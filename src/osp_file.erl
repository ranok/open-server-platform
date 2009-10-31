%% @author Jacob Torrey <torreyji@clarkson.edu>
%% @copyright 2009 Jacob Torrey <torreyji@clarkson.edu>
%% @doc File operations for OSP programs
-module(osp_file).

-include("../include/conf.hrl").
-export([fopen/3, fread/2, fwrite/2, fclose/1, fseek/3]).

%% @doc Calls a command either on the local FS, or the remote FS depending on how the FS is setup
%% @spec call_func(atom(), atom(), list()) -> any()
call_func(M, F, A) ->
    case ?SHARED_FS of
	true ->
	    Ret = apply(M, F, A);
	false ->
	    Ret = rpc:call(?NODENAME, M, F, A)
    end,
    Ret.

%% @doc Opens a file for reading or writing
%% @spec fopen(list(), string(), list()) -> io_device() | {err, list()}
%% @todo Harden the file jailing system
fopen(App, Filename, Flags) ->
    Filename2 = ?FS_PREFIX ++ "/" ++ App ++ "/" ++ Filename,
    Ret = call_func(file, open, [Filename2, Flags]),
    case Ret of
	{ok, FP} ->
	    FP;
	{error, Res} ->
	    {err, Res}
    end.

%% @doc Closes an open file
%% @spec fclose(io_device()) -> ok | {err, Reason}
fclose(FP) ->
    Ret = call_func(file, close, [FP]),
    case Ret of
	ok ->
	    ok;
	{error, Res} ->
	    {err, Res}
    end.

%% @doc Seeks to a position in a file
%% @spec fseek(io_device(), atom(), int()) -> ok | {err, list()}
fseek(FP, cur, Off) ->
    Ret = call_func(file, position, [FP, {cur, Off}]),
    case Ret of
	{ok, _NOff} ->
	    ok;
	{error, Reason} ->
	    {err, Reason}
    end;
fseek(FP, set, Off) ->
    Ret = call_func(file, position, [FP, {bof, Off}]),
    case Ret of
	{ok, _NOff} ->
	    ok;
	{error, Reason} ->
	    {err, Reason}
    end;
fseek(FP, eof, Off) ->
    Ret = call_func(file, position, [FP, {eof, Off}]),
    case Ret of
	{ok, _NOff} ->
	    ok;
	{error, Reason} ->
	    {err, Reason}
    end.

%% @doc Writes to a file
%% @spec fwrite(io_device(), any()) -> ok | {err, list()}
fwrite(FP, Dat) ->
    Ret = call_func(file, write, [FP, Dat]),
    case Ret of
	ok ->
	    ok;
	{error, Reason} ->
	    {err, Reason}
    end.

%% @doc Reads from a file
%% @spec fread(io_device(), int()) -> any() | {eof} | {err, list()}
fread(FP, Num) ->
    Ret = call_func(file, read, [FP, Num]),
    case Ret of
	{ok, Data} ->
	    Data;
	eof ->
	    {eof};
	{error, Reason} ->
	    {err, Reason}
    end.
