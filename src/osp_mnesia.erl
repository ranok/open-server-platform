%% @copyright 2009 Jacob Torrey <torreyji@clarkson.edu>
%% @author Jacob Torrey <torreyji@clarkson.edu>
%% @doc A Mnesia interface for OSP
-module(osp_mnesia).

-export([store/3, retrieve/2, start_atomic/0, flush/1]).

% Define the Mnesia record
-record(osp_table, {key, val}).

%% @doc Stores a key value set to the database
%% @spec(name(), name(), any()) -> ok
store(Tab, Key, Val) ->
    case get(atomic) of
	true ->
	    % Add to transaction log
	    Log = get(transaction_log), 
	    case Log of
		undefined ->
		    put(transaction_log, [{Key, Val}]);
		[_] ->
		    case lists:keymember(Key, 1, Log) of
			true ->
			    put(transaction_list, lists:keyreplace(Key, 1, Log, {Key, Val}));
			false ->
			    put(transaction_log, lists:append(Log, [{Key, Val}]))
		    end
	    end;
	undefined ->
	    % Write right away
	    Row = #osp_table{key = Key, val = Val},
	    F = fun() ->
			mnesia:write(Tab, Row, write)
		end,
	    {atomic, ok} = mnesia:transaction(F)
    end,
    ok.

%% @doc Retrieves the value associated with the key from the database
%% @spec(name(), name()) -> any() | undefined
retrieve(Tab, Key) ->
    case get(atomic) of
	undefined ->
	    F = fun() ->
			mnesia:read({Tab, Key}) 
		end,
	    {atomic, Res} = mnesia:transaction(F),
	    case Res of
		[] ->
		    undefined;
		[{_, Key, Val}] ->
		    Val
	    end;
	true ->
	    Log = get(transaction_log),
	    TV = lists:keysearch(Key, 1, Log),
	    case TV of
		false ->
		    F = fun() ->
				mnesia:read({Tab, Key})
			end,
		    {atomic, Res} = mnesia:transaction(F),
		    case Res of 
			[] ->
			    undefined;
			{_, Key, Val} ->
			    Val
		    end;
		{value, {Key, Val}} ->
		    Val
	    end
    end.

%% @doc Sets the process to be atomic
%% @spec start_atomic() -> ok
start_atomic() ->
    put(atomic, true),
    ok.

%% @doc Flushes the variables to the database
%% @spec(name()) -> ok
flush(Tab) ->
    Log = get(transaction_log),
    case Log of
	undefined ->
	    ok;
	[_] ->
	    F = fun() ->
			WF = fun(Item) ->
				     {Key, Val} = Item,
				     Rec = #osp_table{key = Key, val = Val},
				     mnesia:write(Tab, Rec, write)
			     end,
			lists:foreach(WF, Log)
		end,
	    {atomic, ok} = mnesia:transaction(F),
	    ok
    end.
