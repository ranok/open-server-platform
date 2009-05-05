%% @author Jacob Torrey <torreyji@clarkson.edu>
%% @copyright 2009 Jacob Torrey <torreyji@clarkson.edu>
%% @doc Provides the osp_servlet behavior
-module(osp_servlet).

-export([behaviour_info/1]).

%% @doc Provides the information about the behavior
%% @spec behaviour_info(atom()) -> undefined | list()
behaviour_info(callbacks) ->    
    [{init, 0}, {cleanup, 0}, {server, 1}, {proto, 0}, {start_mnesia, 0}];
behaviour_info(_) -> 
    undefined.

