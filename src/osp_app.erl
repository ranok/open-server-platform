%% @copyright 2008 Jacob Torrey
%% @author Jacob Torrey <torreyji@clarkson.edu>
%% @doc OSP Application Calback module
-module(osp_app).
-behavior(application).

-export([start/2, stop/1]).

start(_Type, _StartArgs) ->
    supervisor_bridge:start_link({local, osp_supervisor}, osp_sup, []).

stop(_State) ->
    exit(whereis(osp_supervisor), shutdown).
