%% @copyright 2008
%% @author Jacob Torrey <torreyji@clarkson.edu>
%% @author Cyrus Katrak <katrakc@gmail.com>
%% @doc Provides an interface to the Mnesia metadata store
-module(erlfs_db).
-author('Jacob I. Torrey <torreyji@clarkson.edu>').
-author('Cyrus R. Katrak <katrakc@gmail.com>').

-export([ls/1,start/0,stop/0,format/0,mkdir/1]).

% Our database records
%-record(dirinfo, {did, name, owner, group, permissions, dids, fids, timestamp}).
-record(fileinfo, {fid,     % Primary Key (16 byte MD5 Digest of trimmed path)
                   name,    % File/Dir Name
                   is_dir = false,  % True if this is a directory
                   owner = "none",
                   group = "none",
                   permissions = 511,
                   size = 0,
                   timestamp,
                   cfids = [] % A list of cids for a file or,
                              % a dictionary of (key:name, value: fid)
                              % for a directory.
                  }).
% Maybe come back and add a "fullpath" attribute and place a secondary index on 
% it to avoid dir tree traversal for every file lookup.
-record(chunkinfo, {cid, nodes}).
% Revist this, ideally we want a compound key {fid, cid}
-record(nodeinfo, {node, totalspace, usedspace, upsince}).

% All the tables
-define(TABLES, [fileinfo, chunkinfo, nodeinfo]).

path_to_path_elements(Path) ->
  string:tokens(Path, "/"). %Todo, strip/trim check for illegal chars

path_elements_to_path([], Path) ->
  Path ++ "/";
path_elements_to_path([Last], Path) ->
  Path ++ "/" ++ Last;
path_elements_to_path([Head|Rest], Path) ->
  path_elements_to_path(Rest, Path ++ "/" ++ Head).

path_elements_to_path(PathElements) ->
  path_elements_to_path(PathElements, "").

%Takes a string path and make it well formed
path_sanitize(Path) ->
  path_elements_to_path(path_to_path_elements(Path)).

%Computes the md5 hash of the sanitized path
path_to_fid(Path) ->
  erlang:md5(path_sanitize(Path)).

root_fid() -> path_to_fid("/").
read_root_record() ->
  [Record] = mnesia:read({fileinfo, root_fid()}),
  Record.

write_root_dir() ->
  mnesia:write(#fileinfo{fid = root_fid(), 
                         is_dir = true, 
                         size = 0,
                         timestamp = timestamp_now(),
                         cfids = dict:new()}).

timestamp_now() ->
  {M, S, _} = erlang:now(),
  M*1000000 + S.


%% @doc Formats the Mnesia database and creates the tables
%% @spec format() -> ok | {error, Reason}
format() ->
  mnesia:stop(),
  mnesia:create_schema([node()]),
  mnesia:start(),
  mnesia:create_table(fileinfo, [{attributes, record_info(fields, fileinfo)},
		                             {disc_copies, [node()]}, {type, set}]),
  mnesia:create_table(chunkinfo, [{attributes, record_info(fields,chunkinfo)},
  		                            {disc_copies, [node()]}, {type, set}]),
  mnesia:create_table(nodeinfo, [{attributes, record_info(fields, nodeinfo)},
		                             {disc_copies, [node()]}, {type, set}]),
  % Insert the root directory (fid = 0)
  % mnesia:transaction(write_root_dir()),
  %start(),
  mnesia:wait_for_tables(?TABLES, 2000),
  {atomic, _} = mnesia:transaction(fun write_root_dir/0),
  mnesia:stop(),
  success.


%% @doc Starts the Mnesia metadata store
%% @spec start() -> ok | {error, Reason}
start() ->
    mnesia:start(),
    mnesia:wait_for_tables(?TABLES, 2000).

%% @doc Stops the Mnesia metadata store
%% @spec stop() -> ok | {error, Reason}
stop() ->
    mnesia:stop().

%% @doc Adds Node to the node info metadata store
%% @spec add_nodeinfo(atom(), int(), int()) -> {atomic, Result} | 
%% {aborted, Reason}
%% @end
%add_nodeinfo(Node, Totalspace, Usedspace) ->
%    Time = erlang:localtime(),
%    Record = #nodeinfo{node = Node, totalspace = Totalspace,
%                       usedspace = Usedspace, upsince = Time},
%    F = fun() ->
%		mnesia:write(nodeinfo, Record, write)
%	end,
%    mnesia:transaction(F).


%% @doc Returns a file/dirs record given a starting record and a list of path
%% elements.
%% @end
%% @spec get_fid(?, [atom()] -> ?)
get_fid_record_slow(CurrentFidR, []) -> CurrentFidR;
get_fid_record_slow(CurrentFidR, [First|Rest]) ->
  NextFid = dict:lookup(First, CurrentFidR#fileinfo.cfids),
  [NextRecord] = mnesia:read({fileinfo, NextFid}),
  get_fid_record_slow(NextRecord, Rest).

%% @doc Returns a filename's record. Not Transactional.
%% @spec get_fid(atom(), atom()) -> int()
get_fid_record_slow(Path) ->
  PathElements = path_to_path_elements(Path),
  get_fid_record_slow(read_root_record(), PathElements).

get_fid_record_fast(Path) ->
  Fid=path_to_fid(path_sanitize(Path)),
  {_,Name}=path_split_parent(Path),
  [Record] = mnesia:read({fileinfo, Fid}),
  case Record#fileinfo.name of
    Name ->
      Record
  end.


is_file(Record) ->
  not Record#fileinfo.is_dir.

%% @doc Lists all files in a directory
%% @spec ls(atom()) -> list()
ls(Path) ->
  F = fun() -> get_fid_record_slow(Path) end,
  {atomic, Record} = mnesia:transaction(F),
  case is_file(Record) of
    false -> 
      List = dict:to_list(Record#fileinfo.cfids),
      [X || {X, _} <- List];
    true ->
      throw(fixme1)
  end.


gen_fid(Path) ->
  gen_fid_internal(path_sanitize(Path)).

gen_fid_internal(Path) ->
  Fid = path_to_fid(Path),
  case mnesia:read({fileinfo, Fid}) of
    [] ->
      Fid;
    [_] ->
      gen_fid_internal(Path++"/")
  end.

mkdir(ParentPath, Name) ->
  ParentDir = get_fid_record_slow(ParentPath),
  case is_file(ParentDir) of
    false ->
      case dict:is_key(Name, ParentDir#fileinfo.cfids) of
        false ->
          NewFid = gen_fid(ParentPath ++ "/" ++ Name),
          ParentDir#fileinfo{cfids=dict:store(Name,
                                              NewFid,
                                              ParentDir#fileinfo.cfids),
                             timestamp = timestamp_now()},
          
		      mnesia:write(nodeinfo, ParentDir, write),
          mnesia:write(#fileinfo{fid = NewFid,
                                 name = Name,
                                 is_dir = true, 
                                 size = 0,
                                 timestamp = timestamp_now(),
                                 cfids = dict:new()}),

          success
      end
  end.

path_split_parent(Path) ->
  PathElems = path_to_path_elements(Path),
  %This is the name of the directory we are creating
  ChildName = lists:last(PathElems),
  %Remove the last element of the list to get the parent directory's path
  ParentPath = path_elements_to_path(lists:sublist(PathElems,
                                                   1,
                                                   length(PathElems)-1)),
  {ParentPath, ChildName}.


mkdir(Path) ->
  {ParentPath, NewDir} = path_split_parent(Path),
  F = fun() -> mkdir(ParentPath, NewDir) end,
  {atomic, _} = mnesia:transaction(F).
