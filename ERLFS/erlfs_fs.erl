%% @copyright 2008 Jacob Torrey
%% @author Jacob Torrey <torreyji@clarkson.edu>
%% @doc ERLFS file interaction library
-module(erlfs_fs).
-author('Jacob I. Torrey <torreyji@clarkson.edu>').

-export([delete/1, replicate/1, read_file/1, write_file/2,
         get_filesystem_size/0, get_max_size/0, get_dir/0, get_new_fid/0]).
-export([create/5, file_size/1]).

-define(MAX_FID, 1024).

%% @doc Deletes the file FID
%% @spec delete(int) -> ok | {error, Reason}
delete(Fid) ->
    Dir = get_dir(),
    file:delete(erlang:atom_to_list(Dir) ++ "/" ++ erlang:integer_to_list(Fid)).

%% @doc Replicates the file FID to the local disk
%% @spec replicate(int()) -> {atomic, ok} | {error, Reason}
replicate(Fid) ->
    [Node|_] = erlfs_db:get_nodes(Fid),
    Bin = rpc:call(Node, erlfs_fs, read_file, [Fid]),
    write_file(Fid, Bin),
    erlfs_db:add_replication_row(node(), Fid, erlang:md5(Bin)).

%% @doc Returns the fsdir
%% @spec get_dir() -> atom()
get_dir() ->
    erlfs_conf:get_conf(fsdir).

%% @doc returns a unique FID
%% @spec get_new_fid() -> int()
get_new_fid() ->
    Try = random:uniform(?MAX_FID),
    case erlfs_db:file_exists(Try) of
	true ->
	    get_new_fid();
	false ->
	    Try
    end.

%% @doc Reads in an entire file, outputs as a binary
%% @spec read_file(int()) -> binary()
read_file(Fid) ->
    Dir = get_dir(),
    {ok, Bin} = file:read_file(erlang:atom_to_list(Dir) ++ "/" ++
                erlang:integer_to_list(Fid)),
    Bin.

%% @doc Returns the filesize of Fid
%% @spec file_size(int()) -> int()
file_size(Fid) ->
    Dir = get_dir(),
    filelib:file_size(erlang:atom_to_list(Dir) ++ "/" ++
                                                  erlang:integer_to_list(Fid)).

%% @doc Writes a binary Bin to file Fid
%% @spec write_file(int(), binary()) -> ok | {error, Reason}
write_file(Fid, Bin) ->
    io:format("Writing ~p\n", [erlang:binary_to_list(Bin)]),
    Dir = get_dir(),
    file:write_file(erlang:atom_to_list(Dir) ++ "/" ++
      erlang:integer_to_list(Fid), Bin).

%% @doc Get the max allowed size for this node's filesystem
%% @spec get_max_size() -> int()
get_max_size() ->
    erlfs_conf:get_conf(fssize).

%% @doc Returns the used size of the filesystem
%% @spec get_filesystem_size() -> int()
get_filesystem_size() ->
    F = fun(F, Acc) ->
		Acc + filelib:file_size(F)
	end,
    filelib:fold_files(get_dir(), ".*", true, F, 0).

%% @todo make it auto replicate
%% @doc Creates a file
%% @spec create(atom(), int(), atom(), atom(), binary()) -> {atomic, ok} | {error, Reason}
create(FilenameDir, Mode, Owner, Group, Data) ->
    Fid = get_new_fid(),
    case string:words(FilenameDir) of
	1 ->
	    File = FilenameDir,
	    Dir = "";
	_ ->
	    File = string:sub_word(FilenameDir, string:words(FilenameDir, $/), $/),
	    Dir = string:substr(FilenameDir, 1, (string:len(FilenameDir) -
            string:len(File)) - 1)
    end,
    MD5 = erlang:md5(Data),
    write_file(Fid, Data),
    FileSize = file_size(Fid),
    erlfs_db:create(File, Dir, Fid, Owner, Group, Mode, FileSize, MD5),
    erlfs_db:add_replication_row(node(), Fid, MD5).
