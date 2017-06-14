%%% @author Johan <>
%%% @copyright (C) 2017, Johan
%%% @doc
%%%
%%% @end
%%% Created : 28 Apr 2017 by Johan <>
%% erl -pa moodle/ebin meadow/ebin -run moodle

-module(moodle).

%% API
-export([start/0,stop/0,restart/0,'force-reload'/0,status/0,
	 map_record/2]).

-include("moodle_internal.hrl").

%% @spec start() -> ok
%% @doc
%%  Start daisy and all applications it depends on, as given by configuration.
%% @end
start() ->
    application:start(?MODULE).


%% @spec stop() -> ok
%% @doc
%%  Stop all all running applications on this node except kernel and stdlib.
%% @end
stop() ->
    halt().

restart() ->
    application:stop(?MODULE),
    application:start(?MODULE).


'force-reload'() ->
    moodle_manager:load_cfg().

status() ->
    moodle_manager:status(all).

%% --- Specific configuration records for moodle
map_record(RecordType,Fields) ->
    case RecordType of
	trigger ->
	    DecFields=record_info(fields,trigger),
	    Defaults=[],
	    FieldList=emd_cfg:create_record_list(DecFields,Fields,Defaults,[]),
	    list_to_tuple([RecordType|FieldList])
    end.
