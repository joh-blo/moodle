%%%-------------------------------------------------------------------
%%% @author Johan <>
%%% @copyright (C) 2017, Johan
%%% @doc
%%%
%%% @end
%%% Created : 28 Apr 2017 by Johan <>
%%%-------------------------------------------------------------------
-module(moodle_manager).

-behaviour(gen_server).

%% API
-export([start_link/0,
	 start_alarm/0,stop_alarm/0,
	 status/0]).

%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
	 terminate/2, code_change/3]).

-define(SERVER, ?MODULE).

-record(state,{
	  alarm_status % (bool) Set to true if alar
	 }).

%%%===================================================================
%%% API
%%%===================================================================

%%--------------------------------------------------------------------
%% @doc
%% Starts the server
%%
%% @spec start_link() -> {ok, Pid} | ignore | {error, Error}
%% @end
%%--------------------------------------------------------------------
start_link() ->
    gen_server:start_link({local, ?SERVER}, ?MODULE, [], []).


start_alarm() ->
    gen_server:cast(?MODULE,start_alarm).

stop_alarm() ->
    gen_server:cast(?MODULE,stop_alarm).

status() ->
    gen_server:call(?MODULE,status).

%%%===================================================================
%%% gen_server callbacks
%%%===================================================================

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Initializes the server
%%
%% @spec init(Args) -> {ok, State} |
%%                     {ok, State, Timeout} |
%%                     ignore |
%%                     {stop, Reason}
%% @end
%%--------------------------------------------------------------------
init([]) ->
    process_flag(trap_exit, true),
    {ok, #state{alarm_status=false}}.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Handling call messages
%%
%% @spec handle_call(Request, From, State) ->
%%                                   {reply, Reply, State} |
%%                                   {reply, Reply, State, Timeout} |
%%                                   {noreply, State} |
%%                                   {noreply, State, Timeout} |
%%                                   {stop, Reason, Reply, State} |
%%                                   {stop, Reason, State}
%% @end
%%--------------------------------------------------------------------
handle_call(fetch_data, _From, State) ->
    %% Fetch sensor data from the server
    %% Prior to get this working, we also need to register this aplication
    %% in the server.
    %% This includes:
    %% - Give this aplication a role
    %% - Give this aplication just enough capabillities to fulfill its task
    Reply = ok,
    {reply, Reply, State};
handle_call(status, _From, State) ->
    %% Read the current status
    AlarmStatus=moodle_alarm:status(),
    Temp=read_temp(),
    Reply=[AlarmStatus,Temp],
    {reply, Reply, State}.


%%--------------------------------------------------------------------
%% @private
%% @doc
%% Handling cast messages
%%
%% @spec handle_cast(Msg, State) -> {noreply, State} |
%%                                  {noreply, State, Timeout} |
%%                                  {stop, Reason, State}
%% @end
%%--------------------------------------------------------------------
handle_cast(start_alarm, State) ->
    %% Manually force start of alarm
    moodle_alarm:start(),
    {noreply, State#state{alarm_status=true}};
handle_cast(stop_alarm, State) ->
    %% Manually force stop of alarm
    moodle_alarm:stop(),
    {noreply, State#state{alarm_status=false}}.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Handling all non call/cast messages
%%
%% @spec handle_info(Info, State) -> {noreply, State} |
%%                                   {noreply, State, Timeout} |
%%                                   {stop, Reason, State}
%% @end
%%--------------------------------------------------------------------
handle_info(_Info, State) ->
    {noreply, State}.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% This function is called by a gen_server when it is about to
%% terminate. It should be the opposite of Module:init/1 and do any
%% necessary cleaning up. When it returns, the gen_server terminates
%% with Reason. The return value is ignored.
%%
%% @spec terminate(Reason, State) -> void()
%% @end
%%--------------------------------------------------------------------
terminate(_Reason, _State) ->
    ok.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Convert process state when code is changed
%%
%% @spec code_change(OldVsn, State, Extra) -> {ok, NewState}
%% @end
%%--------------------------------------------------------------------
code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

%%%===================================================================
%%% Internal functions
%%%===================================================================

%% Assumes we are running this on a Raspberry Pi with Rasbian installed ...
read_temp() ->
    case os:cmd("/opt/vc/bin/vcgencmd measure_temp") of
	"temp="++TempStr ->
io:format("TempStr=~p~n",[TempStr]),
temp_to_float(TempStr,[]);
	Other ->
io:format("Other=~p~n",[Other]),
	    undefined
    end.

temp_to_float([],Out) ->
Str=lists:reverse(Out),
io:format("JB-1 Str=~p~n",[Str]),

list_to_float(Str);
temp_to_float([H|Rest],Out) when $0=<H,H=<$9 ->
    temp_to_float(Rest,[H|Out]);
temp_to_float([$.|Rest],Out) ->
    temp_to_float(Rest,[$.|Out]);
temp_to_float(_,Out) ->
Str=lists:reverse(Out),
io:format("JB-2 Str=~p~n",[Str]),
    list_to_float(Str).

    
