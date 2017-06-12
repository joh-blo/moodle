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
	 load_cfg/0,
	 status/0,status/1]).

%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
	 terminate/2, code_change/3]).

-define(SERVER, ?MODULE).

-record(state,{
	  data_source::tuple(), % RPC arguments, to get the sensor data from
	  data_source_failing::integer(), % Allowed number of failing fetches
	  fetch_interval::integer(), % Time, in seconds, between a fetch of data
	  trigger_levels::list(),  % Sensor level(s) to trigger alarm(s)
	  alarm_status::boolean(),   % Set to true if alarm is ringing
	  failing::integer()
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
    gen_server:call(?MODULE,{status,all}).

status(Key) ->
    gen_server:call(?MODULE,{status,Key}).

load_cfg() ->
    gen_server:call(?MODULE,load_cfg).


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
    DsNMFA=moodle_lib:get_cfg(moodle_external,undefined),
    DsMaxFail=moodle_lib:get_cfg(moodle_external_failing,10),
    FetchInterval=moodle_lib:get_cfg(moodle_fetch_interval),
    TriggerLevels=moodle_lib:get_cfg(moodle_trigger_levels),
    start_timer(FetchInterval),
    {ok, #state{data_source=DsNMFA,
		data_source_failing=DsMaxFail,
		fetch_interval=FetchInterval,
		trigger_levels=TriggerLevels,
		alarm_status=false,
		failing=0}}.

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
handle_call({status,Key}, _From, State) ->
    %% Read the current status    
    Reply=
	if Key==all -> [{alarm,moodle_alarm:status()},{temp,read_temp()}];
	   Key==alarm -> moodle_alarm:status();
	   Key==temp -> read_temp()
	end,
    {reply, Reply, State};
handle_call(load_cfg, _From, State) ->
    %% Read the current status    
    DsNMFA=moodle_lib:get_cfg(moodle_external,undefined),
    DsMaxFail=moodle_lib:get_cfg(moodle_external_failing,10),
    FetchInterval=moodle_lib:get_cfg(moodle_fetch_interval),
    TriggerLevels=moodle_lib:get_cfg(moodle_trigger_levels),
    NewState=State#state{data_source=DsNMFA,
			 data_source_failing=DsMaxFail,
			 fetch_interval=FetchInterval,
			 trigger_levels=TriggerLevels},
    {reply, ok, NewState}.


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
handle_cast(fetch_data, State=#state{fetch_interval=FetchInterval,
				     trigger_levels=TriggerLevels,
				     data_source={N,M,F,A},
				     failing=Failing}) ->
    %% Fetch sensor data from the server
    %% Prior to get this fully working, we should also need to register this
    %% application in the server.
    %% This includes:
    %% - Give this aplication a role
    %% - Give this aplication just enough capabillities to fulfill its task
    {Data,GotData}=
	try case rpc:call(N,M,F,A) of
		V when is_integer(V) -> {V,1};
	        _ -> {0,0}
	    end
	catch
	    _:Reason ->
		emd_log:error("Could not reach ~p:~p at ~p, got ~p",
			      [M,F,N,Reason]),
		{0,0}
	end,
    emd_log:debug("Data=~p TriggerLevel=~p",[Data,TriggerLevels]),
    NewFailing=
	if
	    GotData==1 ->
		0;
	    true ->
		case Failing+GotData of
		    NF when NF>=10 ->
			%% Alert - no contact
			NF;
		    NF ->
			NF
		end
	end,
    trigger_alarms(Data,TriggerLevels),
    start_timer(FetchInterval),
    {noreply, State=#state{failing=NewFailing}};
handle_cast(start_alarm, State) ->
    %% Manually force start of alarm
    moodle_alarm:start(high_sensor_level),
    {noreply, State#state{alarm_status=true}};
handle_cast(stop_alarm, State) ->
    %% Manually force stop of alarm
    moodle_alarm:stop(high_sensor_level),
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

trigger_alarms(Data,[{co2,TriggerLevel,AlarmingType}]) ->
    if
	Data>TriggerLevel ->
	    moodle_alarm:start(AlarmingType);
	true ->
	    moodle_alarm:stop(AlarmingType)
    end.




start_timer(FetchInterval) ->
    erlang:send_after(FetchInterval*1000,self(),{'$gen_cast',fetch_data},[]).




%% Assumes we are running this on a Raspberry Pi with Rasbian installed ...
read_temp() ->
    case os:cmd("/opt/vc/bin/vcgencmd measure_temp") of
	"temp="++TempStr ->
	    io:format("TempStr=~p~n",[TempStr]),
	    temp_to_float(TempStr,[]);
	Other ->
	    emd_log:warning("Unexpected temperature ~p",[Other]),
	    undefined
    end.

temp_to_float([],Out) ->
    Str=lists:reverse(Out),
    list_to_float(Str);
temp_to_float([H|Rest],Out) when $0=<H,H=<$9 ->
    temp_to_float(Rest,[H|Out]);
temp_to_float([$.|Rest],Out) ->
    temp_to_float(Rest,[$.|Out]);
temp_to_float(_,Out) ->
    Str=lists:reverse(Out),
    list_to_float(Str).

    
