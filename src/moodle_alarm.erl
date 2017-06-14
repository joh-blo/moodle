%%%-------------------------------------------------------------------
%%% @author Johan <>
%%% @copyright (C) 2017, Johan
%%% @doc
%%%Some additional ideas for alarming includes:
%%% - If a new alarm occurs, while there already exists some other alarm,
%%%   short intense alarm for some time, then mix steady colour 
%%% - If data source is interrupted, use steady colour but do not mix.
%%%
%%% @end
%%% Created : 28 Apr 2017 by Johan <>
%%%-------------------------------------------------------------------
-module(moodle_alarm).

-behaviour(gen_server).

%% API
-export([start_link/0,
	 start/1,stop/1,
	 status/0
	]).

%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
	 terminate/2, code_change/3]).

-define(SERVER, ?MODULE).

-record(state,{
	  alarm_db::list(),     % Known alarms
	  alarming::boolean(),  % Set to true if an alarm is running
	  alarming_type::atom() % Type of alarm running, if any
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


start(Type) ->
    gen_server:cast(?MODULE,{start_alarm,Type}).

stop(Type) ->
    gen_server:cast(?MODULE,{stop_alarm,Type}).

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
    AlarmDb=moodle_lib:get_cfg(moodle_alarms),
    {ok, #state{alarm_db=AlarmDb,
		alarming=false}}.

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
handle_call(status, _From, State=#state{alarming=Alarming,
					alarming_type=AlarmingType}) ->
    Resp=if
	     Alarming ->
		 [{alarming,Alarming},{alarming_type,AlarmingType}];
	     true ->
		 [{alarming,Alarming}]
	 end,
    {reply, Resp, State}.


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
handle_cast({start_alarm,AlarmingType},State=#state{alarm_db=AlarmDb,
						    alarming_type=AlarmingType0,
						    alarming=Alarming}) ->
    if
	Alarming==false ->
	    emd_log:debug("Raising new alarm ~p",[AlarmingType]),
	    run_alarm(AlarmingType,AlarmDb),
	    gen_server:cast(?MODULE,alarming);
	true ->
	    emd_log:debug("Raising alarm ~p, but was already alarming ~p",
			  [AlarmingType,AlarmingType0]),
	    ok
    end,
    {noreply, State#state{alarming=true,
			  alarming_type=AlarmingType}};
handle_cast({stop_alarm,_AlarmingType}, State) ->
    {noreply, State#state{alarming=false,
			  alarming_type=undefined}};
handle_cast(alarming, State=#state{alarm_db=AlarmDb,
				   alarming=Alarming,
				   alarming_type=AlarmingType}) ->
    if
	Alarming ->
	    run_alarm(AlarmingType,AlarmDb),
	    % timer:sleep(3000),
	    gen_server:cast(?MODULE,alarming);
	true ->
	    ok
    end,
    {noreply, State}.

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

%% Executes the actual alarm.
%% Add support for different AlaramingTypes, for example:
%% - Intense alarming first minute, then keep lighten with steady (lower)
%%   light until sensor level i low
%%   Steady light could havea single colour to be able to easily identify
%%   alarm type.
run_alarm(AlarmingType,AlarmDb) ->
    case lists:keysearch(AlarmingType,1,AlarmDb) of
	{value,{_,external,ExtScript}} ->
	    Cmd=filename:join(code:priv_dir(moodle),ExtScript),
	    os:cmd("sudo python "++Cmd);
	Other ->
	    emd_log:error("Unknown alarm ~p, got ~p",[AlarmingType,Other])
    end.
