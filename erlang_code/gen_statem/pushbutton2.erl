-module(pushbutton2).
-behaviour(gen_statem).

-export([
		 name/0,
		 start/0,
		 push/0,
		 get_count/0,
		 stop/0,
		 cast_msg/0]).
-export([
		 terminate/3,
		 code_change/4,
		 init/1,
		 callback_mode/0,
		 handle_event/4
		]).
%%-export([on/3,off/3]).

name() -> pushbutton_statem2. % The registered server name

%% API.  This example uses a registered name name()
%% and does not link to the caller.
start() ->
	gen_statem:start({local,name()}, ?MODULE, [], []).

push() ->
	gen_statem:call(name(), push).

get_count() ->
	gen_statem:call(name(), get_count).

cast_msg() ->
	gen_statem:cast(name(), cast_msg).

stop() ->
	gen_statem:stop(name()).

%% Mandatory callback functions
terminate(_Reason, _State, _Data) ->
	void.
code_change(_Vsn, State, Data, _Extra) ->
	{ok,State,Data}.
init([]) ->
	%% Set the initial state + data.  Data is used only as a counter.
	State = off, Data = 0,
	{ok, State, Data}.

callback_mode() -> handle_event_function.

%%% state callback(s)

handle_event({call,From}, push, off, Data) ->
	%% Go to 'on', increment count and reply
	%% that the resulting status is 'on'
	{next_state, on, Data+1, [{reply,From,on}]};
handle_event({call,From}, push, on, Data) ->
	%% Go to 'off' and reply that the resulting status is 'off'
	{next_state, off, Data, [{reply,From,off}]};
%%
%% Event handling common to all states
handle_event({call,From}, get_count, State, Data) ->
	%% Reply with the current count
	{next_state, State, Data, [{reply,From,Data}]};

handle_event(cast, CastMsg, State, Data) ->
	io:format("CastMsg:~p~n", [CastMsg]),
	{next_state, State, Data};

handle_event(info, InfoMsg, State, Data) ->
	io:format("InfoMsg:~p~n", [InfoMsg]),
	{next_state, State, Data};

handle_event(Other, OtherMsg, State, Data) ->
	io:format("Other:~p, OtherMsg:~p~n", [Other, OtherMsg]),
	%% Ignore all other events
	{next_state, State, Data}.
