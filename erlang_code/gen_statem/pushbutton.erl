-module(pushbutton).
-behaviour(gen_statem).

-export([start/0,push/0,get_count/0,stop/0]).
-export([terminate/3,code_change/4,init/1,callback_mode/0]).
-export([on/3,off/3]).

name() -> pushbutton_statem. % The registered server name

%% API.  This example uses a registered name name()
%% and does not link to the caller.
start() ->
	gen_statem:start({local,name()}, ?MODULE, [], []).
push() ->
	gen_statem:call(name(), push).
get_count() ->
	gen_statem:call(name(), get_count).
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
	erlang:send_after(1000, self(), test),
	erlang:start_timer(2000, self(), new_test),
	{ok,State,Data}.
callback_mode() -> state_functions.

%%% state callback(s)

off({call,From}, push, Data) ->
	%% Go to 'on', increment count and reply
	%% that the resulting status is 'on'
	{next_state,on,Data+1,[{reply,From,on}]};
off(EventType, EventContent, Data) ->
	handle_event(EventType, EventContent, Data).

on({call,From}, push, Data) ->
	%% Go to 'off' and reply that the resulting status is 'off'
	{next_state,off,Data,[{reply,From,off}]};
on(EventType, EventContent, Data) ->
	handle_event(EventType, EventContent, Data).

%% Handle events common to all states
handle_event({call,From}, get_count, Data) ->
	%% Reply with the current count
	{keep_state,Data,[{reply,From,Data}]};
handle_event(Type, Content, Data) ->
	io:format("Type:~p, Content:~p~n", [Type, Content]),
	%% for the init function test msg, here will print:
	%%  Type:info, Content:test
	%% for the init function new_test msg, here will print:
	%% Type:info, Content:{timeout,#Ref<0.1082072278.3859021828.235514>,new_test}
	%% Ignore all other events
	{keep_state,Data}.
