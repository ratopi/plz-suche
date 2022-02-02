%%%-------------------------------------------------------------------
%%% @author Ralf Th. Pietsch
%%% @copyright (C) 2022, Ralf Th. Pietsch
%%% @doc
%%% Holds the ps_db-PLZ-database
%%% @end
%%%-------------------------------------------------------------------
-module(ps_server).
-author("Ralf Th. Pietsch").

-behaviour(gen_server).

-export([get_by_plz/1, find_nearest/2]).

-export([start_link/0]).

-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2, code_change/3]).

-define(SERVER, ?MODULE).
-define(RELOAD_PERIOD, 31 * 60 * 60 * 1000). % reload any 31 hours

-record(ps_db_server_state, {url, db = undefined}).

%%%===================================================================
%%% API
%%%===================================================================

get_by_plz(Plz) -> gen_server:call(?SERVER, {get_by_plz, Plz}).

find_nearest(Plz, MaxDistance) -> gen_server:call(?SERVER, {find_nearest, Plz, MaxDistance}).

%%%===================================================================
%%% Spawning
%%%===================================================================

start_link() ->
	gen_server:start_link({local, ?SERVER}, ?MODULE, [], []).

%%%===================================================================
%%% gen_server implementation
%%%===================================================================

init([]) ->
	{ok, Url} = application:get_env(plz_url),
	self() ! load,
	timer:send_interval(?RELOAD_PERIOD, load),
	{ok, #ps_db_server_state{url = Url}}.


handle_call({get_by_plz, Plz}, _From, State = #ps_db_server_state{db = DB}) ->
	{reply, ps_db:get_by_plz(DB, Plz), State};

handle_call({find_nearest, Plz, MaxDistance}, _From, State = #ps_db_server_state{db = DB}) ->
	{reply, ps_db:find_nearest(DB, Plz, MaxDistance), State};

handle_call(_Request, _From, State = #ps_db_server_state{}) ->
	{reply, {error, {unknown_request, _Request}}, State}.


handle_cast(_Request, State = #ps_db_server_state{}) ->
	{noreply, State}.


handle_info(load, State = #ps_db_server_state{url = Url}) ->
	Start = erlang:system_time(millisecond),
	{ok, {Status, _Headers, Body}} = httpc:request(get, {Url, []}, [], [{body_format, binary}]),
	case Status of
		{"HTTP/1.1", 200, "OK"} ->
			Entries = jsx:decode(Body),
			DB = ps_db:new(Entries),
			Dur = erlang:system_time(millisecond) - Start,
			io:fwrite("~p: loaded ~p entries in ~p ms~n", [?MODULE, length(Entries), Dur]),
			{noreply, State#ps_db_server_state{db = DB}};
		_ ->
			timer:send_after(5000, load),
			{noreply, State}
	end;

handle_info(_Info, State = #ps_db_server_state{}) ->
	{noreply, State}.


terminate(_Reason, _State = #ps_db_server_state{}) ->
	ok.


code_change(_OldVsn, State = #ps_db_server_state{}, _Extra) ->
	{ok, State}.

%%%===================================================================
%%% Internal functions
%%%===================================================================
