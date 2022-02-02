%%%-------------------------------------------------------------------
%%% @author Ralf Th. Pietsch
%%% @copyright (C) 2022, Ralf Th. Pietsch
%%% @doc
%%% @end
%%%-------------------------------------------------------------------
-module(ps_sup).
-author("Ralf Th. Pietsch").

-behaviour(supervisor).

-export([start_link/0, init/1]).

%%%===================================================================
%%% Spawning
%%%===================================================================

start_link() ->
	supervisor:start_link({local, ?MODULE}, ?MODULE, []).

%%%===================================================================
%%% supervisor implementation
%%%===================================================================

init([]) ->
	Config =
		#{
			strategy => one_for_one,
			intensity => 5,
			period => 30
		},

	Children =
		[
			#{
				id => ps_server,
				start => {ps_server, start_link, []},
				restart => permanent,
				shutdown => 2000,
				type => worker,
				modules => [ps_server]
			}
		],

	{ok, {Config, Children}}.

%%%===================================================================
%%% Internal functions
%%%===================================================================
