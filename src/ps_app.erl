%%%-------------------------------------------------------------------
%%% @author Ralf Th. Pietsch
%%% @copyright (C) 2022, Ralf Th. Pietsch
%%% @doc
%%%
%%% @end
%%% Created : 01. Feb 2022 09:20
%%%-------------------------------------------------------------------
-module(ps_app).
-author("Ralf Th. Pietsch").

-behaviour(application).

%% Application callbacks
-export([start/2, stop/1]).

%%%===================================================================
%%% Application callbacks
%%%===================================================================

start(_StartType, _StartArgs) ->
	ps_sup:start_link().


stop(_State) ->
	ok.

%%%===================================================================
%%% Internal functions
%%%===================================================================
