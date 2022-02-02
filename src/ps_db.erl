%%%-------------------------------------------------------------------
%%% @author Ralf Th. Pietsch
%%% @copyright (C) 2022, Ralf Th. Pietsch
%%% @doc
%%% Handles lookup in the PLZ database
%%% @end
%%%-------------------------------------------------------------------
-module(ps_db).
-author("Ralf Th. Pietsch").

-record(ps_db, {entries = []}).

-export([new/1, get_by_plz/2, find_nearest/3]).

%%%===================================================================
%%% API
%%%===================================================================

new(ListOfMaps = [#{} | _]) ->
	Entries = lists:map(fun convert_entry/1, ListOfMaps),
	#ps_db{entries = Entries}.


% returns: {Plz, Name, {Lat, Lon}} or 'not_found'
get_by_plz(#ps_db{entries = Entries}, Plz) when is_binary(Plz) ->
	inner_get_by_plz(Plz, Entries).


find_nearest(DB = #ps_db{entries = Entries}, Plz, MaxDistance) ->
	case get_by_plz(DB, Plz) of
		not_found ->
			not_found;
		{ok, Entry} ->
			{ok, lists:sort(find_next(Entry, Entries, MaxDistance * MaxDistance, []))}
	end.

%%%===================================================================
%%% Internal functions
%%%===================================================================

convert_entry(Map) ->
	{
		maps:get(<<"plz">>, Map),
		maps:get(<<"Ort">>, Map),
		{
			erlang:binary_to_float(string:trim(maps:get(<<"lat">>, Map))),
			erlang:binary_to_float(string:trim(maps:get(<<"lon">>, Map)))
		}
	}.

%	#{<<"Ort">> => <<"Weinbergen">>,<<"lat">> => <<"51.2360672704631">>,
%		<<"loc_id">> => <<"12812">>,<<"lon">> => <<"10.590576176288">>,
%		<<"plz">> => <<"99998">>}]


inner_get_by_plz(_Plz, []) ->
	not_found;
inner_get_by_plz(Plz, [E = {Plz, _, _} | _]) ->
	{ok, E};
inner_get_by_plz(Plz, [_ | T]) ->
	inner_get_by_plz(Plz, T).



find_next(_Entry, [], _MaxDistance2, Result) ->
	Result;

find_next(Entry = {_, _, Loc1}, [E2 = {_, _, Loc2} | Entries], MaxDistance2, Result) ->
	Distance2 = ps_distance:distance2(Loc1, Loc2),
	case Distance2 > MaxDistance2 of
		true ->
			find_next(Entry, Entries, MaxDistance2, Result);
		false ->
			find_next(Entry, Entries, MaxDistance2, [{math:sqrt(Distance2), E2} | Result])
	end.
