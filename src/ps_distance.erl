%%%-------------------------------------------------------------------
%%% @author Ralf Th. Pietsch
%%% @copyright (C) 2022, Ralf Th. Pietsch
%%% @doc
%%% Tools for calculation distance between locations (in germany)
%%% @end
%%% Created : 01. Feb 2022 10:23
%%%-------------------------------------------------------------------
-module(ps_distance).
-author("Ralf Th. Pietsch").

%% API
-export([distance/2, distance2/2]).

%%%===================================================================
%%% API
%%%===================================================================

%% Calculates distance in km (for germany)
distance(A = {_Lat1, _Lon1}, B = {_Lat2, _Lon2}) ->
	math:sqrt(distance(A, B)).


%% Calculates distance and returns there squared value
distance2({Lat1, Lon1}, {Lat2, Lon2}) ->
	Lat = (Lat1 + Lat2) / 2 * 0.01745,
	Dx = 111.3 * math:cos(Lat) * (Lon1 - Lon2),
	Dy = 111.3 * (Lat1 - Lat2),
	Dx * Dx + Dy * Dy.


%%%===================================================================
%%% Internal functions
%%%===================================================================

%% 1° ~ 111,3km ... in germany
%%
%%
%% distance = sqrt(dx * dx + dy * dy)
%%
%% mit distance: Entfernung in km
%% dx = 111.3 * cos(lat) * (lon1 - lon2)
%% lat = (lat1 + lat2) / 2 * 0.01745
%% dy = 111.3 * (lat1 - lat2)
%% lat1, lat2, lon1, lon2: Breite, Länge in Grad
%%
%% https://www.kompf.de/gps/distcalc.html
