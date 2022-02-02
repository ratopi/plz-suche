# ps - PostleitzahlenSuche

Sucht die geographisch nähesten Postleitzahlen zu einer gegebenen Postleitzahlen.

Beispiel:

    ps_server:find_nearest(<<"01689">>, 10).

sucht die Postleitzahlenkreise in 10km Umkreis um 01689.

Oder suche mit der Angabe von Koordinaten:

    ps_server:find_nearest({51.1835327936398,13.5635350452693}, 10).

Beide Funktionen geben eine Liste von Tupeln der Form

    [ { Distance, {Plz, Ortsname, {Lat, Lon}} }, ... ]

zurück.
Die Liste ist nach Entfernung aufsteigend sortiert.
