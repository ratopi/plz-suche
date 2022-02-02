# ps - PostleitzahlenSuche

Sucht die geographisch nähesten Postleitzahlen zu einer gegebenen Postleitzahlen.

Beispiel:

    ps_server:find_nearest(<<"01689">>, 10).

sucht die Postleitzahlenkreise in 10km Umkreis um 01689.
Gibt eine Liste von Tupeln der Form

    { Distance, {Plz, Ortsname, {Lat, Lon}} }

zurück.
