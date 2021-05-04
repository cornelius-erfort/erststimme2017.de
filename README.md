# erststimme2017.de
Vorhersageprojekt für die Erstimmenergebnisse der Bundestagswahl 2017

Hier tauschen wir Daten zu Wahlergebnissen und Kandidaten, sowie zu anderen Bereichen unseres Projekts aus.

## Direktmandate-Vorhersage
### Bundestagswahl 2017

Für die kommende Bundestagswahl am 24. September 2017 liefert unser Modell vom 19. September 2017 die folgende Vorhersage für den Erstimmenanteil der einzelnen Parteien auf Wahlkreisebene:

![Preview render](https://github.com/cornelius-erfort/erststimme2017.de/blob/master/map_germany.png)

<img src="https://github.com/cornelius-erfort/erststimme2017.de/blob/master/seatsummary.png" width="50%">

Wahlkreis-Shapefile: © Der Bundeswahlleiter, Statistisches Bundesamt, Wiesbaden 2016,
Wahlkreiskarte für die Wahl zum 19. Deutschen Bundestag
Grundlage der Geoinformationen © Geobasis-DE / BKG (2016)

Unser Modell bezieht sich hierbei auf die Prognose des Zweistimmenanteils der Parteien auf Bundesebene von [zweitstimme.org](http://zweitstimme.org/20170918_2_blog.html) vom 19. September 2017 (CDU/CSU 36,3%, SPD 23,0%, Die Linke 9,6%, Bündnis 90/Die Grünen 7,8%, FDP 9,1%).

[Hier](http://galeriasdelsa.com/erststimme/#bundeslaender) können Sie sich die vorhergesagten Erstimmenergebnisse für die kommende Bundestagswahl (sowie die Ergebnisse der vergangenen Wahl) für alle 299 Wahlkreise anzeigen lassen. Diese Werte sind jedoch keineswegs sicher. Die dargestellten Konfidenzintervalle geben für jeden Kandidaten an in welchem Bereich wir das tatsächliche Erstimmenergebnis mit 83% Wahrscheinlichkeit vermuten.


# Das Modell
Unser Modell nutzt Informationen zu den Erst- und Zweitstimmenanteile der Parteien für die letzten vier Bundestagswahlen 2002 - 2013. Hier zählt das Modell zu den Vertretern der sogenannten „Uniform Swing“ Modelle (Miller 1972; Payne 1992). 

Die Grundidee des Modells ist denkbar einfach: Aus der Differenz des Zweitstimmenanteils bei der letzten Wahl und dem aktuellen Prognoseergebnis, ergibt sich für jede Partei ein Trendfaktor (engl. Swing). Unter der Annahme das sich dieser Trend gleichmäßig auf alle Wahlkreise auswirkt, wird dieser zum Erstimmenergebnis der Parteien bei der letzten Bundestagswahl im jeweiligen Wahlkreis hinzuaddiert. Der Zusammenhang zwischen dem Abschneiden einer Partei bzw. eines Kandidaten im jeweiligen Wahlkreis bei der letzten Wahl und dem Trend auf Bundesebene bildet den Kern unserer Vorhersage.

Zusätzlich fließen in unser Modell noch weitere Erklärungsfaktoren mit ein. Einerseits, berücksichtigen wir den sogenannten Amtsbonus, d.h. ob ein Kandidat oder eine Partei bereits bei der letzten Bundestagswahl das Direktmandat im Wahlkreis gewinnen konnte. Und schließlich bezieht unser Modell den Landeslistenplatz eines Kandidaten mit ein. Diese Entscheidung beruht auf der Annahme, dass bekanntere Politiker einer Partei höhere Plätze auf den Landeslisten innehaben, und sich ihre Popularität positiv auf das Erstimmenergebnis auswirkt.

Mehr auf [erststimme2017.de](http://erststimme2017.de)