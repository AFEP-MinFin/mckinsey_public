# Replicatie McKinsey 'Een verstevigd fundament voor iedereen'

Zijn er grote, stabiele verschillen tussen de kwaliteit van scholen in het funderend onderwijs? Het antwoord op deze vraag heeft grote implicaties voor onderwijsbeleid. Twee toonaangevende studies kwamen in korte tijd tot verschillende conclusies. McKinsey voerde in 2018 in opdracht van het Ministerie van OCW een [studie](https://www.rijksoverheid.nl/documenten/rapporten/2020/04/22/een-verstevigd-fundament-voor-iedereen) uit naar het funderend onderwijs. Gebaseerd op een kwantitatieve analyse presenteerde deze studie een aantal bevindingen, waaronder dat "scholen onder vergelijkbare omstandigheden sterk verschillen in de bijdragen die zij leveren aan de ontwikkeling van hun leerlingen". In 2019 verscheen een [notitie](https://www.cpb.nl/sites/default/files/omnidownload/CPB-Notitie-19feb2019-Verschillen-in-leerresultaten-tussen-basisscholen.pdf) van het Centraal Planbureau (CPB) gebaseerd op gegevens uit het primair onderwijs. Deze studie trok een aantal conclusies, waaronder dat de verschillen mogelijk beperkt zijn omdat de scores van scholen sterk variëren van jaar tot jaar.

CPB rapporteerde grote effecten van contextuele factoren (populatiekenmerken), terwijl McKinsey concludeerde dat "[het] verschil [in leerresultaten] wordt niet verklaard door de uitgaven of de context van de school, maar door de dagelijkse keuzes die schoolbestuurders, schoolleiders en leraren maken." McKinsey deed aan de hand van deze conclusie een aantal beleidsaanbevelingen.

Ten behoeve van de beleidsadvisering aan de Minister van Financiën is een aanvullend onderzoek gedaan om de verschillen tussen de twee studies te duiden. Beide studies zijn nader onderzocht. Deze repository heeft betrekking op replicatie van het McKinsey onderzoek.

## Onderzoeksvraag

1.  Kunnen de uitkomsten van McKinsey gereproduceerd worden?
2.  Rechtvaardigt de statistische onderzoeksmethode van McKinsey de uitkomsten zoals gesteld in het rapport?
3.  Hoe kan het ogenschijnlijke verschil met CPB worden verklaard?

## Leeswijzer

-   De aangeleverde code van McKinsey staat in de map [handover](%60handover%60).
-   Uitkomsten zijn gepresenteerd aan de hand van slides (`presentatie/mckinseyv3.pdf`).
-   Er is 1 bestand aangepast om de analyse van McKinsey te testen (`PO model AANGEPAST FIN.R`).

## Replicatie

McKinsey heeft vragenlijsten uitgezet bij schoolbesturen en de antwoorden gebruikt bij de data analyse. De ingevulde vragenlijsten zijn mogelijk te herleiden naar bepaalde schoolbesturen en daarom niet te publiceren als open data. Het Ministerie van Financiën heeft daarom een Data Privacy Impact Assessment (DPIA) afgerond om toegang te krijgen tot de gegevens. Om deze reden worden op deze pagina alleen syntax bestanden gepubliceerd en geen data-bestanden.

## Uitkomst

De uitkomsten van de replicatie zijn intern gepresenteerd aan collega's van het Ministerie van OCW en Financiën. Daarnaast is de replicatie inhoudelijk voorgelegd aan een onafhankelijk methodoloog. Hierbij zijn vragen ontstaan over de gehanteerde onderzoeksmethode van McKinsey waarbij twijfel bestaat of de conclusies over de factoren die onderwijskwaliteit 'verklaren' zijn te trekken op basis van de kwantitatieve analyse van het rapport. De bevindingen op die onderdelen zijn voor ons aanleiding om het rapport van McKinsey niet te citeren of gebruiken in de beleidsvoorbereiding, in ieder geval tot de methode is getoetst door wetenschappers met relevante academische expertise.

1.  *Kunnen de uitkomsten van McKinsey gereproduceerd worden?*

    Het was mogelijk om de uitkomsten in het McKinsey rapport te reproduceren met de aangeleverde bestanden. Uit de bestanden blijkt welke gegevens en methodes McKinsey exact heeft gebruikt.

2.  *Rechtvaardigt de statistische onderzoeksmethode van McKinsey de uitkomsten zoals gesteld in het rapport?*

    De onderzoeksmethode van McKinsey kent kwetsbaarheden, voornamelijk dat zij gevoelig is voor het uitvergroten van willekeurige correlaties tussen variabelen en gebrek aan oorzakelijke verbanden tussen de onafhankelijke variabelen en de afhankelijke variabele.

3.  *Hoe kan het ogenschijnlijke verschil met CPB worden verklaard?*

Grofweg zijn er drie oorzaken van verschillen tussen de analyses:

-   *Verschillende brongegevens:* CPB heeft toegang tot fijnmazige gegevens op leerlingniveau, terwijl McKinsey gemiddelde toetsscores op schoolniveau heeft gebruikt voor de analyse.
-   *Vragenlijsten*: McKinsey heeft additionele gegevens verzameld, inclusief het afnemen van vragenlijsten, om informatie te verzamelen over het management op school.
-   *Andere onderzoeksmethode:* CPB gebruikt regressie-methodes en McKinsey gebruikte 'random forest' modellen om verbanden tussen variabelen te analyseren.

## Dankwoord

Dit onderzoek had niet plaats tot stand kunnen komen zonder:

-   De overdracht van gegevens en syntax door het Ministerie van OCW

We hebben baat gehad bij de feedback van:

-   Collega's van andere onderdelen van het Ministerie van Financiën

-   Collega's van het Ministerie van OCW

-   Twee onderzoekers van McKinsey die voor de replicatie hun onderzoeksaanpak hebben toegelicht.
