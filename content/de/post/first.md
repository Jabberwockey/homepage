---
categories: ["html" "web" "javascript" "server" "cloud"]
comments: true
title: "Umzug nach Netlify"
date: "2019-10-27T11:31:00+01:00"
slug: ""
draft: ""
markup: mmark
diagram: true
---

Webentwicklung funktionierte um 2000 herum wie folgt: man nehme einen Editor,
irgendeinen Editor, öffne eine Datei und fange an zu schreiben:

```html
<html>
  <head>
    <title>Meine tolle neue Seite</title>
  </head>
  <body bgcolor="white">
    Und hier geht es los...
  </body>
</html>
```

Wir hatten ja nichts, damals. Als ich mit dem Kram anfing, da gab es zwar schon CSS, aber
es diente im wesentlichen dazu, eine ohnehin unlösbare Aufgabe noch komplizierter und noch
unlösbarer zu machen, nämlich eine Webseite auf zwei verschiedenen Browsern anzuzeigen, deren
Hersteller gar kein Interesse daran haben, dass es überall gleich aussieht.

Das Resultat waren unzählige Stunden der Verzweiflung und ausgerissener Haare. Und das war noch
*vor* den dynamischen Inhalten.

Zwanzig Jahre später nähert sich das ganze schon eher dem, was man als "cool" bezeichnen kann.
Nach nur wenigen Klicks habe ich auf Netlify zum Nulltarif ein neues Zuhause für meine Homepage
gefunden - es wird automatisch aus meinem Git-Repository gebaut. Und nicht nur versteht
das Hugo-Theme, was zum Einsatz kommt, direkt mathematische Formeln...

$$\frac{a}{b} = \frac{c}{d}$$

...es kann auch einfach so Diagramme erzeugen und darstellen.

```mermaid
graph TD;
  A-->B;
  A-->C;
  B-->D;
  C-->D;
```

Früher war tatsächlich nicht alles besser. Was für eine schöne Erkenntnis.
