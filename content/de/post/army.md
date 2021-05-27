---
categories: ["Programmierung", "Programmieraufgaben", "Wettbewerbe"]
title: "Programmieraufgabe 'Army Strength'"
date: 2021-05-27T17:21:09+02:00
slug: ""
draft: ""
markup: mmark
---

*Ich liebe es, kleine Programmieraufgaben und Kopfnüsse zu knacken. Glücklicherweise gibt es sehr viele Webseiten, auf denen man solche findet.*

*Diese Aufgabe fand ich im Archiv des Internet Problem Solving Contest, und zwar [hier](https://ipsc.ksp.sk/2008/real/problems/a.html).*

---

# Die Aufgabe

Die Problembeschreibung findet sich auf der oben verlinkten Webseite. Hier als Zitat:

> Each army consists of many different monsters. Each monster has a strength that can be described by a positive integer. (The larger the value, the stronger the monster.)
> The war will consist of a series of battles. In each battle, the weakest of all the monsters that are still alive is killed.
> If there are several weakest monsters, but all of them in the same army, one of them is killed at random. If both armies have at least one of the weakest monsters, a random weakest monster of MechaGodzilla’s army is killed.
> The war is over if in one of the armies all monsters are dead. The dead army lost, the other one won.
> You are given the strengths of all the monsters. Find out who wins the war.

# Eingabe

Als Eingabe erhält man eine Auflistung der Monster (im Form ihrer Stärke) auf beiden Seiten.

# Überlegungen

Dies ist ein typisches Beispiel für eine Aufgabe, die zunächst kompliziert klingt, aber bei näherer Betrachtung sehr einfach ist.

Wichtig ist, wie bei vielen solcher Problemstellungen, sich sehr genau vor Augen zu führen, was die Regeln bedeuten, in diesem Fall:

1. Jede Runde stirbt ein Monster mit der niedrigsten Stärke im Spiel.
2. Die Armee, die keine Monster mehr hat, verliert.

Was bedeutet es aber, wenn eine Armee gewinnt? Es bedeutet:

1. Entweder hatte die Armee ein Monster, das stärker war als alle Monster der anderen Armee, oder
2. Das stärkste Monster dieser Armee war genauso stark wie das stärkste Monster der anderen Armee und durch Zufall wurde dieses gewählt.

Anders gesagt: statt das Spiel durchzusimulieren in all seinen Varianten, können wir die Frage reduzieren auf: "Ist das größte Element von Armee A größer oder gleich dem größten Element
von Armee B?"

Im Falle eines Gleichstands gewinnt der Zufall, es ist also für unsere Zwecke ok, von einer beliebigen Seite auszugehen. Auch das reduziert die Komplexität.

# Lösung

Mein Programm sieht wie folgt aus:

```python

#!/usr/bin/python

import sys

def read_biggest_monster():
    return max(map(int, sys.stdin.readline().split()))

def treat_case():
    # Die Leerzeile wird ignoriert
    sys.stdin.readline()
    # Die Anzahl der Monster kann ignoriert werden,
    # da wir split() verwenden
    sys.stdin.readline()
    
    max_monster_g = read_biggest_monster()
    max_monster_m = read_biggest_monster()

    if max_monster_m > max_monster_g:
        print "MechaGodzilla"
    else:
        print "Godzilla"

def treat_cases():
    n = int(sys.stdin.readline())
    for i in xrange(n):
        treat_case()

treat_cases()
```

Auf der Hauptseite des Archivs, [https://ipsc.ksp.sk/archive](https://ipsc.ksp.sk/archive), sind sowohl die Testdatensätze für "leicht" und "schwer" hinterlegt, wie auch die
richtigen Resultate. Es gibt auch eine Musterlösung in Python.

Das Skript der Musterlösung unterscheidet sich nur kosmetisch von meiner. Mein Programm gibt dieselben Lösungen für die Testdatensätze.

Der von mir gewählte Algorithmus ist in O(n), da alle Zahlen gelesen und das Maximum bestimmt werden muss. Das ist auch die bestmögliche Lösung.

Für den Datensatz 1 brauchte das Programm 0,017 Sekunden, für den Datensatz 2 0,167 Sekunden.

# Was man lernen kann

Viele Programmieraufgaben erscheinen auf den ersten Blick schwer, prüfen aber vor allem eins ab, nämlich wie aufmerksam der Programmierer
Aufgabentexte liest. Auf den ersten Blick erscheint es, als müsse man das Spiel durchsimulieren, was das Laufzeitverhalten signifikant in
die Höhe treibt. Nur, wenn man noch einmal darüber nachdenkt, geht einem auf, dass dies gar nicht gefragt wurde.

Bei solchen Spielen also immer schauen ob der Sieger festgestellt werden kann OHNE überhaupt zu spielen. Wenn ja, dann sollte so eine Lösung
immer vorgezogen werden.
