---
categories: ["Programmierung", "Programmieraufgaben", "Wettbewerbe"]
title: "Programmieraufgabe 'Breaking in'"
date: 2021-05-27T19:41:09+02:00
slug: ""
draft: ""
markup: mmark
diagram: true
---

*Ich liebe es, kleine Programmieraufgaben und Kopfnüsse zu knacken. Glücklicherweise gibt es sehr viele Webseiten, auf denen man solche findet.*

*Diese Aufgabe fand ich im Archiv des Internet Problem Solving Contest, und zwar [hier](https://ipsc.ksp.sk/2008/real/problems/b.html).*

---

# Die Aufgabe

Die Problembeschreibung findet sich auf der oben verlinkten Webseite.

Diese ist ein wenig komplexer als die mit der Armee. Genauer ist dies eine graphentheoretische Aufgabe.

Für das erste Zahlenbeispiel der Aufgabenstellung lässt sich folgender Graph erzeugen, wobei eine Kante bedeutet, dass ein Server dem anderen vertraut:

```mermaid
graph TD;
	5-->4;
	3-->1;
	3-->2;
	4-->3;
	5-->3;
```

Das zweite Beispiel entspricht diesem Graph:

```mermaid
graph TD;
	6-->5;
	1-->2;
	2-->3;
	3-->1;
	1-->4;
	5-->6;
```

Im zweiten Fall ist die Lösung 4, da 
