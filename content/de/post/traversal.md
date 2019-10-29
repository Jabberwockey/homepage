---
categories: ["programming", "Lisp", "Java", "algorithms", "data structures", "queues"]
comments: true
title: "Suche und Traversierung neu gedacht - Teil 1 - Coole Sachen mit Java"
date: "2019-10-29T14:31:00+01:00"
slug: ""
draft: ""
markup: mmark
diagram: true
---

Einer der beeindruckendsten Dinge, die ich auf der Universität gelernt hatte, die mir
wirklich den Verstand gesprengt hatte, war, dass das Verhalten einer Suche nur davon
abhängt, welche Art von Queue verwendet wird.

Anders gesagt, es war dieses Skelett für eine allgemeine Suche:

```lisp
(defun search (start expand-fn goal-fn enqueue-fn dequeue-fn)
  (funcall enqueue-fn start)
  (loop for elm in (funcall dequeue-fn)
        while (and elm (not (funcall goal-fn elm)))
		do
		(progn 
		  (mapcar enqueue-fn (funcall expand-fn elm)))))
```

Für all die Leute da draußen die denken, dass Lisp nur eine wahllose
Sammlung von Klammern ist (und für Scheme-Programmierer, die denken,
dass Common Lisp einfach nur unelegant ist), der obige Code macht folgendes:

1. Es nimmt Funktionen um Dinge aus einer, naja, Queue, herauszuholen und einzureihen
2. Es nimmt Funktionen um festzustellen welche weiteren Knoten besucht werden müssen und eine Funktion um festzustellen, welches ein Zielknoten ist oder nicht
3. Während das Ziel noch nicht gefunden wurde, expandiert es fröhlich Knoten und reiht diese in die Queue ein. Die Queue ist die einzige Quelle für die Iteration

Jetzt: ob dies nun eine Tiefensuche, eine Breitensuche oder eine Best-First-Suche ist, hängt nur davon ab, was für Queue-Funktionen an dieses
Stück Code übergeben werden.

Sprich: wenn wir einen Stack verwenden (eine FILO-Queue), dann wird diese Suche eine Tiefensuche durchführen. Wenn wir eine reguläre Queue
(eine FIFO-Queue) verwenden, dann wird es eine Breitensuche. Und wenn wir eine Priority-Queue verwenden, dann wird es eine Best-First-Suche.

Das bedeutet: der Hauptsuchalgorithmus ist *unabhängig* von der verwendeten Suchstrategie! Das Verhalten ist ein Parameter und kann leicht ausgetauscht werden.

Ja. Kopf gesprengt.

Natürlich, als erfahrener Programmierer (ich denke, ich kann mich so nennen, ich habe die grauen Haare um das zu beweisen) weiß ich, dass das
ein perfektes Beispiel ist für das Prinzip

> Abgeschlossen für Veränderung, offen für Erweiterung.

Wir wollen solche Algorithmen. Wir wollen Systeme, die sich einfach durch das Verhalten erweitern lassen, das wir brauchen,
während der Kernalgorithmus so geschlossen wie möglich ist und nie wieder von schmutzigen Grabbelfingern von kleinen
Entwicklerhänden angetatscht werden müssen, zumindest nicht während unserer Lebenszeit - oder zumindest bis wir
kündigen und schnell das Weite suchen.

Aber, noch einmal gründlicher nachgedacht, so ist es offensichtlich, dass diese Suchidee gar nicht mal weit genug geht.
Es ist gut, dass wir das Traversieren einer Datenstruktur mit Queues steuern können, ohne überhaupt nach etwas zu suchen.
Anders gesagt: die Suche selbst ist nur eine Anwendung eines breiteren Algorithmuses, der mit Iteration zu tun hat.

Lasst uns mal sehen, wie man das in Java verwenden kann. Keine Sorge, ich werde das auch in anderen Sprachen machen.

# Java - Queued Iterator

Die neueren Java-Versionen haben die Java-Streams-API eingeführt und das ist der neue heiße Scheiß und wird krampfhaft
von denselben Leuten verwendet, die mir vor Jahren erzählt hatten, dass funktionale Programmierung überhaupt keine
praktischen Anwendungen hat (um fair zu bleiben, damals hatte es keine in Java, aber das nur nebenbei).

Versteht mich nicht falsch, die Java-Streams-API und Lambda-Ausdrücke und all das neue krasse Zeug ist ein Schritt in
die richtige Richtung und es hilft, klarer und deutlicher zu programmieren, ungeachtet all der Nein-Sager.

Aber man sollte nicht vergessen dass im Kern eine ziemlich alte Klasse arbeitet, die für Iterationen seit Java 5 und 6
verwendet wird: Iterator. Ja, das Ding, das diese guten, alten for-Schleifen bedient wie

```java
List<String> strs;
// ...
for (String str : strs) {
  // ...
}
```

Oder, noch schlimmer, diese manuellen while-Schleifen von alten Greisen, wie

```java
Iterator<String> iterator = strs.iterator();
while (iterator.hasNext()) {
  String current = iterator.next();
}
```

Ihr wisst schon. Das echt langweilige, wirklich old-school Zeug.

Aber Geduld. Wir kommen schon noch zu dem neuen, hippen Zeug zu sprechen.

## Eine Queue gleich welchen Namens?

Erst muss ich ein wenig meinen Frust über Java Luft machen. Oder, genauer,
über ein paar der Design-Entscheidungen von Java.

In meinem Lisp-Code mussten wir der Suchfunktion nur mitteilen welche
enqueue und dequeue Funktionen wir brauchten um ordentlich arbeiten zu
können. Das lag daran, dass in Lisp Queues und Stacks und Priority-Queues
im Wesentlichen gleich arbeiten und der einzige Unterschied ist, welches
Element die dequeue-Funktion ausspuckt.

Moderne Java-Versionen erlauben zwar nun auch Funktionsargumente, aber
es ist nicht ganz so einfach hier, da das Verhalten anders ist.

Denn Java kennt PriorityQueue, Stack und irgendeine Geschmacksrichtung
von Queue, sagen wir mal eine einfache LinkedList (das ebenfalls das
Queue-Interface erbt).

Einqueuen ist einfach genug. Alle diese Klassen unterstützen die "add()"-
Funktion. Man könnte für den Stack "push()" verwenden, aber das macht
auch nichts anderes als "add()".

Auslesen ist aber ein anderes Tier. Für Queue ist das "poll()" und wenn
die Queue leer ist, dann gibt das null zurück. Für Stack ist es "pop()" und
wenn der Stack leer ist, dann wirft das eine Exception.

Das nervt etwas. Wenn wir unseren Iterations- / Traversierungsmechanismus
agnostisch machen wollen bezüglich der zugrundeliegende Queue, dann sollten

Gut, zugegeben, das war jetzt gar nicht so stark aufgeregt. Nicht in der Stimmung.

Glücklicherweise ist der präferierte Stil in Java immer noch OOP und da gibt
es eine einfache Lösung in unserem Arsenal an Design-Patterns: Adapter.
Wir definieren wie diese uniforme API aussehen sollte und wir entwickeln
Klassen, die den Unterschied in Verhalten und API ausgleichen.

Für Queues brauchen wir einfach nur, was der ADT verlangt, nämlich:

```java
public interface QueueAdapter<T> {
	boolean isEmpty();
	T dequeue();
	void enqueue(T item);
}
```

Der aufmerksame Leser wird festgestellt haben, dass die ersten zwei Funktionen
uns erschreckend bekannt vorkommen. Das ist eigentlich exakt das, was uns
ein Iterator gibt! Wenn wir einfach den ADT ansehen, dann ist eine Queue nichts
anderes als ein Iterator, dem zusätzliche Elemente zum Traversieren und Besuchen
on the fly hinzugefügt werden können.

Das ist mal eine Art, darüber nachzudenken! Um diese Idee widerzuspiegeln, lasst uns
das Interface umschreiben um anderen Programmierern diese Idee zu vermitteln:

```java
public interface QueueAdapter<T> extends Iterator<T>, Consumer<T> {
}
```

Klingt das richtig?

Eine Queue ist etwas um durch eine "wartende" Menge durchzutraversieren, also ja, es klingt
richtig, sie einen Iterator zu nennen. Aber diese Queue erlaubt auch ständig neue Elemente
für das Traversieren, also ist es auch ein Konsument von Dingen.

Lasst uns mal sehen, wie ein solcher Adapter dann aussehen könnte:

```java
public class StackAdapterImpl<T> implements QueueAdapter<T> {

	private Stack<T> stack = new Stack<>();
	
	public boolean hasNext() {
		return !stack.empty();
	}

	public T next() {
		return stack.empty() ? null : stack.pop();
	}

	public void accept(T arg0) {
		stack.push(arg0);
	}
	
}
```

Und für Queues:

```java
public class QueueAdapterImpl<T> implements QueueAdapter<T> {

	private LinkedList<T> queue = new LinkedList<>();
	
	@Override
	public boolean hasNext() {
		return !queue.isEmpty();
	}

	@Override
	public T next() {
		return queue.poll();
	}

	@Override
	public void accept(T arg0) {
		queue.add(arg0);
	}

}
```

Und schließlich für Priority-Queues:

```java
public class PriorityQueueAdapterImpl<T> implements QueueAdapter<T> {

	private PriorityQueue<T> queue = new PriorityQueue<>();
	
	@Override
	public boolean hasNext() {
		return !queue.isEmpty();
	}

	@Override
	public T next() {
		return queue.poll();
	}

	@Override
	public void accept(T arg0) {
		queue.add(arg0);
	}

}
```

Gna, ich hasse Copy-\&-Paste-Programmierung, aber in diesem Fall gibt es keine guten Ansätze
drum herum (wenn ihr eine Idee habt, schreibt mir ruhig). Glücklicherweise ist das die
einzige Stelle, wo wir das so exzessiv tun müssen.

Jetzt, wo wir das haben, sollten wir einmal über Knoten reden.

## Knoten

Ein Knoten in einem Suchproblem kann alles sein. Es kann ein tatsächlicher Knoten in
einem Baum oder einem Graphen sein, aber es kann auch ein abstrakter Knoten sein, wie
eine bestimmte Position in unserem Suchbaum.

Zum Beispiel, es ist nicht sofort offensichtlich, wenn du ein Schachprogramm schreibst,
dass ein bestimmter Moment deiner Partie ein Knoten in einem Baum ist - aber es ist
so.

Der Punkt für Suchprobleme ist, dass wir die Möglichkeit haben müssen, die "nächsten"
Knoten zu finden, die wir uns ansehen müssen. Das nennt man Knotenexpansion. Zum Beispiel,
denkt an ein Navigationssystem. Du bist an Punkt A, sagen wir, Picadilly Circus in London.
Du möchtest, dass deine Software weiß, welche nächsten Kreuzungen man von dort aus
erreichen kann.

Diesmal können wir all die neuen tollen Funktionen in Java 8 für funktionale Programmierung
gut verwenden:

```java
Function<E, List<E>> expandFn;
```

Das könnte alles Mögliche sein und hilft uns, dass wir keine großen Ansprüche an die
Modelle, die wir durchtraversieren wollen, stellen müssen, nur damit Leute unseren
tollen neuen Iterator verwenden können.

Zeit, all die Theorie in die Praxis umzusetzen.

## Iterator - Version 1

Wenn wir das alles zusammen packen, dann sieht der Iterator wie folgt aus:

```java
public class QueuedIterator<E> implements QueueAdapter<E> {

	private QueueAdapter<E> queue;
	private Function<E, List<E>> expandFn;
	
	public QueuedIterator(E startNode, Function<E, List<E>> expandFn, QueueAdapter<E> queue) {
		this.queue = queue;
		this.expandFn = expandFn;
		accept(startNode);
	}
	
	@Override
	public boolean hasNext() {
		return queue.hasNext();
	}

	@Override
	public E next() {
		E current = queue.next();
		if (current != null)
			expandFn.apply(current).forEach(this::accept);
		return current;
	}

	@Override
	public void accept(E arg0) {
		this.queue.accept(arg0);
	}

}
```

Beachtet, dass diese Klasse auch wieder QueueAdapter implementiert. Es ist ja
eben ein Iterator *und* ein Iterator, dem man während des Betriebs weitere
Elemente hinzufügen kann (expandierte Knoten in diesem Fall). Also, wenn
es quackt wie eine Ente...

Der Rest ist eigentlich recht einfach. Der einzige Unterschied zu unserem
normalen Queue-Verhalten ist, dass für jeden Knoten die Expansionsfunktion
verwendet wird um die expandierten Knoten zu finden und diese der Queue
hinzuzufügen.

Ist das bereits abgeschlossen für Modifikation?

Noch nicht ganz. Es fehlt noch was.

In jeder Graphensuche muss man vorsichtig sein, dass man nicht in Zyklen
läuft. Und wenn du nach dem nächsten Schachzug suchst, dann möchtest
du vermutlich die Suchtiefe limitieren, damit dein Rechner nicht
die ganze Nacht alle möglichen Schachpositionen berechnet.

Mit anderen Worten, wir wollen vielleicht die Iteration später begrenzen.
Einerseits wollen wir bereits besuchte Knoten ausschließen, andererseits
möchten wir unseren Suchbaum stutzen und Äste absägen, die jetzt
irrelevant geworden sind.

Um das in unseren Iterator zu übresetzen: wir möchten vielleicht dem Iterator
sagen, wenn ein Knoten nicht der Queue hinzugefügt werden sollte.

## Iterator - Version 2

Das ist recht einfach und kann wieder mit funktionalem Schnuffschnuff gemacht werden:

```java
package de.grabarske.poirot.iterators.impl;

import java.util.List;
import java.util.function.Function;

import de.grabarske.poirot.queues.api.QueueAdapter;

/**
 * An iterator using a queue as a backend, expanding nodes using an expansion function.
 * 
 * @author Jens Grabarske
 *
 */
public class QueuedIterator<E> implements QueueAdapter<E> {

	private QueueAdapter<E> queue;
	private Function<E, List<E>> expandFn;
	private Function<E, Boolean> prunerFn;
	
	public QueuedIterator(E startNode, Function<E, List<E>> expandFn, Function<E, Boolean> pruner, QueueAdapter<E> queue) {
		this.queue = queue;
		this.expandFn = expandFn;
		this.prunerFn = pruner;
		accept(startNode);
	}
	
	public QueuedIterator(E startNode, Function<E, List<E>> expandFn, QueueAdapter<E> queue) {
		this(startNode, expandFn, a -> false, queue);
	}
	
	@Override
	public boolean hasNext() {
		return queue.hasNext();
	}

	@Override
	public E next() {
		E current = queue.next();
		if (current != null)
			expandFn.apply(current).forEach(this::accept);
		return current;
	}

	@Override
	public void accept(E item) {
		if (!prunerFn.apply(item))
		   this.queue.accept(item);
	}

}
```

Und das wäre es. Der Pruner ist ein Prädikat, das true zurück gibt, wenn ein Knoten abgeschnitten werden sollte. Der Default-Pruner gibt immer false zurück und
erlaubt daher, dass alle Knoten in die Queue aufgenommen werden.

## Bequemlichkeitsklassen

Für Tiefensuche:

```java
public class DepthFirstIterator<E> extends QueuedIterator<E> {

	public DepthFirstIterator(E startNode, Function<E, List<E>> expandFn, Function<E, Boolean> pruner) {
		super(startNode, expandFn, pruner, new StackAdapterImpl<>());
	}

	public DepthFirstIterator(E startNode, Function<E, List<E>> expandFn) {
		this(startNode, expandFn, a -> false);
	}

}
```

Für Breitensuche:

```java
public class BreadthFirstIterator<E> extends QueuedIterator<E> {

	public BreadthFirstIterator(E startNode, Function<E, List<E>> expandFn, Function<E, Boolean> pruner) {
		super(startNode, expandFn, pruner, new QueueAdapterImpl<>());
	}

	public BreadthFirstIterator(E startNode, Function<E, List<E>> expandFn) {
		this(startNode, expandFn, a -> false);
	}

}
```

Und schließlich für Best-First-Suche:

```java
public class BestFirstIterator<E> extends QueuedIterator<E> {

	public BestFirstIterator(E startNode, Function<E, List<E>> expandFn, Function<E, Boolean> pruner) {
		super(startNode, expandFn, pruner, new PriorityQueueAdapterImpl<>());
	}

	public BestFirstIterator(E startNode, Function<E, List<E>> expandFn) {
		this(startNode, expandFn, a -> false);
	}

}
```

Lasst sie uns testen:

```java
class BreadthFirstIteratorTest {

	private TreeNode<Integer> startNode;
	
	@BeforeEach
	void init() {
		startNode = TreeNode.as(8, TreeNode.as(2, TreeNode.as(1)),
				                   TreeNode.as(3, TreeNode.as(9, TreeNode.as(4, TreeNode.as(10)), TreeNode.as(11))));
	}
	
	private void assertIteratorOrder(Iterator<TreeNode<Integer>> iterator, Integer...integers) {
		for (int i : integers) {
			int res = iterator.next().getItem();
			assertEquals(i, res);
		}
	}
	
	@Test
	void testOrder() {
		Iterator<TreeNode<Integer>> iterator = new BreadthFirstIterator<>(startNode, node -> node.getChildren());
		assertIteratorOrder(iterator, 8, 2, 3, 1, 9, 4, 11, 10);
	}

	@Test
	void testPruning() {
		Iterator<TreeNode<Integer>> iterator = new BreadthFirstIterator<>(startNode, node -> node.getChildren(), n -> n.getItem() % 2 == 1);
		assertIteratorOrder(iterator, 8, 2);		
	}
	

	public static class TreeNode<T> {
		private T item;
		private List<TreeNode<T>> children;
		
		public TreeNode(T item, List<TreeNode<T>> children) {
			this.item = item;
			this.children = children;
		}

		public T getItem() {
			return item;
		}

		public List<TreeNode<T>> getChildren() {
			return children;
		}
		
		@SafeVarargs
		public static <T> TreeNode<T> as(T item, TreeNode<T>...nodes) {
			return new TreeNode<T>(item, Arrays.asList(nodes));
		}
		
	}
	
}
```

Das Schreiben der anderen Tests und, viel wichtiger, die Abstraktion von dem, was für alle Tests gleich ist,
wird als Übung dem Leser überlassen.

# Die Hauptpunkte dieses Artikels

Ich weiß. Du hast es eilig. Also lass uns schnell alles zusammenfassen, bevor es weitergeht.

* Was für eine Scuhe du hast, hängt von der Queue ab, die im Hintergrund arbeitet
* Wir können davon abstrahieren um zu einem Konzept für Iteration und Traversierung aller möglichen Datenstrukturen und Suchräumen zu kommen, egal ob virtuell oder real
* Wir können einen Iterator in Java schreiben, der eine beliebige Queue verwendet und einen Pruner, die abgeschlossen für Veränderung aber offen für Erweiterung ist

Das sollte erst einmal alles sein.

Lasst uns nächstes Mal den Iterator erweitern um beliebige Suchen zu unterstützen, lasst
uns sehen, wie man Graphen sicher und effizient durchläuft und wie man all die schönen
Java-Streams-Sachen bekommt, jetzt, wo wir uns um die Basics gekümmert haben.

