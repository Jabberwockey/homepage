---
categories: ["programming", "Lisp", "Java", "algorithms", "data structures", "queues"]
comments: true
title: "Rethinking search and traversal - Part 1 - Cool things with Java"
date: "2019-10-29T14:31:00+01:00"
slug: ""
draft: ""
markup: mmark
diagram: true
---

*Dieser Artikel ist derzeit nur auf Englisch verfügbar. Vielleicht werde ich ihn mal
übersetzen, aber bis dahin ist hier das englischsprachige Original.*

One of the most mind-blowing things I learned at university was that the behaviour of
a search can be determined by the kind of queue that is being used.

In other words, it was this skeleton for a general search:

```sbcl
(defun search (start expand-fn goal-fn enqueue-fn dequeue-fn)
  (funcall enqueue-fn start)
  (loop for elm in (funcall dequeue-fn)
        while (and elm (not (funcall goal-fn elm)))
		do
		(progn 
		  (mapcar enqueue-fn (funcall expand-fn elm)))))
```

For all you people who think that Lisp is just a random number of
parens (and for the Scheme programmers who think that Common Lisp
is just unelegant), here's what this kind of code does:

1. It takes functions to enqueue and dequeue elements from, well, a queue.
2. It takes functions to derive further nodes to be visited and a function to tell whether a node is a goal or not.
3. While the goal hasn't been found yet, it expands nodes and enqueues those children, using the queue as the sole source for the traversal.

Now: whether this is a depth-first search or a breadth-first search or a best-first search depends on the queue functions we
passed to this piece of code.

I.e.: if we use a stack (a FILO queue), then this function will carry out a depth-first search. If we use a regular queue (a FIFO queue), then
this will be a breadth-first search. And if we use a priority queue, this will be a best-first search.

That means: the main search algorithm is *independent* of the search strategy used! The behaviour is plugged in and can be exchanged.

Yeah. Mind blown.

Of course, as a seasoned programmer (I think I can call myself that, I have the grey hair to prove it), I know that this is
a perfect example for the principle

> Closed for modification, open for extension.

We want algorithms like that. We want to have systems that can easily be extended with behaviour we need, while the main
core algorithm is as closed as possible - as in: never needs to be touched by grabby, filthy little developer hands again,
at least not in our lifetime - or at least until we quit and run away from the code base as quickly as possible.

However, rethinking this, it is apparent that the search idea doesn't even go far enough. It's good to be able to guide
the traversal of a data structure with queues in general, without specifically looking for something. In other words:
the search itself is just an application of a broader algorithm having to do with iteration.

Let's see how we can apply this in Java programming. Don't worry, I will do that in other languages as well.

# Java - Queued Iterator

The newer Java versions introduced the Java Streams API which is the "new shit" and used in a frenzy by the same people
who told me years earlier, that functional programming doesn't have any practical applications whatsoever (to be fair, it
didn't have any in Java at that time, but I digress).

Don't get me wrong, the Java Streams API and lambda expressions and all the new fancy stuff is a step in the right
direction and makes for much clearer and concise programming, despite all the nay-sayers.

But one should remember that at its core, there is a rather old class at work that is used for iterations since
Java 5 and 6: Iterator. Yeah, the one that powers your good old for-loops like

```java
List<String> strs;
// ...
for (String str : strs) {
  // ...
}
```

Or, worse even, the manual while-loops done by some old geezers, like

```java
Iterator<String> iterator = strs.iterator();
while (iterator.hasNext()) {
  String current = iterator.next();
}
```

You know. The really boring, really old-school stuff.

But bear with me. We'll get to the new, cool, hip stuff in due time.

## A queue by any other name?

First I need to rant a little about Java. Or, more specifically, about
certain design decisions done by Java.

In my Lisp code above, we only needed to tell my search function which
enqueue and dequeue functions we needed to work properly. That is because
in Lisp, queues and stacks and priority queues basically work the same,
the only difference is, which element the dequeue function returned.

While modern Java versions allow passing function arguments as well, it's
not that simple, as the behaviour is slightly different.

You see, Java knows PriorityQueue, Stack and a flavour of Queue, let's say
a simple LinkedList (as that implements the Queue interface as well).

Enqueueing is easy enough. All of these classes support the "add()" function.
You could use "push()" for Stack, but it doesn't do anything different from
"add()".

Dequeueing however is a different beast. For Queue it's "poll()". And if the
queue is empty, that will return null. For Stack it's "pop()". And if the
stack is empty, it will throw an exception.

That is a bit annoying. If we want our iteration / traversal mechanism to
be agnostic of the used queue, all the queues should behave the same way and
have the same API.

Ok, I admit it, that wasn't that much of a rant. Not in the mood.

Thankfully, the preferred style in Java is still OOP and there is a simple
solution in our arsenal of design patterns: adapters. We define how this uniform
API is supposed to look like and then we design classes that bridge the gap
between the two different behaviours and APIs.

For queues, we pretty much need what the ADT of queues asks for:

```java
public interface QueueAdapter<T> {
	boolean isEmpty();
	T dequeue();
	void enqueue(T item);
}
```

The keen reader will have noticed that the first two functions look awfully
familiar. They are actually exactly what an Iterator gives us! Looking at the ADT,
we can say that a queue is nothing else but an iterator that allows for additional
elements to be traversed to be added on the fly.

What a way to think about that! To reflect this new idea, let's rewrite the interface
to tell other programmers about this idea of ours:

```java
public interface QueueAdapter<T> extends Iterator<T>, Consumer<T> {
}
```

Does that sound correct?

A queue is for traversing through "waiting" crowds, so yes, it sounds apt to call it an iterator.
But a queue also accepts new elements all the time. So it is also a consumer of items.

Let's see how such an adapter could look like:

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

And for queues:

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

And finally for priority queues:

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

Gna, I hate copy \& paste programming, but in this case, there are no good ways around it
(if you have an idea, pop me a line). Thankfully, this is the only bit of that we need
to do.

Now that we have our queue adapters, we need to talk about nodes.

## Nodes

A node in a search problem can be anything. It can be an actual node as in a tree or
a graph, but it can also be an abstract node as in: a specific position in our
search tree.

For example, it is not apparent when you are writing a chess program that a particular
moment in your chess game is a node in a tree - but it is.

The point for search problems is that we need to be able to get all the "next" nodes to
look at. This is called node expansion. For example, think of a navigation system. You
are at point A, let's say, Picadilly Circus in London. You want your software to know
all the next intersections you can get from there.

This time we can put the new functional programming goodness of Java 8 to proper
use:

```java
Function<E, List<E>> expandFn;
```

That could be anything and doesn't ask us to define big models just to use our
spiffy new iterator.

Time to put all that theory to the test.

## Iterator - version 1

Putting it all together, the iterator looks like this:

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

Notice that this class implements QueueAdapter as well. It is, after all,
an iterator *and* an iterator that accepts more elements (expanded notes in
this case). So, if it quacks like a duck...

The rest is rather straight forward. The only difference to the normal
behaviour of our queues is that for every node, the expansion function is
called and the expanded nodes are added to the queue.

Is this already closed for modification?

Not quite. There is something still missing.

In every graph search, you need to be careful to make sure that you do
not run into circles. And if you are looking for your next chess move, you
may want to limit the search depth so that your computer doesn't work all
night to solve all possible chess positions.

In other words, we may want to limit the iteration later. On the one hand,
we may want to exclude nodes we already visited, on the other we may want
to prune our search tree and cut off branches we know are irrelevant.

To translate this to our iterator: we may want to tell the iterator when
a node should not be added to the queue.

## Iterator - version 2

This is rather easy and can be done with some functional goodness as
well:

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

And here we are. The pruner is a predicate which returns true if a node should be pruned. The standard pruner always returns false and therefore allows all nodes
to be added to the queue.

## Convenience classes

For depth-first iteration:

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

For breadth-first iteration:

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

And finally for best-first iteration:

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

Let's test them:

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

Writing the other tests and, more importantly, abstracting away what is the same for all the tests
is left as an exercise for the reader.

# The main points of this article

I know. You are in a hurry. So let's recap quickly before we move on.

* The kind of search you are doing depends on the queue that works in the background
* We can raise the abstraction to use this idea for any iteration or traversal of data structures and search structures, be they virtual or real
* We can code an iterator in Java that uses an arbitrary queue and a pruner which is closed for modification but open for extension

That should be all for now.

Next time, let us extend the iterator to allow for arbitrary searches, let us see how we can
navigate graphs safely and efficiently and how to feed all that Java streams goodness, now that
we have the basics out of the way.
