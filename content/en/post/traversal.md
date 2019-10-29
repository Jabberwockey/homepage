---
categories: ["programming", "Lisp", "Java", "algorithms", "data structures", "queues", "search"]
comments: true
title: "Searching (Part 1)"
date: "2019-10-29T14:31:00+01:00"
slug: ""
draft: ""
markup: mmark
diagram: true
---

Here are three things that I learned at uni that blew my mind:

1. All searches are tree traversals
2. All search strategies can be implemented by choosing the right queue
3. Every (AI) problem can be expressed as a search

Let's look at all three of these statements in turn.

# All searches are tree traversals

Let's look at a couple of examples for searches:

1. Check whether an XML document contains a *node*-tag.
2. Determine whether an array of *n* integers contains the integer *x*.
3. Starting from Picadilly Circus in London, find the best route to Trafalgar Square.
4. Given a set of things we know about the world, try to prove a statement *X*.

For the XML document example, the search is obviously a tree traversal. After all,
an XML document is a tree and, if push comes to shove, we need to check every single
node in that tree. So we need to traverse the tree.

The second example is not that intuitive, but remember that we have are interested
in what we are looking at during a search. If the array is unordered, like this:

5 8 1 10 11 3 9

then we have to look at each element in turn. But that is the same as traversing
a list. And a list is a special case of a tree. So in that example, too, we are
talking about a tree traversal.

This gets a bit more obvious when the array is sorted and we can use a smarter algorithm.

If the array looks like this

1 3 5 8 9 10 11

and I'm wondering whether the number 10 is in the array, we can use the following algorithm for finding
a value $$x$$ in a sorted array $$arr$$ with $$n$$ elements:

1. Set $$a := 0$$ and $$b := n - 1$$
2. If $$x < arr[a]$$ or $$x > arr[b]$$ then $$x$$ is not in $$arr$$. End.
3. Set $$c := \lceil \frac{b-a}{2} \rceil$$
4. If $$x == arr[c]$$ then $$x$$ is in $$arr$$ at position $$c$$. End.
5. If $$x < arr[c]$$ then set $$b := c - 1$$ and go to step 2.
6. If $$x > arr[c]$$ then set $$a := c + 1$$ and go to step 2.

This may not look like a tree at first, until we write down the possible order of nodes that are examined:

```mermaid
graph TD;
  8-->3;
  8-->10;
  3-->1;
  3-->5;
  10-->9;
  10-->11;
```

If $x$ is bigger than 8, we go down the right branch, otherwise the left branch, and so forth.

This gives us a time complexity of $$O(log(n))$$ - the height of a tree.

Alright, but what about our satnav example? I mean, we are talking about a complicated graph there.

Yes, but remember that when searching in graphs, we usually avoid circles. So we are looking for paths,
starting from a start node that do not have cycles - in other words, we are looking for a tree. And we
are looking for that tree by walking through the graph.

So, even if we don't have an explicite data structure called a tree, every search is a tree traversal -
of a so-called search tree.

# All search strategies can be implemented by choosing the right queue

As you may recall, the three main types for search strategies are:

1. Depth-first search
2. Breadth-first search
3. Best-first search

The first tries to dig deep and favours going down new paths before looking at other nodes.
The second one tries to look at all nodes of the same distance from the root node before going deeper.
And the third one looks at the most promising node first, whatever that one is.

My mind was blown when I learned that all of these searches can be implemented by the same
search algorithm if it uses a queue to keep track of all nodes that have yet to be examined
and if you choose the correct type of queue for the strategy you want.

Back then I was big into Lisp programming and this is what it looked like there:

```lisp
(defun search (start expand-fn goal-fn enqueue-fn dequeue-fn)
  (funcall enqueue-fn start)
  (loop for elm in (funcall dequeue-fn)
        while (and elm (not (funcall goal-fn elm)))
		do
		(progn 
		  (mapcar enqueue-fn (funcall expand-fn elm)))))
```

You get the different search strategies by plugging in the correct queue:

Search strategy | Type of Queue
----------------|----------------
Depth-first     | Stack
Breadth-first   | Queue
Best-first      | Priority Queue

That means: you can implement a search algorithm and decide the correct strategy later, closing
the algorithm early for modification, while its behaviour can still be modified.

Nice.

Of course, what's even nicer: tree traversal in general follows one of the three strategies.
So this works even if we "just" want to walk the whole tree, graph or whatever structure we are
looking at. We just need to select the correct queue.

# Every (AI) problem can be expressed as a search

This one takes a lot more explaining than the other two. For now, look at the fourth example.
Imagine that everything a machine "knows" is set as formulas in its brain and now it needs
to find the right knowledge to apply in a situation. We already phrased it the right way, it is
a form of search.

The real proof takes some more time and effort - if you are interested, do tell me and I will
try to put it in understandable terms

# Let's see some code

Alright, enough theory. Let's see how we can use all this in, let's say, Java.

First let us define an interface for our TreeIterator. As it is both an Iterator, but also a consumer
of more items to consider, the interface is quite straight forward to write:

```java
package de.grabarske.poirot.iterators.api;

import java.util.Iterator;
import java.util.function.Consumer;

public interface TreeIterator<T> extends Iterator<T>, Consumer<T> {

}
```

Now for  the class itself:

```java
package de.grabarske.poirot.iterators.impl;

import java.util.Collection;
import java.util.function.Function;
import java.util.function.Predicate;

import de.grabarske.poirot.iterators.api.TreeIterator;

/**
 * An iterator to traverse a tree - virtual or real. The important thing is that for every point
 * of the traversal, there needs to be a clear idea what nodes are "next" nodes. And also there
 * needs to be a strategy in what order the nodes should be visited. The former is done
 * by node expansion and an expansion function, the latter by choosing the right data structure,
 * i.e., the right queue.
 * 
 * @author Jens Grabarske
 *
 */
public class TreeIteratorImpl<T, Q extends Collection<T>> implements TreeIterator<T> {

	private Q queue;
	private Function<Q, T> dequeueFn;
	private Function<T, ? extends Iterable<T>> expandFn;
	private Predicate<T> pruner;
	
	/**
	 * The main constructor for a tree iterator.
	 * 
	 * @param startNode  The node where the iterator should start.
	 * @param expandFn   A function that, given a node, returns the most promising "next" nodes.
	 * @param queue      The queue to be used
	 * @param dequeueFn  A function for dequeuing the next element (depends on the queue, therefore a parameter).
	 * @param pruner     A predicate that may veto the inclusion of a node into the queue for pruning a search
	 */
	public TreeIteratorImpl(T startNode, Function<T, ? extends Iterable<T>> expandFn, Q queue, Function<Q, T> dequeueFn, Predicate<T> pruner) {
		super();
		this.queue = queue;
		this.dequeueFn = dequeueFn;
		this.expandFn = expandFn;
		this.pruner = pruner;
		accept(startNode);
	}

	/**
	 * The constructor for a tree iterator without a pruner.
	 * 
	 * @param startNode  The node where the iterator should start.
	 * @param expandFn   A function that, given a node, returns the most promising "next" nodes.
	 * @param queue      The queue to be used
	 * @param dequeueFn  A function for dequeuing the next element (depends on the queue, therefore a parameter).
	 */
	public TreeIteratorImpl(T startNode, Function<T, ? extends Iterable<T>> expandFn, Q queue, Function<Q, T> dequeueFn) {
		this(startNode, expandFn, queue, dequeueFn, a -> false);
	}

	public boolean hasNext() {
		return !queue.isEmpty();
	}

	public T next() {
		T current = dequeueFn.apply(queue);
		if (current != null)
			expandFn.apply(current).forEach(this::accept);
		return current;
	}

	public void accept(T arg0) {
		if (!pruner.test(arg0))
			this.queue.add(arg0);
	}
}
```

Here we go. This iterator can be used to perform any tree traversal we need - for searches or for walking for a tree.

Of course, it has the constructor signature from hell. Let us use some convenience classes to narrow the
functions we actually want down:

```java
package de.grabarske.poirot.iterators.impl;

import java.util.Stack;
import java.util.function.Function;
import java.util.function.Predicate;

/**
 * A tree iterator that uses depth-first traversal.
 * 
 * @author Jens Grabarske
 *
 */
public class TreeIteratorDepthFirstImpl<T> extends TreeIteratorImpl<T, Stack<T>> {

	public TreeIteratorDepthFirstImpl(T startNode, Function<T, ? extends Iterable<T>> expandFn, Predicate<T> pruner) {
		super(startNode, expandFn, new Stack<T>(), s -> s.isEmpty() ? null : s.pop(), pruner);
	}

	public TreeIteratorDepthFirstImpl(T startNode, Function<T, ? extends Iterable<T>> expandFn) {
		this(startNode, expandFn, a -> false);
	}

}
```

For breadth-first search:

```java
package de.grabarske.poirot.iterators.impl;

import java.util.LinkedList;
import java.util.Queue;
import java.util.function.Function;
import java.util.function.Predicate;

/**
 * An iterator for breadth-first search.
 * 
 * @author Jens Grabarske
 *
 */
public class TreeIteratorBreadthFirstImpl<T> extends TreeIteratorImpl<T, Queue<T>> {

	public TreeIteratorBreadthFirstImpl(T startNode, Function<T, ? extends Iterable<T>> expandFn, Predicate<T> pruner) {
		super(startNode, expandFn, new LinkedList<T>(), q -> q.poll(), pruner);
	}

	public TreeIteratorBreadthFirstImpl(T startNode, Function<T, ? extends Iterable<T>> expandFn) {
		this(startNode, expandFn, a -> false);
	}
}
```

And, last but not least, for best-first search:

```java
package de.grabarske.poirot.iterators.impl;

import java.util.PriorityQueue;
import java.util.function.Function;
import java.util.function.Predicate;

/**
 * An iterator for best-first tree traversal.
 * 
 * @author Jens Grabarske
 *
 */
public class TreeIteratorBestFirstImpl<T> extends TreeIteratorImpl<T, PriorityQueue<T>> {

	public TreeIteratorBestFirstImpl(T startNode, Function<T, ? extends Iterable<T>> expandFn, Predicate<T> pruner) {
		super(startNode, expandFn, new PriorityQueue<>(), p -> p.poll(), pruner);
	}

	public TreeIteratorBestFirstImpl(T startNode, Function<T, ? extends Iterable<T>> expandFn) {
		this(startNode, expandFn, a -> false);
	}
	
}
```

Here we have it, the first step for a general search framework that we can
work with. Basically traversal, but as we learned, every search is traversal,
so this gets us up and running really fast.
