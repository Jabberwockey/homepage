---
title: ➕ The Odds Pyramid
summary: A CS puzzle about sequences and number theory.
date: 2024-12-01
math: true
authors:
  - admin
tags:
  - Math
  - Number theory
  - Programming
image:
  caption: 'A pyramid of odds'
---

I like to show my trainees CS and math puzzles. It is my firm belief that solving them regularly keeps your mind sharp. They usually also teach you about useful and enlightening principles and this is a wonderful example.

It starts as a seemingly tricky programming question, but it ultimately teaches you a lot about number theory and algorithms.

## The task

Let us imagine a pyramid of infinite height that contains all the odd numbers.

The first row just contains 1.

The second row contains 3 and 5.

The third one 7, 9 and 11 and so on.

So the pyramid looks like this:

```
      1
    3   5
  7   9   11
  ...
```
Write a computer program that takes the number of a row and that returns the sum of all the odds in that row.

## Vetting

I like to give young trainees and job applicants this task, as I want to see what kind of programmers they are. Most of them will start writing complicated loops,
wrecking their brains how best to keep track in which row they are, how many odds they have seen, etc.

I usually let them do that for half an hour and then I tell them that it's always a good idea to determine the first results manually to see whether a pattern emerges which
reduces the complexity of the task.

If we do that for this task, we get the following results:

{{< table path="manual.csv" header="true" caption="Table 1: Manual calculations" >}}

In other words, if the row in question is called x, then the result appears to be x³. That would surely be handy if that was the case. No need of weird loops and it
would reduce the complexity of the algorithm from O(n) or even O(n²) down to O(1).

But... is that really the case?

## Proof

To prove this, we need a little number theory. But don't worry, all we need is very easy to introduce without having to get out the old math books.

Let's start with a useful formula a little lazy German boy came up with when his teacher gave him a tedious and boring task.

### Gauß

Gauß was a German mathematician. When he was a little boy, his teacher gave the class the task to add up all numbers from 1 to 100. If you were done, you
were allowed to leave early. As for all schoolboys, that was very enticing for our young genius. He left after 5 minutes - his teacher was astounded
how quick the boy came up with the answer - he didn't add it all up, of course, he used some good, old lateral thinking.

He reasoned as follows: if you write the whole sum from smallest to largest and underneath it from largest to smallest, adding all the summands individually,
then you have 100 times 101. But as you added the sum twice, you just take half of that and that's the result:

{{< math >}}
~~1 + ~~2 + ~~3 + \ldots + 100\\
100 + ~99 + ~98 + \ldots + ~~1\\
\hline
101 + 101 + 101 + \ldots + 101\\
= 100 \cdot 101
{{< /math >}}

#### Proof by induction

{{< math >}}
\sum_{i = 1}^{n \in \mathds{N}} i =^? \frac{n \cdot (n + 1)}{2}
{{< /math >}}

**Basis**

{{< math >}}
\sum_{i = 1}^1 i = 1 = 1 \cdot \frac{2}{2} \square
{{< /math >}}

**Anchor**

Assume, that the formula holds for all numbers up to n.

{{< math >}}
\sum_{i=1}^n i = \frac{n \cdot (n + 1)}{2}
{{< /math >}}

**Step**

{{< math >}}
\sum_{i = 1}^{n + 1} i = (n + 1) + \sum_{i = 1}^n i = (n + 1) + \frac{n \cdot (n + 1)}{2} = \frac{(n + 2) \cdot (n + 1)}{2} \square
{{< /math >}}

### Sum of odd numbers

Another very interesting property of numbers we can use, is: the sum of the first n odd numbers is n². This one is not very intuitive and surprising - one of the little gems you come across when dealing with
number theories. But it's just as easy to prove:

#### Proof by induction

{{< math >}}
\sum_{i = 1}^{n \in \mathds{N}} 2i - 1 =^? n²
{{< /math >}}

**Basis**

{{< math >}}
\sum_{i = 1}^{1} 2i - 1 = 2 \cdot 1 - 1 = 1 = 1² \square
{{< /math >}}

**Anchor**

Assume, that the formula holds for all numbers up to n.

{{< math >}}
\sum_{i = 1}^{n \in \mathds{N}} 2i - 1 = n²
{{< /math >}}

**Step**

{{< math >}}
\sum_{i = 1}^{n + 1} 2i - 1 = 2((n + 1) + 1) + \sum_{i = 1}^{n} 2i - 1 = 2n + 4 + n² = (n + 1)² \square
{{< /math >}}

#### Observation

Such an easy proof and such a mindblowing equivalence. All squares can be composed of all the odds to a certain point.

### The odd pyramid

With all that under our belt, the proof of the original pyramid is easy. Let's say that *P(x)* is the result of the
function we are supposed to write, *S(x)* is the Gauß sum of x and *T(x)* is the sum of the first x odds, then it's clear from the construction, that
the number of odds in a pyramid of height n is *T(S(n))* - the sum of the first S(n) odds. And if we just want to count the nth row, then it's
*P(n) = T(S(n)) - T(S(n-1))*

{{< math >}}
P(n) = T(S(n)) - T(S(n-1)) = S(n)² - S(n-1)² = \frac{n²\cdot(n + 1)²}{4} - \frac{n²\cdot(n - 1)²}{4} = \frac{n^4 + 2n³ + n² - n^4 + 2n³ - n²}{4} = \frac{4n³}{4} = n³ \square
{{< /math >}}

## The program

So, instead of long complicated loops, it's enough to write

```python
def solution(n):
  return n ** 3
```

