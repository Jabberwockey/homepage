---
categories: ["html", "web", "javascript", "server", "cloud"]
comments: true
title: "Moving to Netlify"
date: "2019-10-27T11:31:00+01:00"
slug: ""
draft: ""
markup: mmark
diagram: true
---

Web development in 2000 worked like this: take an editor, any editor,
open a file and write:

```html
<html>
  <head>
    <title>My really great site</title>
  </head>
  <body bgcolor="white">
     And here we go...
  </body>
</html>
```

We didn't have any good things back then. When I started with the stuff, CSS already
existed, barely, but its main task back then was to make an unsolvable problem even
more complicated and even more unsolvable: to make sure that the same website looked
exactly the same on two different browsers while the developers of said browsers have
absolutely no interest in having websites that look the same across the board.

The result were countless hours of pure despair and ripped out hairs. And that was
*before* the dynamic content.

Twenty years later we're approaching something that you can call "cool". Only a few
clicks on Netlify and I have a new home for my website - for free, generated automatically
from my git repository. And not only does the Hugo theme I'm using understand mathematical Formulas...

$$\frac{a}{b} = \frac{c}{d}$$

...it can also draw diagrams easily.

```mermaid
graph TD;
  A-->B;
  A-->C;
  B-->D;
  C-->D;
```

Not everything in the past was better. What a great thing to see.

