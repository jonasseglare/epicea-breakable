# epicea-breakable
Dealing with exceptional situations not using exceptions, in Clojure.

epicea-breakable is a macro-based alternative to exceptions for locally diverting control flow in a Clojure expression when something unexpected happens. Functions that throw exceptions cause a great deal of confusion, because there is always the question: Who is responsible for catching the exception? With return values, we do not have that problem, because whoever gets the return value is responsible for dealing with it. This library facilitates writing programs that only pass values around and don't throw exceptions.

## Introduction
epicea-breakable introduces a ```breakable``` macro that will surround the code where we would like to handle special situations. For instance, if we want to add variables together, but only if they are numbers, we can write
```
(breakable (+ (expect number? a :a-not-a-number)
              (expect number? b :b-not-a-number)))
```
The breakable macro will rewrite this into something like this:
```
(if (number? a) 
  (if (number? b) 
    (clojure.core/let [G__1374 (+ a b)] 
       G__1374) 
    :b-not-a-number) 
  :a-not-a-number)
```
which is the sort of code that we would be writing manually if we were not using exceptions.

See the unit tests for more examples.

## Adding it the leiningen project
...amounts to adding this line to the dependencies
```
[epicea/breakable "0.1.0-SNAPSHOT"]
```

## Licence

Copyright © 2016 Jonas Östlund

Distributed under the Eclipse Public License either version 1.0 or (at
your option) any later version.