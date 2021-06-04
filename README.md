# covering

Assumptions:
===================

1. Events are represented by 2-tuple of integer i.e (Int, Int)
2. The sequence of events is assumed to be an infinite one, something that is a infinitely lived and infinitely long sequence
3. Time here is taken to be represented by the Natural Number line
3. The output of the main structure is a tuple that contains a solution and a data-structure that can be added to incrementally to obtain further results


Main gist of approach
============================

1. I split the time - line (natural number line) in a disjoint way
   and call these segments covers.

2. These covers represent intersections between all the events that have been streamed through.


3.Any new event that streams in is checked to see if intersects with these covers and this can one of three ways
    i. no intersection, event is added as  new cover
    ii. it does intersect and is the union of already existing covers (remember the covers are disjoint)
    iii. it overlaps with a cover in which case we split the existing cover and in addition add a new one if necessary

I use a hash map to keep track of the covers and the events whose intersection it represents,
and modify the hash map for each of the three ways an event-cover intersection is handled


The point of the Red-Black Tree
==============================
To keep the covers in a sorted fashion and handle breaking (and searching) and insertion of new covers efficient in terms of run-time


Things that could be improved
===============================
1. Testing
2. Code could be cleaner and more elegant
3. More testing

  