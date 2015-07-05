# Overview
SQLFragment is a type safe SQL combinator based on the idea that, a SQL query

    * is a monoid

    * joins can be deduced automatically from an join graph.

SQLFragment main intent is to allow to build easily complex query by reusing and combining pre-made fragments (which can be typed or typeless).
This is especially useful when building reporting tools, when a lot of queries are similar and the results 
are either table or charts. In that case, query output can be used \"raw\" (.i.e a list of tuple or equivalent) and don't need to be mapped to any complex data type.
Unlike many other SQL package, which make it hard to combine `SQLFragment` and `String`, SQLFragment makes it easy to write raw SQL if needed.
Its purpose is to help write query quickly not make developper life hard.
We trust the developper to not use \"unsafe\" string.

SQLFragment also provide support for dimensional units, `HList` records and automatic fragments generation from a database. The fragments generation use a space separated values file which can be generated from the database (see corresponding backend).

For more details look at the Database.SQLFragment.SQLFragment and Database.SQLFragment.Operators.
