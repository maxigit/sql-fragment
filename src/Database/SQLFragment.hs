{-| SQLFragment is a type safe SQL combinator based on the idea that, a SQL query

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
-}

module Database.SQLFragment (
-- * Example
-- $example

-- * Documentation

module Database.SQLFragment.SQLFragment
, module Database.SQLFragment.Operators
, module Database.SQLFragment.Dimensional
, module Database.SQLFragment.Join
) where
import Database.SQLFragment.SQLFragment
import Database.SQLFragment.Operators
import Database.SQLFragment.Dimensional
import Database.SQLFragment.Join

{- $example
Let's say we have a table of customers, products, and orders, joining a customer to n products.
I want to display in table the list of the customer which ordered the product 'blue T-shirt'.

With SQLFragment, supposing I have defined `email` and `blue` fragments so that

@
\> toSelectQuery email
\"SELECT email FROM customers\"

\> toSelectQuery blue
\"FROM product WHERE description = 'blue T-shirt'\"
@

and the join graph as been properly set up in `joins`.

I can simply combine those two fragment using `<>`.

@
\> toSelectQuery \$ email <> blue !\@! joins
\"SELECT email 
FROM customers
JOIN orders ON (customers.id = customer_id)
JOIN products ON (products.id = product_id)
WHERE products.description = 'blue T-shirt'
\"
@
-}


