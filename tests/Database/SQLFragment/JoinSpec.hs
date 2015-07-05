{-# LANGUAGE DataKinds, OverloadedStrings #-}
module Database.SQLFragment.JoinSpec where 
-- standard
import Data.List(intercalate)
-- third-party
import Test.Hspec
import Test.HUnit
-- local
import Database.SQLFragment.Join
import Database.SQLFragment

-- * Basic DB schema. A customer can have multiple invoices
-- and multiples orders, each of them referencing to a products.
-- The basic join graph is a diamond graph between `customer`
-- and `item`.
-- customer --+--> order    --> orderDetail  --+--> item
--            |                                |
--            +--> invoice --> invoiceDetail --+ 
-- The graph should be oriented because we don't want automatic join
-- between `orderDetail` , `invoiceDetail` via `item`          

type Date = String -- easier to test
customerName = "customer.name" :: SQLFragment '[String] '[]
customerId = "customer.id" :: SQLFragment '[Int] '[]

itemName = "item.name" :: SQLFragment '[String] '[]
itemId = "item.id" :: SQLFragment '[Int] '[]
itemDescription = "item.description" :: SQLFragment '[Maybe String] '[]

-- Orders
orderId = "order.id" :: SQLFragment '[Int] '[]
orderCustomer = "order.customer_id" :: SQLFragment '[Int] '[]
orderDate = "order.date" :: SQLFragment '[Date] '[]

orderDetailId = "order_detail.id" :: SQLFragment '[Int] '[]
orderDetailOrderId = "order_detail.order_id" :: SQLFragment '[Int] '[]
orderDetailItemId = "order_detail.item_id" :: SQLFragment '[Int] '[]
orderDetailQuantity = "order_detail.quantity" :: SQLFragment '[Int] '[]
orderDetailPrice = "order_detail.price" :: SQLFragment '[Double] '[]


-- Invoices
invoiceId = "invoice.id" :: SQLFragment '[Int] '[]
invoiceCustomer = "invoice.customer_id" :: SQLFragment '[Int] '[]
invoiceDate = "invoice.date" :: SQLFragment '[Date] '[]

invoiceDetailId = "invoice_detail.id" :: SQLFragment '[Int] '[]
invoiceDetailInvoiceId = "invoice_detail.invoice_id" :: SQLFragment '[Int] '[]
invoiceDetailItemId = "invoice_detail.item_id" :: SQLFragment '[Int] '[]
invoiceDetailQuantity = "invoice_detail.quantity" :: SQLFragment '[Int] '[]
invoiceDetailPrice = "invoice_detail.price" :: SQLFragment '[Double] '[]

customerGraph :: [Join]
customerGraph = customerId !<-*>! orderCustomer
    ++ orderId !<->! orderDetailOrderId
    ++ orderDetailItemId !->! itemId
    
    ++ customerId !<-*>! invoiceCustomer
    ++ invoiceId !<->! invoiceDetailInvoiceId
    ++ invoiceDetailItemId !->! itemId

-- **  Simpler Graph
a = "a.x" :: SQLFragment '[String] '[]
b = "b.x" :: SQLFragment '[String] '[]
c = "c.x" :: SQLFragment '[String] '[]
d = "d.x" :: SQLFragment '[String] '[]
aGraph =
    a !<->! b
    ++ a !<->! c
    ++ b !->! d
    ++ c !->! d

-- * Common operator
-- | Normalize white spaces.
normalizeWs :: String -> String
normalizeWs s = intercalate " " $ words s
infix 0 !=@
q !=@ s = normalizeWs s @=? normalizeWs (toSelectQuery q)

compareWithJoin :: [Join] -> SQLFragment e p -> String -> Assertion
compareWithJoin g q s = case q `autoJoin` g of 
    Left q' ->  q' !=@ s
    Right q' -> q' !=@ s

spec :: Spec
spec = do
    describe "#with customer schema" $do
        let q !==@ s = compareWithJoin customerGraph q s
        context "perform joins through order" $do
            it "in normal order" $do
                (customerName !&! orderDetailQuantity !&! itemName) !==@
                    "SELECT customer.name\
                         \, order_detail.quantity\
                         \, item.name\
                   \ FROM customer\
                   \ LEFT JOIN order ON ((customer.id)=(order.customer_id))\
                   \ JOIN order_detail ON ((order.id)=(order_detail.order_id))\
                   \ JOIN item ON ((order_detail.item_id)=(item.id))\
                   \"
    
            it "whichever the order" $do
                (customerName !&! itemName !&! orderDetailQuantity ) !==@
                    "SELECT customer.name\
                         \, item.name\
                         \, order_detail.quantity\
                   \ FROM customer\
                   \ LEFT JOIN order ON ((customer.id)=(order.customer_id))\
                   \ JOIN order_detail ON ((order.id)=(order_detail.order_id))\
                   \ JOIN item ON ((order_detail.item_id)=(item.id))\
                   \"

        context "perform joins through invoice" $do
            it "in normal order" $do
                (customerName !&! invoiceDetailQuantity !&! itemName) !==@
                    "SELECT customer.name\
                         \, invoice_detail.quantity\
                         \, item.name\
                   \ FROM customer\
                   \ LEFT JOIN invoice ON ((customer.id)=(invoice.customer_id))\
                   \ JOIN invoice_detail ON ((invoice.id)=(invoice_detail.invoice_id))\
                   \ JOIN item ON ((invoice_detail.item_id)=(item.id))\
                   \"

        context "#ambiguous case" $do
            it "uses order" $do
                (customerName !&! itemName) !==@
                    "SELECT customer.name\
                         \, item.name\
                   \ FROM customer\
                   \ LEFT JOIN order ON ((customer.id)=(order.customer_id))\
                   \ JOIN order_detail ON ((order.id)=(order_detail.order_id))\
                   \ JOIN item ON ((order_detail.item_id)=(item.id))\
                   \"
        context "#backward graph" $do
            it "uses bidirectional joins (customer) instead of shortest path (item)" $do
                (orderDetailItemId !&! invoiceDetailItemId) !==@
                    "SELECT order_detail.item_id, invoice_detail.item_id\
                   \ FROM order_detail\
                   \ JOIN order ON ((order_detail.order_id)=(order.id))\
                   \ RIGHT JOIN customer ON ((order.customer_id)=(customer.id))\
                   \ LEFT JOIN invoice ON ((customer.id)=(invoice.customer_id))\
                   \ JOIN invoice_detail ON ((invoice.id)=(invoice_detail.invoice_id))\
                   \"
    describe "#abcd diamond" $do
        let q !==@ s = compareWithJoin aGraph q s
        it "a -> (b ->) d" $do
            (a !&! d) !==@ "SELECT a.x, d.x \
                          \FROM a \
                          \JOIN b ON ((a.x)=(b.x)) \
                          \JOIN d ON ((b.x)=(d.x))"
        it "a -> b -> d" $do
            (a !&! b !&! d) !==@ "SELECT a.x, b.x, d.x \
                          \FROM a \
                          \JOIN b ON ((a.x)=(b.x)) \
                          \JOIN d ON ((b.x)=(d.x))"
        it "b -> a -> d" $do
            (b !&! a !&! d) !==@ "SELECT b.x, a.x, d.x \
                          \FROM b \
                          \JOIN a ON ((b.x)=(a.x)) \
                          \JOIN d ON ((b.x)=(d.x))"
        it "a -> c -> d" $do
            (a !&! c !&! d) !==@ "SELECT a.x, c.x, d.x \
                          \FROM a \
                          \JOIN c ON ((a.x)=(c.x)) \
                          \JOIN d ON ((c.x)=(d.x))"
        it "c -> a -> d" $do
            (c !&! a !&! d) !==@ "SELECT c.x, a.x, d.x \
                          \FROM c \
                          \JOIN a ON ((c.x)=(a.x)) \
                          \JOIN d ON ((c.x)=(d.x))"
    describe "#using alias" $ do
        let q !==@ s = compareWithJoin aliasedGraph q s
            a2 = from "(SELECT MIN(x) as x FROM a) AS a2"
            a2x = ("a2.x" :: SQLFragment '[String] '[]) !&! a2
            aliasedGraph = a2x !<->! a
        it "in fragment" $do
            (a !&! a2x) !==@ "SELECT a.x, a2.x \
                    \FROM a \
                    \JOIN (SELECT MIN(x) as x FROM a) AS a2 ON ((a.x)=(a2.x))"
        it "in string" $do
            (a !&! "a2.x") !==@ "SELECT a.x, a2.x \
                    \FROM a \
                    \JOIN (SELECT MIN(x) as x FROM a) AS a2 ON ((a.x)=(a2.x))"

                
            
                 
    
                
                
            
            

main :: IO ()
main = hspec spec
