data BookInfo = Book Int String [String]
                deriving (Show)

data MagazineInfo = Magazine Int String [String]
                    deriving (Show)

myInfo = Book 9780135072455 "Algebra of Programming"
         ["Richard Bird","Oege de Moor"]

data BookReview = BookReview BookInfo CustomerID String

type CustomerID = Int
type ReviewBody = String

data BetterReview = BetterReview BookInfo CustomerID ReviewBody

type BookRecord = (BookInfo, BookReview)

type CardHolder = String
type CardNumber = String
type Address = [String]

data BillingInfo = CreditCard CardNumber CardHolder Address
                 | CashOnDelivery
                 | Invoice CustomerID
                   deriving (Show)

bookId      (Book id _     _      ) = id
bookTitle   (Book _  title _      ) = title
bookAuthors (Book _  _     authors) = authors

data Customer = Customer {
  customerID      :: CustomerID,
  customerName    :: String,
  customerAddress :: Address
} deriving (Show)

customer1 = Customer 238476 "bob" ["1 Main St","Minneapolis, MN 55414","USA"]
customer2 = Customer {
    customerID = 485679,
    customerAddress = ["2 Lamb St","St. Paul, MN 55???","USA"],
    customerName = "jonocule"
}
