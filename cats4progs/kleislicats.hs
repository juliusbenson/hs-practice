import Data.Char

type Writer a = (a, String)

(>=>) :: (a -> Writer b) -> (b -> Writer c) -> (a -> Writer c)
m1 >=> m2 = \x -> 
    let (y, s1) = m1 x
        (z, s2) = m2 y
    in  (z, s1 ++ s2)

return :: a -> Writer a
return x = (x, "")

upCase :: String -> Writer String
upCase x = (map toUpper x, "upCase'd")

toWords :: String -> Writer [String]
toWords x = (words x, "toWords'd")
