data Maybe a = Nothing | Just a Deriving(Eq, Ord)
ppi :: (Num a, b, c) => (a, b, c) -> Int
ppi (inci, width, height)
    = floor(sqrt(width**2 + height**2) / inci)

