module HW1.TowersOfHanoi (hanoi) where

type Peg = Char
type Move = (Peg, Peg)

hanoi :: Integer -> Peg -> Peg -> Peg -> [Move]
hanoi discs p1 p2 p3
  | discs < 1  = []
  | discs == 1 = [(p1, p2)]
  | otherwise  = [(p1, p3), (p1, p2), (p3, p2)] ++ hanoi (discs - 2) p1 p2 p3
