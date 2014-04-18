data Direction = L | R deriving (Eq, Show)
data SyntacticCategory = N | NP | S | Yield SyntacticCategory SyntacticCategory Direction deriving (Show, Eq)

leftYield :: SyntacticCategory -> SyntacticCategory -> SyntacticCategory
leftYield i o = Yield i o L

rightYield :: SyntacticCategory -> SyntacticCategory -> SyntacticCategory
rightYield o i = Yield i o R

applyCategory :: SyntacticCategory -> SyntacticCategory -> Direction -> Maybe SyntacticCategory
applyCategory baseCategory (Yield i o d) direction
  | d == direction && baseCategory == i = Just o
  | otherwise = Nothing

leftApplyCategory :: SyntacticCategory -> SyntacticCategory -> Maybe SyntacticCategory
leftApplyCategory baseCategory receiver = applyCategory baseCategory receiver L

rightApplyCategory :: SyntacticCategory -> SyntacticCategory -> Maybe SyntacticCategory
rightApplyCategory receiver baseCategory = applyCategory baseCategory receiver R

example = (rightYield (leftYield NP S) NP) -- (NP\S)/NP

--rightApplyCategory example NP --=> (leftYield NP S)

