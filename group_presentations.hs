import qualified Data.Set as Set  

type Variable = Char
type Expression = String

data Presentation = Presentation { generators :: Set.Set Variable
                                 , relators :: Set.Set Expression }

