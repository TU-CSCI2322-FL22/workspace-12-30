
class  Show a  where
    {-# MINIMAL showsPrec | show #-}
    showsPrec :: Int    -> a      -> ShowS

    show      :: a   -> String

    showList  :: [a] -> ShowS

    showsPrec _ x s = show x ++ s
    show x          = shows x ""
    showList ls   s = showList__ shows ls s

