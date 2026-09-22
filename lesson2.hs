module MyComplex where

data MyComplex = MyComplex{
  real :: Float,
  mean :: Float
}  

instance Show MyComplex where
  show (MyComplex r m) = show r ++ "+" ++ show m ++ "i" 

instance Num MyComplex where
  (MyComplex real1 mean1) + (MyComplex real2 mean2) = MyComplex (real1 + real2) (mean1 + mean2)

  (MyComplex real1 mean1) - (MyComplex real2 mean2) = MyComplex (real1 - real2) (mean1 - mean2)

  (MyComplex real1 mean1) * (MyComplex real2 mean2) = MyComplex (real1*real2 - mean1*mean2) (real1*mean2+mean1*real1)  

  negate (MyComplex real mean) = MyComplex (-real) (-mean)
instance Fractional MyComplex where
  (MyComplex real1 mean1) / (MyComplex real2 mean2) = MyComplex ((real1*real2 + mean1*mean2)/(real2*real2+mean2*mean2)) ((real2*mean1 - real1*mean2)/(real2*real2+mean2*mean2))  
  

instance Eq MyComplex where
  (MyComplex real1 mean1) == (MyComplex real2 mean2) = (real1 == real2) && (mean1 == mean2)