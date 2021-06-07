-- Prática 05 de Haskell
-- Nome: Luiz Felipe Cavalheiro

bmi :: Float -> Float -> String
bmi peso altura = 
  let imc = peso / altura^2 
  in 
    | imc <= 18.5 = "ABAIXO" 
    | imc < 30.0 = "NORMAL" 
    | otherwise = "ACIMA"


bmi' :: Float -> Float -> String
bmi' peso altura
  | imc <= 18.5 = "ABAIXO"
  | imc < 30.0 = "NORMAL"
  | otherwise  = "ACIMA"
  where imc = peso / altura^2


cpfValid :: [Int] -> Bool
cpfValid cpf = dv1 == cpf !! 9 && dv2 == cpf !! 10
  where digits = take 9 cpf
        dv1 = cpfDV digits [10,9..]
        dv2 = cpfDV (digits ++ [dv1]) [11,10..]
  

cpfDV :: [Int] -> [Int] -> Int
cpfDV digits mults = 
  let expr = (sum $ zipWith (*) digits mults) `mod` 11
  in if expr < 2 then 0 else 11-expr


andTable :: [(Bool, Bool, Bool)]
andTable =     
  let list = [True,False]
  in [(a,b,a && b) | a <- list, b <- list]
