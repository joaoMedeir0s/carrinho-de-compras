module Promocao
(
    calcularDesconto,
    aplicarDesconto
) where

-- Calcula o valor do desconto de acordo com o valor da compra
calcularDesconto :: Double -> Double
calcularDesconto total
    | total >= 1000 = total * 0.10
    | total >= 500  = total * 0.05
    | otherwise     = 0

-- Retorna o valor final após o desconto
aplicarDesconto :: Double -> Double
aplicarDesconto total =
    total - calcularDesconto total