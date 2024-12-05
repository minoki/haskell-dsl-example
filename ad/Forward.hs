type Forward a = (a, a) -- primal, tangent

constant :: Double -> Forward Double
constant x = (x, 0)

add :: Forward Double -> Forward Double -> Forward Double
add (x, xt) (y, yt) = (x + y, xt + yt)

mul :: Forward Double -> Forward Double -> Forward Double
mul (x, xt) (y, yt) = (x * y, x * yt + y * xt)

f :: Forward Double -> Forward Double
f x = let y = add x (constant 1)
          y2 = mul y y
          y4 = mul y2 y2
          y8 = mul y4 y4
      in mul y8 y2

f' :: Double -> Double
f' x = snd (f (x, 1))
