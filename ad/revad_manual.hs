add :: Double -> Double -> (Double, Double -> (Double, Double))
add x y = (x + y, \s -> (s, s))

mul :: Double -> Double -> (Double, Double -> (Double, Double))
mul x y = (x * y, \s -> (y * s, x * s))

f' :: Double -> Double
f' x = let (y, y') = add x 1
           (y2, y2') = mul y y
           (y4, y4') = mul y2 y2
           (y8, y8') = mul y4 y4
           (z, z') = mul y8 y2
           zs = 1
           (y8s, y2s0) = z' zs
           (y4s0, y4s1) = y8' y8s
           y4s = y4s0 + y4s1 -- 複数回使った変数はsensitivityを足し合わせる
           (y2s1, y2s2) = y4' y4s
           y2s = y2s0 + y2s1 + y2s2
           (ys0, ys1) = y2' y2s
           ys = ys0 + ys1
           (xs, _) = y' ys
       in xs
