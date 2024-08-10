{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE ImpredicativeTypes #-}

type Vec2 a = forall a. (Fractional a) => (a, a)

type Vec3 a = forall a. (Fractional a) => (a, a, a)

type Triangle2D a = forall a. (Fractional a) => (Vec2 a, Vec2 a, Vec2 a)

type Triangle3D a = forall a. (Fractional a) => (Vec3 a, Vec3 a, Vec3 a)

v2Sub :: Vec2 a -> Vec2 a -> Vec2 a
v2Sub (x1, y1) (x2, y2) = (x1 - x2, y1 - y2)

v2Add :: Vec2 a -> Vec2 a -> Vec2 a
v2Add (x1, y1) (x2, y2) = (x1 + x2, y1 + y2)

v3Sub :: Vec3 a -> Vec3 a -> Vec3 a
v3Sub (x1, y1, z1) (x2, y2, z2) = (x1 - x2, y1 - y2, z1 - z2)

v3Add :: Vec3 a -> Vec3 a -> Vec3 a
v3Add (x1, y1, z1) (x2, y2, z2) = (x1 + x2, y1 + y2, z1 + z2)

rotatedCW :: Vec2 a -> Vec2 a
rotatedCW (x, y) = (y, -x)

rotatedCCW :: Vec2 a -> Vec2 a
rotatedCCW (x, y) = (-y, x)

dot :: (Fractional a) => Vec2 a -> Vec2 a -> a
dot (x1, y1) (x2, y2) = x1 * y2 + x2 * y1

v3Proj :: Vec3 a -> Vec2 a
v3Proj (x, y, z) = (x / z, y / z)

projectTri :: Triangle3D a -> Triangle2D a
projectTri (a, b, c) = (v3Proj a, v3Proj b, v3Proj c)

triangleSign :: (Fractional a) => Triangle2D a -> a
triangleSign (a, b, c) = dot (v2Sub b a) $ rotatedCW (v2Sub c a)

isCw :: Triangle2D a -> Bool
isCw triangle = triangleSign triangle > 0

isCcw :: Triangle2D a -> Bool
isCcw triangle = triangleSign triangle < 0

inCwTriangle2D :: Triangle2D a -> Vec2 a -> Bool
inCwTriangle2D (a, b, c) v = isCw (a, b, v) && isCw (b, c, v) && isCw (c, a, v)

inCcwTriangle2D :: Triangle2D a -> Vec2 a -> Bool
inCcwTriangle2D (a, b, c) v = isCcw (a, b, v) && isCcw (b, c, v) && isCcw (c, a, v)

rep :: Int -> a -> [a]
rep n x =
  if n == 1 then [x] else x : rep (n - 1) x

screen :: Int -> Int -> [[Char]]
screen w h = rep h $ rep w ' '

printScreen :: [[Char]] -> IO ()
printScreen [x] = print x
printScreen (x : xs) = do
  print x
  printScreen xs

enumerate2D :: [[Char]] -> [[(Integer, Integer, Char)]]
enumerate2D x = fillEnumeration2D $ zip [0 ..] $ map (zip [0 ..]) x

fillEnumeration2D :: [(Integer, [(Integer, Char)])] -> [[(Integer, Integer, Char)]]
fillEnumeration2D = map (\(x, l) -> map (\(i, c) -> (x, i, c)) l)

idxToFloatScreen :: Float -> (Integer, Integer) -> (Float, Float)
idxToFloatScreen scale (i, j) =
  let iF = fromIntegral i :: Float
      jF = fromIntegral j :: Float
   in (iF * scale, jF * scale / 2)

renderTriToScreen :: [[Char]] -> Char -> Triangle3D a -> [[Char]]
renderTriToScreen scr chr tri =
  let scrEnum = enumerate2D scr
      projTri = projectTri tri
   in [['0']]

main = do
  let triangle = ((0, 1, 2), (1, 0, 2), (1, 1, 3))

  putStrLn "Test"
