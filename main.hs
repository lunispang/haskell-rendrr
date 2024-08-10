-- type Vec2 = (Float, Float)
--
-- type Vec3 = (Float, Float, Float)
--
-- type Triangle3D = (Vec3, Vec3, Vec3)
--
-- type Triangle2D = (Vec2, Vec2, Vec2)

v2Sub (x1, y1) (x2, y2) = (x1 - x2, y1 - y2)

v2Add (x1, y1) (x2, y2) = (x1 + x2, y1 + y2)

v3Sub (x1, y1, z1) (x2, y2, z2) = (x1 - x2, y1 - y2, z1 - z2)

v3Add (x1, y1, z1) (x2, y2, z2) = (x1 + x2, y1 + y2, z1 + z2)

rotatedCW (x, y) = (y, -x)

rotatedCCW (x, y) = (-y, x)

dot (x1, y1) (x2, y2) = x1 * y2 + x2 * y1

v3Proj (x, y, z) = (x / z, y / z)

projectTri (a, b, c) = (v3Proj a, v3Proj b, v3Proj c)

triangleSign (a, b, c) = dot (v2Sub b a) $ rotatedCW (v2Sub c a)

isCw triangle = triangleSign triangle > 0

isCcw triangle = triangleSign triangle < 0

inCwTriangle2D (a, b, c) v = isCw (a, b, v) && isCw (b, c, v) && isCw (c, a, v)

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
fillEnumeration2D = map (\ (x, l) -> map (\ (i, c) -> (x, i, c)) l)

idxToFloatScreen :: Float -> (Integer, Integer) -> (Float, Float)
idxToFloatScreen scale (i, j) =
  let iF = fromIntegral i :: Float
      jF = fromIntegral j :: Float
      in (iF * scale, jF * scale / 2)

renderTriToScreen :: [[Char]] -> Char -> a -> [[Char]]
renderTriToScreen scr chr tri =
  let scrEnum = enumerate2D scr
      projTri = projectTri tri
  in [['0']]

main = do
  let triangle = ((0, 1, 2), (1, 0, 2), (1, 1, 3))

  putStrLn "Test"
