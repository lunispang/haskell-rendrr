{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE ImpredicativeTypes #-}

type Vec2 = (Double, Double)

type Vec3 = (Double, Double, Double)

type Triangle2D = (Vec2, Vec2, Vec2)

type Triangle3D = (Vec3, Vec3, Vec3)

v2Sub :: Vec2 -> Vec2 -> Vec2
v2Sub (x1, y1) (x2, y2) = (x1 - x2, y1 - y2)

v2Add :: Vec2 -> Vec2 -> Vec2
v2Add (x1, y1) (x2, y2) = (x1 + x2, y1 + y2)

v3Sub :: Vec3 -> Vec3 -> Vec3
v3Sub (x1, y1, z1) (x2, y2, z2) = (x1 - x2, y1 - y2, z1 - z2)

v3Add :: Vec3 -> Vec3 -> Vec3
v3Add (x1, y1, z1) (x2, y2, z2) = (x1 + x2, y1 + y2, z1 + z2)

rotatedCW :: Vec2 -> Vec2
rotatedCW (x, y) = (y, -x)

rotatedCCW :: Vec2 -> Vec2
rotatedCCW (x, y) = (-y, x)

dot :: Vec2 -> Vec2 -> Double
dot (x1, y1) (x2, y2) = x1 * y2 + x2 * y1

v3Proj :: Vec3 -> Vec2
v3Proj (x, y, z) = (x / z, y / z)

projectTri :: Triangle3D -> Triangle2D
projectTri (a, b, c) = (v3Proj a, v3Proj b, v3Proj c)

triangleSign :: Triangle2D -> Double
triangleSign (a, b, c) = dot (v2Sub b a) $ rotatedCW (v2Sub c a)

isCw :: Triangle2D -> Bool
isCw triangle = triangleSign triangle > 0

isCcw :: Triangle2D -> Bool
isCcw triangle = triangleSign triangle < 0

inCwTriangle2D :: Triangle2D -> Vec2 -> Bool
inCwTriangle2D (a, b, c) v = isCw (a, b, v) && isCw (b, c, v) && isCw (c, a, v)

inCcwTriangle2D :: Triangle2D -> Vec2 -> Bool
inCcwTriangle2D (a, b, c) v = isCcw (a, b, v) && isCcw (b, c, v) && isCcw (c, a, v)

rep :: Int -> a -> [a]
rep n x =
  if n == 1 then [x] else x : rep (n - 1) x

screen :: Int -> Int -> [[Char]]
screen w h = rep h $ rep w ' '

printScreen :: [[Char]] -> IO ()
printScreen [] = putStrLn ""
printScreen [x :: [Char]] = putStrLn x
printScreen (x : xs) = do
  putStrLn x
  printScreen xs

enumerate2D :: [[Char]] -> [[(Integer, Integer, Char)]]
enumerate2D x = fillEnumeration2D $ zip [0 ..] $ map (zip [0 ..]) x

fillEnumeration2D :: [(Integer, [(Integer, Char)])] -> [[(Integer, Integer, Char)]]
fillEnumeration2D = map (\(x, l) -> map (\(i, c) -> (x, i, c)) l)

idxToScreen :: Integer -> (Integer, Integer) -> Vec2
idxToScreen invScale (i, j) =
  let iF = realToFrac i
      jF = realToFrac j
      sF = realToFrac invScale
   in (iF / sF, jF / sF / 2)

-- removeEnumeration2D :: [[(Integer, Integer, Char)]] -> [[Char]]
-- removeEnumeration2D = map (map (\(_, _, c) -> c))

renderTriToScreen :: [[Char]] -> Char -> Triangle3D -> [[Char]]
renderTriToScreen scr c tri =
  map
    ( map
        ( \(i, j, o) ->
            if inCwTriangle2D
              (projectTri tri)
              (idxToScreen 2 (i, j))
              then c
              else o
        )
    )
    (enumerate2D scr)

main :: IO()
main = do
  let triangle = ((0, 1, 3), (1, 0, 3), (0, 0, 5)) :: Triangle3D
  let scr = renderTriToScreen (screen 20 10) '#' triangle
  printScreen scr
