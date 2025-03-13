import Data.ByteString (count)
import Distribution.Simple.Build (build)

--- Exercise 01

type Width = Int

type Height = Int

data Building
  = Rectangle Width Height Building
  | Tip Width Height
  | Split Building Building
  deriving (Eq, Show)

bspBuilding =
  Rectangle
    50
    20
    ( Split
        ( Rectangle
            20
            15
            ( Split
                ( Rectangle
                    10
                    20
                    ( Rectangle
                        8
                        18
                        (Tip 8 14)
                    )
                )
                ( Rectangle
                    8
                    17
                    (Tip 8 14)
                )
            )
        )
        ( Rectangle
            20
            15
            ( Split
                ( Rectangle
                    8
                    17
                    (Tip 8 14)
                )
                ( Rectangle
                    10
                    20
                    ( Split
                        (Tip 5 17)
                        (Tip 5 17)
                    )
                )
            )
        )
    )

-- a
numParts :: Building -> Int
numParts (Tip _ _) = 1
numParts (Rectangle _ _ b) = 1 + numParts b
numParts (Split b1 b2) = numParts b1 + numParts b2

-- b
foldBuilding :: (Width -> Height -> t -> t) -> (Width -> Height -> t) -> (t -> t -> t) -> Building -> t
foldBuilding fRectangle fTip fSplit building = case building of
  Rectangle w h b -> fRectangle w h (foldBuilding fRectangle fTip fSplit b)
  Tip w h -> fTip w h
  Split b1 b2 ->
    fSplit
      (foldBuilding fRectangle fTip fSplit b1)
      (foldBuilding fRectangle fTip fSplit b2)

-- c
maxHeight :: Building -> Height
maxHeight = foldBuilding (\_ h a -> h + a) (\_ h -> h) max

-- d
numPeaks :: Building -> Int
numPeaks = foldBuilding (\_ _ a -> a) (\_ _ -> 1) (+)

-- e
wellformed :: Building -> Bool
wellformed building = snd (foldBuilding (\w _ a -> (w, snd a && w >= fst a)) (\w _ -> (w, True)) (\a b -> (fst a + fst b, snd a && snd b)) building)

--- Exercise 02

type Feature = String

type Min = Int

type Max = Int

data FeatureDiagram
  = Feature Feature [FeatureDiagram]
  | NoGroup ([FeatureDiagram], [FeatureDiagram])
  | OrGroup Min Max [FeatureDiagram]
  | AltGroup [FeatureDiagram]
  deriving (Eq, Show)

bspFD =
  Feature
    "Car"
    [ NoGroup
        ( [ Feature "Carbody" [],
            Feature
              "Gearbox"
              [ AltGroup
                  [ Feature "Manual" [],
                    Feature "Automatic" []
                  ]
              ]
          ],
          [ Feature
              "Radio"
              [ NoGroup
                  ( [],
                    [ Feature
                        "Ports"
                        [ OrGroup
                            1
                            3
                            [ Feature "USB" [],
                              Feature "CD" [],
                              Feature "Bluetooth" []
                            ]
                        ],
                      Feature "Navi" []
                    ]
                  )
              ]
          ]
        )
    ]

-- a
foldFD :: (Feature -> [b] -> b) -> (([b], [b]) -> b) -> (Min -> Max -> [b] -> b) -> ([b] -> b) -> FeatureDiagram -> b
foldFD fFeature fNoGroup fOrGroup fAltGroup fd = case fd of
  Feature name children -> fFeature name (map rec children)
  NoGroup (mandatory, optional) -> fNoGroup (map rec mandatory, map rec optional)
  OrGroup min max children -> fOrGroup min max (map rec children)
  AltGroup children -> fAltGroup (map rec children)
  where
    rec = foldFD fFeature fNoGroup fOrGroup fAltGroup

-- b
featureList :: FeatureDiagram -> [Feature]
featureList = foldFD (\f a -> f : concat a) (\(m, o) -> concat m ++ concat o) (\_ _ a -> concat a) concat

-- c
fdStats :: FeatureDiagram -> (Int, Int, Int)
fdStats = foldFD fFeature fNoGroup fOrGroup fAltGroup
  where
    fFeature _ childCounts = sumTriples childCounts

    fNoGroup (mandatory, optional) =
      let (mCounts, oCounts) = (sumTriples mandatory, sumTriples optional)
       in (fst3 mCounts + length mandatory, snd3 oCounts + length optional, thrd3 mCounts + thrd3 oCounts)

    fOrGroup _ _ childCounts = (mTotal, oTotal, gTotal + length childCounts)
      where
        (mTotal, oTotal, gTotal) = sumTriples childCounts

    fAltGroup childCounts = (mTotal, oTotal, gTotal + length childCounts)
      where
        (mTotal, oTotal, gTotal) = sumTriples childCounts

    sumTriples = foldl sumTriple (0, 0, 0)
    sumTriple (m1, o1, g1) (m2, o2, g2) = (m1 + m2, o1 + o2, g1 + g2)

fst3 (x, _, _) = x

snd3 (_, y, _) = y

thrd3 (_, _, z) = z

-- d
simplify :: FeatureDiagram -> FeatureDiagram
simplify = foldFD fFeature fNoGroup fOrGroup fAltGroup
  where
    fFeature name children = Feature name (map simplify children)

    fNoGroup (mandatory, optional) = NoGroup (map simplify mandatory, map simplify optional)

    fOrGroup min max children
      | min == 1 && max == 1 = AltGroup (map simplify children)
      | otherwise = OrGroup min max (map simplify children)

    fAltGroup children
      | length children == 1 = head (map simplify children)
      | otherwise = AltGroup (map simplify children)

-- e
relax :: FeatureDiagram -> FeatureDiagram
relax = foldFD fFeature fNoGroup fOrGroup fAltGroup
  where
    fFeature name children = Feature name (map relax children)
    fNoGroup (mandatory, optional) = NoGroup ([], map relax mandatory ++ map relax optional)
    fOrGroup min max children = OrGroup min max (map relax children)
    fAltGroup children = OrGroup 1 (length children) (map relax children)