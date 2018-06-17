module Music where

import Data.List ( delete, elemIndex, nub, sort, sortOn, find )
import Data.Maybe ( fromJust, isJust, catMaybes )
import qualified Data.Map as M ( fromList, Map(..), lookup, delete, assocs )

data Note = C | Cs | D | Eb | E | F | Fs | G | Ab | A | Bb | B deriving (Eq, Ord, Enum)

instance Show Note where
  show C = "C"
  show Cs = "C#"
  show D = "D"
  show Eb = "Eb"
  show E = "E"
  show F = "F"
  show Fs = "F#"
  show G = "G"
  show Ab = "Ab"
  show A = "A"
  show Bb = "Bb"
  show B = "B"

notes = [C ..]

fromNote :: Note -> Int
fromNote = fromJust . (`elemIndex` notes)

toNote :: Int -> Note
toNote = (notes !!) . (`mod` (length notes))

transposeBy :: Int -> Note -> Note
transposeBy interval = toNote . (+ interval) . fromNote

intervalBetween a b = ((fromNote b) - (fromNote a)) `mod` (length notes)
transpose oldRoot newRoot = transposeBy $ intervalBetween oldRoot newRoot

data ScaleType = Major | Minor | Diminished | WholeTone deriving (Eq, Enum)
scaleTypes = [Major ..]

nextIn :: Enum a => [a] -> a -> a
nextIn xs x = toEnum $ (fromEnum x + 1) `mod` (length xs)

data Scale = Scale Note ScaleType

instance Show ScaleType where
  show Major = "major"
  show Minor = "minor"
  show Diminished = "diminished"
  show WholeTone = "wholetone"

instance Show Scale where
  show (Scale n t) = (show n) ++ " " ++ (show t)

instance Eq Scale where
  (Scale n1 Major) == (Scale n2 Major) = n1 == n2
  (Scale n1 Minor) == (Scale n2 Minor) = n1 == n2
  (Scale n1 WholeTone) == (Scale n2 WholeTone) = fromNote n1 `mod` 2 == fromNote n2 `mod` 2
  (Scale n1 Diminished) == (Scale n2 Diminished) = fromNote n1 `mod` 3 == fromNote n2 `mod` 3
  _ == _ = False

showInKey :: Note -> Scale -> String
showInKey k s@(Scale r Minor) | k == (transposeBy (-1) r) = (show k) ++ " alt (" ++ show s ++ ")"
showInKey k s@(Scale r Diminished) | s == (Scale k Diminished) = show (Scale k Diminished)
showInKey k s@(Scale r Diminished) | s == (Scale (transposeBy 1 k) Diminished) = show (Scale (transposeBy 1 k) Diminished)
showInKey k s@(Scale r Diminished) | s == (Scale (transposeBy 2 k) Diminished) = show (Scale (transposeBy 2 k) Diminished)
showInKey k s@(Scale r WholeTone) | s == (Scale k WholeTone) = show (Scale k WholeTone)
showInKey k s@(Scale r WholeTone) | s == (Scale (transposeBy 1 k) WholeTone) = show (Scale (transposeBy 1 k) WholeTone)
showInKey k s@(Scale r Major) = case modeForScale s k of
                                  Just m -> show m ++ " (" ++ show s ++ ")"
                                  _ -> show s
showInKey _ s = show s

data ModeType = Ionian | Dorian | Phrygian | Lydian | Mixolydian | Aeolian | Locrian deriving (Enum, Show)
modeTypes = [Ionian ..]

data Mode = Mode Note ModeType

instance Show Mode where
  show (Mode n t) = (show n) ++ " " ++ (show t)

modeForScale :: Scale -> Note -> Maybe Mode
modeForScale s@(Scale k Major) root =
  case root `elemIndex` (scaleNotes s) of
    Just i -> Just $ Mode root $ modeTypes !! (i `mod` (length modeTypes))
    _ -> Nothing
mode _ _ = Nothing

inScale :: Scale -> Note -> Bool
inScale scale n = n `elem` (scaleNotes scale)

scaleNotes :: Scale -> [Note]
scaleNotes (Scale root scaleType) =
  (scanl (flip transposeBy) root) . scaleIntervals $ scaleType
  where
    scaleIntervals Major = [2, 2, 1, 2, 2, 2]
    scaleIntervals Minor = [2, 1, 2, 2, 2, 2]
    scaleIntervals Diminished = [1, 2, 1, 2, 1, 2, 1]
    scaleIntervals WholeTone = [2, 2, 2, 2, 2]

data ChordType = Maj | Maj7 | Dom7 | Dom7b5
               | Min | Min7 | MinMaj7 | Min7b5
               | Alt deriving (Eq, Enum)

data Chord = Chord Note ChordType deriving (Eq)

instance Show Chord where
  show (Chord r t) = (show r) ++ (chord t) where
    chord Maj = ""
    chord Maj7 = "M7"
    chord Dom7 = "7"
    chord Dom7b5 = "7b5"
    chord Min = "m"
    chord Min7 = "m7"
    chord MinMaj7 = "mM7"
    chord Min7b5 = "m7b5"
    chord Alt = "alt"

chordNotes (Chord root chordType) = (transposeTo root) . chordNotes' $ chordType
  where
    transposeTo = map . (transpose C)
    chordNotes' Maj = [C]
    chordNotes' Dom7 = [C, Bb]
    chordNotes' Maj7 = [C, B]
    chordNotes' Min = [C, Eb] 
    chordNotes' Dom7b5 = [C, Fs, Bb]
    chordNotes' Min7 = [C, Eb, Bb]
    chordNotes' MinMaj7 = [C, Eb, B]
    chordNotes' Min7b5 = [C, Eb, Fs, Bb]
    chordNotes' Alt = [C, E, Bb, Eb, Ab]

xs `containedIn` ys = all (`elem` ys) xs

scalesFor :: [Note] -> [Scale]
scalesFor [] = []
scalesFor notes =
  nub $ [ s | s <- scales, notes `containedIn` (scaleNotes s) ] where
    scales = [Scale r t | r <- [C ..], t <- [Major ..]]

scaleTypesFor :: Note -> [Note] -> [ScaleType]
scaleTypesFor root = map (\(Scale _ st) -> st) . filter (\s@(Scale r st) -> s == (Scale root st)) . scalesFor

scalesForRoot :: Note -> [Note] -> [Scale]
scalesForRoot root = filter (\s@(Scale r st) -> s == (Scale root st)) . scalesFor

chordsForRoot :: Note -> [Note] -> [Chord] 
chordsForRoot _ [] = []
chordsForRoot root notes = buildChords [] chords
  where
  buildChords :: [Chord] -> [Chord] -> [Chord]
  buildChords bcs [] = bcs
  buildChords bcs (t:cs) | (chordNotes t) `containedIn` notes && not (any (\c -> (chordNotes t) `containedIn` (chordNotes c)) bcs) = buildChords (t:bcs) cs
  buildChords bcs (t:cs) = buildChords bcs cs

  chords = sortOn ((0-) . length . chordNotes) [Chord root t | t <- [Maj ..]]

data ChordX = ChordX Note MajMinX (Maybe SeventhX) [AltX]

data MajMinX = MajX | MinX
instance Show MajMinX where
  show MinX = "m"
  show _ = ""

data AltX = FlatX Int | SharpX Int
instance Show AltX where
  show (FlatX p) = "b" ++ show p
  show (SharpX p) = "#" ++ show p

data SeventhX = Dom7X | Dom9X | Dom13X | Maj7X
instance Show SeventhX where
  show Dom7X = "7"
  show Dom9X = "9"
  show Dom13X = "13"
  show Maj7X = "M7"

instance Show ChordX where
  show (ChordX root majMin seventh alts) = (show root) ++ (show majMin) ++ (showSeventh seventh) ++ (concatMap show alts) where
    showSeventh (Just sth) = show sth
    showSeventh _ = ""

data Alteration = Flat | Sharp | Natural deriving (Eq, Ord, Show)

chordX :: [Note] -> Maybe ChordX
chordX [] = Nothing
chordX ns@(root:_) = Just $ toChord (M.fromList $ alterations (delete root $ nub ns)) where
  toChord :: M.Map Int Alteration -> ChordX
  toChord alts = ChordX root majMin seventh alts' where
    majMin | M.lookup 3 alts == Just Flat = MinX
           | otherwise = MajX

    seventh | M.lookup 7 alts == Just Natural = Just Maj7X
            | M.lookup 7 alts == Just Flat && M.lookup 13 alts == Just Natural = Just Dom13X
            | M.lookup 7 alts == Just Flat && M.lookup 9 alts == Just Natural = Just Dom9X
            | M.lookup 7 alts == Just Flat = Just Dom7X
            | otherwise = Nothing
    
    alts' = catMaybes $ map (\(p, a) -> case a of
                                          Flat -> Just $ FlatX p
                                          Sharp -> Just $ SharpX p) $ sortOn snd $ filter ((/= Natural) . snd) $ filter (\(p, _) -> p /= 3 && p /= 7) (M.assocs alts)

  note :: (Int, Alteration) -> Note
  note (pos, alt) = let n = (scaleNotes (Scale root Major)) !! ((pos - 1) `mod` 7)
                    in case alt of
                         Flat -> transposeBy (-1) n
                         Sharp -> transposeBy 1 n
                         _ -> n

  alterations :: [Note] -> [(Int, Alteration)]
  alterations ns = alts ns [] [ (3, Natural), (5, Natural), (7, Natural), (9, Natural), (11, Natural), (13, Natural)
                              , (3, Flat), (5, Sharp), (7, Flat), (9, Flat), (9, Sharp), (11, Sharp), (5, Flat), (13, Flat)]
    
  alts :: [Note] -> [(Int, Alteration)] -> [(Int, Alteration)] -> [(Int, Alteration)]
  alts [] as _ = as
  alts _ as [] = as
  alts ns as (p:pos) = let (ns', as') = findAlt ns as p
                       in alts ns' as' pos
    
  findAlt :: [Note] -> [(Int, Alteration)] -> (Int, Alteration) -> ([Note], [(Int, Alteration)])
  findAlt ns as p = case (find (== (note p)) ns, any ((== (fst p)) . fst) as) of
                      (Just n, False) -> (delete n ns, p:as)
                      _ -> (ns, as)

neighbours :: Scale -> Int -> [Scale]
neighbours scale dist =
  [s | s <- scales, distance' s <= dist]
  where
    distance' s = distance (scaleNotes scale) (scaleNotes s)
    scales = [Scale r t | r <- [C ..], t <- [Major ..]]

distanceMap :: [(Scale, Scale, Int)]
distanceMap =
  [(a, b, distance (scaleNotes a) (scaleNotes b)) | a <- scales, b <- scales]
  where
    scales = [Scale r t | r <- [C ..], t <- [Major ..]]

distance :: Ord a => [a] -> [a] -> Int
distance as bs =
  distance' (sort $ as) (sort $ bs) 0 where
    distance' [] [] d = d
    distance' [] (b:bs) d = distance' [] bs (d + 1)
    distance' (a:as) [] d = distance' as [] (d + 1)
    distance' (a:as) (b:bs) d
      | a == b = distance' as bs d
      | otherwise = minimum [ distance' (a:as) bs (d + 1)
                            , distance' as (b:bs) (d + 1)
                            , distance' as bs (d + 1)
                            ]

data ScaleChord = I | II | III | IV | V | VI | VII | VIII deriving (Eq, Enum, Show)
fromScaleChord = fromJust . (`elemIndex` [I ..])
toScaleChord = ([I ..] !!)

scalesForChord :: Chord -> [(Scale, ScaleChord)]
scalesForChord chord =
  [(s, fromJust p) | (s, p) <- scaleChords, isJust p]
  where
    scaleChords = [(s, scaleChordFor chord s) | s <- scales]
    scales = [Scale r t | r <- [C ..], t <- [Major ..]]

scaleChordFor chord@(Chord chroot chordType) scale@(Scale sroot scaleType) =
  if (chordNotes chord) `containedIn` (scaleNotes scale)
    then (toScaleChord <$> chroot `elemIndex` (scaleNotes scale))
    else Nothing

scaleCycle cycles gap startNote =
  scaleCycle' cycles startNote
  where
    scaleCycle' 0 n | n == startNote = []
    scaleCycle' c n | n == startNote = n : (scaleCycle' (c - 1) $ transposeBy gap n)
                    | otherwise = n : (scaleCycle' c $ transposeBy gap n)

indexify = zip [0..]
indexify2 =
  concat . collapseIndexes . indexify . (map indexify)
  where
    collapseIndexes = map $ \(i, ys) -> map (\(j, y) -> ((i, j), y)) ys

keyByValue :: (Eq a, Eq b) => [(a, b)] -> [(b, [a])]
keyByValue =
  foldl addToKeysForValue []
  where
    addToKeysForValue xs (k, v) =
      case lookup v xs of
        Just ks -> (v, (k : ks)) : (delete (v, ks) xs)
        _ -> (v, [k]) : xs