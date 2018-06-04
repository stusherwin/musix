module Music where

import qualified Data.Map.Lazy as M
import Data.Map.Lazy ((!))

data NoteName = A | B | C | D | E | F | G deriving (Show, Eq, Ord)

data Accidental = Natural | Sharp | Flat | DoubleSharp | DoubleFlat
instance Show Accidental where
  show Natural = ""
  show Sharp = "#"
  show Flat = "b"
  show DoubleSharp = "##"
  show DoubleFlat = "bb"

adjust Natural pitch = pitch
adjust Sharp pitch = pitch + 1
adjust Flat pitch = pitch - 1
adjust DoubleSharp pitch = pitch + 2
adjust DoubleFlat pitch = pitch - 2

data Note = Note NoteName Accidental

instance Show Note where
  show (Note name acc) = (show name) ++ (show acc)

data KeyType = MajorKey | MinorKey
data Key = Key Note KeyType
instance Show Key where
  show (Key keyNote MajorKey) = (show keyNote) ++ " major"
  show (Key keyNote MinorKey) = (show keyNote) ++ " minor"

data Scale = Major | Minor | WholeTone

intervals Major = [2, 2, 1, 2, 2, 2, 1]
intervals Minor = [2, 1, 2, 2, 2, 2, 1]
intervals WholeTone = [2, 2, 2, 2, 2, 2]

noteNameToBasePitch = M.fromList [(A, 0), (B, 2), (C, 3), (D, 5), (E, 7), (F, 8), (G, 10)]
basePitchToNoteName = M.fromList $ map (\(a, b) -> (b, a)) $ M.toList noteNameToBasePitch

pitch (Note name acc) oct =
  (oct * 12) + (adjust acc $ noteNameToBasePitch ! name)

type Octave = Int

expand scale startNote oct =
  scanl (+) (pitch startNote oct) (cycle $ intervals scale)

accidentalFor pitch targetPitch
  | targetPitch == pitch     = Natural
  | targetPitch == pitch + 1 = Sharp
  | targetPitch == pitch - 1 = Flat
  | targetPitch == pitch + 2 = DoubleSharp
  | targetPitch == pitch - 2 = DoubleFlat
  | otherwise                = error "No accidental available"

note (Key (Note name acc) keyType) pitch =
  if M.member pitch' basePitchToNoteName
    then Note (basePitchToNoteName ! pitch') Natural
    else case acc of
           Flat -> Note (basePitchToNoteName ! ((pitch' + 1) `mod` 12)) Flat
           _ -> Note (basePitchToNoteName ! ((pitch' - 1) `mod` 12)) Sharp
  where
    pitch' = pitch `mod` 12

expandNotes scale startNote oct =
  map (note (Key startNote MajorKey)) $ expand scale startNote oct