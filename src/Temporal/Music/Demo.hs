-- | Library provides functions to render 'Score' 
-- made with 'temporal-music-notation' library to 
-- midi file from 'HCodecs' package.
module Temporal.Music.Demo(
    module  Temporal.Music,
    module  Data.Finite,
    MidiNote(..),
    -- * Instruments
    instr, drumInstr,
    -- * Rendering
    renderMidi, FilePath, exportMidi) 
where

import Control.Arrow(first, second)
import Control.Applicative

import Data.Maybe
import Data.List
import Data.Function(on)
import Data.Word

import Data.Binary
import Data.Binary.Put

import qualified Codec.Midi as M

import Data.Finite

import Temporal.Music

-- TODO :
--  reading midi
--  midi :: FileName -> Score MidiEvent 
--  toScore :: Diap -> Scale -> Score MidiEvent -> Score Note
--
-- better to track function (currently it doesn't handle several
-- same notes playing in parallel)
type T = Double
type Instr = Int


type MidiEvent = Event T MidiNote

-- | This type represents midi note.
data MidiNote = MidiNote {
        midiNoteInstr     :: Maybe Instr,
        midiNoteVolume    :: MidiVolume,
        midiNotePitch     :: MidiPitch 
    } deriving (Show)

               

newtype MidiVolume = MidiVolume { volumeId :: Int }
    deriving (Show)
    

data MidiPitch  = MidiPitch  {
            pitchId :: Int,
            bendId  :: Double
    } deriving (Show)

type VolumeId   = Int
type PitchId    = Int


isDrum :: MidiNote -> Bool
isDrum = isNothing . midiNoteInstr

----------------------------------------------------
--

-- | Render Score to midi file and save 
-- results in current directory.
exportMidi :: FilePath -> Score MidiNote -> IO ()
exportMidi f = M.exportFile f . renderMidi

-- | Apply midi instrument.
instr :: (Finite vol, Finite pch) 
    => Instr -> Score (Note vol pch a) -> Score MidiNote
instr i = fmap $ 
    \n -> MidiNote (Just i)
                (midiVolume $ getVolume n) 
                (midiPitch  $ getPitch  n)
                
    
-- | Apply midi drum instrument.
drumInstr :: Finite vol
    => Instr -> Score (Drum vol a) -> Score MidiNote
drumInstr i = fmap $ 
    \n -> MidiNote  Nothing
                (midiVolume $ getVolume n) 
                (drumPitch i)                
    where drumPitch i = MidiPitch i 0


------------------------------------------
-- render evenrs


-- | Render to 'Midi'.
renderMidi :: Score MidiNote -> M.Midi
renderMidi s = M.Midi M.SingleTrack timeDiv [toTrack s]

timeDiv :: M.TimeDiv
timeDiv = M.TicksPerBeat 96

toTrack :: Score MidiNote -> M.Track M.Ticks
toTrack = addEndMsg . maybe [] phi . checkOnEmpty . trackEvents
    where phi = tfmTime . mergeInstr . groupInstr
          checkOnEmpty x 
            | null x    = Nothing
            | otherwise = Just x

addEndMsg :: M.Track M.Ticks -> M.Track M.Ticks
addEndMsg = (++ [(0, M.TrackEnd)])

tfmTime :: M.Track Double -> M.Track M.Ticks
tfmTime = M.fromAbsTime . M.fromRealTime timeDiv . 
     sortBy (compare `on` fst)


groupInstr :: [Event T MidiNote] -> ([[MidiEvent]], [MidiEvent])
groupInstr = first groupByInstrId . 
    partition (not . isDrum . eventContent) . alignByZero 
    where groupByInstrId = groupBy ((==) `on` instrId) . 
                           sortBy  (compare `on` instrId)
          

trackEvents = render

mergeInstr :: ([[MidiEvent]], [MidiEvent]) -> M.Track Double
mergeInstr (instrs, drums) = concat $ drums' : instrs'
    where instrs' = zipWith setChannel ([0 .. 8] ++ [10 .. 15]) instrs
          drums'  = setDrumChannel drums  

setChannel :: M.Channel -> [MidiEvent] -> M.Track Double
setChannel ch ms = case ms of
    []      -> []
    x:xs    -> (0, M.ProgramChange ch (instrId x)) : (fromEvent ch =<< ms)
    

setDrumChannel :: [MidiEvent] -> M.Track Double
setDrumChannel ms = fromEvent drumChannel =<< ms 
    where drumChannel = 9
                                                      
instrId = fromJust . midiNoteInstr . eventContent


fromEvent :: M.Channel -> MidiEvent -> M.Track Double
fromEvent ch e = (t1, m1) : zip (repeat t0) m0
    where t0 = eventStart e
          t1 = eventStart e + eventDur e
          (m0, m1) = toMessages ch $ eventContent e

clipToMidi :: (Ord a, Num a) => a -> a
clipToMidi = max 0 . min 127

---------------------------------------------------

toMessages :: M.Channel -> MidiNote 
     -> ([M.Message], M.Message)
toMessages ch e = toMessages' ch (midiNoteVolume e) (midiNotePitch e)

toMessages' :: M.Channel -> MidiVolume -> MidiPitch 
    -> ([M.Message], M.Message)
toMessages' ch mv mp = (addTune [M.NoteOn ch p v], M.NoteOff ch p 64)
    where addTune = maybe id (:) $ tuneMessage <$> tuneParams mp
          v = clipToMidi $ volumeId mv
          p = clipToMidi $ pitchId mp

----------------------------------------------
-- construct MidiEvent from general Notes

-- set diapason to midi diapason (0, 127), initial
-- diapason is forgotten
midiVolume :: Finite a => Volume a -> MidiVolume
midiVolume v = MidiVolume $ floor $ 127 * volumeAsDouble v


midiPitch :: Finite s => Pitch s -> MidiPitch
midiPitch p = uncurry MidiPitch $ properFraction $  
      69 + 12 * (scaleStepFactor s n 
    + scaleOctaveFactor s k + scaleBendFactor s n r)
    where (d, r) = properFraction $ pitchAsDouble p
          (k, n) = divMod d $  scaleSize s
          s      = pitchScale p

log2 :: (Floating a) => a -> a
log2 = logBase 2

-- log2 (f0 * s / 440)
scaleStepFactor :: Scale -> Int -> Interval
scaleStepFactor s n = log2 $ (scaleStep s n) * f0 / 440
    where f0 = scaleBase s

-- k * log2 d
scaleOctaveFactor :: Scale -> Int -> Interval
scaleOctaveFactor s k 
    | abs (d - 2) < 1e-9 = k'
    | otherwise          = k' * log2 d
    where d  = scaleOctave s
          k' = fromIntegral k 

-- x * log2 (r/l)
scaleBendFactor :: Scale -> Int -> Interval -> Interval
scaleBendFactor s n x 
    | abs x < 1e-9 = 0
    | x > 0        =     x * log2 (r / c)
    | otherwise    = abs x * log2 (l / c)
    where c = scaleStep s n
          l = scaleStep s $ n - 1
          r = scaleStep s $ n + 1



-----------------------------------------------------
-- Microsound

type TuneId = (KeyId, Cents)
type KeyId = Word8

type Cent0 = Word8
type Cent1 = Word8

type Cents = (Cent0, Cent1)

cents :: Double -> (Cent0, Cent1)
cents d = (fromIntegral c0, fromIntegral c1)
    where (c0, c1) = flip divMod (128::Int) $ 
                        fst $ properFraction (d/deltaTune)

tuneParams :: MidiPitch -> Maybe TuneId
tuneParams (MidiPitch p d)  
    | c == (0, 0) = Nothing
    | otherwise   = Just (fromIntegral p, c)
    where c = cents d

-- | 1 semitone / 2^14 
deltaTune :: Double
deltaTune = 0.000061


tuneMessage :: TuneId -> M.Message 
tuneMessage (x, (a, b)) = M.Sysex 240 $ 
    runPut $ do
        putWord8 127
        putWord8 0
        putWord8 8
        putWord8 2
        putWord8 0
        putWord8 1
        putWord8 x
        putWord8 x
        putWord8 a
        putWord8 b
        putWord8 247 


