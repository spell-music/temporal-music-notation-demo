-- | Generates module 'GeneralMidi'

import Data.Char
import Data.List.Split

import Language.Haskell.Exts.Syntax
import Language.Haskell.Exts.Pretty
import Language.Haskell.Exts.Build'

import qualified Text.PrettyPrint as P

toFunName :: String -> String
toFunName x = map toLower pref ++ suff
    where pref = takeWhile isUpper x
          suff = dropWhile isUpper x  

-- module


moduleGM name instrs drums = P.vcat $ P.punctuate wspace [
        P.text "-- | General MIDI names",
        moduleHead name instrs drums,
        imports,
        division "Instruments",
        pp $ instrSig instrs,
        P.vcat $ map pp $ instrFuns instrs,
        division "Percussion",
        pp $ drumSig drums,
        P.vcat $ map pp $ drumFuns drums]
    where pp = P.text . prettyPrint
          wspace     = P.text "\n\n"
          division x = (P.text $ replicate 20 '-') 
                        P.$$ (P.text $ "--" ++ x)


moduleHead name instrs drums = P.vcat [
    P.text $ "module " ++ name ++ "(",
    P.nest 4 $ exports instrs drums P.<> P.rparen,
    P.text "where"]

exports instrs drums =
   P.vcat [
        P.text (toSec1 "Instruments"),
        
        P.vcat $ map ( P.<> P.comma) $
            map (uncurry expFunNames) $ 
            expInstrFunNames (map toSec2 familyNames) instrs, 
        P.text "\n\n",
        expFunNames (toSec1 "Percussion")  drums]

toSec1 = ("-- * " ++ )
toSec2 = ("-- ** " ++)

expInstrFunNames fams names = zip fams $ splitEvery 8 names 

expFunNames :: String -> [String] -> P.Doc
expFunNames sec names = (P.text sec P.$$ ) $ P.vcat $ 
    P.punctuate P.comma $ map P.text names

imports :: P.Doc
imports = P.vcat [
    P.text "import Temporal.Music.Notation(Score, Seg)",
    P.text "import Temporal.Music.Notation.Note(Note, Drum)",
    P.text "import Temporal.Music.Notation.Demo"]

-- instruments

zSrcLoc = SrcLoc "" 0 0 

instrSig :: [String] -> Decl
instrSig xs = typeSig zSrcLoc xs $ 
    [var "Seg" "nVol", var "Seg" "nPch"] 
    =>> var "Score" <$> var "Note" `apps` [var "nVol", var "nPch", var "a"] 
    ->> var "Score" <$> var "MidiEvent"

instrFuns = toFuns 0 "instr"

-- drums

drumSig :: [String] -> Decl
drumSig xs = typeSig zSrcLoc xs $ 
    [var "Seg" "nVol"] 
    =>> var "Score" <$> var "Drum" `apps` [var "nVol", var "a"] 
    ->> var "Score" <$> var "MidiEvent"

drumFuns = toFuns 35 "drumInstr"

-- funs

toFuns :: Integer -> String -> [String] -> [Decl]
toFuns i0 constr = map (uncurry $ toFun constr) . zip [i0 .. ]

toFun :: String -> Integer -> String -> Decl
toFun constr id name = fun zSrcLoc name [] <=> (var constr <$> int id)

out :: Pretty a => a -> IO ()
out = putStrLn . prettyPrint


main = do
    let i = map (toFunName . show) $ [(minBound :: InstrumentName) .. maxBound]
        d = map (toFunName . show) $ [(minBound :: PercussionName) .. maxBound] 

    writeFile "GeneralMidi.hs" $ show $ 
        moduleGM "Temporal.Music.Notation.Demo.GeneralMidi" i d 

    
familyNames :: [String]
familyNames = map (splitAtUpper . show) 
        [(minBound :: InstrumentFamily) .. maxBound] 
        where splitAtUpper = unwords . split (keepDelimsL $ whenElt isUpper)  


data InstrumentFamily =  
    Piano          | ChromaticPercussion     | Organ            | Guitar
  | Bass           | Strings                 | Ensemble         | Brass 
  | Reed           | Pipe                    | SynthLead        | SynthPad    
  | SynthEffects   | Ethnic                  | Percussive       | SoundEffects
  deriving (Show, Enum, Bounded)


data InstrumentName = 
     AcousticGrandPiano     | BrightAcousticPiano    | ElectricGrandPiano
  |  HonkyTonkPiano         | RhodesPiano            | ChorusedPiano
  |  Harpsichord            | Clavinet               | Celesta 
  |  Glockenspiel           | MusicBox               | Vibraphone  
  |  Marimba                | Xylophone              | TubularBells
  |  Dulcimer               | HammondOrgan           | PercussiveOrgan 
  |  RockOrgan              | ChurchOrgan            | ReedOrgan
  |  Accordion              | Harmonica              | TangoAccordion
  |  AcousticGuitarNylon    | AcousticGuitarSteel    | ElectricGuitarJazz
  |  ElectricGuitarClean    | ElectricGuitarMuted    | OverdrivenGuitar
  |  DistortionGuitar       | GuitarHarmonics        | AcousticBass
  |  ElectricBassFingered   | ElectricBassPicked     | FretlessBass
  |  SlapBass1              | SlapBass2              | SynthBass1   
  |  SynthBass2             | Violin                 | Viola  
  |  Cello                  | Contrabass             | TremoloStrings
  |  PizzicatoStrings       | OrchestralHarp         | Timpani
  |  StringEnsemble1        | StringEnsemble2        | SynthStrings1
  |  SynthStrings2          | ChoirAahs              | VoiceOohs
  |  SynthVoice             | OrchestraHit           | Trumpet
  |  Trombone               | Tuba                   | MutedTrumpet
  |  FrenchHorn             | BrassSection           | SynthBrass1
  |  SynthBrass2            | SopranoSax             | AltoSax 
  |  TenorSax               | BaritoneSax            | Oboe  
  |  Bassoon                | EnglishHorn            | Clarinet
  |  Piccolo                | Flute                  | Recorder
  |  PanFlute               | BlownBottle            | Shakuhachi
  |  Whistle                | Ocarina                | Lead1Square
  |  Lead2Sawtooth          | Lead3Calliope          | Lead4Chiff
  |  Lead5Charang           | Lead6Voice             | Lead7Fifths
  |  Lead8BassLead          | Pad1NewAge             | Pad2Warm
  |  Pad3Polysynth          | Pad4Choir              | Pad5Bowed
  |  Pad6Metallic           | Pad7Halo               | Pad8Sweep
  |  FX1Train               | FX2Soundtrack          | FX3Crystal
  |  FX4Atmosphere          | FX5Brightness          | FX6Goblins
  |  FX7Echoes              | FX8SciFi               | Sitar
  |  Banjo                  | Shamisen               | Koto
  |  Kalimba                | Bagpipe                | Fiddle 
  |  Shanai                 | TinkleBell             | Agogo  
  |  SteelDrums             | Woodblock              | TaikoDrum
  |  MelodicDrum            | SynthDrum              | ReverseCymbal
  |  GuitarFretNoise        | BreathNoise            | Seashore
  |  BirdTweet              | TelephoneRing          | Helicopter
  |  Applause               | Gunshot     
    deriving (Show, Enum, Bounded)

data PercussionName = 
    AcousticBassDrum       | BassDrum1	            | SideStick	 
  | AcousticSnare	       | HandClap	            | ElectricSnare	 
  | LowFloorTom	           | ClosedHiHat	        | HighFloorTom	 
  | PedalHiHat	           | LowTom	                | OpenHiHat	 
  | LowMidTom	           | HiMidTom	            | CrashCymbal1	 
  | HighTom	               | RideCymbal1	        | ChineseCymbal	 
  | RideBell	           | Tambourine	            | SplashCymbal	 
  | Cowbell	               | CrashCymbal2	        | Vibraslap	 
  | RideCymbal2	           | HiBongo	            | LowBongo	 
  | MuteHiConga	           | OpenHiConga	        | LowConga	 
  | HighTimbale	           | LowTimbale	            | HighAgogo	 
  | LowAgogo	           | Cabasa	                | Maracas	 
  | ShortWhistle	       | LongWhistle	        | ShortGuiro	 
  | LongGuiro	           | Claves	                | HiWoodBlock	 
  | LowWoodBlock	       | MuteCuica	            | OpenCuica	 
  | MuteTriangle	       | OpenTriangle
    deriving (Show, Enum, Bounded)

