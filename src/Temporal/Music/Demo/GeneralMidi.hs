-- | General MIDI names


module Temporal.Music.Demo.GeneralMidi(
    module Temporal.Music.Demo,
    -- * Instruments
    -- **  Piano
    acousticGrandPiano,
    brightAcousticPiano,
    electricGrandPiano,
    honkyTonkPiano,
    rhodesPiano,
    chorusedPiano,
    harpsichord,
    clavinet,
    -- **  Chromatic Percussion
    celesta,
    glockenspiel,
    musicBox,
    vibraphone,
    marimba,
    xylophone,
    tubularBells,
    dulcimer,
    -- **  Organ
    hammondOrgan,
    percussiveOrgan,
    rockOrgan,
    churchOrgan,
    reedOrgan,
    accordion,
    harmonica,
    tangoAccordion,
    -- **  Guitar
    acousticGuitarNylon,
    acousticGuitarSteel,
    electricGuitarJazz,
    electricGuitarClean,
    electricGuitarMuted,
    overdrivenGuitar,
    distortionGuitar,
    guitarHarmonics,
    -- **  Bass
    acousticBass,
    electricBassFingered,
    electricBassPicked,
    fretlessBass,
    slapBass1,
    slapBass2,
    synthBass1,
    synthBass2,
    -- **  Strings
    violin,
    viola,
    cello,
    contrabass,
    tremoloStrings,
    pizzicatoStrings,
    orchestralHarp,
    timpani,
    -- **  Ensemble
    stringEnsemble1,
    stringEnsemble2,
    synthStrings1,
    synthStrings2,
    choirAahs,
    voiceOohs,
    synthVoice,
    orchestraHit,
    -- **  Brass
    trumpet,
    trombone,
    tuba,
    mutedTrumpet,
    frenchHorn,
    brassSection,
    synthBrass1,
    synthBrass2,
    -- **  Reed
    sopranoSax,
    altoSax,
    tenorSax,
    baritoneSax,
    oboe,
    bassoon,
    englishHorn,
    clarinet,
    -- **  Pipe
    piccolo,
    flute,
    recorder,
    panFlute,
    blownBottle,
    shakuhachi,
    whistle,
    ocarina,
    -- **  Synth Lead
    lead1Square,
    lead2Sawtooth,
    lead3Calliope,
    lead4Chiff,
    lead5Charang,
    lead6Voice,
    lead7Fifths,
    lead8BassLead,
    -- **  Synth Pad
    pad1NewAge,
    pad2Warm,
    pad3Polysynth,
    pad4Choir,
    pad5Bowed,
    pad6Metallic,
    pad7Halo,
    pad8Sweep,
    -- **  Synth Effects
    fx1Train,
    fx2Soundtrack,
    fx3Crystal,
    fx4Atmosphere,
    fx5Brightness,
    fx6Goblins,
    fx7Echoes,
    fx8SciFi,
    -- **  Ethnic
    sitar,
    banjo,
    shamisen,
    koto,
    kalimba,
    bagpipe,
    fiddle,
    shanai,
    -- **  Percussive
    tinkleBell,
    agogo,
    steelDrums,
    woodblock,
    taikoDrum,
    melodicDrum,
    synthDrum,
    reverseCymbal,
    -- **  Sound Effects
    guitarFretNoise,
    breathNoise,
    seashore,
    birdTweet,
    telephoneRing,
    helicopter,
    applause,
    gunshot,
    


    -- * Percussion
    acousticBassDrum,
    bassDrum1,
    sideStick,
    acousticSnare,
    handClap,
    electricSnare,
    lowFloorTom,
    closedHiHat,
    highFloorTom,
    pedalHiHat,
    lowTom,
    openHiHat,
    lowMidTom,
    hiMidTom,
    crashCymbal1,
    highTom,
    rideCymbal1,
    chineseCymbal,
    rideBell,
    tambourine,
    splashCymbal,
    cowbell,
    crashCymbal2,
    vibraslap,
    rideCymbal2,
    hiBongo,
    lowBongo,
    muteHiConga,
    openHiConga,
    lowConga,
    highTimbale,
    lowTimbale,
    highAgogo,
    lowAgogo,
    cabasa,
    maracas,
    shortWhistle,
    longWhistle,
    shortGuiro,
    longGuiro,
    claves,
    hiWoodBlock,
    lowWoodBlock,
    muteCuica,
    openCuica,
    muteTriangle,
    openTriangle)
where


import Temporal.Music.Demo


--------------------
--Instruments


 
acousticGrandPiano, brightAcousticPiano, electricGrandPiano,
                    honkyTonkPiano, rhodesPiano, chorusedPiano, harpsichord, clavinet,
                    celesta, glockenspiel, musicBox, vibraphone, marimba, xylophone,
                    tubularBells, dulcimer, hammondOrgan, percussiveOrgan, rockOrgan,
                    churchOrgan, reedOrgan, accordion, harmonica, tangoAccordion,
                    acousticGuitarNylon, acousticGuitarSteel, electricGuitarJazz,
                    electricGuitarClean, electricGuitarMuted, overdrivenGuitar,
                    distortionGuitar, guitarHarmonics, acousticBass,
                    electricBassFingered, electricBassPicked, fretlessBass, slapBass1,
                    slapBass2, synthBass1, synthBass2, violin, viola, cello,
                    contrabass, tremoloStrings, pizzicatoStrings, orchestralHarp,
                    timpani, stringEnsemble1, stringEnsemble2, synthStrings1,
                    synthStrings2, choirAahs, voiceOohs, synthVoice, orchestraHit,
                    trumpet, trombone, tuba, mutedTrumpet, frenchHorn, brassSection,
                    synthBrass1, synthBrass2, sopranoSax, altoSax, tenorSax,
                    baritoneSax, oboe, bassoon, englishHorn, clarinet, piccolo, flute,
                    recorder, panFlute, blownBottle, shakuhachi, whistle, ocarina,
                    lead1Square, lead2Sawtooth, lead3Calliope, lead4Chiff,
                    lead5Charang, lead6Voice, lead7Fifths, lead8BassLead, pad1NewAge,
                    pad2Warm, pad3Polysynth, pad4Choir, pad5Bowed, pad6Metallic,
                    pad7Halo, pad8Sweep, fx1Train, fx2Soundtrack, fx3Crystal,
                    fx4Atmosphere, fx5Brightness, fx6Goblins, fx7Echoes, fx8SciFi,
                    sitar, banjo, shamisen, koto, kalimba, bagpipe, fiddle, shanai,
                    tinkleBell, agogo, steelDrums, woodblock, taikoDrum, melodicDrum,
                    synthDrum, reverseCymbal, guitarFretNoise, breathNoise, seashore,
                    birdTweet, telephoneRing, helicopter, applause, gunshot ::
                      (Finite vol, Finite pch) => Score (Note vol pch a) -> Score MidiNote


acousticGrandPiano = instr 0
brightAcousticPiano = instr 1
electricGrandPiano = instr 2
honkyTonkPiano = instr 3
rhodesPiano = instr 4
chorusedPiano = instr 5
harpsichord = instr 6
clavinet = instr 7
celesta = instr 8
glockenspiel = instr 9
musicBox = instr 10
vibraphone = instr 11
marimba = instr 12
xylophone = instr 13
tubularBells = instr 14
dulcimer = instr 15
hammondOrgan = instr 16
percussiveOrgan = instr 17
rockOrgan = instr 18
churchOrgan = instr 19
reedOrgan = instr 20
accordion = instr 21
harmonica = instr 22
tangoAccordion = instr 23
acousticGuitarNylon = instr 24
acousticGuitarSteel = instr 25
electricGuitarJazz = instr 26
electricGuitarClean = instr 27
electricGuitarMuted = instr 28
overdrivenGuitar = instr 29
distortionGuitar = instr 30
guitarHarmonics = instr 31
acousticBass = instr 32
electricBassFingered = instr 33
electricBassPicked = instr 34
fretlessBass = instr 35
slapBass1 = instr 36
slapBass2 = instr 37
synthBass1 = instr 38
synthBass2 = instr 39
violin = instr 40
viola = instr 41
cello = instr 42
contrabass = instr 43
tremoloStrings = instr 44
pizzicatoStrings = instr 45
orchestralHarp = instr 46
timpani = instr 47
stringEnsemble1 = instr 48
stringEnsemble2 = instr 49
synthStrings1 = instr 50
synthStrings2 = instr 51
choirAahs = instr 52
voiceOohs = instr 53
synthVoice = instr 54
orchestraHit = instr 55
trumpet = instr 56
trombone = instr 57
tuba = instr 58
mutedTrumpet = instr 59
frenchHorn = instr 60
brassSection = instr 61
synthBrass1 = instr 62
synthBrass2 = instr 63
sopranoSax = instr 64
altoSax = instr 65
tenorSax = instr 66
baritoneSax = instr 67
oboe = instr 68
bassoon = instr 69
englishHorn = instr 70
clarinet = instr 71
piccolo = instr 72
flute = instr 73
recorder = instr 74
panFlute = instr 75
blownBottle = instr 76
shakuhachi = instr 77
whistle = instr 78
ocarina = instr 79
lead1Square = instr 80
lead2Sawtooth = instr 81
lead3Calliope = instr 82
lead4Chiff = instr 83
lead5Charang = instr 84
lead6Voice = instr 85
lead7Fifths = instr 86
lead8BassLead = instr 87
pad1NewAge = instr 88
pad2Warm = instr 89
pad3Polysynth = instr 90
pad4Choir = instr 91
pad5Bowed = instr 92
pad6Metallic = instr 93
pad7Halo = instr 94
pad8Sweep = instr 95
fx1Train = instr 96
fx2Soundtrack = instr 97
fx3Crystal = instr 98
fx4Atmosphere = instr 99
fx5Brightness = instr 100
fx6Goblins = instr 101
fx7Echoes = instr 102
fx8SciFi = instr 103
sitar = instr 104
banjo = instr 105
shamisen = instr 106
koto = instr 107
kalimba = instr 108
bagpipe = instr 109
fiddle = instr 110
shanai = instr 111
tinkleBell = instr 112
agogo = instr 113
steelDrums = instr 114
woodblock = instr 115
taikoDrum = instr 116
melodicDrum = instr 117
synthDrum = instr 118
reverseCymbal = instr 119
guitarFretNoise = instr 120
breathNoise = instr 121
seashore = instr 122
birdTweet = instr 123
telephoneRing = instr 124
helicopter = instr 125
applause = instr 126
gunshot = instr 127


--------------------
--Percussion


 
acousticBassDrum, bassDrum1, sideStick, acousticSnare, handClap,
                  electricSnare, lowFloorTom, closedHiHat, highFloorTom, pedalHiHat,
                  lowTom, openHiHat, lowMidTom, hiMidTom, crashCymbal1, highTom,
                  rideCymbal1, chineseCymbal, rideBell, tambourine, splashCymbal,
                  cowbell, crashCymbal2, vibraslap, rideCymbal2, hiBongo, lowBongo,
                  muteHiConga, openHiConga, lowConga, highTimbale, lowTimbale,
                  highAgogo, lowAgogo, cabasa, maracas, shortWhistle, longWhistle,
                  shortGuiro, longGuiro, claves, hiWoodBlock, lowWoodBlock,
                  muteCuica, openCuica, muteTriangle, openTriangle ::
                     (Finite vol) => Score (Drum vol a) -> Score MidiNote


acousticBassDrum = drumInstr 35
bassDrum1 = drumInstr 36
sideStick = drumInstr 37
acousticSnare = drumInstr 38
handClap = drumInstr 39
electricSnare = drumInstr 40
lowFloorTom = drumInstr 41
closedHiHat = drumInstr 42
highFloorTom = drumInstr 43
pedalHiHat = drumInstr 44
lowTom = drumInstr 45
openHiHat = drumInstr 46
lowMidTom = drumInstr 47
hiMidTom = drumInstr 48
crashCymbal1 = drumInstr 49
highTom = drumInstr 50
rideCymbal1 = drumInstr 51
chineseCymbal = drumInstr 52
rideBell = drumInstr 53
tambourine = drumInstr 54
splashCymbal = drumInstr 55
cowbell = drumInstr 56
crashCymbal2 = drumInstr 57
vibraslap = drumInstr 58
rideCymbal2 = drumInstr 59
hiBongo = drumInstr 60
lowBongo = drumInstr 61
muteHiConga = drumInstr 62
openHiConga = drumInstr 63
lowConga = drumInstr 64
highTimbale = drumInstr 65
lowTimbale = drumInstr 66
highAgogo = drumInstr 67
lowAgogo = drumInstr 68
cabasa = drumInstr 69
maracas = drumInstr 70
shortWhistle = drumInstr 71
longWhistle = drumInstr 72
shortGuiro = drumInstr 73
longGuiro = drumInstr 74
claves = drumInstr 75
hiWoodBlock = drumInstr 76
lowWoodBlock = drumInstr 77
muteCuica = drumInstr 78
openCuica = drumInstr 79
muteTriangle = drumInstr 80
openTriangle = drumInstr 81
