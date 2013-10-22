-- J.S. Bach - Choral prelude F-moll 'Ich ruf zu dir Herr Jesu Christ'

import System.Cmd(system)

import Temporal.Music.Western.P12
import Temporal.Music.Demo.GeneralMidi


pedal = sustainT
trill n a b = loop n $ mel [a, b]

-- alto dynamics

type Score12 = Score (Note ())

up :: Double -> [Score12] -> Score12
up x = withAccentRel [x, x + accDiap] . mel . map sn 

down :: Double -> [Score12] -> Score12
down x = withAccentRel [x + accDiap, x] . mel . map sn 

upDown :: Double -> [Score12] -> Score12
upDown x = withAccentRel [x, x + accDiap, x] . mel . map sn 

downUp :: Double -> [Score12] -> Score12
downUp x = withAccentRel [x, x-accDiap, x] . mel . map sn 

flat :: Double -> [Score12] -> Score12
flat x = accent x . mel . map sn

accDiap = 0.5

--------------------------------------------------
-- solo

-- Part I

solo0 = qn $ high c

solo1 = mel [
-- 1 bar
    qn af, qn bf, den af, sn g, den f, sn g,
-- 2 bar
    withAccentRel [0, 0.4, 0] $ mel [mel $ map sn [af, bf, af, bf],
    dim 1.5 $ hn $ trill 6 (accent 0.2 $ tn $ high c) (tn bf), tn af, tn bf], 
    high $ mel [accent 0.2 $ qn $ c, den c, sn df, 
-- 3 bar
    qn ef, tn df, str (1/4 - 3/32) $ wn c, 
    sn $ low bf, qn $ low af, en $ low bf, en c] 
    ]

solo11 = high $ mel [
    str (1/4 + 1/16) $ wn df, 
    mel $ map tn [ef, f, ef, df], sn c, qn c, qn c]


solo12 = high $ mel [
    qn df, sn df,
    mel $ map tn [ef, f, ef, df],sn c,  qn c, qn ef]

    
soloI = mel [solo0, reprise solo1 solo11 solo12]


-- Part II

solo21 = high $ mel [
-- 1 bar
        qn f, en ef, tn df, tn c, sn df,
    low $ mel $ map en [high c, bf, af, bf],
-- 2 bar
        qn c, qn c, qn $ low bf, qn $ low af,
-- 3 bar
        low $ mel [hn g, hn f, 
-- 4 bar    
        qn af, qn g, hn f]
    ]

solo22 = mel [
-- 5 bar
        dhn ef, qn ef,
-- 6 bar
        qn af, qn af, qn bf, qn bf,
-- 7 bar   
    high $ mel [dhn c, qn df],
-- 8 bar
    high $ qn c, qn bf, qn af, den f, sn g,
-- 9 bar
    qn af, qn g, qn f
    ]

soloII = solo21 +:+ solo22

solo = pedal (1/64) $ soloI +:+ soloII

---------------------------------------------------
-- alto

-- Part I

alto0 = high $ up 0 [low af, c, f, e] 

alto1 = mel [
-- 1 bar
        high $ down 0.5 [f, c, low af, low f], 
        upDown 0 [g, bf, high df, high c],
        upDown 0 [f, af, high c, bf],
        downUp 0 [af, f, af, high c],

-- 2 bar
        high $ downUp 0.2 [f, e, f, af],
        high $ down 0 [g, f, e, f],
        high $ downUp 0 [e, c, low g, low bf, low af, c, f, af],

-- 3 bar
        high $ flat 0 [g, ef, af, g],
        high $ up 0 [af, ef, f, gf, f, df, f, af, g, df, c, gf]
     ] 

alto11 = high $ mel [
        upDown 0 [f, low bf, df, f, bf, af, g, af, g, c, e, low bf],
        upDown 0 [low f, c, f, e]
    ]

alto12 = high $ mel [
        upDown 0 [f, low bf, df, f, bf, af, g, af],
        down 0 [g, low bf, low af, f, low g, df, low af, c]
    ]

    
altoI = mel [alto0, reprise alto1 alto11 alto12]

-- Part II

alto21 = high $ mel [
-- 1 bar
        upDown 0 [
            low af, c, low bf, df, low bf, df, af, g, 
            af, ef, df, g, c, f, af, g],
-- 2 bar
        down (-accDiap) [
            af,  ef, low af, gf, f, low af, low g, df, 
            c, low af, c, ef, g, c, low bf, g,
-- 3 bar 
            df, f, g, f],
        up (-accDiap) [
                            e, low bf, df, c,
                low af, c, f, e, f, c, low af, low f],
-- 4 bar
        flat 0 [
            low bf, f, g, f, low bf, ef, f, ef,
            c, ef, f, ef, d, low g, low b, d
        ]
    ]

alto22 = high $ mel [
-- 5 bar
        down (-accDiap) $ [
            low g, c, ef, df, low g, low bf, df, c] ++
            map low [ef, af, high c, bf, high df, bf, c, high df,
-- 6 bar
                f, af, high df, high c, f, af, high c, bf, 
                f, af, bf, af, g, bf, high df, high c],

-- 7 bar
        up (-accDiap) [
            low af, c, ef, af, ef, bf, high c, bf,
            a, ef, gf, low a, low bf, g, low af, f
            ],

-- 8 bar
        down (-accDiap) [
            low af, ef, d, ef, low f, df, ef, df, 
            low ef, c, df, c, low bf, f, g, f,
-- 9 bar
            d, f, g, f,   e, df, low bf, low g, 
            low a, c]

    ]

alto23 = en $ high f

altoII = mel [alto21, alto22, alto23]

alto = pedal (1/128) $ lower 3 $ mel [altoI, altoII]


----------------------------------------------------------
-- bass 

-- Part I

bass0 = lower 1 $ mel [en f, en f]

bass1 = (mel $ map en [
-- 1 bar
        f, f, f, e, f, f, f, ef,
-- 2 bar
        df, df, df, df, c, c, f, f]) +:+
-- 3 bar
    (high $ mel $ map en [c, c, c, c, c, c, low bf, low a])

bass11 = mel $ map en [bf, af, g, f, e, c, low f, low f]

bass12 = mel $ map en [bf, af, g, f, e, f, c, c]

bassI = mel [bass0, reprise bass1 bass11 bass12]


-- Part II

bass21 = mel $ map en [
-- 1 bar
        df, df, ef, ef, af, ef, f, df,
-- 2 bar
        low f, low f, f, f, d, e, f, df,
-- 3 bar
        low bf, low g, c, c, df, df, df , df,
-- 4 bar
        d, d, d, d, low a, low a, low b, low b
    ] 
    
bass22 = low $ mel $ map en [
-- 5 bar
        high c, high c, bf, bf, af, af, g, g,
-- 6 bar
        f, f, ef, ef, d, d, ef, ef, 
-- 7 bar
        af, af, gf, gf, f, f, bf, bf,
-- 8 bar
        bf, af, af, g, g, f, high df, high df,
-- 9 bar
        b, b, high c, high c
    ]

bass23 = qn $ low f

    
bassII = mel [bass21, bass22, bass23]

bass = lower 3 $ mel [bassI, bassII]

sco = del (-4) $ bpm (lento $ -0.2) $ har [
        solo, 
        mp' $ higher 2 alto, 
        accent 0.2 $ p' $ higher 2 $ bass,
        rest 0
      ]

file = "out.csd"
flags = "-d"

out = acousticGrandPiano sco

main = do
    exportMidi "choral.mid" $ out
    system "timidity choral.mid"

