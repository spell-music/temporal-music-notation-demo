-- break beat

import System.Cmd(system)

import Temporal.Music.Western.P12
import Temporal.Music.Demo.GeneralMidi

tom11 = mel [qd 0, qd 0, qnr, qd 0, hd 0.5, dhnr]
tom12 = mel [qd 0, qd 0, qnr, qd 0, hd 0.5, qnr, qd 0]

scoTom = ff' $ mel [tom11, tom12]
scoSnare = mp' $ mel [hnr,  hd 0, hnr, hd 0]

scoMaracas = mp' $ mel [dhnr, trn $ mel [ed 0.5, ed 0, ed 0]]

drums = loop 8 $ har [
        bassDrum1 scoTom,
        loop 2 $ acousticSnare scoSnare,
        loop 3 $ maracas scoMaracas
    ]


scoAccomp =  withAccentRel [0, 1, 0.5, 1.5, 0.5, 1, 0] $ 
    loop 2 $ 
    mf' $ high $ sustain 0.5 $ har [
        p' $ loop 2 $ qn $ mel [f, c, e, f, c, e, f, c],
        accent 1 $ low $ mel [dwn a, dwn gs, hnr],
        accent 1.5 $ mel [bnr, hnr, trn $ mel [dhn gs, dhn e, dhn d]]
    ]

accomp = loop 4 $ glockenspiel scoAccomp 

-- fade out after 0.7*totalDur
res = withAccentSeg [0, 0.7, 0, 0.3, -5] $ har [
    drums,
    accomp
    ]

main = do
      exportMidi "break.mid" res
      system "timidity break.mid"


