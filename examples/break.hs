-- break beat

import System.Cmd(system)

import Temporal.Music.Western.P12
import Temporal.Music.Demo.GeneralMidi

tom11 = line [qd 0, qd 0, qnr, qd 0, hd 0.5, dhnr]
tom12 = line [qd 0, qd 0, qnr, qd 0, hd 0.5, qnr, qd 0]

scoTom = ff' $ line [tom11, tom12]
scoSnare = mp' $ line [hnr,  hd 0, hnr, hd 0]

scoMaracas = mp' $ line [dhnr, tri $ line [ed 0.5, ed 0, ed 0]]

drums = loop 8 $ chord [
        bassDrum1 scoTom,
        loop 2 $ acousticSnare scoSnare,
        loop 3 $ maracas scoMaracas
    ]


scoAccomp =  envelopeRel [0, 1, 0.5, 1.5, 0.5, 1, 0] $ 
    loop 2 $ 
    mf' $ high $ sustain 0.5 $ chord [
        p' $ loop 2 $ qn $ line [f, c, e, f, c, e, f, c],
        accent 1 $ low $ line [dwn a, dwn gs, hnr],
        accent 1.5 $ line [bnr, hnr, tri $ line [dhn gs, dhn e, dhn d]]
    ]

accomp = loop 4 $ glockenspiel scoAccomp 

-- fade out after 0.7*totalDur
res = {- envelopeSeg [0, 0.7, 0, 0.3, -2] $ -} chord [
    drums,
    accomp
    ]

main = do
      exportMidi "break.mid" res
      system "timidity break.mid"


