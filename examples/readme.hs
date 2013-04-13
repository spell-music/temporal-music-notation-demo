{-
Readme
'temporal-music-notation-demo' examples.


Examples depend on an external package 'temporal-music-notation-western'.
This package contains names specific to western music tradition.
So to run them do first.

>>cabal update
>>cabal install temporal-music-notation-western

A tune is first rendered to midi-file and then file is played back with the program timidity.
Timidity is recommended for this library, because timidity can interpret microtonal midi-messages.
-}
