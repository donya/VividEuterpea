Playback from Euterpea through Vivid
Donya Quick

> {-# LANGUAGE DataKinds, ExtendedDefaultRules #-}
> module PlayV where
> import Vivid
> import Euterpea
> import Control.Exception

A default sound to use if no other synthesizer is defined. The 
default sound is a sine wave at a given note's frequency with 
a maximum amplitude of 0.3.
   
> defaultSound :: SynthDef '["note"]
> defaultSound = sd (0 ::I "note") $ do
>    s <- 0.3 ~* sinOsc (freq_ $ midiCPS (V::V "note"))
>    out 0 [s,s]

Lookup table type for synthesizers by a String name.

> type SynthTable = [(String, SynthDef '["note"])]

Euterpea has an Instrument constructor that allows a custom 
string. This extracts it easily.
  
> getInstrName (CustomInstrument s) = s
> getInstrName _ = ""

Playback functions for sending to Vivid synths. Sequentially infinite 
Euterpea values are permitted and Ctrl+C then Enter will exit out of the 
cycle.
   
> playV :: (ToMusic1 a) => SynthTable -> Music a -> IO ()
> playV t m = onException (playMEvs t 0 $ perform m) cmdPeriod

Euterpea's PlayParams can also be used to provide a customized performance
algorithm. Other fields of PlayParams are ignored currently; the timing 
strictness field has no effect.

> playVC :: (ToMusic1 a) => SynthTable -> PlayParams -> Music a -> IO ()
> playVC t pp m = onException op cmdPeriod where
>     op = playMEvs t 0 $ (perfAlg pp . toMusic1) m

------------------------------

Supporting implementation for playback through Vivid. 

> playEvent :: VividAction m => SynthTable -> MEvent -> m()
> playEvent insts me = 
>     let iName = getInstrName $ eInst me
>         x = lookup iName insts
>         sDef = maybe defaultSound id x
>     in  do
>             s0 <- synth sDef (fromIntegral (ePitch me) :: I "note")
>             wait (fromRational (eDur me))
>             free s0     
     
> playMEvs :: SynthTable -> PTime -> [MEvent] -> IO ()
> playMEvs insts cTime [] = return ()
> playMEvs insts cTime [me] = fork $ do
>     wait $ fromRational (eTime me - cTime)
>     playEvent insts me
> playMEvs insts cTime (me1:me2:mevs) = do
>     wait $ fromRational (eTime me1 - cTime)
>     fork $ playEvent insts me1 
>     playMEvs insts (eTime me1) (me2:mevs)

