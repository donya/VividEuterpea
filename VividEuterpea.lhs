Playback from Euterpea through Vivid
Donya Quick

> {-# LANGUAGE DataKinds, ExtendedDefaultRules #-}
> module VividEuterpea where
> import Vivid
> import Euterpea
> import Control.Exception
> import Control.DeepSeq

The VInstr type takes a note duration (Dur), a pitch number (AbsPitch), 
and a volume from 0-127 (Volume) and returns a Vivid synthesizer. The 
VInstr type mirrors the Instr type used in Euterpea's offline sound 
synthesis.

> type Params = [Double]
> type VInstr = Dur -> AbsPitch -> Volume -> Params -> SynthDef '[]
> data SynthInfo = SynthInfo {
>     synthDef :: VInstr, -- sound generation function
>     releaseTime :: Dur, -- what is the expected release time after note off?
>     durDependent :: Bool -- does the sound depend on note duration? 
>     }

> toSynth :: SynthInfo -> AbsPitch -> Volume -> Params -> SynthDef '[]
> toSynth e ap v p = (synthDef e) (releaseTime e) ap v p

Lookup table type for synthesizers by an Instrumentname. Note that the 
durDependent field should be True for sounds with a sustain region. 
Percussive sounds with no sustain that are primarily release-based should 
have durDependent=False to avoid excessive cycles being spent on the sound.

> type SynthTable = [(InstrumentName, SynthInfo)]

A default sound to use if no other synthesizer is defined. The 
default sound is a sine wave at a given note's frequency with 
a maximum amplitude of 0.3. The note's duration is ignored.

> defaultSound :: VInstr
> defaultSound _ ap _ _ = sd () $ do
>    s <- 0.3 ~* sinOsc (freq_ $ midiCPS ap)
>    e <- envGen (env 1.0 [(0.0,0.25)] Curve_Linear) DoNothing
>    out 0 [s ~* e, s ~* e]

> defaultSynth = SynthInfo defaultSound 0.25 False

Playback functions for sending to Vivid synths. Sequentially infinite 
Euterpea values are permitted and Ctrl+C then Enter will exit out of the 
cycle.
   
> playV :: (ToMusic1 a) => SynthTable -> Music a -> IO ()
> playV t m = onException (playMEvs t 0 $ perform m) cmdPeriod

Euterpea's PlayParams can also be used to provide a customized performance
algorithm. The playVC function allows use of the strict parameter in the
PlayParams datatype. The playVS function is just a shorthand way of doing this.

> instance NFData MEvent where
>     rnf (MEvent t i ap d v params) = 
>         rnf t `seq` rnf i `seq` rnf ap `seq` rnf d `seq` rnf v `seq` rnf params

> playVC :: (ToMusic1 a) => SynthTable -> PlayParams -> Music a -> IO ()
> playVC t pp m = 
>     let x = (perfAlg pp . toMusic1) m
>     in  if strict pp then deepseq x $ onException (playMEvs t 0 x) cmdPeriod
>         else onException (playMEvs t 0 x) cmdPeriod
    
> playVS :: (ToMusic1 a) => SynthTable -> Music a -> IO ()
> playVS t = playVC t defParams{strict=True}

Supporting definitions for handling of MEvents in the functions above.

> playEvent :: VividAction m => SynthTable -> MEvent -> m()
> playEvent insts me = 
>     let x = lookup (eInst me) insts
>         eSyn = maybe defaultSynth id x
>         sd = toSynth eSyn (ePitch me) (eVol me) (eParams me)
>         waitTime = if durDependent eSyn then eDur me + releaseTime eSyn
>                    else releaseTime eSyn
>     in  do
>             s0 <- synth sd ()
>             wait (fromRational waitTime)
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

