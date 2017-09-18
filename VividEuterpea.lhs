Playback from Euterpea through Vivid
Donya Quick

> {-# LANGUAGE DataKinds, ExtendedDefaultRules #-}
> module VividEuterpea where
> import Vivid
> import Euterpea
> import Control.Exception
> import Control.DeepSeq
> import System.Info
> import WriteNRTWin

The VInstr type takes a note duration (Dur), a pitch number (AbsPitch), 
and a volume from 0-127 (Volume) and returns a Vivid synthesizer. The 
VInstr type mirrors the Instr type used in Euterpea's offline sound 
synthesis.

> type Params = [Double]
> type VInstr = Dur -> AbsPitch -> Volume -> Params -> SynthDef '["gate", "fadeSecs"]

> data ReleaseType = 
>     FixedDuration -- sound duration unaffected by note duration (percussive, frees itself)
>     | Internal -- sound duration handled within synth def with envelopes (frees itself)
>     | External -- fade out and free expected to be handled externally to synth def
>     deriving (Eq, Show, Ord, Enum)

> data SynthInfo = SynthInfo {
>     synthDef :: VInstr, -- sound generation function
>     releaseTime :: Dur, -- what is the expected release time after note off?
>     releaseType :: ReleaseType -- does the sound depend on note duration? 
>     }

> toSynth :: SynthInfo -> AbsPitch -> Volume -> Params -> SynthDef '["gate", "fadeSecs"]
> toSynth e ap v p = (synthDef e) (releaseTime e) ap v p

Notes on ReleaseType:
- Synthesizers using FixedDuration are expected to NOT free themselves via envelopes. 
  They will be freed by the playback algorithm. Freeing internally with envelopes 

Lookup table type for synthesizers by an Instrumentname. Note that the 
durDependent field should be True for sounds with a sustain region. 
Percussive sounds with no sustain that are primarily release-based should 
have durDependent=False to avoid excessive cycles being spent on the sound.

> type SynthTable = [(InstrumentName, SynthInfo)]

A default sound to use if no other synthesizer is defined. The 
default sound is a sine wave at a given note's frequency with 
a maximum amplitude of 0.3. The note's duration is ignored, 
meaning that the SynthInfo will have a releaseType of FixedDuration.

> defaultSound :: VInstr
> defaultSound _ ap _ _ = sd (1 ::I "gate", 0 ::I "fadeSecs") $ do
>    s <- 0.3 ~* sinOsc (freq_ $ midiCPS ap)
>    e <- envGen (env 1.0 [(0.0,0.25)] Curve_Linear) FreeEnclosing
>    out 0 [s ~* e, s ~* e]

> defaultSynth = SynthInfo defaultSound 0.25 FixedDuration

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
>         waitTime = if releaseType eSyn == Internal then eDur me + releaseTime eSyn else
>                    if releaseType eSyn == External then eDur me else releaseTime eSyn
>     in  if releaseType eSyn == External then do
>             s0 <- synth sd ()
>             wait $ fromRational waitTime
>             set s0 (fromRational (releaseTime eSyn) :: I "fadeSecs")
>             release s0
>             wait $ releaseTime eSyn
>         else do -- for fixed and internal release cases
>             s0 <- synth sd ()
>             wait (fromRational waitTime)
>             -- Note: call to free removed. Synth defs are expected to do this themselves now.
     
> playMEvs :: VividAction m => SynthTable -> PTime -> [MEvent] -> m ()
> playMEvs insts cTime [] = return ()
> playMEvs insts cTime [me] = fork $ do
>     wait $ fromRational (eTime me - cTime)
>     playEvent insts me
> playMEvs insts cTime (me1:me2:mevs) = do
>     wait $ fromRational (eTime me1 - cTime)
>     fork $ playEvent insts me1 
>     playMEvs insts (eTime me1) (me2:mevs)

Writing to a WAV file

> writeWavV :: (ToMusic1 a) => FilePath -> SynthTable -> Music a -> IO ()
> writeWavV outFile t m = 
>     if os == "mingw32" then writeNRTWin outFile (playMEvs t 0 $ perform m)
>     else writeNRT outFile (playMEvs t 0 $ perform m)

