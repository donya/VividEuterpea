Examples of real-time audio playback of Euterpea values using Vivid

> {-# LANGUAGE DataKinds, ExtendedDefaultRules #-}
> import Vivid hiding (forever)
> import Euterpea
> import PlayV

> m1, m2 :: Music Pitch
> m1 = c 4 wn :=: (e 4 qn :+: f 4 qn :+: g 4 hn)
> m2 = forever (m1 :+: transpose 2 m1)

> sound1 :: SynthDef '["note"]
> sound1 = sd (0 ::I "note") $ do
>    wobble <- sinOsc (freq_ 5) ? KR ~* 10
>    s <- 0.3 ~* sinOsc (freq_ $ midiCPS (V::V "note") ~+ wobble)
>    out 0 [s,s]

> sound2 :: SynthDef '["note"]
> sound2 = sd (0 ::I "note") $ do
>    wobble <- sinOsc (freq_ 15) ? KR ~* 30
>    s <- 0.3 ~* sinOsc (freq_ $ midiCPS (V::V "note") ~+ wobble)
>    out 0 [s,s]

> synthTable = [("Synth1", sound1), ("Synth2", sound2)]

> cInst :: String -> Music a -> Music a
> cInst str = instrument (CustomInstrument str)

> m1a = cInst "Synth1" m1
> m1b = cInst "Synth2" m1

> main = playV synthTable (m1 :+: m1a :+: m1b)

Also try this to hear an infinite value: 
playV synthTable m2
You can use Ctrl+C and then Enter to break out of it.

------------------------------

A variation of Blue Lambda's main motifs

> x1 = c 4 en :+: g 4 en :+: c 5 en :+: g 5 en
> x2 = x1 :+: transpose 3 x1
> x3 = x2 :+: x2 :+: invert x2 :+: retro x2
> x4 = forever x3 :=: forever (tempo (2/3) x3)

> playXV :: (ToMusic1 a) => Music a -> IO ()
> playXV = playVC synthTable defParams{perfAlg = eventMerge . perform} where 
>     eventMerge :: Performance -> Performance
>     eventMerge (e1:e2:es) = 
>         let e1' = e1{eDur = eTime e2 + eDur e2 - eTime e1}
>         in  if ePitch e1 == ePitch e2 then eventMerge (e1':es) 
>             else e1 : eventMerge (e2:es)
>     eventMerge e = e

Try with: 
playXV [] x4
Use Ctrl+C then Enter to stop.