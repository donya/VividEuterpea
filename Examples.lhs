Examples of real-time audio playback of Euterpea values using Vivid

> {-# LANGUAGE DataKinds, ExtendedDefaultRules #-}
> import Vivid hiding (forever, line)
> import Euterpea
> import PlayV
> import Data.List hiding (transpose)

Let's define some music values.

> m1, m2 :: Music Pitch
> m1 = c 4 wn :=: (e 4 qn :+: f 4 qn :+: g 4 hn)
> m2 = forever (m1 :+: transpose 2 m1)
> m3 = line $ intersperse (rest qn) [c 4 qn, d 4 qn, e 4 qn, f 4 qn, g 4 qn]

And now some instruments.

These are much like the example on the Vivid Hackage page but they feature 
additional envelopes to control volume relative to the duration of the note 
along with a release AFTER the note-off would occur - which is not something 
that Euterpea's built-in sound synthesis supports.

> sound1 :: VInstr
> sound1 d ap _ = let d' = fromRational d in sd () $ do
>    wobble <- sinOsc (freq_ 5) ? KR ~* 10
>    s <- 0.2 ~* sinOsc (freq_ $ midiCPS ap ~+ wobble)
>    e <- envGen (env 1.0 [(1.0, d'), (0.0,0.25)] Curve_Linear) DoNothing
>    out 0 [s ~* e ,s  ~* e]

> sound2 :: VInstr
> sound2 d ap _ = let d' = fromRational d in sd () $ do
>    wobble <- sinOsc (freq_ 15) ? KR ~* 30
>    s <- 0.2 ~* sinOsc (freq_ $ midiCPS ap ~+ wobble)
>    e <- envGen (env 0.0 [(1.0,0.1),(1.0, d'-0.1), (0.0,0.5)] Curve_Linear) DoNothing
>    out 0 [s ~* e ,s  ~* e]

> sound3 :: VInstr
> sound3 d ap _ = let d' = fromRational d in sd () $ do
>    s <- 0.2 ~* sinOsc (freq_ $ midiCPS ap)
>    e <- envGen (env 0.0 [(1.0,0.01),(1.0, d'-0.01), (0.0,0.25)] Curve_Linear) DoNothing
>    out 0 [s ~* e ,s  ~* e]

Now we construct a table mapping instrument names to the definitions above so that 
we can construct Music values with instrument assignments.

> synthTable = [(CustomInstrument "Synth1", SynthInfo sound1 0.25 True), 
>               (CustomInstrument "Synth2", SynthInfo sound2 0.5 True), 
>               (CustomInstrument "Synth3", SynthInfo sound3 0.25 True)]

> m1a = instrument (CustomInstrument "Synth1") m1
> m1b = instrument (CustomInstrument "Synth2") m1
> m3a = instrument (CustomInstrument "Synth3") m3

Finally we'll hear it all together:

> main = playV synthTable (m1 :+: m1a :+: m1b :+: m3 :+: m3a)

Also try this to hear an infinite value: 
playV synthTable m2
You can use Ctrl+C and then Enter to break out of it.

------------------------------

A variation of Blue Lambda's main motifs

> x1 = c 4 en :+: g 4 en :+: c 5 en :+: g 5 en
> x2 = x1 :+: transpose 3 x1
> x3 = x2 :+: x2 :+: invert x2 :+: retro x2
> x4 = instrument (CustomInstrument "Synth3") 
>     (forever x3 :=: forever (tempo (2/3) x3))

> playXV :: (ToMusic1 a) => Music a -> IO ()
> playXV = playVC synthTable defParams{perfAlg = eventMerge . perform} where 
>     eventMerge :: Performance -> Performance
>     eventMerge (e1:e2:es) = 
>         let e1' = e1{eDur = eTime e2 + eDur e2 - eTime e1}
>         in  if ePitch e1 == ePitch e2 then eventMerge (e1':es) 
>             else e1 : eventMerge (e2:es)
>     eventMerge e = e

Try with: 
playXV x4
Use Ctrl+C then Enter to stop.

------------------------------

The code to create a bell sound in Euterpea:

 bellInstr :: Instr (AudSF () Double)
 bellInstr dur ap vol pfields = 
   let dur' = fromRational dur 
       f = apToHz ap
   in  proc () -> do
         x1 <- osc sineTable 0 -< f
         x2 <- osc sineTable 0 -< f*4.1
         x3 <- osc sineTable 0 -< f*6.05
         x4 <- osc sineTable 0 -< f*8.2
         env1 <- envLineSeg [1.0, 0.2, 0, 0] [1, 2, 100] -< ()
         env2 <- envLineSeg [0, 0.8, 0, 0] [0.05, 2, 100] -< ()
         env3 <- envLineSeg [0, 0.5, 0, 0] [0.08, 2, 100] -< ()
         env4 <- envLineSeg [0, 0.3, 0, 0] [0.015, 1, 100] -< ()
         envs <- envLineSeg [1,0.5,0.2,0,0] [0.05,0.2,3,100] -< ()
         let partials = ((x1*env1) + (x2*env2) + (x3*env3) + (x4*env4)) / 4
         outA -< 0.95 * envs * partials

And using a Vivid-based instrument:
         
> bellSynth :: VInstr
> bellSynth _ ap v = let v' = fromIntegral v / 127.0 in sd () $ do
>    x1 <- sinOsc (freq_ $ midiCPS ap)
>    x2 <- sinOsc (freq_ $ midiCPS ap ~* 4.1)
>    x3 <- sinOsc (freq_ $ midiCPS ap ~* 6.05)
>    x4 <- sinOsc (freq_ $ midiCPS ap ~* 8.2)
>    env1 <- envGen (env 1.0 [(0.2, 1.0), (0.0, 2.0)] Curve_Linear) DoNothing
>    env2 <- envGen (env 0.0 [(0.8, 0.5), (0.0, 2.0)] Curve_Linear) DoNothing
>    env3 <- envGen (env 0.0 [(0.5, 0.08), (0.0, 2.0)] Curve_Linear) DoNothing
>    env4 <- envGen (env 0.0 [(0.3, 0.015), (0.0, 1.0)] Curve_Linear) DoNothing
>    envs <- envGen (env 1.0 [(0.5, 0.05), (0.2, 2.0), (0.0, 3.0)] Curve_Linear) DoNothing
>    partials <- ((x1 ~* env1) ~+ (x2 ~* env2) ~+ (x3 ~* env3) ~+ (x4 ~* env4)) ~/ 4
>    out 0 [v' ~* 0.95 ~* envs ~* partials, v' ~* 0.95 ~* envs ~* partials]

> synthTable2 = [(CustomInstrument "Bell", SynthInfo bellSynth 5.05 False)] 

> bellScale = instrument (CustomInstrument "Bell") $ 
>     addVolume 100 $ line $ map ($qn) [c 5, d 5, e 5, f 5, g 5, a 5, b 5, c 6]

Try with: 
playV synthTable2 bellScale