{- a lot of the code in this is inefficient, and just for messing around
with different effects. Specifically vector -> list -> vector is very
inefficient. Some of these sound absolutely disgusting. Might want to turn
down your volume! -}

import Prelude hiding (readFile, take, reverse)
import Control.Concurrent (threadDelay)
import System.Exit (ExitCode)
import Foreign.Storable (Storable)
import Text.Printf (printf)
import Control.Monad (void)
import Sound.Sox.Play (simple)
import Sound.Sox.Option.Format (numberOfChannels)

import Sound.File.Sndfile (Info, readFile, frames, samplerate, channels, 
                           sections, duration)

import Sound.File.Sndfile.Buffer.StorableVector (fromBuffer)
import Synthesizer.Generic.Cut (Transform)

import Synthesizer.Generic.Filter.NonRecursive (downsample, fadeInOut, amplify,
                                                downsample2, envelope)

import Synthesizer.Generic.Filter.Recursive.Comb (run)
import Synthesizer.Generic.Filter.Delay (static)
import Synthesizer.Generic.Signal (LazySize(..), take, mix, append, reverse)
import Synthesizer.Generic.Analysis (average)
import Synthesizer.Generic.Noise (white)
import Synthesizer.Basic.Distortion (powerSigned)

import qualified Synthesizer.Generic.Loop as Loop (fade, simple)
import qualified Synthesizer.Generic.Displacement as Distort (distort, mixMulti)
import qualified Data.StorableVector as V (Vector, unpack)

import qualified Data.StorableVector.Lazy as LazyV (Vector(..), hPut, repeat, 
                                                    defaultChunkSize, map, 
                                                    minimum, pack)

main :: IO ()
main = do
    (info, Just test1) <- readFile "SmokeOnTheWater.ogg"
    (_, Just test2) <- readFile "FallenDownTheWire.ogg"
    (_, Just test3) <- readFile "Normal.ogg"

    printf "The file has %d frames, a sample rate of %d, %d channels, \
            \%d sections, and lasts for %f seconds\n" (frames info)
            (samplerate info) (channels info) (sections info) (duration info)

    let snippet = take tenSeconds (toLazy $ fromBuffer test1)
        snippet2 = take tenSeconds (toLazy $ fromBuffer test2)
        snippet3 = take tenSeconds (toLazy $ fromBuffer test3)
        tenSeconds = numSeconds info 10.0

        playEffect' f msg = playEffect snippet f msg info

        fadeLoop = take tenSeconds . Loop.fade (0.5 :: Double) 
                   (numSeconds info 2) (numSeconds info 2)

        loop = take tenSeconds . Loop.simple (numSeconds info 1)
               (numSeconds info 3)

        downSample x s = simple LazyV.hPut (numberOfChannels 2)
                         (round $ 88200 * (1/ fromIntegral x)) 
                         (downsample (LazySize 1) x s)

        --fade in time, normal volume time, fade out time
        fade = fadeInOut (numSeconds info 3) (numSeconds info 5)
                         (numSeconds info 3)

        distort = amplify 0.05 . Distort.distort powerSigned
                  (LazyV.repeat LazyV.defaultChunkSize 0)

        noisy s = LazyV.map (\x -> if x > average s then x + 0.1 else x - 0.1) s

        actions = [(id, "Original file..."),
                   (downsample2 (LazySize 1), "Speed up..."),
                   (eightbit, "Rounded amplitude range..."),
                   (removeSmallChanges, "Remove small changes in amplitude..."),
                   (boostBigChanges, "Increase large changes in amplitude..."),
                   (mix (static (numSeconds info 0.1) snippet), "Delay..."),
                   (LazyV.map clip', "Clipped..."),
                   (\x -> Distort.mixMulti [x, snippet2, snippet3], 
                    "Mix multi songs"),
                   (distort, "Distortion..."),
                   (fadeLoop, "Fade loop"),
                   (fade, "Fade..."),
                   (amplify 0.1, "Decrease volume..."),
                   (envelope (white $ LazySize 1), "Splice with white noise"),
                   (loop, "Loop"),
                   (reverse, "Reversed"),
                   (mix snippet2, "Mixed with another track..."),
                   (run (numSeconds info 1) (1 :: Double), "Echoes..."),
                   (reverse' info, "Half reverse..."),
                   (noisy, "Noisy...")
                  ]

    mapM_ (uncurry playEffect') actions

    -- can't do this with playEffect' because sample rate when playing needs
    -- to be decreased so it plays at normal speed
    void $ putStrLn "Downsample..." >> downSample 16 snippet

removeSmallChanges :: (Fractional y, Ord y, Storable y) => LazyV.Vector y
                                                        -> LazyV.Vector y
removeSmallChanges = LazyV.map (\x -> if x < 0.02 then 0 else x)

boostBigChanges :: (Fractional y, Ord y, Storable y) => LazyV.Vector y 
                                                     -> LazyV.Vector y
boostBigChanges = LazyV.map (\x -> if x > 0.05 then x+0.05 else x)

eightbit :: LazyV.Vector Double -> LazyV.Vector Double
eightbit file = LazyV.map (\x -> (fromIntegral x / 16) - min') ints
    where ints = LazyV.map (round . (*16)) normalized :: LazyV.Vector Int
          normalized = LazyV.map (+ min') file
          min' = LazyV.minimum file

-- this is bad - it's inefficent as hell, and there's got to be a better
-- way! fromBuffer returns a Data.StorableVector, but play wants a
-- Data.StorableVector.Lazy. For now this is a quick hack, but if performance
-- starts being needed, you should definitely fix this
toLazy :: (Storable a) => V.Vector a -> LazyV.Vector a
toLazy xs = LazyV.pack LazyV.defaultChunkSize $ V.unpack xs

-- need to set sample rate to sample rate * numchannels - plays in half speed
-- otherwise. I guess sample rate is per channel?
play :: LazyV.Vector Double -> Info -> IO ExitCode
play file info = simple LazyV.hPut (numberOfChannels numChannels)
                                   (samplerate info * numChannels) file
    where numChannels = channels info

numSeconds :: Info -> Double -> Int
numSeconds info n = round $ n * fromIntegral (samplerate info) 
                              * fromIntegral (channels info)

playEffect :: t -> (t -> LazyV.Vector Double) -> String -> Info -> IO ()
playEffect signal f msg info = do
    putStrLn msg
    void $ play (f signal) info
    threadDelay oneSecond
    where oneSecond = 1000000

reverse' :: Synthesizer.Generic.Cut.Transform t => Info -> t -> t
reverse' info sig = append half (reverse half)
    where half = take (numSeconds info 5) sig

clip' :: (Fractional a, RealFrac a1) => a1 -> a
clip' a = round' a 1
    where round' num digits = (fromInteger . round $ num * (10^digits)) / 
                                                           (10.0^^digits)
