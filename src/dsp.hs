import System.Exit (ExitCode)
import Sound.Sox.Play
import Sound.Sox.Option.Format
import Sound.File.Sndfile
import Sound.File.Sndfile.Buffer.StorableVector
import qualified Data.StorableVector as V
import qualified Data.StorableVector.Lazy as LazyV
import Synthesizer.Generic.Cut
import Synthesizer.Generic.Filter.NonRecursive
import Synthesizer.Generic.Filter.Recursive.Comb
import Synthesizer.Generic.Signal
import Synthesizer.Generic.Noise
import qualified Synthesizer.Generic.Loop as Loop
import Foreign.Storable
import qualified Control.Concurrent.Thread.Delay as Delay
import Text.Printf
import Control.Monad
import Prelude hiding (readFile, take, negate, reverse)

main :: IO ()
main = do
    (info, Just test1) <- readFile "test.flac"
    (_, Just test2) <- readFile "test2.flac"

    printf "The file has %d frames, a sample rate of %d, %d channels, \
            \%d sections, and lasts for %f seconds\n" (frames info)
            (samplerate info) (channels info) (sections info) (duration info)

    let snippet = take (numSeconds info 11) (toLazy $ fromBuffer test1)
        snippet2 = take (numSeconds info 11) (toLazy $ fromBuffer test2)

    let playEffect' f msg = playEffect snippet f msg info

    playEffect' id "Original file..."

    -- fade in time, normal volume time, fade out time
    playEffect' (fadeInOut (numSeconds info 3) (numSeconds info 5) 
                (numSeconds info 3)) "Fade..."

    playEffect' (downsample2 (LazySize 8)) "This is apparently a downsample, I\
                                           \ must be doing it wrong. It sounds\
                                           \ funny though..."

    playEffect' (amplify 0.1) "Negative amplify..."

    playEffect' (envelope (white $ LazySize 8)) 
                "Splicing with some disgusting white noise..."

    playEffect' (take (numSeconds info 11) . Loop.simple (numSeconds info 1)
                (numSeconds info 3)) "Loop..."

    playEffect' reverse "Reversed..."

    playEffect' (mix snippet2) "Mixed..."

    playEffect' (run (numSeconds info 1) (1 :: Double)) "Echoes..."

{- this is VERY bad - it's inefficent as hell, and there's got to be a better
way! fromBuffer returns a Data.StorableVector, but play wants a
Data.StorableVector.Lazy. For now this is a quick hack, but if performance
starts being needed, you should definitely fix this -}
toLazy :: (Storable a) => V.Vector a -> LazyV.Vector a
toLazy xs = LazyV.pack LazyV.defaultChunkSize $ V.unpack xs

{- need to set sample rate to sample rate * numchannels - plays in half speed
otherwise. I guess sample rate is per channel? -}
play :: LazyV.Vector Double -> Info -> IO ExitCode
play file info = simple LazyV.hPut (numberOfChannels numChannels)
                                   (samplerate info * numChannels) file
    where numChannels = channels info

numSeconds :: Info -> Int -> Int
numSeconds info n = n * samplerate info * channels info

playEffect :: t -> (t -> LazyV.Vector Double) -> String -> Info -> IO ()
playEffect signal f msg info = do
    putStrLn msg
    void $ play (f signal) info
    Delay.delay oneSecond
    where oneSecond = 1000000
