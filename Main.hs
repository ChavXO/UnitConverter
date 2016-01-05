module Main where

import Data.Maybe
import Control.Monad

import Graphics.UI.WX hiding (Event)
import Reactive.Banana
import Reactive.Banana.WX


main :: IO ()
main = start converter

lengthUnits = ["miles", "kilometers"]
weightUnits = ["pounds", "kilograms"]
units = ["Length", "Weight"]

converter :: IO ()
converter = do
    f <- frame [text := "Unit Converter"
                , clientSize := sz 500 150]
    p <- panel f []
    unitCon   <- comboBox p [items := units, selection := 0]
    conBox1   <- comboBox p [items := lengthUnits, selection := 0]
    conBox2   <- comboBox p [items := lengthUnits, selection := 1]
    input1    <- textCtrl p []
    input2    <- textCtrl p []
    set unitCon [ clientSize := sz 460 50
                , position := pt 20 10]
    set input1  [ clientSize := sz 220 30
                , position := pt 20 70]
    set input2  [ clientSize := sz 220 30
                , position := pt 260 70]
    set conBox1 [ clientSize := sz 220 30 
                , position := pt 20 110]
    set conBox2 [ clientSize := sz 220 30 
                , position := pt 260 110]    
    let networkDescription :: MomentIO ()
        networkDescription = do
        
        binput1  <- behaviorText input1 ""
        unit1 <- newBehavior conBox1
        unit2 <- newBehavior conBox2
        
        let
            result :: Behavior (Maybe Double)
            result = convert (unit1, "kilometers") <$> binput1
            showNumber   = maybe "--" show
    
        sink input2 [text :== showNumber <$> result]   

    network <- compile networkDescription    
    actuate network

readNumber s = listToMaybe [x | (x,"") <- reads s]  

convert (from, to) x 
    | from == "miles" && to == "kilometers" = liftA2 (*) (readNumber x) (Just 1.60934)
    | from == "kilometers" && to == "miles" = liftA2 (/) (readNumber x) (Just 1.60934)
    | otherwise                             = liftA (id) (readNumber x)
