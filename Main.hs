module Main where

{- 
    An FRP unit convertor
-}

import Data.Maybe
import Control.Monad

import Graphics.UI.WX hiding (Event)
import Reactive.Banana
import Reactive.Banana.WX

{-----------------------------------------------------------------------------
    Main
------------------------------------------------------------------------------}

lengthUnits = ["miles", "kilometers"]
weightUnits = ["pounds", "kilograms"]

main :: IO ()
main = start $ do
    f      <- frame         [text := "Unit Convertor", clientSize := sz 10 10]                               

    -- create statusbar field
    status <- statusField   [text := "Welcome to the Unit Convertor"]
    input1    <- entry f []
    conFrom   <- comboBox f [items := lengthUnits, selection := 0]
    conTo     <- comboBox f [items := lengthUnits, selection := 1]
    output    <- staticText f []
    opts <- get conFrom items
    nConTo   <- get conTo   selection
    nConFrom <- get conFrom selection

    let tConFrom = opts !! nConFrom
    let tConTo   = opts !! nConTo
    
    
    -- create conversion menu  
    conversion   <- menuPane            [text := "&Conversions"]
    conLength    <- menuItem conversion [text := "&Length",
                    on command := do
                                    mapM_ (\x -> set x [items := weightUnits, selection := 0]) [conFrom, conTo]
                                    nConTo   <- get conTo   selection
                                    nConFrom <- get conFrom selection
                                    let tConFrom = lengthUnits !! nConFrom
                                    let tConTo   = lengthUnits !! nConTo
                                    return ()
                    ]
    conWeight    <- menuItem conversion [text := "&Weight", 
                    on command := do
                                    mapM_ (\x -> set x [items := weightUnits, selection := 0]) [conFrom, conTo]
                                    nConTo   <- get conTo   selection
                                    nConFrom <- get conFrom selection
                                    let tConFrom = weightUnits !! nConFrom
                                    let tConTo   = weightUnits !! nConTo
                                    return ()
                    ]
    quit   <- menuQuit conversion [help := "Quit the demo", on command := close f]

    -- create Help menu
    hlp    <- menuHelp      []
    about  <- menuAbout hlp [help := "About Unit convertor"]
    
    set f [ statusBar := [status]
            , menuBar   := [conversion,hlp]
            ,on (menu about) := infoDialog f "About wxHaskell" "This is a Unit Convertor",
            layout := margin 10 $ row 10
            [widget input1, label "From: ", widget conFrom, 
             label "To: ", widget conTo
            , minsize (sz 150 20) $ widget output]]
     
    let networkDescription :: MomentIO ()
        networkDescription = do
        
        binput1  <- behaviorText input1 ""
        
        
        let
            result :: Behavior (Maybe Double)
            result = convert (tConFrom, tConTo) <$> binput1
            showNumber   = maybe "--" show
    
        sink output [text :== showNumber <$> result]   

    network <- compile networkDescription    
    actuate network

readNumber s = listToMaybe [x | (x,"") <- reads s]  

convert (from, to) x 
    | from == "miles" && to == "kilometers" = liftA2 (*) (readNumber x) (Just 1.60934)
    | from == "kilometers" && to == "miles" = liftA2 (/) (readNumber x) (Just 1.60934)
    | otherwise                             = liftA (id) (readNumber x)
