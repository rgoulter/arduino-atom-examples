{-# LANGUAGE QuasiQuotes #-}
module Spaceship (main) where

import Text.Heredoc
import Language.Atom

greenLED = "PORTD3"
redLED1  = "PORTD4"
redLED2  = "PORTD5"

ledOn  led = action (\v -> "PORTD |= _BV(" ++ led ++ ")")  []
ledOff led = action (\v -> "PORTD &= ~_BV(" ++ led ++ ")") []

-- | Our main Atom program.
--   Periodically blinks the LED
spaceship :: Atom ()
spaceship = do
    switchState <- bool "switchState" True

    -- read into switchState
    call "readButton"

    atom "standBy" $ do
        cond $ not_ (value switchState)
        ledOn greenLED
        ledOff redLED1
        ledOff redLED2

    atom "blinking" $ do
        cond (value switchState)
        ledOff greenLED

        let halfDelay = 25000
        let blinkPeriod = 2 * halfDelay

        period blinkPeriod $ phase 0 $ atom "blink1" $ do
            ledOff redLED1
            ledOn  redLED2

        period blinkPeriod $ phase halfDelay $ atom "blink2" $ do
            ledOn  redLED1
            ledOff redLED2

cHeader :: String
cHeader = [here|
#include <avr/io.h>

static inline void readButton(void);
|]

cFooter :: String
cFooter = [here|
static inline void readButton() {
    // read switch state into the Atom variable
    state.Spaceship.switchState = (PIND & _BV(PORTD2)) != 0;
}

int main (void) {
    // Set input PD2,
    // Set output PD3, PD4, PD5
    DDRD &= ~_BV(DDD2);
    DDRD |= _BV(DDD3) | _BV(DDD4) | _BV(DDD5);

    while(1) {
        Spaceship();
    }

    return 0; // Never reaches
}
|]

main :: IO ()
main = do
    let atomName = "Spaceship"
    let code _ _ _ = (cHeader, cFooter)
    let cfg = defaults {cCode = code,
                        cRuleCoverage = False,
                        cAssert = False}
    (schedule, _, _, _, _) <- compile atomName cfg spaceship
    putStrLn $ reportSchedule schedule
