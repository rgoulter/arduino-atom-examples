{-# LANGUAGE QuasiQuotes #-}
{-|
Minimalistic Atom reset example for ATMega328P.
The LED is assumed to be on PB5 (e.g. Arduino Uno).

No Arduino Libraries are required.

Compilation example:
@
runghc Blink.hs
avr-gcc -o Blink.elf -Os -Wall -mmcu=atmega328p -DF_CPU=16000000L blink.c
avr-objcopy -O ihex -R .eeprom Blink.elf Blink.hex
@
-}
module Blink (main) where

import Text.Heredoc
import Language.Atom

-- | Our main Atom program.
--   Periodically blinks the LED
blink :: Atom ()
blink = do
    -- Declare a local state variable
    --  that controls if the LED is on or off
    on <- bool "on" True
    -- Every 50000'th call of blink() ...
    -- period 50000 $ atom "blinkOn" $ do
    period 50000 $ atom "blinkOn" $ do
        -- invert LED state
        on <== not_ (value on)
        -- and write the new state to PORTB
        call "showLED"

-- | C code that will be inserted BEFORE the atom code
cHeader :: String
cHeader = [here|
#include <avr/io.h>
#include <util/delay.h>

static inline void showLED(void);
|]

-- | C code that will be inserted AFTER the atom code
cFooter :: String
cFooter = [here|
//LED: PB5
#define LED_PIN (1<<5)

//Set/reset the LED
static inline void showLED() {
    if(state.blink.on) {
        PORTB |= LED_PIN;
    } else {
        PORTB &= ~(LED_PIN);
    }
}

int main (void) {
    //Set LED pin to OUTPUT
    DDRB |= LED_PIN;
    while(1) {
        blink();
    }
    return 0; //Never reache
}
|]

main :: IO ()
main = do
    let code _ _ _ = (cHeader, cFooter)
    let cfg = defaults {cCode = code,
                        cRuleCoverage = False,
                        cAssert = False}
    (schedule, _, _, _, _) <- compile "blink" cfg blink
    putStrLn $ reportSchedule schedule
