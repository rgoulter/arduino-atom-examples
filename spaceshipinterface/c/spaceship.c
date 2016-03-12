#include <avr/io.h>
#include <util/delay.h>

#define BLINK_DELAY_MS 250

int main (void) {
    /* set inputs:
     *   PD2
     * and outputs:
     *   PD3, PD4, PD5
     */
    DDRD &= ~_BV(DDD2);
    DDRD |= _BV(DDD3) | _BV(DDD4) | _BV(DDD5);

    while(1) {
        /* read state of switch from PortD2 */
        unsigned char switchState = PIND & _BV(PORTD2);

        if (switchState == 0) {
            /* enable PortD3 (green), disable PortD4 & PortD5 (red) */
            PORTD |= _BV(PORTD3);
            PORTD &= ~(_BV(PORTD4) | _BV(PORTD5));
        } else {
            /* disable PortD3 (green) */
            PORTD &= ~_BV(PORTD3);

            /* disable PortD4, enable PortD5 (red) */
            PORTD &= ~_BV(PORTD4);
            PORTD |= _BV(PORTD5);
            _delay_ms(BLINK_DELAY_MS);

            /* enable PortD4, disable PortD5 (red) */
            PORTD |= _BV(PORTD4);
            PORTD &= ~_BV(PORTD5);
            _delay_ms(BLINK_DELAY_MS);
        }
    }
}
