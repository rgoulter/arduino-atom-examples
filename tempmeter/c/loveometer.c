// ADC code adapted from:
//   https://sites.google.com/site/qeewiki/books/avr-guide/analog-input
// for Project 03 of the Arduino Projects Book

#include <stdio.h>
#include <stdint.h>
#include <util/delay.h>

#include <avr/io.h>
#include <avr/interrupt.h>

#define BLINK_DELAY_MS 250

#define F_CPU 16000000
#define BAUD 9600
#define MYUBRR (((((F_CPU * 10) / (16L * BAUD)) + 5) / 10))

FILE mystdout;

volatile uint16_t ADCvalue;    // Global variable, set to volatile if used with ISR

int uart_putchar(char c, FILE *stream) {
    if (c == '\n')
        uart_putchar('\r', stream);
    loop_until_bit_is_set(UCSR0A, UDRE0);
    UDR0 = c;

    return 0;
}

void ioinit(void) {
    UBRR0H = MYUBRR >> 8;
    UBRR0L = MYUBRR;
    UCSR0B = (1<<RXEN0)|(1<<TXEN0);

    fdev_setup_stream(&mystdout, uart_putchar, NULL, _FDEV_SETUP_WRITE);
    stdout = &mystdout;
}

void initadc(void) {
    ADMUX = 0;                // use ADC0
    ADMUX |= (1 << REFS0);    // use AVcc as the reference

    // Right adjusted by default
    // ADMUX |= (1 << ADLAR);

    ADCSRA |= (1 << ADPS2) | (1 << ADPS1) | (1 << ADPS0); // 128 prescale for 16Mhz
    ADCSRA |= (1 << ADATE);   // Set ADC Auto Trigger Enable

    ADCSRB = 0;               // 0 for free running mode

    ADCSRA |= (1 << ADEN);    // Enable the ADC
    ADCSRA |= (1 << ADIE);    // Enable Interrupts

    ADCSRA |= (1 << ADSC);    // Start the ADC conversion

    sei();                    // global interrupt enable
}

int main(void) {
    ioinit();
    initadc();

    // init LEDs
    DDRD |= _BV(DDD2) | _BV(DDD3) | _BV(DDD4);
    PORTD &= ~(_BV(PORTD2) | _BV(PORTD3) | _BV(PORTD4));

    while(1) {
        float voltage = (ADCvalue / 1024.0) * 5.0;
        float temperature = (voltage - 0.5) * 100;

        const float baselineTemp = 26.0;
        if (temperature < baselineTemp) {
            PORTD &= ~(_BV(PORTD2) | _BV(PORTD3) | _BV(PORTD4));
        } else if (temperature < baselineTemp + 1) {
            PORTD |= _BV(PORTD2);
            PORTD &= ~(_BV(PORTD3) | _BV(PORTD4));
        } else if (temperature < baselineTemp + 2) {
            PORTD |= _BV(PORTD2) | _BV(PORTD3);
            PORTD &= ~(_BV(PORTD4));
        } else {
            PORTD |= _BV(PORTD2) | _BV(PORTD3) | _BV(PORTD4);
        }

        // Compile with:
        //   -Wl,-u,vfprintf -lprintf_flt -lm
        // to enable printf with float-conversions.
        printf("Sensor Val: %d, Volts: %f, degrees C: %f\n", ADCvalue, voltage, temperature);

        _delay_ms(BLINK_DELAY_MS);
    }

    return(0);
}

ISR(ADC_vect) {
    uint8_t l = ADCL;
    uint8_t h = ADCH;
    ADCvalue = (h << 8) + l;
    // REMEMBER: once ADCH is read the ADC will update
}
