;vfdFu24.asm V0.7 QRT 180729
;
;fuse bits          cksel3...0     1101    ceramic/crystal 3..8 MHz                     <-
;                   cksel3...0     1111    ceramic/crystal 8..  MHz
;                   sut1..0          00    ceramic resonator, slow rising power
;                                    11    crystal oscillator, slow rising power        <-
;
;                   cksel3...0     0010    for calibrated internal RC oscillator 4 MHz
;                   cksel3...0     0100    for calibrated internal RC oscillator 8 MHz
;                   sut1..0          10    RC oscillator, slow rising power
;
;                   bodlevel2..0    111    off   brownout detection                     <-
;                                   110    1.8 V
;                                   101    2.7 V
;                                   100    4.3 V

;-------------------------------------------------------------------------------

;V0.5   plain char display
;V0.6   vertical scroll chars
;V0.7   scroll new/different

;-------------------------------------------------------------------------------
;0 no debug, 1 counter output
.define DEBUG   0

;0 off, 1 on
.define TESTDISPLAY    0

;-------------------------------------------------------------------------------

.include "tn2313def.inc"

;-------------------------------------------------------------------------------

.cseg
.org $0000
rjmp main                                   ;Reset Handler
;.org $0001
;rjmp INT0_IR                               ;External Interrupt0 Handler
;.org $0002
;rjmp INT1_IR                               ;External Interrupt1 Handler
;.org $0003
;rjmp TIM1_CAPT_IR                          ;Timer1 Capture Handler

TIMER1_COMPB_IR:
        set                                 
        rjmp    shla            

.org $0004
TIM1_COMPA_IR:                              ;Timer1 CompareA Handler
        clt    
shla:   out     VFDB_I,r8                   ;toggle shift latch, r8 = SLATR             
        out     VFDB_I,r8                                 
        reti

;.org $0005
;rjmp TIM1_OVF_IR                           ;Timer1 Overflow Handler
;.org $0006
;rjmp TIM0_OVF_IR                           ;Timer0 Overflow Handler
;.org $0007
;rjmp USART0_RXC_IR                         ;USART0 RX Complete Handler
;.org $0008
;rjmp USART0_DRE_IR                         ;USART0,UDR Empty Handler
;.org $0009
;rjmp USART0_TXC_IR                         ;USART0 TX Complete Handler
;.org $000a
;rjmp ANA_COMP_IR                           ;Analog Comparator Handler
;.org $000b
;rjmp PCINT_IR                              ;Pin Change Interrupt

.org $000c
        rjmp    TIMER1_COMPB_IR             ;Timer1 CompareB Handler

.org $000d
        rjmp    TIMER0_COMPA_IR             ;Timer0 Compare A Handler

.org $000e
TIMER0_COMPB_IR:                            ;Timer0 Compare B Handler
        out     VFDA_I,r4                   ;toggle filament supply, r4 = FISUTR
        reti

;.org $000f
;rjmp USI_START_IR                          ;USI Start Handler
;.org $0010
;rjmp USI_OVERFLOW_IR:                      ;USI Overflow Handler
;.org $0011
;rjmp EE_READY_IR                           ;EEPROM Ready Handler
;.org $0012
;rjmp WDT_OVERFLOW_IR                       ;Watchdog Overflow Handler

;-------------------------------------------------------------------------------

TIMER0_COMPA_IR:
        cbi     VFDA_P,FIC_A                ;filament supply off
        reti

;-------------------------------------------------------------------------------

.def    a0          =   r0                  ;main registers set a
.def    a1          =   r1
.def    a2          =   r2
.def    a3          =   r3
.def    a4          =   r24                 ;main registers set a immediate
.def    a5          =   r25
.def    a6          =   r16
.def    a7          =   r17

.def    NULR        =   r15                 ;NULL register

.def    FISUTR      =   r4                  ;filament supply constant
.def    USIOIFR     =   r5                  ;USI OV flag
.def    SCLKR       =   r6                  ;shift clock
.def    SDATR       =   r7                  ;      data
.def    SLATR       =   r8                  ;      latch

.def    cuchp       =   r18                 ;custom char pointer
.def    cuchc       =   r19                 ;            counter

.if TESTDISPLAY==1
.def    tcnt        =   r20                 ;test counter
.def    tchr        =   r21                 ;     char
.endif

;XL text pointer write, YL text pointer read

;-------------------------------------------------------------------------------

;GPIOR0 bits
.equ    SCROL_EN    =   0                   ;scroll enable
.equ    SCROL_DC    =   1                   ;       0=new chars, 1=different new chars

;GPIOR1 bits
;GPIOR2 bits

;-------------------------------------------------------------------------------

.equ    VFDA_P      =   PORTB               ;VFD A port
.equ    VFDA_D      =   DDRB                ;      ddr
.equ    VFDA_I      =   PINB                ;      pin
.equ    SPI_SCL     =   PINB7               ;      SPI clock            in      c
.equ    SPI_DO      =   PINB6               ;          data             out     o
.equ    SPI_DI      =   PINB5               ;          data             in      i
.equ    SPI_SS      =   PINB4               ;          select           in      s
.equ    SUPPLY      =   PINB3               ;      supply               out     p
.equ    FIC_A       =   PINB2               ;      filament clock OC0A  out     a
.equ    SHI_DA      =   PINB1               ;      shift data           out     d
.equ    SHI_CLK     =   PINB0               ;            clock          out     l

.equ    VFDB_P      =   PORTD               ;VFD B port
.equ    VFDB_D      =   DDRD                ;      ddr
.equ    VFDB_I      =   PIND                ;      pin
.equ    SHI_LA      =   PIND6               ;      shift latch          out     l
.equ    FIC_B       =   PIND5               ;      filament clock OC0B  out     b

.equ    COUNT_P     =   PORTD               ;count port
.equ    COUNT_D     =   DDRD                ;      ddr
.equ    COUNT_I     =   PIND                ;      pin
.equ    CRESET      =   PIND1               ;      reset    H           out     r
.equ    CCLK        =   PIND0               ;      clock    H->L        out     c

;                         coispadl          c spiclk o spiout i spiin s spisel p supply a fila d shidat l shiclk
;                         IOIIOOOO          I input O output L low H high
;                         PLPPYLL          P pullup N no pullup
.equ    DDRBM       =   0b01001111
.equ    PORTBM      =   0b10110000

.if DEBUG==0
;                         -lbuuuuu          l latch b filb u unused
;                         -OOIIIII
;                         -LLNNNNN
.equ    DDRDM       =   0b01100000
.equ    PORTDM      =   0b00000000
.elif DEBUG==1
;                         -lbuuurc          l latch b filb r cntres c cntclk u unused
;                         -OOIIIOO
;                         -LLNNNLH
.equ    DDRDM       =   0b01100011
.equ    PORTDM      =   0b00000001
.endif

;-------------------------------------------------------------------------------

.equ    NUMDIGITS   =   24                      ;number of digits
.equ    BRISTEPS    =   64                      ;brightness steps

.equ    CMD_SETCUR  =   0                       ;command set cursor 0..NUMDIGITS-1

.equ    CCO_FIRST   =   32                      ;char code first
.equ    CCO_CUST0   =   128                     ;          custom 0..7
.equ    CCO_LAST    =   135                     ;          last

.equ    CMD_CLRHOME =   136                     ;command clear home
.equ    CMD_SUPOFF  =   137                     ;        supply off
.equ    CMD_SUPON   =   138                     ;               on
.equ    CMD_SETCUCH =   139                     ;        set custom char
.equ    CMD_SCLOFF  =   140                     ;        scroll off
.equ    CMD_SCLON   =   141                     ;               on
.equ    CMD_SCLDOFF =   142                     ;               different off
.equ    CMD_SCLDON  =   143                     ;                         on

.equ    CMD_BRIGHT  =   192                     ;command brightness 192..255 (64 steps)

;-------------------------------------------------------------------------------

.equ    text        =   SRAM_START              ;text buffer, NUMDIGITS byte
.equ    stext       =  (text+NUMDIGITS)         ;scroll text
.equ    stime       =  (stext+NUMDIGITS)        ;       timer
.equ    cuchRam     =  (stime+NUMDIGITS)        ;custom char RAM, 8 * 5 byte

;-------------------------------------------------------------------------------
;onRatio = (VFR_REC - DARK_CYC) / VFR_REC, (3000 - 800) / 3000 = 0.73   (< 0.75)
;refresh = fosc / VFR_REC / NUMDIGITS,     4E6 / 3000 / 24     = 55 Hz 
.equ    VFR_REC     =   3000                    ;VFD refresh cycle
.equ    VFR_DACMIN  =   800                     ;    bright             (> 600)
.equ    VFR_DACMAX  =   2600                    ;    dark            
.equ    BRIDIFF     =   VFR_DACMAX-VFR_DACMIN   ;bright difference
.equ    BRISTEP     =   BRIDIFF/BRISTEPS        ;(2600-800)/64 = 28 = 2^5 - 4

;Ueff = (FILAPOW / 256) * Us, Us = 5 - 2 * 0.7 = 3.6 V
.equ    FILAPOW     =   192                     ;filament power 2.7 V

.equ    SCCYC       =   6                       ;scroll cycle

.equ    TDELAY      =   250                     ;test delay

;-------------------------------------------------------------------------------

.macro shiftOut
.endmac

.macro shiftOut_i
        ldi     a5,(1<<@0)                  ;get current state of data line
        sbis    VFDA_P,SHI_DA
        clr     a5
        
        eor     a5,a0                       ;current state ^ column 4 row bit @0
        
        sbrc    a5,@0                       ;toggel necessary?
        out     VFDA_I,SDATR                ;yes
        out     VFDA_I,SCLKR                ;shift clock toggle
        out     VFDA_I,SCLKR

        sbrc    a1,@0                       ;pre calculated toggle data
        out     VFDA_I,SDATR
        out     VFDA_I,SCLKR
        out     VFDA_I,SCLKR

        sbrc    a2,@0
        out     VFDA_I,SDATR
        out     VFDA_I,SCLKR
        out     VFDA_I,SCLKR

        sbrc    a3,@0
        out     VFDA_I,SDATR
        out     VFDA_I,SCLKR
        out     VFDA_I,SCLKR

        sbrc    a4,@0
        out     VFDA_I,SDATR
        out     VFDA_I,SCLKR
        out     VFDA_I,SCLKR                ;(24 cycles)
.endmacro

.macro baseAd
.endmacro

.macro baseAd_8
        subi    @0,CCO_FIRST                ;subtract first char

        ldi     ZL,low(ascTab<<1)           ;char table
        ldi     ZH,high(ascTab<<1)
        add     ZL,@0                       ;+ offset, char * 5
        adc     ZH,NULR
        lsl     @0
        lsl     @0        
        adc     ZH,NULR
        add     ZL,@0  
        adc     ZH,NULR                     ;(10 cycles)
.endmacro

.macro scrollP
.endmacro

.macro scrollP_8
        lpm     a5,Z+                       ;scroll text
        mov     a6,a7                       ;       count
        lsr     a5
        ror     @0
        subi    a6,SCCYC
        brcc    PC-3                        ;(8..39 cycles)
.endmacro

.macro scrollR
.endmacro

.macro scrollR_8
        ld      a5,Z+                       ;scroll text
        mov     a6,a7                       ;       data
        lsr     a5
        ror     @0
        subi    a6,SCCYC
        brcc    PC-3                        ;(7..38 cycles)
.endmacro

;-------------------------------------------------------------------------------

main:
        ldi     a4,low(RAMEND)              ;main program start
        out     SPL,a4                      ;set stack pointer to top of RAM

        rcall   init                        ;init

;- - - - - - - - - - - - - - - - - - - -
; ... calc char, wait end dark | light, service, wait end cycle | dark ...

m00:                                        ;<- insert optional 5 bit LED shift data here

;- - - - - - - - - - - - - - - - - - - -

        mov     a4,YL                       ;copy text pointer 0..23 + text
        subi    a4,text                     ;-> grid pos 0..23
        mov     a5,a4                       ;copy
        breq    m02                         ;first grid active? yes, jump

        cbi     VFDA_P,SHI_DA               ;grids not active
m01:    out     VFDA_I,SCLKR                ;shift clock toggle
        out     VFDA_I,SCLKR
        dec     a4                          ;next grid
        brne    m01                         ;all done? no, jump

m02:    ldi     a4,24                       ;grid pos -> grids to go 24..1
        sub     a4,a5

        sbi     VFDA_P,SHI_DA               ;one active grid
        out     VFDA_I,SCLKR                ;shift clock toggle
        out     VFDA_I,SCLKR
        dec     a4                          ;next grid
        breq    m04                         ;all done? yes, jump

        cbi     VFDA_P,SHI_DA               ;grids not active
m03:    out     VFDA_I,SCLKR                ;shift clock toggle
        out     VFDA_I,SCLKR
        dec     a4                          ;next grid
        brne    m03                         ;all done? no, jump                 (131 cycles)

;- - - - - - - - - - - - - - - - - - - -

m04:    ld      a4,Y+                       ;text char      
        sbrc    a4,7                        ;custom char?
        rjmp    cuchT                       ;yes

        baseAd  [a4]                        ;calc base address                  (10 cycles)

        lpm     a0,Z+                       ;load char data         column 4
        lpm     a1,Z+                       ;6..0 row bits each            3
        lpm     a2,Z+                       ;                              2
        lpm     a3,Z+                       ;                              1
        lpm     a4,Z+                       ;                              0

m05:    ldd     a7,Y+(NUMDIGITS*2-1)        ;scroll timer
        tst     a7                          ;       done?
        breq    m06                         ;yes, jump

        ldd     a5,Y+(NUMDIGITS-1)          ;scroll text
        sbrc    a5,7                        ;custom char?
        rjmp    cuchS                       ;yes

        baseAd  [a5]                        ;calc char address                  (10 cycles)

        scrollP [a0]
        scrollP [a1]
        scrollP [a2]
        scrollP [a3]
        scrollP [a4]                        ;                                   (195 cycles)

m06:    eor     a4,a3                       ;data -> toggle data           0
        eor     a3,a2                       ;                              1
        eor     a2,a1                       ;                              2
        eor     a1,a0                       ;                              3

        shiftOut    [6]                     ;shift out row 6
        shiftOut    [5]                     ;              5
        shiftOut    [0]                     ;              0
        shiftOut    [1]                     ;              1
        shiftOut    [4]                     ;              4
        shiftOut    [3]                     ;              3
        shiftOut    [2]                     ;              2                    (168 cycles -> 570 + 30 IRs ~ 600 total)

;- - - - - - - - - - - - - - - - - - - -

        brtc    PC                          ;wait for end of dark time

;- - - - - - - - - - - - - - - - - - - -    light time

        cpi     YL,(text+NUMDIGITS)         ;next char 
        brne    PC+2                       
        ldi     YL,text                 

;-  -  -  -  -  -  -  -  -  -  -  -  -

.if TESTDISPLAY==0
        sbis    USISR,USIOIF                ;USI OV?
        rjmp    m07                         ;no, jump

        in      a4,USIDR                    ;get data
        out     USISR,USIOIFR               ;reset USI OV flag
        rcall   procData                    ;process data
.else
        rcall   testData                    ;generate test data
.endif

;-  -  -  -  -  -  -  -  -  -  -  -  -

m07:    cbi     VFDA_P,SHI_DA               ;prepare dark time                  (298 cycles)
        ldi     a4,(NUMDIGITS+5*7)
m08:    out     VFDA_I,SCLKR
        out     VFDA_I,SCLKR
        dec     a4
        brne    m08

;-  -  -  -  -  -  -  -  -  -  -  -  -

        ldd     a7,Y+(NUMDIGITS*2)          ;scroll timer
        tst     a7                          ;       done?
        breq    m10                         ;yes, jump

        inc     a7                          ;scroll timer ++
        cpi     a7,(SCCYC*7)                ;       done?
        brne    m09                         ;no, jump

        clr     a7                          ;reset scroll timer
        ldd     a4,Y+NUMDIGITS              ;load         char
        st      Y,a4                        ;store
        
m09:    std     Y+(NUMDIGITS*2),a7          ;store 

;- - - - - - - - - - - - - - - - - - - -    dark time

m10:    brts    PC                          ;wait for refresh cycle

;- - - - - - - - - - - - - - - - - - - -    dark time

        rjmp    m00                         ;main loop

;---------------------------------------

cuchT:  subi    a4,CCO_CUST0

        ldi     ZL,cuchRam                  ;custom char base
        add     ZL,a4                       ;+ offset, char * 5
        lsl     a4
        lsl     a4        
        add     ZL,a4  

        ld      a0,Z+                       ;load char data         column 4
        ld      a1,Z+                       ;6..0 row bits each            3
        ld      a2,Z+                       ;                              2
        ld      a3,Z+                       ;                              1
        ld      a4,Z+                       ;                              0

        rjmp    m05

;---------------------------------------

cuchS:  subi    a5,CCO_CUST0

        ldi     ZL,cuchRam                
        add     ZL,a5                     
        lsl     a5
        lsl     a5       
        add     ZL,a5 

        scrollR [a0]
        scrollR [a1]
        scrollR [a2]
        scrollR [a3]
        scrollR [a4]                        

        rjmp    m06

;-------------------------------------------------------------------------------

setCustomChar:
        tst     cuchc                       ;custom char counter set?
        brne    sc01                        ;yes, jump

        add     cuchp,a4                    ;char offset
        lsl     a4
        lsl     a4
        add     cuchp,a4

        ldi     cuchc,5                     ;set counter
        ret

;- - - - - - - - - - - - - - - - - - - -

sc01:   mov     ZL,cuchp                    ;load pointer
        st      Z+,a4                       ;store char data
        mov     cuchp,ZL                    ;store pointer

        dec     cuchc                       ;counter--
        brne    PC+2                        ;data complete
        clr     cuchp                       ;yes, reset pointer

        ret

;-------------------------------------------------------------------------------

procData:
        tst     cuchp                       ;receive custom char?
        brne    setCustomChar               ;yes

        cpi     a4,CCO_FIRST                ;char?               
        brlo    ud01                        ;no, cursor pos

        cpi     a4,CCO_LAST+1               ;char?
        brsh    ud02                        ;no, command

        mov     ZL,XL                       ;copy write pointer              
        ld      a5,X+                       ;load stored char, inc write pointer

        sbis    GPIOR0,SCROL_EN             ;scroll enabled?
        rjmp    pd02                        ;no, store char

        sbis    GPIOR0,SCROL_DC             ;scroll different?
        rjmp    pd01                        ;no, scroll new

        cp      a4,a5                       ;current == new char?
        breq    pd03                        ;yes, no scroll, no store

pd01:   ldi     a5,1                        ;scroll timer start value
        std     Z+(NUMDIGITS*2),a5          ;             store 
        subi    ZL,(-NUMDIGITS)             ;       pointer

pd02:   st      Z,a4                        ;store char

pd03:   cpi     XL,(text+NUMDIGITS)         ;text buffer roll over
        brlo    PC+2
        ldi     XL,text

        ret

;- - - - - - - - - - - - - - - - - - - -

ud01:   cpi     a4,NUMDIGITS                ;cursor pos, check range                
        brsh    ud99                        

        subi    a4,-text                    ;+ base               
        mov     XL,a4                       ;store
ud99:   ret

;- - - - - - - - - - - - - - - - - - - -

ud02:   cpi     a4,CMD_CLRHOME              ;clear home?
        brne    ud03                        ;no, jump

clearHome:
        ldi     ZL,text                     ;base pointer
        ldi     a4,' '                      ;space

ch01:   std     Z+(NUMDIGITS*2),NULR        ;reset scroll timer
        std     Z+NUMDIGITS,a4              ;             char buffer
        st      Z+,a4                       ;       char buffer
        cpi     ZL,(text+NUMDIGITS)         ;all done?
        brne    ch01                        ;no, loop

        ldi     XL,text                     ;init text pointer write
        ret

;- - - - - - - - - - - - - - - - - - - -

ud03:   cpi     a4,CMD_SUPON                ;supply on?
        brne    ud04                        ;no, jump

        sbi     VFDA_P,SUPPLY               ;on
        ret

ud04:   cpi     a4,CMD_SUPOFF               ;supply off?
        brne    ud05                        ;no, jump

        cbi     VFDA_P,SUPPLY               ;off
        ret

;- - - - - - - - - - - - - - - - - - - -

ud05:   cpi     a4,CMD_SETCUCH              ;set custom char?
        brne    ud06                        ;no, jump

        ldi     cuchp,cuchRam               ;load base pointer
        ret

;- - - - - - - - - - - - - - - - - - - -

ud06:   cpi     a4,CMD_SCLOFF
        brne    ud07

        cbi     GPIOR0,SCROL_EN
        ret

ud07:   cpi     a4,CMD_SCLON
        brne    ud08

        sbi     GPIOR0,SCROL_EN
        ret

;- - - - - - - - - - - - - - - - - - - -

ud08:   cpi     a4,CMD_SCLDOFF
        brne    ud09

        cbi     GPIOR0,SCROL_DC
        ret

ud09:   cpi     a4,CMD_SCLDON
        brne    ud10

        sbi     GPIOR0,SCROL_DC
        ret

;- - - - - - - - - - - - - - - - - - - -

ud10:   cpi     a4,CMD_BRIGHT               ;bright?
        brlo    ud99                        ;no, ignore and exit

setBright:
        mov     a6,a4                       ;copy cmd + bright               

        ldi     a4,BRISTEPS+CMD_BRIGHT-1    ;192..255 -> 63..0
        sub     a4,a6
        clr     a5

        lsl     a4                          ;*2
        lsl     a4                          ;*4
        mov     a6,a4
        lsl     a4                          ;*8
        rol     a5
        lsl     a4                          ;*16
        rol     a5
        lsl     a4                          ;*32
        rol     a5
        sub     a4,a6                       ;*28
        sbc     a5,NULR
        
        subi    a4,low(-VFR_DACMIN)
        sbci    a5,high(-VFR_DACMIN)

        out     OCR1BH,a5
        out     OCR1BL,a4
        ret

;-------------------------------------------------------------------------------

init:
        ldi     a4,PORTBM                   ;init ports
        out     PORTB,a4
        ldi     a4,DDRBM
        out     DDRB,a4

        ldi     a4,PORTDM
        out     PORTD,a4
        ldi     a4,DDRDM
        out     DDRD,a4

        ldi     a4,(1<<CLKPCE)              ;CLKPCE=1
        out     CLKPR,a4
        ldi     a4,0                        ;CLKPCE=0, div 1
        out     CLKPR,a4                    ;system clock 4 MHz

;- - - - - - - - - - - - - - - - - - - -

        sbi     ACSR,ACD                    ;AC OFF

;- - - - - - - - - - - - - - - - - - - -

        clr     ZH                          ;clear registers 0..31
        ldi     ZL,30                       ;start with r29
in01:   st      -Z,ZH                       ;store indirect
        tst     ZL                          ;all registers?
        brne    in01                        ;no, jump

;- - - - - - - - - - - - - - - - - - - -

        ldi     XL,low(SRAM_START)          ;delete SRAM
in02:   st      X+,NULR
        cpi     XL,low(RAMEND-1)            ;do not delete return address
        brne    in02

;- - - - - - - - - - - - - - - - - - - -

        ldi     a4,(1<<SHI_CLK)             ;init shift clock toggle
        mov     SCLKR,a4

        ldi     a4,(1<<SHI_DA)              ;           data
        mov     SDATR,a4

        ldi     a4,(1<<SHI_LA)              ;           latch
        mov     SLATR,a4

        ldi     a4,(1<<USIOIF)              ;     USI OV flag
        mov     USIOIFR,a4

        ldi     a4,(1<<FIC_A)               ;     filament supply toggle
        mov     FISUTR,a4

        ldi     YL,text                     ;     text pointer read
        rcall   clearHome                   ;                  write, clear display

.if TESTDISPLAY==1
        ldi     tcnt,TDELAY                 ;test delay
        ldi     tchr,' '                    ;     char
.endif

;- - - - - - - - - - - - - - - - - - - -
;spi    MSB first, mode 3
;avr    slave, three wire mode, positive edge

        ldi     a4,(1<<USIWM0|1<<USICS1)            ;three wire mode, external positive both edges
        out     USICR,a4

;- - - - - - - - - - - - - - - - - - - -

        ldi     a4,FILAPOW                          ;filament 
        out     OCR0A,a4                            ;         power
        lsr     a4                                  ;         polarity ratio
        out     OCR0B,a4                            

        ldi     a4,(1<<COM0B1|1<<WGM01|1<<WGM00)    ;OC0B CMP L, TOP H, fast PWM
        out     TCCR0A,a4

        ldi     a4,(1<<CS00)                        ;div 1      4E6 / 256 / 1 = 15.625 kHz
;       ldi     a4,(1<<CS01)                        ;    8      4E6 / 256 / 8 =  1.953 kHz
        out     TCCR0B,a4                           ;start

;- - - - - - - - - - - - - - - - - - - -

        ldi     a4,low(VFR_REC-1)               ;T1 CMP A value, VDF refresh cycle
        ldi     a5,high(VFR_REC-1)
        out     OCR1AH,a5
        out     OCR1AL,a4

        ldi     a4,(1<<WGM12|1<<CS10)           ;CTC, div 1
        out     TCCR1B,a4                       ;start

;- - - - - - - - - - - - - - - - - - - -

        ldi     a4,(1<<OCIE1A|1<<OCIE1B|1<<OCIE0B|1<<OCIE0A)    ;enable T1+T0 CMP A+B IR
        out     TIMSK,a4

        sei                                     ;enable IRs
        ret

;-------------------------------------------------------------------------------
; display                    storage        chars (95)
;       column               byte           SPACE..~
;       4 3 2 1 0            0 1 2 3 4       0x20..0x7e
; row 6                bit 6
;     5                    5
;     4                    4
;     3                    3
;     2                    2
;     1                    1
;     0                    0
;
; pin row order     2 3 4 1 0 5 6
; shift bit order   6 5 0 1 4 3 2

ascTab:
.DB $00,$00,$00,$00,$00,$00,$00,$3d,$00,$00,$00,$60,$00,$60,$00,$14         ;fut57 consolas, fonter TLY07R, no descender
.DB $7f,$14,$7f,$14,$12,$2a,$7f,$2a,$24,$21,$02,$0c,$10,$21,$22,$55         ;  a  b  c  d                   anta, antb, antc, degree celsius
.DB $49,$34,$00,$00,$00,$60,$00,$00,$00,$1c,$22,$41,$00,$00,$41,$22         ;
.DB $1c,$00,$2a,$1c,$3e,$1c,$2a,$08,$08,$3e,$08,$08,$00,$00,$03,$00         ;  e  f  g  h  i   j  k  l  m
.DB $00,$08,$08,$08,$08,$08,$00,$00,$01,$00,$00,$01,$02,$0c,$10,$20         ; -4 -3 -2 -1  0  +1 +2 +3 +4   down-neutral-up arrows
.DB $3e,$45,$49,$51,$3e,$10,$20,$7f,$00,$00,$47,$49,$49,$49,$31,$41
.DB $49,$49,$49,$36,$0c,$14,$24,$7f,$04,$79,$49,$49,$49,$46,$3e,$49
.DB $49,$49,$06,$40,$40,$47,$48,$70,$36,$49,$49,$49,$36,$30,$49,$49
.DB $49,$3e,$00,$00,$09,$00,$00,$00,$00,$0b,$00,$00,$08,$14,$22,$7f
.DB $00,$14,$14,$14,$14,$14,$00,$7f,$22,$14,$08,$20,$25,$28,$28,$10
.DB $1e,$21,$2d,$2d,$18,$0f,$34,$44,$34,$0f,$7f,$49,$49,$49,$36,$3e
.DB $41,$41,$41,$41,$7f,$41,$41,$41,$3e,$7f,$49,$49,$49,$41,$7f,$48
.DB $48,$48,$40,$3e,$41,$49,$49,$4e,$7f,$08,$08,$08,$7f,$41,$41,$7f
.DB $41,$41,$41,$41,$41,$41,$7e,$7f,$08,$14,$22,$41,$7f,$01,$01,$01
.DB $01,$7f,$20,$10,$20,$7f,$7f,$20,$10,$08,$7f,$3e,$41,$41,$41,$3e
.DB $7f,$48,$48,$48,$30,$3e,$41,$45,$42,$3d,$7f,$48,$4c,$4a,$31,$31
.DB $49,$49,$49,$46,$40,$40,$7f,$40,$40,$7e,$01,$01,$01,$7e,$70,$0e
.DB $01,$0e,$70,$7f,$02,$04,$02,$7f,$63,$14,$08,$14,$63,$60,$10,$0f
.DB $10,$60,$43,$45,$49,$51,$61,$00,$7f,$41,$41,$00,$20,$10,$0c,$02
.DB $01,$00,$41,$41,$7f,$00,$10,$20,$40,$20,$10,$01,$01,$01,$01,$01
.DB $00,$40,$20,$00,$00,$00,$00,$07,$00,$00,$20,$10,$0f,$00,$00,$20
.DB $10,$0f,$10,$20,$30,$30,$1e,$21,$21,$08,$0c,$0e,$0c,$08,$08,$04
.DB $0a,$04,$08,$08,$04,$02,$04,$08,$08,$00,$04,$00,$08,$08,$00,$08
.DB $00,$08,$08,$00,$10,$00,$08,$08,$10,$20,$10,$08,$08,$10,$28,$10
.DB $08,$08,$18,$38,$18,$08,$1f,$08,$10,$10,$0f,$0e,$11,$11,$11,$0e
.DB $1f,$14,$14,$14,$08,$08,$14,$14,$14,$1f,$1f,$08,$10,$10,$08,$09
.DB $15,$15,$15,$12,$00,$08,$3e,$09,$01,$1e,$01,$01,$02,$1f,$18,$06
.DB $01,$06,$18,$1e,$01,$06,$01,$1e,$11,$0a,$04,$0a,$11,$19,$05,$02
.DB $04,$18,$11,$13,$15,$19,$11,$00,$08,$36,$41,$00,$00,$00,$7f,$00
.DB $00,$00,$41,$36,$08,$00,$08,$10,$18,$08,$10,$00

; .DB $00,$00,$00,$00,$00,$00,$00,$3d,$00,$00,$00,$60,$00,$60,$00,$14     ;fut57 consolas, fonter TLY07R, no descender
; .DB $7f,$14,$7f,$14,$12,$2a,$7f,$2a,$24,$21,$02,$0c,$10,$21,$22,$55
; .DB $49,$34,$00,$00,$00,$60,$00,$00,$00,$1c,$22,$41,$00,$00,$41,$22
; .DB $1c,$00,$2a,$1c,$3e,$1c,$2a,$08,$08,$3e,$08,$08,$00,$00,$03,$00
; .DB $00,$08,$08,$08,$08,$08,$00,$00,$01,$00,$00,$01,$02,$0c,$10,$20
; .DB $3e,$45,$49,$51,$3e,$10,$20,$7f,$00,$00,$47,$49,$49,$49,$31,$41
; .DB $49,$49,$49,$36,$0c,$14,$24,$7f,$04,$79,$49,$49,$49,$46,$3e,$49
; .DB $49,$49,$06,$40,$40,$47,$48,$70,$36,$49,$49,$49,$36,$30,$49,$49
; .DB $49,$3e,$00,$00,$09,$00,$00,$00,$00,$0b,$00,$00,$08,$14,$22,$7f
; .DB $00,$14,$14,$14,$14,$14,$00,$7f,$22,$14,$08,$20,$25,$28,$28,$10
; .DB $1e,$21,$2d,$2d,$18,$0f,$34,$44,$34,$0f,$7f,$49,$49,$49,$36,$3e
; .DB $41,$41,$41,$41,$7f,$41,$41,$41,$3e,$7f,$49,$49,$49,$41,$7f,$48
; .DB $48,$48,$40,$3e,$41,$49,$49,$4e,$7f,$08,$08,$08,$7f,$41,$41,$7f
; .DB $41,$41,$41,$41,$41,$41,$7e,$7f,$08,$14,$22,$41,$7f,$01,$01,$01
; .DB $01,$7f,$20,$10,$20,$7f,$7f,$20,$10,$08,$7f,$3e,$41,$41,$41,$3e
; .DB $7f,$48,$48,$48,$30,$3e,$41,$45,$42,$3d,$7f,$48,$4c,$4a,$31,$31
; .DB $49,$49,$49,$46,$40,$40,$7f,$40,$40,$7e,$01,$01,$01,$7e,$70,$0e
; .DB $01,$0e,$70,$7f,$02,$04,$02,$7f,$63,$14,$08,$14,$63,$60,$10,$0f
; .DB $10,$60,$43,$45,$49,$51,$61,$00,$7f,$41,$41,$00,$20,$10,$0c,$02
; .DB $01,$00,$41,$41,$7f,$00,$10,$20,$40,$20,$10,$01,$01,$01,$01,$01
; .DB $00,$40,$20,$00,$00,$02,$15,$15,$15,$0f,$3f,$09,$09,$09,$06,$06
; .DB $09,$09,$09,$09,$06,$09,$09,$09,$3f,$0e,$15,$15,$15,$0d,$08,$1f
; .DB $28,$28,$28,$08,$15,$15,$15,$0e,$3f,$04,$08,$08,$07,$09,$09,$2f
; .DB $01,$01,$09,$09,$09,$2e,$00,$3f,$04,$0a,$11,$00,$21,$21,$1f,$01
; .DB $01,$1f,$10,$1f,$10,$0f,$1f,$08,$10,$10,$0f,$0e,$11,$11,$11,$0e
; .DB $1f,$14,$14,$14,$08,$08,$14,$14,$14,$1f,$1f,$08,$10,$10,$08,$09
; .DB $15,$15,$15,$12,$00,$08,$3e,$09,$01,$1e,$01,$01,$02,$1f,$18,$06
; .DB $01,$06,$18,$1e,$01,$06,$01,$1e,$11,$0a,$04,$0a,$11,$19,$05,$02
; .DB $04,$18,$11,$13,$15,$19,$11,$00,$08,$36,$41,$00,$00,$00,$7f,$00
; .DB $00,$00,$41,$36,$08,$00,$08,$10,$18,$08,$10,$00

; .DB $00,$00,$00,$00,$00,$00,$00,$7a,$00,$00,$00,$60,$00,$60,$00,$14   ;fut57 consolas, fonter TLY07R, with descender
; .DB $3e,$14,$3e,$14,$12,$2a,$7f,$2a,$24,$22,$04,$08,$10,$22,$22,$55
; .DB $49,$34,$00,$00,$00,$60,$00,$00,$00,$18,$24,$42,$00,$00,$42,$24
; .DB $18,$00,$2a,$1c,$3e,$1c,$2a,$08,$08,$3e,$08,$08,$00,$00,$03,$00
; .DB $00,$08,$08,$08,$08,$08,$00,$00,$02,$00,$00,$02,$04,$08,$10,$20
; .DB $3c,$4a,$52,$62,$3c,$12,$22,$7e,$02,$02,$22,$46,$4a,$52,$22,$42
; .DB $52,$52,$52,$2c,$0c,$14,$24,$7e,$04,$72,$52,$52,$52,$4c,$3c,$52
; .DB $52,$52,$0c,$40,$46,$48,$50,$60,$2c,$52,$52,$52,$2c,$30,$4a,$4a
; .DB $4a,$3c,$00,$00,$12,$00,$00,$00,$00,$0b,$00,$00,$00,$08,$14,$22
; .DB $00,$14,$14,$14,$14,$14,$00,$22,$14,$08,$00,$40,$4a,$50,$50,$20
; .DB $3c,$42,$5a,$5a,$30,$0e,$34,$44,$34,$0e,$7e,$52,$52,$52,$2c,$3c
; .DB $42,$42,$42,$42,$7e,$42,$42,$42,$3c,$7e,$52,$52,$52,$52,$7e,$50
; .DB $50,$50,$50,$3c,$42,$4a,$4a,$4c,$7e,$10,$10,$10,$7e,$42,$42,$7e
; .DB $42,$42,$42,$42,$42,$42,$7c,$7e,$08,$14,$24,$42,$7e,$02,$02,$02
; .DB $02,$7e,$20,$10,$20,$7e,$7e,$20,$10,$08,$7e,$3c,$42,$42,$42,$3c
; .DB $7e,$48,$48,$48,$30,$3c,$42,$4a,$44,$3a,$7e,$48,$48,$4c,$32,$22
; .DB $52,$52,$52,$4c,$40,$40,$7e,$40,$40,$7c,$02,$02,$02,$7c,$70,$0c
; .DB $02,$0c,$70,$7e,$04,$08,$04,$7e,$46,$28,$10,$28,$46,$60,$10,$0e
; .DB $10,$60,$42,$46,$4a,$52,$62,$00,$7e,$42,$42,$00,$20,$10,$08,$04
; .DB $02,$00,$42,$42,$7e,$00,$10,$20,$40,$20,$10,$01,$01,$01,$01,$01
; .DB $00,$40,$20,$00,$00,$04,$2a,$2a,$2a,$1e,$7e,$12,$12,$12,$0c,$1c
; .DB $22,$22,$22,$22,$0c,$12,$12,$12,$7e,$1c,$2a,$2a,$2a,$1a,$10,$3f
; .DB $50,$50,$50,$18,$25,$25,$25,$1e,$7e,$08,$10,$10,$0e,$12,$12,$5e
; .DB $02,$02,$11,$11,$11,$5e,$00,$7e,$08,$14,$22,$00,$42,$42,$7e,$02
; .DB $02,$3e,$20,$3e,$20,$1e,$3e,$10,$20,$20,$1e,$1c,$22,$22,$22,$1c
; .DB $3f,$24,$24,$24,$18,$18,$24,$24,$24,$3f,$3e,$10,$20,$20,$10,$12
; .DB $2a,$2a,$2a,$24,$00,$10,$7c,$12,$02,$3c,$02,$02,$04,$3e,$30,$0c
; .DB $02,$0c,$30,$3c,$02,$0c,$02,$3c,$22,$14,$08,$14,$22,$31,$0a,$04
; .DB $08,$30,$22,$26,$2a,$32,$22,$00,$08,$36,$41,$00,$00,$00,$3f,$00
; .DB $00,$00,$41,$36,$08,$00,$08,$10,$18,$08,$10,$00

;-------------------------------------------------------------------------------

.if TESTDISPLAY==1
testData:
        sbic    VFDA_P,SUPPLY
        rjmp    td01

        sbi     VFDA_P,SUPPLY
        ldi     a4,BRISTEPS/2
        rcall   setBright

        sbi     GPIOR0,SCROL_EN

        ; ldi     a4,low(VFR_DACMIN)
        ; ldi     a5,high(VFR_DACMIN)
        ; out     OCR1BH,a5
        ; out     OCR1BL,a4

td01:   dec     tcnt
        brne    td09

        ldi     tcnt,TDELAY

        mov     a4,tchr
        rcall   procData

        inc     tchr
        cpi     tchr,127
        brlo    PC+2
        ldi     tchr,' '

td09:   ret
.endif

;-------------------------------------------------------------------------------

.if DEBUG==1
cntOut:
        sbi     COUNT_P,CRESET              ;reset
        cbi     COUNT_P,CRESET

        cp      a4,NULR
        cpc     a5,NULR
        breq    co09

        push    a4
        push    a5

        tst     a5
        brpl    co01

        com     a5                          ;abs(d)
        neg     a4
        sbci    a5,-1

        subi    a4,low(-1000)
        sbci    a5,high(-1000)

co01:   cbi     COUNT_P,CCLK                ;clock
        sbi     COUNT_P,CCLK
        sbiw    a5:a4,1
        brne    co01

        pop     a5
        pop     a4

co09:   ret
.endif

;-------------------------------------------------------------------------------

;filament supply clock 1:1
;
;.equ    FSC_CYC     =   133                 ;filament supply clock cycle 15 KHz     15E3^-1 / 2 / (4E6^-1 * DIV1)
;
;        ldi     a4,(FSC_CYC-1)                     ;T0 CMP A/B value, supply clock cycle
;        out     OCR0A,a4
;        out     OCR0B,a4
;
;        ldi     a4,(1<<COM0A1|1<<COM0B1|1<<COM0B0)  ;OC0A:OC0B L:H phase
;        out     TCCR0A,a4
;
;        ldi     a4,(1<<FOC0A|1<<FOC0B)              ;force output compare to fix phase
;        out     TCCR0B,a4
;
;        ldi     a4,(1<<COM0A0|1<<COM0B0|1<<WGM01)   ;OC0A:OC0B toggle, CTC
;        out     TCCR0A,a4
;
;        ldi     a4,(1<<CS00)                        ;div 1
;        out     TCCR0B,a4                           ;start

;        ret

;-------------------------------------------------------------------------------

;filament supply clock variable 2 IRs (not tested)
;
;example:
;        ldi     a4,64                              ;power 8..120
;        rcall   setFilSupClk
;
;initFilSupClk:
;        ldi     a4,(FSC_CYC-1)                      ;T0 CMP A value, supply clock cycle
;        out     OCR0A,a4
;        lsl     a4                                  ;       B value
;        out     OCR0B,a4
;
;        ldi     a4,(1<<COM0A1|1<<WGM01|1<<WGM00)    ;   OC0A CMP clear, TOP set, fast PWM
;        out     TCCR0A,a4
;
;        ldi     a4,(1<<OCIE0B|1<<OCIE0A)            ;   enable CMP A+B IR
;        out     TIMSK,a4
;
;        ldi     a4,(1<<CS00)                        ;   div 1
;        out     TCCR0B,a4                           ;   start

;        ret
;
;setFilSupClk:
;        out     OCR0A,a4
;        lsl     a4                                  ;       B value
;        out     OCR0B,a4
;        ret
;
;TIMER0_COMPA_IR:
;        sbi     VFDB_P,FIC_B
;        reti
;
;TIMER0_COMPB_IR:
;        cbi     VFDB_P,FIC_B
;        reti
;

;-------------------------------------------------------------------------------
