\ b16 boot loader
\ $Log: boot.asm,v $
\ Revision 1.10  2003/01/06 20:36:04  bernd
\ Added comments to asm files
\
\ Revision 1.8  2002/03/31 23:08:39  bernd
\ Made downloader work. Improved testbench to allow serial communication
\
\ Revision 1.7  2002/03/24 17:46:15  bernd
\ Added bit-bang input
\
\ Revision 1.6  2002/03/23 22:59:10  bernd
\ Faster upload
\ Fixes for not changing state
\ Macro 2/ in boot.asm
\
\ Revision 1.5  2002/03/23 22:05:12  bernd
\ Made b16 another step faster
\
\ Revision 1.4  2002/03/23 21:35:57  bernd
\ Made b16 faster
\
\ Revision 1.3  2002/03/10 13:49:15  bernd
\ Added blink code
\
\ Revision 1.2  2002/02/17 23:30:22  bernd
\ Added greeting message as test
\ Changed baud rate to 87 cycles/114942 (closer to 115200)
\
\ Revision 1.1  2002/02/17 23:19:51  bernd
\ Added RS232 send at 115200 baud (really 116279 baud)
\
: reset
    AHEAD

macro: 2/ \ macro for division by two
    0 # + *+ end-macro \ clear carry and *+ for shift right
macro: 2* \ macro for multiplication by two
    dup + end-macro    \ dup TOS and add
\ flash sequence: 5555H AAH 2AAAH 55H 5555H A0H PA Data
\ : flash! ( c addr -- )
\     $AA # $AAAA # !
\     $55 # $5554 # !
\     $A0 # $AAAA # !
\     >A dup A! dup
\     BEGIN  drop dup A@ $FF # and xor -UNTIL drop drop ;
\ bit-banging serial interface: 87 cycles per bit for 115200 Baud @ 10MHz
: send-rest
    *+ \ divide by two
: wait-bit \ counted loop: loop 7 times
    1 # $FFF9 # BEGIN  over +  cUNTIL  drop drop ;
: send-bit ( c -- c' )
    nop \ delay at start
: send-bit-fast ( c -- c' )
    $FFFE # >a \ Address of bit-bang port to A register
    dup 1 # and \ check if LSB is set
    IF   drop $0001 # a@ or  a!* send-rest ;
    \ if so, set serial port TX, and go to send rest
    THEN drop $FFFE # a@ and a!* send-rest ;
    \ if not, clear serial port TX, and go to send rest
: emit ( c -- ) \ 8N1, 115200 baud
    \ this emits one byte on the serial port
    \ first one zero bit as start
    >r 06 # send-bit r>
    \ then 8 data bits
    send-bit-fast send-bit send-bit send-bit
    send-bit send-bit send-bit send-bit
    \ and finally two one-bits as stop
    drop send-bit-fast send-bit drop ;
: wait-start ( -- )
    \ wait for a start bit, with timeout
    $FFFC # >a \ bit-bang port status to A register
    1 # $8000 # \ move 1 and count on stack
    BEGIN  over a@ and  WHILE  drop over +  cUNTIL
    \ loop until either the status bit goes zero, or the loop expires
	nip r> drop ;  THEN
    \ if the loop exired, drop caller address from return stack
    drop drop drop ;
    \ otherwise drop temporaries
| bit-var 0 ,
: get-bit ( c -- c' )  wait-bit
    \ fetch one serial bit:
    $FFFC # @ 1 # and
    \ isolate bit 1 of port status
    bit-var # >a ac!
    \ store as byte
    a@*
    \ fetch as word. The architecture is big endian, so
    \ this is equvalent to a shift left by 8
    + 2/
    \ add to the previous bits and shift right
    nop nop nop ;
    \ these nops are delays to reach the 87 cycles
: key ( -- c ) \ 8N1, 115200 baud
    \ that waits for a start bit and samples one byte
    wait-start 0 # wait-bit \ sample in the middle
    \ fetch 8 bits
    get-bit get-bit get-bit get-bit get-bit get-bit get-bit get-bit ;

\ The following stuff contains the boot loader functions

\ addr turns two bytes from the serial line into an address on the stack
\ by storing the bytes into addr-var, and fetching the whole word

| addr-var 0 ,
: addr ( -- addr )  key addr-var # c!  key  addr-var 1+ # c!
    addr-var # @ ;

\ program loads n bytes from the serial port and stores them
\ into consecutive RAM locations starting at addr.

: programm ( addr n -- )
    >r >a $FFFF # r> over + a >r
    \ stack shuffling: the result of this is
    \ $FFFF n-1 on the data stack
    \ addr on the return stack
    BEGIN \ start a loop
	key	\ wait for a byte on the serial line
		\ the byte is now on top of stack
	r> >a	\ move addr from the return stack to A
	ac!+	\ store byte and increment A
	a >r	\ move A to return stack
	over +	\ decrement N
	-cUNTIL	\ loop until carry clear
    drop drop r> drop ; \ clean up stack and return

\ .$ prints a counted string
: .$ ( addr -- ) >a ac@+ >r \ move address to A, and count to R
    AHEAD \ jump over to the next THEN
\ type prints a string with addr and count
: type ( addr n -- )
    >r >a \ move address to A, and count to R
    THEN \ resolve jump form last AHEAD
    $FFFF # r> over + \ $FFFF n-1 on the data stack
    a >r \ address on the return stack
    BEGIN
	rc@+	\ fetch byte from address pointed by R
	emit	\ emit it on serial line
	over +	\ decrement
	-cUNTIL	\ loop until carry is clear
    drop drop r> drop ; \ clean up stack and return

\ handle-key takes one key and decides what to do
: handle-key ( c -- )
    \ 0: <addr> <len> <len*data> programm
    $FFD0 # + -IF  drop addr key programm ;  THEN
\ if the key is ASCII '0', take an address, a count, and
\ program that into RAM
    \ 1: <addr> <len> check
    $FFFF # + -IF  drop addr key type ;  THEN
\ if the key is ASCII '1', take an address, a count, and send that back
    \ 2: <addr> execute
    $FFFF # + -IF  drop addr goto  THEN
\ if the key is ASCII '2', take an address and go there.
    $32 # + emit ;
\ otherwise, echo the input character.

\ and now some LED lights, start with two lights at the right
| blinkvar  3 ,

\ check-key checks a key, and moves blinkvar to the LED bits
\ they have to be inverted (1=off, 0=on)
: check-key ( -- key/0 blink ) key
    blinkvar # @ dup com $FFFE # c! ;
\ blinkl shifts the leds to the left
: blinkl check-key 2* AHEAD
\ and blinkr to the right
: blinkr check-key 2/ THEN
\ both updated blinkvar
  blinkvar # !
\ and jump to handle-key if the key is not zero (i.e. no timeout)
  ' handle-key jnz  drop ;

\ this is commented out: hi would print "b16 function test"
\ | greeting s" b16 function test" F dup 1+ c, $, $0A c, align
\ : hi  greeting # .$ ;

\ the boot command
: boot  THEN
    3 # blinkvar # !
    BEGIN
	\ 6 times shift to the left
	blinkl blinkl blinkl blinkl blinkl blinkl
	\ and 6 times shift to the right
	blinkr blinkr blinkr blinkr blinkr blinkr
    AGAIN ;
.end
