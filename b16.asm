#! /usr/bin/gforth b16-asm.fs
: reset			\ label reset
   ' boot jmp		\ jump forward to boot
: stack			\ stack test
    1 #c 2 #c 3 #	\ fill stack with 1 2 3 4 5 6 7 8 9 A B
    4 # 5 #c 6 #c
    7 #c 8 #c 9 #
    $A # $B # nop
    drop drop drop	\ drop values from stack
    drop drop drop
    drop drop drop
    drop drop
;			\ return to caller
: alu			\ test ALU
    $1234 # $5678 # +	\ add two literals
    $1234 # com +c	\ complement and add with carry = subtract
    $1234 # $5678 # or	\ OR two literals
    $1234 # $5678 # and	\ AND two literals
    xor drop drop	\ XOR the two results from above, and drop results
;			\ return to caller
: mul ( u1 u2 -- ud )	\ unsigned expanding multiplication
    >A			\ move multiplicant to register A
    0 # dup +		\ put zero on top of stack and clear carry flag
    *+ *+ *+ *+ *+ *+ *+ *+ *+ *+ *+ *+ *+ *+ *+ *+ *+
    			\ 17 mul-step instructions
    >r drop a r>	\ drop second multiplicant, reorder results
;			\ return to caller
: div ( ud udiv -- uqout umod )	\ unsigned division with remainder
    com			\ invert divider
    >r >r >a r> r>	\ move low part of divident to A
    over 0 # +		\ copy low part of divider to top, clear carry
    /-  /- /- /- /-  /- /- /- /-  /- /- /- /-  /- /- /- /-
    			\ 17 div-step instructions
    nip nip a >r	\ reorder results
    -cIF		\ branch if carry set
        *+ r>		\ divide correction if carry was clear
    ;			\ return to caller
    THEN		\ resolve -cIF branch
    0 # +		\ clear carry
    *+			\ mul-step divides by two
    $8000 # + r>	\ insert carry
;			\ return to caller
: jumps			\ test a few jumps
    0 #c 1 #c    -IF  drop 3 # + THEN  drop \ jump if non-zero
    0 #c 0 #c     IF  drop 4 # + THEN  drop \ jump if zero
    0 # com     -cIF  drop 5 # + THEN  drop \ jump if carry
    0 #c 0 #c +  cIF  drop 6 # + THEN  drop \ jump if no carry
;			\ return to caller
: -jumps		\ test a few jumps the other way round
    0 #c 1 #c     IF  drop 3 # + THEN
    0 #c 0 #c    -IF  drop 4 # + THEN
    0 # com      cIF  drop 5 # + THEN
    0 #c 0 #c + -cIF  drop 6 # + THEN
;			\ return to caller
: boot			\ resolve forward jump from reset
    stack		\ call stack test
    alu			\ call ALU test
    $1234 # $5678 # mul	\ multiply 1234 with 5678 (hex)
    $789a # div drop drop \ divide by 789A, drop results
    jumps -jumps	\ call jump tests
;			\ endless loop
.end			\ end of test program
.hex
