#! /usr/bin/gforth b16-asm.fs
: reset			\ label reset
    fw jmp		\ jump forward to boot
: wfill ( n addr u -- ) over >a + com
    BEGIN  over a!+ dup a + drop cUNTIL  drop drop ;
: sieve ( -- n )
    $0101 # $100 # 8192 # wfill
    $100 # 8192 # over >a + com 2 # 0 # >r
    BEGIN
	ac@+ IF  r> + >r a >r
	    BEGIN  dup a + >a over a + drop -cWHILE
		0 # ac!+  AGAIN  THEN
	    r> >a dup  THEN  drop
	2 # + over a + drop cUNTIL
	drop drop r> ;
: main THEN  sieve a!* BEGIN AGAIN ;
.end
