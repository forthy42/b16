/*
 * Linux uploader for Altera FPGAs (passive serial via Byteblaster)
 * 
 * $Log: la.c,v $
 * Revision 1.4  2003/01/06 20:35:21  bernd
 * Changes to run with Icarus Verilog
 * USB interrupts
 * Added interrupts to the b16 core
 *
 * Revision 1.3  2002/03/23 22:59:10  bernd
 * Faster upload
 * Fixes for not changing state
 * Macro 2/ in boot.asm
 *
 * Revision 1.2  2002/03/23 22:33:39  bernd
 * Fixed initialization bug
 *
 */

#include <stdio.h>
#include <sys/io.h>

unsigned char rbt[256000];
unsigned int lpt_addr=0x378;

/*
Data Out        W   Base+0      D0-D7   8 LS TTL outputs
Status In       R   Base+1      S3-S7   5 LS TTL inputs
Control Out     W   Base+2      C0-C3   4 TTL Open Collector outputs
    "           "     "         C4      internal, IRQ enable
    "           "     "         C5      internal, Tristate data [PS/2]

Pin signals and register bits

<= in   DB25    Cent    Name of         Reg
=> out  pin     pin     Signal          Bit     Function Notes
------  ----    ----    --------        ---     -----------------------------
=>       2       2      Data 0          D0      DCLK
=>       3       3      Data 1          D1      nCONFIG
=>       4       4      Data 2          D2      ...
=>       5       5      Data 3          D3      ...
=>       6       6      Data 4          D4      ...
=>       7       7      Data 5          D5      ...
=>       8       8      Data 6          D6      DATA0
=>       9       9      Data 7          D7      
<=      10      10      -Ack            S6+ IRQ 
<=      11      11      +Busy           S7-     CONF_DONE
<=      12      12      +PaperEnd       S5+     
<=      13      13      +SelectIn       S4+     nSTATUS
=>      14      14      -AutoFd         C1-     
<=      15      32      -Error          S3+     
=>      16      31      -Init           C2+     
=>      17      36      -Select         C3-     
==      18-25   19-30,  Ground
               33,17,16
*/

void load_altera(unsigned int len)
{
  unsigned char a,b,c, init;
  unsigned int i, j, k=0;
  unsigned char* roll="|/-\\";
  ioperm(lpt_addr, 3, 1);
  printf("Uploading... ");
  init = inb(lpt_addr+2);
  outb((init | 0x02) & 0xDF, lpt_addr+2);
  outb(0x00, lpt_addr);
  for (i=0; i<len; i++) {
    if((i & 0x3FF) == 0x00) {
      printf("%c\b", roll[k++]);
      fflush(stdout);
      k &= 0x03;
    }
    a=rbt[i];
    for (j=0; j<8; j++)
    {
      b=(a & 1) << 6;
      b |= 0x02;
      c=b | 0x01;
    		                //d1 - data;	pin3
      outb(b, lpt_addr);  	//d0 - clock;	pin2
      outb(c, lpt_addr);
      a >>= 1;
    }
  }
  outb(0x02, lpt_addr);
  printf("\b\b\b\b\b\b\b%s successful\n", inb(lpt_addr+1) == 0x18 ? "" : " not");
  outb((init) & 0xDF, lpt_addr+2);
  ioperm(lpt_addr, 3, 0);
}

int main(int argc,char *argv[])
{
  unsigned int len = 0;
  FILE * rba;

  if((argv[1][0] == '-') && (argv[1][1] == 'p')) {
    lpt_addr = (argv[1][2] == '0') ? 0x378 : 0x278;
    rba=fopen(argv[2],"rb");
  } else
    rba=fopen(argv[1],"rb");

  len=fread(rbt,1,256000,rba);
  fclose(rba);
  if (rbt[0]==0xff && rbt[1]==0xff) {
    load_altera(len);
    exit(0);
  } else {
    fprintf(stderr, "Wrong file format\n");
    exit(1);
  }
}
