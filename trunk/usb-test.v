/*
 * USB test module
 * 
 * $Log: usb-test.v,v $
 * Revision 1.5  2004/05/02 21:40:21  bernd
 * Changes for USB and Cyclone
 *
 * Revision 1.4  2003/01/06 20:35:21  bernd
 * Changes to run with Icarus Verilog
 * USB interrupts
 * Added interrupts to the b16 core
 *
 * Revision 1.3  2003/01/06 14:21:05  bernd
 * Fixed odd byte sending/receiving
 *
 * Revision 1.2  2003/01/05 21:59:11  bernd
 * First completely working version
 *
 * Revision 1.1  2003/01/05 12:09:50  bernd
 * Initial checkin
 *
 */

`timescale 1ns / 1ns

`define USBBIT 40

// token

`define OUT   4'b0001
`define IN    4'b1001
`define SOF   4'b0101
`define SETUP 4'b1101
`define ACK   4'b0010
`define NAK   4'b1010
`define STALL 4'b1110
`define PRE   4'b1100
`define DATA0 4'b0011
`define DATA1 4'b1011

module usbtest;
   reg [15:0] crc;
   reg 	      crcen, crc16;
   reg [2:0]  bitstuff;
   reg 	      usbclk;
   wire       usbclk4;
   reg 	      usbclken;
   reg 	      sendcrc;
   
   wire       dif;
   reg 	      olddif, tx;
   wire       DP, DN;
   reg 	      dp, dn;
   reg 	      reset;
   
   reg 	      sel, r, ack, intack;
   reg [1:0]  addr, w;
   reg [15:0] din;
   wire [15:0] dout, dma;
   wire        intreq, reqr, reqw;
   wire [3:0]  pid;

   reg [15:0]  memory[0:32367];

   assign     { DP, DN } = { dp, dn };

   wire       tx = (sendcrc & sendcrcen) ? crcout : byte[0];
   
   assign     dif = DP & ~DN;
   assign #(`USBBIT/4) usbclk4 = usbclken & ~usbclk4;

   always @(posedge usbclk4)
     if(ack !== 1'b0) begin
	ack <= 1'b0;
	if(reqw) memory[dma[15:1]] <= dout;
	if(reqw) $display("mem[%x] <= %x", dma, dout);
     end else if(reqr | reqw) begin
	ack <= 1;
	if(reqr) din <= memory[dma[15:1]];
	if(reqr) $display("%x <= mem[%x]", memory[dma[15:1]], dma);
     end

   always @(posedge usbclk4)
     if(intreq) begin
	intack <= 1;
	if(!intack) $display("USB IRQ pid %x", pid);
     end else begin
	intack <= 0;
     end

   usbphys usbp0(DP, DN, dif0, send0, dp0, dn0);
   usbphys usbp1(DP, DN, dif1, send1, dp1, dn1);
   
   usb #(0) usb0(usbclk4, reset, DP, DN, dif0, send0, dp0, dn0,
		 sel, addr, r, w, dma, reqr, reqw, ack, din, dout,
		 pid, intreq, intack);
   usb #(1) usb1(usbclk4, reset, DP, DN, dif1, send1, dp1, dn1,
		 0, 0, 0, 0, , , , 0, 0, , , , 0);
   
   always @(negedge usbclk)
     begin
	if(crcen && (bitstuff != 6))
	  crc <= { crc[14:0], ~sendcrc } ^
				 (~sendcrc ^ tx ^ (crc16 ? crc[15] : crc[4])
				  ? 16'h8005 : 16'h0);
        olddif <= dif;
     end
   
   reg [4:0] bitcount;
   reg [3:0] oversample;
   reg [7:0] bytein;
   reg [2:0] bits;
   reg [3:0] eop;
   wire      se0;

   assign    se0 = ~(DP | DN);

   always @(posedge usbclk4)
     begin
        oversample <= { oversample[2:0], dif };
        bitcount <= bitcount + ~&bitcount;
        if(|bitcount[4:2]) begin
           if((oversample[2] ^ oversample[0]) && (bitcount[1:0] != 2'b00))
             begin
                if(bitcount[4:2] != 3'h7 || bitcount == 5'h1F) begin
                   bytein <= { 1'b0, bytein[7:1] };
                   bits <= bits + 1;
                end
                bitcount <= 5'h02;
             end
           else if((bitcount[1:0] == 2'b11) && |bitcount[4:2] && ~&bitcount[4:2])
             begin
                bytein <= { 1'b1, bytein[7:1] };
                bits <= bits + 1;
             end
        end
        eop <= se0 ? eop + 1 : 0;
	if(eop[2]) begin
	   bits <= 0;
	   bitcount <= 5'h1F;
	end
     end

   task sendbit;
      input bit;
      begin
	 usbclk <= 1;
	 tx <= bit;
//	 $display("send bit %b", bit);
	 if(bitstuff == 6) begin
	    { dp, dn } <= { dn, dp };
	    bitstuff <= 0;
	    #(`USBBIT) usbclk <= 0;
	    #(`USBBIT) usbclk <= 1;
	 end
	 if(!bit) begin
	    { dp, dn } <= { dn, dp };
	    bitstuff <= 0;
	 end else bitstuff <= bitstuff + 1;
	 #(`USBBIT) usbclk <= 0;
	 #(`USBBIT) usbclk <= 1;
      end
   endtask // sendbit

   task sendbyte;
      input [7:0] byte;
      begin
//	 $display("send byte %x", byte);
	 repeat(8) begin
	    sendbit(byte[0]);
	    byte = byte[7:1];
	 end
      end
   endtask // sendbyte

   task sendpid;
      input [3:0] pid;
      begin
	 crc = 0;
	 sendcrc = 0;
	 crcen = 0;
	 { dp, dn } = 2'b10;
	 sendbyte(8'h80); // sync
	 sendbyte({ ~pid, pid });
      end
   endtask // sendpid
   
   task eop;
      begin
	 #(`USBBIT*0) { dp, dn } <= 2'b00;
	 #(`USBBIT*4) { dp, dn } <= 2'b10;
	 #(`USBBIT*2) { dp, dn } <= 2'bzz;
      end
   endtask // eop

   task sendtoken;
      input [3:0] pid;
      input [6:0] addr;
      input [3:0] endp;
      begin
	 sendpid(pid);
	 crcen = 1;
	 crc16 = 0;
	 repeat(7) begin
	    sendbit(addr[0]);
	    addr <= addr[6:1];
	 end
	 repeat(4) begin
	    sendbit(endp[0]);
	    endp <= endp[3:1];
	 end
	 sendcrc = 1;
	 repeat(5) begin
	    sendbit(crc[4]);
	 end
	 sendcrc = 0;
	 eop;
      end
   endtask

   task senddata;
      input toggle;
      input [2:0] count;
      input [63:0] data;
      begin
	 sendpid(toggle ? `DATA1 : `DATA0);
	 crcen = 1;
	 crc16 = 1;
	 repeat(count+1) begin
	    sendbyte(data[63:56]);
	    data = { data[55:0], 8'h00 };
	 end
	 sendcrc = 1;
	 repeat(16) begin
	    sendbit(crc[15]);
	 end
	 sendcrc = 0;
	 eop;
      end
   endtask // senddata

   task write;
      input [1:0] addrin;
      input [15:0] data;
      begin
	 @(posedge usbclk4) sel <= 1;
	 addr <= addrin;
	 din <= data;
	 r <= 1'b0;
	 w <= 2'b11;
	 @(posedge usbclk4) sel <= 0;
      end
   endtask // write
   
`define SENDTEST
`define RECEIVETEST
   
   initial
     begin
	$dumpfile("usb.vcd");
	$dumpvars();

	reset = 0;
	sel = 0;
	usbclk = 0;
	usbclken = 0;
	dp = 1'bz;
	dn = 1'bz;
	bitstuff = 0;
	bitcount = 0;
	oversample = 0;
	bytein = 0;
	bits = 0;
	eop = 0;
	crcen = 0;
	crc16 = 0;
	sendcrc = 0;
	crc = 0;
	#100 reset = 1;
	usbclken = 1;
	`ifdef SENDTEST
	#1000 sendtoken(`SOF, 6'h00, 4'h0);
	#1000 sendtoken(`SETUP, 6'h12, 4'h3);
	write(2, 16'h2000);
        write(0, 16'h000B);
	#1000 senddata(0, 3'h7, 64'h123456789ABCDEF0);
	#1000 sendtoken(`OUT, 6'h7F, 4'hF);
	write(2, 16'h3000);
	write(0, 16'h0005);
	#1000 senddata(1, 3'h7, 64'hFEDCBA9876543210);
	`endif
	`ifdef RECEIVETEST
	#1000 write(1, 16'h1234);
	write(2, 16'h5678);
	write(0, 16'h5801);
	#4000 memory[15'h2B3C] = 16'h1234;
	memory[15'h2B3D] = 16'h5678;
	memory[15'h2B3E] = 16'h9ABC;
	memory[15'h2B3F] = 16'hDEF0;
	write(0, 16'h3809);
	#10000 write(2, 16'h5678);
	write(0, 16'hB806);
	#10000 write(0, 16'h3800);
	#3000 write(0, 16'h0000);
	`endif
	$finish;
     end
   
endmodule // usbtest
