/*
 * USB core
 *
 * $Log: usb.v,v $
 * Revision 1.7  2004/05/02 21:40:21  bernd
 * Changes for USB and Cyclone
 *
 * Revision 1.6  2003/01/11 21:00:47  bernd
 * Added scan signal to enable clock gating during scan
 *
 * Revision 1.5  2003/01/11 20:57:52  bernd
 * Cleaned up clock gating
 *
 * Revision 1.4  2003/01/06 20:35:21  bernd
 * Changes to run with Icarus Verilog
 * USB interrupts
 * Added interrupts to the b16 core
 *
 * Revision 1.3  2003/01/06 14:20:51  bernd
 * Fixed odd byte sending/receiving
 *
 * Revision 1.2  2003/01/05 21:59:03  bernd
 * First completely working version
 *
 * Revision 1.1  2003/01/05 12:09:36  bernd
 * Initial checkin
 *
 */

`timescale 1ns / 1ns

`ifdef CLK_GATE
 `define AT_BIT 1'b1
 `define AT_BYTE 1'b1
`else
 `define AT_BIT usbclk
 `define AT_BYTE byteclk
`endif
`ifdef SCANEN
`else
 `define SCANEN 1'b0
`endif

/* CRC module generator
 *
 * These people at Intel should be shot. This is not how a CRC should
 * look like. I found out a trick to make it to be as compact as possible,
 * but I still don't like it. For the 5 bit CRC, going for a zero residual
 * is possible, but for the 16 bit CRC, you can't know when the packet ends,
 * so you have to check for the residual (inverted here). When sending, the
 * residual is zero in any case.
 */

module usb_crc(clk, usbclk, reset, en, send, tx, crc16, out, val);
   input clk, usbclk, reset, en, send, tx, crc16;
   output out, val;
   
   reg [15:0] crc;

   assign out = crc16 ? crc[15] : crc[4];
   assign val = crc16 ? (crc == 16'h7FF2) : (crc[4:0] == 5'h00);
   
   always @(posedge clk or negedge reset)
     if(!reset)
       crc <= 16'h0000;
     else if(en & `AT_BIT)
       crc <= { crc[14:0], ~send } ^ ((~send ^ tx ^ out) ? 16'h8005 : 16'h0);

endmodule // usb_crc

/* Bit handling
 * 
 * This handles the bit serial part. Four times oversampling is used to
 * get more reliable data. The actual transition phase is ignored.
 * This module also delivers a bit clock, which is a digitally locked
 * PLL signal, locked on the transition. EOP detection happens here, too.
 * Finally, it also flags bit stuffing
 */

module usb_bit(clk, reset, dif, se0, send,
	       bitclk, bitin, stuff, endp);
   input clk, reset, dif, se0, send;
   output bitclk, bitin, stuff, endp;

   reg [3:0] oversample;
   reg [4:0] bitcount;
   reg [2:0] eop;
   reg 	     bitin;

   wire [3:0] phase = 1 << bitcount[1:0];
   assign     stuff = bitcount[4:2] == 3'h7;
   assign     bitclk = phase[0];
   assign     endp = send ? &eop : eop[2];

   always @(posedge clk or negedge reset)
     if(!reset) begin
	oversample <= 4'b1111;
	bitcount   <= 5'b11111;
	eop        <= 3'b000;
	bitin      <= 1;
     end else begin
        eop <= se0 ? eop + ~&eop : 3'b000;
        oversample <= endp ? 4'b1111 : { oversample[2:0], dif };
        bitcount   <= endp ? 5'h1F : bitcount + (send | ~&bitcount);
        if(|bitcount[4:2]) begin
           if((oversample[3] == oversample[2]) &&
	      (oversample[2] != oversample[0]) &&
	      (oversample[0] == dif) && !phase[0])
             begin
                if(!stuff || bitcount == 5'h1F)
		  bitin <= 1'b0;
                bitcount <= 5'h02;
             end
           else if(phase[3] && |bitcount[4:2] && ~&bitcount[4:2])
	     bitin <= 1'b1;
	end
     end
   
endmodule // usb_bit

/* Byte handling
 * 
 * This handles byte edge detection. A byte clock is delivered,
 * and a indication when crc5 sending/receiving happens.
 */

module usb_byte(clk, usbclk, reset, bit, stuff, eop, send, nextbyte,
		byte, byteclk, crc5, bytetop);
   input clk, usbclk, reset, bit, stuff, eop, send;
   input [7:0] nextbyte;
   output [7:0] byte;
   output 	byteclk, crc5, bytetop;

   reg [7:0] 	byte;
   reg [3:0] 	count;
   
   wire 	count_rst = reset & ~eop;
   assign 	byteclk = ~|count[2:0];
   assign 	crc5 = count > 4'hA;
   assign 	bytetop = count[3];

   always @(posedge clk or negedge count_rst)
     if(!count_rst) begin
	count <= 0;
     end else if(!stuff & `AT_BIT) begin
	count <= count + 1;
     end

   always @(posedge clk or negedge reset)
     if(!reset) begin
	byte <= 8'h00;
     end else if(!stuff & `AT_BIT) begin
	byte <= send & byteclk ? nextbyte : { bit, byte[7:1] };
     end

endmodule // usb_byte

/* Receiving state
 * 
 * USB can either wait for a sync byte
 * read in the PID
 * or read in data
 * or be in an error state
 * 
 * State is reset by eop or USB reset
 * 
 * After transmission is completed successfull,
 * ok is set.
 */

module usb_state(clk, reset, eop, crc, crc5, byte, byteclk, send,
		 state, ok);
   parameter show=0;
   input clk, reset, eop, crc, crc5, byteclk, send;
   input [7:0] byte;
   output [1:0] state;
   output 	ok;

   reg [1:0] 	state;
   reg 		ok;

   wire 	state_rst = reset & ~eop;
   
   always @(posedge clk or negedge state_rst)
     if(!state_rst) begin
	state <= 2'b00;
	if(show) $display("eop");
     end else if(`AT_BYTE) begin
	case(state)
	  2'b00: begin
	     if(send | byte == 8'h80) begin
		state <= 2'b01; // pid state
	     end
	  end
	  2'b01: begin
	     if(send || byte[3:0] != 0 || (~byte[7:4]) == byte[3:0]) begin
		state <= 2'b10; // data state
	     end else begin
		state <= 2'b11; // error state
	     end
	  end
	  2'b10: begin
	     if(crc5) state <= 2'b11; // error state
	  end
	endcase // case(state)
     end
       
   always @(posedge clk or negedge reset)
     if(!reset) begin
	ok <= 0;
     end else if(`AT_BYTE) begin
	case(state)
	  2'b00: ok <= 1;
	  2'b10: begin
	     ok <= crc;
	     if(show & crc) $display("packet ok");
	  end
	endcase // case(state)
     end
       
endmodule // usb_state

/* Top level module
 * 
 * Instantiates submodules, handles CPU interface
 * 
 * Registers are as follows:
 * 
 * 00: state+cmd PID, ok, sendz, countsend
 * 02: buf16,    buffer for short messages and DMA
 * 04: addr      address for DMA (0: no DMA)
 */

module usb(clk, reset, dpin, dnin, dif, send, dp, dn,
	   sel, addr, r, w, dma, reqr, reqw1, ack, din, dout,
	   pid, intreq, intack);
   parameter show=0;

   input clk, reset;
   input dpin, dnin, dif;
   output send, dn, dp;
   input sel, r, ack;
   input [1:0] addr, w;
   input [15:0] din;
   output [15:0] dma, dout;
   output 	 reqr, reqw1;
   output [3:0]  pid;
   output 	 intreq;
   input 	 intack;
   
   wire [7:0] byte;
   wire       usbclk, byteclk, bytetop;
   wire       crc5, bitin;
   wire [1:0] state;
   reg 	      crcsend, send, dp, dn;
   reg 	      sendz, sendcrc, sendcrcen;
   reg [15:0] dmb, dma;
   reg [3:0]  pid;
   reg [9:0]  count;
   reg 	      suppress;
   reg 	      byteclk1, sendz1;
   
   wire       se0 = ~(dpin | dnin);
   wire       stuff, crcout, crcval, eop, ok;
   wire       tx = (sendcrc & sendcrcen) ? crcout : byte[0];
   wire       bit = send ? tx : bitin;
   wire       crcen = state == 2'b10;     // data receiving state
   wire       crc16 = pid[2:0] == 3'b011; // data0 or data1
   wire       reccrc = crc16 ? crcsend : crc5;
   wire [7:0] nextbyte = state[1] ? bytetop ? dmb[15:8] : dmb[7:0]
                                  : bytetop ? { ~pid, pid } : 8'h80;
   wire       bytepulse = byteclk & ~byteclk1;
   
`ifdef CLK_GATE
   wire       bit_clk = ~(`SCANEN | usbclk) | clk;
   wire       byte_clk = ~(`SCANEN | bytepulse) | clk;
   wire       req_clk = ~(`SCANEN | ack | eop | usbclk) | clk;
   wire       reg_clk = ~(`SCANEN | ack | eop | bytepulse | intack | sel | suppress) | clk;
`else
   wire       bit_clk = clk;
   wire       byte_clk = clk;
   wire       req_clk = clk;
   wire       reg_clk = clk;
`endif
   
   usb_crc crc_(bit_clk, usbclk, send ? sendcrcen : crcen, ~stuff,
		send ? sendcrc : reccrc,
		bit, crc16, crcout, crcval);
   usb_bit bit_(clk, reset, dif, se0, send, usbclk, bitin, stuff, eop);
   usb_byte byte_(bit_clk, usbclk, reset, bit, stuff, eop, send, nextbyte,
		  byte, byteclk, crc5, bytetop);
   usb_state #(show) state_(byte_clk, reset, eop, crcval, ~crc16 & crc5,
			    byte, bytepulse & ~suppress, send, state, ok);

   // register part

   // delay chain for DMA request - must be delayed to detect EOP
   reg 	  intreq, reqr, reqw, reqw0, reqw1;

   always @(posedge req_clk or negedge reset)
     if(!reset)
       { reqw1, reqw0 } <= 2'b00;
     else if(ack | eop)
       { reqw1, reqw0 } <= 2'b00;
     else if(usbclk)
       { reqw1, reqw0 } <= { reqw0, reqw };

   always @(posedge clk or negedge reset)
     if(!reset)
       byteclk1 <= 1;
     else
       byteclk1 <= byteclk;

   wire   suppress_rst = byteclk & reset;
   wire   send_rst = reset & ~eop;

   always @(posedge usbclk or negedge send_rst)
     if(!send_rst) begin
	{ dp, dn } <= 2'b10;
	sendz1 <= 0;
     end else begin
	if(!suppress & send & (!tx | stuff))
	  { dp, dn } <= sendz1 ? 2'b00 : { dn, dp };
	sendz1 <= sendz;
     end
   
   always @(negedge usbclk or negedge send_rst)
     if(!send_rst) begin
	sendcrc <= 0;
	sendcrcen <= 0;
     end else begin
	sendcrc <= reccrc;
	sendcrcen <= crcen;
     end
  
   always @(posedge reg_clk or negedge suppress_rst)
     if(!suppress_rst) suppress <= 0;
     else if(sel && w[1] && addr == 2'b00) suppress <= din[12];

   always @(posedge reg_clk or negedge reset)
     if(!reset) { send, sendz } <= 2'b00;
     else if(sel && w[1] && addr == 2'b00) { send, sendz } <= din[11:10];
     else if(bytepulse && state != 2'b00 && count == 10'h0) sendz <= 1;
     else if(eop) send <= 0;

   assign dout =
	  (ack & reqw) ? dmb :
	  (sel & r && addr == 2'b00) ? { pid, ok, sendz, count } :
	  (sel & r && addr == 2'b01) ? dmb :
	  (sel & r && addr == 2'b10) ? dma : 16'hzzzz;
   
   always @(posedge reg_clk or negedge reset)
     if(!reset) begin
	crcsend  <= 0;
	pid      <= 0;
	reqr     <= 0;
	reqw     <= 0;
	intreq   <= 0;
	count    <= 10'h000;
	dmb      <= 16'h0000;
	dma      <= 16'h0000;
     end else begin
	if(intack) intreq <= 0;
	if(eop & ~se0 & (ok | send)) begin
	   intreq <= 1;
	   if(send) pid <= 4'b0000;
	end
	if(ack) begin
	   // byte swap for sending strings in right order
	   if(reqr) dmb <= { din[7:0], din[15:8] };
	   dma <= dma + 16'h2;
	end
	if(ack | eop) begin
	   reqr <= 0;
	   reqw <= 0;
	end
	if(sel) begin
	   if(show && w) $display("usb[%x] <= %x", addr, din);
	   case(addr)
	     2'b00: begin
		if(w[1]) { pid, count[9:8] } <= { din[15:12], din[9:8] };
		if(w[0]) count[7:0] <= din[ 7:0];
		if(w[1] & din[11]) begin
		   if(din[14:12] == 3'b011) reqr <= 1;
		   crcsend <= 1'b0;
		end
	     end
	     2'b01: begin
		if(w[1]) dmb[15:8] <= din[15:8];
		if(w[0]) dmb[ 7:0] <= din[ 7:0];
	     end
	     2'b10: begin
		if(w[1]) dma[15:8] <= din[15:8];
		if(w[0]) dma[ 7:0] <= din[ 7:0];
	     end
	   endcase // case(addr)
	end
	if(show && bytepulse) begin
	   case(state)
	     2'b00: $display("sync %x", byte);
	     2'b01: $display("pid  %x", byte[3:0]);
	     2'b10: $display("byte %x", byte);
	     2'b11: $display("err  %x", byte);
	   endcase // case(state)
	end
	if(bytepulse) begin
	   if(state == 2'b01) begin
	      if(!send) pid <= byte[3:0];
	      if(send && count == 10'h1) crcsend <= crc16;
	   end
	   if(crcen) begin
	      if(!send)
		if(bytetop ^ (~send & ~crc16))
		  dmb[15:8] <= byte;
		else
		  dmb[ 7:0] <= byte;
	      if(count != 10'h0) begin
		 count <= count - 10'h1;
	      end
	      if(count >= 10'h2) begin
		 if(send) reqr <= ~bytetop;
		 else     reqw <= ~bytetop;
	      end
	      if(send && count == 10'h2) begin
		 crcsend <= crc16;
		 reqr <= 0;
	      end
	   end
	end
     end

endmodule // usb
