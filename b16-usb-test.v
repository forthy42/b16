/* test module for c16 core */

`define L [l-1:0]

`timescale 1ns / 1ns

module memory(reset, sel, addr, r, w, data, dout);
   parameter l=16;
   input     reset;
   input     `L addr;
   input     sel, r;
   input [1:0] w;
   output      `L data;
   input       `L dout;

   reg 	       `L mem[((1<<(l-1)))-1:0];
   wire        `L dstore, dfetch;

   assign data = (r & sel) ? dfetch : 16'hzzzz;
   assign dfetch = mem[addr[l-1:1]];
   assign dstore = { w[1] ? dout[15:8] : dfetch[15:8],
		     w[0] ? dout[7:0]  : dfetch[7:0] };
   
   always @(w)
      if(reset & sel & |w) begin
	 #1 $display("cpu: mem[%x] <= %x", addr[l-1:1], dstore);
	 mem[addr[l-1:1]] <= dstore;
      end

   initial
      $readmemh("b16.hex", mem);

//   always @(data)
//      $display("data=%x[%x]", data, addr[l-1:1]);
      
endmodule // memory

module test_top;
   parameter l=16;
   wire `L mem_addr, addr, data, dout, dma;
   wire r, mem_r;
   wire [1:0] w, mem_w;
   reg 	      clk, reset, ack;
   wire       DP, DN;
   wire [3:0] pid;

   wire       intreq;
   wire [7:0] intvec = { 1'b0, pid, 3'b000 };

   assign     (weak0, weak1) { DP, DN } = 2'b10;
   assign     mem_r = ack ? reqr : r;
   assign     mem_w = ack ? {(2){reqw}} : w;
   assign     mem_addr = ack ? dma : addr;
   assign     data = ~|w ? 16'hzzzz : dout;
   wire       usb_sel = &addr[15:3];
   wire       mem_sel = ~usb_sel;
   
   usbphys usbp0(DP, DN, dif0, send0, dp0, dn0);
   usbphys usbp1(DP, DN, dif1, send1, dp1, dn1);
   
   memory mem1(reset, mem_sel, mem_addr, mem_r, mem_w, data, data);
   cpu    cpu1(clk, reset, addr, r, w, data, dout, intreq, intack, intvec);
   usb #(1) usb1(clk, reset, DP, DN, dif0, send0, dp0, dn0,
		 usb_sel, addr[2:1], r, w, dma, reqr, reqw, ack, data, data,
		 pid, intreq, intack);

   wire       cpu_rw = r | |w;

   always @(negedge cpu_rw or negedge reqr or negedge reqw)
     ack <= reqr | reqw;
   
   reg 	      selu;
   reg [1:0]  addru, wu;
   reg 	      ru, acku;
   reg 	      `L datau;
   reg [15:0]  memory[0:32367];
   wire [15:0] doutu, dmau;
   wire        reqru, reqwu;

   usb #(0) usb2(clk, reset, DP, DN, dif1, send1, dp1, dn1,
		 selu, addru, ru, wu, dmau, reqru, reqwu, acku,
		 datau, doutu, , , 0);

   always @(posedge clk)
     if(acku !== 1'b0) begin
	acku <= 1'b0;
	if(reqwu) memory[dmau[15:1]] <= doutu;
	if(reqwu) $display("usb: mem[%x] <= %x", dmau, doutu);
     end else if(reqru | reqwu) begin
	acku <= 1;
	if(reqru) datau <= memory[dmau[15:1]];
	if(reqru) $display("usb: %x <= mem[%x]", memory[dmau[15:1]], dmau);
     end

   initial
     begin
	ack <= 0;
	reset <= 0;
	clk <= 0;
	#10 reset <= 1;
	repeat (2000)
	  #10 clk <= ~clk;
	$finish;
     end
   
   initial
      begin
	 $dumpfile("b16-usb.vcd");
	 $dumpvars();
      end // UNMATCHED !!

   task write;
      input [1:0] addrin;
      input [15:0] data;
      begin
	 @(posedge clk) selu <= 1;
	 addru <= addrin;
	 datau <= data;
	 ru <= 1'b0;
	 wu <= 2'b11;
	 @(posedge clk) selu <= 0;
      end
   endtask // write

   initial
     begin
	#100 write(1, 16'h1234);
	write(2, 16'h5678);
	write(0, 16'h5801);
	#4000 memory[15'h2B3C] = 16'h1234;
	memory[15'h2B3D] = 16'h5678;
	memory[15'h2B3E] = 16'h9ABC;
	memory[15'h2B3F] = 16'hDEF0;
	write(1, 16'h0000);
	write(0, 16'h9801);
	#4000 write(0, 16'h3809);
     end
   
endmodule // test_top
