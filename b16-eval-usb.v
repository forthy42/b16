// Interface to elements on the evaluation board

/* 
 * $Log: b16-eval-usb.v,v $
 * Revision 1.2  2004/05/02 21:40:21  bernd
 * Changes for USB and Cyclone
 *
 * Revision 1.1  2004/05/02 19:22:22  bernd
 * Initial checkin
 *
 */

`define L [l-1:0]

module b16_eval_usb(clk, nreset, a, d, wr_b, rd_b, ble_b, bhe_b,
		    DP, DN, dif, send, dp, dn);
   parameter l=16, bootram="b16.hex";
   input clk, nreset;
   output `L a;
   inout  `L d;
   output wr_b, rd_b, ble_b, bhe_b;
   input DP, DN, dif;
   output send, dp, dn;
   
   wire `L mem_addr, addr, dout, dout_boot, dma, data;
   wire r, mem_r, intreq, intack, reqr, reqw;
   wire [3:0] pid;
   wire [7:0] intvec = { 1'b0, pid, 3'b000 };
   wire [1:0] w, mem_w;
   reg [2:0]  sel;
   reg [1:0]  dout24;
   reg 	      ack;

   wire       nrst = nreset;
   wire       reset = nreset;
   wire       cpu_rw = |w | r;

   always @(negedge cpu_rw or negedge reqr or negedge reqw)
     ack <= reqr | reqw;

   assign     mem_r = ack ? reqr : r;
   assign     mem_w = ack ? {(2){reqw}} : w;
   assign     mem_addr = ack ? dma : addr;
   wire       usb_sel = sel[2];
   wire       mem_sel = ~usb_sel;

   cpu #(1) b16(clk, nrst, addr, r, w, data, dout, intreq, intack, intvec);
   usb #(1) usb1(clk, reset, DP, DN, dif, send, dp, dn,
		 usb_sel, addr[2:1], r, w, dma, reqr, reqw, ack, data, data,
		 pid, intreq, intack);
   ram1024 #(bootram) ram(mem_addr[10:1], w, ~clk, data, sel[1] & |w, dout_boot);

   assign a = { 1'b0, mem_addr[l-1:1] };
   assign d = mem_r ? 16'hzzzz : data;

   always @(addr)
      if(&addr[15:3]) sel <= 3'b100;
      else if(addr[15:11] == 5'h00) sel <= 3'b010;
	   else sel <= 3'b001;
  
   reg 	      wr_b, rd_b, ble_b, bhe_b;
   
   always @(clk or nrst or r or w or sel)
      if(sel[0] & nrst) begin
	 rd_b <= ~r;
	 { wr_b, ble_b, bhe_b } <= 3'b111;
	 if(!clk) begin
	    wr_b <= ~|w;
	    { bhe_b, ble_b } <= ~w & { ~r, ~r };
	 end
      end
      else { wr_b, rd_b, ble_b, bhe_b } <= 4'b1111;

   assign data = mem_w ? dout :
	  { r, sel } == 4'b1010 ? dout_boot :
	  { r, sel } == 4'b1010 ? d : 16'hzzzz;

endmodule
   

