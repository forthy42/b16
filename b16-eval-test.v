// b16 evalboard test environment

/*
 * $Log: b16-eval-test.v,v $
 * Revision 1.5  2003/01/06 20:35:21  bernd
 * Changes to run with Icarus Verilog
 * USB interrupts
 * Added interrupts to the b16 core
 *
 * Revision 1.4  2002/03/31 23:08:39  bernd
 * Made downloader work. Improved testbench to allow serial communication
 *
 * Revision 1.3  2002/03/24 17:46:15  bernd
 * Added bit-bang input
 *
 * Revision 1.2  2002/03/23 21:35:57  bernd
 * Made b16 faster
 *
 * Revision 1.1  2002/02/17 23:18:58  bernd
 * Initial version of test environment
 *
 */

`include "b16-eval.v"
`include "b16.v"

module eval_test;
   parameter l=16;
   reg clk, reset;
   reg `L din;
   wire `L dout, a;
   wire `L d;
   wire wr_b, rd_b, ble_b, bhe_b;

   b16_eval flk10k30e(clk, reset, din, dout, a, d, wr_b, rd_b, ble_b, bhe_b);

   integer oldtime;
   
   always @(dout)
      begin
	 $display("%d: dout <= %b", $time-oldtime, dout);
	 oldtime <= $time;
      end

   initial
      begin
	 reset <= 1;
	 clk <= 0;
	 #1000 reset <= 0;
	 forever #50 clk <= ~clk;
      end

   task send_byte;
      input [7:0] byte;
      begin
	 #8700 din[0] <= 0;
	 #8700 din[0] <= byte[0];
	 #8700 din[0] <= byte[1];
	 #8700 din[0] <= byte[2];
	 #8700 din[0] <= byte[3];
	 #8700 din[0] <= byte[4];
	 #8700 din[0] <= byte[5];
	 #8700 din[0] <= byte[6];
	 #8700 din[0] <= byte[7];
	 #8700 din[0] <= 1;
	 #8700 din[0] <= 1;
      end
   endtask
   
   initial
      begin
	 din <= 16'h0001;
	 #100000 send_byte(8'h30); // start bit
	 send_byte(8'h03);
	 send_byte(8'h02);
	 send_byte(8'h04);
	 send_byte(8'h12);
	 send_byte(8'h34);
	 send_byte(8'h56);
	 send_byte(8'h78);
	 #100000 send_byte(8'h31);
	 send_byte(8'h03);
	 send_byte(8'h02);
	 send_byte(8'h04);
      end
   
endmodule // eval_test



