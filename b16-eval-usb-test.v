// test b16+usb eval code

/*
 * $Log$
 */

module test;
   wire [15:0] a0, a1, d0, d1;
   wire        DP, DN, send0, send1;
   reg 	       clk, nreset;
   
   assign { DP, DN } = (send0 | send1) ? 2'bzz : 2'b10;

   b16_eval_usb #(16, "master.hex")
     core0(clk, nreset, a0, d0, wr_b0, rd_b0, ble_b0, bhe_b0,
	   DP, DN, dif0, send0, dp0, dn0);
   b16_eval_usb #(16, "slave.hex")
     core1(clk, nreset, a1, d1, wr_b1, rd_b1, ble_b1, bhe_b1,
	   DP, DN, dif1, send1, dp1, dn1);

   usbphys usbp0(DP, DN, dif0, send0, dp0, dn0);
   usbphys usbp1(DP, DN, dif1, send1, dp1, dn1);

   initial
     begin
	$dumpfile("b16-eval-usb.vcd");
	$dumpvars();
	nreset <= 0;
	clk <= 0;
	#10000 nreset <= 1;
	repeat(10000)
	  #10 clk <= ~clk;
	$finish;
     end
  
endmodule // test
