// behavioural verison of stackram

module stackram(data, wren, wraddress, rdaddress, clock, q);
   input [15:0] data;
   input 	wren;
   input [2:0] 	wraddress;
   input [2:0] 	rdaddress;
   input 	clock;
   output [15:0] q;

   reg [15:0] 	 ram[0:7];
   reg [15:0] 	 q;

   always @(posedge clock)
     begin
	if(wren) ram[wraddress] <= data;
	q <= ram[rdaddress];
     end
   
endmodule // stackram

