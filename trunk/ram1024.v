// behavioural verison of ram1024 (boot ram)

module ram1024 (address, byteena, clock, data, wren, q);
   parameter bootram="b16.hex";
   input [9:0] address;
   input [1:0] byteena;
   input       clock;
   input [15:0] data;
   input 	wren;
   output [15:0] q;
   
   reg [15:0] 	 ram[0:1023];
   reg [15:0] 	 q;

   wire [15:0] 	 dfetch = ram[address];
   wire [15:0] 	 dstore = { byteena[1] ? data[15:8] : dfetch[15:8],
			    byteena[0] ? data[7:0]  : dfetch[7:0] };
   
   always @(posedge clock)
     begin
	if(wren) ram[address] <= dstore;
	q <= #2 dfetch;
     end

   initial
      $readmemh(bootram, ram);
   
endmodule // ram1024
