/*
 * b16 core: 16 bits, 
 * inspired by c18 core from Chuck Moore
 *
 * Instruction set:
 * 1, 5, 5, 5 bits
 *     0    1    2    3    4    5    6    7
 *  0: nop  call jmp  ret  jz   jnz  jc   jnc
 *  /3      exec goto ret  gz   gnz  gc   gnc
 *  8: xor  com  and  or   +    +c   *+   /-
 * 10: A!+  A@+  R@+  lit  Ac!+ Ac@+ Rc@+ litc
 *  /1 A!   A@   R@   lit  Ac!  Ac@  Rc@  litc
 * 18: nip  drop over dup  >r   >a   r>   a
 */
 
`define L [l-1:0]
`define DROP { sp, T, N } <= { spinc, N, toN } 
`timescale 1ns / 1ns

module alu(res, carry, zero, T, N, c, inst);
   parameter l=16;
   input `L T, N;
   input c;
   input [2:0] inst;
   output `L res;
   output carry, zero;

   wire prop, andor, selr;

   assign #1 { prop, andor, selr } = inst;
   
   wire        `L sum, logic;
   wire        cout;
   
   assign { cout, sum } = 
          T + N + ((c | andor) & selr);
   assign logic = andor ? 
                  (selr ? (T | N) : (T & N)) : 
                  T ^ N;
   assign { carry, res } =
          prop ? { cout, sum } : { c, logic };
   assign zero = ~|T;
   
endmodule // alu
module stack(clk, sp, spdec, push, in, out);
   parameter dep=3, l=16;
   input clk, push;
   input [dep-1:0] sp, spdec; 
   input `L in;
   output `L out; 
   reg `L stackmem[0:(1<<dep)-1];
   wire [dep-1:0]  spset;

`ifdef BEH_STACK
   always @(clk or push or spset or in)
     if(push & ~clk) stackmem[spset] <= #1 in;
 
   assign spset = push ? spdec : sp;
   assign #1 out = stackmem[spset];
`else
   stackram stram(in, push, spdec, sp, ~clk, out);
`endif
endmodule // stack
module cpu(clk, reset, addr, rd, wr, data, T, 
           intreq, intack, intvec);
   parameter show=0, l=16, sdep=3, rdep=3;
   input clk, reset;
   output `L addr;
   output rd;
   output [1:0] wr;
   input  `L data;
   output `L T;
   input  intreq;
   output intack;
   input [7:0] intvec; // interrupt jump vector
   reg    rd;
   reg [1:0] wr;
   reg [sdep-1:0] sp;
   reg [rdep-1:0] rp;

   reg `L T, N, I, P, A, addr;

   reg [2:0] state;
   reg c;
   reg incby;
   reg intack;
   // instruction and branch target selection   
   reg [4:0] inst;
   reg `L jmp;

   always @(state or I)
      case(state[1:0])
        2'b00: inst <= { 4'b0000, I[15] };
        2'b01: inst <=   I[14:10];
        2'b10: inst <=   I[9:5];
        2'b11: inst <=   I[4:0];
      endcase // casez(state)

   always @(state or I or P or T)
      case(state[1:0])
        2'b00: jmp <= { I[14:0], 1'b0 };
        2'b01: jmp <= { P[15:11], I[9:0], 1'b0 };
        2'b10: jmp <= { P[15:6], I[4:0], 1'b0 };
        2'b11: jmp <= { T[15:1], 1'b0 };
      endcase // casez(state)
   wire `L res, toN;
   wire carry, zero;

   alu #(l) alu16(res, carry, zero, 
                  T, N, c, inst[2:0]);
   wire `L toaddr, incaddr, toR, R;
   wire tos2r;

   assign toaddr = inst[1] ? (inst[0] ? P : R) : A;
   assign incaddr = 
       { addr[l-1:1] + (incby | addr[0]), 
                      ~(incby | addr[0]) };
   assign tos2r = inst == 5'b11100;
   assign toR = state[2] ? incaddr : 
                (tos2r ? T : { P[15:1], c });
   reg dpush, rpush;

   always @(clk or state or inst or rd)
     begin
        dpush <= 1'b0;
        rpush <= 1'b0;
        if(state[2]) begin
           dpush <= |state[1:0] & rd;
           rpush <= state[1] & (inst[1:0]==2'b10);
        end else
           casez(inst)
             5'b00001: rpush <= 1'b1;
             5'b11100: rpush <= 1'b1;
             5'b11?1?: dpush <= 1'b1;
           endcase // case(inst)
     end
   wire [sdep-1:0] spdec, spinc;
   wire [rdep-1:0] rpdec, rpinc;

   stack #(sdep,l) dstack(clk, sp, spdec, 
                          dpush, N, toN);
   stack #(rdep,l) rstack(clk, rp, rpdec, 
                          rpush, toR, R);

   assign spdec = sp-{{(sdep-1){1'b0}}, 1'b1};
   assign spinc = sp+{{(sdep-1){1'b0}}, 1'b1};
   assign rpdec = rp+{(rdep){(~state[2] | tos2r)}};
   assign rpinc = rp+{{(rdep-1){1'b0}}, 1'b1};
   reg [2:0] nextstate;

   always @(inst or state)
      if(state[2]) begin 
         nextstate <= state[1:0] + { 2'b0, |state[1:0] }; 
      end else begin 
         casez(inst) 
            5'b00000: nextstate <= state[1:0] + 3'b001; 
            5'b00???: nextstate <= 3'b100; 
            5'b10???: nextstate <= { 1'b1, state[1:0] }; 
            5'b?????: nextstate <= state[1:0] + 3'b001; 
         endcase // casez(inst[0:2]) 
      end // else: !if(state[2]) end

   always @(posedge clk or negedge reset)
      if(!reset) begin
         state <= 3'b011;
         incby <= 1'b0;
         P <= 16'h0000;
         addr <= 16'h0000;
         A <= 16'h0000;
         T <= 16'h0000;
         N <= 16'h0000;
         I <= 16'h0000;
         c <= 1'b0;
         rd <= 1'b0;
         wr <= 2'b00;
         sp <= 0;
         rp <= 0;
         intack <= 0;
      end else if(state[2]) begin
         if(show) begin
            $write("%b[%b] T=%b%x:%x[%x], ",
                   inst, state, c, T, N, sp);
            $write("P=%x, I=%x, A=%x, R=%x[%x], res=%b%x\n",
                   P, I, A, R, rp, carry, res);
         end
         state <= nextstate;
         casez({ state[1:0], inst[1:0] }) 
            4'b00??: P <= !intreq ? incaddr : addr; 
            4'b1?0?: A <= incaddr; 
         // 4'b1?10: R <= incaddr; 
            4'b??11: P <= incaddr; 
         endcase // casez({ state[1:0], inst[1:0] }) 
         rd <= 1'b0; 
         wr <= 2'b0; 
         if(|state[1:0]) begin 
            if(rd) 
               if(incby) 
                  { sp, T, N } <= { spdec, data, T }; 
               else 
                  { sp, T, N } <= { spdec, 8'h00,
                    addr[0] ? data[7:0] : data[l-1:8], T }; 
            if(|wr) 
               `DROP; 
            incby <= 1'b1; 
         end else begin 
            intack <= intreq;
            if(intreq)
              I <= { 8'h81, intvec }; // call $200+intvec*2
            else
              I <= data; 
            if(!intreq & !data[15]) state[1:0] <= 2'b01;
         end 
         if(nextstate == 3'b100) begin 
             { addr, rd } <= { &inst[1:0] ? 
                               incaddr : P, 1'b1 }; 
         end // if (nextstate == 3'b100)
      end else begin // if (state[2])
         if(show) begin
            $write("%b[%b] T=%b%x:%x[%x], ",
                   inst, state, c, T, N, sp);
            $write("P=%x, I=%x, A=%x, R=%x[%x], res=%b%x\n",
                   P, I, A, R, rp, carry, res);
         end
         if(nextstate == 3'b100)
            { addr, rd } <= { P, 1'b1 };
         state <= nextstate;
         incby <= (inst[4:2] != 3'b101);
         casez(inst)
            5'b00001: begin
               rp <= rpdec;
               addr <= jmp;
               c <= 1'b0;
               if(state == 3'b011) `DROP;
            end // case: 5'b00001
            5'b00010: begin
               addr <= jmp;
               if(state == 3'b011) `DROP;
            end
            5'b00011: begin
               { c, addr } <= { R[0], R[l-1:1], 1'b0 };
               rp <= rpinc;
            end // case: 5'b01111
            5'b001??: begin
               if((inst[1] ? c : zero) ^ inst[0]) 
                  addr <= jmp;
               if(state == 3'b011) `DROP;
            end
            5'b01001: { c, T } <= { 1'b1, ~T };
            5'b01110: { T, A, c } <= 
               { c ? { carry, res } : { 1'b0, T }, A };
            5'b01111: { c, T, A } <= 
               { (c | carry) ? res : T, A, (c | carry) };
            5'b01???: begin
               c <= carry;
               { sp, T, N } <= { spinc, res, toN };
            end // case: 5'b01???
            5'b10000: begin
               addr <= toaddr;
               wr <= 2'b11;
            end
            5'b10100: begin
               addr <= toaddr;
               wr <= { ~toaddr[0], toaddr[0] };
               T <= { T[7:0], T[7:0] };
            end
            5'b10???: begin 
               addr <= toaddr;
               rd <= 1'b1;
            end
            5'b11000: { sp, N } <= { spinc, toN };
            5'b11001: `DROP;
            5'b11010: { sp, T, N } <= { spdec, N, T };
            5'b11011: { sp, N } <= { spdec, T };
            5'b11100: begin
               rp <= rpdec; `DROP;
            end // case: 5'b11100
            5'b11101: begin
               A <= T; `DROP;
            end // case: 5'b11101
            5'b11110: begin
               { sp, T, N } <= { spdec, R, T };
               rp <= rpinc;
            end // case: 5'b11110
            5'b11111:  { sp, T, N } <= { spdec, A, T };
         endcase // case(inst)
      end // else: !if(reset)

endmodule // cpu
