import "DPI-C" function void ingress_tick
  (
   input longint  ingress_id,
   input string   config_str,
   input longint  current_cycle,
   input bit 	  flit_out_ready,
   output bit 	  flit_out_valid,
   output bit 	  flit_out_head,
   output bit 	  flit_out_tail,
   output longint flit_out_egress_id,
   output longint flit_out_unique_id
   );

import "DPI-C" function void egress_tick
  (
   input longint egress_id,
   input string  config_str,
   input longint current_cycle,
   output bit 	 flit_in_ready,
   input bit 	 flit_in_valid,
   input bit 	 flit_in_head,
   input bit 	 flit_in_tail,
   input longint flit_in_ingress_id,
   input longint flit_in_unique_id,
   output bit 	 done,
   output bit 	 success
   );


module TrafficEvalIngress #(parameter INGRESS_ID,
			    parameter CONFIG_STR) (
    input 	  clock,
    input 	  reset,
    input [63:0]  current_cycle,
    input 	  flit_out_ready,
    output 	  flit_out_valid,
    output 	  flit_out_head,
    output 	  flit_out_tail,
    output [63:0] flit_out_egress_id,
    output [63:0] flit_out_unique_id

    );

   wire [63:0] 	  __current_cycle;
   wire 	  __flit_out_ready;
   bit 		  __flit_out_valid;
   bit 		  __flit_out_head;
   bit 		  __flit_out_tail;
   longint 	  __flit_out_egress_id;
   longint 	  __flit_out_unique_id;

   reg 		  __flit_out_valid_reg;
   reg 		  __flit_out_head_reg;
   reg 		  __flit_out_tail_reg;
   reg [63:0] 	  __flit_out_egress_id_reg;
   reg [63:0] 	  __flit_out_unique_id_reg;

   always @(posedge clock) begin
      if (reset) begin
	 __flit_out_valid = 1'b0;
	 __flit_out_valid_reg <= 1'b0;
      end else begin
	 ingress_tick(INGRESS_ID,
		      CONFIG_STR,
		      __current_cycle,
		      __flit_out_ready,
		      __flit_out_valid,
		      __flit_out_head,
		      __flit_out_tail,
		      __flit_out_egress_id,
		      __flit_out_unique_id);
	 __flit_out_valid_reg <= __flit_out_valid;
	 __flit_out_head_reg  <= __flit_out_head;
	 __flit_out_tail_reg  <= __flit_out_tail;
	 __flit_out_egress_id_reg <= __flit_out_egress_id;
	 __flit_out_unique_id_reg <= __flit_out_unique_id;
      end // else: !if(reset)
   end // always @ (posedge clock)

   assign __current_cycle = current_cycle;
   assign __flit_out_ready = flit_out_ready;
   assign flit_out_valid = __flit_out_valid_reg;
   assign flit_out_head  = __flit_out_head_reg;
   assign flit_out_tail  = __flit_out_tail_reg;
   assign flit_out_egress_id = __flit_out_egress_id_reg;
   assign flit_out_unique_id = __flit_out_unique_id_reg;
endmodule



module TrafficEvalEgress #(parameter EGRESS_ID,
			   parameter CONFIG_STR) (
    input 	 clock,
    input 	 reset,
    input [63:0] current_cycle,
    output 	 flit_in_ready,
    input 	 flit_in_valid,
    input 	 flit_in_head,
    input 	 flit_in_tail,
    input [63:0] flit_in_ingress_id,
    input [63:0] flit_in_unique_id,
    output 	 success,
    output 	 fatal
    );

   wire [63:0] 	  __current_cycle;

   bit 		  __flit_in_ready;
   wire 	  __flit_in_valid;
   wire		  __flit_in_head;
   wire		  __flit_in_tail;
   wire [63:0] 	  __flit_in_ingress_id;
   wire [63:0]	  __flit_in_unique_id;
   bit 		  __success;
   bit 		  __fatal;

   reg 		  __flit_in_ready_reg;
   reg 		  __success_reg;
   reg 		  __fatal_reg;


   always @(posedge clock) begin
      if (reset) begin
	 __flit_in_ready = 1'b0;
	 __flit_in_ready_reg <= 1'b0;
	 __success = 1'b0;
	 __success_reg <= 1'b0;
	 __fatal = 1'b0;
	 __fatal_reg <= 1'b0;

      end else begin
	 egress_tick(EGRESS_ID,
		     CONFIG_STR,
		     __current_cycle,
		     __flit_in_ready,
		     __flit_in_valid,
		     __flit_in_head,
		     __flit_in_tail,
		     __flit_in_ingress_id,
		     __flit_in_unique_id,
		     __success,
		     __fatal
		     );
	 __flit_in_ready_reg <= __flit_in_ready;
	 __success_reg <= __success;
	 __fatal_reg <= __fatal;

      end // else: !if(reset)
   end // always @ (posedge clock)

   assign flit_in_ready = __flit_in_ready_reg;
   assign success = __success_reg;
   assign fatal = __fatal_reg;
   assign __current_cycle = current_cycle;
   assign __flit_in_valid = flit_in_valid;
   assign __flit_in_head = flit_in_head;
   assign __flit_in_tail = flit_in_tail;
   assign __flit_in_ingress_id = flit_in_ingress_id;
   assign __flit_in_unique_id = flit_in_unique_id;
endmodule
