

import "DPI-C" function void ingressunit_init(
    input longint unsigned      num_flits,
    input longint unsigned      num_ingresses,
    input longint unsigned      num_egresses
);

import "DPI-C" function void ingressunit_tick
(
    input longint unsigned      ingress_id,
    input longint unsigned      cycle_count,
    input bit                   noc_ready,

    output bit                  flit_out_valid,
    output bit                  flit_out_head,
    output bit                  flit_out_tail,
    output longint unsigned     flit_out_egress_id,
    output longint unsigned     flit_out_payload
    // see InputGen in TestHarness.scala for how IngressFlit are generated
    // probably easier to have verilog/C++ return IngressFlit bits and then
    // actually construct IngressFlit in Chisel using those values
);

module IngressUnit #(
    NUM_INGRESSES = 1,
    NUM_EGRESSES = 1,
    INGRESS_ID = 0,
    INJECTION_RATE = 100,
    NUM_FLITS = 1
) (
    input clock,
    input reset,
);
    initial begin
        if (INGRESS_ID == 0) begin
            // TODO (ANIMESH): call ingressunit_init here
        end
    end

    always @(posedge clk) begin
        if (reset) begin

        end else begin
            // TODO (ANIMESH): call ingressunit_tick here
        end

    end


endmodule