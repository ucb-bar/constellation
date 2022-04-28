import "DPI-C" function void egressunit_tick
(
    input longint unsigned egress_id,
    input longint unsigned cycle_count,
    input bit              noc_valid,
    input bit              flit_in_head,
    input bit              flit_in_tail,
    input longint unsigned flit_in_ingress_id,
    input longint unsigned flit_in_payload,

    output bit             egressunit_ready,
    output bit             success
);

module EgressUnit #(
    EGRESS_ID = 0,

    INGRESS_BITS = 64,
    CYCLE_COUNT_BITS = 64,
    PAYLOAD_BITS = 64
) (
    input                           clock,
    input                           reset,

    input [CYCLE_COUNT_BITS-1:0]    cycle_count,
    input                           noc_valid,
    input                           flit_in_head,
    input                           flit_in_tail,
    input [INGRESS_BITS-1:0]        flit_in_ingress_id,
    input [PAYLOAD_BITS-1:0]        flit_in_payload,

    output                          egressunit_ready,
    output                          success
);
    bit _egressunit_ready;
    bit _success;

    reg _egressunit_ready_reg;
    reg _success_reg;

    always @(posedge clk) begin
        if (reset) begin
            _egressunit_ready_reg <= 1'b0;
            _success_reg <= 1'b0;
        end else begin
            egressunit_tick(
                EGRESS_ID,
                cycle_count,
                noc_valid,
                flit_in_head,
                flit_in_tail,
                flit_in_ingress_id,
                flit_in_payload,
                _egressunit_ready,
                _success
            );

            /* verilator lint_off WIDTH */
            _egressunit_ready_reg <= _egressunit_ready;
            _success_reg <= _success;
            /* verilator lint_on WIDTH */
        end
    end

    assign egressunit_ready = _egressunit_ready_reg;
    assign success = _success_reg;

endmodule


