

import "DPI-C" function void instrumentationunit_init(
    input longint unsigned   num_ingresses,
    input longint unsigned   num_egresses
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
);

module BlackBoxIngressUnit #(
    NUM_INGRESSES = 1,
    NUM_EGRESSES = 1,
    INGRESS_ID = 0,

    CYCLE_COUNT_BITS = 64,
    EGRESS_BITS = 64,
    PAYLOAD_BITS = 64
) (
    input                         clock,
    input                         reset,

    input [CYCLE_COUNT_BITS-1:0]  cycle_count,
    input                         noc_ready,

    output                        flit_out_valid,
    output                        flit_head,
    output                        flit_tail,
    output [EGRESS_BITS-1:0]      flit_egress_id,
    output [PAYLOAD_BITS-1:0]     flit_payload
);

    /* for each output:
        - create a C-type variable in here (Ex: longint unsigned _varname)
        - create a reg to take in the value (Ex: reg [BITS-1:0] varname_reg)
        - always @(posedge clk)

    */
    bit                _flit_out_valid;
    bit                _flit_head;
    bit                _flit_tail;
    longint unsigned   _flit_egress_id;
    longint unsigned   _flit_payload;

    reg                       _flit_out_valid_reg;
    reg                       _flit_head_reg;
    reg                       _flit_tail_reg;
    reg [EGRESS_BITS-1:0]     _flit_egress_id_reg;
    reg [PAYLOAD_BITS-1:0]    _flit_payload_reg;

    initial begin
        if (INGRESS_ID == 0) begin
            instrumentationunit_init(NUM_INGRESSES, NUM_EGRESSES);
        end
    end

    always @(posedge clock) begin
        if (reset) begin
            _flit_out_valid_reg <= 1'b0;
        end else begin
            ingressunit_tick(
                INGRESS_ID,
                cycle_count,
                noc_ready,
                _flit_out_valid,
                _flit_head,
                _flit_tail,
                _flit_egress_id,
                _flit_payload
            );

            /* verilator lint_off WIDTH */
            _flit_out_valid_reg <= _flit_out_valid;
            _flit_head_reg <= _flit_head;
            _flit_tail_reg <= _flit_tail;
            _flit_egress_id_reg <= _flit_egress_id;
            _flit_payload_reg <= _flit_payload;
            /* verilator lint_on WIDTH */
        end
    end

    assign flit_out_valid = _flit_out_valid_reg;
    assign flit_head = _flit_head_reg;
    assign flit_tail = _flit_tail_reg;
    assign flit_egress_id = _flit_egress_id_reg;
    assign flit_payload = _flit_payload_reg;

endmodule