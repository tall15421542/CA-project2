module Instruction_Memory(
	clk_i,
	start_i,
	IDATA_ren,
	IDATA_addr,
	IDATA_rdata
);
	input 		  clk_i, start_i;
	input         IDATA_ren;
	input  [7:0]  IDATA_addr;
	output [31:0] IDATA_rdata;

	reg [31:0] memory [0:255];
	assign IDATA_rdata = memory[IDATA_addr>>2];

endmodule


module Data_Memory(
	clk_i,
	start_i,
	DDATA_ren,
	DDATA_wen,
	DDATA_addr,
	DDATA_rdata,
	DDATA_wdata,
    DDATA_ready
);
	input          clk_i, start_i;
	input          DDATA_ren, DDATA_wen;
	input  [26:0]  DDATA_addr;
	input  [255:0] DDATA_wdata;
    output         DDATA_ready;
	output [255:0] DDATA_rdata;
   
    reg [1:0]   state;
    reg [3:0]   count;
    reg [255:0] data;
	reg [255:0] memory [0:511];
    
    wire ready;
    parameter MEM_IDLE = 1'b0,
              MEM_WAIT = 1'b1;

    assign DDATA_rdata = data;
    assign DDATA_ready = ready;
    assign ready = 1'b1;
/*
    assign ready = (state == MEM_WAIT) & (count == 4'd9);

    // Control
    always @(posedge clk_i) begin 
        if(start_i) begin 
            case(state)
                MEM_IDLE: begin 
                    if(DDATA_ren | DDATA_wen) begin 
                        state <= MEM_WAIT;
                    end
                    else begin 
                        state <= state;
                    end 
                end 
                MEM_WAIT: begin 
                    if(count == 4'd9) begin 
                        count <= 4'd0;
                        state <= MEM_IDLE;
                    end
                    else begin 
                        state <= state;
                        count <= count + 1;
                    end 
                end
                default: begin 
                    state <= MEM_IDLE;
                    count <= 4'd0;
                end 
            endcase 
        end
    end
*/ 
    // read data 
    always @(*) begin 
        if(start_i &  DDATA_ren) begin 
            data <= memory[DDATA_addr];
        end
    end 
	
    // write data 
    always @(*) begin
		if (start_i & DDATA_wen) begin
			memory[DDATA_addr] <= DDATA_wdata;
		end
	end

endmodule
