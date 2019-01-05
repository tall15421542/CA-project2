module cache(
    clk,
    proc_start,
    proc_read,
    proc_write,
    proc_addr,
    proc_rdata,
    proc_wdata,
    proc_stall,
    mem_read,
    mem_write,
    mem_addr,
    mem_rdata,
    mem_wdata,
    mem_ready
);
//==== input/output definition ============================
    input          clk;
    // processor interface
    
    input          proc_start;
    input          proc_read, proc_write;
    input   [31:0] proc_addr;
    input   [31:0] proc_wdata;
    output         proc_stall;
    output  [31:0] proc_rdata;
    // memory interface
    input  [255:0] mem_rdata;
    input          mem_ready;
    output         mem_read, mem_write;
    output  [26:0] mem_addr;
    output [255:0] mem_wdata;

    wire dirty, hit, addrToMem;
    wire [1:0] state;
    wire [26:0] cache_addr;

    assign mem_addr = (addrToMem) ? proc_addr[31:5] : cache_addr;
    
    CACONTROL control(
        .clk(clk),
        .proc_start(proc_start),
        .proc_read(proc_read),
        .proc_write(proc_write),
        .proc_stall(proc_stall),
        .hit(hit),
        .dirty(dirty),
        .state(state),
        .addrToMem(addrToMem),
        .mem_read(mem_read),
        .mem_write(mem_write),
        .mem_ready(mem_ready)
    );
    RF rf(
        .clk(clk),
        .proc_start(proc_start),
        .proc_write(proc_write),
        .proc_addr(proc_addr),
        .proc_wdata(proc_wdata),
        .proc_rdata(proc_rdata),
        .state(state),
        .hit(hit),
        .dirty(dirty),
        .cache_addr(cache_addr),
        .mem_wdata(mem_wdata),
        .mem_rdata(mem_rdata),
        .mem_ready(mem_ready)
    );

endmodule

module CACONTROL(
    clk,
    proc_start,
    proc_read,
    proc_write,
    proc_stall,
    hit,
    dirty,
    state,
    addrToMem,
    mem_read,
    mem_write,
    mem_ready
);
    input  proc_start, proc_read, proc_write;
    input  clk, mem_ready, hit, dirty;
    output proc_stall, mem_read, mem_write, addrToMem;
    output reg [1:0] state;

    parameter Compare    = 2'b01;
    parameter WriteBack  = 2'b10;
    parameter Allocate   = 2'b11;
    reg [1:0] next_state;

    assign mem_write = (state == WriteBack) | (proc_write & ((state == Compare) & ~hit & dirty));
    assign mem_read  =  (state == Allocate) | (proc_read & ((state == Compare) & ~hit & ~dirty));
    assign proc_stall = (state != Compare) | ((proc_read | proc_write) & ~hit);
    assign addrToMem = state[0] & ~((state == Compare) & ~hit & dirty);

    always@(*) begin
    case(state)
        Compare:   next_state = ((proc_read | proc_write) & ~hit) ? (dirty ? WriteBack : Allocate) : Compare;
        WriteBack: next_state = mem_ready ? Allocate : WriteBack;
        Allocate:  next_state = mem_ready ? Compare : Allocate;
        default:   next_state = state;
    endcase
    end

    always @(posedge clk) begin 
        if(proc_start) begin     
            state <= next_state;
        end 
    end

endmodule
module RF(
    clk,
    proc_start,
    proc_write,
    proc_addr,
    proc_wdata,
    proc_rdata,
    state,
    hit,
    dirty,
    cache_addr,
    mem_wdata,
    mem_rdata,
    mem_ready
);
    input  clk, proc_start, proc_write, mem_ready;
    input  [1:0] state;
    input  [31:0] proc_addr;
    input  [31:0] proc_wdata;
    input  [255:0] mem_rdata;
    output reg hit, dirty;
    output reg [31:0] proc_rdata;
    output reg [255:0] mem_wdata;
    output reg [26:0] cache_addr;

    reg [255:0] registers [0:31];
    reg [21:0]  reg_tags [0:31];
    reg [0:31]  reg_valid;
    reg [0:31]  reg_dirty;

    reg [255:0] registers_next [0:31];
    reg [21:0]  reg_tags_next [0:31];
    reg [0:31]  reg_valid_next;
    reg [0:31]  reg_dirty_next;

    wire [21:0] tag;
    wire [4:0]  index;
    wire [4:0]  block_offset;
    integer i, j;
    assign {tag, index, block_offset} = proc_addr;
    always @(*) begin
        hit = (reg_valid[index] & (reg_tags[index] == tag));
        dirty = reg_dirty[index];
        mem_wdata = registers[index];
        cache_addr = {reg_tags[index], index};
        for(i=0; i<256; i=i+4) begin 
            if(i == block_offset) begin 
                proc_rdata = registers[index][i << 3 +: 31];
            end
        end 
    end

    integer k, t;
    always @(*) begin
        for (k=0; k<32; k=k+1) begin
            if (k == index) begin
                if (state == 2'b01 & proc_write & hit) begin
                    for(t=0; t<256; t=t+4) begin 
                        if(t == block_offset) begin 
                            registers_next[k][t << 3 +: 31] = proc_wdata;
                        end
                    end
                    reg_tags_next[k] = reg_tags[k];
                    reg_valid_next[k] = reg_valid[k];
                    reg_dirty_next[k] = 1'b1;
                end
                else if (state == 2'b11 & mem_ready) begin
                    registers_next[k] = mem_rdata;
                    reg_tags_next[k] = tag;
                    reg_valid_next[k] = 1'b1;
                    reg_dirty_next[k] = 1'b0;

                end
                else begin
                    registers_next[k] = registers[k];
                    reg_tags_next[k] = reg_tags[k];
                    reg_valid_next[k] = reg_valid[k];
                    reg_dirty_next[k] = reg_dirty[k];
                end

                
            end
            else begin
                registers_next[k] = registers[k];
                reg_tags_next[k] = reg_tags[k];
                reg_valid_next[k] = reg_valid[k];
                reg_dirty_next[k] = reg_dirty[k];
            end
        end
    end
    always @(posedge clk) begin
        if (proc_start) begin
            for (i=0; i<32; i=i+1) begin
                registers[i] <= registers_next[i];
                reg_tags[i] <= reg_tags_next[i];
                reg_valid[i] <= reg_valid_next[i];
                reg_dirty[i] <= reg_dirty_next[i];
            end  
        end
    end
endmodule
