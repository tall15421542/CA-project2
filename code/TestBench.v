`define CYCLE_TIME 50            

module TestBench;

reg                flag;
reg                Clk;
reg                Start;
reg [26:0]         address;
reg [21:0]         tag;
reg [4:0]          index;
integer            i, outfile, outfile2, counter;


wire         MEM_ren;
wire         MEM_wen;
wire         MEM_ready;
wire [26:0]  MEM_addr;
wire [255:0] MEM_rdata;
wire [255:0] MEM_wdata;

always #(`CYCLE_TIME/2) Clk = ~Clk;    

CPU CPU  (
	.clk_i(Clk),
	.start_i(Start),

    .MEM_ren(MEM_ren),
    .MEM_wen(MEM_wen),
    .MEM_ready(MEM_ready),
    .MEM_addr(MEM_addr),
    .MEM_rdata(MEM_rdata),
    .MEM_wdata(MEM_wdata)

);


Data_Memory Data_Memory(
    .clk_i(Clk),
    .start_i(Start),

    .DDATA_ren(MEM_ren),
    .DDATA_wen(MEM_wen),
    .DDATA_addr(MEM_addr),
    .DDATA_rdata(MEM_rdata),
    .DDATA_wdata(MEM_wdata),
    .DDATA_ready(MEM_ready)
);

  
initial begin
    counter = 1;
     
    // initialize instruction memory
    for(i=0; i<256; i=i+1) begin
        CPU.Instruction_Memory.memory[i] = 32'b0;
    end
    
    // initialize data memory
    for(i=0; i<512; i=i+1) begin
        CPU.Data_Memory.memory[i] = 256'b0;
    end    
        
    // initialize Register File
    for(i=0; i<32; i=i+1) begin
        CPU.i_RISC.rf.registers[i] = 32'b0;
    end
    
    // initialize cache meomery

    for(i=0; i<32; i=i+1) begin 
        /* rf part */
        CPU.cache.rf.registers[i] = 256'b0;
        CPU.cache.rf.reg_tags[i] = 22'b0;
        CPU.cache.rf.reg_valid[i] = 1'b0;
        CPU.cache.rf.reg_dirty[i] = 1'b0;
    end
    for(i=0; i<512; i=i+1) begin 
        Data_Memory.memory[i] = 256'b0;
    end
    /* control part */ 
    CPU.cache.control.state = 2'b01;

    // initialize PC & pipeline registers
    CPU.i_RISC.pc.PCnext = 8'b0;
    CPU.i_RISC.r0.PCnext_ID = 8'b0;
    CPU.i_RISC.r0.IDATA_rdata_ID = 32'b0;
    CPU.i_RISC.r1.EX_signals = 113'b0;
    CPU.i_RISC.r2.MEM_signals = 73'b0;
    CPU.i_RISC.r3.WB_signals = 71'b0;

    $dumpfile("Pipeline.vcd");
    $dumpvars;
    
    // Load instructions into instruction memory
    $readmemb("instruction.txt", CPU.Instruction_Memory.memory);
    
    // Open output file
    outfile = $fopen("output.txt") | 1;
    outfile2 = $fopen("cache.txt") | 1;
   
    // Set Input n into data memory at 0x00
    Data_Memory.memory[0] = 256'h5;       // n = 5 for example
    
    Clk = 0;
    Start = 0;
    
    #(`CYCLE_TIME/4);
    Start = 1;
end
  
always@(posedge Clk) begin
	if(counter == 150) begin	// store cache to memory
		$fdisplay(outfile, "Flush Cache! \n");
		for(i=0; i<32; i=i+1) begin
			tag = CPU.cache.rf.reg_tags[i];
			index = i;
			address = {tag, index};
			CPU.Data_Memory.memory[address] = CPU.cache.rf.registers[i];
		end 
    end 

	if(counter > 150) begin	// stop 
		$finish;
	end
		
	$fdisplay(outfile, "cycle = %d, Start = %b", counter, Start);
	// print PC 
	$fdisplay(outfile, "PC = %d", CPU.i_RISC.pc.PCnext);
	
    // print Registers
    $fdisplay(outfile, "Registers");
    $fdisplay(outfile, "R0(r0) = %h, R8 (t0) = %h, R16(s0) = %h, R24(t8) = %h", CPU.i_RISC.rf.registers[0], CPU.i_RISC.rf.registers[8] , CPU.i_RISC.rf.registers[16], CPU.i_RISC.rf.registers[24]);
    $fdisplay(outfile, "R1(at) = %h, R9 (t1) = %h, R17(s1) = %h, R25(t9) = %h", CPU.i_RISC.rf.registers[1], CPU.i_RISC.rf.registers[9] , CPU.i_RISC.rf.registers[17], CPU.i_RISC.rf.registers[25]);
    $fdisplay(outfile, "R2(v0) = %h, R10(t2) = %h, R18(s2) = %h, R26(k0) = %h", CPU.i_RISC.rf.registers[2], CPU.i_RISC.rf.registers[10], CPU.i_RISC.rf.registers[18], CPU.i_RISC.rf.registers[26]);
    $fdisplay(outfile, "R3(v1) = %h, R11(t3) = %h, R19(s3) = %h, R27(k1) = %h", CPU.i_RISC.rf.registers[3], CPU.i_RISC.rf.registers[11], CPU.i_RISC.rf.registers[19], CPU.i_RISC.rf.registers[27]);
    $fdisplay(outfile, "R4(a0) = %h, R12(t4) = %h, R20(s4) = %h, R28(gp) = %h", CPU.i_RISC.rf.registers[4], CPU.i_RISC.rf.registers[12], CPU.i_RISC.rf.registers[20], CPU.i_RISC.rf.registers[28]);
    $fdisplay(outfile, "R5(a1) = %h, R13(t5) = %h, R21(s5) = %h, R29(sp) = %h", CPU.i_RISC.rf.registers[5], CPU.i_RISC.rf.registers[13], CPU.i_RISC.rf.registers[21], CPU.i_RISC.rf.registers[29]);
    $fdisplay(outfile, "R6(a2) = %h, R14(t6) = %h, R22(s6) = %h, R30(s8) = %h", CPU.i_RISC.rf.registers[6], CPU.i_RISC.rf.registers[14], CPU.i_RISC.rf.registers[22], CPU.i_RISC.rf.registers[30]);
    $fdisplay(outfile, "R7(a3) = %h, R15(t7) = %h, R23(s7) = %h, R31(ra) = %h", CPU.i_RISC.rf.registers[7], CPU.i_RISC.rf.registers[15], CPU.i_RISC.rf.registers[23], CPU.i_RISC.rf.registers[31]);
    
    // print meomery
	$fdisplay(outfile, "Data Memory: 0x0000 = %h", Data_Memory.memory[0]);
	$fdisplay(outfile, "Data Memory: 0x0020 = %h", Data_Memory.memory[1]);
	$fdisplay(outfile, "Data Memory: 0x0040 = %h", Data_Memory.memory[2]);
	$fdisplay(outfile, "Data Memory: 0x0060 = %h", Data_Memory.memory[3]);
	$fdisplay(outfile, "Data Memory: 0x0080 = %h", Data_Memory.memory[4]);
	$fdisplay(outfile, "Data Memory: 0x00A0 = %h", Data_Memory.memory[5]);
	$fdisplay(outfile, "Data Memory: 0x00C0 = %h", Data_Memory.memory[6]);
	$fdisplay(outfile, "Data Memory: 0x00E0 = %h", Data_Memory.memory[7]);
	$fdisplay(outfile, "Data Memory: 0x0400 = %h", Data_Memory.memory[32]);

	// print Data Memory
	
	// print Data Cache Status
	if(CPU.cache.proc_stall && CPU.cache.state==2'b01) begin
		if(CPU.cache.rf.dirty) begin
			if(CPU.cache.proc_write) 
				$fdisplay(outfile2, "Cycle: %d, Write Miss, Address: %h, Write Data: %h (Write Back!)", counter, CPU.cache.proc_addr, CPU.cache.proc_wdata);
			else if(CPU.cache.proc_read) 
				$fdisplay(outfile2, "Cycle: %d, Read Miss , Address: %h, Read Data : %h (Write Back!)", counter, CPU.cache.proc_addr, CPU.cache.proc_rdata);
		end
		else begin
			if(CPU.cache.proc_write) 
				$fdisplay(outfile2, "Cycle: %d, Write Miss, Address: %h, Write Data: %h", counter, CPU.cache.proc_addr, CPU.cache.proc_wdata);
			else if(CPU.cache.proc_read) 
				$fdisplay(outfile2, "Cycle: %d, Read Miss , Address: %h, Read Data : %h", counter, CPU.cache.proc_addr, CPU.cache.proc_rdata);
		end
		flag = 1'b1;
	end
	else if(!CPU.cache.proc_stall) begin
		if(!flag) begin
			if(CPU.cache.proc_write) 
				$fdisplay(outfile2, "Cycle: %d, Write Hit , Address: %h, Write Data: %h", counter, CPU.cache.proc_addr, CPU.cache.proc_wdata);
			else if(CPU.cache.proc_read) 
				$fdisplay(outfile2, "Cycle: %d, Read Hit  , Address: %h, Read Data : %h", counter, CPU.cache.proc_addr, CPU.cache.proc_rdata);
		end
		flag = 1'b0;
	end
	counter = counter + 1;
end

  
endmodule
