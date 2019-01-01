module CPU  (
	clk_i,
	start_i,
    MEM_ren,
    MEM_wen,
    MEM_ready,
    MEM_addr,
    MEM_rdata,
    MEM_wdata
);

	// start_i = 0 -> reset
	// start_i = 1 -> start
	input clk_i, start_i;
    input          MEM_ready;
    input [255:0]  MEM_rdata;
    output         MEM_ren;
    output         MEM_wen;
    output [26:0]  MEM_addr;
    output [255:0] MEM_wdata;

	// wire declaration
	wire        IDATA_ren;
	wire [31:0]  IDATA_addr;
	wire [31:0] IDATA_rdata;
    
    wire        proc_stall;
	wire        DDATA_ren;
	wire        DDATA_wen;
	wire [31:0] DDATA_addr;
	wire [31:0] DDATA_wdata;
	wire [31:0] DDATA_rdata;

	RISC_Pipeline i_RISC (
//----------control interface------
		clk_i, 
		start_i,
//----------I DATA interface-------		
		IDATA_ren,
		IDATA_addr,
		IDATA_rdata,
//----------D DATA interface-------
		DDATA_ren,
		DDATA_wen,
		DDATA_addr,
		DDATA_wdata,
		DDATA_rdata,
//----------CACHE STALL------------
        proc_stall
	);

	Instruction_Memory Instruction_Memory(
		clk_i,
		start_i,
		IDATA_ren,
		IDATA_addr,
		IDATA_rdata
	);
	
    cache cache(
        .clk(clk_i),
        .proc_start(start_i),
        .proc_read(DDATA_ren),
        .proc_write(DDATA_wen),
        .proc_addr(DDATA_addr),
        .proc_rdata(DDATA_rdata),
        .proc_wdata(DDATA_wdata),
        .proc_stall(proc_stall),
        .mem_read(MEM_ren),
        .mem_write(MEM_wen),
        .mem_addr(MEM_addr),
        .mem_rdata(MEM_rdata),
        .mem_wdata(MEM_wdata),
        .mem_ready(MEM_ready)
    );



endmodule

module RISC_Pipeline(
		clk_i, 
		start_i,
		IDATA_ren,
		IDATA_addr,
		IDATA_rdata,
		DDATA_ren,
		DDATA_wen,
		DDATA_addr,
		DDATA_wdata,
		DDATA_rdata,
        proc_stall
);

	input         clk_i, start_i;
//----------I DATA interface-------		
	input  [31:0] IDATA_rdata;
	output        IDATA_ren;
	output [31:0]  IDATA_addr;
//----------D DATA interface-------
	input  [31:0] DDATA_rdata;
	output        DDATA_ren, DDATA_wen;
	output [31:0] DDATA_addr;
	output [31:0] DDATA_wdata;
//----------Cache STALL------------
    input proc_stall;

	// global wires
	wire isFlush, isSTALL;

	// IF wires
	wire [31:0] PCnext, PCadder1, PCin;

	// ID wires
	wire Branch_true;
	wire MemWrite, MemRead, RegWrite, Branch, MemToReg;
	wire [1:0] ALUSrc;
	wire [1:0] isFWD_R1_ID, isFWD_R2_ID;
	wire [2:0] funct3;
	wire [2:0] ALUctrl;
	wire [4:0] Reg_R1, Reg_R2, Reg_W; // Reg_R1 -> rs, Reg_R2 -> rt, Reg_W -> rd
	wire [6:0] opcode;
	wire [6:0] funct7;
	wire [7:0] PCnext_ID, PCadder2;
	wire [11:0] Imm_I, Imm_S, Imm_SB;
	wire [31:0] IDATA_rdata_ID;
	wire [31:0] ReadData1_tmp, ReadData2_tmp;
	reg  [31:0] ReadData1, ReadData2;

	// EX wires
	wire RegWrite_EX, MemRead_EX, MemWrite_EX, MemToReg_EX;
	wire [1:0] ALUSrc_EX;
	wire [1:0] isFWD_R1_EX, isFWD_R2_EX;
	wire [2:0] ALUctrl_EX;
	wire [4:0] Reg_R1_EX, Reg_R2_EX, Reg_W_EX;
	wire [11:0] Imm_I_EX, Imm_S_EX;
	wire [31:0] ReadData1_EX, ReadData2_EX;
	wire [31:0] ALUresult;
	reg  [31:0] ALUin1, ALUin2, ALUin2_reg;

	// MEM wires
	wire RegWrite_MEM, MemWrite_MEM, MemRead_MEM, MemToReg_MEM;
	wire [4:0] Reg_W_MEM;
	wire [31:0] DDATA_rdata, ALUresult_MEM, ReadData2_MEM;

	// WB wires
	wire RegWrite_WB, MemToReg_WB;
	wire [4:0] Reg_W_WB;
	wire [31:0] DDATA_rdata_WB, ALUresult_WB, WriteData;

	// output
	assign IDATA_ren = 1'b1;
	assign IDATA_addr = PCnext;
	assign DDATA_ren = MemRead_MEM;
	assign DDATA_wen = MemWrite_MEM;
	assign DDATA_addr = ALUresult_MEM;
	assign DDATA_wdata = ReadData2_MEM;

	// Global logics
	assign isFlush = Branch_true;
	STALL_UNIT stall_unit(
		.Branch(Branch),
		.Reg_R1(Reg_R1),
		.Reg_R2(Reg_R2),
		.RegWrite_EX(RegWrite_EX),
		.MemRead_EX(MemRead_EX),
		.Reg_W_EX(Reg_W_EX),
		.MemRead_MEM(MemRead_MEM),
		.Reg_W_MEM(Reg_W_MEM),
		.isSTALL(isSTALL)
	);

	// IF logics
	assign PCadder1 = PCnext + 8'd4;
	PC pc(clk_i, start_i,isSTALL, proc_stall, PCin, PCnext);
	Reg_IF_ID r0(
		.clk_i(clk_i),
		.start_i(start_i),
		.isSTALL(isSTALL),
		.isFlush(isFlush),
		.IDATA_rdata(IDATA_rdata),
		.PCnext(PCnext),
		.IDATA_rdata_ID(IDATA_rdata_ID),
		.PCnext_ID(PCnext_ID),
        .proc_stall(proc_stall)
	);

	// ID logics
	assign {funct7, Reg_R2, Reg_R1, funct3, Reg_W, opcode} = IDATA_rdata_ID;
	assign Imm_I = IDATA_rdata_ID[31:20]; // arithmetic with immediate
	assign Imm_S = {IDATA_rdata_ID[31:25], IDATA_rdata_ID[11:7]}; // store operation with immediate
	assign Imm_SB = {IDATA_rdata_ID[31], IDATA_rdata_ID[7], IDATA_rdata_ID[30:25], IDATA_rdata_ID[11:8]};
	assign PCadder2 = PCnext_ID + {Imm_SB, 1'b0};
	assign Branch_true = (ReadData1 == ReadData2) & Branch;
	assign PCin = Branch_true ? PCadder2 : PCadder1;
	always@(*) begin
		case (isFWD_R1_ID)
			2'd0: ReadData1 = ReadData1_tmp;
			2'd1: ReadData1 = ALUresult_MEM;
			2'd2: ReadData1 = WriteData;
			default: ReadData1 = ReadData1_tmp;
		endcase
		case (isFWD_R2_ID)
			2'd0: ReadData2 = ReadData2_tmp;
			2'd1: ReadData2 = ALUresult_MEM;
			2'd2: ReadData2 = WriteData;
			default: ReadData2 = ReadData2_tmp;
		endcase
	end
	CONTROL control(
		.opcode(opcode),
		.funct7(funct7),
		.funct3(funct3),
		.ctrl_signals({MemWrite, MemRead, RegWrite, Branch,
					   MemToReg, ALUSrc, ALUctrl})
	);
	Register_file rf(
		.clk_i(clk_i),
		.start_i(start_i),
		.Reg_R1(Reg_R1),
		.Reg_R2(Reg_R2),
		.Reg_W(Reg_W_WB),
		.RegWrite(RegWrite_WB),
		.WriteData(WriteData),
		.ReadData1(ReadData1_tmp),
		.ReadData2(ReadData2_tmp)
	);
	FWD_unit_ID fwd_unit_id(
		.Branch(Branch),
		.Reg_R1(Reg_R1),
		.Reg_R2(Reg_R2),
		.Reg_W_MEM(Reg_W_MEM),
		.Reg_W_WB(Reg_W_WB),
		.RegWrite_MEM(RegWrite_MEM),
		.RegWrite_WB(RegWrite_WB),
		.isFWD_R1_ID(isFWD_R1_ID),
		.isFWD_R2_ID(isFWD_R2_ID)
	);
	Reg_ID_EX r1(
		.clk_i(clk_i),
		.start_i(start_i),
		.isSTALL(isSTALL),
        .proc_stall(proc_stall),
		.ID_signals({RegWrite, MemRead, MemWrite,
					 ReadData1, ReadData2, 
					 Reg_R1, Reg_R2, Reg_W,
					 Imm_I, Imm_S, 
					 ALUctrl, MemToReg, ALUSrc
				   }),
		
		.EX_signals({RegWrite_EX, MemRead_EX, MemWrite_EX, 
					 ReadData1_EX, ReadData2_EX, 
					 Reg_R1_EX, Reg_R2_EX, Reg_W_EX,
					 Imm_I_EX, Imm_S_EX, 
					 ALUctrl_EX, MemToReg_EX, ALUSrc_EX
				   })
	);

	// EX logics
	always @(*) begin
		case(isFWD_R1_EX)
			2'b00:   ALUin1 = ReadData1_EX;
			2'b01:   ALUin1 = WriteData;
			2'b10:   ALUin1 = ALUresult_MEM;
			default: ALUin1 = ReadData1_EX;
		endcase
		case(isFWD_R2_EX)
			2'b00:   ALUin2_reg = ReadData2_EX;
			2'b01:   ALUin2_reg = WriteData;
			2'b10:   ALUin2_reg = ALUresult_MEM;
			default: ALUin2_reg = ReadData2_EX;
		endcase
		case(ALUSrc_EX)
			2'b00:   ALUin2 = ALUin2_reg;
			2'b01:   ALUin2 = {{20{Imm_I_EX[11]}}, Imm_I_EX};
			2'b10:   ALUin2 = {{20{Imm_S_EX[11]}}, Imm_S_EX};
			default: ALUin2 = ALUin2_reg;
		endcase
	end
	FWD_UNIT_EX fwd_unit_ex(
		.Reg_R1_EX(Reg_R1_EX),
		.Reg_R2_EX(Reg_R2_EX),
		.Reg_W_MEM(Reg_W_MEM),
		.Reg_W_WB(Reg_W_WB),
		.RegWrite_MEM(RegWrite_MEM),
		.RegWrite_WB(RegWrite_WB),
		.isFWD_R1_EX(isFWD_R1_EX),
		.isFWD_R2_EX(isFWD_R2_EX)
	);
	ALU alu(ALUin1, ALUin2, ALUctrl_EX, ALUresult);
	Reg_EX_MEM r2(
		.clk_i(clk_i),
		.start_i(start_i),
        .proc_stall(proc_stall),
		.EX_signals({RegWrite_EX, MemWrite_EX, MemRead_EX,
					 MemToReg_EX, Reg_W_EX, ALUresult, ALUin2_reg
				   }),

		.MEM_signals({RegWrite_MEM, MemWrite_MEM, MemRead_MEM,
			 		  MemToReg_MEM, Reg_W_MEM, ALUresult_MEM, ReadData2_MEM
					})
	);
	// MEM logics
	Reg_MEM_WB r3(
		.clk_i(clk_i),
		.start_i(start_i),
        .proc_stall(proc_stall),
		.MEM_signals({RegWrite_MEM, MemToReg_MEM, Reg_W_MEM,
		    		  DDATA_rdata, ALUresult_MEM
		    		}),
		.WB_signals({RegWrite_WB, MemToReg_WB, Reg_W_WB,
		    		 DDATA_rdata_WB, ALUresult_WB
		    	   })
	);

	// WB logics
	assign WriteData = MemToReg_WB ? DDATA_rdata_WB : ALUresult_WB;

endmodule

module STALL_UNIT(
	Branch,
	Reg_R1,
	Reg_R2,
	RegWrite_EX,
	MemRead_EX,
	Reg_W_EX,
	MemRead_MEM,
	Reg_W_MEM,
	isSTALL
);
	// Two situation need stall
	// 1. Load-use hazard
	// 2. Branch using the value at EX/MEM stage
	input Branch, MemRead_EX, RegWrite_EX, MemRead_MEM;
	input [4:0] Reg_W_EX, Reg_W_MEM, Reg_R1, Reg_R2;
	output reg isSTALL;

	always @(*) begin
		// situation 1
		if (MemRead_EX && ((Reg_W_EX == Reg_R1) || (Reg_W_EX == Reg_R2))) begin
			isSTALL = 1'b1;
		end
		// situation 2
		else if (Branch) begin
			if (MemRead_MEM && ((Reg_W_MEM == Reg_R1) || (Reg_W_MEM == Reg_R2))) begin
				isSTALL = 1'b1;
			end
			else if (RegWrite_EX && ((Reg_W_EX == Reg_R1) || (Reg_W_EX == Reg_R2))) begin
				isSTALL = 1'b1;
			end
			else isSTALL = 1'b0;
		end
		else begin
			isSTALL = 1'b0;
		end
	end
endmodule

module PC(clk_i, start_i, isSTALL, proc_stall, PCin, PCnext);
	input            clk_i, start_i, isSTALL, proc_stall;
	input      [31:0] PCin;
	output reg [31:0] PCnext;

	always @(posedge clk_i) begin
		if (start_i) begin 
			if (isSTALL | proc_stall)
				PCnext <= PCnext;
			else
				PCnext <= PCin;
		end
	end
endmodule

module Reg_IF_ID(
	clk_i,
	start_i,
	isSTALL,
	isFlush,
    proc_stall,
	IDATA_rdata,
	PCnext,
	IDATA_rdata_ID,
	PCnext_ID
);
	input clk_i, start_i, isSTALL, isFlush, proc_stall;
	input [31:0]  PCnext;
	input [31:0] IDATA_rdata;
	output reg [7:0] PCnext_ID;
	output reg [31:0] IDATA_rdata_ID;

	always @(posedge clk_i) begin
		if (start_i) begin
			if (isSTALL | proc_stall) begin
				IDATA_rdata_ID <= IDATA_rdata_ID;
				PCnext_ID <= PCnext_ID;
			end
			else if (isFlush) begin
				IDATA_rdata_ID <= 32'd0;
				PCnext_ID <= PCnext;
			end
            else begin
				IDATA_rdata_ID <= IDATA_rdata;
				PCnext_ID <= PCnext;
			end
		end
	end
endmodule

module FWD_unit_ID(
	Branch,
	Reg_R1,
	Reg_R2,
	Reg_W_MEM,
	Reg_W_WB,
	RegWrite_MEM,
	RegWrite_WB,
	isFWD_R1_ID,
	isFWD_R2_ID
);

	input Branch;
	input RegWrite_MEM, RegWrite_WB;
	input [4:0] Reg_R1, Reg_R2;
	input [4:0] Reg_W_MEM, Reg_W_WB;
	output reg [1:0] isFWD_R1_ID, isFWD_R2_ID;
	// isFWD_R1_ID : 0 -> ReadData1_tmp,
	//               1 -> ALUresult_MEM,
	//               2 -> WriteData
	always @(*) begin
		if (RegWrite_WB && (Reg_W_WB == Reg_R1)) isFWD_R1_ID = 2'd2;
		else if (Branch && RegWrite_MEM && (Reg_W_MEM == Reg_R1)) begin
			isFWD_R1_ID = 2'd1;
		end
		else isFWD_R1_ID = 2'd0;
	end

	// isFWD_R2_ID : 0 -> ReadData2_tmp,
	//               1 -> ALUresult_MEM,
	//               2 -> WriteData
	always @(*) begin
		if (RegWrite_WB && (Reg_W_WB == Reg_R2)) isFWD_R2_ID = 2'd2;
		else if (Branch && RegWrite_MEM && (Reg_W_MEM == Reg_R2)) begin
			isFWD_R2_ID = 2'd1;
		end
		else isFWD_R2_ID = 2'd0;
	end
endmodule

module CONTROL(
	opcode,
	funct7,
	funct3,
	ctrl_signals
);
	input  [6:0]  opcode, funct7;
	input  [2:0]  funct3;
	output [9:0] ctrl_signals;

	reg       MemWrite, MemRead, RegWrite, Branch, MemToReg;
	reg [1:0] ALUSrc;
	reg [2:0] ALUctrl;

	assign ctrl_signals = {MemWrite, MemRead, RegWrite, Branch, MemToReg, ALUSrc, ALUctrl};
	always @(*) begin
		MemWrite = (opcode == 7'b0100011);
		MemRead = (opcode == 7'b0000011);
		RegWrite = (opcode == 7'b0110011) || (opcode == 7'b0010011) || (opcode == 7'b0000011);
		Branch = (opcode == 7'b1100011);
		MemToReg = (opcode == 7'b0000011);
	end

	always @(*) begin
		if (~opcode[5]) ALUSrc = 2'd1;
		else if (opcode[4]) ALUSrc = 2'd0;
		else ALUSrc = 2'd2;
	end

	always @(*) begin
		// R-type
		if (opcode == 7'b0110011) begin
			case (funct3)
				3'b110:  ALUctrl = 3'b000;
				3'b111:  ALUctrl = 3'b001;
				3'b000:  begin
					case (funct7)
						7'b0000000: ALUctrl = 3'b010;
						7'b0100000: ALUctrl = 3'b011;
						7'b0000001: ALUctrl = 3'b100;
						default:    ALUctrl = 3'b010;
					endcase
				end
				default: ALUctrl = 3'b010;
			endcase
		end
		else ALUctrl = 3'b010;
	end
endmodule

module Register_file(
	clk_i,
	start_i,
	Reg_R1,
	Reg_R2,
	Reg_W,
	RegWrite,
	WriteData,
	ReadData1,
	ReadData2
);
	input clk_i, start_i, RegWrite;
	input [4:0] Reg_R1, Reg_R2, Reg_W;
	input [31:0] WriteData;
	output [31:0] ReadData1, ReadData2;

	reg [31:0] registers [0:31];
	assign ReadData1 = registers[Reg_R1];
	assign ReadData2 = registers[Reg_R2];
	always @(posedge clk_i) begin
		if (RegWrite && start_i)
			registers[Reg_W] <= WriteData;
	end
endmodule

module Reg_ID_EX(
	clk_i,
	start_i,
	isSTALL,
    proc_stall,
	ID_signals,
	EX_signals
);
	input              clk_i, start_i, isSTALL, proc_stall;
	input      [111:0] ID_signals;
	output reg [111:0] EX_signals;

	always @(posedge clk_i) begin
		if (start_i) begin
			if (isSTALL)  // insert nop to EX stage
				EX_signals <= 112'b0;
            else if(proc_stall)
                EX_signals <= EX_signals;
			else 
				EX_signals <= ID_signals;
		end
	end
endmodule

module FWD_UNIT_EX(
	Reg_R1_EX,
	Reg_R2_EX,
	Reg_W_MEM,
	Reg_W_WB,
	RegWrite_MEM,
	RegWrite_WB,
	isFWD_R1_EX,
	isFWD_R2_EX
);
	input RegWrite_MEM, RegWrite_WB;
	input [4:0] Reg_R1_EX, Reg_R2_EX, Reg_W_MEM, Reg_W_WB;
	output reg [1:0] isFWD_R1_EX, isFWD_R2_EX;

	// isFWD_R1_EX: 0->ReadData1_EX, 1->WriteData, 2->ALUresult_MEM
	always@(*) begin
		if (RegWrite_MEM && (Reg_W_MEM == Reg_R1_EX)) begin
			isFWD_R1_EX = 2'b10;
		end
		else if (RegWrite_WB && (Reg_W_WB == Reg_R1_EX)) begin
			isFWD_R1_EX = 2'b01;
		end
		else begin
			isFWD_R1_EX = 2'b00;
		end
	end

	// isFWD_R2_EX: 0->ReadData2_EX, 1->WriteData, 2->ALUresult_MEM
	always@(*) begin
		if (RegWrite_MEM && (Reg_W_MEM == Reg_R2_EX)) begin
			isFWD_R2_EX = 2'b10;
		end
		else if (RegWrite_WB && (Reg_W_WB == Reg_R2_EX)) begin
			isFWD_R2_EX = 2'b01;
		end
		else begin
			isFWD_R2_EX = 2'b00;
		end
	end
endmodule

module ALU(ALUin1, ALUin2, ALUctrl, ALUresult);
	input      [31:0] ALUin1, ALUin2;
	input      [2:0]  ALUctrl;
	output reg [31:0] ALUresult;

	wire carry;
	wire [31:0] add_result, sub_result, and_result, or_result;
	wire [31:0] mul_result;

	assign add_result = ALUin1 + ALUin2;
	assign {carry, sub_result} = ALUin1 - ALUin2;
	assign and_result = ALUin1 & ALUin2;
	assign or_result = ALUin1 | ALUin2;
	assign mul_result = ALUin1 * ALUin2;

	always @(*) begin
		case (ALUctrl)
			3'b000: ALUresult = or_result;
			3'b001: ALUresult = and_result;
			3'b010: ALUresult = add_result;
			3'b011: ALUresult = sub_result;
			3'b100: ALUresult = mul_result;
			default: ALUresult = or_result;
		endcase
	end
endmodule

module Reg_EX_MEM(
	clk_i,
	start_i,
    proc_stall,
	EX_signals,
	MEM_signals
);
	input             clk_i, start_i, proc_stall;
	input      [72:0] EX_signals;
	output reg [72:0] MEM_signals;

	always @(posedge clk_i) begin
		if (start_i) begin
            if(proc_stall) begin 
                MEM_signals <= MEM_signals;
            end 
            else begin 
			    MEM_signals <= EX_signals;
            end
		end
	end
    endmodule

module Reg_MEM_WB(
	clk_i,
	start_i,
    proc_stall,
	MEM_signals,
	WB_signals
);
	input             clk_i, start_i, proc_stall;
	input      [70:0] MEM_signals;
	output reg [70:0] WB_signals;
	always @(posedge clk_i) begin
		if (start_i) begin
            if(proc_stall) begin 
                WB_signals <= WB_signals;
            end 
            else begin 
			    WB_signals <= MEM_signals;
            end
		end
	end
endmodule
