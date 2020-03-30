package posit


import chisel3._
import chisel3.Driver
import chisel3.util._

class PositSQRT (ps : Int) extends Module {
    val io = IO(new Bundle {
        val i_posit = Input(new Posit(ps))
        val i_ready        = Input(Bool())

        val o_posit = Output(new Posit(ps))
        val o_ready        = Output(Bool())
    })

    val sqrt_module = Module(new UIntSqrt((ps/2)))
    sqrt_module.io.i_bits := io.i_posit.fraction << ( (io.i_posit.fraction_size & 1.U) + (io.i_posit.exponent & 1.U) )
    sqrt_module.io.i_ready := io.i_ready
    io.o_posit.fraction := sqrt_module.io.o_bits
    io.o_ready :=  sqrt_module.io.o_ready

    io.o_posit.sign := 0.U
    io.o_posit.special_number := 0.U
    when( (io.i_posit.sign & io.i_posit.special_number) === 1.U ) {
        io.o_posit.sign := 1.U
        io.o_posit.special_number := 1.U
    } .elsewhen ( (~io.i_posit.sign & io.i_posit.special_number) === 1.U) {
        io.o_posit.sign := 0.U
        io.o_posit.special_number := 1.U
    } .elsewhen ( (~io.i_posit.sign) === 1.U) {
        io.o_posit.sign := 1.U
        io.o_posit.special_number := 1.U
    } .otherwise {
        io.o_posit.sign := 0.U
        io.o_posit.special_number := 0.U
        io.o_posit.exponent_size := io.i_posit.exponent_size
        io.o_posit.regime := io.i_posit.regime >> 1
        io.o_posit.exponent := (io.i_posit.exponent + (io.i_posit.regime & 1.S).asUInt << io.o_posit.exponent_size) >> 1
        io.o_posit.fraction_size := (io.i_posit.fraction_size + io.i_posit.fraction_size & 1.U) >> 1
        io.o_posit.b_m := 0.U
    }
}

class UIntSqrt_old_1(size : Int) extends Module {
    val io = IO(new Bundle {
        val i_bits        = Input(Bits((2*size).W))
        val o_bits        = Output(Bits(size.W))
        val o_ready        = Output(Bool())
    })

    val r_sem = RegInit(0.U(1.W))
    val r_result = RegInit(0.U(size.W))
    val r_quo = RegInit(0.U(size.W))
    val r_value = RegInit(0.U(size.W))
    val r_counter = RegInit(0.U(size.W))
    r_counter := r_counter + 1.U
    val w_substract = Wire(UInt(size.W))
    w_substract := Cat(r_result, r_quo) & Fill(size, r_quo)
    val w_q_1_value = Wire(UInt(size.W))
    w_q_1_value := Cat(r_result, 1.U) & Fill(size, 1.U)
    val w_q_2_value = Wire(UInt(size.W))
    w_q_2_value := 0.U

    r_value := ( (r_value - w_substract) << 2) | ((io.i_bits >> ((size-2).U - (r_counter << 1))) & 3.U)
    
    when(w_q_1_value > r_value) {
        r_quo := 0.U
    } .otherwise {
        r_quo := 1.U
    }

    r_sem := r_sem
    when(r_counter >= size.U) {
        when(r_sem === 1.U) {
            r_result := r_result
        } .otherwise {
            r_result := (r_result << 1) | r_quo
            r_sem := 1.U
        }
    } .otherwise {
        r_result := (r_result << 1) + (r_quo << 1)
    }
    io.o_bits := r_result
    io.o_ready :=  r_sem
}


class UIntSqrt_old_2(size : Int) extends Module {
    val io = IO(new Bundle {
        val i_bits        = Input(Bits((2*size).W))
        val i_ready        = Input(Bool())
        val o_bits        = Output(Bits(size.W))
        val o_ready        = Output(Bool())
        val o_R        = Output(Bits((size+1).W))
        val o_D        = Output(Bits((2*size).W))
        val o_counter  = Output(Bits(size.W))
        val o_wR1  = Output(Bits((size+1).W))
        val o_wR2_1  = Output(Bits((size+1).W))
        val o_wR2_2  = Output(Bits((size+1).W))
    })

    val r_Q = RegInit(0.U(size.W))
    val r_R = RegInit(0.S((size+1).W))
    val r_D = RegInit(0.U((2*size).W));
    val r_counter = RegInit(0.U(size.W))
    val w_R1_1 = Wire(SInt((size+1).W))
    w_R1_1 := (r_R << 2) | ( (r_D >> (r_counter + r_counter)).zext & 3.S)
    val w_R1_2 = Wire(SInt((size+1).W))
    w_R1_2 := w_R1_1 - ((r_Q.zext << 2) | 1.S)
    val w_R2_1 = Wire(SInt((size+1).W))
    w_R2_1 := (r_R << 2) | ( (r_D >> (r_counter + r_counter)).zext & 3.S)
    val w_R2_2 = Wire(SInt((size+1).W))
    w_R2_2 := w_R2_1 + ((r_Q.zext << 2) | 3.S)
    
    when(io.i_ready) {
        r_D := io.i_bits
        r_counter := (size-1).U
        r_R := 0.S
        r_Q := 0.U
    } .otherwise {
        when(r_R >= 0.S) {
            r_R := w_R1_2
            when(w_R1_2 >= 0.S) {
                r_Q := (r_Q << 1) | 1.U
            } .otherwise {
                r_Q := r_Q << 1
            }
        } .otherwise {
            r_R := w_R2_2
            when(w_R2_2 >= 0.S) {
                r_Q := (r_Q << 1) | 1.U
            } .otherwise {
                r_Q := r_Q << 1
            }
        }
        r_counter := r_counter - 1.U
        //r_D := r_D >> 2
    }

    io.o_bits := r_Q
    io.o_ready :=  (r_counter === 0.U)

    io.o_D := r_D
    io.o_R := r_R.asUInt
    io.o_counter  := r_counter
    io.o_wR1  := w_R1_1.asUInt
    io.o_wR2_1  := w_R1_2.asUInt
    io.o_wR2_2  := w_R2_2.asUInt

}



class UIntSqrt(size : Int) extends Module {
    val io = IO(new Bundle {
        val i_bits        = Input(Bits((2*size).W))
        val i_ready        = Input(Bool())
        val o_bits        = Output(Bits(size.W))
        val o_ready        = Output(Bool())
        val o_R        = Output(Bits((size+1).W))
        val o_D        = Output(Bits((2*size).W))
        val o_counter  = Output(Bits(size.W))
        val o_wR1  = Output(Bits((size+1).W))
        val o_wR2_1  = Output(Bits((size+1).W))
        val o_wR2_2  = Output(Bits((size+1).W))
    })

    val r_Q = RegInit(0.U(size.W))
    val r_R = RegInit(0.S((size+2).W))
    val r_D = RegInit(0.U((2*size).W));
    val r_counter = RegInit(0.U(size.W))
    val w_R1_1 = Wire(UInt((size+2).W))
    w_R1_1 := (r_R(size,0) << 2) | Cat(r_D(0), r_D(1))
    val w_R1_2 = Wire(SInt((size+2).W))
    w_R1_2 := w_R1_1.asSInt - ((r_Q.zext << 2) | 1.S)
    val w_R2_1 = Wire(UInt((size+2).W))
    w_R2_1 := (r_R(size,0) << 2) | Cat(r_D(0), r_D(1))
    val w_R2_2 = Wire(SInt((size+2).W))
    w_R2_2 := w_R2_1.asSInt + ((r_Q.zext << 2) | 3.S)
    
    when(io.i_ready) {
        r_D := Reverse(io.i_bits)
        r_counter := 0.U
        r_R := 0.S
        r_Q := 0.U
    } .otherwise {
        when(r_R(size+1) === 0.U) {
            r_R := w_R1_2
            when(w_R1_2(size+1) === 0.U) {
                r_Q := (r_Q << 1) | 1.U
            } .otherwise {
                r_Q := r_Q << 1
            }
        } .otherwise {
            r_R := w_R2_2
            when(w_R2_2(size+1) === 0.U) {
                r_Q := (r_Q << 1) | 1.U
            } .otherwise {
                r_Q := r_Q << 1
            }
        }
        r_counter := r_counter + 1.U
        r_D := r_D >> 2
    }

    io.o_bits := r_Q
    io.o_ready :=  (r_counter === size.U)

    io.o_D := r_D
    io.o_R := r_R.asUInt
    io.o_counter  := r_counter
    io.o_wR1  := w_R1_1
    io.o_wR2_1  := w_R1_2.asUInt
    io.o_wR2_2  := w_R2_2.asUInt

}