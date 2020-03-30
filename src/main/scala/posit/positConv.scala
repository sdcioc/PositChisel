package posit


import chisel3._
import chisel3.Driver
import chisel3.util._

class PositToInt(ps : Int, is : Int) extends Module {
    val io = IO(new Bundle {
        val i_posit       = Input(new Posit(ps))
        val o_integer     = Output(Bits(is.W))
    })

    val exponent = Wire(UInt((2*ps).W))
    val integer_aux = Wire(UInt(is.W))
    val shiftLeft = Wire(UInt((log2Ceil(2*ps)+1).W))
    val shiftRight = Wire(UInt((log2Ceil(2*ps)+1).W))

    io.o_integer := 0.U
    exponent := 0.U
    integer_aux := 0.U
    shiftLeft := 0.U
    shiftRight := 0.U
    when( (io.i_posit.sign & io.i_posit.special_number) === 1.U ) {
        io.o_integer := Cat(0.U(1.W),Fill(is-1, 1.U(1.W)))
    } .elsewhen ( (~io.i_posit.sign & io.i_posit.special_number) === 1.U) {
        io.o_integer := 0.U
    } .elsewhen ( io.i_posit.regime < 0.S) {
        io.o_integer := 0.U
    } .otherwise {
        exponent := (io.i_posit.regime.asUInt << io.i_posit.exponent_size) + io.i_posit.exponent
        when(exponent >= (is-1).U) {
            integer_aux := Cat(0.U(1.W),Fill(is-1, 1.U(1.W)))
        } .otherwise {
            when(exponent > io.i_posit.fraction_size) {
                shiftLeft := (exponent - io.i_posit.fraction_size)
                integer_aux := io.i_posit.fraction << shiftLeft
            } .otherwise {
                shiftRight := (io.i_posit.fraction_size - exponent)
                integer_aux := io.i_posit.fraction >> shiftRight
            }
        }
        when(io.i_posit.sign) {
            io.o_integer := (~integer_aux + 1.U)
        } .otherwise {
            io.o_integer := integer_aux
        }
    }
}

class IntToPosit(ps : Int, is : Int) extends Module {
    val io = IO(new Bundle {
        val i_integer     = Input(Bits(is.W))
        val i_es          = Input(Bits(log2Ceil(ps).W))
        val o_posit       = Output(new Posit(ps))
    })

    val exponent = Wire(UInt((2*ps).W))
    val integer_aux = Wire(UInt(is.W))

    io.o_posit.sign := io.i_integer(is-1)

    io.o_posit.special_number := 0.U
    when(io.i_integer === 0.U) {
        io.o_posit.special_number := 1.U
    } .otherwise {
        io.o_posit.special_number := 0.U
    }

    integer_aux := io.i_integer
    when(io.o_posit.sign === 1.U) {
        integer_aux := ~io.i_integer + 1.U
    } .otherwise {
        integer_aux := io.i_integer
    }

    exponent := (is-1).U - PriorityEncoder(Reverse(integer_aux.asUInt))
    io.o_posit.exponent_size := io.i_es
    io.o_posit.regime := (exponent >> io.o_posit.exponent_size).zext
    io.o_posit.exponent := exponent & ((1.U <<io.o_posit.exponent_size) - 1.U)
    io.o_posit.fraction := integer_aux
    io.o_posit.fraction_size := exponent
    io.o_posit.b_m := 0.U

}



class PositPositToIntTester (ps : Int, is : Int) extends Module {
    val io = IO(new Bundle {
        val i_posit         = Input(Bits(ps.W))
        val i_es            = Input(UInt(log2Ceil(ps).W))
        val o_integer       = Output(Bits(is.W))
        val debug_fraction  = Output(UInt((2*ps).W))
        val debug_exponent  = Output(UInt(ps.W))
        val debug_regime    = Output(SInt(ps.W))
        val debug_fs        = Output(UInt(ps.W))
    })

    val decoder =  Module(new DecodePosit(ps))
    decoder.io.i_bits := io.i_posit
    decoder.io.i_es := io.i_es
    val positToIntConverter =  Module(new PositToInt(ps, is))
    positToIntConverter.io.i_posit := decoder.io.o_posit
    io.debug_fraction := decoder.io.o_posit.fraction
    io.debug_exponent := decoder.io.o_posit.exponent
    io.debug_regime := decoder.io.o_posit.regime
    io.debug_fs := decoder.io.o_posit.fraction_size
    io.o_integer :=  positToIntConverter.io.o_integer
}


class PositIntToPositTester  (ps : Int, is : Int) extends Module {
    val io = IO(new Bundle {
        val i_es            = Input(UInt(log2Ceil(ps).W))
        val i_integer       = Input(Bits(is.W))
        val o_posit         = Output(Bits(ps.W))
        val debug_fraction  = Output(UInt((2*ps).W))
        val debug_exponent  = Output(UInt(ps.W))
        val debug_regime    = Output(SInt(ps.W))
        val debug_fs        = Output(UInt(ps.W))
    })

    val intToPositConverter =  Module(new IntToPosit(ps, is))
    intToPositConverter.io.i_integer := io.i_integer
    intToPositConverter.io.i_es := io.i_es
    val encoder =  Module(new EncodePosit(ps))
    encoder.io.i_posit := intToPositConverter.io.o_posit
    io.o_posit := encoder.io.o_bits
    
    io.debug_fraction := intToPositConverter.io.o_posit.fraction
    io.debug_exponent := intToPositConverter.io.o_posit.exponent
    io.debug_regime := intToPositConverter.io.o_posit.regime
    io.debug_fs := intToPositConverter.io.o_posit.fraction_size
}