package posit


import chisel3._
import chisel3.Driver
import chisel3.util._


class PositMultiplier (ps : Int) extends Module {
    val io = IO(new Bundle {
        val i_posit_1 = Input(new Posit(ps))
        val i_posit_2 = Input(new Posit(ps))

        val o_posit = Output(new Posit(ps))
    })

    io.o_posit.sign := 1.U
    io.o_posit.special_number := 1.U
    io.o_posit.exponent_size := io.i_posit_1.exponent_size
    io.o_posit.exponent := 0.U
    io.o_posit.regime := 0.S
    io.o_posit.fraction_size := 0.U
    io.o_posit.fraction := 0.U
    io.o_posit.b_m := 0.U
    when( (io.i_posit_1.sign & io.i_posit_1.special_number) === 1.U ) {
        io.o_posit.sign := 1.U
        io.o_posit.special_number := 1.U
    } .elsewhen ( (io.i_posit_2.sign & io.i_posit_2.special_number) === 1.U) {
        io.o_posit.sign := 1.U
        io.o_posit.special_number := 1.U
    } .elsewhen ( (~io.i_posit_2.sign & io.i_posit_2.special_number) === 1.U) {
        io.o_posit.sign := 0.U
        io.o_posit.special_number := 1.U
    } .elsewhen ( (~io.i_posit_1.sign & io.i_posit_1.special_number) === 1.U) {
        io.o_posit.sign := 0.U
        io.o_posit.special_number := 1.U
    } .otherwise {
        io.o_posit.sign := io.i_posit_1.sign ^ io.i_posit_2.sign
        io.o_posit.special_number := 0.U
        io.o_posit.exponent_size := io.i_posit_1.exponent_size
        io.o_posit.exponent := io.i_posit_1.exponent + io.i_posit_2.exponent
        io.o_posit.regime := io.i_posit_1.regime + io.i_posit_2.regime
        io.o_posit.fraction_size := io.i_posit_1.fraction_size + io.i_posit_2.fraction_size
        io.o_posit.fraction := io.i_posit_1.fraction * io.i_posit_2.fraction
        io.o_posit.b_m := 0.U
    }
}

class PositMultiplierTester (ps : Int) extends Module {
    val io = IO(new Bundle {
        val i_posit_1 = Input(Bits(ps.W))
        val i_posit_2 = Input(Bits(ps.W))
        val i_es      = Input(UInt(log2Ceil(ps).W))
        val o_posit   = Output(Bits(ps.W))
        val debug_fraction       = Output(UInt((2*ps).W))
        val debug_exponent        = Output(UInt(ps.W))
        val debug_regime        = Output(SInt(ps.W))
        val debug_fraction_2       = Output(UInt((2*ps).W))
        val debug_exponent_2        = Output(UInt(ps.W))
        val debug_regime_2        = Output(SInt(ps.W))
        val debug_b_nplus1        = Output(Bits(1.W))
        val debug_b_m        = Output(Bits(1.W))
        val debug_addOne        = Output(Bits(1.W))
        val debug_fs        = Output(UInt(ps.W))
        val debug_fraction_3       = Output(UInt((2*ps).W))
    })

    val multiplier =  Module(new PositMultiplier(ps))
    val decoder_1 =  Module(new DecodePosit(ps))
    decoder_1.io.i_bits := io.i_posit_1
    decoder_1.io.i_es := io.i_es
    val decoder_2 =  Module(new DecodePosit(ps))
    decoder_2.io.i_bits := io.i_posit_2
    decoder_2.io.i_es := io.i_es
    multiplier.io.i_posit_1 := decoder_1.io.o_posit
    multiplier.io.i_posit_2 := decoder_2.io.o_posit
    val encoder =  Module(new EncodePosit(ps))
    encoder.io.i_posit := multiplier.io.o_posit
    io.debug_fraction := encoder.io.debug_fraction
    io.debug_exponent := encoder.io.debug_exponent
    io.debug_regime := encoder.io.debug_regime
    io.debug_fraction_2 := multiplier.io.o_posit.fraction
    io.debug_exponent_2 := multiplier.io.o_posit.exponent
    io.debug_regime_2 := multiplier.io.o_posit.regime
    io.debug_b_nplus1 := encoder.io.debug_b_nplus1
    io.debug_b_m := encoder.io.debug_b_m
    io.debug_addOne := encoder.io.debug_addOne
    io.debug_fs := decoder_2.io.o_posit.fraction_size
    io.debug_fraction_3 := decoder_2.io.o_posit.fraction
    io.o_posit := encoder.io.o_bits
}