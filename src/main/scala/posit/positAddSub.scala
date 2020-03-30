package posit


import chisel3._
import chisel3.Driver
import chisel3.util._


class PositAddSubSelector(ps : Int) extends Module {
    val io = IO(new Bundle {
        val i_posit_1 = Input(new Posit(ps))
        val i_posit_2 = Input(new Posit(ps))
        val i_op      = Input(Bits(1.W))


        val o_posit_1 = Output(new Posit(ps))
        val o_posit_2 = Output(new Posit(ps))
        val o_op      = Output(Bits(1.W))
        val o_sign    = Output(Bits(1.W))
    })

    val sign_aux    = Wire(UInt(1.W))
    val gt = Wire(UInt(1.W))

    io.o_op := io.i_op
    sign_aux := io.i_posit_1.sign
    when (io.i_op === 0.U) {
        when(io.i_posit_1.sign === io.i_posit_2.sign) {
            sign_aux := io.i_posit_1.sign
            io.o_op := io.i_op
        } .elsewhen (io.i_posit_1.sign === 1.U) {
            io.o_op := 1.U
            sign_aux := 1.U
        } .otherwise {
            io.o_op := 1.U
            sign_aux := 0.U
        }
    } .otherwise {
        when(io.i_posit_1.sign === io.i_posit_2.sign) {
            sign_aux := io.i_posit_1.sign
            io.o_op := io.i_op
        } .elsewhen (io.i_posit_1.sign === 1.U) {
            io.o_op := 0.U
            sign_aux := 1.U
        } .otherwise {
            io.o_op := 0.U
            sign_aux := 0.U
        }
    }

    io.o_sign := sign_aux
    io.o_posit_1 := io.i_posit_1
    io.o_posit_2 := io.i_posit_2
    gt := (io.i_posit_2.regime > io.i_posit_1.regime) || ((io.i_posit_2.regime === io.i_posit_1.regime) && (io.i_posit_2.exponent > io.i_posit_1.exponent)) || ((io.i_posit_2.regime === io.i_posit_1.regime) && (io.i_posit_2.exponent === io.i_posit_1.exponent) && (io.i_posit_2.fraction > io.i_posit_1.fraction));
    when(gt === 1.U) {
        when(io.o_op === 1.U) {
            io.o_sign := ~sign_aux
        }
        io.o_posit_1 := io.i_posit_2
        io.o_posit_2 := io.i_posit_1
    }
}

class PositAdderSubtractor (ps : Int) extends Module {
    val io = IO(new Bundle {
        val i_posit_1 = Input(new Posit(ps))
        val i_posit_2 = Input(new Posit(ps))
        val i_op      = Input(Bits(1.W))

        val o_posit = Output(new Posit(ps))
        val debug_l_t  = Output(UInt(log2Ceil(2*ps).W))

    })

    val l_op_aux    = Wire(UInt(1.W))
    val l_posit_1 = Wire(new Posit(ps))
    val l_posit_2 = Wire(new Posit(ps))
    val l_t_aux   = Wire(SInt((ps+1).W))
    val l_t   = Wire(UInt(log2Ceil(2*ps).W))
    io.debug_l_t := l_t

    val selector = Module(new PositAddSubSelector(ps))
    selector.io.i_op := io.i_op
    selector.io.i_posit_1 := io.i_posit_1
    selector.io.i_posit_2 := io.i_posit_2

    l_op_aux := selector.io.o_op
    l_posit_1 := selector.io.o_posit_1
    l_posit_2 := selector.io.o_posit_2


    l_t_aux := ((l_posit_1.regime - l_posit_2.regime) << l_posit_1.exponent_size) + (l_posit_1.exponent.zext - l_posit_2.exponent.zext)

    io.o_posit.sign := 0.U
    io.o_posit.special_number := 0.U
    io.o_posit.regime := 0.S
    io.o_posit.exponent := 0.U
    io.o_posit.fraction := 0.U
    io.o_posit.exponent_size := 0.U
    io.o_posit.fraction_size :=0.U
    io.o_posit.b_m := 0.U
    l_t := 0.U
    when( ( (l_posit_1.sign & l_posit_1.special_number)|(l_posit_2.sign & l_posit_2.special_number) ) === 1.U ) {
        io.o_posit.sign := 1.U
        io.o_posit.special_number := 1.U
    } .elsewhen ( (~l_posit_2.sign & l_posit_2.special_number) === 1.U) {
        io.o_posit.sign := selector.io.o_sign
        io.o_posit.special_number := l_posit_1.special_number
        io.o_posit.regime := l_posit_1.regime
        io.o_posit.exponent := l_posit_1.exponent
        io.o_posit.fraction := l_posit_1.fraction
        io.o_posit.exponent_size := l_posit_1.exponent_size
        io.o_posit.fraction_size := l_posit_1.fraction_size
        io.o_posit.b_m := l_posit_1.b_m
    } .otherwise {
        //TODO: maybe to add lposit 1 b_m and lposit2 b_m
        io.o_posit.sign := selector.io.o_sign
        io.o_posit.special_number := 0.U
        io.o_posit.regime := l_posit_1.regime
        io.o_posit.exponent := l_posit_1.exponent
        io.o_posit.exponent_size := l_posit_1.exponent_size
        io.o_posit.fraction_size := (2*ps-4).U
        when(l_t_aux > (2*ps-4).S) {
            io.o_posit.fraction := (l_posit_1.fraction << (io.o_posit.fraction_size -  l_posit_1.fraction_size))
            when(l_posit_2.fraction > (1.U << l_posit_2.fraction_size)) {
                io.o_posit.b_m := 1.U
            } .otherwise {
                io.o_posit.b_m := 0.U
            }
        } .otherwise {
            l_t := l_t_aux.asUInt
            when(l_op_aux === 0.U) {
                io.o_posit.fraction := (l_posit_1.fraction << (io.o_posit.fraction_size -  l_posit_1.fraction_size)) + ( (l_posit_2.fraction << (io.o_posit.fraction_size -  l_posit_2.fraction_size)) >> l_t)
                io.o_posit.b_m := ((l_posit_2.fraction << (io.o_posit.fraction_size -  l_posit_2.fraction_size)) & ((1.U<<l_t)-1.U)) =/= 0.U
            } .otherwise {
                io.o_posit.fraction := (l_posit_1.fraction << (io.o_posit.fraction_size -  l_posit_1.fraction_size)) - ( (l_posit_2.fraction << (io.o_posit.fraction_size -  l_posit_2.fraction_size)) >> l_t)
                io.o_posit.b_m := ((l_posit_2.fraction << (io.o_posit.fraction_size -  l_posit_2.fraction_size)) & ((1.U<<l_t)-1.U)) =/= 0.U
            }
        }

    }
}


class PositAdderSubtractorTester (ps : Int) extends Module {
    val io = IO(new Bundle {
        val i_posit_1 = Input(Bits(ps.W))
        val i_posit_2 = Input(Bits(ps.W))
        val i_op      = Input(Bits(1.W))
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
        val debug_l_t  = Output(UInt(log2Ceil(2*ps).W))
    })

    val addersubtractor =  Module(new PositAdderSubtractor(ps))
    val decoder_1 =  Module(new DecodePosit(ps))
    decoder_1.io.i_bits := io.i_posit_1
    decoder_1.io.i_es := io.i_es
    val decoder_2 =  Module(new DecodePosit(ps))
    decoder_2.io.i_bits := io.i_posit_2
    decoder_2.io.i_es := io.i_es
    addersubtractor.io.i_op := io.i_op
    addersubtractor.io.i_posit_1 := decoder_1.io.o_posit
    addersubtractor.io.i_posit_2 := decoder_2.io.o_posit
    val encoder =  Module(new EncodePosit(ps))
    encoder.io.i_posit := addersubtractor.io.o_posit
    io.debug_l_t := addersubtractor.io.debug_l_t
    io.debug_fraction := encoder.io.debug_fraction
    io.debug_exponent := encoder.io.debug_exponent
    io.debug_regime := encoder.io.debug_regime
    io.debug_fraction_2 := addersubtractor.io.o_posit.fraction
    io.debug_exponent_2 := addersubtractor.io.o_posit.exponent
    io.debug_regime_2 := addersubtractor.io.o_posit.regime
    io.debug_b_nplus1 := encoder.io.debug_b_nplus1
    io.debug_b_m := encoder.io.debug_b_m
    io.debug_addOne := encoder.io.debug_addOne
    io.debug_fs := decoder_2.io.o_posit.fraction_size
    io.debug_fraction_3 := decoder_2.io.o_posit.fraction
    io.o_posit := encoder.io.o_bits
}