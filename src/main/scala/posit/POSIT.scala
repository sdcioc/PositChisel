package posit


import chisel3._
import chisel3.Driver
import chisel3.util._


class Posit (size : Int) extends Bundle {
    val sign = Bool()
    val special_number = Bool()
    val regime = SInt(size.W)
    val exponent = UInt(size.W)
    val fraction = UInt((2*size).W)
    val exponent_size = UInt(log2Ceil(size).W)
    val fraction_size = UInt(log2Ceil(2*size).W)
    val b_m = Bool()

    override def cloneType = (new Posit(size)).asInstanceOf[this.type]
}


class NormalisePosit(size : Int) extends Module {
    val io = IO(new Bundle {
        val i_k           = Input(SInt(size.W))
        val i_e           = Input(UInt((size).W))
        val i_f           = Input(UInt((2*size).W))
        val i_fs          = Input(UInt(log2Ceil(2*size).W))
        val i_es          = Input(UInt(log2Ceil(size).W))
        val o_k           = Output(SInt(size.W))
        val o_e           = Output(UInt((size).W))
        val o_f           = Output(UInt((2*size).W))
    })

    val exponent_aux = Wire(UInt((size).W))
    val regime_aux = Wire(SInt(size.W))
    val regime_aux_2 = Wire(SInt(size.W))
    val mf = Wire(UInt(log2Ceil(2*size).W))
    val mf_aux = Wire(UInt(size.W))
    val mfe = Wire(UInt((size).W))
    val mfk = Wire(SInt(size.W))

    exponent_aux := io.i_e
    regime_aux := io.i_k
    io.o_f := io.i_f
    io.o_k := regime_aux
    io.o_e := exponent_aux

    when(io.i_e >= (1.U<<io.i_es)) {
        exponent_aux := io.i_e - (1.U<<io.i_es)
        regime_aux := io.i_k + 1.S
    }

    mf_aux := PriorityEncoder(Reverse(io.i_f))
    mf := mf_aux
    mfk := (mf >> io.i_es).zext
    mfe := mf & ((1.U << (io.i_es)) - 1.U)
    regime_aux_2 := regime_aux

    when(io.i_f >= (1.U<<(io.i_fs+1.U))) {
        mf := ( (2*size-1).U - io.i_fs) - mf_aux
        io.o_f := io.i_f >> mf
        when((exponent_aux + mfe) >= (1.U<<io.i_es)) {
            io.o_e := exponent_aux + mfe - (1.U<<io.i_es)
            regime_aux_2 := regime_aux + 1.S
        } .otherwise {
            io.o_e := exponent_aux + mfe
            regime_aux_2 := regime_aux
        }
        io.o_k := regime_aux_2 + mfk
    } .elsewhen (io.i_f < (1.U<<(io.i_fs))) {
        mf := mf_aux - ( (2*size-1).U - io.i_fs)
        io.o_f := io.i_f << mf
        when(exponent_aux < mfe) {
            io.o_e := exponent_aux + (1.U<<io.i_es) - mfe 
            regime_aux_2 := regime_aux - 1.S
        } .otherwise {
            io.o_e := exponent_aux - mfe
            regime_aux_2 := regime_aux
        }
        io.o_k := regime_aux_2 - mfk
    } .otherwise {
        io.o_f := io.i_f
        io.o_e := exponent_aux
        io.o_k := regime_aux
    }

}

object NormalisePosit extends App {
  chisel3.Driver.execute(Array[String](), () => new NormalisePosit(32))
}


class EncodePosit(size : Int) extends Module {
    val io = IO(new Bundle {
        val i_posit       = Input(new Posit(size))
        val o_bits        = Output(UInt(size.W))
        val debug_fraction       = Output(UInt((2*size).W))
        val debug_exponent        = Output(UInt(size.W))
        val debug_regime        = Output(SInt(size.W))
        val debug_return_value        = Output(UInt(size.W))
        val debug_rn        = Output(UInt(size.W))
        val debug_rs        = Output(UInt(size.W))
        val debug_nrs        = Output(UInt(size.W))
        val debug_regimebits        = Output(UInt(size.W))
        val debug_otherbits        = Output(UInt(size.W))
        val debug_b_nplus1        = Output(Bits(1.W))
        val debug_b_m        = Output(Bits(1.W))
        val debug_addOne        = Output(Bits(1.W))
        val debug_value        = Output(UInt((3*size).W))
    })

    val fraction = Wire(UInt((2*size).W))
    io.debug_fraction := fraction
    val exponent = Wire(UInt((size).W))
    io.debug_exponent := exponent
    val regime = Wire(SInt(size.W))
    io.debug_regime := regime
    val return_value = Wire(UInt((size).W))
    io.debug_return_value := return_value
    val rn = Wire(UInt(size.W))
    io.debug_rn := rn
    val rs = Wire(UInt(size.W))
    io.debug_rs := rs
    val nrs = Wire(UInt(log2Ceil(size).W))
    io.debug_nrs := nrs
    val regimebits = Wire(UInt(size.W))
    io.debug_regimebits := regimebits
    val otherbits = Wire(UInt(size.W))
    io.debug_otherbits := otherbits
    val b_nplus1 = Wire(UInt(1.W))
    io.debug_b_nplus1 := b_nplus1
    val b_m = Wire(UInt(1.W))
    io.debug_b_m := b_m
    val addOne = Wire(UInt(1.W))
    io.debug_addOne := addOne
    val value = Wire(UInt((3*size).W))
    io.debug_value := value

    val normlaiser = Module(new NormalisePosit(size))
    normlaiser.io.i_k := io.i_posit.regime
    normlaiser.io.i_e := io.i_posit.exponent
    normlaiser.io.i_f := io.i_posit.fraction
    normlaiser.io.i_fs := io.i_posit.fraction_size
    normlaiser.io.i_es := io.i_posit.exponent_size
    regime := normlaiser.io.o_k
    exponent := normlaiser.io.o_e
    fraction := normlaiser.io.o_f
    


    return_value := 0.U
    rn := Mux(regime >= 0.S, regime.asUInt + 1.U, (-regime).asUInt)
    rs := rn + 1.U
    nrs := Mux(0.S < ((size-1).S - rs.zext), ((size-1).U - rs), 0.U)
    regimebits := Mux(regime >= 0.S, (Cat(Fill(size-1, 1.U(1.W)), 0.U(1.W)) << (nrs)) & (~(1.U <<(size-1))), 1.U << (nrs))


    value := Cat(exponent, (fraction << ( (2*size).U - io.i_posit.fraction_size))(2*size-1, 0)) << (size.U-io.i_posit.exponent_size)
    otherbits := value(3*size-1,2*size) >> ((size).U - nrs)

    b_nplus1 := (value(3*size-1,2*size) >> ((size-1).U - nrs)) & 1.U

    b_m := (value(3*size-1,2*size) &  ( (1.U << ((size-1).U - nrs)) - 1.U)).orR | value(2*size-1,0).orR | io.i_posit.b_m
    addOne := b_nplus1 & (b_m | ( ~b_m & ( otherbits(0) | regimebits(0) )  ))


    when(io.i_posit.special_number === 1.U) {
        when(io.i_posit.sign === 1.U) {
            io.o_bits := 1.U << (size-1) 
        } .otherwise {
            io.o_bits := 0.U 
        }
    } .elsewhen (fraction === 0.U) {
        io.o_bits := 0.U
    } .otherwise {
        when(regime >= ((size-2).S)) {
            return_value := (1.U << (size-1)) - 1.U
        } .elsewhen (regime < (-(size-2).S)) {
            return_value := 1.U
        } .otherwise {
            return_value := (regimebits | otherbits) + addOne
        }
        when(io.i_posit.sign === 1.U) {
            io.o_bits := ~return_value + 1.U
        } .otherwise {
            io.o_bits := return_value
        }
    }

}


object EncodePosit extends App {
  chisel3.Driver.execute(Array[String](), () => new EncodePosit(8))
}


class DecodePosit(size : Int) extends Module {
    val io = IO(new Bundle {
        val i_bits        = Input(Bits(size.W))
        val i_es          = Input(UInt(log2Ceil(size).W))
        val o_posit       = Output(new Posit(size))
    })

    val rs = Wire(UInt(size.W))
    val ers = Wire(UInt(size.W))
  
    io.o_posit.special_number := Mux( (io.i_bits === Fill(size, 0.U(1.W))) ||
                                      (io.i_bits === (1.U(1.W) << (size-1) )),
                                    1.U,
                                    0.U)
 
    io.o_posit.sign := io.i_bits(size-1)
 
    val bits_to_decode = Wire(UInt((size-1).W))

    io.o_posit.exponent_size := io.i_es

  
    bits_to_decode := Mux(io.o_posit.sign,
                          ~io.i_bits((size-2),0) + 1.U,
                          io.i_bits((size-2),0))
    
    val number_of_same_bit_value = Wire(UInt(log2Ceil(size).W))
    number_of_same_bit_value := Mux(bits_to_decode((size-2)),
                                    PriorityEncoder(Reverse(~bits_to_decode)),
                                    PriorityEncoder(Reverse(bits_to_decode)))
    
    io.o_posit.regime := Mux(bits_to_decode((size-2)),
                            Mux(bits_to_decode(0)===1.U && number_of_same_bit_value===(size-2).U,
                                number_of_same_bit_value.zext,
                                number_of_same_bit_value.zext - 1.S),
                             Mux(bits_to_decode(0)===0.U && number_of_same_bit_value===(size-2).U,
                             -number_of_same_bit_value.zext - 1.S,
                             -number_of_same_bit_value.zext))

    rs := number_of_same_bit_value + 1.U

    val posible_fraction_size = Wire(SInt((log2Ceil(size)+1).W))
    posible_fraction_size := size.S - 1.S - rs.zext -io.o_posit.exponent_size.zext
    io.o_posit.fraction_size := Mux(posible_fraction_size <= 0.S,
                                    0.U,
                                    size.U - 1.U - rs - io.o_posit.exponent_size)

    io.o_posit.fraction := Mux(io.o_posit.fraction_size === 0.U,
                               0.U,
                               ( Fill(size, 1.U(1.W)) >> (size.U-io.o_posit.fraction_size) & bits_to_decode)) | (1.U << io.o_posit.fraction_size)

    val posible_exponent_size_1 = Wire(SInt((log2Ceil(size)+1).W))
    posible_exponent_size_1 := size.S - 1.S - rs.zext
    val posible_exponent_size_2 = Wire(SInt((log2Ceil(size)+1).W))
    posible_exponent_size_2 := Mux(posible_exponent_size_1 < io.o_posit.exponent_size.zext,
                                   posible_exponent_size_1,
                                   io.o_posit.exponent_size.zext)
    ers := Mux(posible_exponent_size_2 <= 0.S,
                                    0.U,
                                    posible_exponent_size_2.asUInt)


    io.o_posit.exponent := Mux(ers === 0.U,
                               0.U,
                               (bits_to_decode >> (io.o_posit.fraction_size)) &
                               (Fill(size, 1.U(1.W))>> (size.U - ers)) ) << (io.o_posit.exponent_size - ers)
    io.o_posit.b_m := 0.U

}

object DecodePosit extends App {
  chisel3.Driver.execute(Array[String](), () => new DecodePosit(8))
}
