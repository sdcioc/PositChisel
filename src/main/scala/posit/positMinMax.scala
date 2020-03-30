package posit


import chisel3._
import chisel3.Driver
import chisel3.util._


class PositMinMax(ps : Int) extends Module {
    val io = IO(new Bundle {
        val i_bits_1        = Input(Bits(ps.W))
        val i_bits_2        = Input(Bits(ps.W))
        val i_op            = Input(UInt(1.W))
        val o_bits        = Output(Bits(ps.W))
    })

    io.o_bits := 0.U

    val lt =  Module(new PositL(ps))
    lt.io.i_bits_1 := io.i_bits_1
    lt.io.i_bits_2 := io.i_bits_2

    when( ( (lt.io.o_result === 1.U) && (io.i_op === 0.U) ) || ( (lt.io.o_result === 0.U) && (io.i_op === 1.U) ) ) {
        io.o_bits := io.i_bits_1
    } .otherwise {
        io.o_bits := io.i_bits_2
    }

}