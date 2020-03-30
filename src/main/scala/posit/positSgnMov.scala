package posit


import chisel3._
import chisel3.Driver
import chisel3.util._


class PositSGNJ(size : Int) extends Module {
    val io = IO(new Bundle {
        val i_bits_1        = Input(Bits(size.W))
        val i_bits_2        = Input(Bits(size.W))
        val i_op            = Input(Bits(2.W))
        val o_bits        = Output(Bits(size.W))
    })

    val value = Wire(Bits((size-1).W))
    val sign = Wire(UInt(1.W))
    sign := ((io.i_bits_1(size-1) ^ io.i_bits_2(size-1)) & io.i_op(1)) | (io.i_bits_2(size-1) ^ io.i_op(0))
    value := 0.U
    when(io.i_bits_1(size-1) === 1.U) {
        value := (((~(io.i_bits_1)) + 1.U)) & ((1.U<<(size-1)) - 1.U)
    } .otherwise {
        value := io.i_bits_1(size-2, 0)
    }
    io.o_bits := 0.U
    when(sign === 1.U) {
        io.o_bits := (1.U<<(size-1)) | (((~(value)) + 1.U) & ((1.U<<(size-1)) - 1.U))
    } .otherwise {
        io.o_bits := value
    }
}