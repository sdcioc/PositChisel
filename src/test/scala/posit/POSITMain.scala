package posit

import chisel3._
import chisel3.iotesters._
import java.math.BigInteger
import scala.math.{abs, max, min, sqrt, pow}

class TestPosit (val size : Int) {
    var sign: Int = 0
    var special_number: Int = 0
    var regime: Int = 0
    var exponent: BigInt = 0
    var fraction: BigInt = 0
    var exponent_size: Int = 0
    var fraction_size: Int = 0
    var b_m: Int = 0


    def min(that : TestPosit) : TestPosit = {
        if(this < that)
            return this
        else
            return that
    }

    def max(that : TestPosit) : TestPosit = {
        if(this < that)
            return that
        else
            return this
    }

    def sgnj(that : TestPosit, op : Int) : TestPosit = {
        var my_posit: TestPosit = new TestPosit(this.size)
        my_posit.sign = op match {
            case 0 => that.sign //sgnj
            case 1 => 1 - that.sign //sgnjn
            case 2 => this.sign ^ that.sign //sgnjx
            case _ => this.sign
        }
        my_posit.special_number = this.special_number
        my_posit.regime = this.regime
        my_posit.exponent = this.exponent
        my_posit.exponent_size = this.exponent_size
        my_posit.fraction = this.fraction
        my_posit.fraction_size = this.fraction_size
        my_posit.b_m = this.b_m

        return my_posit

    }

    def displayValue() : String = {
        return "s:" + this.sign.toString() + " sn:" + this.special_number.toString() + " k:" + this.regime.toString() + " e:" + this.exponent.toString() + " f:" + this.fraction.toString() + " es:" + this.exponent_size.toString() + " fs:" + this.fraction_size.toString() + " bm:" + this.b_m.toString()
    }

    def toDouble() : Double = {
        var return_value: Double = 0

        if(this.special_number == 1) {
            return 0
        }

        return_value =  scala.math.pow(scala.math.pow(2,scala.math.pow(2,this.exponent_size).toInt).toInt, this.regime).toDouble
        return_value =  return_value * scala.math.pow(2,this.exponent.toInt).toDouble

        return_value = return_value * (this.fraction.toDouble / (scala.math.pow(2,this.fraction_size).toDouble))
        
        if(this.sign == 1) {
            return_value = return_value * -1
        } else {
            return_value = return_value
        }
        return return_value
    }

    def toBigInt(is : Int) : BigInt = {
        var exponent : BigInt = 0
        var my_int : BigInt = 0
        var aux : BigInt = 0
        if( (this.special_number == 1) && (this.sign == 1)) {
            my_int = 1
            my_int = (my_int << (is - 1)) -1
            return my_int
        }
        if( (this.special_number == 1) && (this.sign == 0)) {
            return 0
        }
        if(this.regime < 0) {
            return 0
        }

        exponent =  (this.regime << this.exponent_size) + this.exponent
        //println("exponent=" + exponent.toString())
        //println("this.fraction=" + this.fraction.toString())
        if(exponent > (is-1)) {
            my_int = 1
            my_int = (my_int<<(is-1))-1
        } else {
            if(exponent > this.fraction_size) {
                aux = exponent - new BigInteger(this.fraction_size.toString())
                aux = this.fraction << aux.toInt
                my_int = aux
            } else {
                aux = new BigInteger(this.fraction_size.toString())
                aux =  aux - exponent
                aux = this.fraction >> aux.toInt
                my_int = aux
            }
        }
        /*
        println("aux=" + aux.toString())
        println("my_int=" + my_int.toString())
        println("~my_int=" + (~my_int).toString())
        println("~my_int+1=" + (~my_int+1).toString(2))
        aux = 1
        aux = (aux << (is+1)) - 1
        println("aux=" + aux.toString(2))
        println("(~my_int + 1) & aux=" + ((~my_int + 1) & aux).toString())
        println("this.sign=" + this.sign.toString())
        */
        if(this.sign == 1) {
            aux = 1
            aux = (aux << (is)) - 1
            my_int = (~my_int + 1) & aux
        }
        return my_int

    }

    def fromBigInt(a : BigInt, is : Int, es : Int) = {
        var sign : Int = 0
        var m : Int = 0
        var exponent : Int = 0
        var my_int : BigInt = 0
        var aux : BigInt = 0
        my_int = a
        if(my_int == 0 ) {
            this.special_number = 1
        } else {
            this.special_number = 0
        }
        sign = ((my_int >> (is-1))&1).toInt
        //println("sign=" + sign.toString())
        if(sign==1) {
            aux = 1
            aux = (aux << is) - 1
            my_int = (~my_int + 1) & aux
        }


        var countReg : Int = 0
        var sem: Int = 0
        for (index <- 0 to (is-1)) {
            aux = (my_int >> (is-1-index))
            //println("aux=" + aux.toString())
            aux =  aux & 1
            
            if(aux == 0 && sem == 0) {
                countReg = countReg + 1
            } else {
                sem = 1
            }
        }
        //println("countReg=" + countReg.toString())
        exponent = is - 1 - countReg
        //println("exponent=" + exponent.toString())
        //println("my_int=" + my_int.toString())
        this.exponent_size = es
        this.regime = exponent >> this.exponent_size
        this.exponent = exponent & ((1<<es)-1)
        this.fraction = my_int
        this.fraction_size = exponent
        if(sign==1) {
            this.sign = 1
        } else {
            this.sign = 0
        }
        
    }


    def sqrt() : TestPosit = {
        var my_posit: TestPosit = new TestPosit(this.size)
        var aux : BigInt = 0
        var sqrt_result_double : Double = 0

        if( this.special_number == 1 ) {
            my_posit = this
            return my_posit
        }
        if(this.sign == 1) {
            my_posit.sign = 1
            my_posit.special_number = 1
            return my_posit
        }
        my_posit.sign = 0
        my_posit.special_number = 0
        my_posit.regime = this.regime >> 1
        aux =  new BigInteger(this.regime.toString())
        aux = aux & 1
        aux = aux << this.exponent_size
        my_posit.exponent = (aux + this.exponent) >> 1
        my_posit.exponent_size = this.exponent_size
        my_posit.fraction_size = ( this.fraction_size + (this.fraction_size & 1) ) >> 1
        //my_posit.fraction = sqrt(this.fraction << (this.exponent & 1 + this.fraction_size & 1))
        //TODO
        sqrt_result_double = scala.math.sqrt( ( this.fraction << ( (this.exponent & 1).toInt + (this.fraction_size & 1) ) ).toLong )
        my_posit.fraction = new BigInteger(sqrt_result_double.toLong.toString())
        if (sqrt_result_double != sqrt_result_double.toLong.toDouble)
            my_posit.b_m = 1
        else
            my_posit.b_m = 0
        return my_posit
    }


    def -() : TestPosit = {
        var my_posit: TestPosit = new TestPosit(this.size)
        my_posit = this

        if( this.special_number == 1 ) {
            return my_posit
        }
        my_posit.sign = 1 - my_posit.sign
        return my_posit
    }

    def /(that : TestPosit) : TestPosit = {
        var my_posit: TestPosit = new TestPosit(this.size)

        if( (this.special_number == 1) && (this.sign == 1)) {
            my_posit = this
            return my_posit
        }
        if( (that.special_number == 1) && (that.sign == 1)) {
            my_posit = that
            return my_posit
        }
        if( (that.special_number == 1) && (that.sign == 0)) {
            my_posit.sign = 1
            my_posit.special_number = 1
            return my_posit
        }
        if( (this.special_number == 1) && (this.sign == 0)) {
            my_posit = this
            return my_posit
        }

        my_posit.sign = this.sign ^ that.sign
        my_posit.special_number = 0
        my_posit.regime = this.regime - that.regime
        if(that.exponent > this.exponent) {
            my_posit.exponent = this.exponent + (1<<this.exponent_size) - that.exponent
            my_posit.regime = my_posit.regime - 1
        } else {
            my_posit.exponent = this.exponent - that.exponent
        }
        my_posit.exponent_size = this.exponent_size
        my_posit.fraction_size = this.fraction_size + this.size - that.fraction_size
        my_posit.fraction = (this.fraction << this.size) /  that.fraction
        if(((this.fraction << this.size) %  that.fraction) != 0) {
            my_posit.b_m = 1
        } else {
            my_posit.b_m = 0
        }
        return my_posit
    }

    def *(that : TestPosit) : TestPosit = {
        var my_posit: TestPosit = new TestPosit(this.size)

        if( (this.special_number == 1) && (this.sign == 1)) {
            return this
        }
        if( (that.special_number == 1) && (that.sign == 1)) {
            return that
        }
        if( (this.special_number == 1) && (this.sign == 0)) {
            return this
        }
        if( (that.special_number == 1) && (that.sign == 0)) {
            return that
        }

        my_posit.sign = this.sign ^ that.sign
        my_posit.special_number = 0
        my_posit.regime = this.regime + that.regime
        my_posit.exponent = this.exponent + that.exponent
        my_posit.exponent_size = this.exponent_size
        my_posit.fraction_size = this.fraction_size + that.fraction_size
        my_posit.fraction = this.fraction * that.fraction
        my_posit.b_m = 0
        return my_posit

    }

    def -(that : TestPosit) : TestPosit = {
        var my_posit: TestPosit = new TestPosit(this.size)
        var t : Int = 0
        if(this.special_number == 1) {
            if(this.sign == 1) {
                return this
            } else {
                my_posit = that
                if(that.special_number == 0) {
                    my_posit.sign = 1 - my_posit.sign
                }
                return my_posit
            }
        }

        if(that.special_number == 1) {
            if(that.sign == 1) {
                return that
            } else {
                return this
            }
        }

        if(this.sign != that.sign) {
            my_posit = that
            my_posit.sign = 1 - that.sign
            return this + my_posit
        }
        if(that.abs() > this.abs()) {
            my_posit = that - this
            my_posit.sign = 1 - my_posit.sign
            return my_posit
        }


        my_posit.sign = this.sign
        my_posit.special_number = 0
        my_posit.regime = this.regime
        my_posit.exponent = this.exponent
        my_posit.exponent_size = this.exponent_size
        my_posit.fraction_size = this.size * 2 - 4
        //TODO de modificat t in BigInt la fel pentru +
        t = (this.regime << this.exponent_size) + this.exponent.toInt - (that.regime << that.exponent_size) - that.exponent.toInt
        my_posit.fraction = (this.fraction << (my_posit.fraction_size - this.fraction_size)) - ((that.fraction << (my_posit.fraction_size - that.fraction_size) ) >> t )
        if(((that.fraction << (my_posit.fraction_size - that.fraction_size) ) & ((1<<t)-1) ) != 0)
            my_posit.b_m = 1
        else
            my_posit.b_m = 0
        
        return my_posit

    }

    def +(that : TestPosit) : TestPosit = {
        var my_posit: TestPosit = new TestPosit(this.size)
        var t : Int = 0

        if(this.special_number == 1) {
            if(this.sign == 1) {
                return this
            } else {
                return that
            }
        }

        if(that.special_number == 1) {
            if(that.sign == 1) {
                return that
            } else {
                return this
            }
        }

        if(this.sign != that.sign) {
            my_posit = that
            my_posit.sign = 1 - that.sign
            return this - my_posit
        }
        if(that.abs() > this.abs()) {
            return that + this
        }


        my_posit.sign = this.sign
        my_posit.special_number = 0
        my_posit.regime = this.regime
        my_posit.exponent = this.exponent
        my_posit.exponent_size = this.exponent_size
        my_posit.fraction_size = this.size * 2 - 4
        t = (this.regime << this.exponent_size) + this.exponent.toInt - (that.regime << that.exponent_size) - that.exponent.toInt
        my_posit.fraction = (this.fraction << (my_posit.fraction_size - this.fraction_size)) + ((that.fraction << (my_posit.fraction_size - that.fraction_size) ) >> t )
        if(((that.fraction << (my_posit.fraction_size - that.fraction_size) ) & ((1<<t)-1) ) != 0)
            my_posit.b_m = 1
        else
            my_posit.b_m = 0
        
        return my_posit



    }

    def ===(that : TestPosit) : Boolean = {
        var posit_1: TestPosit = new TestPosit(this.size)
        posit_1 = this
        var posit_2: TestPosit = new TestPosit(this.size)
        posit_2 = that
        var aux : Boolean = false
        if(posit_1.binaryEncode() == posit_2.binaryEncode()) {
           return true
        }
        return false
    }

    def ==(that : TestPosit) : Boolean = {
        var aux : Boolean = false
        if(this.sign == that.sign) {
           if(this.regime == that.regime) {
                if(this.exponent == that.exponent) {
                    if(this.fraction == that.fraction) {
                        return true
                    }
                }
            }
        }
        return false
    }

    def >(that : TestPosit) : Boolean = {
        return !(this <= that)
    }

    def <=(that : TestPosit) : Boolean = {
        return (this < that) || (this == that)
    }
    

    def <(that : TestPosit) : Boolean = {
        var aux : Boolean = false
        if(this.sign > that.sign) {
            return true
        } else if(this.sign < that.sign) {
            return false
        } else {
            if(that.special_number == 1) {
                return false
            } else if(this.special_number == 1) {
                return true
            } else if(this.regime > that.regime) {
                aux = false
            } else if (this.regime < that.regime) {
                aux = true
            } else {
                if(this.exponent > that.exponent) {
                    aux = false
                } else if (this.exponent < that.exponent) {
                    aux = true
                } else {
                    if(this.fraction > that.fraction) {
                        aux = false
                    } else if (this.fraction < that.fraction) {
                        aux = true
                    } else {
                        return false
                    }
                }
            }
            if(this.sign == 1) {
                return !aux
            } else {
                return aux
            }
        }
    }

    def abs() : TestPosit = {
        var my_posit: TestPosit = new TestPosit(this.size)
        if( (this.sign == 1) && (this.special_number==1)) {
            my_posit.sign = 1
            my_posit.special_number = 1
        } else {
            my_posit.sign = 0
            my_posit.special_number = this.special_number
            my_posit.regime = this.regime
            my_posit.exponent = this.exponent
            my_posit.fraction = this.fraction
            my_posit.exponent_size = this.exponent_size
            my_posit.fraction_size = this.fraction_size
            my_posit.b_m = this.b_m
        }
        return my_posit
    }

    def normalise() = {
        var fraction: BigInt = 0
        var exponent: BigInt = 0
        var regime: Int = 0
        var ps : Int = 0
        var es: Int = 0
        var fs: Int = 0
        var aux: BigInt = 0
        fraction = this.fraction
        exponent = this.exponent
        regime = this.regime
        ps = this.size
        es = this.exponent_size
        fs = this.fraction_size
        
        aux = 1
        aux = aux << (fs+1)
        while(fraction >= aux) {
            fraction = fraction >> 1
            exponent = exponent + 1
        }
        if(fraction != 0) {
            aux = 1
            aux = aux << (fs)
            while(fraction < aux) {
                fraction = fraction << 1
                exponent = exponent - 1
            }
        } else {
            regime = -(ps*3)
        }

        while(exponent < 0) {
            exponent = exponent + (1<<es)
            regime = regime - 1

        }
        while(exponent >= (1<<es)) {
            exponent = exponent - (1<<es)
            regime = regime + 1
        }

        this.regime = regime
        this.exponent = exponent
        this.fraction = fraction
    }

    def binaryEncode() : BigInt = {
        var return_value: BigInt = 0
        var ps : Int = 0
        var fraction: BigInt = 0
        var fs: Int = 0
        var exponent: BigInt = 0
        var es: Int = 0
        var regime: Int = 0
        var rn: Int = 0
        var rs: Int = 0
        var regime_bits: BigInt = 0
        var ers: Int = 0
        var frs: Int = 0
        var b_nplus1: Boolean = false
        var b_m: Boolean = false
        var addOne: BigInt = 0
        var exponent_bits: BigInt = 0
        var fraction_bits: BigInt = 0
        var aux : BigInt = 0
        var aux_2 : BigInt = 0
        ps = this.size


        if(this.special_number == 1) {
            if(this.sign == 1) {
                aux = 1
                aux = aux << (ps-1)
                return_value = aux
            } else {
                return_value = 0
            }
            return return_value
        }

        this.normalise()

        fraction = this.fraction
        exponent = this.exponent
        regime = this.regime
        es = this.exponent_size
        fs = this.fraction_size
        
        if(fraction == 0) {
            return_value = 0
            return return_value
        } else if(regime >= (ps-2)) {
            aux = 1
            aux = aux << (ps-1)
            aux - aux - 1
            return_value = aux
        } else if(regime < -(ps-2)) {
            return_value = 1
        } else {
            if(regime >= 0) {
                rn = regime + 1
                rs = rn + 1
                aux = 1
                aux = aux << rn
                aux = aux - 1
                aux = aux << 1
                regime_bits = aux
            } else {
                rn = -regime
                rs = rn + 1
                regime_bits = 1
            }

            ers = scala.math.max(0, scala.math.min(es, ps - rs - 1))
            frs = scala.math.max(0, ps-rs-es-1)
            b_nplus1 = false
            b_m = (this.b_m != 0)
            /*
            println("ers=" + ers.toString())
            println("frs=" + frs.toString())
            println("rs=" + rs.toString())
            println("rn=" + rn.toString())
            println("regime=" + regime.toString())
            println("exponent=" + exponent.toString())
            println("fraction=" + fraction.toString())
            println("regime_bits=" + regime_bits.toString())
            */

            if(es-ers==0) {
                exponent_bits = exponent
                if(frs>=fs) {
                    b_nplus1 = false
                    fraction_bits = (fraction & ((1<<fs)-1)) << (frs-fs)
                    b_m = b_m
                } else {
                    //println("fraction_bits_aux=" + (fraction & ((1<<(fs-frs-1))-1)).toString())
                    //println("fraction_bits_aux2=" + fraction.toString(2))
                    aux = 1
                    aux = aux << (fs-frs-1)
                    b_nplus1 = ((fraction & aux) != 0) || b_nplus1
                    if(frs <= (fs-2)) {
                        aux = 1
                        aux = aux << (fs-frs-1)
                        aux = aux - 1
                        b_m = ((fraction & aux) != 0) || b_m
                    } else {
                        b_m = b_m
                    }
                    if(frs == 0 ) {
                        fraction_bits = 0
                    } else {
                        aux = 1
                        aux = aux << (fs)
                        aux = aux - 1
                        fraction_bits = (fraction & aux) >> (fs-frs)
                    }
                }
            } else {
                if(ers == 0) {
                    exponent_bits = 0
                } else {
                    exponent_bits = (exponent & ((1<<es)-1)) >> (es-ers)
                }
                b_nplus1 = ((exponent & (1<<(es-ers-1))) != 0) || b_nplus1
                aux = 1
                aux = aux << (fs)
                aux = aux - 1
                b_m = ((fraction & aux)!=0) || b_m
                if(es-ers > 1) {
                    b_m = ((exponent & ((1<<(es-ers-1))-1)) != 0) || b_m
                }
                fraction_bits = 0
            }
            //println("exponent_bits=" + exponent_bits.toString())
            //println("fraction_bits=" + fraction_bits.toString())
            return_value = (regime_bits << (ers+frs)) | (exponent_bits << frs) | fraction_bits
            if(b_nplus1 && (b_m || (!b_m && ( (return_value & 1) != 0 )))) {
                addOne = 1
            } else {
                addOne = 0
            }
            //println("b_nplus1=" + b_nplus1.toString())
            //println("b_m=" + b_m.toString())
            //println("addOne=" + addOne.toString())
            return_value = return_value + addOne
        }
        if(this.sign == 1) {
            aux = 1
            aux = aux << (ps-1)
            aux_2 = aux
            aux = aux - 1

            return_value = ((~(return_value)+1) & aux) | aux_2
        }
        return return_value
    }


    def decodeBinary(a : BigInt, es : Int)  = {
        var ps : Int = 0
        var bp: BigInt = 0
        var aux: BigInt = 0
        var ri: BigInt = 0
        var rn : Int = 0
        var sem: Int = 0
        var k: Int = 0
        var rs: Int = 0
        var ers: Int = 0
        var frs: Int = 0
        var e: BigInt = 0
        var f: BigInt = 0

        ps = this.size
        if((a & ((1<<(ps-1))-1)) == 0) {
             this.special_number = 1
        } else {
             this.special_number = 0
        }
        
        this.sign = ((a >> (ps-1)) & 1).intValue

        if(this.sign > 0) {
            bp = (~a & ((1<<(ps-1))-1)) + 1
        } else {
            bp = a & ((1<<(ps-1))-1)
        }

        ri = (bp >> (ps -2)) & 1
        rn = 1
        for (index <- 0 to (ps-3)) {
            aux = (bp >> (ps-3-index)) & 1
            if(aux == ri && sem == 0) {
                rn = rn + 1
            } else {
                sem = 1
            }
        }
        if(ri == 0) {
            k = -rn
        } else {
            k = rn - 1
        }

        rs = rn + 1
        ers = scala.math.max(0, scala.math.min(es, ps-rs-1))
        frs = scala.math.max(0, ps-rs-es-1)
        if(ers == 0) {
            e = 0
        } else {
            e = ((bp & ((1<<(ps-1-rs))-1)) >> (ps-rs-ers-1)) << (es-ers)
        }

        if(frs == 0) {
            f = 0
        } else {
            f = (bp & ((1<<(ps-rs-es))-1)) 
        }

        f = f | (1<<frs)
        
        this.regime = k
        this.exponent = e
        this.exponent_size = es
        this.fraction = f
        this.fraction_size = frs
        this.b_m = 0
    }
}

/*
POSIT - ENCODE/DECODE
*/

class DanTester(dut : EncodePosit) extends PeekPokeTester(dut) {
    var my_posit: TestPosit = new TestPosit(8)
    var aux: BigInt = 0;
    var max_exponent_size: Int = 3
    var bigValue : BigInt = 0
    

    /*
    var value: Int = 129
    bigValue = new BigInteger(value.toString())
    my_posit = PositTestDecodeEncode.newDecode(bigValue, 8, max_exponent_size)
    println("int value:" + value.toString() + "int value is is: " +  my_posit.toBigInt(32) + " for:" + my_posit.toDouble())
    
    
    for (value <- 0 until  256) {
        bigValue = new BigInteger(value.toString())
        my_posit = PositTestDecodeEncode.newDecode(bigValue, 8, max_exponent_size)
        println("int value:" + value.toString() + "int value is is: " +  my_posit.toBigInt(32) + " for:" + my_posit.toDouble())
    }
    */
    
    
    /*
    var value: Int = 12
    bigValue = new BigInteger(value.toString())
    my_posit.fromBigInt(bigValue, 32, false, max_exponent_size)
    println(my_posit.displayValue())
    aux = my_posit.binaryEncode()
    my_posit = PositTestDecodeEncode.newDecode(aux, 8, max_exponent_size)
    println(my_posit.displayValue())
    println("int value:" + value.toString() + "int value is is: " +  my_posit.toBigInt(32) + " for:" + my_posit.toDouble() + " binary:" + aux.toString(2))
    */
    /*
    for (value <- 0 until  256) {
        bigValue = new BigInteger(value.toString())
        my_posit.fromBigInt(bigValue, 32, false, max_exponent_size)
        aux = my_posit.binaryEncode()
        my_posit = PositTestDecodeEncode.newDecode(aux, 8, max_exponent_size)
        println("int value:" + value.toString() + "int value is is: " +  my_posit.toBigInt(32) + " for:" + my_posit.toDouble())
    }
    */
    
    for (value <- 0 until  256) {
        bigValue = new BigInteger(value.toString())
        my_posit.decodeBinary(bigValue, max_exponent_size)
        println("int value:" + value.toString() + "int value is is: " +  my_posit.toBigInt(32) + " for:" + my_posit.toDouble())
    }


}

object DanTester extends App {
    chisel3.iotesters.Driver(() => new EncodePosit(32)) { c => new DanTester(c) }
}

/*
POSIT - ADD/SUB
*/

class AdderSubtractorTester(dut : PositAdderSubtractorTester) extends PeekPokeTester(dut) {
    var i_posit_1: TestPosit = new TestPosit(32)
    var i_posit_2: TestPosit = new TestPosit(32)
    var o_posit: TestPosit = new TestPosit(32)
    var aux: BigInt = 0;
    var max_exponent_size: Int = 3
    var bigValue_1 : BigInt = 0
    var bigValue_2 : BigInt = 0
     //poke(dut.io.i_bits, "b1_0000_000_0000_0000_0000_0000_0000_0001".asUInt(32.W))
    /*
    var value_1: Int = 1
    var value_2: Int = 5
    bigValue_1 = new BigInteger(value_1.toString())
    i_posit_1.decodeBinary(bigValue_1, max_exponent_size)
    bigValue_2 = new BigInteger(value_2.toString())
    i_posit_2.decodeBinary(bigValue_2, max_exponent_size)
    poke(dut.io.i_posit_1, bigValue_1)
    poke(dut.io.i_posit_2, bigValue_2)
    poke(dut.io.i_op, 1)
    poke(dut.io.i_es, max_exponent_size)
    step(1)
    o_posit = i_posit_1 - i_posit_2
    println(i_posit_1.displayValue())
    println(i_posit_2.displayValue())
    println("Regim is: " + peek(dut.io.debug_regime_2).toString)
    println("exponent is: " + peek(dut.io.debug_exponent_2).toString(2))
    println("fraction is: " + peek(dut.io.debug_fraction_2).toString(2))
    println("Regim is: " + peek(dut.io.debug_regime).toString)
    println("exponent is: " + peek(dut.io.debug_exponent).toString(2))
    println("fraction is: " + peek(dut.io.debug_fraction).toString(2))
    println("debug_b_nplus1 size is: " + peek(dut.io.debug_b_nplus1).toString)
    println("debug_b_m size is: " + peek(dut.io.debug_b_m).toString)
    println("debug_addOne size is: " + peek(dut.io.debug_addOne).toString)
    println("debug_fs size is: " + peek(dut.io.debug_fs).toString)
    println("fraction is: " + peek(dut.io.debug_fraction_3).toString(2))
    println("debug_l_t size is: " + peek(dut.io.debug_l_t).toString)
    println(o_posit.displayValue())
    expect(dut.io.o_posit, o_posit.binaryEncode())
    println(o_posit.displayValue())
    
    */
    for (value_1 <- 0 until  256) {
        bigValue_1 = new BigInteger(value_1.toString())
        i_posit_1.decodeBinary(bigValue_1, max_exponent_size)
        for (value_2 <- 0 until  256) {
            bigValue_2 = new BigInteger(value_2.toString())
            i_posit_2.decodeBinary(bigValue_2, max_exponent_size)
            poke(dut.io.i_posit_1, bigValue_1)
            poke(dut.io.i_posit_2, bigValue_2)
            poke(dut.io.i_op, 0)
            poke(dut.io.i_es, max_exponent_size)
            step(1)
            o_posit = i_posit_1 + i_posit_2
            expect(dut.io.o_posit, o_posit.binaryEncode())
        }
    }
    //*/


}

object AdderSubtractorTester extends App {
    chisel3.iotesters.Driver(() => new PositAdderSubtractorTester(32)) { c => new AdderSubtractorTester(c) }
}

/*
POSIT - MUL
*/

class MultiplierTester(dut : PositMultiplierTester) extends PeekPokeTester(dut) {
    var i_posit_1: TestPosit = new TestPosit(32)
    var i_posit_2: TestPosit = new TestPosit(32)
    var o_posit: TestPosit = new TestPosit(32)
    var aux: BigInt = 0;
    var max_exponent_size: Int = 3
    var bigValue_1 : BigInt = 0
    var bigValue_2 : BigInt = 0
     //poke(dut.io.i_bits, "b1_0000_000_0000_0000_0000_0000_0000_0001".asUInt(32.W))
    /*
    var value_1: Int = 5
    var value_2: Int = 91
    bigValue_1 = new BigInteger(value_1.toString())
    i_posit_1.decodeBinary(bigValue_1, max_exponent_size)
    bigValue_2 = new BigInteger(value_2.toString())
    i_posit_2.decodeBinary(bigValue_2, max_exponent_size)
    poke(dut.io.i_posit_1, bigValue_1)
    poke(dut.io.i_posit_2, bigValue_2)
    poke(dut.io.i_es, max_exponent_size)
    step(1)
    o_posit = i_posit_1 * i_posit_2
    println("Regim is: " + peek(dut.io.debug_regime_2).toString)
    println("exponent is: " + peek(dut.io.debug_exponent_2).toString(2))
    println("fraction is: " + peek(dut.io.debug_fraction_2).toString(2))
    println("Regim is: " + peek(dut.io.debug_regime).toString)
    println("exponent is: " + peek(dut.io.debug_exponent).toString(2))
    println("fraction is: " + peek(dut.io.debug_fraction).toString(2))
    println("debug_b_nplus1 size is: " + peek(dut.io.debug_b_nplus1).toString)
    println("debug_b_m size is: " + peek(dut.io.debug_b_m).toString)
    println("debug_addOne size is: " + peek(dut.io.debug_addOne).toString)
    println("debug_fs size is: " + peek(dut.io.debug_fs).toString)
    println("fraction is: " + peek(dut.io.debug_fraction_3).toString(2))
    println(o_posit.displayValue())
    expect(dut.io.o_posit, o_posit.binaryEncode())
    println(o_posit.displayValue())
    
    */
    for (value_1 <- 0 until  256) {
        bigValue_1 = new BigInteger(value_1.toString())
        i_posit_1.decodeBinary(bigValue_1, max_exponent_size)
        for (value_2 <- 0 until  256) {
            bigValue_2 = new BigInteger(value_2.toString())
            i_posit_2.decodeBinary(bigValue_2, max_exponent_size)
            poke(dut.io.i_posit_1, bigValue_1)
            poke(dut.io.i_posit_2, bigValue_2)
            poke(dut.io.i_es, max_exponent_size)
            step(1)
            o_posit = i_posit_1 * i_posit_2
            expect(dut.io.o_posit, o_posit.binaryEncode())
        }
    }
    //*/


}

object MultiplierTester extends App {
    chisel3.iotesters.Driver(() => new PositMultiplierTester(32)) { c => new MultiplierTester(c) }
}


/*
POSIT - DIV
*/

class DividerTester(dut : PositDividerTester) extends PeekPokeTester(dut) {
    var i_posit_1: TestPosit = new TestPosit(32)
    var i_posit_2: TestPosit = new TestPosit(32)
    var o_posit: TestPosit = new TestPosit(32)
    var aux: BigInt = 0;
    var max_exponent_size: Int = 3
    var bigValue_1 : BigInt = 0
    var bigValue_2 : BigInt = 0
     //poke(dut.io.i_bits, "b1_0000_000_0000_0000_0000_0000_0000_0001".asUInt(32.W))
    /*
    var value_1: Int = 1
    var value_2: Int = 1
    bigValue_1 = new BigInteger(value_1.toString())
    i_posit_1.decodeBinary(bigValue_1, max_exponent_size)
    bigValue_2 = new BigInteger(value_2.toString())
    i_posit_2.decodeBinary(bigValue_2, max_exponent_size)
    poke(dut.io.i_posit_1, bigValue_1)
    poke(dut.io.i_posit_2, bigValue_2)
    poke(dut.io.i_es, max_exponent_size)
    step(1)
    o_posit = i_posit_1 / i_posit_2
    println("Regim is: " + peek(dut.io.debug_regime_2).toString)
    println("exponent is: " + peek(dut.io.debug_exponent_2).toString(2))
    println("fraction is: " + peek(dut.io.debug_fraction_2).toString(2))
    println("Regim is: " + peek(dut.io.debug_regime).toString)
    println("exponent is: " + peek(dut.io.debug_exponent).toString(2))
    println("fraction is: " + peek(dut.io.debug_fraction).toString(2))
    println("debug_b_nplus1 size is: " + peek(dut.io.debug_b_nplus1).toString)
    println("debug_b_m size is: " + peek(dut.io.debug_b_m).toString)
    println("debug_addOne size is: " + peek(dut.io.debug_addOne).toString)
    println("debug_fs size is: " + peek(dut.io.debug_fs).toString)
    println("fraction is: " + peek(dut.io.debug_fraction_3).toString(2))
    println(o_posit.displayValue())
    expect(dut.io.o_posit, o_posit.binaryEncode())
    println(o_posit.displayValue())
    
    */
    for (value_1 <- 0 until  256) {
        bigValue_1 = new BigInteger(value_1.toString())
        i_posit_1.decodeBinary(bigValue_1, max_exponent_size)
        for (value_2 <- 0 until  256) {
            bigValue_2 = new BigInteger(value_2.toString())
            i_posit_2.decodeBinary(bigValue_2, max_exponent_size)
            poke(dut.io.i_posit_1, bigValue_1)
            poke(dut.io.i_posit_2, bigValue_2)
            poke(dut.io.i_es, max_exponent_size)
            step(1)
            o_posit = i_posit_1 / i_posit_2
            expect(dut.io.o_posit, o_posit.binaryEncode())
        }
    }
    //*/


}

object DividerTester extends App {
    chisel3.iotesters.Driver(() => new PositDividerTester(32)) { c => new DividerTester(c) }
}




/*
POSIT INT CONVERSION
*/


class PositToIntTester(dut : PositPositToIntTester) extends PeekPokeTester(dut) {
    var i_posit: TestPosit = new TestPosit(32)
    var aux: BigInt = 0;
    var max_exponent_size: Int = 3
    var bigValue : BigInt = 0
    var integer_size : Int = 32
     //poke(dut.io.i_bits, "b1_0000_000_0000_0000_0000_0000_0000_0001".asUInt(32.W))
    /*
    var value: Int = 129
    bigValue = new BigInteger(value.toString())
    i_posit.decodeBinary(bigValue, max_exponent_size)
    poke(dut.io.i_posit, bigValue)
    poke(dut.io.i_es, max_exponent_size)
    step(1)
    println("Regim is: " + peek(dut.io.debug_regime).toString)
    println("exponent is: " + peek(dut.io.debug_exponent).toString(2))
    println("fraction is: " + peek(dut.io.debug_fraction).toString(2))
    println("debug_fs size is: " + peek(dut.io.debug_fs).toString)
    println("o_integer size is: " + peek(dut.io.o_integer).toString(2))
    println("i_posit.toBigInt(integer_size) size is: " + i_posit.toBigInt(integer_size).toString(2))
    expect(dut.io.o_integer, i_posit.toBigInt(integer_size))
    println(i_posit.displayValue())
    
    */
    for (value <- 0 until  65535) {
        bigValue = new BigInteger(value.toString())
        i_posit.decodeBinary(bigValue, max_exponent_size)
        poke(dut.io.i_posit, bigValue)
        poke(dut.io.i_es, max_exponent_size)
        step(1)
        expect(dut.io.o_integer, i_posit.toBigInt(integer_size))
    }
    //*/


}

object PositToIntTester extends App {
    chisel3.iotesters.Driver(() => new PositPositToIntTester(32,32)) { c => new PositToIntTester(c) }
}


class IntToPositTester(dut : PositIntToPositTester ) extends PeekPokeTester(dut) {
    var i_posit: TestPosit = new TestPosit(32)
    var aux: BigInt = 0;
    var max_exponent_size: Int = 3
    var bigValue : BigInt = 0
    var integer_size : Int = 32
     //poke(dut.io.i_bits, "b1_0000_000_0000_0000_0000_0000_0000_0001".asUInt(32.W))
    /*
    var value: Int = 129
    bigValue = new BigInteger(value.toString())
    i_posit.decodeBinary(bigValue, max_exponent_size)
    poke(dut.io.i_posit, bigValue)
    poke(dut.io.i_es, max_exponent_size)
    step(1)
    println("Regim is: " + peek(dut.io.debug_regime).toString)
    println("exponent is: " + peek(dut.io.debug_exponent).toString(2))
    println("fraction is: " + peek(dut.io.debug_fraction).toString(2))
    println("debug_fs size is: " + peek(dut.io.debug_fs).toString)
    println("o_integer size is: " + peek(dut.io.o_integer).toString(2))
    println("i_posit.toBigInt(integer_size) size is: " + i_posit.toBigInt(integer_size).toString(2))
    expect(dut.io.o_integer, i_posit.toBigInt(integer_size))
    println(i_posit.displayValue())
    
    */
    for (value <- 0 until  65535) {
        bigValue = new BigInteger(value.toString())
        i_posit.fromBigInt(bigValue, integer_size, max_exponent_size)
        poke(dut.io.i_integer, bigValue)
        poke(dut.io.i_es, max_exponent_size)
        step(1)
        expect(dut.io.o_posit, i_posit.binaryEncode())
    }
    //*/


}

object IntToPositTester extends App {
    chisel3.iotesters.Driver(() => new PositIntToPositTester (32,32)) { c => new IntToPositTester(c) }
}


/*
POSIT CMP
*/

class TesterLPosit(dut : PositL) extends PeekPokeTester(dut) {
     var i_posit_1: TestPosit = new TestPosit(32)
    var i_posit_2: TestPosit = new TestPosit(32)
    var o_result: Boolean = false
    var aux: BigInt = 0;
    var max_exponent_size: Int = 3
    var bigValue_1 : BigInt = 0
    var bigValue_2 : BigInt = 0
     //poke(dut.io.i_bits, "b1_0000_000_0000_0000_0000_0000_0000_0001".asUInt(32.W))
    /*
    var value_1: Int = 128
    var value_2: Int = 130
    bigValue_1 = new BigInteger(value_1.toString())
    i_posit_1.decodeBinary(bigValue_1, max_exponent_size)
    bigValue_2 = new BigInteger(value_2.toString())
    i_posit_2.decodeBinary(bigValue_2, max_exponent_size)
    poke(dut.io.i_bits_1, bigValue_1)
    poke(dut.io.i_bits_2, bigValue_2)
    step(1)
    o_result = (i_posit_1 < i_posit_2)
    expect(dut.io.o_result, o_result)
    println(i_posit_1.displayValue())
    println(i_posit_2.displayValue())
    
    */
    for (value_1 <- 0 until  256) {
        bigValue_1 = new BigInteger(value_1.toString())
        i_posit_1.decodeBinary(bigValue_1, max_exponent_size)
        for (value_2 <- 0 until  256) {
            bigValue_2 = new BigInteger(value_2.toString())
            i_posit_2.decodeBinary(bigValue_2, max_exponent_size)
            poke(dut.io.i_bits_1, bigValue_1)
            poke(dut.io.i_bits_2, bigValue_2)
            step(1)
            o_result = i_posit_1 < i_posit_2
            expect(dut.io.o_result, o_result)
        }
    }
    //*/
}

object TesterLPosit extends App {
    chisel3.iotesters.Driver(() => new PositL(32)) { c => new TesterLPosit(c) }
}


class TesterEPosit(dut : PositE) extends PeekPokeTester(dut) {
    var i_posit_1: TestPosit = new TestPosit(32)
    var i_posit_2: TestPosit = new TestPosit(32)
    var o_result: Boolean = false
    var aux: BigInt = 0;
    var max_exponent_size: Int = 3
    var bigValue_1 : BigInt = 0
    var bigValue_2 : BigInt = 0
     //poke(dut.io.i_bits, "b1_0000_000_0000_0000_0000_0000_0000_0001".asUInt(32.W))
    /*
    var value_1: Int = 128
    var value_2: Int = 130
    bigValue_1 = new BigInteger(value_1.toString())
    i_posit_1.decodeBinary(bigValue_1, max_exponent_size)
    bigValue_2 = new BigInteger(value_2.toString())
    i_posit_2.decodeBinary(bigValue_2, max_exponent_size)
    poke(dut.io.i_bits_1, bigValue_1)
    poke(dut.io.i_bits_2, bigValue_2)
    step(1)
    o_result = (i_posit_1 == i_posit_2)
    expect(dut.io.o_result, o_result)
    println(i_posit_1.displayValue())
    println(i_posit_2.displayValue())
    
    
    */
    for (value_1 <- 0 until  256) {
        bigValue_1 = new BigInteger(value_1.toString())
        i_posit_1.decodeBinary(bigValue_1, max_exponent_size)
        for (value_2 <- 0 until  256) {
            bigValue_2 = new BigInteger(value_2.toString())
            i_posit_2.decodeBinary(bigValue_2, max_exponent_size)
            poke(dut.io.i_bits_1, bigValue_1)
            poke(dut.io.i_bits_2, bigValue_2)
            step(1)
            o_result = i_posit_1 == i_posit_2
            expect(dut.io.o_result, o_result)
        }
    }
    //*/
}

object TesterEPosit extends App {
    chisel3.iotesters.Driver(() => new PositE(32)) { c => new TesterEPosit(c) }
}



class PositSGNJTester(dut : PositSGNJ) extends PeekPokeTester(dut) {
    var i_posit_1: TestPosit = new TestPosit(32)
    var i_posit_2: TestPosit = new TestPosit(32)
    var o_posit: TestPosit = new TestPosit(32)
    var aux: BigInt = 0;
    var max_exponent_size: Int = 3
    var bigValue_1 : BigInt = 0
    var bigValue_2 : BigInt = 0
     //poke(dut.io.i_bits, "b1_0000_000_0000_0000_0000_0000_0000_0001".asUInt(32.W))
    /*
    var value_1: Int = 0
    var value_2: Int = 0
    bigValue_1 = new BigInteger(value_1.toString())
    i_posit_1.decodeBinary(bigValue_1, max_exponent_size)
    bigValue_2 = new BigInteger(value_2.toString())
    i_posit_2.decodeBinary(bigValue_2, max_exponent_size)
    poke(dut.io.i_bits_1, bigValue_1)
    poke(dut.io.i_bits_2, bigValue_2)
    poke(dut.io.i_op, 1)
    step(1)
    o_posit = i_posit_1.sgnj(i_posit_2, 1)
    expect(dut.io.o_bits,  o_posit.binaryEncode())
    println(i_posit_1.displayValue())
    println(i_posit_2.displayValue())
    println(o_posit.displayValue())
    
    
    */
    for (value_1 <- 0 until  256) {
        bigValue_1 = new BigInteger(value_1.toString())
        i_posit_1.decodeBinary(bigValue_1, max_exponent_size)
        for (value_2 <- 0 until  256) {
            bigValue_2 = new BigInteger(value_2.toString())
            i_posit_2.decodeBinary(bigValue_2, max_exponent_size)
            poke(dut.io.i_bits_1, bigValue_1)
            poke(dut.io.i_bits_2, bigValue_2)
            poke(dut.io.i_op, 1)
            step(1)
            o_posit = i_posit_1.sgnj(i_posit_2, 1)
            expect(dut.io.o_bits, o_posit.binaryEncode())
        }
    }
    //*/
}

object PositSGNJTester extends App {
    chisel3.iotesters.Driver(() => new PositSGNJ(32)) { c => new PositSGNJTester(c) }
}


class PositMinMaxTester(dut : PositMinMax) extends PeekPokeTester(dut) {
    var i_posit_1: TestPosit = new TestPosit(32)
    var i_posit_2: TestPosit = new TestPosit(32)
    var o_posit: TestPosit = new TestPosit(32)
    var aux: BigInt = 0;
    var max_exponent_size: Int = 3
    var bigValue_1 : BigInt = 0
    var bigValue_2 : BigInt = 0
     //poke(dut.io.i_bits, "b1_0000_000_0000_0000_0000_0000_0000_0001".asUInt(32.W))
    /*
    var value_1: Int = 128
    var value_2: Int = 130
    bigValue_1 = new BigInteger(value_1.toString())
    i_posit_1.decodeBinary(bigValue_1, max_exponent_size)
    bigValue_2 = new BigInteger(value_2.toString())
    i_posit_2.decodeBinary(bigValue_2, max_exponent_size)
    poke(dut.io.i_bits_1, bigValue_1)
    poke(dut.io.i_bits_2, bigValue_2)
    poke(dut.io.i_op, 0)
    step(1)
    o_posit = i_posit_1.sgnj(i_posit_2, 0)
    expect(dut.io.o_result, o_result)
    println(i_posit_1.displayValue())
    println(i_posit_2.displayValue())
    
    
    */
    for (value_1 <- 0 until  256) {
        bigValue_1 = new BigInteger(value_1.toString())
        i_posit_1.decodeBinary(bigValue_1, max_exponent_size)
        for (value_2 <- 0 until  256) {
            bigValue_2 = new BigInteger(value_2.toString())
            i_posit_2.decodeBinary(bigValue_2, max_exponent_size)
            poke(dut.io.i_bits_1, bigValue_1)
            poke(dut.io.i_bits_2, bigValue_2)
            poke(dut.io.i_op, 1)
            step(1)
            o_posit = i_posit_1.max(i_posit_2)
            expect(dut.io.o_bits, o_posit.binaryEncode())
        }
    }
    //*/
}

object PositMinMaxTester extends App {
    chisel3.iotesters.Driver(() => new PositMinMax(32)) { c => new PositMinMaxTester(c) }
}


class SQRTTester(dut : PositSQRTTester) extends PeekPokeTester(dut) {
    var i_posit: TestPosit = new TestPosit(32)
    var o_posit: TestPosit = new TestPosit(32)
    var aux: BigInt = 0;
    var max_exponent_size: Int = 3
    var bigValue : BigInt = 0
     //poke(dut.io.i_bits, "b1_0000_000_0000_0000_0000_0000_0000_0001".asUInt(32.W))
    /*
    var value: Int = 17
    bigValue = new BigInteger(value.toString())
    i_posit.decodeBinary(bigValue, max_exponent_size)
    poke(dut.io.i_posit, bigValue)
    poke(dut.io.i_es, max_exponent_size)
    poke(dut.io.i_ready, 1)
    step(1)
    poke(dut.io.i_ready, 0)
    step(4)
    println("Regim is: " + peek(dut.io.debug_regime).toString)
    println("exponent is: " + peek(dut.io.debug_exponent).toString(2))
    println("fraction is: " + peek(dut.io.debug_fraction).toString(2))
    println("debug_b_nplus1 size is: " + peek(dut.io.debug_b_nplus1).toString)
    println("debug_b_m size is: " + peek(dut.io.debug_b_m).toString)
    println("debug_addOne size is: " + peek(dut.io.debug_addOne).toString)
    println("debug_fs size is: " + peek(dut.io.debug_fs).toString)
    o_posit = i_posit.sqrt()
    expect(dut.io.o_posit, o_posit.binaryEncode())
    println(i_posit.displayValue())
    println(o_posit.displayValue())
    
    
    */
    for (value <- 0 until  65535) {
        bigValue = new BigInteger(value.toString())
        i_posit.decodeBinary(bigValue, max_exponent_size)
        poke(dut.io.i_posit, bigValue)
        poke(dut.io.i_es, max_exponent_size)
        poke(dut.io.i_ready, 1)
        step(1)
        poke(dut.io.i_ready, 0)
        step(16)
        o_posit = i_posit.sqrt()
        expect(dut.io.o_posit, o_posit.binaryEncode())
        expect(dut.io.o_ready, 1)
    }
    //*/
}

object SQRTTester extends App {
    chisel3.iotesters.Driver(() => new PositSQRTTester(32)) { c => new SQRTTester(c) }
}



class DisplayValueTester(dut : PositSQRTTester) extends PeekPokeTester(dut) {
    var i_posit: TestPosit = new TestPosit(32)
    var max_exponent_size: Int = 3
    var aux: Double = 0;

    var jindex: BigInt = new BigInteger("4e800000", 16)
    i_posit.decodeBinary(jindex, max_exponent_size)
    println("jindex is: " + jindex.toString)
    println("aux is: " + i_posit.toDouble())
    println(i_posit.displayValue())
    //*/
}

object DisplayValueTester extends App {
    chisel3.iotesters.Driver(() => new PositSQRTTester(8)) { c => new DisplayValueTester(c) }
}