package easyfloat

import circt.stage.ChiselStage
import chisel3._
import chisel3.util._

class RawFloat_Div(expWidth: Int, mantissaWidth: Int, bitsPerCycle: Int = 2) extends Module {

  val io = IO(new Bundle {
    // The module itself does not hold `a` and `b`
    val in = Flipped(Irrevocable(new Bundle {
        val a = new RawFloat(expWidth, mantissaWidth)
        val b = new RawFloat(expWidth, mantissaWidth)
    }))
    val out = DecoupledIO(new RawFloat(expWidth + 1, mantissaWidth + 2))
  })

  val iterCycles = Div.iterCycles(mantissaWidth - 1, bitsPerCycle)
  val quotientBits = iterCycles * bitsPerCycle

  val quotient = Reg(UInt(quotientBits.W))
  val reminder = Reg(UInt((1 + mantissaWidth).W))

  val s_idle :: s_iter :: s_done :: Nil = Enum(3)
  val state = RegInit(s_idle)
  val (iterCount, iterDone) = Counter(state === s_iter, iterCycles)

  val divisor = io.in.bits.b.mantissa
  // (rem, divisor) -> (remNext, quotientNext)
  def div_iter(rem: UInt, quot: UInt): (UInt, UInt) = {
    val q = rem >= divisor
    val remNext = (Mux(q, rem - divisor, rem) << 1).tail(1)
    val quotientNext = (quot ## q).tail(1)
    (remNext, quotientNext)
  }

  val (remNext, quotientNext) = (0 until bitsPerCycle).foldLeft((reminder, quotient)) {
    case ((rem, quot), _) => div_iter(rem, quot)
  }

  switch(state) {
    is(s_idle) {
      when(io.in.valid) {
        state := s_iter
        quotient := 0.U
        reminder := io.in.bits.a.mantissa
      }
    }
    is(s_iter) {
      when(iterDone) {
        state := s_done
      }
      reminder := remNext
      quotient := quotientNext
    }
    is(s_done) {
      when(io.out.ready) {
        state := s_idle
      }
    }
  }

  io.in.ready := state === s_idle
  io.out.valid := state === s_done

  val quotMSB = quotient.head(1).asBool
  val adjustedQuotient = Mux(quotMSB, quotient, quotient.tail(1) ## 0.U(1.W)) ## reminder.orR

  io.out.bits.isNaN := io.in.bits.a.isNaN || io.in.bits.b.isNaN || (io.in.bits.a.isInf && io.in.bits.b.isInf)
  io.out.bits.isInf := !io.in.bits.a.isZero && io.in.bits.b.isZero
  io.out.bits.isZero := io.in.bits.a.isZero || io.in.bits.b.isInf
  io.out.bits.sign := io.in.bits.a.sign ^ io.in.bits.b.sign
  io.out.bits.exp := (io.in.bits.a.exp -& io.in.bits.b.exp) - Mux(quotMSB, 0.S, 1.S)
  if (adjustedQuotient.getWidth > io.out.bits.mantissa.getWidth) {
    io.out.bits.mantissa := adjustedQuotient.head(io.out.bits.mantissa.getWidth - 1) ##
        adjustedQuotient.tail(io.out.bits.mantissa.getWidth - 1).orR
  } else if (adjustedQuotient.getWidth < io.out.bits.mantissa.getWidth) {
    io.out.bits.mantissa := adjustedQuotient ##
        0.U((io.out.bits.mantissa.getWidth - adjustedQuotient.getWidth).W)
  } else {
    io.out.bits.mantissa := adjustedQuotient
  }
  io.out.valid := state === s_done

}

class Div(expWidth: Int, mantissaWidth: Int, bitsPerCycle: Int = 2) extends Module {
    val io = IO(new Bundle {
        val in_a = Input(UInt((1 + expWidth + mantissaWidth).W))
        val in_b = Input(UInt((1 + expWidth + mantissaWidth).W))
        val in_valid = Input(Bool())
        val out = Output(UInt((1 + expWidth + mantissaWidth).W))
        val out_valid = Output(Bool())
    })
    val rawDiv = Module(new RawFloat_Div(1 + expWidth, 1 + mantissaWidth, bitsPerCycle))
    rawDiv.io.in.bits.a.fromIEEE(io.in_a, expWidth, mantissaWidth)
    rawDiv.io.in.bits.b.fromIEEE(io.in_b, expWidth, mantissaWidth)
    rawDiv.io.in.valid := io.in_valid
    rawDiv.io.out.ready := true.B
    io.out := Rounding.round(rawDiv.io.out.bits, RoundingMode.RNE, expWidth, mantissaWidth)
    io.out_valid := rawDiv.io.out.valid
}


object Div {
  def iterCycles(ieeeMantissaWidth: Int, bitsPerCycle: Int) = {
    (ieeeMantissaWidth + 3 + bitsPerCycle - 1) / bitsPerCycle
  }
  def nCycles(ieeeMantissaWidth: Int, bitsPerCycle: Int = 2) =
    iterCycles(ieeeMantissaWidth, bitsPerCycle) + 2

  def main(args: Array[String]): Unit = {
    ChiselStage.emitSystemVerilogFile(new Div(8, 23, 2))
  }
}