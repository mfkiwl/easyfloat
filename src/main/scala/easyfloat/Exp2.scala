package easyfloat

import circt.stage.ChiselStage
import chisel3._
import chisel3.util._


class RawFloat_SplitIF(expWidth: Int, mantissaWidth: Int, nMSBs: Int) extends Module {
  val io = IO(new Bundle {
    val in = Input(new RawFloat(expWidth, mantissaWidth))
    val outInt = Output(SInt(expWidth.W))
    val outFracMSBs = Output(UInt(nMSBs.W))
    val outRawFloat = Output(new RawFloat(expWidth, mantissaWidth))
  })

  val rawIn = io.in
  io.outRawFloat.sign := rawIn.sign
  io.outRawFloat.isZero := rawIn.isZero
  io.outRawFloat.isNaN := rawIn.isNaN
  io.outRawFloat.isInf := rawIn.isInf

  when(rawIn.exp < 0.S) {
    // the integer part is already zero
    val rightShiftAmt = (-1).S - rawIn.exp
    val fracAlignedToMinusOne = rawIn.mantissa >> rightShiftAmt.take(nMSBs.U.getWidth)
    io.outInt := 0.S
    io.outRawFloat.exp := rawIn.exp
    io.outRawFloat.mantissa := rawIn.mantissa
    io.outFracMSBs := Mux(rightShiftAmt < nMSBs.S, fracAlignedToMinusOne.head(nMSBs), 0.U)
  }.otherwise({
    // exp >= 0
    val exp = rawIn.exp.tail(1)
    /*
      rawIn:
      1.xxxx * 2^n  -> 1.xxx.x00 * 2^0
    */
    val intBits = expWidth - 1
    // we already have 1 integer bit
    val maxShift = intBits - 1
    val shiftedMantissa = rawIn.mantissa << exp.take(maxShift.U.getWidth)
    val xi = shiftedMantissa.head(intBits)
    val xf = shiftedMantissa.tail(intBits)
    require(xf.getWidth == mantissaWidth - 1)
    when(exp > maxShift.U) {
      io.outInt := 0.S
      io.outRawFloat.exp := rawIn.exp
      io.outRawFloat.mantissa := rawIn.mantissa
      io.outRawFloat.isInf := true.B
      io.outFracMSBs := 0.U
    }.otherwise({
      io.outInt := Mux(rawIn.sign, -xi.zext, xi.zext)
      io.outFracMSBs := xf.head(nMSBs)
      when(xf === 0.U) {
        io.outRawFloat.exp := 0.S
        io.outRawFloat.mantissa := 0.U
        io.outRawFloat.isZero := true.B
      }.otherwise({
        val lzc = PriorityEncoder((0.U(1.W) ## xf).asBools.reverse)
        io.outRawFloat.exp := -lzc.zext
        io.outRawFloat.mantissa := (xf << lzc).take(mantissaWidth)
      })
    })
  })
}

class RawFloat_MulAddExp2
(
  mulExpWidth: Int, mulMantissaWidth: Int,
  addExpWidth: Int, addMantissaWidth: Int
) extends Module {

  val fma = Module(new RawFloat_FMA(
    aEW = mulExpWidth, aMW = mulMantissaWidth,
    bEW = mulExpWidth, bMW = mulMantissaWidth,
    cEW = addExpWidth, cMW = addMantissaWidth
  ))

  val io = IO(new Bundle {
    val in_exp2 = Input(Bool())
    val in_a = Input(new RawFloat(mulExpWidth, mulMantissaWidth))
    val in_b = Input(new RawFloat(mulExpWidth, mulMantissaWidth))
    val in_c = Input(new RawFloat(addExpWidth, addMantissaWidth))
    val out = Output(new RawFloat(fma.outEW, addMantissaWidth + 2))
  })

  // TODO: do not hard code it
  val slopes = VecInit(Seq(
    0x3eb95c1e,
    0x3eca22e7,
    0x3edc6e66,
    0x3ef061c9,
    0x3f0311b7,
    0x3f0eee96,
    0x3f1bde51,
    0x3f29f9c9
  ).map(_.U(32.W)).reverse)
  val intercepts = VecInit(Seq(
    0x3f5cae0f,
    0x3f640507,
    0x3f6ae156,
    0x3f711d65,
    0x3f768dcf,
    0x3f7b00a2,
    0x3f7e3c91,
    0x3f800000
  ).map(_.U(32.W)).reverse)

  val split = Module(new RawFloat_SplitIF(mulExpWidth, mulMantissaWidth, 3))

  split.io.in := io.in_a
  io.out := fma.io.out
  when(io.in_exp2) {
    fma.io.a := split.io.outRawFloat
    fma.io.b.fromIEEE(slopes(split.io.outFracMSBs), mulExpWidth - 1, mulMantissaWidth - 1)
    fma.io.c.fromIEEE(intercepts(split.io.outFracMSBs), addExpWidth - 1, addMantissaWidth - 1)
    io.out.exp := split.io.outInt + fma.io.out.exp
    io.out.sign := false.B
    io.out.isInf := false.B
    io.out.isZero := fma.io.out.isInf
  }.otherwise({
    fma.io.a := io.in_a
    fma.io.b := io.in_b
    fma.io.c := io.in_c
  })
}

class MulAddExp2(expWidth: Int, mantissaWidth: Int) extends Module {
  val io = IO(new Bundle {
    val in_exp2 = Input(Bool())
    val in_a = Input(UInt((1 + expWidth + mantissaWidth).W))
    val in_b = Input(UInt((1 + expWidth + mantissaWidth).W))
    val in_c = Input(UInt((1 + expWidth + mantissaWidth).W))
    val out = Output(UInt((1 + expWidth + mantissaWidth).W))
  })
  val raw = Module(new RawFloat_MulAddExp2(
    expWidth + 1, mantissaWidth + 1,
    expWidth + 1, mantissaWidth + 1,
  ))
  raw.io.in_exp2 := io.in_exp2
  raw.io.in_a.fromIEEE(io.in_a, expWidth, mantissaWidth)
  raw.io.in_b.fromIEEE(io.in_b, expWidth, mantissaWidth)
  raw.io.in_c.fromIEEE(io.in_c, expWidth, mantissaWidth)
  io.out := Rounding.round(raw.io.out, RoundingMode.RNE, expWidth, mantissaWidth)
}

object MulAddExp2 {
  def main(args: Array[String]): Unit = {
    ChiselStage.emitSystemVerilogFile(new MulAddExp2(8, 23))
  }
}

