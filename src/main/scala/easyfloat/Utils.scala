package easyfloat

import java.io.File
import scala.sys.process._
import scala.util.{Failure, Success, Try}
import chisel3._
import chisel3.util._

/**
  * in => shift | collect sticky bit => {in_shifted, sticky}
  */
class ShiftRightJam(val len: Int) extends Module {
  val max_shift_width = log2Up(len + 1)
  val io = IO(new Bundle() {
    val in = Input(UInt(len.W))
    val shamt = Input(UInt())
    val out = Output(UInt(len.W))
    val sticky = Output(Bool())
  })
  val exceed_max_shift = io.shamt > len.U
  val shamt = io.shamt(max_shift_width - 1, 0)
  val sticky_mask =
    ((1.U << shamt).asUInt - 1.U)(len - 1, 0) | Fill(len, exceed_max_shift)
  io.out := Mux(exceed_max_shift, 0.U, io.in >> io.shamt)
  io.sticky := (io.in & sticky_mask).orR
}

object ShiftRightJam {
  def apply(x: UInt, shamt: UInt): (UInt, Bool) = {
    val shiftRightJam = Module(new ShiftRightJam(x.getWidth))
    shiftRightJam.io.in := x
    shiftRightJam.io.shamt := shamt
    (shiftRightJam.io.out, shiftRightJam.io.sticky)
  }
}

object PyFPConst {

  /*
  projectDir: when running easyfloat in standalone mode, it's '.'
              but when running it as a submodule in chipyard,
              the project dir should be changed to 'generators/easyfloat'
  */
  private def runScript(choice: String, ew: Int, mw: Int, projectDir: String = ".", otherArgs: Seq[String]= Nil): String = {
    val scriptPath = new File(projectDir, "fp_consts.py").getAbsolutePath
    val command = Seq(
      "uv", "run", "--project", projectDir, scriptPath,
      choice, "--ew", ew.toString, "--mw", mw.toString
    ) ++ otherArgs
    val output: Try[String] = Try(command.!!)
    output match {
      case Failure(exception) =>
        sys.error(exception.getMessage)
      case Success(stdout) =>
        stdout
    }
  }

  def attentionScale(ew: Int, mw: Int, dk: Int, projectDir: String = "."): BigInt = {
    val out = runScript("attentionScale", ew, mw, projectDir, Seq("--dk", dk.toString)).strip()
    BigInt(out, 16)
  }

  private def pwl(choice: String)(ew: Int, mw: Int, projectDir: String = ".", pieces: Int = 8): Seq[BigInt] = {
    runScript(choice, ew, mw, projectDir, Seq("--pwl-pieces", pieces.toString)).split("\n").map(s =>
      BigInt(s, 16)
    )
  }

  def slopes(ew: Int, mw: Int, projectDir: String = ".", pieces: Int = 8) : Seq[BigInt] =
    pwl("slopes")(ew, mw, projectDir, pieces)

  def intercepts(ew: Int, mw: Int, projectDir: String = ".", pieces: Int = 8) : Seq[BigInt] =
    pwl("intercepts")(ew, mw, projectDir, pieces)
}