// See LICENSE for license details.

package chiselTests

import chisel3._
import chisel3.experimental.ChiselAnnotation
import chisel3.util._
import chisel3.testers.BasicTester
import chisel3.experimental.{Analog, attach}

// IO for Modules that just connect bus to out
class AnalogReaderIO extends Bundle {
  val bus = Analog(32.W)
  val out = Output(UInt(32.W))
}
// IO for Modules that drive bus from in (there should be only 1)
class AnalogWriterIO extends Bundle {
  val bus = Analog(32.W)
  val in = Input(UInt(32.W))
}

trait AnalogReader extends AnalogAnnotator {
  self: Module =>
  final val io = self.IO(new AnalogReaderIO)
  analog(io.bus, "busbus")
}

class AnalogReaderBlackBox extends BlackBox with AnalogReader

class AnalogReaderWrapper extends Module with AnalogReader {
  val mod = Module(new AnalogReaderBlackBox)
  io <> mod.io
}
class AnalogWriterBlackBox extends BlackBox {
  val io = IO(new AnalogWriterIO)
}
// Connects two Analog ports
class AnalogConnector extends Module {
  val io = IO(new Bundle {
    val bus1 = Analog(32.W)
    val bus2 = Analog(32.W)
  })
  io.bus1 <> io.bus2
}

// import chisel3.internal.InstanceId
import firrtl.ir.{AnalogType, Circuit, DefModule, Expression, HasName, Port, Statement, Type}
import firrtl.{CircuitForm, CircuitState, LowForm, Transform}
import firrtl.annotations.{Annotation, ModuleName, Named, ComponentName}
import firrtl.Mappers._

object AnalogVerilogTpeAnnotation {
  def apply(target: Named, value: String): Annotation =
    Annotation(target, classOf[AnalogVerilogTpeAnnotation], value)

  def unapply(a: Annotation): Option[(ComponentName, String)] = a match {
    case Annotation(named, t, value) if t == classOf[AnalogVerilogTpeAnnotation] => named match {
      case c: ComponentName => Some((c, value))
      case _ => None
    }
    case _ => None
  }
}

class AnalogVerilogTpeAnnotation extends Transform {
  override def inputForm: CircuitForm  = LowForm
  override def outputForm: CircuitForm = LowForm

  override def execute(state: CircuitState): CircuitState = {
    getMyAnnotations(state) match {
      case Nil => state
      case annos =>
        val analogs = annos.collect { case AnalogVerilogTpeAnnotation(ana, name) => (ana, name) }
        println(s"All the analogs are $analogs")
        state.copy(circuit = run(state.circuit, analogs))
    }
  }

  def run(circuit: Circuit, annos: Seq[(ComponentName, String)]): Circuit = {
    circuit map walkModule(annos)
  }
  def walkModule(annos: Seq[(ComponentName, String)])(m: DefModule): DefModule = {
    val filteredAnnos = Map(annos.filter(a => a._1.module.name == m.name).map {
      case (c, s) => c.name.replace(".", "_") -> s
    }: _*)
    println(s"The filtered annos are $filteredAnnos in module ${m.name}")
    m map walkStatement(filteredAnnos) map walkPort(filteredAnnos)
  }
  def walkStatement(annos: Map[String, String])(s: Statement): Statement = {
    s map walkExpression(annos)
  }
  def walkPort(annos: Map[String, String])(p: Port): Port = {
    println(s"Visited port $p with name ${p.name}, annos.contains(p.name) = ${annos.contains(p.name)}")
    if (annos.contains(p.name)) {
      updateAnalogVerilog(annos(p.name))(p.tpe)
    }
    p
  }
  def walkExpression(annos: Map[String, String])(e: Expression): Expression = {
    println(s"Visited expression $e")
    e match {
      case h: HasName =>
        println("Expression $e has name ${h.name}, annos.contains(h.name) = ${annos.contains(h.name)}")
        if (annos.contains(h.name)) e mapType updateAnalogVerilog(annos(h.name))
      case _ =>
    }
    e
  }
  def updateAnalogVerilog(value: String)(tpe: Type): Type = {
    println(s"Updating analog verilog on $tpe to $value")
    tpe match {
      case a: AnalogType =>
        println(s"Setting $a to $value")
        a.verilogTpe = value
        a
      case t => t
    }
  }
}

trait AnalogAnnotator { self: Module =>
  def analog(component: Analog, value: String): Unit = {
    annotate(ChiselAnnotation(component, classOf[AnalogVerilogTpeAnnotation], value))
  }
}


// Parent class for tests connecing up AnalogReaders and AnalogWriters
abstract class AnalogTester extends BasicTester {
  final val BusValue = "hdeadbeef".U

  final val (cycle, done) = Counter(true.B, 2)
  when (done) { stop() }

  final val writer = Module(new AnalogWriterBlackBox)
  writer.io.in := BusValue

  final def check(reader: Module with AnalogReader): Unit =
    assert(reader.io.out === BusValue)
}

class AnalogSpec extends ChiselFlatSpec {
  behavior of "Analog"

  it should "do something with annotations" in {
    Driver.execute(Array("--target-dir", "test_run_dir"), () => new AnalogReaderWrapper) match {
      case ChiselExecutionSuccess(Some(circuit), emitted, _) =>
        val annos = circuit.annotations
        annos.length should be (2)

      case _ =>
        assert(false)
      }
  }


  it should "NOT be bindable to registers" in {
    a [ChiselException] should be thrownBy {
      elaborate { new Module {
        val io = IO(new Bundle {})
        val reg = Reg(Analog(32.W))
      }}
    }
  }

  it should "NOT be bindable to a direction" in {
    a [ChiselException] should be thrownBy {
      elaborate { new Module {
        val io = IO(new Bundle {
          val a = Input(Analog(32.W))
        })
      }}
    }
    a [ChiselException] should be thrownBy {
      elaborate { new Module {
        val io = IO(new Bundle {
          val a = Output(Analog(32.W))
        })
      }}
    }
  }

  it should "be flippable" in {
    elaborate { new Module {
      val io = IO(new Bundle {
        val a = Flipped(Analog(32.W))
      })
    }}
  }

  // There is no binding on the type of a memory
  // Should this be an error?
  ignore should "NOT be a legal type for Mem" in {
    a [ChiselException] should be thrownBy {
      elaborate { new Module {
        val io = IO(new Bundle {})
        val mem = Mem(16, Analog(32.W))
      }}
    }
  }

  it should "NOT be bindable to Mem ports" in {
    a [ChiselException] should be thrownBy {
      elaborate { new Module {
        val io = IO(new Bundle {})
        val mem = Mem(16, Analog(32.W))
        val port = mem(5.U)
      }}
    }
  }

  // TODO This should probably be caught in Chisel
  // Also note this relies on executing Firrtl from Chisel directly
  it should "NOT be connectable to UInts" in {
    a [Exception] should be thrownBy {
      runTester { new BasicTester {
        val uint = Wire(init = 0.U(32.W))
        val sint = Wire(Analog(32.W))
        sint := uint
      }}
    }
  }

  it should "work with 2 blackboxes bulk connected" in {
    assertTesterPasses(new AnalogTester {
      val mod = Module(new AnalogReaderBlackBox)
      mod.io.bus <> writer.io.bus
      check(mod)
    }, Seq("/AnalogBlackBox.v"))
  }

  it should "error if any bulk connected more than once" in {
    a [ChiselException] should be thrownBy {
      elaborate(new Module {
        val io = IO(new Bundle {})
        val wires = List.fill(3)(Wire(Analog(32.W)))
        wires(0) <> wires(1)
        wires(0) <> wires(2)
      })
    }
  }

  it should "work with 3 blackboxes attached" in {
    assertTesterPasses(new AnalogTester {
      val mods = Seq.fill(2)(Module(new AnalogReaderBlackBox))
      attach(writer.io.bus, mods(0).io.bus, mods(1).io.bus)
      mods.foreach(check(_))
    }, Seq("/AnalogBlackBox.v"))
  }

  it should "work with 3 blackboxes separately attached via a wire" in {
    assertTesterPasses(new AnalogTester {
      val mods = Seq.fill(2)(Module(new AnalogReaderBlackBox))
      val busWire = Wire(Analog(32.W))
      attach(busWire, writer.io.bus)
      attach(busWire, mods(0).io.bus)
      attach(mods(1).io.bus, busWire)
      mods.foreach(check(_))
    }, Seq("/AnalogBlackBox.v"))
  }

  // This does not currently work in Verilator unless Firrtl does constant prop and dead code
  // elimination on these wires
  ignore should "work with intermediate wires attached to each other" in {
    assertTesterPasses(new AnalogTester {
      val mod = Module(new AnalogReaderBlackBox)
      val busWire = Seq.fill(2)(Wire(Analog(32.W)))
      attach(busWire(0), writer.io.bus)
      attach(busWire(1), mod.io.bus)
      attach(busWire(0), busWire(1))
      check(mod)
    }, Seq("/AnalogBlackBox.v"))
  }

  it should "work with blackboxes at different levels of the module hierarchy" in {
    assertTesterPasses(new AnalogTester {
      val mods = Seq(Module(new AnalogReaderBlackBox), Module(new AnalogReaderWrapper))
      val busWire = Wire(writer.io.bus)
      attach(writer.io.bus, mods(0).io.bus, mods(1).io.bus)
      mods.foreach(check(_))
    }, Seq("/AnalogBlackBox.v"))
  }

  // This does not currently work in Verilator, but does work in VCS
  ignore should "support two analog ports in the same module" in {
    assertTesterPasses(new AnalogTester {
      val reader = Module(new AnalogReaderBlackBox)
      val connector = Module(new AnalogConnector)
      connector.io.bus1 <> writer.io.bus
      reader.io.bus <> connector.io.bus2
      check(reader)
    }, Seq("/AnalogBlackBox.v"))
  }

  it should "NOT support conditional connection of analog types" in {
    a [ChiselException] should be thrownBy {
      assertTesterPasses(new AnalogTester {
        val mod = Module(new AnalogReaderBlackBox)
        when (cycle > 3.U) {
          mod.io.bus <> writer.io.bus
        }
        check(mod)
      }, Seq("/AnalogBlackBox.v"))
    }
  }
}

