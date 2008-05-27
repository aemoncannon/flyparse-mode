//script


import scala.tools.nsc.reporters.{Reporter, ConsoleReporter}
import scala.tools.nsc.ast._
import compat.Platform.{EOL => LINE_SEPARATOR}
import java.io.{OutputStream, PrintWriter, Writer}
import scala.tools.nsc.symtab.Flags._
import scala.tools.nsc.symtab.SymbolTable
import scala.tools.nsc.ast.parser._
import scala.tools.nsc.ast.parser.Parsers
import scala.tools.nsc.util._
import scala.tools.nsc.util.SourceFile
import scala.tools.nsc.io._
import scala.tools.nsc._
import scala.tools.nsc.{Global, Settings}
import java.nio.charset._
import java.io.File
import java.io.PrintWriter

val settings = new Settings
settings.stop.value = List("namer")
settings.Xprintpos.value = true
val sourceFiles = List(new File("Draggable.scala"))
val reporter = new ConsoleReporter(settings)
val compiler = new Global(settings, reporter)
import compiler._

class TreePrinter(out: PrintWriter) {
  protected var indentMargin = 0
  protected val indentStep = 2
  protected var indentString = "                                        " // 40
  val showOuterTests = false


  def flush() = out.flush()

  def indent = indentMargin += indentStep
  def undent = indentMargin -= indentStep

  def println {
    out.println()
    while (indentMargin > indentString.length())
    indentString += indentString
    if (indentMargin > 0)
    out.write(indentString, 0, indentMargin)
  }

  def printSeq[a](ls: List[a])(printelem: a => Unit)(printsep: => Unit) {
    ls match {
      case List() =>
      case List(x) => printelem(x)
      case x :: rest => printelem(x); printsep; printSeq(rest)(printelem)(printsep)
    }
  }

  def printColumn(ts: List[Tree], start: String, sep: String, end: String) {
    print(start); indent; println
    printSeq(ts){print}{print(sep); println}; undent; println; print(end)
  }

  def printRow(ts: List[Tree], start: String, sep: String, end: String) {
    print(start); printSeq(ts){print}{print(sep)}; print(end)
  }

  def printRow(ts: List[Tree], sep: String) { printRow(ts, "", sep, "") }

  def printTypeParams(ts: List[TypeDef]) {
    if (!ts.isEmpty) {
      print("["); printSeq(ts){printParam}{print(", ")}; print("]")
    }
  }

  def printValueParams(ts: List[ValDef]) {
    print("(")
    if (!ts.isEmpty) printFlags(ts.head.mods.flags & IMPLICIT, "")
    printSeq(ts){printParam}{print(", ")}
    print(")")
  }

  def printParam(tree: Tree) {
    tree match {
      case ValDef(mods, name, tp, rhs) =>
      printAnnotations(tree)
      print(symName(tree, name)); printOpt(": ", tp)
      case TypeDef(mods, name, tparams, rhs) =>
      print(symName(tree, name))
      printTypeParams(tparams); print(rhs)
    }
  }

  def printBlock(tree: Tree) {
    tree match {
      case Block(_, _) =>
      print(tree)
      case _ =>
      printColumn(List(tree), "{", ";", "}")
    }
  }

  def symName(tree: Tree, name: Name): String =
  if (tree.symbol != null && tree.symbol != NoSymbol) {
    ((if (tree.symbol.isMixinConstructor) "/*"+tree.symbol.owner.name+"*/" else "") +
      tree.symbol.nameString)
  } else name.toString();

  def printOpt(prefix: String, tree: Tree) {
    if (!tree.isEmpty) { print(prefix); print(tree) }
  }

  def printModifiers(tree: Tree, mods: Modifiers) {
    if (tree.symbol == NoSymbol)
    printFlags(mods.flags, mods.privateWithin.toString)
    else if (tree.symbol.privateWithin == NoSymbol ||
      tree.symbol.privateWithin == tree.symbol.owner)
    printFlags(tree.symbol.flags, "")
    else
    printFlags(tree.symbol.flags, tree.symbol.privateWithin.name.toString)
  }

  def printFlags(flags: Long, privateWithin: String) {
    var mask: Long = if (settings.debug.value) -1L else PrintableFlags
    val s = flagsToString(flags & mask, privateWithin.toString)
    if (s.length() != 0) print(s + " ")
  }

  def printAnnotations(tree: Tree) {
    val annots = tree.symbol.attributes
    if (!annots.isEmpty)
    annots foreach { annot => print("@"+annot+" ") }
    else {
      val annots = tree.asInstanceOf[MemberDef].mods.annotations
      if (!annots.isEmpty) 
      annots foreach { annot => print("@"+annot+" ") }
    }
  }

  def print(str: String) { out.print(str) }
  def print(name: Name) { print(name.toString()) }

  def printTree(name:String, rest:List[Tree]) { 
    print("(" + name)
    for(ea <- rest){ print(ea) }
    print(")")
  }

  def printTree(name:String) { 
    print("(" + name + ")")
  }

  private var currentOwner: Symbol = NoSymbol
  private var selectorType: Type = NoType

  def printRaw(tree: Tree) {
    tree match {
      case EmptyTree =>
      printTree("")

      case ClassDef(mods, name, tparams, impl) =>
//      printAnnotations(tree)
//      printModifiers(tree, mods)
      print((if (mods hasFlag TRAIT) "trait " else "class ") + symName(tree, name))
      printTypeParams(tparams)
      print(" extends "); 
      printTree("BLOCK", List(impl))

      case PackageDef(packaged, stats) =>
      printAnnotations(tree)
      print("package "); print(packaged); printColumn(stats, " {", ";", "}")

      case ModuleDef(mods, name, impl) =>
      printAnnotations(tree)
      printModifiers(tree, mods); print("object " + symName(tree, name))
      print(" extends "); print(impl)

      case ValDef(mods, name, tp, rhs) =>
      printAnnotations(tree)
      printModifiers(tree, mods)
      print(if (mods.hasFlag(MUTABLE)) "var " else "val ")
      print(symName(tree, name))
      printOpt(": ", tp)
      if (!mods.hasFlag(DEFERRED)) {
        print(" = ")
        if (rhs.isEmpty) print("_") else print(rhs)
      }

      case DefDef(mods, name, tparams, vparamss, tp, rhs) =>
      printAnnotations(tree)
      printModifiers(tree, mods)
      print("def " + symName(tree, name))
      printTypeParams(tparams); vparamss foreach printValueParams
      printOpt(": ", tp); printOpt(" = ", rhs)

      case TypeDef(mods, name, tparams, rhs) =>
      if (mods hasFlag (PARAM | DEFERRED)) {
        printModifiers(tree, mods); print("type "); printParam(tree)
      } else {
        printAnnotations(tree)
        printModifiers(tree, mods); print("type " + symName(tree, name))
        printTypeParams(tparams); printOpt(" = ", rhs)
      }

      case LabelDef(name, params, rhs) =>
      print(symName(tree, name)); printRow(params, "(", ",", ")"); printBlock(rhs)

      case Import(expr, selectors) =>
      def selectorToString(s: (Name, Name)): String = 
      if (s._1 == nme.WILDCARD || s._1 == s._2) s._1.toString()
      else s._1.toString() + "=>" + s._2.toString()
      print("import "); print(expr)
      print(".")
      selectors.map(selectorToString) match {
        case List(one) => print(one)
        case many => print(many.mkString("{", ", ", "}"))
      }

      case DocDef(comment, definition) =>
      print(comment); println; print(definition)

      case Annotation(Apply(Select(New(tpt), nme.CONSTRUCTOR), args), elements) =>
      print(tpt)
      if (!args.isEmpty)
      printRow(args, "(", ",", ")")
      if (!elements.isEmpty)
      print((for (Assign(name, value) <- elements) yield "val " + name + " = " + value).
        mkString("{", ",", "}"))

      case Template(parents, self, body) =>
      val currentOwner1 = currentOwner
      if (tree.symbol != NoSymbol) currentOwner = tree.symbol.owner
      printRow(parents, " with ")
      if (!body.isEmpty) {
        if (self.name != nme.WILDCARD) {
          print(" { "); print(self.name); printOpt(": ", self.tpt); print(" => ") 
        } else if (!self.tpt.isEmpty) {
          print(" { _ : "); print(self.tpt); print(" => ") 
        } else {
          print(" {")
        }
        printColumn(body, "", ";", "}")
      }
      currentOwner = currentOwner1

      case Block(stats, expr) =>
      printColumn(stats ::: List(expr), "{", ";", "}")

      case Match(selector, cases) =>
      val selectorType1 = selectorType
      selectorType = selector.tpe
      print(selector); printColumn(cases, " match {", "", "}")
      selectorType = selectorType1

      case CaseDef(pat, guard, body) =>
      print("case ")
      def patConstr(pat: Tree): Tree = pat match {
        case Apply(fn, args) => patConstr(fn)
        case _ => pat
      }
      if (showOuterTests &&
        needsOuterTest(
          patConstr(pat).tpe.finalResultType, selectorType, currentOwner))
      print("???")
      print(pat); printOpt(" if ", guard)
      print(" => "); print(body)

      case Sequence(trees) =>
      printRow(trees, "[", ", ", "]")

      case Alternative(trees) =>
      printRow(trees, "(", "| ", ")")

      case Star(elem) =>
      print("("); print(elem); print(")*")

      case Bind(name, t) =>
      print("("); print(symName(tree, name)); print(" @ "); print(t); print(")")

      case UnApply(fun, args) =>
      print(fun); print(" <unapply> "); printRow(args, "(", ", ", ")")

      case ArrayValue(elemtpt, trees) =>
      print("Array["); print(elemtpt); printRow(trees, "]{", ", ", "}")

      case Function(vparams, body) =>
      print("("); printValueParams(vparams); print(" => "); print(body); print(")")

      case Assign(lhs, rhs) =>
      print(lhs); print(" = "); print(rhs)

      case If(cond, thenp, elsep) =>
      print("if ("); print(cond); print(")"); indent; println
      print(thenp); undent
      if (!elsep.isEmpty) {
        println; print("else"); indent; println; print(elsep); undent
      }

      case Return(expr) =>
      print("return "); print(expr)

      case Try(block, catches, finalizer) =>
      print("try "); printBlock(block)
      if (!catches.isEmpty) printColumn(catches, " catch {", "", "}")
      printOpt(" finally ", finalizer)

      case Throw(expr) =>
      print("throw "); print(expr)

      case New(tpe) =>
      print("new "); print(tpe)

      case Typed(expr, tp) =>
      print("("); print(expr); print(": "); print(tp); print(")")

      case TypeApply(fun, targs) =>
      print(fun); printRow(targs, "[", ", ", "]")

      case Apply(fun, vargs) =>
      print(fun); printRow(vargs, "(", ", ", ")")

      case ApplyDynamic(qual, vargs) =>
      print("<apply-dynamic>("); print(qual); print("#"); print(tree.symbol.nameString)
      printRow(vargs, ", (", ", ", "))")

      case Super(qual, mix) =>
      if (!qual.isEmpty || tree.symbol != NoSymbol) print(symName(tree, qual) + ".")
      print("super")
      if (!mix.isEmpty)
      print("[" + mix + "]")

      case This(qual) =>
      if (!qual.isEmpty) print(symName(tree, qual) + ".")
      print("this")

      case Select(qual @ New(tpe), name) if (!settings.debug.value) =>
      print(qual)

      case Select(qualifier, name) =>
      print(qualifier); print("."); print(symName(tree, name))

      case Ident(name) =>
      print(symName(tree, name))

      case Literal(x) =>
      print(x.escapedStringValue)

      case TypeTree() =>
      print(
        if (tree.tpe eq null)
        "<type ?>"
        else if ((tree.tpe.typeSymbol ne null) && tree.tpe.typeSymbol.isAnonymousClass)
        tree.tpe.typeSymbol.toString()
        else
        tree.tpe.toString()
      )

      case Annotated(Annotation(Apply(Select(New(tpt), nme.CONSTRUCTOR), args), elements), tree) =>
      def printAnnot() {
        print("@"); print(tpt)
        if (!args.isEmpty)
        printRow(args, "(", ",", ")")
        if (!elements.isEmpty)
        print((for (Assign(name, value) <- elements) yield "val " + name + " = " + value).
          mkString("{", ",", "}"))
      }
      if (tree.isType) { printAnnot(); print(" "); print(tree) }
      else { print(tree); print(": "); printAnnot() }
      
      case SingletonTypeTree(ref) =>
      print(ref); print(".type")

      case SelectFromTypeTree(qualifier, selector) =>
      print(qualifier); print("#"); print(symName(tree, selector))

      case CompoundTypeTree(templ) =>
      print(templ)

      case AppliedTypeTree(tp, args) =>
      print(tp); printRow(args, "[", ", ", "]")

      case TypeBoundsTree(lo, hi) =>
      printOpt(" >: ", lo); printOpt(" <: ", hi)

      case ExistentialTypeTree(tpt, whereClauses) =>
      print(tpt); 
      printColumn(whereClauses, " forSome { ", ";", "}")

      case tree: StubTree =>
      print(tree.toString)

      case tree => 
      print("<unknown tree of class "+tree.getClass+">")
    }
    if (settings.printtypes.value && tree.isTerm && !tree.isEmpty) {
      print("{"); print(if (tree.tpe eq null) "<null>" else tree.tpe.toString()); print("}")
    }
  }

  def print(tree: Tree) {
    if (settings.Xprintpos.value) print("(" + tree.pos.offset.getOrElse{-1} + ")")
    printRaw(
      if (tree.isDef && tree.symbol != NoSymbol && tree.symbol.isInitialized) {
        tree match {
          case ClassDef(_, _, _, impl)           => ClassDef(tree.symbol, impl)
          case ModuleDef(_, _, impl)             => ModuleDef(tree.symbol, impl)
          case ValDef(_, _, _, rhs)              => ValDef(tree.symbol, rhs)
          case DefDef(_, _, _, vparamss, _, rhs) => DefDef(tree.symbol, vparamss, rhs)
          case TypeDef(_, _, _, rhs)             => TypeDef(tree.symbol, rhs)
          case _ => tree
        }
      } else tree)
  }

}

try {
  val run = new Run
  run.compile(sourceFiles.map (_.toString))
  (new TreePrinter(new PrintWriter(System.out, true))).print(run.units.next.body)
}
catch {
  case exception: Throwable if (exception.getMessage ne null) =>
  exception.printStackTrace()
  error("Compile failed because of an internal compiler error (" +
    exception.getMessage + "); see the error output for details.")
  case exception =>
  exception.printStackTrace()
  error("Compile failed because of an internal compiler error " +
    "(no error message provided); see the error output for details.")
}




