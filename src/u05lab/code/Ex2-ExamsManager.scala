package u05lab.code

import u05lab.code.Kind.ExamKind
import scala.collection.immutable.HashMap

object Kind {
    sealed trait ExamKind
    case object FAILED extends ExamKind
    case object RETIRED extends ExamKind
    case object SUCCEEDED extends ExamKind
}

sealed trait ExamResult {
    import Kind._

    def getKind(): ExamKind
    def getEvaluation(): Option[Int]
    def cumLaude(): Boolean
}

sealed trait ExamsManager {
    def createNewCall(call: String): Unit
    def addStudentResult(call: String, student:String, result: ExamResult): Unit
    def getAllStudentsFromCall(call: String): Set[String]
    def getEvaluationsMapFromCall(call: String): HashMap[String, Int]
}

case class ExamsManagerImpl() extends ExamsManager {
    private var callsAndStudents: HashMap[String, HashMap[String, ExamResult]] = HashMap.empty

    override def createNewCall(call: String): Unit = {
        if (callsAndStudents.contains(call))
            throw new IllegalArgumentException

        callsAndStudents = callsAndStudents + (call -> HashMap.empty)
    }

    override def addStudentResult(call: String, student: String, result: ExamResult): Unit = {
        if (!callsAndStudents.contains(call) ||
            callsAndStudents(call).contains(student))
            throw new IllegalArgumentException

        callsAndStudents += (call -> (callsAndStudents(call) + (student -> result)))
    }

    override def getAllStudentsFromCall(call: String): Set[String] = {
        if (!callsAndStudents.contains(call))
            throw new IllegalArgumentException

        callsAndStudents(call).keySet
    }

    override def getEvaluationsMapFromCall(call: String): HashMap[String, Int] = {
        if (!callsAndStudents.contains(call))
            throw new IllegalArgumentException

        callsAndStudents(call)
          .filter(studEval => studEval._2.getEvaluation().isDefined)
          .map(studEval => (studEval._1 -> studEval._2.getEvaluation().get))
    }
}

sealed trait ExamResultFactory {
    def failed(): ExamResult
    def retired(): ExamResult
    def succeededCumLaude(): ExamResult
    def succeeded(evaluation: Int): ExamResult
}

case class ExamResultFactoryImpl() extends ExamResultFactory {
    abstract class AbstractExamResult(private val kind: ExamKind = Kind.FAILED) extends ExamResult {
        override def getKind(): ExamKind = kind
        override def getEvaluation(): Option[Int] = Option.empty
        override def cumLaude(): Boolean = false
        override def toString: String = kind.toString
    }

    /**
      *
      * @throws IllegalArgumentException - when the evaluation is not in a valid range
      */
    abstract class AbstractSucceededExam(private val evaluation: Int) extends AbstractExamResult(Kind.SUCCEEDED) {
        // vValidating the given evaluation
        if (evaluation <= 18 && evaluation >= 30)
            throw new IllegalArgumentException("Evaluation must be in the range 18..30")

        override def getEvaluation(): Option[Int] = Option(evaluation)
        override def toString: String = super.toString + " with " + evaluation + (if (this.cumLaude()) "L" else "")
    }

    override def failed(): ExamResult = new AbstractExamResult(){}
    override def retired(): ExamResult = new AbstractExamResult(Kind.RETIRED){}
    @throws(classOf[java.lang.IllegalArgumentException])
    override def succeeded(evaluation: Int): ExamResult = new AbstractSucceededExam(evaluation){}
    override def succeededCumLaude(): ExamResult = new AbstractSucceededExam(evaluation = 30) {
        override def cumLaude(): Boolean = true
    }
}