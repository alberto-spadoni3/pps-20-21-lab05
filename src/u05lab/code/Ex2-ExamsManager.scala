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

    def getKind: ExamKind
    def getEvaluation: Option[Int]
    def cumLaude(): Boolean
}

sealed trait ExamsManager {
    def createNewCall(call: String): Unit
    def addStudentResult(call: String, student:String, result: ExamResult): Unit
    def getAllStudentsFromCall(call: String): Set[String]
    def getEvaluationsMapFromCall(call: String): HashMap[String, Int]
    def getResultsMapFromStudent(student: String): HashMap[String, String]
}

case class ExamsManagerImpl() extends ExamsManager {
    private var callsAndStudents: HashMap[String, HashMap[String, ExamResult]] = HashMap.empty

    override def createNewCall(call: String): Unit = {
        require(!callsAndStudents.contains(call))
        callsAndStudents += (call -> HashMap.empty)
    }

    override def addStudentResult(call: String, student: String, result: ExamResult): Unit = {
        require(callsAndStudents.contains(call) && !callsAndStudents(call).contains(student))
        callsAndStudents += (call -> (callsAndStudents(call) + (student -> result)))
    }

    override def getAllStudentsFromCall(call: String): Set[String] = {
        require(callsAndStudents.contains(call))
        callsAndStudents(call).keySet
    }

    override def getEvaluationsMapFromCall(call: String): HashMap[String, Int] = {
        require(callsAndStudents.contains(call))
        callsAndStudents(call)
          .filter(studEval => studEval._2.getEvaluation.isDefined)
          .map(studEval => studEval._1 -> studEval._2.getEvaluation.get)
    }

    override def getResultsMapFromStudent(student: String): HashMap[String, String] = {
        callsAndStudents.filter(entry => entry._2.contains(student))
          .map(entry => entry._1 -> entry._2(student).toString)
    }
}

sealed trait ExamResultFactory {
    def failed(): ExamResult
    def retired(): ExamResult
    def succeededCumLaude(): ExamResult
    def succeeded(evaluation: Int): ExamResult
}

case class ExamResultFactoryImpl() extends ExamResultFactory {
    class ExamResultImpl(private val kind: ExamKind = Kind.FAILED) extends ExamResult {
        override def getKind: ExamKind = kind
        override def getEvaluation: Option[Int] = Option.empty
        override def cumLaude(): Boolean = false
        override def toString: String = kind.toString
    }

    /**
      *
      * @throws IllegalArgumentException - when the evaluation is not in a valid range
      */
    class SucceededExamImpl(private val evaluation: Int) extends ExamResultImpl(Kind.SUCCEEDED) {
        // vValidating the given evaluation
        if (evaluation <= 18 && evaluation >= 30)
            throw new IllegalArgumentException("Evaluation must be in the range 18..30")

        override def getEvaluation: Option[Int] = Option(evaluation)
        override def toString: String = super.toString + " with " + evaluation + (if (this.cumLaude()) "L" else "")
    }

    override def failed(): ExamResult = new  ExamResultImpl
    override def retired(): ExamResult = new ExamResultImpl(Kind.RETIRED)
    @throws(classOf[java.lang.IllegalArgumentException])
    override def succeeded(evaluation: Int): ExamResult = new SucceededExamImpl(evaluation)
    override def succeededCumLaude(): ExamResult = new SucceededExamImpl(evaluation = 30) {
        override def cumLaude(): Boolean = true
    }
}