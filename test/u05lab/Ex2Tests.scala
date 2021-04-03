package u05lab

import org.junit.jupiter.api.Assertions.{assertEquals, assertThrows, assertTrue}
import org.junit.jupiter.api.Test

import scala.collection.immutable.HashMap

class Ex2Tests {
    import u05lab.code._
    @Test
    def generalTests(): Unit = {
        val evaluation = 25
        val erf = ExamResultFactoryImpl()
        assertEquals(Kind.FAILED.toString, erf.failed().toString)
        assertEquals(Kind.RETIRED.toString, erf.retired().toString)
        assertEquals("SUCCEEDED with " + evaluation, erf.succeeded(evaluation).toString)
        assertEquals("SUCCEEDED with 30" + "L", erf.succeededCumLaude().toString)
        // assertThrows(classOf[IllegalArgumentException], () => erf.succeeded(300))
        assertTrue(erf.succeededCumLaude().cumLaude())
        assertEquals(Option(evaluation), erf.succeeded(evaluation).getEvaluation)
    }

    @Test
    def examManagerTests(): Unit = {
        val em = ExamsManagerImpl()
        val julyCall = "July"
        val septemberCall = "September"
        val student1 = "Alberto"
        val stud1Evaluation = ExamResultFactoryImpl().succeeded(28)
        val student2 = "Asia"
        val stud2Evaluation = ExamResultFactoryImpl().succeededCumLaude()

        em.createNewCall(julyCall)
        em.createNewCall(septemberCall)
        em.addStudentResult(julyCall, student1, stud1Evaluation)
        em.addStudentResult(julyCall, student2, stud2Evaluation)
        em.addStudentResult(septemberCall, student1, ExamResultFactoryImpl().retired())
        em.addStudentResult(septemberCall, student2, ExamResultFactoryImpl().failed())

        // testing getAllStudentsFromCall
        assertEquals(Set(student1, student2), em.getAllStudentsFromCall(julyCall))
        assertEquals(Set(student1, student2).size, em.getAllStudentsFromCall(septemberCall).size)

        // testing input validation
        assertThrows(classOf[IllegalArgumentException], () => em.createNewCall(septemberCall))
        assertThrows(classOf[IllegalArgumentException], () => em.addStudentResult(julyCall, student2, ExamResultFactoryImpl().failed()))
        assertThrows(classOf[IllegalArgumentException], () => em.getAllStudentsFromCall("May"))

        // testing getEvaluationsMapFromCall
        val evaluationsMapFromCall = HashMap(student1 -> stud1Evaluation.getEvaluation.get, student2 -> stud2Evaluation.getEvaluation.get)
        assertEquals(evaluationsMapFromCall, em.getEvaluationsMapFromCall(julyCall))
        assertEquals(stud2Evaluation.getEvaluation, em.getEvaluationsMapFromCall(julyCall).get(student2))
        assertEquals(stud1Evaluation.getEvaluation, em.getEvaluationsMapFromCall(julyCall).get(student1))
        assertEquals(Map.empty, em.getEvaluationsMapFromCall(septemberCall))

        // testing getResultsMapFromStudent
        val resultsOfStudent1 = HashMap(julyCall -> stud1Evaluation.toString, septemberCall -> ExamResultFactoryImpl().retired().toString)
        val resultsOfStudent2 = HashMap(julyCall -> stud2Evaluation.toString, septemberCall -> ExamResultFactoryImpl().failed().toString)
        assertEquals(resultsOfStudent1, em.getResultsMapFromStudent(student1))
        assertEquals(resultsOfStudent2, em.getResultsMapFromStudent(student2))
    }
}
