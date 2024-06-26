package ex2

import org.junit.*
import org.junit.Assert.*

class ManagementApplicationTest:

    import ManagementApplication.*
    import ConferenceReviewing.*
    import Question.* 

    val cr: ConferenceReviewing = ConferenceReviewing()

    cr.loadReview(1, 8, 8, 6, 8)
    cr.loadReview(1, 9, 9, 6, 9); // 5.4 
    cr.loadReview(2, 9, 9, 10, 9); // 9.0
    cr.loadReview(2, 4, 6, 10, 6); // 6.0
    cr.loadReview(3, 3, 3, 3, 3); // 0.9
    cr.loadReview(3, 4, 4, 4, 4); // 1.6
    cr.loadReview(4, 6, 6, 6, 6); // 3.6
    cr.loadReview(4, 7, 7, 8, 7); // 5.6

    import scala.collection.mutable.{Map => MutableMap}

    val map: MutableMap[Question, Int] = MutableMap()
    map.put(Question.RELEVANCE, 8)
    map.put(Question.SIGNIFICANCE, 8)
    map.put(Question.CONFIDENCE, 7) // 5.6
    map.put(Question.FINAL, 8)
    println(map)
    cr.loadReview(4, map.toMap);
    cr.loadReview(5, 6, 6, 6, 10); // 6.0
    cr.loadReview(5, 7, 7, 7, 10);
    
    @Test def testOrderedScores() =
        assertEquals(cr.orderedScores(2, Question.RELEVANCE), List(4,9))
        assertEquals(cr.orderedScores(4, Question.CONFIDENCE), List(6, 7, 8))
        assertEquals(cr.orderedScores(5, Question.FINAL), List(10, 10))

    @Test def testAverageFinalScore() =
        assertEquals(cr.averageFinalScore(1), 8.5, 0.01)
        assertEquals(cr.averageFinalScore(2), 7.5, 0.01)
        assertEquals(cr.averageFinalScore(3), 3.5, 0.01)
        assertEquals(cr.averageFinalScore(4), 7.0, 0.01)
        assertEquals(cr.averageFinalScore(5), 10.0, 0.01)

    @Test def testAcceptedArticles() =
        assertEquals(cr.acceptedArticles, Set(1,2,4))

    @Test def testSortedAcceptedArticles() =
        assertEquals(cr.sortedAcceptedArticles, List((4, 7.0), (2, 7.5), (1, 8.5)))

    @Test def testAverageWeightedFinalScore() =
        assertEquals(cr.averageWeightedFinalScoreMap(1), (4.8 + 5.4) / 2, 0.01)
        assertEquals(cr.averageWeightedFinalScoreMap(2), (9.0 + 6.0) / 2, 0.01)
        assertEquals(cr.averageWeightedFinalScoreMap(3), (0.9 + 1.6) / 2, 0.01)
        assertEquals(cr.averageWeightedFinalScoreMap(4), (3.6 + 5.6 + 5.6) / 3, 0.01)
        assertEquals(cr.averageWeightedFinalScoreMap(5), (6.0 + 7.0) / 2, 0.01)
        assertEquals(cr.averageWeightedFinalScoreMap.size, 5)