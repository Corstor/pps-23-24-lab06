package ex2

import ex1.List.apply
import ex2.ManagementApplication.ConferenceReviewing

object ManagementApplication:
    enum Question:
            case RELEVANCE;
            case SIGNIFICANCE;
            case CONFIDENCE;
            case FINAL;

    trait ConferenceReviewing:        
        def loadReview(article: Int, scores: Map[Question, Int]): Unit
        def loadReview(article: Int, relevance: Int, significance: Int, confidence: Int, fin: Int): Unit
        def orderedScores(article: Int, question: Question): List[Int]
        def averageFinalScore(article: Int): Double
        def acceptedArticles: Set[Int]
        def sortedAcceptedArticles: List[(Int, Double)]
        def averageWeightedFinalScoreMap: Map[Int, Double]

    object ConferenceReviewing:
        private class ConferenceReviewingImpl extends ConferenceReviewing:
            import Question.*
            import PrivateMethods.*

            private var reviews: List[(Int, Map[Question, Int])] = List()

            override def acceptedArticles: Set[Int] = reviews.map(_._1).distinct.filter(accepted).toSet

            override def sortedAcceptedArticles: List[(Int, Double)] = this.acceptedArticles
                .toList
                .map(e => (e, this.averageFinalScore(e)))
                .sorted((e1, e2) => e1._2.compareTo(e2._2))

            override def averageWeightedFinalScoreMap: Map[Int, Double] = reviews
                .map(_._1)
                .distinct
                .map((article: Int) => (article, averageWeightedFinalScore(article)))
                .toMap

            override def loadReview(article: Int, relevance: Int, significance: Int, confidence: Int, fin: Int): Unit = 
                reviews = reviews.appended((article, Map(
                        (RELEVANCE, relevance),
                        (SIGNIFICANCE, significance),
                        (CONFIDENCE, confidence),
                        (FINAL, fin)
                    )
                ))

            override def loadReview(article: Int, scores: Map[Question, Int]): Unit = 
                require(scores.size <= Question.values.length)
                reviews = reviews.appended((article, Map.from(scores)))

            override def orderedScores(article: Int, question: Question): List[Int] = reviews
                .filter(_._1 == article)
                .map(_._2(question))
                .sorted

            override def averageFinalScore(article: Int): Double =
                val scores = reviews
                    .filter(_._1 == article)
                    .map(_._2(FINAL).toDouble)
                
                mean(scores)

            private object PrivateMethods:
                def accepted(article: Int): Boolean = averageFinalScore(article) > 5.0 &&
                    reviews
                    .filter(_._1 == article)
                    .map(_._2)
                    .map(_.toSet)
                    .flatMap(_.toStream)
                    .exists(e => e._1 == RELEVANCE && e._2 >= 8)

                def averageWeightedFinalScore(article: Int): Double = 
                    val scores = reviews
                        .filter(_._1 == article)
                        .map(p => p._2(FINAL) * p._2(CONFIDENCE) / 10.0)

                    mean(scores)

                def mean(list: List[Double]): Double =
                    list.sum / list.size

        def apply(): ConferenceReviewing = ConferenceReviewingImpl()