

import objsets.{Empty, TweetList, TweetReader, TweetSet}
import objsets.TweetReader._

val google = List("android", "Android", "galaxy", "Galaxy", "nexus", "Nexus")
val apple = List("ios", "iOS", "iphone", "iPhone", "ipad", "iPad")

val allTweet=TweetReader.allTweets

lazy val googleTweets: TweetSet = allTweet.filter(p => google.exists(x => p.text.contains(x)))

lazy val appleTweets: TweetSet = allTweet.filter(p => apple.exists(x => p.text.contains(x)))

googleTweets.mostRetweeted
appleTweets.mostRetweeted
/**
  * A list of all tweets mentioning a keyword from either apple or google,
  * sorted by the number of retweets.
  */
//lazy val trending: TweetList = (appleTweets union googleTweets).descendingByRetweet
val tw=appleTweets union googleTweets
//tw.foreach(f => println(f.text))
tw.mostRetweeted
val test=tw.remove(tw.mostRetweeted)
//  test.foreach(f => println(f.text))
//test.mostRetweeted
//val t1=test.remove(test.mostRetweeted)
//val t2=t1.mostRetweeted
//val tw2 =t1.remove(t2)
//val t3=tw2.mostRetweeted
lazy val trending: TweetList = (appleTweets union googleTweets).descendingByRetweet
trending
