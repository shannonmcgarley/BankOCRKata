val one = List('a','b','c','d')
val two = List('a', 'c', 'c','z')


one.zip(two).count(x => x._1 != x._2)