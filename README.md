### References

* http://www.swsys-web.ru/en/x-tree-superarns-algorithms.html
* http://www.dbs.ifi.lmu.de/Publikationen/Papers/x-tree.ps
* http://ceur-ws.org/Vol-74/files/FORUM_18.pdf
* https://infolab.usc.edu/csci599/Fall2001/paper/rstar-tree.pdf

### Наблюдения
X-дерево тормозит, если много раз не получается разбить внутренний узел

java.lang.OutOfMemoryError: GC overhead limit exceeded
	at scala.collection.immutable.List.flatMap(List.scala:267)
	at scala.collection.immutable.List.flatMap(List.scala:79)
	at indices.BulkRTreeVertex$BulkRTreeNode.findMBR(BulkRTreeVertex.scala:38)
	at indices.BulkRTreePointIndex.$anonfun$findRules$2(BulkRTreePointIndex.scala:28)
	at indices.BulkRTreePointIndex$$Lambda$5130/1470434837.apply(Unknown Source)
	at scala.collection.immutable.List.flatMap(List.scala:265)
	at indices.BulkRTreePointIndex.$anonfun$findRules$1(BulkRTreePointIndex.scala:28)
	at indices.BulkRTreePointIndex.$anonfun$findRules$1$adapted(BulkRTreePointIndex.scala:26)
	at indices.BulkRTreePointIndex$$Lambda$5111/74952016.apply(Unknown Source)
	at fs2.Stream$.go$21(Stream.scala:1210)
	at fs2.Stream$.$anonfun$flatMap$1(Stream.scala:1220)
	at fs2.Stream$$$Lambda$4990/826130315.apply(Unknown Source)
	at fs2.internal.FreeC$$anon$1.cont(FreeC.scala:29)
	at fs2.internal.FreeC$ViewL$$anon$9$$anon$10.cont(FreeC.scala:196)
	at fs2.internal.FreeC$ViewL$.mk(FreeC.scala:185)
	at fs2.internal.FreeC$ViewL$.apply(FreeC.scala:176)
	at fs2.internal.FreeC.viewL(FreeC.scala:79)
	at fs2.internal.Algebra$.go$1(Algebra.scala:178)
	at fs2.internal.Algebra$.$anonfun$compileLoop$8(Algebra.scala:227)
	at fs2.internal.Algebra$$$Lambda$5014/1039338228.apply(Unknown Source)
	at fs2.internal.Algebra$.$anonfun$compileLoop$1(Algebra.scala:197)
	at fs2.internal.Algebra$$$Lambda$5013/1021872124.apply(Unknown Source)
	at zio.internal.FiberContext.evaluateNow(FiberContext.scala:335)
	at zio.internal.FiberContext.$anonfun$evaluateLater$1(FiberContext.scala:687)
	at zio.internal.FiberContext$$Lambda$4948/1118508214.run(Unknown Source)
	at java.util.concurrent.ThreadPoolExecutor.runWorker(ThreadPoolExecutor.java:1149)
	at java.util.concurrent.ThreadPoolExecutor$Worker.run(ThreadPoolExecutor.java:624)
	at java.lang.Thread.run(Thread.java:748)
