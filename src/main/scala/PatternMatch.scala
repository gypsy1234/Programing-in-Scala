object PatternMatch {

  //ワイルドカードパターン
  def wildcard(expr: Expr): String = expr match {
    case _ => "ワイルドカードパターン"
  }

  //定数パターン
  def describe(x: Any): String = x match {
    case 5 => "five"
    case true => "truth"
    case "hello" => "hi"
    case Nil => "the empty list"
    case _ => "something else"
  }

  //変数パターン
  def variable(expr: Int): String = expr match {
    case 0 => "zero"
    case somethingElse => "not zero: " + somethingElse
  }

  //コンストラクターパターン
  def constructor(expr: Expr): String = expr match {
    case BinOp("+", e, Number(0)) => "BinOpで第三引数がNumberでかつ値が0であることまでチェックしている"
    case _ => ""
  }

  //シーケンスパターン
  def seq(expr: Any): String = expr match {
    case List(0, _, _) => "先頭が0で要素数3のList"
    case List(0, _*) => "先頭が0で要素数の縛りはないList"
  }

  //タプルパターン
  def tuple(expr: Any): String = expr match {
    case (a, b, c) => "要素数3のタプル"
  }

  //型付パターン
  def typePattern(x: Any): String = x match {
    case s: String => "String型"
    case m: Map[_, _] => "Map型"
    //case m: Map[Int, Int] はできない
    case a: Array[String] => "Arrayのみジェネリクスまでマッチ可能"
  }

  //変数束縛パターン
  def variableBinding(expr: Expr): String = expr match {
    case UnOp("abs", e @ UnOp("abs", _)) => """UnOpで第二引数がUnOp("abs", _)である場合、変数eに束縛する"""
    case _ => ""
  }

  //パターンガード
  def patternGuard(e: Expr): Expr = e match {
    case BinOp("+", x, y) if x == y => BinOp("*", x, Number(2))
    case _ => e
  }

  //シールドクラスを継承するクラスをパターンマッチする際、全てのケースが網羅されていなくてもコンパイラが警告を出さないようにする方法
  def unchecked(e: Expr):String = (e: @unchecked) match {
    case Number(_) => "a number"
    case Var(_) => "a variable"
  }

  //Option型
  def option(x: Option[String]): String = x match {
    case Some(s) => s
    case None => "?"
  }

  //さまざまなパターンの活用
  def variousPattern(): Unit = {

    //タプルの要素を分解して変数に格納
    val myTuple = (123, "abc")
    val (number, string) = myTuple

    //インスタンスの要素をばらして新たなインスタンスを作成
    val exp = BinOp("*", Number(5), Number(1))
    val BinOp(op, left, right) = exp

    //for文でのパターンマッチ
    val capitals = Map[String, String]("France" -> "Paris", "Japan" -> "Tokyo")
    for ((country, city) <- capitals) println("The capital of " + country + " is " + city)

    //for文でのパターン時、マッチしないもの（ここではNone）は捨てられる
    val results = List(Some("apple"), None, Some("orange"))
    for (Some(fruit) <- results) println(fruit)
  }

  //Option[Int]を引数にとりIntを返す関数をあらわす？
  val withDefault: Option[Int] => Int = {
    case Some(x) => x
    case None => 0
  }

  //要素数3のリストを引数にとり、2番目の要素を返す関数をあらわす　p284
  val second: PartialFunction[List[Int], Int] = {
    case x :: y :: _ => y
  }
}