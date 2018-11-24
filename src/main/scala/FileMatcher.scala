import java.io.File

//before

//object FileMatcher {
//  private def filesHere = (new File(".")).listFiles
//  def filesEnding(query: String) =
//    for (file <- filesHere if file.getName.endsWith(query))
//      yield file
//  def filesContaining(query: String) =
//    for (file <- filesHere if file.getName.contains(query))
//      yield  file
//  def filesRegex(query: String) =
//    for (file <- filesHere if file.getName.matches(query))
//      yield  file
//}


//after

object FileMatcher {
  private def filesHere = new java.io.File(".").listFiles
  private def filesMatching(matcher: String => Boolean) =
    for (file <- filesHere if matcher(file.getName))
      yield file
  def filesEnding(query: String): Array[File] = filesMatching(_.endsWith(query))
  def filesContaining(query: String): Array[File] = filesMatching(_.contains(query))
  def filesRegex(query: String): Array[File] = filesMatching(_.matches(query))
}