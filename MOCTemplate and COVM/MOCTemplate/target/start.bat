@echo off
set PROJECT_DIR=%~dp0\..

set MAINCLASS=de.tuberlin.uebb.comp1.moc.Main


java %JOPTS% -cp "E:\Exercise\MOCTemplate and COVM\MOCTemplate\target\scala-2.11\classes;E:\Exercise\MOCTemplate and COVM\MOCTemplate\lib\covm_2.11-0.1.1.jar;E:\Exercise\MOCTemplate and COVM\MOCTemplate\lib\parsercombinators_2.11-0.1.0.jar;C:\Users\liuyu_000\.ivy2\cache\org.scala-lang\scala-library\jars\scala-library-2.11.2.jar;C:\Users\liuyu_000\.ivy2\cache\org.scala-lang.modules\scala-parser-combinators_2.11\bundles\scala-parser-combinators_2.11-1.0.2.jar;C:\Users\liuyu_000\.ivy2\cache\com.github.scala-incubator.io\scala-io-core_2.11\jars\scala-io-core_2.11-0.4.3.jar;C:\Users\liuyu_000\.ivy2\cache\com.madgag\scala-arm_2.11\jars\scala-arm_2.11-1.3.3.jar;C:\Users\liuyu_000\.ivy2\cache\org.scala-lang.plugins\scala-continuations-library_2.11\bundles\scala-continuations-library_2.11-1.0.1.jar;C:\Users\liuyu_000\.ivy2\cache\com.github.scala-incubator.io\scala-io-file_2.11\jars\scala-io-file_2.11-0.4.3.jar" "%MAINCLASS%" %*

