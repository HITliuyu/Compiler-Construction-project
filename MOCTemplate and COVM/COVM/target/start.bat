@echo off
set PROJECT_DIR=%~dp0\..

set MAINCLASS=de.tuberlin.uebb.comp1.covm.Main


java %JOPTS% -cp "E:\Exercise\MOCTemplate and COVM\COVM\target\scala-2.11\classes;C:\Users\liuyu_000\.ivy2\cache\org.scala-lang\scala-library\jars\scala-library-2.11.2.jar;C:\Users\liuyu_000\.ivy2\cache\org.scala-lang.modules\scala-parser-combinators_2.11\bundles\scala-parser-combinators_2.11-1.0.2.jar;C:\Users\liuyu_000\.ivy2\cache\com.github.scopt\scopt_2.11\jars\scopt_2.11-3.2.0.jar" "%MAINCLASS%" %*

