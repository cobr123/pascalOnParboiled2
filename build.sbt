lazy val root = (project in file(".")).
  settings(
    name := "pascal_parboiled2",
    version := "1.0",
    libraryDependencies += "org.parboiled" %% "parboiled" % "2.1.0"
  )

