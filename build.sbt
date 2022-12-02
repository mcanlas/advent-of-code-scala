lazy val `advent-of-code-scala` =
  project
    .in(file("."))
    .withCats
    .withEffectMonad
    .withFileIO
    .withTesting
    .aggregate(
      `year-2020`,
      `year-2021`,
      `year-2021-scala3`,
      `year-2022`
    )

lazy val `year-2020` =
  project.withCats.withEffectMonad.withFileIO.withTesting

lazy val `year-2021` =
  project.withCats.withEffectMonad.withFileIO.withTesting

lazy val `year-2021-scala3` =
  project.withScala3.withCats.withEffectMonad.withFileIOScala3.withTesting

lazy val `year-2022` =
  project
