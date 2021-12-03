lazy val `advent-of-code-2020-scala` =
  project
    .in(file("."))
    .withCats
    .withEffectMonad
    .withFileIO
    .withTesting
    .aggregate(`year-2020`, `year-2021`)

lazy val `year-2020` =
  project
    .withCats
    .withEffectMonad
    .withFileIO
    .withTesting

lazy val `year-2021` =
  project
    .withCats
    .withEffectMonad
    .withFileIO
    .withTesting
