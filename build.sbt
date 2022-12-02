lazy val `advent-of-code-scala` =
  project
    .in(file("."))
    .withCats
    .withFileIO
    .aggregate(
      `year-2020`,
      `year-2021`,
      `year-2021-scala3`,
      `year-2022`
    )

lazy val `year-2020` =
  project
    .withCats
    .withFileIO

lazy val `year-2021` =
  project
    .withCats
    .withFileIO

lazy val `year-2021-scala3` =
  project.withCats

lazy val `year-2022` =
  project.withFileIO

ThisBuild / credentials += Credentials(
  "GitHub Package Registry",
  "maven.pkg.github.com",
  "mcanlas",
  sys.env("GH_PACKAGES_TOKEN")
)

ThisBuild / resolvers += "mcanlas/rufio" at "https://maven.pkg.github.com/mcanlas/rufio/"
