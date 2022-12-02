lazy val `advent-of-code-scala` =
  project
    .in(file("."))
    .withCats
    .aggregate(
      core,
      `year-2020`,
      `year-2021`,
      `year-2021-scala3`,
      `year-2022`
    )

lazy val core =
  project
    .withCats
    .withFileIO

lazy val `year-2020` =
  project
    .dependsOn(core)

lazy val `year-2021` =
  project
    .dependsOn(core)

lazy val `year-2021-scala3` =
  project
    .dependsOn(core)

lazy val `year-2022` =
  project
    .dependsOn(core)

ThisBuild / credentials += Credentials(
  "GitHub Package Registry",
  "maven.pkg.github.com",
  "mcanlas",
  sys.env("GH_PACKAGES_TOKEN")
)

ThisBuild / resolvers += "mcanlas/rufio" at "https://maven.pkg.github.com/mcanlas/rufio/"
