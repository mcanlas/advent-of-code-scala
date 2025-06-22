lazy val `advent-of-code-scala` =
  project
    .in(file("."))
    .aggregate(
      core,
      `year-2020`,
      `year-2021`,
      `year-2021-scala3`,
      `year-2022`,
      `year-2023`
    )

lazy val core =
  project
    .withCats
    .withFileIO
    .withGitHubPackagesCredentials
    .withResolver("rufio")

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

lazy val `year-2023` =
  project
    .dependsOn(core)
