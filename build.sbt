enablePlugins(JavaAppPackaging)

scalaVersion := "3.9.0"


Compile / mainClass := Some("Main")
name := "jev-llm-connect-four"

libraryDependencies ++= Seq(
  "com.jamesward" %% "zio-bedrock-converse" % "0.1.1",
  "com.jamesward" %% "zio-typesafe-ai" % "0.0.2",
  "dev.zio" %% "zio-http" % "3.11.6",
  "org.webjars.npm" % "tailwindcss__browser" % "4.3.3" % WebJar,
  "dev.zio" %% "zio-test" % "2.1.26" % Test,
  "dev.zio" %% "zio-test-sbt" % "2.1.26" % Test,
)

fork := true

// Local-only development MCP server. It remains loopback-bound and the plugin
// automatically disables it in CI. `sbt-task` can execute arbitrary sbt tasks.
Global / mcpEnabled := true
Global / mcpHost := "127.0.0.1"
Global / mcpPort := 5014

addCommandAlias("dev", "~runReload")

// Agent Skills are extracted for coding agents and stay off the application classpath.
skillsJarsOutputDir := Some(file(".kiro/skills"))
libraryDependencies += "com.jamesward" % "skills" % "0.0.3" % Skills
