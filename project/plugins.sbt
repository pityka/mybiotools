resolvers += Resolver.url("artifactory", url("http://scalasbt.artifactoryonline.com/scalasbt/sbt-plugin-releases"))(Resolver.ivyStylePatterns)

addSbtPlugin("org.scalastyle" %% "scalastyle-sbt-plugin" % "0.4.0")

addSbtPlugin("com.eed3si9n" % "sbt-assembly" % "0.10.1")

credentials += {
      val credsFile = (Path.userHome / ".ivy2" / ".credentials")
      Credentials(credsFile)
    }

addSbtPlugin("org.scalariform" % "sbt-scalariform" % "1.5.1")

addSbtPlugin("de.djini" % "xsbt-reflect" % "0.0.3")

addSbtPlugin("org.scala-js" % "sbt-scalajs" % "0.6.3")

addSbtPlugin("com.lihaoyi" % "workbench" % "0.2.3")

addSbtPlugin("io.spray" % "sbt-revolver" % "0.7.2")
