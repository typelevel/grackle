resolvers += Resolver.sonatypeRepo("public")

addSbtPlugin("edu.gemini"            % "sbt-gsp"        % "0.2.5")
addSbtPlugin("com.geirsson"          % "sbt-ci-release" % "1.5.5")
addSbtPlugin("io.spray"              % "sbt-revolver"   % "0.9.1")
addSbtPlugin("com.typesafe.sbt"      % "sbt-site"       % "1.4.1")
addSbtPlugin("com.typesafe.sbt"      % "sbt-ghpages"    % "0.6.3")
addSbtPlugin("ch.epfl.scala"         % "sbt-scalafix"   % "0.9.26")
