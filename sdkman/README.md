## SDKMAN!

Homebrew defaults to use openjdk for Java and packages that depend on it.
That version of Java does not receive security updates for long, even LTS versions.

That is where this `sdkman` package comes in.
Install it manually, without modifying the shell config:

```
curl -s "https://get.sdkman.io?rcupdate=false" | bash
```

After installing, ensure at least one JDK is installed, like Eclipse Adoptium Temurin.
Afterwards, see if the JVM related tools (`maven`, `leiningen`, `clojure`, `sbt` or `scala`) use the correct JVM.
