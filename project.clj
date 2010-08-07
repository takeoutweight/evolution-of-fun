(defproject levelevolve "0.0.2-SNAPSHOT"
  :description "Several related experimentations on level design."
  :dependencies [[org.clojure/clojure "1.5.0"] 
								 [trammel "0.7.0"]
								 [incanter "1.5.1"]
                 [JaCoP "3.2"] ;only available from http://sourceforge.net/projects/jacop-solver/. Install manually via: mvn install:install-file "-Dfile=JaCoP-3.2.jar" "-DgroupId=JaCoP" "-DartifactId=JaCoP" "-Dversion=3.2" "-Dpackaging=jar" "-DgeneratePom=true"mvn install:install-file "-Dfile=JaCoP-3.2.jar" "-DgroupId=JaCoP" "-DartifactId=JaCoP" "-Dversion=3.2" "-Dpackaging=jar" "-DgeneratePom=true"
                 ] 
  :profiles {:dev {:plugins [[lein-swank "1.4.5"]]}
             :precomp {:java-source-paths ["src/java-precomp"]} ;NathanSorensonLevelGenerator.java -> ClojureLevelGenerator.clj -> LevelStub.java
             :postcomp [:precomp {:java-source-paths ["src/java-postcomp"]}]}
  :resource-paths ["resources"]
	:aot [nathansorenson.ClojureLevelGenerator]
  :java-source-paths ["src/java"]
  :source-paths ["src/clj"])
