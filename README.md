# The Evolution of Fun
## A Generic Model of Video Game Challenge for Automatic Level Design

This is an assortment of code that supported a good deal of my master's research from 2008-2010. I combined genetic algorithms with constraint satisfaction to test a model for identifying fun video game levels. This model was based on the notion of "rhythm groups:" periodic configurations of difficult sections followed by easier sections.

Note that this is "research code." It wasn't intended to serve as a slick, well documented library. Much of it is rather old: some of it pre-dates the Clojure 1.0 release!

## Getting Started

You need [Leiningen](http://leiningen.org/).

You will also need the JaCoP 3.2 constraint satisfaction library. You can download the [jar](http://sourceforge.net/projects/jacop-solver/). It isn't available on any Maven repositories, but can be installed locally so Leiningen can find it via

    mvn install:install-file "-Dfile=JaCoP-3.2.jar" "-DgroupId=JaCoP" \
	  "-DartifactId=JaCoP" "-Dversion=3.2" "-Dpackaging=jar" "-DgeneratePom=true"

(thanks to [this blog post](http://xfthhxk.blogspot.ca/2013/05/maven-mayhem.html)).

There is a mixture of Clojure and Java code, so the project needs to be compiled in stages:

    lein with-profile precomp javac
    lein compile
    lein with-profile postcomp javac

To try some auto-generated levels, start a repl: `lein repl` and load the program via `(require '[nathansorenson.level-evolve :as le])` and start evolving with `(le/start)`.

After a few minutes, you can `(le/run-best)` to play the current highest-fitness  level.

## References

The Evolution of Fun: A Generic Model of Video Game Challenge for Automatic Level Design. (Thesis) M.Sc. Simon Fraser University. 2010. [Open Access Link](http://summit.sfu.ca/item/11485)

A Generic Approach to Challenge Modeling for the Procedural Creation of Video Game Levels. Sorenson, Nathan and Pasquier, Philippe and DiPaola, Steve. IEEE Transactions on Computational Intelligence and AI in Games - IEEE, pg 229-244, 2011

The 2010 Mario AI Championship: Level Generation Track. Shaker, Noor and Togelius, Julian and Yannakakis, Georgios N. and Weber, Ben and Shimizu, Tomoyuki and Hashiyama, Tomonori and Sorenson, Nathan and Pasquier, Philippe and Mawhorter, Peter and Takahashi, Glen and Smith, Gillian and Baumgarten, Robin. IEEE Transactions on Computational Intelligence and AI in Games - IEEE, pg 332-347, 2011

Towards a Generic Framework for Automated Video Game Level Creation
International Conference on Evolutionary Computation in Games (EvoGames), Istanbul, Springer. 2010.

The Evolution of Fun: Automatic Level Design through Challenge Modeling
Proceedings of the First International Conference on Computational Creativity (ICCCX), Lisbon, Portugal, ACM Press. 2010.
