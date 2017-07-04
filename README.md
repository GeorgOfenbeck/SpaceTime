# Staging for Generic Programming in Space and Time [![Build Status](https://travis-ci.org/GeorgOfenbeck/SpaceTime.svg?branch=master)](https://travis-ci.org/GeorgOfenbeck/SpaceTime)

This github repository hosts the code used within my thesis work and my last publication.

1. The thesis can be found here: [Thesis]( https://drive.google.com/open?id=0B9SH4AFkecQFMnkzWi1IRGprSFE) <sup>[1](#myfootnote1)</sup>

2. The publication can be found here: (to be published)

The code itself is a major artifact of the thesis - I therefore encourage to explore it.

I assume that the reader of this document is familiar with Scala and Leightweight Modular Staging, otherwise see the Background section.

## Prerequisites

The only software pre-requirement is the [SBT](http://www.scala-sbt.org/) build tool.

## Navigating the Code

This section will give the reader a high level guide to browse the code and relate it to the publication / thesis.

### Modified version of LMS

This work utilizes a modified version of LMS found [here](https://github.com/GeorgOfenbeck/SpaceTime/tree/master/src/main/scala). The high level differences to the original [work](http://scala-lms.github.io/) are

1. Removed the dependency to [Scala Virtualized Compiler](https://github.com/TiarkRompf/scala-virtualized).
   This removes the abilitie to overload constructs such as the if statement. The focus of this work is not on DSL's, therefore we prefer to work on the main fork of the compiler.
2. Changed the reifying, codemotion, traversal and unparsing infrastructure in the backend - splitting them in seperate passes that each yield a resulting state.
3. Changed the handling of staged functions to work with only a single parameter and return type, which in turn expose nested staged values through type classes.

Changes 1 and 2 are described in Section 6 of the thesis. Change 3 is explained in Section 4.8 in the thesis and in Section 3.6 of the paper.

### Introductory examples

The examples used in chapter 3.1 of the paper and chapter 4.1 of the thesis can be found [here](https://github.com/GeorgOfenbeck/SpaceTime/tree/master/src/main/PaperExamples). More concretly

1. The [code snippets](https://github.com/GeorgOfenbeck/SpaceTime/blob/master/src/main/PaperExamples/PaperExamples.scala) used in the Section 2 of the paper / thesis.
2. The [original](https://github.com/GeorgOfenbeck/SpaceTime/blob/master/src/main/RunExampleTest.scala) and [final implementation](https://github.com/GeorgOfenbeck/SpaceTime/blob/master/src/main/PaperExamples/Section3.scala) of of the time and space generic running example described in Section 3.1-3.12 of the paper and Section 4.1 - 4.12 of the thesis.

Note that, unlike in both the paper and the thesis, we use abstract type members instead of type parameters in the implementation. While we considered the verbosity of the type parameters benefitial for the text, for a full fletched implementation they cause an unexceptable amount of boiler plate. We implemented the filter case study with both coding styles, and the tradeoff is very visible when comparing these two variants.

The two variants can be found here:
1.







## Deployment

Add additional notes about how to deploy this on a live system

## Built With

* [Dropwizard](http://www.dropwizard.io/1.0.2/docs/) - The web framework used
* [Maven](https://maven.apache.org/) - Dependency Management
* [ROME](https://rometools.github.io/rome/) - Used to generate RSS Feeds

## Contributing

Please read [CONTRIBUTING.md](https://gist.github.com/PurpleBooth/b24679402957c63ec426) for details on our code of conduct, and the process for submitting pull requests to us.

## License

This project is licensed under the MIT License - see the [LICENSE.md](LICENSE.md) file for details

## Acknowledgments

* This work would not have been possible without my PhD advisor [Markus Püschel](https://www.inf.ethz.ch/personal/markusp/)
* The whole PhD thesis was heavily co-adviced by [Tiark  Rompf](https://tiarkrompf.github.io/) and this code is based of his work on [LMS](http://scala-lms.github.io/)
* Many inspiring discusions within my former group at [ETH](https://acl.inf.ethz.ch/) influenced this work. In particular I colaborated often with [Alen Stojanov](https://acl.inf.ethz.ch/people/astojanov/) and our works are related.
* Furthermore my interaction with the [LAMP](https://lamp.epfl.ch/) group at EPFL and with all the people from the [Institute of Computer Systems](http://www.cs.inf.ethz.ch/) influenced this work

<a name="myfootnote1">1</a>: temporary link till it is available within the ETH digital library

