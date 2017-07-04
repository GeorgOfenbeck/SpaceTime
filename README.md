# Staging for Generic Programming in Space and Time [![Build Status](https://travis-ci.org/GeorgOfenbeck/SpaceTime.svg?branch=master)](https://travis-ci.org/GeorgOfenbeck/SpaceTime)

This github repository hosts the code used within my thesis work and my last publication.

1. The thesis can be found here: [Thesis]( https://drive.google.com/open?id=0B9SH4AFkecQFMnkzWi1IRGprSFE) <sup>[1](#myfootnote1)</sup>

2. The publication can be found here: (to be published)

The code itself is a major artifact of the thesis - I therefore encourage to explore it. For background material see the thesis and the publication.

## Prerequisites

The only software pre-requirement is the [SBT](http://www.scala-sbt.org/) build tool.

## Navigating the Code

This section will give the reader a high level guide to browse the code and relate it to the publication / thesis.

### Modified version of LMS

This work utilizes a modified version of LMS found [here](https://github.com/GeorgOfenbeck/SpaceTime/tree/master/src/main/scala). The high level differences to the original [work](http://scala-lms.github.io/) are

1. Removed the dependency to [Scala Virtualized Compiler](https://github.com/TiarkRompf/scala-virtualized).
  This removes the ability to overload constructs such as the if statement. The focus of this work is not on DSL's, therefore we prefer to work on the main fork of the compiler.
2. Changed the reifying, codemotion, traversal and unparsing infrastructure in the backend - splitting them in separate passes that each yield a resulting state.
3. Changed the handling of staged functions to work with only a single parameter and return type, which in turn expose nested staged values through type classes.

Changes 1 and 2 are described in Section 6 of the thesis. Change 3 is explained in Section 4.8 in the thesis and in Section 3.6 of the paper.

### Introductory examples

The examples used in chapter 3.1 of the paper and chapter 4.1 of the thesis can be found [here](https://github.com/GeorgOfenbeck/SpaceTime/tree/master/src/main/PaperExamples). More concretely

1. The [code snippets](https://github.com/GeorgOfenbeck/SpaceTime/blob/master/src/main/PaperExamples/PaperExamples.scala) used in the Section 2 of the paper / thesis.
2. The [original](https://github.com/GeorgOfenbeck/SpaceTime/blob/master/src/main/RunExampleTest.scala) and [final implementation](https://github.com/GeorgOfenbeck/SpaceTime/blob/master/src/main/PaperExamples/Section3.scala) of of the time and space generic running example described in Section 3.1-3.12 of the paper and Section 4.1 - 4.12 of the thesis.

Note that, unlike in both the paper and the thesis, we use abstract type members instead of type parameters in the implementation. While we considered the verbosity of the type parameters beneficial for the text, for a full scale implementation they cause an unacceptable amount of boilerplate. We implemented the filter case study with both coding styles, and the tradeoff is very visible when comparing these two variants.

The two variants can be found here:
1. [Version using type parameters](https://github.com/GeorgOfenbeck/SpaceTime/tree/master/src/main/Filter)
2. [Version using abstract member types](https://github.com/GeorgOfenbeck/SpaceTime/tree/master/src/main/Filter2)

### Filter case study

The filter case study used within the paper in Section 4.1 can be found [here](https://github.com/GeorgOfenbeck/SpaceTime/tree/master/src/main/Filter2). As mentioned above, we implemented this particular case study in two variants. The one used to create the final results utilizes the abstract type member approach. The major constructs can be found in the following files

1. The core logic of the time and space generic convolution is found [here](https://github.com/GeorgOfenbeck/SpaceTime/blob/master/src/main/Filter2/Core.scala)
2. To execute the generator adapt the path in *codeexport()* in the [Core](https://github.com/GeorgOfenbeck/SpaceTime/blob/master/src/main/Filter2/Core.scala) and the execute the main function in [GenMain](https://github.com/GeorgOfenbeck/SpaceTime/blob/master/src/main/Filter2/GenMain.scala)
3. The type classes, supporting the time generic implementation are divided into generic once in [Skeleton](https://github.com/GeorgOfenbeck/SpaceTime/blob/master/src/main/Filter2/Skeleton.scala) and Filter specifi ones in [Header](https://github.com/GeorgOfenbeck/SpaceTime/blob/master/src/main/Filter2/FilterHeader.scala)
4. Validation and benchmarking was performed with [ImageTest](https://github.com/GeorgOfenbeck/SpaceTime/blob/master/src/main/Filter2/ImageTest.scala)

### FFT case study

The prime use case of the thesis and the paper can be found [here](https://github.com/GeorgOfenbeck/SpaceTime/tree/master/src/main/SpiralSThesis). The code is separated into:
1. The [Core Logic](https://github.com/GeorgOfenbeck/SpaceTime/blob/master/src/main/SpiralSThesis/Core.scala): Chapter 5 of the thesis is dedicated to explaining each generic construct of the core logic step by step. The final implementation in Section 5.3 is the one found within this file.
2. The type classes, supporting the time generic implementation are divided into [Skeleton](https://github.com/GeorgOfenbeck/SpaceTime/blob/master/src/main/Filter2/Skeleton.scala) and [Header](https://github.com/GeorgOfenbeck/SpaceTime/blob/master/src/main/Filter2/FilterHeader.scala)
3. [Twiddle.scala](https://github.com/GeorgOfenbeck/SpaceTime/blob/master/src/main/SpiralSThesis/Twiddle.scala) contains the logic required to calculate the DFT specific Twiddle factors.
4. [CorewGlue.scala](https://github.com/GeorgOfenbeck/SpaceTime/blob/master/src/main/SpiralSThesis/CorewGlue.scala) is used to invoke the [core logic](https://github.com/GeorgOfenbeck/SpaceTime/blob/master/src/main/SpiralSThesis/Core.scala) with accordingly typed parameters. In addition it contains hard coded infrastructure code that was used for timing and validation, which is unparsed in combination with the generated code.

#### Executing the FFT case study

To execute the case study there are two options.
1. A website based version of the generator can be found [here] (**online asap**)
2. The [GUI](https://github.com/GeorgOfenbeck/SpaceTime/tree/master/src/main/SpiralSThesisGui) module contains a Java Swing Interface that was used to produce all performance plots and the comparision to JTransform within the thesis and the publication. This was used for internal testing and prototyping and is not documented or maintained.


## License

This project is licensed under the MIT License - see the [LICENSE.md](LICENSE.md) file for details

## Acknowledgments

* This work would not have been possible without my PhD advisor [Markus Püschel](https://www.inf.ethz.ch/personal/markusp/)
* The whole PhD thesis was heavily co-advised by [Tiark  Rompf](https://tiarkrompf.github.io/) and this code is based of his work on [LMS](http://scala-lms.github.io/)
* Many inspiring discussions within my former group at [ETH](https://acl.inf.ethz.ch/) influenced this work. In particular I collaborated often with [Alen Stojanov](https://acl.inf.ethz.ch/people/astojanov/) and our works are related.
* Furthermore my interaction with the [LAMP](https://lamp.epfl.ch/) group at EPFL and with all the people from the [Institute of Computer Systems](http://www.cs.inf.ethz.ch/) influenced this work

<a name="myfootnote1">1</a>: temporary link till it is available within the ETH digital library



