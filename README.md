# Automata-Learning
Program that learns automata from queries and counterexamples. Based on [Dana Angluin's Paper](http://people.eecs.berkeley.edu/~dawnsong/teaching/s10/papers/angluin87.pdf).
The program is learns from a *minimally adequate teacher*. This means that the teacher need to be able to answer membership queries on a given string and also needs to be able
to test a given Hypothesis. If a hypothesis is incorrect, the teacher needs to be able to provide a counter-example which shows the learner why their hypothesis is incorrect.

#Running
The project builds using `sbt`. Change into the directory with the project and run `sbt`. Main.scala two example teachers, one that learns the regex `ab*` and another 
that learns the regex that accepts only the strings with even number of 1s and 0s (Same as the example in the paper). Both the teachers require input from the user to resolve the
counter examples. Alternatively, one can build a DFA-equivalence checker and use that to provide examples. 

Select the desired teacher and type `run` in the sbt console to run learn the implementation.
