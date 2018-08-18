## Simakka2: Semi-Distributed Discrete Event Engine using Akka actors  

### Objectives ( Nothing is implemented yet)

* Targets multi-core PCs
* Define more light-weight entities to allow simulating millions of them at the same time.
* Light-weight compared with thread based solutions
* Avoid using thread locks in user code. Locks are still an option but are not recommended.
* Simplified dynamic lookahead protocol for semi-distribution
* Statistical collection model which allows efficient recording of statistical variables
* Extension points to allow writing custom statistical variables monitors for visualization, real-time processing, etc
* Defining and using statistical variables very similar to defining primitive variables (Double)
* Extension points...
* Simple DSL for common simulation behaviours
* To be easy to integrate with jvm based systems.

## Requirement 
TODO provide sbt file

* Java virtual machine: Java8 or Java9
* Scala 2.12.xx
* Akka 2.5.xx

