This is a microbenchmark I wrote to see whether JNI was actually
really slow, by comparison to dlsym().  Edit the makefile to fix up
the path to your JDK includes, then run "make all" to build everything
and "make run" to run it all.

YMMV, but on my laptop the c/dlsym() and the java/jni programs run at
about the same speed. The main difference between them seems to be the
JVM's start-up time.
