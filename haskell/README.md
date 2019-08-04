# Brainfuck compile and virtual machine in Haskell

This is a [brainfuck](https://en.wikipedia.org/wiki/Brainfuck) compile and virtual machine.
It's a toy project to experiment with simple programming languages and their implementations.

The compiler is currently non-optimizing and emits byte-code that can be run on the 
provided virtual machine. There is currently a single implementation of the VM which is a stack VM
with an accumulator. 

Although the language is very, very simple it still provides a good basis to study the implementation
efforts required to make it compilable and runnable.

The haskell code provided is a bit dated and I would not build it this way today but at that time
a monad stack seemed to be a natural choice for the VM.

Additionally this is not using stack yet, which should be relatively simple to add though.
