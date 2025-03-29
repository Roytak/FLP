# FLP Project 1 --- Decision Tree Classifier

This project implements a decision tree classifier in Haskell. It has two modes - a classifying and a training one.
A simplified version of the CART algorithm is used for the tree training. Only best tree splits are performed based
on Gini index calculation, however no pruning on the tree is done.

## Usage

flp-fun [-1 <treeFile> <dataFile> | -2 <trainingDataFile>]
&nbsp; &nbsp; &nbsp; &nbsp; -1 <treeFile> <dataFile> : Classify data using the decision tree from <treeFile> on <dataFile>
&nbsp; &nbsp; &nbsp; &nbsp; -2 <trainingDataFile>    : Build a decision tree from the training data in <trainingDataFile>

## Building

A `Makefile` is provided to build the project. To build on Merlin, use `make merlin`.

## Testing

The project was successfully tested on the tests that were provided. The `ghc` version `9.10.1` was used to compile and test the project.
