#!/bin/bash
sbt generator/docker:stage;generator/docker:publish;classifier/docker:stage;classifier/docker:publish;validator/docker:stage;validator/docker:publish