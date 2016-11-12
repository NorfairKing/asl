#!/bin/bash

# Abort on error
set -e

stack install
orc pre-commit
