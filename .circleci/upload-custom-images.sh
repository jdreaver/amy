#!/usr/bin/env bash

# Builds and uploads all of our custom images needed for CircleCI

set -eux;

image_tag="jdreaver/circleci-amy:latest"
docker build -t "$image_tag" .
docker push "$image_tag"
