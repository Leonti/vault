#!/usr/bin/env bash
set -e

version=$(date +"%y.%m.%d.%H.%M")

docker login -u="$DOCKER_USERNAME" -p="$DOCKER_PASSWORD"
sudo docker build -t leonti/vault:$version .
sudo docker push leonti/vault

echo $version" is built"
