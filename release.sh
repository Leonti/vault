#!/bin/bash

version=$(date +"%y.%m.%d.%H.%M")

sudo docker build -t leonti/vault:$version .
sudo docker push leonti/vault
git tag -a v$version -m 'new version $version'

echo "Released version $version"

