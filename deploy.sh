#!/bin/bash

version=$1
docker stop vault
docker rm vault
sudo docker run -d -v /data:/data -e SETTINGS=/data/settings.cfg --name vault leonti/vault
