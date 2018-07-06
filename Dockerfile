FROM ubuntu:18.04

# npm install -g purescript@0.11.7
# npm install -g pulp@12.2.0
# npm install -g psc-package0.3.2

RUN \
  apt-get update && \
  apt-get install -y python python-dev python-pip libffi-dev

COPY config /config
WORKDIR /config
RUN pip install -r requirements.txt

COPY server /app
WORKDIR /app
RUN pip install -r requirements.txt

COPY client/dist /app/static

EXPOSE  5000

CMD ["python", "vault.py"]
