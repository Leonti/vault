FROM ubuntu:15.04

RUN \
  apt-get update && \
  apt-get install -y python python-dev python-pip libffi-dev

COPY . /root
RUN cd /root; pip install -r requirements.txt

WORKDIR /root

EXPOSE  5000

CMD ["python", "vault.py"]
