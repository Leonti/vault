###Vault - personal online password manager with notes
**Vault** is a small web application which you can run on your own server which stores your sensitive data in s single encrypted file and provides web interface to view and edit it.  

####The why
I wanted something to store my passwords and notes in a secure way which could be accessible both online/offline and from my Android mobile. I wasn't satisfied with services like LastPass because they are not comfortable to work with passwords other than web ones and I need to store my ssh passwords and keys somewhere. I would like to be able to access my data offline from a file without an internet connection.  
Also I already have a server running, so why not build something fun and run it for free?

####How it works  
Your data is stored in AES-256 encrypted file in your private S3 bucket.  
Web application written in Python uses a password you provide over *https* connection to decrypt the data and present it to you in the browser window.  

####What you need
* A server to run the web application. Docker support is not mandatory but it makes the whole thing a lot easier 
* Amazon S3 bucket to store files
* A domain name and a certificate for it. I use cheap-ass domain validated certificate. Self-signed one would also work.

####How secure it is?
* Data between your browser and your server is protected by *https*
* Only sha-256 salted hash is stored on your server
* The data itself is encrypted using AES-256 encryption, so getting an access to the file from S3 would also not pose a huge security risk  

####Demo
Here you can see a demo of the UI (data is stored in your browser session) [Demo](http://leonti.github.io/vault/demo)  
Any password will do :)  

####How to install  

Build docker image:  
`sudo docker build -t leonti/vault .`  

Generate configuration file:  
`sudo docker run -v /data:/data -e SETTINGS=/data/settings.cfg -t -i leonti/vault python generate_config.py`  
You will have to provide your S3 config and your master password.  

Run the whole thing:  
`sudo docker run -v /data:/data -e SETTINGS=/data/settings.cfg --name vault leonti/vault python vault.py`  

After that you have to run a docker nginx instance with domain and ssl configuration, and link `vault` container to it.  

Since everything is stored in a single file you can download it from `/download` url. This allows you to back up the file wherever you want - on your disk, usb drive, etc.  
It can be decrypted on your machine using:    
`openssl aes-256-cbc -d -a -in vault.db -out vault.dec`  
