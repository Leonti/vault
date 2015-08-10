from flask import Flask, Response, request, send_from_directory, send_file
import StringIO
from flask.ext.httpauth import HTTPBasicAuth
from SimpleAES import SimpleAES
import simples3
import hashlib, binascii
import json

# curl -H "Content-type: application/json" -u "admin:test" -X PUT http://127.0.0.1:5000/db -d '{"message":"Hello Data"}'
# curl -u "admin:test" http://127.0.0.1:5000/db
# openssl aes-256-cbc -d -a -in passwords -out passwords.dec

app = Flask(__name__)
app.config.from_envvar('SETTINGS')
app.config.update(SEND_FILE_MAX_AGE_DEFAULT=0)
auth = HTTPBasicAuth()

@auth.get_password
def get_pw(username):
    return app.config['PASSWORD_HASH']

@auth.hash_password
def hash_pw(password):
    dk = hashlib.pbkdf2_hmac('sha256', password, app.config['PASSWORD_SALT'], 100000)
    return binascii.hexlify(dk)

@app.route("/")
def index():
    return app.send_static_file('index.html')

@app.route('/js/<path:path>')
def send_js(path):
    return send_from_directory('static/js', path)

@app.route('/css/<path:path>')
def send_css(path):
    return send_from_directory('static/css', path)

@app.route('/tpl/<path:path>')
def send_tpl(path):
    return send_from_directory('static/tpl', path)

def get_s3_bucket():
    return simples3.S3Bucket(
                app.config['S3_BUCKET'],
                access_key=app.config['S3_ACCESS_KEY'], 
                secret_key=app.config['S3_SECRET_KEY'], 
                base_url='https://s3-' + app.config['S3_REGION'] + '.amazonaws.com/' + app.config['S3_BUCKET'])

def read_data(master_password):
    aes = SimpleAES(master_password)
    try:
        encrypted = get_s3_bucket().get(app.config['DB_FILE']).read()
        decrypted = aes.decrypt(encrypted)  
        return decrypted
    except:
        return json.dumps({'credentials': [], 'notes': []})
def save_data(master_password, data):
    aes = SimpleAES(master_password)
    encrypted = aes.encrypt(data)
    get_s3_bucket().put(app.config['DB_FILE'], encrypted)

@app.route("/db", methods = ['GET'])
@auth.login_required
def db_get():
    return Response(read_data(request.authorization.password), status=200, mimetype='application/json')

@app.route("/download", methods = ['GET'])
@auth.login_required
def db_download():
    strIO = StringIO.StringIO()
    strIO.write(get_s3_bucket().get(app.config['DB_FILE']).read())
    strIO.seek(0)
    return send_file(strIO,
                     attachment_filename="vault.db",
                     as_attachment=True)

@app.route("/db", methods = ['PUT'])
@auth.login_required
def db_put():
    save_data(request.authorization.password, request.data)
    return "DB is updated!"

if __name__ == "__main__":
    app.run(host='0.0.0.0')
