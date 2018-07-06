import bcrypt
import hashlib, binascii
import os

s3_access_key = raw_input('s3 access key:\n')
s3_secret = raw_input('s3 secret:\n')
s3_region = raw_input('s3 region:\n')
s3_bucket = raw_input('s3 bucket:\n')

salt = bcrypt.gensalt()
password = raw_input('Master password:\n')

dk = hashlib.pbkdf2_hmac('sha256', password, salt, 100000)
master_password_hash = binascii.hexlify(dk)

config = """
PASSWORD_HASH='{0}'
PASSWORD_SALT='{1}'
S3_ACCESS_KEY='{2}'
S3_SECRET_KEY='{3}'
S3_REGION='{4}'
S3_BUCKET='{5}'
DB_FILE='{6}'
"""

formatted_config = config.format(master_password_hash, salt, s3_access_key, s3_secret, s3_region, s3_bucket, "vault")

with open(os.environ['SETTINGS'], "w") as config_file:
    config_file.write(formatted_config)

print formatted_config
print "\nwritten to: " + os.environ['SETTINGS']
