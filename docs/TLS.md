## Generate a self-signed certificate for testing
`
openssl genrsa -out key.pem 2048
openssl req -new -key key.pem -out certificate.csr
openssl x509 -req -in certificate.csr -signkey key.pem -out certificate.pem
`

### Generate v3 version certifiacate which will help to escape the LeafNotV3 error.
checkLeafV3 = False to skip this error.

`
openssl req -new -x509 -nodes -set_serial 2005100101 -keyout key_cert.pem -out key_cert.pem -days 365
`


## The commands below can be used to view the certificate fingerprint/thumbprint.

openssl x509 -noout -fingerprint -sha256 -inform pem -in key_cert.pem
openssl x509 -noout -fingerprint -sha1 -inform pem -in key_cert.pem
openssl x509 -noout -fingerprint -md5 -inform pem -in key_cert.pem
