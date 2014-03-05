import bencode
import requests
import hashlib
import urllib

with open('../test.torrent') as inpf:
    minfo = bencode.bdecode(inpf.read())

encoded_info = bencode.bencode(minfo['info'])
hash = hashlib.sha1()
hash.update(encoded_info)
print "RAW: " + hash.digest()
print "HASH: " + hash.hexdigest()
print "URLENCODED: " + urllib.urlencode({"info_hash": hash.digest()})

