# Note to self, this is now you supply a HTTP Basic auth username and password
# to python2.4's urllib2. I have no idea whether this has since been made
# less verbose.

def openWithBasicAuth(uri, u, p):
    pm = urllib2.HTTPPasswordMgrWithDefaultRealm()
    pm.add_password(None,uri,u,p)
    h = urllib2.HTTPBasicAuthHandler(pm)
    o = urllib2.build_opener(h)
    return o.open(uri)
