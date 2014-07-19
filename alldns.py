#!/usr/bin/env python2
import subprocess
import sys

# Usage: python alldns.py www.example.com. A
#
# The implementation here is hacky. This script sends a
# DNS query with 'dig' to every one of the nameservers
# governing a particular FQDN.
#
# Main use is for spotting DNS servers which are lagging
# on updating their zone files, or have completely gone
# out of sync.
#
# Hackiness is: rather than doing whatever one is supposed
# to do to get the nameservers for a domain, I just strip
# off the first subdomain and send an NS query to that.
# Doesn't work for bare domains or 2nd-level subdomains.


def lookup(args):
	return subprocess.Popen(["dig", "+noall", "+answer"] + args, stdout=subprocess.PIPE)


def getns(domain):
	dig = lookup([domain, "NS"])
	o, e = dig.communicate()
	assert dig.wait() == 0
	return [l.split()[4].strip() for l in o.split("\n") if l.strip()]


def strip_prefix(fqdn):
	return ".".join(fqdn.split(".")[1:])


def main():
	ns_servers = getns(strip_prefix(sys.argv[1]))
	queries = []
	for ns in ns_servers:
		queries.append([ns, lookup(["@"+ns] + sys.argv[1:])])
	for ns, query in queries:
		o, e = query.communicate()
		print ns, o.strip()
	for ns, query in queries:
		query.wait()


if __name__ == '__main__':
	main()
