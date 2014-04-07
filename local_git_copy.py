#!/usr/bin/env python

# Copy a git repo locally the dirty way, preserving the URL of the
# existing remotes, but also adding a new remote called "local" unless
# one already exists. :)

import sys
import subprocess
import os, os.path


def main(from_repo, unto_repo):
    if os.path.exists(unto_repo):
        print "%r already exists." % unto_repo
        return 1
    from_dotgit = os.path.join(from_repo, '.git')
    unto_dotgit = os.path.join(unto_repo, '.git')
    if not os.path.isdir(from_dotgit):
        print "%r doesn't have a git repo." % from_repo
        return 1
    subprocess.check_call(["mkdir", "-p", unto_repo])
    subprocess.check_call(["cp", "-r", from_dotgit, unto_dotgit])
    subprocess.call(
        ["git", "remote", "add", "local", os.path.abspath(from_repo)],
        cwd=unto_repo,
    )
    subprocess.check_call(["git", "reset", "--hard", "HEAD"], cwd=unto_repo)


if __name__ == '__main__':
    if len(sys.argv) != 3:
        print "Usage: local_git_copy input_repo output_repo"
    sys.exit(main(sys.argv[1], sys.argv[2]))
