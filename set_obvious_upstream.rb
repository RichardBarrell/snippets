#!/usr/bin/env ruby
ref = `git symbolic-ref HEAD`
r = Regexp.new '^refs/heads/([^/ \f\n\r\t\v]+)$'
m = r.match(ref)
if not m
  puts "You don't appear to be on an ordinary branch."
  exit 1
end
branch = m[1]
puts "Okay, you're on #{branch}."
`git branch --set-upstream-to=origin/#{branch} #{branch}`
