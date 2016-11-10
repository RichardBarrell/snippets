-- postgres function for hashing strings into numbers in [0, 1]
-- replace 16777215.0 with 16777216.0 if you want [0, 1) instead
--
-- This is not useful for anything security related, instead it's
-- a deterministic function that looks a bit random-ish. Intended
-- use is for things like staggering the timings of periodic
-- events, by computing a random-ish looking number on demand
-- rather than having to generate *and store* a random number.
--
-- e.g. see how Jenkins interprets the 'H' symbol in a periodic
--      schedule string. http://stackoverflow.com/a/21939671
--
-- You'd typically generate the random-ish looking number from
-- any data lying around with a little bit of entropy in it,
-- such as the job's name or something.
--
-- This isn't a very precise RNG anyway, it can only generate
-- about 16 million different values (uniformly distributed in 0..1)
--
-- Also, to reiterate, not for anything security related.
-- MD5 isn't a great hash function to begin with, truncating the
-- output to 24 bits makes it outright silly for crypto.
--
CREATE FUNCTION md5_rng ( IN hash_me TEXT ) RETURNS float4 AS $$
 SELECT ('x' || substring(md5(hash_me), 1, 6))::bit(24)::int4::float4 / 16777215.0::float4;
$$ LANGUAGE sql IMMUTABLE RETURNS NULL ON NULL INPUT;
