#!/usr/bin/env bash

set -euET -o pipefail

# Scribble (in Racket 7.3 at least) incorrectly redirects some local links within the page to a remote server, this restores the links back to local.
# Replace "https://docs.racket-lang.org/local-redirect/index.html?doc=phc-thesis&rel=index.html%23…"
# with a single #
# and translate back the %25 escape sequences in the URL
# sed can't do a replacement within the matched pattern & and my perl is rusty…

set -x
sed -i -e 's~X~XA~g' doc/phc-thesis/index.html
sed -i -e 's~Y~XB~g' doc/phc-thesis/index.html
tr \\n Y < doc/phc-thesis/index.html > doc/phc-thesis/index.html2
mv doc/phc-thesis/index.html2 doc/phc-thesis/index.html
sed -i -e 's~"https://docs\.racket-lang\.org/local-redirect/index\.html[?]doc=phc-thesis&amp;rel=index\.html%23\([^"]*"\)~"#\nXC\1\n"~g' doc/phc-thesis/index.html
sed -i -e '/^XC/s~%25~XD~g' doc/phc-thesis/index.html
if grep 'XC.*%' doc/phc-thesis/index.html; then
  echo "Error: some urlencoded escape code was not properly filtered by fixlinks.sh"
  exit 1
fi
tr -d \\n < doc/phc-thesis/index.html > doc/phc-thesis/index.html2
mv doc/phc-thesis/index.html2 doc/phc-thesis/index.html
sed -i -e 's~Y~\n~g' doc/phc-thesis/index.html
sed -i -e 's~XD~%~g' doc/phc-thesis/index.html
sed -i -e 's~XC~~g' doc/phc-thesis/index.html
sed -i -e 's~XB~Y~g' doc/phc-thesis/index.html
sed -i -e 's~XA~X~g' doc/phc-thesis/index.html

# Transform 'href="index.html#…' to 'href="#…' so that links within the page don't cause a reload if the URL was …/phc-thesis/ vs. …/phc-thesis/index.html
sed -i -e 's/href="index.html#/href="#/g' doc/phc-thesis/index.html

cp doc/phc-thesis/index.html doc/index.html
sed -i -e 's~"../pdf/~"pdf/~' doc/index.html
sed -i -e 's~"../phc-thesis-/~"phc-thesis-/~' doc/index.html
set +x
for i in doc/phc-thesis/*; do
  escaped_name="$(basename "$i" | sed -e 's~[]$.*/[\^]~\\&~g' )"
  sed -i -e 's/\(href\|src\)="'"\($escaped_name\)"'"/\1="phc-thesis\/\2"/g' doc/index.html
done
set -x

sed -i -e '1i<!-- This file is a copy of ../index.html with the relative URLs adjusted, it is kept here so that the old URL …/phc-theis/index.html keeps working but the new URL …/index.html should be used. -->' doc/phc-thesis/index.html
