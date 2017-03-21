#!/bin/sh
set -e
set +x
if test "$(git config remote.origin.url)" != "https://github.com/jsmaniac/phc-thesis.git"; then
  echo "Not on official repo, will not deploy gh-pages."
elif test "$TRAVIS_PULL_REQUEST" != "false"; then
  echo "This is a Pull Request, will not deploy gh-pages."
elif test "$TRAVIS_BRANCH" != "master"; then
  echo "Not on master branch (TRAVIS_BRANCH = $TRAVIS_BRANCH), will not deploy gh-pages."
elif test -z "${encrypted_8fdb34b09f5e_key:-}" -o -z "${encrypted_8fdb34b09f5e_iv:-}"; then
  echo "Travis CI secure environment variables are unavailable, will not deploy gh-pages."
else
  set -x
  echo "Automatic push to gh-pages"

  # Git configuration:
  git config --global user.name "$(git log --format="%aN" HEAD -1) (Travis CI automatic commit)"
  git config --global user.email "$(git log --format="%aE" HEAD -1)"

  # SSH configuration
  mkdir -p ~/.ssh
  chmod 700 ~/.ssh
  set +x
  if openssl aes-256-cbc -K $encrypted_8fdb34b09f5e_key -iv $encrypted_8fdb34b09f5e_iv -in travis-deploy-key-id_rsa.enc -out ~/.ssh/travis-deploy-key-id_rsa -d >/dev/null 2>&1; then
    echo "Decrypted key successfully."
  else
    echo "Error while decrypting key."
  fi
  set -x
  chmod 600 ~/.ssh/travis-deploy-key-id_rsa
  set +x
  eval `ssh-agent -s`
  set -x
  ssh-add ~/.ssh/travis-deploy-key-id_rsa

  TRAVIS_GH_PAGES_DIR="$HOME/travis-gh-pages-$(date +%s)"
  if test -e $TRAVIS_GH_PAGES_DIR; then rm -rf $TRAVIS_GH_PAGES_DIR; fi
  mv -i doc $TRAVIS_GH_PAGES_DIR
  git init $TRAVIS_GH_PAGES_DIR
  #rm -f $TRAVIS_GH_PAGES_DIR/MathJax
  #mkdir $TRAVIS_GH_PAGES_DIR/MathJax
  #echo 'document.write("<script src=\"http://cdn.mathjax.org/mathjax/latest/MathJax.js?config=default\"></script>");' > $TRAVIS_GH_PAGES_DIR/MathJax/MathJax.js
  touch $TRAVIS_GH_PAGES_DIR/.nojekyll
  (cd $TRAVIS_GH_PAGES_DIR && git add -A . && git commit -m "Auto-publish to gh-pages")
  echo '(cd '"$TRAVIS_GH_PAGES_DIR"'; git push --force --quiet "https://git@github.com/jsmaniac/phc-thesis.git" master:gh-pages)'
  (cd $TRAVIS_GH_PAGES_DIR; git push --force --quiet "https://git@github.com/jsmaniac/phc-thesis.git" master:gh-pages >/dev/null 2>&1 || true) >/dev/null 2>&1 # redirect to /dev/null to avoid showing credentials.
fi