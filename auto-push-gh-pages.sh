#!/bin/sh
set -e
set +x
ssh_config(){
  # SSH configuration
  mkdir -p ~/.ssh
  chmod 700 ~/.ssh
  set +x
  if openssl aes-256-cbc -K $encrypted_8fdb34b09f5e_key -iv $encrypted_8fdb34b09f5e_iv -in travis-deploy-key-id_rsa.enc -out travis-deploy-key-id_rsa -d >/dev/null 2>&1; then
    echo "Decrypted key successfully."
  else
    echo "Error while decrypting key."
  fi
  mv travis-deploy-key-id_rsa ~/.ssh/travis-deploy-key-id_rsa
  set -x
  chmod 600 ~/.ssh/travis-deploy-key-id_rsa
  set +x
  eval `ssh-agent -s`
  set -x
  ssh-add ~/.ssh/travis-deploy-key-id_rsa
}

if test "$TRAVIS_BRANCH" == "auto-push"; then
  target_branch=gh-pages
  ssh_config
  git clone https://github.com/jsmaniac/phc-thesis.git gh-pages-candidate
  # TODO: check that the .version corresponds
  # TODO: then auto-push, or leave the if statement and let the regular build take place.
  exit 0;
else
  target_branch=gh-pages-candidate
fi


if test "$(git config remote.origin.url)" != "https://github.com/jsmaniac/phc-thesis.git"; then
  echo "Not on official repo, will not deploy gh-pages."
elif test "$TRAVIS_EVENT_TYPE" != "push" -a "$TRAVIS_EVENT_TYPE" = "cron"; then
  echo "This is a Pull Request, API request, will not deploy gh-pages."
elif test "$TRAVIS_BRANCH" != "dev" && "$TRAVIS_BRANCH" != "master"; then
  echo "Not on master or dev branch (TRAVIS_BRANCH = $TRAVIS_BRANCH), will not deploy gh-pages."
elif test -z "${encrypted_8fdb34b09f5e_key:-}" -o -z "${encrypted_8fdb34b09f5e_iv:-}"; then
  echo "Travis CI secure environment variables are unavailable, will not deploy gh-pages."
else
  set -x
  echo "Automatic push to $target_branch"

  # Git configuration:
  git config --global user.name "$(git log --format="%aN" HEAD -1) (Travis CI automatic commit)"
  git config --global user.email "$(git log --format="%aE" HEAD -1)"

  ssh_config

  TRAVIS_GH_PAGES_DIR="$HOME/travis-gh-pages-$(date +%s)"
  if test -e $TRAVIS_GH_PAGES_DIR; then rm -rf $TRAVIS_GH_PAGES_DIR; fi
  mv -i doc $TRAVIS_GH_PAGES_DIR
  git init $TRAVIS_GH_PAGES_DIR
  #rm -f $TRAVIS_GH_PAGES_DIR/MathJax
  #mkdir $TRAVIS_GH_PAGES_DIR/MathJax
  #echo 'document.write("<script src=\"http://cdn.mathjax.org/mathjax/latest/MathJax.js?config=default\"></script>");' > $TRAVIS_GH_PAGES_DIR/MathJax/MathJax.js
  touch $TRAVIS_GH_PAGES_DIR/.nojekyll
  (cd $TRAVIS_GH_PAGES_DIR && git add -A . && git commit -m "Auto-publish to gh-pages")
  (cd $TRAVIS_GH_PAGES_DIR && git log --oneline --decorate --graph -10)
  echo '(cd '"$TRAVIS_GH_PAGES_DIR"'; git push --force --quiet "git@github.com/jsmaniac/phc-thesis.git" master:'"$target_branch"')'
  (cd $TRAVIS_GH_PAGES_DIR; git push --force --quiet "git@github.com:jsmaniac/phc-thesis.git" master:$target_branch >/dev/null 2>&1) >/dev/null 2>&1 # redirect to /dev/null to avoid showing credentials.
fi
