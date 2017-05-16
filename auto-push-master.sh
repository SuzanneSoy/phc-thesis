#!/bin/sh
set -e
set +x
if test "$(git config remote.origin.url)" != "https://github.com/jsmaniac/phc-thesis.git"; then
  echo "Not on official repo, will not auto-push to master."
elif test "$TRAVIS_EVENT_TYPE" != "push"; then
  echo "This is a not a Push event (TRAVIS_EVENT_TYPE=$TRAVIS_EVENT_TYPE), will not auto-push to master."
elif test "$TRAVIS_BRANCH" != "dev"; then
  echo "Not on dev branch (TRAVIS_BRANCH = $TRAVIS_BRANCH), will not auto-push to master."
elif test -z "${encrypted_8fdb34b09f5e_key:-}" -o -z "${encrypted_8fdb34b09f5e_iv:-}"; then
  echo "Travis CI secure environment variables are unavailable, will not auto-push to master."
else
  set -x
  echo "Automatic push to master"

  # Git configuration:
  git config --global user.name "$(git log --format="%aN" HEAD -1) (Travis CI automatic commit)"
  git config --global user.email "$(git log --format="%aE" HEAD -1)"

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

  # Push to the auto-git branch, which can then auto-push to master, after this build finished
  repo_url="$(git config remote.origin.url)"
  ssh_repo_url="$(echo "$repo_url" | sed -e 's|^https://github.com/|git@github.com:|')"
  commit_hash="$(git rev-parse HEAD)"
  git log --oneline --decorate --graph -10
  git fetch origin auto-push
  git checkout FETCH_HEAD
  echo "$commit_hash" > commit_hash
  git add commit_hash
  git commit -m "Request to auto-push $commit_hash to master" --allow-empty
  git log --oneline --decorate --graph -10
  git push --quiet "$ssh_repo_url" HEAD:auto-push || true # do not cause a tantrum in case of race conditions.
fi
