#!/bin/sh
set -e
set +x
if test "$(git config remote.origin.url)" != "https://github.com/jsmaniac/phc-thesis.git"; then
  echo "Not on official repo, will not auto-push to master."
elif test "$TRAVIS_PULL_REQUEST" != "false"; then
  echo "This is a Pull Request, will not auto-push to master."
elif test "$TRAVIS_BRANCH" != "auto-push"; then
  echo "Not on auto-push branch (TRAVIS_BRANCH = $TRAVIS_BRANCH), will not auto-push to master."
elif test -z "${encrypted_8fdb34b09f5e_key:-}" -o -z "${encrypted_8fdb34b09f5e_iv:-}"; then
  echo "Travis CI secure environment variables are unavailable, will not auto-push to master."
else
  set -x
  echo "Automatic push to master"

  ## Git configuration:
  #git config --global user.name "$(git log --format="%aN" HEAD -1) (Travis CI automatic commit)"
  #git config --global user.email "$(git log --format="%aE" HEAD -1)"

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

  # Fetch commit
  repo_url="$(git config remote.origin.url)"
  commit_hash="$(cat commit_hash)"
  ssh_repo_url="$(echo "$repo_url" | sed -e 's|^https://github.com/|git@github.com:|')"
  #git init auto-git/
  #cd auto-git
  #git remote add origin "$ssh_repo_url"
  git fetch origin "$commit_hash"
  git log --oneline --decorate --graph -10 "$commit_hash"
  git push --quiet "$ssh_repo_url" "$commit_hash:master" || true # Do not make a tantrum in case of race conditions
fi
